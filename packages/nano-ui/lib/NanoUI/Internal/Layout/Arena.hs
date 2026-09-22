{-# LANGUAGE RecordWildCards #-}

-- | Layout nodes for one frame, stored in flat mutable arrays.
--
-- A node is an index ('NodeIdx') into those arrays. Indices count up in the
-- order the view adds nodes, so a parent's index is lower than its children's.
-- The strided arrays give every node a row of fixed width, and a column such
-- as 'GeomX' or 'TreeParent' names a slot in the row: node @idx@ keeps
-- geometry column @col@ at element @idx * geomStride + fromEnum col@ of
-- 'naArrGeom'. 'NodeArenaArrays' lists the arrays, and accessors such as
-- 'getRect' and 'getParent' hide the arithmetic.
--
-- The widgets ("NanoUI.Internal.Widgets.Node") fill the arena while the view runs,
-- "NanoUI.Internal.Layout.Solve" writes every node's rect into it, and the frame's
-- input, paint and damage passes read it. 'resetNodeArena' empties it before
-- the view runs again, and a node index means nothing after that.
--
-- The arena also owns the solver's scratch buffers and memos, so that they
-- stay allocated from one frame to the next. The 'LayoutCache' defined here
-- lets a frame whose layout inputs did not change skip the solve.
module NanoUI.Internal.Layout.Arena
  ( NodeIdx
  , IOArr
  , NodeType (..)
  , NodeArenaArrays (..)
  , isWidgetNode
  , hasCenteredLabel
  , isContainerNode
  , isScrollNode
  , isFloatingNode
  , NodeClass (..)
  , SizingTag (..)
  , DirTag (..)
  , NodeArena (..)
  , FlexScratch (..)
  , newNodeArena
  , resetNodeArena
  , arenaCount
  , topModalNode
  , floatingNodeCount
  , arenaArrays
  , withArenaArraysSnap
  , GeomCol (..)
  , StyleCol (..)
  , TagCol (..)
  , TreeCol (..)
  , readGeom
  , writeGeom
  , readStyle
  , readTagEnum
  , writeTagEnum
  , readTree
  , writeTree
  , addNode
  , addNodeFromLayout
  , rootAttachParent
  , setNodeText
  , getParent
  , getFirstChild
  , getNextSibling
  , getNodeType
  , getDirection
  , getScrollContentW
  , setScrollContentW
  , AxisSizing (..)
  , readAxisSizing
  , getWidthSizing
  , getHeightSizing
  , getPadding
  , parentIsRow
  , getAlignX
  , getAlignY
  , getRect
  , getNodeRect
  , setRect
  , getClipRect
  , setClipRect
  , getText
  , getOptions
  , setOptions
  , getWidgetId
  , setWidgetId
  , lookupNodeByWidgetId
  , lookupNodeByKey
  , getStyleIdx
  , setStyleIdx
  , getNodeValue
  , setNodeValue
  , getNodeFontSize
  , getNodeFontColor
  , getNodeScope
  , getArenaScope
  , setArenaScope
  , getScopeSignature
  , ensureScratchCapacity
  , AxisSnapshot (..)
  , ensureAxisSnapshot
  , memoizeWidth
  , forFloatingNodes_
  , forChildNodes_
  , foldFlowChildrenM
  , findNodeRevM
  , findClassNodeM
  , findClassNodeRevM
  , foldClassNodesM
  , foldClassNodeRevM
  , forClassNodes_
  , walkFloatingAncestors
  , findNodeM
  , foldNodesM
  , findChildM
  , walkAncestors
  , LayoutCache (..)
  , CustomMeasureRecord
  , newLayoutCache
  , captureLayoutCache
  , layoutSigMatches
  , computeSubtreeHashes
  , subtreeArrays
  , restoreLayoutCache
  ) where

import Control.Exception (bracket_)
import Control.Monad (forM_, unless, when)
import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.HashTable.IO (BasicHashTable)
import qualified Data.HashTable.IO as HT
import Data.Hashable (Hashable, hash)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Primitive.Array (MutableArray, copyMutableArray, newArray, readArray, sizeofMutableArray, writeArray)
import Data.Primitive.PrimArray
  ( MutablePrimArray
  , copyMutablePrimArray
  , getSizeofMutablePrimArray
  , newPrimArray
  , readPrimArray
  , setPrimArray
  , writePrimArray
  , resizeMutablePrimArray
  )
import Data.Primitive.Types (Prim)
import GHC.Exts (RealWorld)
import Data.Text (Text)
import Data.Word (Word8, Word32, Word64)
import qualified Data.Text as T
import GHC.Float (castFloatToWord32)
import NanoUI.Internal.Id (WidgetId (..), hashWidgetId)
import NanoUI.Internal.Store (ptrEq)
import NanoUI.Internal.Style (AlignX, AlignY, Direction (..), Layout (..), Padding (..), Sizing (..))
import NanoUI.Internal.Types (Color (..), Rect (..))

-- | A node's position in the arena's arrays. It is valid from the 'addNode'
-- that returned it until the next 'resetNodeArena'. Where an index names an
-- optional node (a parent, a first child, a next sibling), -1 means none.
type NodeIdx = Int

-- | The arena's mutable unboxed arrays.
type IOArr = MutablePrimArray RealWorld

-- | What kind of node this is. The solver chooses how to measure a node by
-- its type, and paint chooses how to draw it. The arena stores the type as a
-- 'Word8' ('TagNodeType') through the derived 'Enum' instance, so there can be
-- at most 256 constructors. Several types keep type-specific data in the
-- node's style index ('getStyleIdx').
data NodeType
  = NodeContainer
  -- ^ A row, column or grid.
  | NodeText
  -- ^ A label. Its style index packs the font variant, weight, slant and
  -- text decoration.
  | NodeSpacer
  -- ^ Empty space.
  | NodeSeparator
  -- ^ A one-pixel rule.
  | NodeButton
  -- ^ A button. Flags in its style index turn it into a close button, a tab,
  -- a table header, a menu-bar title or a menu item.
  | NodeCheckbox
  | NodeSlider
  | NodeTextInput
  -- ^ A single-line text field. Flags in its style index turn it into a search
  -- field, a numeric field, a password field or selectable read-only text.
  | NodeTextArea
  -- ^ A multi-line text field, which scrolls its own content.
  | NodeScrollContainer
  -- ^ A container that scrolls its children. Its style index holds the encoded
  -- scroll configuration.
  | NodeSelect
  -- ^ A select box. Its choices are the node's options ('getOptions').
  | NodeModal
  -- ^ A modal dialog. Floating: see 'isFloatingNode'.
  | NodeImage
  -- ^ An image. The node's text is the image id in decimal.
  | NodePanel
  -- ^ A container that paints the theme's panel background and border, and
  -- clips its children to the inside of the border.
  | NodeWindow
  -- ^ A window inside the application's window. Floating.
  | NodeBox
  -- ^ A solid rectangle. Its style index is the fill colour's 32-bit word.
  | NodeRadio
  -- ^ A radio button. Its style index is its option's index in the group.
  | NodeColorPicker
  -- ^ One part of a colour picker. Its style index says which part.
  | NodeTree
  -- ^ A row of a tree view. Its style index packs the row's depth and whether
  -- it has children and is expanded.
  | NodePopup
  -- ^ A popup such as a menu or a tooltip. Floating.
  | NodeDrawing
  -- ^ A widget the application draws: a custom widget or a drawing.
  deriving (Eq, Show, Enum, Bounded)

-- | Whether the node is a control the pointer can hover: a button, checkbox,
-- radio button, slider, text field, text area, select, colour picker part,
-- tree row or drawing. Hover detection and the layout of widget labels use
-- it. Labels, images, boxes, spacers, separators and containers are not.
isWidgetNode :: NodeType -> Bool
isWidgetNode nt =
  case nt of
    NodeButton -> True
    NodeCheckbox -> True
    NodeRadio -> True
    NodeSlider -> True
    NodeTextInput -> True
    NodeTextArea -> True
    NodeSelect -> True
    NodeColorPicker -> True
    NodeTree -> True
    NodeDrawing -> True
    _ -> False

-- | Whether the node paints one line of label text centered vertically in its
-- box: a button, select, tree row, checkbox or radio button. The solver takes
-- that line's baseline as the node's baseline when a row aligns its children
-- on their baselines, and "NanoUI.Internal.Frame.Spans" caches where the label goes
-- (@computeWidgetLabel@).
hasCenteredLabel :: NodeType -> Bool
hasCenteredLabel nt =
  case nt of
    NodeButton -> True
    NodeSelect -> True
    NodeTree -> True
    NodeCheckbox -> True
    NodeRadio -> True
    _ -> False

-- | Whether the node lays out children: a plain container, scroll container,
-- panel, modal, window or popup.
isContainerNode :: NodeType -> Bool
isContainerNode nt =
  case nt of
    NodeContainer -> True
    NodeScrollContainer -> True
    NodeModal -> True
    NodePanel -> True
    NodeWindow -> True
    NodePopup -> True
    _ -> False

-- | Whether the node is a 'NodeScrollContainer'. A text area scrolls its own
-- content and does not count.
isScrollNode :: NodeType -> Bool
isScrollNode nt = nt == NodeScrollContainer

-- | Whether the node is a modal, window or popup. The solver leaves a floating
-- node out of its parent's flow and places it after the solve
-- ('NanoUI.Internal.Layout.Solve.placeFloatingNodes'), and paint draws it over
-- the page.
isFloatingNode :: NodeType -> Bool
isFloatingNode nt = nt == NodeModal || nt == NodeWindow || nt == NodePopup

-- | A kind of node the arena lists as the view adds them, so that a pass
-- interested only in that kind visits those nodes and not the whole arena.
-- Each list is in arena order.
data NodeClass
  = PointerNodes
  -- ^ The nodes a pointer hit test can want: the controls of 'isWidgetNode'
  -- and the scroll containers ('isScrollNode').
  | SelectionNodes
  -- ^ Checkboxes, radio buttons and tree rows, whose node value mirrors
  -- selection state in the store.
  | DrawingNodes
  -- ^ Widgets the application draws ('NodeDrawing').
  | FloatingNodes
  -- ^ Windows, modals and popups ('isFloatingNode'), so the passes that look
  -- only at floating panels skip the rest of the arena.
  deriving (Eq, Enum, Bounded)

-- | The constructor of a 'Sizing' without its number, as the arena stores it
-- in 'TagWSizing' and 'TagHSizing'. The number goes in 'StyleWVal' or
-- 'StyleHVal'.
data SizingTag
  = SizingFixed
  | SizingFit
  | SizingGrow
  | SizingShrink
  | SizingPercent
  deriving (Eq, Show, Enum, Bounded)

-- | A 'Direction' as the arena stores it in 'TagDirection'. On a container
-- it is the axis the children are laid out along. A separator carries its
-- parent's direction, which decides whether the rule is horizontal or
-- vertical.
data DirTag = DirRow | DirColumn
  deriving (Eq, Show, Enum, Bounded)

-- | The arena's per-node arrays. The first four are strided: node @idx@ owns
-- the half-open range @idx * stride@ to @(idx + 1) * stride@, and the columns
-- ('GeomCol', 'StyleCol', 'TagCol', 'TreeCol') name the slots of that row.
-- The other arrays hold one element per node. Growing the arena replaces all
-- of them together.
data NodeArenaArrays = NodeArenaArrays
  { naArrGeom :: !(IOArr Float)
  -- ^ Rects, 8 floats per node. See 'GeomCol'.
  , naArrStyle :: !(IOArr Float)
  -- ^ Layout inputs and a few other numbers, 16 floats per node. See
  -- 'StyleCol'.
  , naArrTags :: !(IOArr Word8)
  -- ^ Enum values, 8 bytes per node. See 'TagCol'.
  , naArrTree :: !(IOArr Int)
  -- ^ Tree links and other integers, 8 per node. See 'TreeCol'.
  , naArrTextStore :: !(MutableArray RealWorld Text)
  -- ^ Each node's text. 'addNode' does not clear a slot, so read it with
  -- 'getText', which checks that the node set a text.
  , naArrOptionsStore :: !(MutableArray RealWorld [Text])
  -- ^ Each node's option list ('getOptions'), empty unless the node set one.
  , naArrFontColor :: !(IOArr Int)
  -- ^ Each node's font colour: 0 for none, and otherwise the colour's 32-bit
  -- word with bit 32 set. See 'getNodeFontColor'.
  , naArrScope :: !(IOArr Int)
  -- ^ The paint scope each node was added under: the theme that a @styled@ or
  -- @disabledWhen@ block around the node selected, and whether the node is
  -- disabled. The theme index sits above bit 0, with 0 for the context's
  -- theme, and bit 0 is the disabled flag. See
  -- 'NanoUI.Internal.Context.Types.ThemeScopes'.
  }

-- | The arena: the node arrays, the number of nodes in use, and the solver's
-- working memory. 'newNodeArena' makes one for a context, and
-- 'resetNodeArena' empties it for each run of the view.
data NodeArena = NodeArena
  { naCount :: IORef Int
  -- ^ Nodes in use. The next node gets this index.
  , naCapacity :: IORef Int
  -- ^ Nodes the arrays have room for.
  , naArrays :: IORef NodeArenaArrays
  -- ^ The current arrays. Read them through 'arenaArrays'.
  , naArraysSnap :: IORef (Maybe NodeArenaArrays)
  -- ^ The current arrays again while 'withArenaArraysSnap' runs, and 'Nothing'
  -- the rest of the time.
  , naScratch :: IORef FlexScratch
  -- ^ The solver's buffers for the container it is working on.
  , naSnapLevels :: IORef (MutableArray RealWorld AxisSnapshot)
  -- ^ One 'AxisSnapshot' per nesting depth of the position pass, empty
  -- until the pass first reaches that depth. Laying out a child overwrites
  -- 'naScratch', so a container copies its child list here, at its own depth,
  -- before it recurses into its children.
  , naFrameTag :: IORef Word32
  -- ^ The tag that marks the live entries of 'naWrapMemo' and 'naFitMemo'.
  -- 'resetNodeArena' changes it, which expires every entry at once. That is
  -- enough because a node's text and style do not change between resets. It
  -- is never 0.
  , naWrapMemo :: IORef WidthMemo
  -- ^ For a text node and a wrap width, the width and height of the wrapped
  -- text.
  , naFitMemo :: IORef WidthMemo
  -- ^ For a node and the width it is assigned, the height its content needs.
  -- Only the second value of an entry is used.
  , naEpoch :: IORef Word32
  -- ^ The tag that marks the live entries of 'naIndex'. 'resetNodeArena'
  -- increments it. It is never 0.
  , naIndex :: IORef (BasicHashTable WidgetId Word64)
  -- ^ Node index by widget id, for 'lookupNodeByWidgetId'. A value packs the
  -- epoch it was written in (high 32 bits) with the node index (low 32 bits).
  , naScope :: IORef Int
  -- ^ The paint scope 'addNode' gives new nodes, encoded as in 'naArrScope'.
  , naScopeSig :: IORef Word64
  -- ^ A hash over the index and scope of every node added under a scope other
  -- than 0 since the reset. See 'getScopeSignature'.
  , naInputSig :: IOArr Word64
  -- ^ A hash over every layout input written since the reset, in one unboxed
  -- slot so a mix allocates nothing: each node's layout and links as
  -- 'addNode' wrote them, and every later change through the input setters
  -- ('setNodeText', 'setStyleIdx', 'setOptions', 'setWidgetId'). Solver
  -- outputs, paint state, and geometry are excluded. See 'getInputSignature'.
  , naTextHash :: IORef (IOArr Word64)
  -- ^ Per node, the hash of the text the last 'setNodeText' stored. A node
  -- re-set with the same 'Text' object keeps its hash, so a steady frame
  -- hashes no text bytes.
  , naOptionsHash :: IORef (IOArr Word64)
  -- ^ Per node, the hash of the option list the last 'setOptions' stored,
  -- reused the same way as 'naTextHash'.
  , naOwnHash :: IORef (IOArr Word64)
  -- ^ Per node, the hash of the node's own creation inputs, as 'addNode'
  -- wrote it. 'computeSubtreeHashes' folds these into 'naSubHash'.
  , naSubHash :: IORef (IOArr Word64)
  -- ^ Per node, the key the solve restores its measurement by: a hash of its
  -- own inputs, its ancestors' (text wraps at an ancestor's width, a scroll
  -- bar sits by its panel) and, through the children's keys, every
  -- descendant's. A mismatch against the layout cache marks the node dirty
  -- and propagates to every ancestor, since each ancestor's key covers its
  -- descendants. Written by 'computeSubtreeHashes'.
  , naMeasured :: IORef (IOArr Float)
  -- ^ Per node, the width and height the measure pass took for it (two
  -- floats). 'captureLayoutCache' snapshots it, and a reused solve restores
  -- clean subtrees' measurements from the snapshot instead of measuring
  -- again.
  , naClassNodes :: IORef (IOArr Int)
  -- ^ The node lists of 'NodeClass', one after another: class @c@ keeps its
  -- @i@th node at element @fromEnum c * capacity + i@. A list never holds
  -- more nodes than the arena, so each has room for 'naCapacity' of them.
  , naClassCounts :: IOArr Int
  -- ^ Nodes in each list of 'naClassNodes', by 'fromEnum' of the class.
  }

-- | The solver's buffers for the flow children of one container (its children
-- that are not floating), indexed by the child's position. The solver loads
-- the children, shares the container's spare or missing space among them
-- along its main axis, and reads the result back. One set of buffers serves
-- the whole tree, which is why a container copies what it needs into an
-- 'AxisSnapshot' before it lays out its children. Sizes are in logical pixels.
data FlexScratch = FlexScratch
  { fsCap :: !Int
  -- ^ Children that each array has room for.
  , fsIdx :: !(IOArr Int)
  -- ^ Node index of each child.
  , fsW :: !(IOArr Float)
  -- ^ Width of each child before the sharing: its measured width, or its
  -- percentage of the container's inner width.
  , fsH :: !(IOArr Float)
  -- ^ Height of each child before the sharing.
  , fsOut :: !(IOArr Float)
  -- ^ Size of each child along the main axis after the space is shared out.
  , fsGrow :: !(IOArr Float)
  -- ^ Working space for the sharing: each child's grow factor.
  }

-- | A memo with one entry per node: a width, and two results computed for the
-- node at that width. An entry counts only while its tag equals the arena's
-- 'naFrameTag', so every entry expires at the next 'resetNodeArena'.
-- 'memoizeWidth' reads and writes it.
data WidthMemo = WidthMemo
  { wmTags :: !(IOArr Word32)
  -- ^ Per node, the frame tag its entry was written under. 0 marks an entry
  -- that was never written.
  , wmSlots :: !(IOArr Float)
  -- ^ Per node, @memoStride@ floats: the width, then the two results.
  }

-- | A container's copy of its child list, taken at the container's nesting
-- depth before the position pass recurses into the children and overwrites
-- 'FlexScratch'. Both arrays are indexed by the child's position.
data AxisSnapshot = AxisSnapshot
  { asIdx :: !(IOArr Int)
  -- ^ Node index of each flow child, in the order the view declared them.
  , asOut :: !(IOArr Float)
  -- ^ For a row or column, the size each child gets along the main axis. For
  -- a grid, each child's measured height.
  }

-- | Nodes a new arena has room for. 'ensureCapacity' doubles the capacity when
-- the view adds more.
initialCapacity :: Int
initialCapacity = 256

-- | The width of a node's row in 'naArrGeom', 'naArrStyle', 'naArrTags' and
-- 'naArrTree'. Each is at least the number of columns of 'GeomCol',
-- 'StyleCol', 'TagCol' and 'TreeCol', and the columns past those are unused.
geomStride, styleStride, tagStride, treeStride :: Int
geomStride = 8
styleStride = 16
tagStride = 8
treeStride = 8

-- | Columns of 'naArrGeom', in logical pixels and window coordinates.
--
-- * 'GeomX', 'GeomY', 'GeomW', 'GeomH': the node's rect. The solver's measure
--   pass stores the measured size in 'GeomW' and 'GeomH' with the origin at 0.
--   Its position pass then writes the placed rect. Last,
--   'NanoUI.Internal.Frame.Scroll.applyScrollOffsets' moves the origin by the offsets
--   of the scroll containers around the node.
-- * 'GeomClipX', 'GeomClipY', 'GeomClipW', 'GeomClipH': the node's clip rect,
--   in window coordinates. See 'getClipRect'.
data GeomCol
  = GeomX | GeomY | GeomW | GeomH
  | GeomClipX | GeomClipY | GeomClipW | GeomClipH
  deriving (Enum)

-- | Columns of 'naArrStyle': the numbers the node was laid out with, in
-- logical pixels unless stated.
--
-- * 'StyleWVal', 'StyleHVal': the number of the width or height 'Sizing'. It
--   is the size for 'Fixed', the factor for 'Grow' and 'Shrink', the
--   percentage for 'Percent', and 0 for 'Fit'. 'TagWSizing' and 'TagHSizing'
--   say which.
-- * 'StylePadL', 'StylePadR', 'StylePadT', 'StylePadB': the padding inside the
--   left, right, top and bottom edges.
-- * 'StyleGap': the space between neighbouring children.
-- * 'StyleMinW', 'StyleMinH', 'StyleMaxW', 'StyleMaxH': the size limits. A
--   maximum of 1e8 or more means no limit, and the default layout uses 1e9.
-- * 'StyleScrollContentW': the content width of a scroll container that
--   scrolls both ways, which the solver writes ('getScrollContentW').
-- * 'StyleNodeValue': the node value ('getNodeValue').
-- * 'StyleGridMinColW': the least column width of a grid that fits as many
--   columns as it can, or 0.
-- * 'StyleFontSize': the font size, or 0 for the default ('getNodeFontSize').
--
-- The layout cache does not compare 'StyleScrollContentW' and
-- 'StyleNodeValue', which are not layout inputs.
data StyleCol
  = StyleWVal | StyleHVal
  | StylePadL | StylePadR | StylePadT | StylePadB
  | StyleGap | StyleMinW | StyleMinH | StyleMaxW | StyleMaxH
  | StyleScrollContentW | StyleNodeValue | StyleGridMinColW | StyleFontSize
  deriving (Enum)

-- | Columns of 'naArrTags'. Each holds one enum value as a 'Word8', written
-- with 'writeTagEnum'.
--
-- * 'TagNodeType': the 'NodeType'.
-- * 'TagDirection': the 'DirTag'.
-- * 'TagWSizing', 'TagHSizing': the 'SizingTag' of the width and the height.
-- * 'TagScrollBarSlot': a 'NanoUI.Internal.Font.ScrollBarSlot', which says where a
--   scroll container's bar sits. The solver's measure pass writes it, so the
--   input signature leaves it out and the layout cache restores it on a hit.
-- * 'TagAlignX', 'TagAlignY': the 'AlignX' and the 'AlignY'.
data TagCol
  = TagNodeType | TagDirection | TagWSizing | TagHSizing
  | TagScrollBarSlot | TagAlignX | TagAlignY
  deriving (Enum)

-- | Columns of 'naArrTree'. A link that leads nowhere is -1.
--
-- * 'TreeParent': the parent's index.
-- * 'TreeFirstChild': the child that was added last. 'addNode' puts every new
--   child at the head of its parent's list.
-- * 'TreeNextSibling': the sibling that was added before this node. Following
--   these links therefore visits a node's children from the last to the first.
-- * 'TreeChildCount': the number of children, floating ones included.
-- * 'TreeWidgetId': the 'WidgetId' converted to an 'Int', or 0 for none.
-- * 'TreeStyleIdx': the style index ('getStyleIdx').
-- * 'TreeTextIdx': the node's slot in 'naArrTextStore', which is its own
--   index, or -1 when the node has no text.
-- * 'TreeGridCols': the column count of a grid, or 0.
data TreeCol
  = TreeParent | TreeFirstChild | TreeNextSibling | TreeChildCount
  | TreeWidgetId | TreeStyleIdx | TreeTextIdx | TreeGridCols
  deriving (Enum)

-- | Read geometry column @col@ of node @idx@. Like the other raw accessors
-- below, it takes the arrays so that a loop can fetch them once with
-- 'arenaArrays', and it does not check that @idx@ is below 'arenaCount'.
{-# INLINE readGeom #-}
readGeom :: NodeArenaArrays -> NodeIdx -> GeomCol -> IO Float
readGeom a idx col = readPrimArray (naArrGeom a) (idx * geomStride + fromEnum col)

-- | Write geometry column @col@ of node @idx@.
{-# INLINE writeGeom #-}
writeGeom :: NodeArenaArrays -> NodeIdx -> GeomCol -> Float -> IO ()
writeGeom a idx col = writePrimArray (naArrGeom a) (idx * geomStride + fromEnum col)

-- | Read style column @col@ of node @idx@.
{-# INLINE readStyle #-}
readStyle :: NodeArenaArrays -> NodeIdx -> StyleCol -> IO Float
readStyle a idx col = readPrimArray (naArrStyle a) (idx * styleStride + fromEnum col)

-- | Write style column @col@ of node @idx@.
{-# INLINE writeStyle #-}
writeStyle :: NodeArenaArrays -> NodeIdx -> StyleCol -> Float -> IO ()
writeStyle a idx col = writePrimArray (naArrStyle a) (idx * styleStride + fromEnum col)

-- | Read tag column @col@ of node @idx@ and decode it with 'toEnum'. The
-- caller picks the result type, which must be the type the column was written
-- with: 'NodeType' for 'TagNodeType', 'DirTag' for 'TagDirection', and so on.
{-# INLINE readTagEnum #-}
readTagEnum :: Enum e => NodeArenaArrays -> NodeIdx -> TagCol -> IO e
readTagEnum a idx col = do
  t <- readPrimArray (naArrTags a) (idx * tagStride + fromEnum col)
  pure $! toEnum (fromIntegral t)

-- | Write an enum value as one byte. Its 'fromEnum' value must be in 0-255
-- and must use the type expected by the column.
{-# INLINE writeTagEnum #-}
writeTagEnum :: Enum e => NodeArenaArrays -> NodeIdx -> TagCol -> e -> IO ()
writeTagEnum a idx col v = writePrimArray (naArrTags a) (idx * tagStride + fromEnum col) (fromIntegral (fromEnum v))

-- | Read tree column @col@ of node @idx@.
{-# INLINE readTree #-}
readTree :: NodeArenaArrays -> NodeIdx -> TreeCol -> IO Int
readTree a idx col = readPrimArray (naArrTree a) (idx * treeStride + fromEnum col)

-- | Write tree column @col@ of node @idx@. Writing a link column by hand can
-- leave the child lists and the child counts inconsistent.
{-# INLINE writeTree #-}
writeTree :: NodeArenaArrays -> NodeIdx -> TreeCol -> Int -> IO ()
writeTree a idx col = writePrimArray (naArrTree a) (idx * treeStride + fromEnum col)

-- | Arrays with room for @cap@ nodes. The primitive arrays start
-- uninitialised: 'addNode' fills in a node's elements when it adds the node.
newNodeArenaArrays :: Int -> IO NodeArenaArrays
newNodeArenaArrays cap = do
  naArrGeom <- newPrimArray (cap * geomStride)
  naArrStyle <- newPrimArray (cap * styleStride)
  naArrTags <- newPrimArray (cap * tagStride)
  naArrTree <- newPrimArray (cap * treeStride)
  naArrTextStore <- newArray cap T.empty
  naArrOptionsStore <- newArray cap []
  naArrFontColor <- newPrimArray cap
  naArrScope <- newPrimArray cap
  pure NodeArenaArrays {..}

-- | Uninitialised solver buffers with room for @fsCap@ children.
newFlexScratch :: Int -> IO FlexScratch
newFlexScratch fsCap = do
  fsIdx <- newPrimArray fsCap
  fsW <- newPrimArray fsCap
  fsH <- newPrimArray fsCap
  fsOut <- newPrimArray fsCap
  fsGrow <- newPrimArray fsCap
  pure FlexScratch {..}

-- | Floats per node in 'wmSlots': the width and the two results.
memoStride :: Int
memoStride = 3

-- | An empty memo for @cap@ nodes. Every tag starts at 0 and 'naFrameTag' is
-- never 0, so a lookup cannot hit an entry that was never written.
newWidthMemo :: Int -> IO WidthMemo
newWidthMemo cap = do
  wmTags <- newPrimArray cap
  setPrimArray wmTags 0 cap 0
  wmSlots <- newPrimArray (cap * memoStride)
  pure WidthMemo {..}

-- | An empty arena. It starts with room for 256 nodes (@initialCapacity@), 64
-- children of one container in the solver's buffers, and 256 nesting depths.
-- All three grow when a view needs more.
newNodeArena :: IO NodeArena
newNodeArena = do
  let cap = initialCapacity
  naCount <- newIORef 0
  naCapacity <- newIORef cap
  naArrays <- newIORef =<< newNodeArenaArrays cap
  naArraysSnap <- newIORef Nothing
  naScratch <- newIORef =<< newFlexScratch 64
  naSnapLevels <- newIORef =<< newArray 256 =<< newAxisSnapshot 0
  naFrameTag <- newIORef 1
  naWrapMemo <- newIORef =<< newWidthMemo cap
  naFitMemo <- newIORef =<< newWidthMemo cap
  naEpoch <- newIORef 1
  naIndex <- newIORef =<< HT.new
  naScope <- newIORef 0
  naScopeSig <- newIORef 0
  naInputSig <- newPrimArray 1
  writePrimArray naInputSig 0 0
  -- Zeroed: the stores start as the shared 'T.empty' and '[]', which pass the
  -- same-object check, and 0 marks a hash that was never taken.
  naTextHash <- newIORef =<< newZeroedPrimArray cap
  naOptionsHash <- newIORef =<< newZeroedPrimArray cap
  naOwnHash <- newIORef =<< newPrimArray cap
  naSubHash <- newIORef =<< newPrimArray cap
  naMeasured <- newIORef =<< newPrimArray (cap * 2)
  naClassNodes <- newIORef =<< newPrimArray (cap * nodeClassCount)
  naClassCounts <- newPrimArray nodeClassCount
  setPrimArray naClassCounts 0 nodeClassCount 0
  pure NodeArena {..}

nodeClassCount :: Int
nodeClassCount = fromEnum (maxBound :: NodeClass) + 1

newZeroedPrimArray :: Int -> IO (IOArr Word64)
newZeroedPrimArray n = do
  arr <- newPrimArray n
  setPrimArray arr 0 n 0
  pure arr

-- | Begin an empty frame while retaining array capacity. Invalidates node
-- indices, width memos, and widget-id lookups, and resets paint-scope and
-- floating-panel state. 'addNode' initialises each reused slot.
resetNodeArena :: NodeArena -> IO ()
resetNodeArena na = do
  writeIORef (naCount na) 0
  writeIORef (naScope na) 0
  writeIORef (naScopeSig na) 0
  writePrimArray (naInputSig na) 0 0
  setPrimArray (naClassCounts na) 0 nodeClassCount 0
  -- 0 marks a memo entry that was never written, so the tag wraps to 1.
  !ft <- readIORef (naFrameTag na)
  writeIORef (naFrameTag na) (if ft == maxBound then 1 else ft + 1)
  -- Lookups reject entries from other epochs. Replacing the table every 128
  -- epochs bounds retained stale ids without allocating a table every frame.
  !ep <- readIORef (naEpoch na)
  let !ep' = ep + 1
  writeIORef (naEpoch na) (if ep' == 0 then 1 else ep')
  when (ep' .&. 0x7F == 0) $ writeIORef (naIndex na) =<< HT.new

-- | The topmost (last added) modal node, if any.
{-# INLINE topModalNode #-}
topModalNode :: NodeArena -> IO (Maybe NodeIdx)
topModalNode na = findClassNodeRevM na FloatingNodes (fmap (== NodeModal) . getNodeType na)

-- | Fold a tagged value into a running hash. Tags separate the fields so a
-- value moving between fields of one node changes the hash.
{-# INLINE mixTagged #-}
mixTagged :: Word64 -> Word64 -> Word64 -> Word64
mixTagged acc tag v = (acc * 0x9E3779B97F4A7C15) `xor` (tag * 0x100000001b3 `xor` v)

-- | Fold one input's tag and value into 'naInputSig'.
{-# INLINE mixInputSig #-}
mixInputSig :: NodeArena -> Word64 -> Word64 -> IO ()
mixInputSig na tag v = do
  sig <- readPrimArray (naInputSig na) 0
  writePrimArray (naInputSig na) 0 (mixTagged sig tag v)

-- | Fold a post-creation input change into the node's own hash and the
-- frame's input signature. The subtree hash a later solve compares against
-- is built from the own hashes, so a change must reach both.
{-# INLINE mixNodeInput #-}
mixNodeInput :: NodeArena -> NodeIdx -> Word64 -> Word64 -> IO ()
mixNodeInput na idx tag v = do
  own <- readIORef (naOwnHash na)
  o <- readPrimArray own idx
  writePrimArray own idx (mixTagged o tag v)
  mixInputSig na tag v

-- | Number of modal, window, and popup nodes added since the last reset.
{-# INLINE floatingNodeCount #-}
floatingNodeCount :: NodeArena -> IO Int
floatingNodeCount na = readPrimArray (naClassCounts na) (fromEnum FloatingNodes)

-- | Live node count. Valid indices are 0 through count minus one.
{-# INLINE arenaCount #-}
arenaCount :: NodeArena -> IO Int
arenaCount na = readIORef (naCount na)

-- | Current backing arrays. Do not retain them across arena growth or reset.
{-# INLINE arenaArrays #-}
arenaArrays :: NodeArena -> IO NodeArenaArrays
arenaArrays na = do
  m <- readIORef (naArraysSnap na)
  case m of
    Just a -> pure a
    Nothing -> readIORef (naArrays na)

-- | Cache the current array references during a pass. This does not pin memory
-- for FFI use; it avoids rereading 'naArrays' in each accessor. Do not nest calls.
withArenaArraysSnap :: NodeArena -> IO a -> IO a
withArenaArraysSnap na act =
  bracket_
    (readIORef (naArrays na) >>= writeIORef (naArraysSnap na) . Just)
    (writeIORef (naArraysSnap na) Nothing)
    act

{-# NOINLINE ensureCapacity #-}
ensureCapacity :: NodeArena -> Int -> IO ()
ensureCapacity na needed = do
  cap <- readIORef (naCapacity na)
  when (needed >= cap) $ do
    let newCap = cap * 2
    newA <- readIORef (naArrays na) >>= growNodeArenaArrays cap newCap
    growWidthMemo (naWrapMemo na) cap newCap
    growWidthMemo (naFitMemo na) cap newCap
    writeIORef (naArrays na) newA
    let growRef r old new = readIORef r >>= \a -> writeIORef r =<< growPrimArrayCopy a old new 0
    growRef (naTextHash na) cap newCap
    growRef (naOptionsHash na) cap newCap
    growRef (naOwnHash na) cap newCap
    growRef (naSubHash na) cap newCap
    growRef (naMeasured na) (cap * 2) (newCap * 2)
    -- Each class list starts at a multiple of the capacity, so the lists move
    -- apart as it grows.
    oldClass <- readIORef (naClassNodes na)
    newClass <- newPrimArray (newCap * nodeClassCount)
    forM_ [0 .. nodeClassCount - 1] $ \c -> do
      k <- readPrimArray (naClassCounts na) c
      copyMutablePrimArray newClass (c * newCap) oldClass (c * cap) k
    writeIORef (naClassNodes na) newClass
    readIORef (naArraysSnap na) >>= mapM_ (\_ -> writeIORef (naArraysSnap na) (Just newA))
    writeIORef (naCapacity na) newCap

-- | Copy of @a@ with room for @newCap@ nodes; new slots are zero or empty.
growNodeArenaArrays :: Int -> Int -> NodeArenaArrays -> IO NodeArenaArrays
growNodeArenaArrays cap newCap a = do
  naArrGeom <- growPrimArrayCopy (naArrGeom a) (cap * geomStride) (newCap * geomStride) 0
  naArrStyle <- growPrimArrayCopy (naArrStyle a) (cap * styleStride) (newCap * styleStride) 0
  naArrTags <- growPrimArrayCopy (naArrTags a) (cap * tagStride) (newCap * tagStride) 0
  naArrTree <- growPrimArrayCopy (naArrTree a) (cap * treeStride) (newCap * treeStride) 0
  naArrTextStore <- growBoxedStoreCopy T.empty (naArrTextStore a) cap newCap
  naArrOptionsStore <- growBoxedStoreCopy [] (naArrOptionsStore a) cap newCap
  naArrFontColor <- growPrimArrayCopy (naArrFontColor a) cap newCap 0
  naArrScope <- growPrimArrayCopy (naArrScope a) cap newCap 0
  pure NodeArenaArrays {..}

{-# NOINLINE growPrimArrayCopy #-}
growPrimArrayCopy :: Prim a => IOArr a -> Int -> Int -> a -> IO (IOArr a)
growPrimArrayCopy oldArr cap newCap defVal = do
  newArr <- resizeMutablePrimArray oldArr newCap
  setPrimArray newArr cap (newCap - cap) defVal
  pure newArr

growWidthMemo :: IORef WidthMemo -> Int -> Int -> IO ()
growWidthMemo ref cap newCap = do
  WidthMemo tags slots <- readIORef ref
  wmTags <- growPrimArrayCopy tags cap newCap 0
  wmSlots <- growPrimArrayCopy slots (cap * memoStride) (newCap * memoStride) 0
  writeIORef ref WidthMemo {..}

{-# NOINLINE growBoxedStoreCopy #-}
growBoxedStoreCopy :: a -> MutableArray RealWorld a -> Int -> Int -> IO (MutableArray RealWorld a)
growBoxedStoreCopy emptyVal arr oldCap newCap = do
  newArr <- newArray newCap emptyVal
  copyMutableArray newArr 0 arr 0 oldCap
  pure newArr

{-# INLINE sizingTag #-}
sizingTag :: Sizing -> (SizingTag, Float)
sizingTag (Fixed v) = (SizingFixed, v)
sizingTag Fit = (SizingFit, 0)
sizingTag (Grow g) = (SizingGrow, g)
sizingTag (Shrink s) = (SizingShrink, s)
sizingTag (Percent p) = (SizingPercent, p)

-- | Keep a non-negative parent; otherwise attach to node 0 if it exists, or
-- return -1 for an empty arena. This keeps top-level floating nodes reachable
-- from the page root.
rootAttachParent :: NodeArena -> Int -> IO Int
rootAttachParent na parent
  | parent >= 0 = pure parent
  | otherwise = do
      n <- arenaCount na
      pure (if n > 0 then 0 else -1)

-- | Append a node laid out by the given 'Layout' and link it at the head of
-- its parent's child list. The parent must be -1 or an existing node index.
-- The caller assigns widget identity, text and type-specific style data.
{-# INLINE addNode #-}
addNode :: NodeArena -> NodeType -> Int -> Layout -> IO NodeIdx
addNode na nt parent Layout {..} = do
  idx <- readIORef (naCount na)
  ensureCapacity na (idx + 1)
  let (wTag, wVal) = sizingTag layoutWidth
      (hTag, hVal) = sizingTag layoutHeight
      pad = layoutPadding
  a <- arenaArrays na

  setPrimArray (naArrGeom a) (idx * geomStride) geomStride 0

  writeStyle a idx StyleWVal wVal
  writeStyle a idx StyleHVal hVal
  writeStyle a idx StylePadL (padL pad)
  writeStyle a idx StylePadR (padR pad)
  writeStyle a idx StylePadT (padT pad)
  writeStyle a idx StylePadB (padB pad)
  writeStyle a idx StyleGap layoutGap
  writeStyle a idx StyleMinW layoutMinW
  writeStyle a idx StyleMinH layoutMinH
  writeStyle a idx StyleMaxW layoutMaxW
  writeStyle a idx StyleMaxH layoutMaxH
  let !valuesOff = fromEnum StyleScrollContentW
  setPrimArray (naArrStyle a) (idx * styleStride + valuesOff) (styleStride - valuesOff) 0
  writeStyle a idx StyleGridMinColW layoutGridMinColW
  writeStyle a idx StyleFontSize layoutFontSize

  setPrimArray (naArrTags a) (idx * tagStride) tagStride 0
  writeTagEnum a idx TagNodeType nt
  writeTagEnum a idx TagDirection $ case layoutDirection of
    Row -> DirRow
    Column -> DirColumn
  writeTagEnum a idx TagWSizing wTag
  writeTagEnum a idx TagHSizing hTag
  writeTagEnum a idx TagAlignX layoutAlignX
  writeTagEnum a idx TagAlignY layoutAlignY

  setPrimArray (naArrTree a) (idx * treeStride) treeStride 0
  writeTree a idx TreeParent parent
  writeTree a idx TreeFirstChild (-1)
  writeTree a idx TreeNextSibling (-1)
  writeTree a idx TreeTextIdx (-1)
  writeTree a idx TreeGridCols layoutGridCols

  -- Fold this node's creation inputs into the frame's input signature, the
  -- O(1) successor of comparing every column at reuse time. Values are the
  -- ones already in registers above.
  let !nodeSig =
        foldl'
          (\acc (t, v) -> mixTagged acc t v)
          (fromIntegral idx `shiftL` 32 .|. fromIntegral (idx + 1) :: Word64)
          [ (0x4e54, fromIntegral (fromEnum nt))
          , (0x4449, fromIntegral (fromEnum layoutDirection))
          , (0x5754, fromIntegral (fromEnum wTag))
          , (0x5746, fromIntegral (castFloatToWord32 wVal))
          , (0x4854, fromIntegral (fromEnum hTag))
          , (0x4846, fromIntegral (castFloatToWord32 hVal))
          , (0x504c, fromIntegral (castFloatToWord32 (padL pad)))
          , (0x5052, fromIntegral (castFloatToWord32 (padR pad)))
          , (0x5054, fromIntegral (castFloatToWord32 (padT pad)))
          , (0x5042, fromIntegral (castFloatToWord32 (padB pad)))
          , (0x4741, fromIntegral (castFloatToWord32 layoutGap))
          , (0x4d57, fromIntegral (castFloatToWord32 layoutMinW))
          , (0x4d48, fromIntegral (castFloatToWord32 layoutMinH))
          , (0x5857, fromIntegral (castFloatToWord32 layoutMaxW))
          , (0x5848, fromIntegral (castFloatToWord32 layoutMaxH))
          , (0x4743, fromIntegral layoutGridCols)
          , (0x474d, fromIntegral (castFloatToWord32 layoutGridMinColW))
          , (0x4648, fromIntegral (castFloatToWord32 layoutFontSize))
          , (0x4158, fromIntegral (fromEnum layoutAlignX))
          , (0x4159, fromIntegral (fromEnum layoutAlignY))
          , (0x5041, fromIntegral (parent + 1))
          ]
  mixInputSig na 0x4e4f nodeSig
  ownA <- readIORef (naOwnHash na)
  writePrimArray ownA idx nodeSig

  -- The font colour is paint state, so the signature leaves it out.
  writePrimArray (naArrFontColor a) idx $ case layoutFontColor of
    Nothing -> 0
    Just (Color w) -> 0x100000000 .|. fromIntegral w
  scope <- readIORef (naScope na)
  writePrimArray (naArrScope a) idx scope
  when (scope /= 0) $ do
    sig <- readIORef (naScopeSig na)
    writeIORef (naScopeSig na) $! (sig * 0x100000001b3) `xor` (fromIntegral idx `shiftL` 32 .|. fromIntegral scope)
  writeArray (naArrOptionsStore a) idx []

  when (parent >= 0) $ do
    fc <- readTree a parent TreeFirstChild
    writeTree a idx TreeNextSibling fc
    writeTree a parent TreeFirstChild idx
    cc <- readTree a parent TreeChildCount
    writeTree a parent TreeChildCount (cc + 1)
  when (isFloatingNode nt) $ pushClassNode na FloatingNodes idx
  when (isWidgetNode nt || isScrollNode nt) $ do
    pushClassNode na PointerNodes idx
    when (nt == NodeCheckbox || nt == NodeRadio || nt == NodeTree) $
      pushClassNode na SelectionNodes idx
    when (nt == NodeDrawing) $ pushClassNode na DrawingNodes idx
  writeIORef (naCount na) (idx + 1)
  pure idx

-- | Append node @idx@ to the list of class @c@. 'addNode' has made room.
{-# INLINE pushClassNode #-}
pushClassNode :: NodeArena -> NodeClass -> NodeIdx -> IO ()
pushClassNode na c idx = do
  let ci = fromEnum c
  k <- readPrimArray (naClassCounts na) ci
  cap <- readIORef (naCapacity na)
  arr <- readIORef (naClassNodes na)
  writePrimArray arr (ci * cap + k) idx
  writePrimArray (naClassCounts na) ci (k + 1)

-- | 'addNode' as a call instead of inlined, for the view's many call sites.
addNodeFromLayout :: NodeArena -> NodeType -> Int -> Layout -> IO NodeIdx
addNodeFromLayout na nt parent l = addNode na nt parent l

-- | Assign text to a live node and mark its text slot as present. Re-setting
-- the same 'Text' object reuses its cached hash, so a steady frame hashes no
-- text bytes.
{-# INLINE setNodeText #-}
setNodeText :: NodeArena -> NodeIdx -> Text -> IO ()
setNodeText na idx txt = do
  a <- arenaArrays na
  old <- readArray (naArrTextStore a) idx
  h <- cachedHash (naTextHash na) idx (old `ptrEq` txt) 0x54455854 txt
  writeArray (naArrTextStore a) idx txt
  writeTree a idx TreeTextIdx idx
  mixNodeInput na idx 0x5458 h

-- | Parent index, or -1 for a root.
{-# INLINE getParent #-}
getParent :: NodeArena -> NodeIdx -> IO NodeIdx
getParent na idx = arenaArrays na >>= \a -> readTree a idx TreeParent

-- | Most recently added direct child, or -1 when there are none.
{-# INLINE getFirstChild #-}
getFirstChild :: NodeArena -> NodeIdx -> IO NodeIdx
getFirstChild na idx = arenaArrays na >>= \a -> readTree a idx TreeFirstChild

-- | Next sibling in reverse declaration order, or -1 at the end.
{-# INLINE getNextSibling #-}
getNextSibling :: NodeArena -> NodeIdx -> IO NodeIdx
getNextSibling na idx = arenaArrays na >>= \a -> readTree a idx TreeNextSibling

-- | Node kind assigned at insertion, which selects layout and paint behaviour.
{-# INLINE getNodeType #-}
getNodeType :: NodeArena -> NodeIdx -> IO NodeType
getNodeType na idx = arenaArrays na >>= \a -> readTagEnum a idx TagNodeType

-- | Main layout axis stored on the node.
{-# INLINE getDirection #-}
getDirection :: NodeArena -> NodeIdx -> IO DirTag
getDirection na idx = arenaArrays na >>= \a -> readTagEnum a idx TagDirection

-- | One axis of a node's layout constraints: the sizing mode, its number (see
-- 'StyleWVal' for units), and the minimum and maximum size in logical pixels.
data AxisSizing = AxisSizing
  { axTag :: !SizingTag
  , axVal :: !Float
  , axMin :: !Float
  , axMax :: !Float
  }

-- | The width's 'AxisSizing' with @horizontal@, else the height's.
{-# INLINE readAxisSizing #-}
readAxisSizing :: NodeArenaArrays -> NodeIdx -> Bool -> IO AxisSizing
readAxisSizing a idx horizontal = do
  AxisSizing
    <$> readTagEnum a idx (if horizontal then TagWSizing else TagHSizing)
    <*> readStyle a idx (if horizontal then StyleWVal else StyleHVal)
    <*> readStyle a idx (if horizontal then StyleMinW else StyleMinH)
    <*> readStyle a idx (if horizontal then StyleMaxW else StyleMaxH)

-- | The width's 'AxisSizing'.
{-# INLINE getWidthSizing #-}
getWidthSizing :: NodeArena -> NodeIdx -> IO AxisSizing
getWidthSizing na idx = arenaArrays na >>= \a -> readAxisSizing a idx True

-- | The height's 'AxisSizing'.
{-# INLINE getHeightSizing #-}
getHeightSizing :: NodeArena -> NodeIdx -> IO AxisSizing
getHeightSizing na idx = arenaArrays na >>= \a -> readAxisSizing a idx False

-- | Insets in logical pixels, ordered left, right, top, bottom.
{-# INLINE getPadding #-}
getPadding :: NodeArena -> NodeIdx -> IO Padding
getPadding na idx = do
  a <- arenaArrays na
  Padding <$> readStyle a idx StylePadL <*> readStyle a idx StylePadR <*> readStyle a idx StylePadT <*> readStyle a idx StylePadB

-- | Solved horizontal content extent of a two-axis scroller, in logical pixels.
{-# INLINE getScrollContentW #-}
getScrollContentW :: NodeArena -> NodeIdx -> IO Float
getScrollContentW na idx = arenaArrays na >>= \a -> readStyle a idx StyleScrollContentW

-- | Store the solver's horizontal content extent for a scroller.
{-# INLINE setScrollContentW #-}
setScrollContentW :: NodeArena -> NodeIdx -> Float -> IO ()
setScrollContentW na idx v = arenaArrays na >>= \a -> writeStyle a idx StyleScrollContentW v

-- | Whether the parent has row direction. A root returns 'False'.
{-# INLINE parentIsRow #-}
parentIsRow :: NodeArena -> NodeIdx -> IO Bool
parentIsRow na idx = do
  p <- getParent na idx
  if p < 0 then pure False else (== DirRow) <$> getDirection na p

-- | Horizontal alignment requested by the node.
{-# INLINE getAlignX #-}
getAlignX :: NodeArena -> NodeIdx -> IO AlignX
getAlignX na idx = arenaArrays na >>= \a -> readTagEnum a idx TagAlignX

-- | Vertical alignment requested by the node.
{-# INLINE getAlignY #-}
getAlignY :: NodeArena -> NodeIdx -> IO AlignY
getAlignY na idx = arenaArrays na >>= \a -> readTagEnum a idx TagAlignY

-- | Current x, y, width, height in logical pixels. After scroll offsets are
-- applied, the origin is in window coordinates; before layout it is unset.
{-# INLINE getRect #-}
getRect :: NodeArena -> NodeIdx -> IO (Float, Float, Float, Float)
getRect na idx = do
  a <- arenaArrays na
  (,,,) <$> readGeom a idx GeomX <*> readGeom a idx GeomY <*> readGeom a idx GeomW <*> readGeom a idx GeomH

-- | 'getRect' as a 'Rect'.
{-# INLINE getNodeRect #-}
getNodeRect :: NodeArena -> NodeIdx -> IO Rect
getNodeRect na idx = do
  a <- arenaArrays na
  Rect <$> readGeom a idx GeomX <*> readGeom a idx GeomY <*> readGeom a idx GeomW <*> readGeom a idx GeomH

-- | Write x, y, width, and height. Does not update the saved layout origin or clip.
{-# INLINE setRect #-}
setRect :: NodeArena -> NodeIdx -> Float -> Float -> Float -> Float -> IO ()
setRect na idx x y w h = do
  a <- arenaArrays na
  writeGeom a idx GeomX x
  writeGeom a idx GeomY y
  writeGeom a idx GeomW w
  writeGeom a idx GeomH h

-- | Positive-area clip in logical window coordinates. 'Nothing' means the
-- stored clip is empty or unset; those cases share the same representation.
{-# INLINE getClipRect #-}
getClipRect :: NodeArena -> NodeIdx -> IO (Maybe Rect)
getClipRect na idx = do
  a <- arenaArrays na
  x <- readGeom a idx GeomClipX
  y <- readGeom a idx GeomClipY
  w <- readGeom a idx GeomClipW
  h <- readGeom a idx GeomClipH
  pure (if w > 0 && h > 0 then Just (Rect x y w h) else Nothing)

-- | Store a clip in logical window coordinates. Empty clips read back as 'Nothing'.
{-# INLINE setClipRect #-}
setClipRect :: NodeArena -> NodeIdx -> Rect -> IO ()
setClipRect na idx (Rect x y w h) = do
  a <- arenaArrays na
  writeGeom a idx GeomClipX x
  writeGeom a idx GeomClipY y
  writeGeom a idx GeomClipW w
  writeGeom a idx GeomClipH h

-- | Cached layout signature and solved geometry for whole-layout reuse. The
-- backing arrays are reused; only cache misses capture a new solved frame.
data LayoutCache = LayoutCache
  { lcCap :: !Int
  , lcCount :: !Int
  , lcSig :: !Word64
  -- ^ The input signature ('getInputSignature') the solve was captured with.
  , lcMeasures :: !(IntMap CustomMeasureRecord)
  -- ^ Per custom-measured node, what the solve's measure recorded.
  -- Frame owns checking these: a measure may read state outside the arena,
  -- which the input signature cannot see. Empty when no node measured
  -- itself custom, so the common frame pays nothing.
  , lcMeasureHooks :: !IntSet
  -- ^ The widgets with a custom measure registered at capture. Frame checks
  -- the set is unchanged, since gaining a hook changes a node's size without
  -- touching the arena.
  , lcSub :: !(IOArr Word64)
  -- ^ The restore keys ('naSubHash') of the captured frame. A node whose
  -- current key matches this was measured the same way last solve, and the
  -- solve restores its measured size instead of measuring again.
  , lcMeasured :: !(IOArr Float)
  -- ^ Per captured node, the width and height its measure took (two floats).
  , lcGeom :: !(IOArr Float)
  -- ^ The captured 'naArrGeom'.
  , lcStyle :: !(IOArr Float)
  -- ^ The captured 'naArrStyle'. A restore reads only the columns the solver
  -- writes.
  , lcTags :: !(IOArr Word8)
  -- ^ The captured 'naArrTags'. A restore reads only 'TagScrollBarSlot'.
  }

-- | A custom measure's (available width, available height, measured width,
-- measured height), as raw float words.
type CustomMeasureRecord = (Word32, Word32, Word32, Word32)

-- | Empty layout cache with at least 16 slots. Capture a solved frame before reuse.
newLayoutCache :: Int -> IO LayoutCache
newLayoutCache cap0 = do
  let !cap = max 16 cap0
  lcSub <- newPrimArray cap
  lcMeasured <- newPrimArray (cap * 2)
  lcGeom <- newPrimArray (cap * geomStride)
  lcStyle <- newPrimArray (cap * styleStride)
  lcTags <- newPrimArray (cap * tagStride)
  pure (LayoutCache cap 0 0 IM.empty IS.empty lcSub lcMeasured lcGeom lcStyle lcTags)

-- | Snapshot the current (post-solve) arena form, constraints and rects.
captureLayoutCache :: NodeArena -> LayoutCache -> IO LayoutCache
captureLayoutCache na lc0 = do
  n <- arenaCount na
  sig <- getInputSignature na
  subA <- readIORef (naSubHash na)
  measuredA <- readIORef (naMeasured na)
  -- The copies below overwrite everything a capture reads, so a cache too
  -- small for the arena is replaced rather than grown.
  lc <- if n <= lcCap lc0 then pure lc0 else newLayoutCache (max n (lcCap lc0 * 2))
  a <- arenaArrays na
  copyMutablePrimArray (lcGeom lc) 0 (naArrGeom a) 0 (n * geomStride)
  copyMutablePrimArray (lcStyle lc) 0 (naArrStyle a) 0 (n * styleStride)
  copyMutablePrimArray (lcTags lc) 0 (naArrTags a) 0 (n * tagStride)
  copyMutablePrimArray (lcSub lc) 0 subA 0 n
  copyMutablePrimArray (lcMeasured lc) 0 measuredA 0 (n * 2)
  pure lc {lcCount = n, lcSig = sig}

-- | Whether the frame's layout inputs hash to what the cache captured.
layoutSigMatches :: NodeArena -> LayoutCache -> IO Bool
layoutSigMatches na lc = do
  n <- arenaCount na
  if n <= 0 || n /= lcCount lc
    then pure False
    else (== lcSig lc) <$> getInputSignature na

-- | Restore only solver outputs. Rebuilt paint values/colors must survive a
-- cache hit; copying the entire cached style array would revert them.
restoreLayoutCache :: NodeArena -> LayoutCache -> IO ()
restoreLayoutCache na lc = do
  a <- arenaArrays na
  let !n = lcCount lc
  copyMutablePrimArray (naArrGeom a) 0 (lcGeom lc) 0 (n * geomStride)
  -- Scroll content width, node value (the content height) and scrollbar
  -- slot. Scroll containers are pointer nodes, so walk that class, not the
  -- whole arena.
  forClassNodes_ na PointerNodes $ \i -> do
    nt <- readTagEnum a i TagNodeType
    when (isScrollNode nt) $ do
      let !off = i * styleStride + fromEnum StyleScrollContentW
          !slotOff = i * tagStride + fromEnum TagScrollBarSlot
      copyMutablePrimArray (naArrStyle a) off (lcStyle lc) off 2
      readPrimArray (lcTags lc) slotOff >>= writePrimArray (naArrTags a) slotOff

-- | Node text, or empty text when no text was assigned this frame.
{-# INLINE getText #-}
getText :: NodeArena -> NodeIdx -> IO Text
getText na idx = do
  a <- arenaArrays na
  ti <- readTree a idx TreeTextIdx
  if ti < 0
    then pure T.empty
    else readArray (naArrTextStore a) ti

-- | Choices stored on a select node, or an empty list when none were assigned.
{-# INLINE getOptions #-}
getOptions :: NodeArena -> NodeIdx -> IO [Text]
getOptions na idx = arenaArrays na >>= \a -> readArray (naArrOptionsStore a) idx

-- | Replace a node's choice labels for this frame. Re-setting the same list
-- object reuses its cached hash, like 'setNodeText'.
{-# INLINE setOptions #-}
setOptions :: NodeArena -> NodeIdx -> [Text] -> IO ()
setOptions na idx opts = do
  a <- arenaArrays na
  old <- readArray (naArrOptionsStore a) idx
  h <- cachedHash (naOptionsHash na) idx (old `ptrEq` opts) 0x4f505453 opts
  writeArray (naArrOptionsStore a) idx opts
  mixNodeInput na idx 0x4f50 h

-- | The node's value hash from @ref@ when @same@ says the value is the
-- object last hashed there, else a fresh hash of @x@, salted and cached.
{-# INLINE cachedHash #-}
cachedHash :: Hashable a => IORef (IOArr Word64) -> NodeIdx -> Bool -> Word64 -> a -> IO Word64
cachedHash ref idx same salt x = do
  arr <- readIORef ref
  c <- readPrimArray arr idx
  if same && c /= 0
    then pure c
    else do
      let !h = fromIntegral (hash x) `xor` salt
      h <$ writePrimArray arr idx h

-- | Identity assigned to the node, or @WidgetId 0@ for an untagged node.
{-# INLINE getWidgetId #-}
getWidgetId :: NodeArena -> NodeIdx -> IO WidgetId
getWidgetId na idx = arenaArrays na >>= \a -> WidgetId . fromIntegral <$> readTree a idx TreeWidgetId

-- | Assign a node's identity and index nonzero ids for lookup. Assign once per
-- node: this does not remove a mapping previously stored under another id.
{-# INLINE setWidgetId #-}
setWidgetId :: NodeArena -> NodeIdx -> WidgetId -> IO ()
setWidgetId na idx wid = do
  a <- arenaArrays na
  let WidgetId w = wid
  writeTree a idx TreeWidgetId (fromIntegral w)
  mixNodeInput na idx 0x5749 w
  when (hashWidgetId wid /= 0) $ do
    !ep <- readIORef (naEpoch na)
    table <- readIORef (naIndex na)
    HT.insert table wid (fromIntegral ep `shiftL` 32 .|. (fromIntegral idx .&. 0xFFFFFFFF))

-- | Node most recently indexed under this id in the current frame. Returns
-- 'Nothing' for zero, an unknown id, or an entry from an earlier frame.
{-# INLINE lookupNodeByWidgetId #-}
lookupNodeByWidgetId :: NodeArena -> WidgetId -> IO (Maybe NodeIdx)
lookupNodeByWidgetId na wid
  | hashWidgetId wid == 0 = pure Nothing
  | otherwise = do
      table <- readIORef (naIndex na)
      mVal <- HT.lookup table wid
      case mVal of
        Nothing -> pure Nothing
        Just val -> do
          !ep <- readIORef (naEpoch na)
          let !idx = fromIntegral (val .&. 0xFFFFFFFF)
          pure (if val `shiftR` 32 == fromIntegral ep then Just idx else Nothing)

-- | 'lookupNodeByWidgetId' using the id's integer store key.
{-# INLINE lookupNodeByKey #-}
lookupNodeByKey :: NodeArena -> Int -> IO (Maybe NodeIdx)
lookupNodeByKey na key = lookupNodeByWidgetId na (WidgetId (fromIntegral key))

-- | Type-specific numeric value, such as selection state or a scroller's
-- solved content height. Interpret it according to 'getNodeType'.
{-# INLINE getNodeValue #-}
getNodeValue :: NodeArena -> NodeIdx -> IO Float
getNodeValue na idx = arenaArrays na >>= \a -> readStyle a idx StyleNodeValue

-- | Set the type-specific numeric value read by the solver or painter.
{-# INLINE setNodeValue #-}
setNodeValue :: NodeArena -> NodeIdx -> Float -> IO ()
setNodeValue na idx v = arenaArrays na >>= \a -> writeStyle a idx StyleNodeValue v

-- | Explicit logical font size, or zero for the backend default.
{-# INLINE getNodeFontSize #-}
getNodeFontSize :: NodeArena -> NodeIdx -> IO Float
getNodeFontSize na idx = arenaArrays na >>= \a -> readStyle a idx StyleFontSize

-- | Explicit font colour, or 'Nothing' to use the theme. This is paint-only
-- state and does not invalidate cached layout.
{-# INLINE getNodeFontColor #-}
getNodeFontColor :: NodeArena -> NodeIdx -> IO (Maybe Color)
getNodeFontColor na idx = do
  a <- arenaArrays na
  val <- readPrimArray (naArrFontColor a) idx
  let hasColor = (val .&. 0x100000000) /= 0
  pure (if hasColor then Just (Color (fromIntegral (val .&. 0xFFFFFFFF))) else Nothing)

-- | Packed paint scope: theme index above bit 0, disabled flag in bit 0.
{-# INLINE getNodeScope #-}
getNodeScope :: NodeArena -> NodeIdx -> IO Int
getNodeScope na idx = arenaArrays na >>= \a -> readPrimArray (naArrScope a) idx

-- | Packed paint scope that newly added nodes inherit.
{-# INLINE getArenaScope #-}
getArenaScope :: NodeArena -> IO Int
getArenaScope na = readIORef (naScope na)

-- | Select the packed paint scope for subsequent nodes. Existing nodes keep theirs.
{-# INLINE setArenaScope #-}
setArenaScope :: NodeArena -> Int -> IO ()
setArenaScope na = writeIORef (naScope na)

-- | Hash of non-default node scopes recorded during insertion this frame.
-- Used to detect changes in scoped painting; zero is the reset value.
{-# INLINE getScopeSignature #-}
getScopeSignature :: NodeArena -> IO Word64
getScopeSignature na = readIORef (naScopeSig na)

-- | Hash over every layout input written since the reset: each node's
-- layout and tree links as 'addNode' wrote them, plus every later change
-- through 'setNodeText', 'setOptions', 'setWidgetId' and 'setStyleIdx'.
-- Solver outputs ('setScrollContentW', 'TagScrollBarSlot', rects) and paint
-- state ('setNodeValue', the font colour) are excluded. Tree links need no mix
-- of their own: every node's index and parent are in its creation hash, and
-- children are prepended in index order, so the child lists follow. The node
-- count enters through the indices too; 'layoutSigMatches' also checks it
-- exactly.
{-# INLINE getInputSignature #-}
getInputSignature :: NodeArena -> IO Word64
getInputSignature na = readPrimArray (naInputSig na) 0

-- | Compute every node's restore key ('naSubHash'). A first pass, parents
-- before children, parks each node's ancestor chain (its ancestors' own
-- hashes) in its slot. A second pass, from the last node down so each child's
-- key is final before its parent reads it, folds the node's own hash and chain
-- with its children's keys. Child positions mix in, so reordering children
-- counts as a change. Run after the view has built and before the solve.
computeSubtreeHashes :: NodeArena -> IO ()
computeSubtreeHashes na = do
  n <- arenaCount na
  a <- arenaArrays na
  own <- readIORef (naOwnHash na)
  sub <- readIORef (naSubHash na)
  let chain !i
        | i >= n = pure ()
        | otherwise = do
            p <- readTree a i TreeParent
            anc <-
              if p < 0
                then pure 0
                else mixTagged <$> readPrimArray sub p <*> pure 0x414e <*> readPrimArray own p
            writePrimArray sub i anc
            chain (i + 1)
      fold !i
        | i < 0 = pure ()
        | otherwise = do
            o <- readPrimArray own i
            anc <- readPrimArray sub i
            fc <- readTree a i TreeFirstChild
            (cnt, acc) <- walkKids a sub 0 o fc
            writePrimArray sub i (mixTagged acc cnt (o `xor` anc))
            fold (i - 1)
  chain 0
  fold (n - 1)

-- | Fold a child list (most recently added first) into the parent's subtree
-- hash, each child's position mixed in so reordering counts as a change.
walkKids :: NodeArenaArrays -> IOArr Word64 -> Word64 -> Word64 -> NodeIdx -> IO (Word64, Word64)
walkKids a sub !pos !h !c
  | c < 0 = pure (pos, h)
  | otherwise = do
      sh <- readPrimArray sub c
      nxt <- readTree a c TreeNextSibling
      walkKids a sub (pos + 1) (mixTagged h pos sh) nxt

-- | The subtree-hash and measured-size arrays the solver reads and writes
-- during a solve. Do not retain them across arena growth.
subtreeArrays :: NodeArena -> IO (IOArr Word64, IOArr Float)
subtreeArrays na = (,) <$> readIORef (naSubHash na) <*> readIORef (naMeasured na)

-- | Type-specific style code. Its encoding depends on 'getNodeType', such as
-- button flags, a radio option index, or packed text styling.
{-# INLINE getStyleIdx #-}
getStyleIdx :: NodeArena -> NodeIdx -> IO Int
getStyleIdx na idx = arenaArrays na >>= \a -> readTree a idx TreeStyleIdx

-- | Store a style code encoded for this node's type. Part of the layout
-- input signature, except on box, image, and drawing nodes: their style code
-- is paint data (a colour, a version).
{-# INLINE setStyleIdx #-}
setStyleIdx :: NodeArena -> NodeIdx -> Int -> IO ()
setStyleIdx na idx v = do
  a <- arenaArrays na
  writeTree a idx TreeStyleIdx v
  nt <- readTagEnum a idx TagNodeType
  unless (nt == NodeBox || nt == NodeImage || nt == NodeDrawing) $
    mixNodeInput na idx 0x5354 (fromIntegral v)

-- | The snapshot buffers for nesting depth @depth@, with room for at least
-- @needed@ entries. Each depth keeps its buffers across frames, so nothing is
-- allocated in steady state once they are big enough, and nesting depth has
-- no fixed limit.
{-# NOINLINE ensureAxisSnapshot #-}
ensureAxisSnapshot :: NodeArena -> Int -> Int -> IO AxisSnapshot
ensureAxisSnapshot na depth needed = do
  levels0 <- readIORef (naSnapLevels na)
  let !sz = sizeofMutableArray levels0
  levels <-
    if depth < sz
      then pure levels0
      else do
        empty <- newAxisSnapshot 0
        levels <- growBoxedStoreCopy empty levels0 sz (max (depth + 1) (sz * 2))
        levels <$ writeIORef (naSnapLevels na) levels
  s <- readArray levels depth
  cap <- getSizeofMutablePrimArray (asIdx s)
  if needed <= cap
    then pure s
    else do
      s' <- newAxisSnapshot (max needed (max 64 (cap * 2)))
      s' <$ writeArray levels depth s'

newAxisSnapshot :: Int -> IO AxisSnapshot
newAxisSnapshot cap = AxisSnapshot <$> newPrimArray cap <*> newPrimArray cap

-- | Memoize @compute@ for node @idx@ at width @key@ in one of the arena's
-- per-frame memos. Widths within 0.25 px share an entry so near-identical
-- reflows still hit.
{-# INLINE memoizeWidth #-}
memoizeWidth :: NodeArena -> IORef WidthMemo -> NodeIdx -> Float -> IO (Float, Float) -> IO (Float, Float)
memoizeWidth na ref idx key compute = do
  ft <- readIORef (naFrameTag na)
  WidthMemo tags slots <- readIORef ref
  tag <- readPrimArray tags idx
  let !base = idx * memoStride
  hit <-
    if tag /= ft
      then pure False
      else do
        k <- readPrimArray slots base
        pure (abs (k - key) <= 0.25)
  if hit
    then (,) <$> readPrimArray slots (base + 1) <*> readPrimArray slots (base + 2)
    else do
      r@(x, y) <- compute
      WidthMemo tags' slots' <- readIORef ref
      writePrimArray tags' idx ft
      writePrimArray slots' base key
      writePrimArray slots' (base + 1) x
      writePrimArray slots' (base + 2) y
      pure r

-- | The flex scratch, with room for at least @needed@ entries. Growing it
-- replaces the buffers without copying: a container fills them only after
-- asking for room, and reads its children back from its snapshot once a
-- child may have used them.
{-# INLINE ensureScratchCapacity #-}
ensureScratchCapacity :: NodeArena -> Int -> IO FlexScratch
ensureScratchCapacity na needed = do
  s <- readIORef (naScratch na)
  if needed <= fsCap s then pure s else growScratch na (max needed (fsCap s * 2))

{-# NOINLINE growScratch #-}
growScratch :: NodeArena -> Int -> IO FlexScratch
growScratch na cap = do
  s <- newFlexScratch cap
  s <$ writeIORef (naScratch na) s

-- | Visit the nodes of a floating type (modal, window, popup) in arena
-- order, looking only at the floating nodes.
{-# INLINE forFloatingNodes_ #-}
forFloatingNodes_ :: NodeArena -> NodeType -> (NodeIdx -> IO ()) -> IO ()
forFloatingNodes_ na t f =
  forClassNodes_ na FloatingNodes $ \idx -> getNodeType na idx >>= \nt -> when (nt == t) (f idx)



-- | Visit direct children in reverse declaration order, including floating nodes.
{-# INLINE forChildNodes_ #-}
forChildNodes_ :: NodeArena -> NodeIdx -> (NodeIdx -> IO ()) -> IO ()
forChildNodes_ na parentIdx f = do
  fc <- getFirstChild na parentIdx
  let go !ci
        | ci < 0 = pure ()
        | otherwise = do
            f ci
            ns <- getNextSibling na ci
            go ns
  go fc

-- | Fold over a node's children in sibling order, skipping floating
-- (modal, window, popup) children, which are placed outside the flow.
{-# INLINE foldFlowChildrenM #-}
foldFlowChildrenM :: NodeArena -> NodeIdx -> (acc -> NodeIdx -> IO acc) -> acc -> IO acc
foldFlowChildrenM na parentIdx f z = do
  fc <- getFirstChild na parentIdx
  let go !ci !acc
        | ci < 0 = pure acc
        | otherwise = do
            nt <- getNodeType na ci
            ns <- getNextSibling na ci
            if isFloatingNode nt
              then go ns acc
              else f acc ci >>= go ns
  go fc z

-- | Strict fold over the nodes @at 0@ to @at (k - 1)@, or from the last to
-- the first with @rev@.
{-# INLINE foldSeqM #-}
foldSeqM :: Bool -> Int -> (Int -> IO NodeIdx) -> (a -> NodeIdx -> IO a) -> a -> IO a
foldSeqM rev k at f = go (if rev then k - 1 else 0)
  where
    go !i !acc
      | i < 0 || i >= k = pure acc
      | otherwise = at i >>= f acc >>= go (if rev then i - 1 else i + 1)

-- | The first of the nodes @at 0@ to @at (k - 1)@, or the last with @rev@,
-- that satisfies the predicate.
{-# INLINE findSeqM #-}
findSeqM :: Bool -> Int -> (Int -> IO NodeIdx) -> (NodeIdx -> IO Bool) -> IO (Maybe NodeIdx)
findSeqM rev k at p = go (if rev then k - 1 else 0)
  where
    go !i
      | i < 0 || i >= k = pure Nothing
      | otherwise = do
          idx <- at i
          ok <- p idx
          if ok then pure (Just idx) else go (if rev then i - 1 else i + 1)

-- | First node, in arena order, satisfying the predicate.
{-# INLINE findNodeM #-}
findNodeM :: NodeArena -> (NodeIdx -> IO Bool) -> IO (Maybe NodeIdx)
findNodeM na p = arenaCount na >>= \n -> findSeqM False n pure p

-- | Find the last declared matching node, or 'Nothing'. Stops at the first match
-- while scanning backwards.
{-# INLINE findNodeRevM #-}
findNodeRevM :: NodeArena -> (NodeIdx -> IO Bool) -> IO (Maybe NodeIdx)
findNodeRevM na p = arenaCount na >>= \n -> findSeqM True n pure p

-- | Left fold over every node in arena order.
{-# INLINE foldNodesM #-}
foldNodesM :: NodeArena -> (a -> NodeIdx -> IO a) -> a -> IO a
foldNodesM na f z = arenaCount na >>= \n -> foldSeqM False n pure f z


-- | The length of the list of class @c@ in 'naClassNodes', and a reader for
-- its @i@th node.
{-# INLINE classNodes #-}
classNodes :: NodeArena -> NodeClass -> IO (Int, Int -> IO NodeIdx)
classNodes na c = do
  let ci = fromEnum c
  arr <- readIORef (naClassNodes na)
  cap <- readIORef (naCapacity na)
  k <- readPrimArray (naClassCounts na) ci
  -- Forced here: a lazy offset would be a thunk and a box on every walk.
  let !base = ci * cap
  pure (k, \i -> readPrimArray arr (base + i))

-- | 'findNodeM' over the nodes of one class: the first in arena order that
-- satisfies the predicate.
{-# INLINE findClassNodeM #-}
findClassNodeM :: NodeArena -> NodeClass -> (NodeIdx -> IO Bool) -> IO (Maybe NodeIdx)
findClassNodeM na c p = classNodes na c >>= \(k, at) -> findSeqM False k at p

-- | 'findNodeRevM' over the nodes of one class: the last in arena order that
-- satisfies the predicate.
{-# INLINE findClassNodeRevM #-}
findClassNodeRevM :: NodeArena -> NodeClass -> (NodeIdx -> IO Bool) -> IO (Maybe NodeIdx)
findClassNodeRevM na c p = classNodes na c >>= \(k, at) -> findSeqM True k at p

-- | 'foldNodesM' over the nodes of one class, in arena order.
{-# INLINE foldClassNodesM #-}
foldClassNodesM :: NodeArena -> NodeClass -> (a -> NodeIdx -> IO a) -> a -> IO a
foldClassNodesM na c f z = classNodes na c >>= \(k, at) -> foldSeqM False k at f z

-- | 'foldClassNodesM' from last declared to first.
{-# INLINE foldClassNodeRevM #-}
foldClassNodeRevM :: NodeArena -> NodeClass -> (a -> NodeIdx -> IO a) -> a -> IO a
foldClassNodeRevM na c f z = classNodes na c >>= \(k, at) -> foldSeqM True k at f z

-- | Visit the nodes of one class in arena order.
{-# INLINE forClassNodes_ #-}
forClassNodes_ :: NodeArena -> NodeClass -> (NodeIdx -> IO ()) -> IO ()
forClassNodes_ na c f = foldClassNodesM na c (\() idx -> f idx) ()

-- | The first result @step@ finds walking up from @idx@, the node itself
-- first.
{-# INLINE walkAncestors #-}
walkAncestors :: NodeArena -> NodeIdx -> (NodeIdx -> IO (Maybe a)) -> IO (Maybe a)
walkAncestors na idx step = go idx
  where
    go !i
      | i < 0 = pure Nothing
      | otherwise = step i >>= maybe (getParent na i >>= go) (pure . Just)

-- | 'walkAncestors' that offers @step@ only the floating nodes (modal,
-- window, popup), with their type. Most frames have no floating node, and
-- then this skips the walk.
{-# INLINE walkFloatingAncestors #-}
walkFloatingAncestors :: NodeArena -> NodeIdx -> (NodeIdx -> NodeType -> IO (Maybe a)) -> IO (Maybe a)
walkFloatingAncestors na idx step = do
  floating <- floatingNodeCount na
  if floating <= 0
    then pure Nothing
    else walkAncestors na idx $ \i -> do
      nt <- getNodeType na i
      if isFloatingNode nt then step i nt else pure Nothing

-- | First direct child of @parentIdx@ satisfying the predicate.
{-# INLINE findChildM #-}
findChildM :: NodeArena -> NodeIdx -> (NodeIdx -> IO Bool) -> IO (Maybe NodeIdx)
findChildM na parentIdx p = do
  fc <- getFirstChild na parentIdx
  let go !ci
        | ci < 0 = pure Nothing
        | otherwise = do
            ok <- p ci
            if ok then pure (Just ci) else getNextSibling na ci >>= go
  go fc
