{-# LANGUAGE RecordWildCards #-}

-- | Layout nodes for one frame, stored in flat mutable arrays.
--
-- A node is an index ('NodeIdx') into those arrays. Indices count up in the
-- order the view adds nodes, so a parent's index is lower than its children's.
-- The strided arrays give every node a row of fixed width, and a column
-- constant such as 'geomX' or 'treeParent' names a slot in the row: node
-- @idx@ keeps geometry column @col@ at element @idx * geomStride + col@ of
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
  , foldFloatingNodesM
  , foldFloatingNodeRevM
  , arenaArrays
  , withArenaArraysSnap
  , geomX
  , geomY
  , geomW
  , geomH
  , styleWVal
  , styleHVal
  , styleMinW
  , styleMinH
  , styleMaxW
  , styleMaxH
  , stylePadL
  , stylePadR
  , stylePadT
  , stylePadB
  , styleGap
  , styleGridMinColW
  , tagNodeType
  , tagDirection
  , tagWSizing
  , tagHSizing
  , tagScrollBarSlot
  , treeParent
  , treeFirstChild
  , treeNextSibling
  , treeStyleIdx
  , treeGridCols
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
  , getChildCount
  , getNodeType
  , getDirection
  , getGridCols
  , getGridMinColW
  , getScrollContentW
  , setScrollContentW
  , getWidthSizing
  , getHeightSizing
  , getPadding
  , getGap
  , getMinMax
  , parentIsRow
  , getAlignX
  , getAlignY
  , getRect
  , getNodeRect
  , setRect
  , getLayoutRect
  , getClipRect
  , setClipRect
  , snapshotLayoutRects
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
  , forNodes_
  , forFloatingNodes_
  , forChildNodes_
  , foldFlowChildrenM
  , findNodeRevM
  , findFloatingNodeRevM
  , findClassNodeM
  , findClassNodeRevM
  , foldClassNodesM
  , foldClassNodeRevM
  , forClassNodes_
  , walkFloatingAncestors
  , foldNodeRevM
  , findNodeM
  , foldNodesM
  , findChildM
  , walkAncestors
  , LayoutCache (..)
  , CustomMeasureRecord
  , newLayoutCache
  , captureLayoutCache
  , layoutCacheEligible
  , layoutSigMatches
  , getInputSignature
  , computeSubtreeHashes
  , subtreeArrays
  , restoreLayoutCache
  ) where

import Control.Exception (bracket_)
import Control.Monad (foldM, forM_, unless, when)
import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.HashTable.IO (BasicHashTable)
import qualified Data.HashTable.IO as HT
import Data.Hashable (Hashable, hash)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Primitive.Array (MutableArray, copyMutableArray, newArray, readArray, sizeofMutableArray, writeArray)
import Data.Primitive.PrimArray
  ( MutablePrimArray
  , copyMutablePrimArray
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
-- 'Word8' ('tagNodeType') through the derived 'Enum' instance, so there can be
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
  | NodeWidget
  -- ^ A widget that paints nothing. No widget in the library adds one.
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
    NodeWidget -> True
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
-- ('NanoUI.Internal.Layout.Solve.placeModals' and the functions next to it), and paint
-- draws it over the page.
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
  deriving (Eq, Enum, Bounded)

-- | The constructor of a 'Sizing' without its number, as the arena stores it
-- in 'tagWSizing' and 'tagHSizing'. The number goes in 'styleWVal' or
-- 'styleHVal'.
data SizingTag
  = SizingFixed
  | SizingFit
  | SizingGrow
  | SizingShrink
  | SizingPercent
  deriving (Eq, Show, Enum, Bounded)

-- | A 'Direction' as the arena stores it in 'tagDirection'. On a container
-- it is the axis the children are laid out along. A separator carries its
-- parent's direction, which decides whether the rule is horizontal or
-- vertical.
data DirTag = DirRow | DirColumn
  deriving (Eq, Show, Enum, Bounded)

-- | The arena's per-node arrays. The first four are strided: node @idx@ owns
-- the half-open range @idx * stride@ to @(idx + 1) * stride@, and the column
-- constants ('geomX', 'styleGap', 'tagNodeType', 'treeParent' and the rest)
-- name the slots of that row. The other arrays hold one element per node.
-- Growing the arena replaces all of them together.
data NodeArenaArrays = NodeArenaArrays
  { naArrGeom :: !(IOArr Float)
  -- ^ Rects, 10 floats per node. See 'geomX'.
  , naArrStyle :: !(IOArr Float)
  -- ^ Layout inputs and a few other numbers, 16 floats per node. See
  -- 'styleWVal'.
  , naArrTags :: !(IOArr Word8)
  -- ^ Enum values, 8 bytes per node. See 'tagNodeType'.
  , naArrTree :: !(IOArr Int)
  -- ^ Tree links and other integers, 8 per node. See 'treeParent'.
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
  , naSnapCap :: IORef Int
  -- ^ Entries that each buffer in 'naSnapLevels' has room for.
  , naSnapLevels :: IORef (MutableArray RealWorld (Maybe AxisSnapshot))
  -- ^ One 'AxisSnapshot' per nesting depth of the position pass, 'Nothing'
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
  -- slot so a mix allocates nothing: each node's
  -- constraints and links as 'addNode' wrote them, and every later change
  -- through the input setters ('setNodeText', 'setStyleIdx', 'setOptions',
  -- 'setWidgetId', 'setGridCols', 'setGridMinColW', 'setNodeFontSize').
  -- Solver outputs, paint state, and geometry are excluded. See
  -- 'getInputSignature'.
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
  , naTopModal :: IORef Int
  -- ^ Index of the last modal node added this frame, or -1. Node types are
  -- fixed when a node is added and indices only grow until a reset, so this
  -- is the topmost modal without a scan.
  , naFloatingNodes :: IORef [NodeIdx]
  -- ^ Floating nodes (windows, modals, popups) added this frame, last added
  -- first, so the passes that look only at floating panels skip the rest of
  -- the arena.
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
  , fsOutW :: !(IOArr Float)
  -- ^ Width of each child after the space is shared out. A column copies
  -- 'fsW' through unchanged.
  , fsOutH :: !(IOArr Float)
  -- ^ Height of each child after the space is shared out. A row copies 'fsH'
  -- through unchanged.
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

-- | Initial nesting-depth capacity. 'ensureSnapLevelsArr' grows it as needed.
maxSnapDepth :: Int
maxSnapDepth = 256

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

-- | Columns of 'naArrGeom', in logical pixels and window coordinates.
-- @geomStride@ is the width of a node's row.
--
-- * 'geomX', 'geomY', 'geomW', 'geomH': the node's rect. The solver's measure
--   pass stores the measured size in 'geomW' and 'geomH' with the origin at 0.
--   Its position pass then writes the placed rect. Last,
--   'NanoUI.Internal.Frame.Scroll.applyScrollOffsets' moves the origin by the offsets
--   of the scroll containers around the node.
-- * @geomLayoutX@, @geomLayoutY@: the origin the solver placed the node at,
--   before scrolling moved it. 'snapshotLayoutRects' fills them in.
geomStride, geomX, geomY, geomW, geomH, geomLayoutX, geomLayoutY :: Int
geomStride = 10
geomX = 0
geomY = 1
geomW = 2
geomH = 3
geomLayoutX = 4
geomLayoutY = 5

-- | Columns of 'naArrGeom' that hold the node's clip rect, in window
-- coordinates. See 'getClipRect'.
geomClipX, geomClipY, geomClipW, geomClipH :: Int
geomClipX = 6
geomClipY = 7
geomClipW = 8
geomClipH = 9

-- | Columns of 'naArrStyle': the numbers the node was laid out with, in
-- logical pixels unless stated. @styleStride@ is the width of a node's row.
--
-- * 'styleWVal', 'styleHVal': the number of the width or height 'Sizing'. It
--   is the size for 'Fixed', the factor for 'Grow' and 'Shrink', the
--   percentage for 'Percent', and 0 for 'Fit'. 'tagWSizing' and 'tagHSizing'
--   say which.
-- * 'stylePadL', 'stylePadR', 'stylePadT', 'stylePadB': the padding inside the
--   left, right, top and bottom edges.
styleStride, styleWVal, styleHVal, stylePadL, stylePadR, stylePadT, stylePadB :: Int
styleStride = 16
styleWVal = 0
styleHVal = 1
stylePadL = 2
stylePadR = 3
stylePadT = 4
stylePadB = 5

-- | More columns of 'naArrStyle', in logical pixels.
--
-- * 'styleGap': the space between neighbouring children.
-- * 'styleMinW', 'styleMinH', 'styleMaxW', 'styleMaxH': the size limits. A
--   maximum of 1e8 or more means no limit, and the default layout uses 1e9.
-- * @styleGrow@: the @grow@ argument of 'addNode'. The solver does not read
--   it. A node's grow factor is the number of its 'Grow' sizing.
styleGap, styleMinW, styleMinH, styleMaxW, styleMaxH, styleGrow :: Int
styleGap = 6
styleMinW = 7
styleMinH = 8
styleMaxW = 9
styleMaxH = 10
styleGrow = 11

-- | The last columns of 'naArrStyle'. The first two are not layout inputs, so
-- the layout cache does not compare them.
--
-- * @styleScrollContentW@: the content width of a scroll container that
--   scrolls both ways, which the solver writes ('getScrollContentW').
-- * @styleNodeValue@: the node value ('getNodeValue').
-- * 'styleGridMinColW': the least column width of a grid that fits as many
--   columns as it can, or 0 ('getGridMinColW').
-- * @styleFontSize@: the font size, or 0 for the default ('getNodeFontSize').
styleScrollContentW, styleNodeValue, styleGridMinColW, styleFontSize :: Int
styleScrollContentW = 12
styleNodeValue = 13
styleGridMinColW = 14
styleFontSize = 15

-- | Columns of 'naArrTags'. Each holds one enum value as a 'Word8', written
-- with 'writeTagEnum'. @tagStride@ is the width of a node's row, and column 7
-- is unused.
--
-- * 'tagNodeType': the 'NodeType'.
-- * 'tagDirection': the 'DirTag'.
-- * 'tagWSizing', 'tagHSizing': the 'SizingTag' of the width and the height.
-- * 'tagScrollBarSlot': a 'NanoUI.Internal.Font.ScrollBarSlot', which says where a
--   scroll container's bar sits. The solver's measure pass writes it, so the
--   input signature leaves it out and the layout cache restores it on a hit.
-- * @tagAlignX@, @tagAlignY@: the 'AlignX' and the 'AlignY'.
tagStride, tagNodeType, tagDirection, tagWSizing, tagHSizing, tagScrollBarSlot, tagAlignX, tagAlignY :: Int
tagStride = 8
tagNodeType = 0
tagDirection = 1
tagWSizing = 2
tagHSizing = 3
tagScrollBarSlot = 4
tagAlignX = 5
tagAlignY = 6

-- | Columns of 'naArrTree'. @treeStride@ is the width of a node's row. A link
-- that leads nowhere is -1.
--
-- * 'treeParent': the parent's index.
-- * 'treeFirstChild': the child that was added last. 'addNode' puts every new
--   child at the head of its parent's list.
-- * 'treeNextSibling': the sibling that was added before this node. Following
--   these links therefore visits a node's children from the last to the first.
-- * @treeChildCount@: the number of children, floating ones included.
treeStride, treeParent, treeFirstChild, treeNextSibling, treeChildCount :: Int
treeStride = 8
treeParent = 0
treeFirstChild = 1
treeNextSibling = 2
treeChildCount = 3

-- | More columns of 'naArrTree'.
--
-- * @treeWidgetId@: the 'WidgetId' converted to an 'Int', or 0 for none.
-- * 'treeStyleIdx': the style index ('getStyleIdx').
-- * @treeTextIdx@: the node's slot in 'naArrTextStore', which is its own
--   index, or -1 when the node has no text.
-- * 'treeGridCols': the column count of a grid, or 0 ('getGridCols').
treeWidgetId, treeStyleIdx, treeTextIdx, treeGridCols :: Int
treeWidgetId = 4
treeStyleIdx = 5
treeTextIdx = 6
treeGridCols = 7

-- | Read geometry column @col@ ('geomX' and the rest) of node @idx@. Like the
-- other raw accessors below, it takes the arrays so that a loop can fetch them
-- once with 'arenaArrays', and it does not check that @idx@ is below
-- 'arenaCount'.
{-# INLINE readGeom #-}
readGeom :: NodeArenaArrays -> NodeIdx -> Int -> IO Float
readGeom a idx col = readPrimArray (naArrGeom a) (idx * geomStride + col)

-- | Write geometry column @col@ of node @idx@.
{-# INLINE writeGeom #-}
writeGeom :: NodeArenaArrays -> NodeIdx -> Int -> Float -> IO ()
writeGeom a idx col = writePrimArray (naArrGeom a) (idx * geomStride + col)

-- | Read style column @col@ ('styleWVal' and the rest) of node @idx@.
{-# INLINE readStyle #-}
readStyle :: NodeArenaArrays -> NodeIdx -> Int -> IO Float
readStyle a idx col = readPrimArray (naArrStyle a) (idx * styleStride + col)

-- | Write style column @col@ of node @idx@.
{-# INLINE writeStyle #-}
writeStyle :: NodeArenaArrays -> NodeIdx -> Int -> Float -> IO ()
writeStyle a idx col = writePrimArray (naArrStyle a) (idx * styleStride + col)

-- | Read tag column @col@ of node @idx@ and decode it with 'toEnum'. The
-- caller picks the result type, which must be the type the column was written
-- with: 'NodeType' for 'tagNodeType', 'DirTag' for 'tagDirection', and so on.
{-# INLINE readTagEnum #-}
readTagEnum :: Enum e => NodeArenaArrays -> NodeIdx -> Int -> IO e
readTagEnum a idx col = do
  t <- readPrimArray (naArrTags a) (idx * tagStride + col)
  pure $! toEnum (fromIntegral t)

-- | Write an enum value as one byte. Its 'fromEnum' value must be in 0-255
-- and must use the type expected by the column.
{-# INLINE writeTagEnum #-}
writeTagEnum :: Enum e => NodeArenaArrays -> NodeIdx -> Int -> e -> IO ()
writeTagEnum a idx col v = writePrimArray (naArrTags a) (idx * tagStride + col) (fromIntegral (fromEnum v))

-- | Read tree column @col@ ('treeParent' and the rest) of node @idx@.
{-# INLINE readTree #-}
readTree :: NodeArenaArrays -> NodeIdx -> Int -> IO Int
readTree a idx col = readPrimArray (naArrTree a) (idx * treeStride + col)

-- | Write tree column @col@ of node @idx@. Writing a link column by hand can
-- leave the child lists and the child counts inconsistent.
{-# INLINE writeTree #-}
writeTree :: NodeArenaArrays -> NodeIdx -> Int -> Int -> IO ()
writeTree a idx col = writePrimArray (naArrTree a) (idx * treeStride + col)

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
  fsOutW <- newPrimArray fsCap
  fsOutH <- newPrimArray fsCap
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
-- children of one container in the solver's buffers, and 256 nesting depths
-- (@maxSnapDepth@). All three grow when a view needs more.
newNodeArena :: IO NodeArena
newNodeArena = do
  let cap = initialCapacity
      scratchCap = 64
  naCount <- newIORef 0
  naCapacity <- newIORef cap
  naArrays <- newIORef =<< newNodeArenaArrays cap
  naArraysSnap <- newIORef Nothing
  naScratch <- newIORef =<< newFlexScratch scratchCap
  naSnapCap <- newIORef scratchCap
  naSnapLevels <- newIORef =<< newArray maxSnapDepth Nothing
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
  naTopModal <- newIORef (-1)
  naFloatingNodes <- newIORef []
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
  writeIORef (naTopModal na) (-1)
  writeIORef (naFloatingNodes na) []
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
topModalNode na = do
  i <- readIORef (naTopModal na)
  pure (if i >= 0 then Just i else Nothing)

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
floatingNodeCount na = length <$> readIORef (naFloatingNodes na)

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

-- | Append a node and link it at the head of its parent's child list. Parent
-- must be -1 or an existing node index. Arguments after padding are gap,
-- minimum width/height, maximum width/height, and a stored grow value; the
-- solver takes grow weights from the sizing arguments. Lengths use logical pixels.
{-# INLINE addNode #-}
addNode ::
  NodeArena ->
  NodeType ->
  Int ->
  Direction ->
  Sizing ->
  Sizing ->
  Padding ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  AlignX ->
  AlignY ->
  IO NodeIdx
addNode na nt parent dir wSiz hSiz pad gap minW minH maxW maxH grow ax ay = do
  idx <- readIORef (naCount na)
  ensureCapacity na (idx + 1)
  let (wTag, wVal) = sizingTag wSiz
      (hTag, hVal) = sizingTag hSiz
  a <- arenaArrays na

  setPrimArray (naArrGeom a) (idx * geomStride) geomStride 0

  writeStyle a idx styleWVal wVal
  writeStyle a idx styleHVal hVal
  writeStyle a idx stylePadL (padL pad)
  writeStyle a idx stylePadR (padR pad)
  writeStyle a idx stylePadT (padT pad)
  writeStyle a idx stylePadB (padB pad)
  writeStyle a idx styleGap gap
  writeStyle a idx styleMinW minW
  writeStyle a idx styleMinH minH
  writeStyle a idx styleMaxW maxW
  writeStyle a idx styleMaxH maxH
  writeStyle a idx styleGrow grow
  setPrimArray (naArrStyle a) (idx * styleStride + styleScrollContentW) (styleStride - styleScrollContentW) 0

  setPrimArray (naArrTags a) (idx * tagStride) tagStride 0
  writeTagEnum a idx tagNodeType nt
  writeTagEnum a idx tagDirection $ case dir of
    Row -> DirRow
    Column -> DirColumn
  writeTagEnum a idx tagWSizing wTag
  writeTagEnum a idx tagHSizing hTag
  writeTagEnum a idx tagAlignX ax
  writeTagEnum a idx tagAlignY ay

  setPrimArray (naArrTree a) (idx * treeStride) treeStride 0
  writeTree a idx treeParent parent
  writeTree a idx treeFirstChild (-1)
  writeTree a idx treeNextSibling (-1)
  writeTree a idx treeTextIdx (-1)

  -- Fold this node's creation inputs into the frame's input signature, the
  -- O(1) successor of comparing every column at reuse time. Values are the
  -- ones already in registers above.
  let !nodeSig =
        foldl'
          (\acc (t, v) -> mixTagged acc t v)
          (fromIntegral idx `shiftL` 32 .|. fromIntegral (idx + 1) :: Word64)
          [ (0x4e54, fromIntegral (fromEnum nt))
          , (0x4449, fromIntegral (fromEnum dir))
          , (0x5754, fromIntegral (fromEnum wTag))
          , (0x5746, fromIntegral (castFloatToWord32 wVal))
          , (0x4854, fromIntegral (fromEnum hTag))
          , (0x4846, fromIntegral (castFloatToWord32 hVal))
          , (0x504c, fromIntegral (castFloatToWord32 (padL pad)))
          , (0x5052, fromIntegral (castFloatToWord32 (padR pad)))
          , (0x5054, fromIntegral (castFloatToWord32 (padT pad)))
          , (0x5042, fromIntegral (castFloatToWord32 (padB pad)))
          , (0x4741, fromIntegral (castFloatToWord32 gap))
          , (0x4d57, fromIntegral (castFloatToWord32 minW))
          , (0x4d48, fromIntegral (castFloatToWord32 minH))
          , (0x5857, fromIntegral (castFloatToWord32 maxW))
          , (0x5848, fromIntegral (castFloatToWord32 maxH))
          , (0x4752, fromIntegral (castFloatToWord32 grow))
          , (0x4158, fromIntegral (fromEnum ax))
          , (0x4159, fromIntegral (fromEnum ay))
          , (0x5041, fromIntegral (parent + 1))
          ]
  mixInputSig na 0x4e4f nodeSig
  ownA <- readIORef (naOwnHash na)
  writePrimArray ownA idx nodeSig

  writePrimArray (naArrFontColor a) idx 0
  scope <- readIORef (naScope na)
  writePrimArray (naArrScope a) idx scope
  when (scope /= 0) $ do
    sig <- readIORef (naScopeSig na)
    writeIORef (naScopeSig na) $! (sig * 0x100000001b3) `xor` (fromIntegral idx `shiftL` 32 .|. fromIntegral scope)
  writeArray (naArrOptionsStore a) idx []

  when (parent >= 0) $ do
    fc <- readTree a parent treeFirstChild
    writeTree a idx treeNextSibling fc
    writeTree a parent treeFirstChild idx
    cc <- readTree a parent treeChildCount
    writeTree a parent treeChildCount (cc + 1)
  when (isFloatingNode nt) $ do
    when (nt == NodeModal) $ writeIORef (naTopModal na) idx
    modifyIORef' (naFloatingNodes na) (idx :)
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

-- | Add a node using layout fields, including grid and font-size/colour options.
-- The caller assigns widget identity, text, and type-specific style data.
addNodeFromLayout :: NodeArena -> NodeType -> Int -> Layout -> IO NodeIdx
addNodeFromLayout na nt parent l = do
  idx <-
    addNode
      na
      nt
      parent
      (layoutDirection l)
      (layoutWidth l)
      (layoutHeight l)
      (layoutPadding l)
      (layoutGap l)
      (layoutMinW l)
      (layoutMinH l)
      (layoutMaxW l)
      (layoutMaxH l)
      0
      (layoutAlignX l)
      (layoutAlignY l)
  setGridCols na idx (layoutGridCols l)
  setGridMinColW na idx (layoutGridMinColW l)
  setNodeFontSize na idx (layoutFontSize l)
  setNodeFontColor na idx (layoutFontColor l)
  pure idx

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
  writeTree a idx treeTextIdx idx
  mixNodeInput na idx 0x5458 h

-- | Parent index, or -1 for a root.
{-# INLINE getParent #-}
getParent :: NodeArena -> NodeIdx -> IO NodeIdx
getParent na idx = arenaArrays na >>= \a -> readTree a idx treeParent

-- | Most recently added direct child, or -1 when there are none.
{-# INLINE getFirstChild #-}
getFirstChild :: NodeArena -> NodeIdx -> IO NodeIdx
getFirstChild na idx = arenaArrays na >>= \a -> readTree a idx treeFirstChild

-- | Next sibling in reverse declaration order, or -1 at the end.
{-# INLINE getNextSibling #-}
getNextSibling :: NodeArena -> NodeIdx -> IO NodeIdx
getNextSibling na idx = arenaArrays na >>= \a -> readTree a idx treeNextSibling

-- | Number of direct children, including floating nodes.
{-# INLINE getChildCount #-}
getChildCount :: NodeArena -> NodeIdx -> IO Int
getChildCount na idx = arenaArrays na >>= \a -> readTree a idx treeChildCount

-- | Node kind assigned at insertion, which selects layout and paint behaviour.
{-# INLINE getNodeType #-}
getNodeType :: NodeArena -> NodeIdx -> IO NodeType
getNodeType na idx = arenaArrays na >>= \a -> readTagEnum a idx tagNodeType

-- | Main layout axis stored on the node.
{-# INLINE getDirection #-}
getDirection :: NodeArena -> NodeIdx -> IO DirTag
getDirection na idx = arenaArrays na >>= \a -> readTagEnum a idx tagDirection

-- | Explicit grid column count, or zero when none is set.
{-# INLINE getGridCols #-}
getGridCols :: NodeArena -> NodeIdx -> IO Int
getGridCols na idx = arenaArrays na >>= \a -> readTree a idx treeGridCols

{-# INLINE setGridCols #-}
setGridCols :: NodeArena -> NodeIdx -> Int -> IO ()
setGridCols na idx c = do
  arenaArrays na >>= \a -> writeTree a idx treeGridCols c
  mixNodeInput na idx 0x4743 (fromIntegral c)

-- | Width sizing mode and its parameter; see 'styleWVal' for units.
{-# INLINE getWidthSizing #-}
getWidthSizing :: NodeArena -> NodeIdx -> IO (SizingTag, Float)
getWidthSizing na idx = arenaArrays na >>= \a -> (,) <$> readTagEnum a idx tagWSizing <*> readStyle a idx styleWVal

-- | Height sizing mode and its parameter; see 'styleHVal' for units.
{-# INLINE getHeightSizing #-}
getHeightSizing :: NodeArena -> NodeIdx -> IO (SizingTag, Float)
getHeightSizing na idx = arenaArrays na >>= \a -> (,) <$> readTagEnum a idx tagHSizing <*> readStyle a idx styleHVal

-- | Insets in logical pixels, ordered left, right, top, bottom.
{-# INLINE getPadding #-}
getPadding :: NodeArena -> NodeIdx -> IO Padding
getPadding na idx = do
  a <- arenaArrays na
  Padding <$> readStyle a idx stylePadL <*> readStyle a idx stylePadR <*> readStyle a idx stylePadT <*> readStyle a idx stylePadB

-- | Space between children, in logical pixels.
{-# INLINE getGap #-}
getGap :: NodeArena -> NodeIdx -> IO Float
getGap na idx = arenaArrays na >>= \a -> readStyle a idx styleGap

-- | Minimum width, minimum height, maximum width, maximum height, in logical pixels.
{-# INLINE getMinMax #-}
getMinMax :: NodeArena -> NodeIdx -> IO (Float, Float, Float, Float)
getMinMax na idx = do
  a <- arenaArrays na
  (,,,) <$> readStyle a idx styleMinW <*> readStyle a idx styleMinH <*> readStyle a idx styleMaxW <*> readStyle a idx styleMaxH

-- | Solved horizontal content extent of a two-axis scroller, in logical pixels.
{-# INLINE getScrollContentW #-}
getScrollContentW :: NodeArena -> NodeIdx -> IO Float
getScrollContentW na idx = arenaArrays na >>= \a -> readStyle a idx styleScrollContentW

-- | Store the solver's horizontal content extent for a scroller.
{-# INLINE setScrollContentW #-}
setScrollContentW :: NodeArena -> NodeIdx -> Float -> IO ()
setScrollContentW na idx v = arenaArrays na >>= \a -> writeStyle a idx styleScrollContentW v

-- | Adaptive grid's minimum column width in logical pixels, or zero if unset.
{-# INLINE getGridMinColW #-}
getGridMinColW :: NodeArena -> NodeIdx -> IO Float
getGridMinColW na idx = arenaArrays na >>= \a -> readStyle a idx styleGridMinColW

{-# INLINE setGridMinColW #-}
setGridMinColW :: NodeArena -> NodeIdx -> Float -> IO ()
setGridMinColW na idx v = do
  arenaArrays na >>= \a -> writeStyle a idx styleGridMinColW v
  mixNodeInput na idx 0x474d (fromIntegral (castFloatToWord32 v))

-- | Whether the parent has row direction. A root returns 'False'.
{-# INLINE parentIsRow #-}
parentIsRow :: NodeArena -> NodeIdx -> IO Bool
parentIsRow na idx = do
  p <- getParent na idx
  if p < 0
    then pure False
    else do
      dir <- getDirection na p
      pure (dir == DirRow)

-- | Horizontal alignment requested by the node.
{-# INLINE getAlignX #-}
getAlignX :: NodeArena -> NodeIdx -> IO AlignX
getAlignX na idx = arenaArrays na >>= \a -> readTagEnum a idx tagAlignX

-- | Vertical alignment requested by the node.
{-# INLINE getAlignY #-}
getAlignY :: NodeArena -> NodeIdx -> IO AlignY
getAlignY na idx = arenaArrays na >>= \a -> readTagEnum a idx tagAlignY

-- | Current x, y, width, height in logical pixels. After scroll offsets are
-- applied, the origin is in window coordinates; before layout it is unset.
{-# INLINE getRect #-}
getRect :: NodeArena -> NodeIdx -> IO (Float, Float, Float, Float)
getRect na idx = do
  a <- arenaArrays na
  (,,,) <$> readGeom a idx geomX <*> readGeom a idx geomY <*> readGeom a idx geomW <*> readGeom a idx geomH

-- | 'getRect' as a 'Rect'.
{-# INLINE getNodeRect #-}
getNodeRect :: NodeArena -> NodeIdx -> IO Rect
getNodeRect na idx = do
  a <- arenaArrays na
  Rect <$> readGeom a idx geomX <*> readGeom a idx geomY <*> readGeom a idx geomW <*> readGeom a idx geomH

-- | Write x, y, width, and height. Does not update the saved layout origin or clip.
{-# INLINE setRect #-}
setRect :: NodeArena -> NodeIdx -> Float -> Float -> Float -> Float -> IO ()
setRect na idx x y w h = do
  a <- arenaArrays na
  writeGeom a idx geomX x
  writeGeom a idx geomY y
  writeGeom a idx geomW w
  writeGeom a idx geomH h

-- | Saved pre-scroll origin with the current width and height. Requires
-- 'snapshotLayoutRects' after layout.
{-# INLINE getLayoutRect #-}
getLayoutRect :: NodeArena -> NodeIdx -> IO (Float, Float, Float, Float)
getLayoutRect na idx = do
  a <- arenaArrays na
  (,,,) <$> readGeom a idx geomLayoutX <*> readGeom a idx geomLayoutY <*> readGeom a idx geomW <*> readGeom a idx geomH

-- | Positive-area clip in logical window coordinates. 'Nothing' means the
-- stored clip is empty or unset; those cases share the same representation.
{-# INLINE getClipRect #-}
getClipRect :: NodeArena -> NodeIdx -> IO (Maybe Rect)
getClipRect na idx = do
  a <- arenaArrays na
  x <- readGeom a idx geomClipX
  y <- readGeom a idx geomClipY
  w <- readGeom a idx geomClipW
  h <- readGeom a idx geomClipH
  let r = Rect x y w h
  pure (if w > 0 && h > 0 then Just r else Nothing)

-- | Store a clip in logical window coordinates. Empty clips read back as 'Nothing'.
{-# INLINE setClipRect #-}
setClipRect :: NodeArena -> NodeIdx -> Rect -> IO ()
setClipRect na idx (Rect x y w h) = do
  a <- arenaArrays na
  writeGeom a idx geomClipX x
  writeGeom a idx geomClipY y
  writeGeom a idx geomClipW w
  writeGeom a idx geomClipH h

-- | Save each node's origin after layout and before scrolling shifts it.
{-# INLINE snapshotLayoutRects #-}
snapshotLayoutRects :: NodeArena -> IO ()
snapshotLayoutRects na = do
  a <- arenaArrays na
  forNodes_ na $ \i -> do
    readGeom a i geomX >>= writeGeom a i geomLayoutX
    readGeom a i geomY >>= writeGeom a i geomLayoutY

-- | Cached layout signature and solved geometry for whole-layout reuse. The
-- backing arrays are reused; only cache misses capture a new solved frame.
-- The font colour and scope columns are paint state and stay unused.
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
  , lcArrays :: !NodeArenaArrays
  }

-- | A custom measure's (available width, available height, measured width,
-- measured height), as raw float words.
type CustomMeasureRecord = (Word32, Word32, Word32, Word32)

-- | Empty layout cache with at least 16 slots. Capture a solved frame before reuse.
newLayoutCache :: Int -> IO LayoutCache
newLayoutCache cap0 = do
  let !cap = max 16 cap0
  lcArrays <- newNodeArenaArrays cap
  lcSub <- newPrimArray cap
  lcMeasured <- newPrimArray (cap * 2)
  pure (LayoutCache cap 0 0 IM.empty IS.empty lcSub lcMeasured lcArrays)

-- | Snapshot the current (post-solve) arena form, constraints and rects.
captureLayoutCache :: NodeArena -> LayoutCache -> IO LayoutCache
captureLayoutCache na lc0 = do
  n <- arenaCount na
  sig <- getInputSignature na
  subA <- readIORef (naSubHash na)
  measuredA <- readIORef (naMeasured na)
  let !oldCap = lcCap lc0
      !newCap = max n (oldCap * 2)
  lc <-
    if n <= oldCap
      then pure lc0
      else do
        lcArrays <- growNodeArenaArrays oldCap newCap (lcArrays lc0)
        lcSub <- growPrimArrayCopy (lcSub lc0) oldCap newCap 0
        lcMeasured <- growPrimArrayCopy (lcMeasured lc0) (oldCap * 2) (newCap * 2) 0
        pure lc0 {lcCap = newCap, lcSub, lcMeasured, lcArrays}
  a <- arenaArrays na
  let c = lcArrays lc
  copyMutablePrimArray (naArrGeom c) 0 (naArrGeom a) 0 (n * geomStride)
  copyMutablePrimArray (naArrStyle c) 0 (naArrStyle a) 0 (n * styleStride)
  copyMutablePrimArray (naArrTags c) 0 (naArrTags a) 0 (n * tagStride)
  copyMutablePrimArray (naArrTree c) 0 (naArrTree a) 0 (n * treeStride)
  copyMutableArray (naArrTextStore c) 0 (naArrTextStore a) 0 n
  copyMutableArray (naArrOptionsStore c) 0 (naArrOptionsStore a) 0 n
  copyMutablePrimArray (lcSub lc) 0 subA 0 n
  copyMutablePrimArray (lcMeasured lc) 0 measuredA 0 (n * 2)
  pure lc {lcCount = n, lcSig = sig}

-- | Whether the arena holds a layout to cache. The cache holds the solve
-- before floating placement, which depends on state outside the arena and
-- runs again on reuse. Custom measurement is checked separately by Frame,
-- which owns its registration.
layoutCacheEligible :: NodeArena -> IO Bool
layoutCacheEligible na = (> 0) <$> arenaCount na

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
      c = lcArrays lc
  copyMutablePrimArray (naArrGeom a) 0 (naArrGeom c) 0 (n * geomStride)
  let go !i
        | i >= n = pure ()
        | otherwise = do
            nt <- readTagEnum a i tagNodeType
            -- Scroll content width, node value (the content height) and
            -- scrollbar slot.
            when (isScrollNode nt) $ do
              let !off = i * styleStride + styleScrollContentW
                  !slotOff = i * tagStride + tagScrollBarSlot
              copyMutablePrimArray (naArrStyle a) off (naArrStyle c) off 2
              readPrimArray (naArrTags c) slotOff >>= writePrimArray (naArrTags a) slotOff
            go (i + 1)
  go 0

-- | Node text, or empty text when no text was assigned this frame.
{-# INLINE getText #-}
getText :: NodeArena -> NodeIdx -> IO Text
getText na idx = do
  a <- arenaArrays na
  ti <- readTree a idx treeTextIdx
  if ti < 0
    then pure T.empty
    else readArray (naArrTextStore a) ti

-- | Choices stored on a select node, or an empty list when none were assigned.
{-# INLINE getOptions #-}
getOptions :: NodeArena -> NodeIdx -> IO [Text]
getOptions na idx = do
  a <- arenaArrays na
  readArray (naArrOptionsStore a) idx

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
getWidgetId na idx = arenaArrays na >>= \a -> WidgetId . fromIntegral <$> readTree a idx treeWidgetId

{-# INLINE packEpochNode #-}
packEpochNode :: Word32 -> NodeIdx -> Word64
packEpochNode !epoch !idx = (fromIntegral epoch `shiftL` 32) .|. (fromIntegral idx .&. 0xFFFFFFFF)

{-# INLINE unpackEpochNode #-}
unpackEpochNode :: Word64 -> (Word32, NodeIdx)
unpackEpochNode !w = (fromIntegral (w `shiftR` 32), fromIntegral (w .&. 0xFFFFFFFF))

-- | Assign a node's identity and index nonzero ids for lookup. Assign once per
-- node: this does not remove a mapping previously stored under another id.
{-# INLINE setWidgetId #-}
setWidgetId :: NodeArena -> NodeIdx -> WidgetId -> IO ()
setWidgetId na idx wid = do
  a <- arenaArrays na
  let WidgetId w = wid
  writeTree a idx treeWidgetId (fromIntegral w)
  mixNodeInput na idx 0x5749 w
  when (hashWidgetId wid /= 0) $ do
    !ep <- readIORef (naEpoch na)
    table <- readIORef (naIndex na)
    HT.insert table wid (packEpochNode ep idx)

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
          let (!entryEp, !idx) = unpackEpochNode val
          pure (if entryEp == ep then Just idx else Nothing)

-- | 'lookupNodeByWidgetId' using the id's integer store key.
{-# INLINE lookupNodeByKey #-}
lookupNodeByKey :: NodeArena -> Int -> IO (Maybe NodeIdx)
lookupNodeByKey na key = lookupNodeByWidgetId na (WidgetId (fromIntegral key))

-- | Type-specific numeric value, such as selection state or a scroller's
-- solved content height. Interpret it according to 'getNodeType'.
{-# INLINE getNodeValue #-}
getNodeValue :: NodeArena -> NodeIdx -> IO Float
getNodeValue na idx = arenaArrays na >>= \a -> readStyle a idx styleNodeValue

-- | Set the type-specific numeric value read by the solver or painter.
{-# INLINE setNodeValue #-}
setNodeValue :: NodeArena -> NodeIdx -> Float -> IO ()
setNodeValue na idx v = arenaArrays na >>= \a -> writeStyle a idx styleNodeValue v

-- | Explicit logical font size, or zero for the backend default.
{-# INLINE getNodeFontSize #-}
getNodeFontSize :: NodeArena -> NodeIdx -> IO Float
getNodeFontSize na idx = arenaArrays na >>= \a -> readStyle a idx styleFontSize

{-# INLINE setNodeFontSize #-}
setNodeFontSize :: NodeArena -> NodeIdx -> Float -> IO ()
setNodeFontSize na idx v = do
  arenaArrays na >>= \a -> writeStyle a idx styleFontSize v
  mixNodeInput na idx 0x4648 (fromIntegral (castFloatToWord32 v))

-- | Explicit font colour, or 'Nothing' to use the theme. This is paint-only
-- state and does not invalidate cached layout.
{-# INLINE getNodeFontColor #-}
getNodeFontColor :: NodeArena -> NodeIdx -> IO (Maybe Color)
getNodeFontColor na idx = do
  a <- arenaArrays na
  val <- readPrimArray (naArrFontColor a) idx
  if (val .&. 0x100000000) /= 0
    then pure (Just (Color (fromIntegral (val .&. 0xFFFFFFFF))))
    else pure Nothing

{-# INLINE setNodeFontColor #-}
setNodeFontColor :: NodeArena -> NodeIdx -> Maybe Color -> IO ()
setNodeFontColor na idx mCol = do
  a <- arenaArrays na
  let val = case mCol of
        Nothing -> 0
        Just (Color w) -> 0x100000000 .|. fromIntegral w
  writePrimArray (naArrFontColor a) idx val

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
-- constraints and tree links as 'addNode' wrote them, plus every later change
-- through 'setNodeText', 'setOptions', 'setWidgetId', 'setStyleIdx',
-- 'setGridCols', 'setGridMinColW' and 'setNodeFontSize'. Solver outputs
-- ('setScrollContentW', 'tagScrollBarSlot', rects) and paint state
-- ('setNodeValue', 'setNodeFontColor') are excluded. Tree links need no mix
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
            p <- readTree a i treeParent
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
            fc <- readTree a i treeFirstChild
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
      nxt <- readTree a c treeNextSibling
      walkKids a sub (pos + 1) (mixTagged h pos sh) nxt

-- | The subtree-hash and measured-size arrays the solver reads and writes
-- during a solve. Do not retain them across arena growth.
subtreeArrays :: NodeArena -> IO (IOArr Word64, IOArr Float)
subtreeArrays na = (,) <$> readIORef (naSubHash na) <*> readIORef (naMeasured na)

-- | Type-specific style code. Its encoding depends on 'getNodeType', such as
-- button flags, a radio option index, or packed text styling.
{-# INLINE getStyleIdx #-}
getStyleIdx :: NodeArena -> NodeIdx -> IO Int
getStyleIdx na idx = arenaArrays na >>= \a -> readTree a idx treeStyleIdx

-- | Store a style code encoded for this node's type. Part of the layout
-- input signature, except on box, image, and drawing nodes: their style code
-- is paint data (a colour, a version).
{-# INLINE setStyleIdx #-}
setStyleIdx :: NodeArena -> NodeIdx -> Int -> IO ()
setStyleIdx na idx v = do
  a <- arenaArrays na
  writeTree a idx treeStyleIdx v
  nt <- readTagEnum a idx tagNodeType
  unless (nt == NodeBox || nt == NodeImage || nt == NodeDrawing) $
    mixNodeInput na idx 0x5354 (fromIntegral v)

-- | Get the snapshot buffers for a recursion depth, grown to hold at least
-- @needed@ entries. Buffers are reused across frames; nothing is allocated in
-- steady state once capacity is warm.
{-# NOINLINE ensureAxisSnapshot #-}
ensureAxisSnapshot :: NodeArena -> Int -> Int -> IO AxisSnapshot
ensureAxisSnapshot na depth needed = do
  arr0 <- readIORef (naSnapLevels na)
  let !d = max 0 depth
  arr <- ensureSnapLevelsArr na arr0 (d + 1)
  cap <- readIORef (naSnapCap na)
  if needed <= cap
    then getLevel arr d cap
    else do
      let !newCap = max needed (cap * 2)
          !levels = sizeofMutableArray arr
      forM_ [0 .. levels - 1] $ \i ->
        readArray arr i >>= mapM_ (\(AxisSnapshot idx out) -> do
          idx' <- growPrimArrayCopy idx cap newCap 0
          out' <- growPrimArrayCopy out cap newCap 0
          writeArray arr i (Just (AxisSnapshot idx' out')))
      writeIORef (naSnapCap na) newCap
      getLevel arr d newCap
  where
    getLevel arr d currentCap = do
      m <- readArray arr d
      case m of
        Just s -> pure s
        Nothing -> do
          asIdx <- newPrimArray currentCap
          asOut <- newPrimArray currentCap
          let s = AxisSnapshot asIdx asOut
          writeArray arr d (Just s)
          pure s

-- | Grow the per-depth snapshot-level array to hold at least @need@ levels,
-- so nesting depth has no fixed limit.
ensureSnapLevelsArr :: NodeArena -> MutableArray RealWorld (Maybe AxisSnapshot) -> Int -> IO (MutableArray RealWorld (Maybe AxisSnapshot))
ensureSnapLevelsArr na arr need = do
  let !sz = sizeofMutableArray arr
  if need <= sz
    then pure arr
    else do
      let !newSz = max need (sz * 2)
      arr' <- newArray newSz Nothing
      copyMutableArray arr' 0 arr 0 sz
      writeIORef (naSnapLevels na) arr'
      pure arr'

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

-- | The flex scratch, grown to hold at least @needed@ entries.
{-# INLINE ensureScratchCapacity #-}
ensureScratchCapacity :: NodeArena -> Int -> IO FlexScratch
ensureScratchCapacity na needed = do
  s <- readIORef (naScratch na)
  if needed <= fsCap s then pure s else growScratch na s needed

{-# NOINLINE growScratch #-}
growScratch :: NodeArena -> FlexScratch -> Int -> IO FlexScratch
growScratch na s needed = do
  let !cap = fsCap s
      !newCap = max needed (cap * 2)
  fsIdx <- growPrimArrayCopy (fsIdx s) cap newCap (-1)
  fsW <- growPrimArrayCopy (fsW s) cap newCap 0
  fsH <- growPrimArrayCopy (fsH s) cap newCap 0
  fsOutW <- growPrimArrayCopy (fsOutW s) cap newCap 0
  fsOutH <- growPrimArrayCopy (fsOutH s) cap newCap 0
  let s' = FlexScratch {fsCap = newCap, ..}
  writeIORef (naScratch na) s'
  pure s'

-- | Visit live nodes in declaration order. The count is captured before traversal.
{-# INLINE forNodes_ #-}
forNodes_ :: NodeArena -> (NodeIdx -> IO ()) -> IO ()
forNodes_ na f = do
  n <- arenaCount na
  let go !i
        | i >= n = pure ()
        | otherwise = f i >> go (i + 1)
  go 0

-- | Visit the nodes of a floating type (modal, window, popup) in arena
-- order, looking only at the floating nodes.
{-# INLINE forFloatingNodes_ #-}
forFloatingNodes_ :: NodeArena -> NodeType -> (NodeIdx -> IO ()) -> IO ()
forFloatingNodes_ na t f = foldFloatingNodesM na (\() idx -> getNodeType na idx >>= \nt -> when (nt == t) (f idx)) ()

-- | Strict fold over the floating nodes in arena order.
{-# INLINE foldFloatingNodesM #-}
foldFloatingNodesM :: NodeArena -> (a -> NodeIdx -> IO a) -> a -> IO a
foldFloatingNodesM na f z = readIORef (naFloatingNodes na) >>= foldM f z . reverse

-- | Strict fold over the floating nodes from last declared to first.
{-# INLINE foldFloatingNodeRevM #-}
foldFloatingNodeRevM :: NodeArena -> (a -> NodeIdx -> IO a) -> a -> IO a
foldFloatingNodeRevM na f z = readIORef (naFloatingNodes na) >>= foldM f z

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

-- | Find the last declared matching node, or 'Nothing'. Stops at the first match
-- while scanning backwards.
{-# INLINE findNodeRevM #-}
findNodeRevM :: NodeArena -> (NodeIdx -> IO Bool) -> IO (Maybe NodeIdx)
findNodeRevM na p = do
  n <- arenaCount na
  let go !i
        | i < 0 = pure Nothing
        | otherwise = do
            ok <- p i
            if ok then pure (Just i) else go (i - 1)
  go (n - 1)

-- | 'findNodeRevM' for a predicate that only floating nodes can satisfy. It
-- visits only the floating nodes.
{-# INLINE findFloatingNodeRevM #-}
findFloatingNodeRevM :: NodeArena -> (NodeIdx -> IO Bool) -> IO (Maybe NodeIdx)
findFloatingNodeRevM na p = readIORef (naFloatingNodes na) >>= go
  where
    go [] = pure Nothing
    go (i : is) = p i >>= \ok -> if ok then pure (Just i) else go is

-- | Where the list of class @c@ starts in 'naClassNodes', and its length.
{-# INLINE classNodes #-}
classNodes :: NodeArena -> NodeClass -> IO (IOArr Int, Int, Int)
classNodes na c = do
  let ci = fromEnum c
  arr <- readIORef (naClassNodes na)
  cap <- readIORef (naCapacity na)
  k <- readPrimArray (naClassCounts na) ci
  -- Forced here: a lazy offset would be a thunk and a box on every walk.
  let !base = ci * cap
  pure (arr, base, k)

-- | 'findNodeM' over the nodes of one class: the first in arena order that
-- satisfies the predicate.
{-# INLINE findClassNodeM #-}
findClassNodeM :: NodeArena -> NodeClass -> (NodeIdx -> IO Bool) -> IO (Maybe NodeIdx)
findClassNodeM na c p = do
  (arr, base, k) <- classNodes na c
  let go !i
        | i >= k = pure Nothing
        | otherwise = do
            idx <- readPrimArray arr (base + i)
            ok <- p idx
            if ok then pure (Just idx) else go (i + 1)
  go 0

-- | 'findNodeRevM' over the nodes of one class: the last in arena order that
-- satisfies the predicate.
{-# INLINE findClassNodeRevM #-}
findClassNodeRevM :: NodeArena -> NodeClass -> (NodeIdx -> IO Bool) -> IO (Maybe NodeIdx)
findClassNodeRevM na c p = do
  (arr, base, k) <- classNodes na c
  let go !i
        | i < 0 = pure Nothing
        | otherwise = do
            idx <- readPrimArray arr (base + i)
            ok <- p idx
            if ok then pure (Just idx) else go (i - 1)
  go (k - 1)

-- | 'foldNodesM' over the nodes of one class, in arena order.
{-# INLINE foldClassNodesM #-}
foldClassNodesM :: NodeArena -> NodeClass -> (a -> NodeIdx -> IO a) -> a -> IO a
foldClassNodesM na c f z = do
  (arr, base, k) <- classNodes na c
  let go !i !acc
        | i >= k = pure acc
        | otherwise = do
            idx <- readPrimArray arr (base + i)
            acc' <- f acc idx
            go (i + 1) acc'
  go 0 z

-- | 'foldNodeRevM' over the nodes of one class, from last declared to first.
{-# INLINE foldClassNodeRevM #-}
foldClassNodeRevM :: NodeArena -> NodeClass -> (a -> NodeIdx -> IO a) -> a -> IO a
foldClassNodeRevM na c f z = do
  (arr, base, k) <- classNodes na c
  let go !i !acc
        | i < 0 = pure acc
        | otherwise = do
            idx <- readPrimArray arr (base + i)
            acc' <- f acc idx
            go (i - 1) acc'
  go (k - 1) z

-- | 'forNodes_' over the nodes of one class, in arena order.
{-# INLINE forClassNodes_ #-}
forClassNodes_ :: NodeArena -> NodeClass -> (NodeIdx -> IO ()) -> IO ()
forClassNodes_ na c f = foldClassNodesM na c (\() idx -> f idx) ()

-- | Strict effectful fold over nodes from last declared to first.
{-# INLINE foldNodeRevM #-}
foldNodeRevM :: NodeArena -> (a -> NodeIdx -> IO a) -> a -> IO a
foldNodeRevM na f z = do
  n <- arenaCount na
  let go !i !acc
        | i < 0 = pure acc
        | otherwise = do
            acc' <- f acc i
            go (i - 1) acc'
  go (n - 1) z

-- ---------------------------------------------------------------------------
-- Frame traversal helpers: forward node scans and child searches, shaped like
-- 'forNodes_' and 'findNodeRevM'.
-- ---------------------------------------------------------------------------

-- | First node, in arena order, satisfying the predicate.
{-# INLINE findNodeM #-}
findNodeM :: NodeArena -> (NodeIdx -> IO Bool) -> IO (Maybe NodeIdx)
findNodeM na p = do
  n <- arenaCount na
  let go !i
        | i >= n = pure Nothing
        | otherwise = do
            ok <- p i
            if ok then pure (Just i) else go (i + 1)
  go 0

-- | Left fold over every node in arena order.
{-# INLINE foldNodesM #-}
foldNodesM :: NodeArena -> (a -> NodeIdx -> IO a) -> a -> IO a
foldNodesM na f z = do
  n <- arenaCount na
  let go !i !acc
        | i >= n = pure acc
        | otherwise = f acc i >>= go (i + 1)
  go 0 z

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
