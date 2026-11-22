{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
-- | Arena storage types, column descriptors, and typed slot accessors.
module NanoUI.Internal.Layout.Arena.Types
  ( NodeIdx
  , IOArr
  , NodeType (..)
  , NodeArenaArrays (..)
  , isWidgetNode
  , packsNodeFont
  , labelFontShift
  , nodeFontBits
  , hasCenteredLabel
  , isContainerNode
  , isScrollNode
  , isFloatingNode
  , NodeClass (..)
  , nodeClassCount
  , SizingTag (..)
  , DirTag (..)
  , NodeArena (..)
  , WidthMemo (..)
  , memoStride
  , IdIndex (..)
  , newIdIndex
  , probeSlot
  , slotNode
  , FlexScratch (..)
  , AxisSnapshot (..)
  , initialCapacity
  , GeomCol (..)
  , StyleCol (..)
  , TagCol (..)
  , TreeCol (..)
  , geomStride
  , styleStride
  , tagStride
  , treeStride
  , readGeom
  , writeGeom
  , readStyle
  , writeStyle
  , readTagEnum
  , writeTagEnum
  , readTree
  , writeTree
  , newNodeArenaArrays
  , newFlexScratch
  , newWidthMemo
  , arenaCount
  , floatingNodeCount
  , layeredNodeCount
  , arenaArrays
  , mixTagged
  , mixInputSig
  , mixPaintSig
  , mixNodeInput
  , growPrimArrayCopy
  , growBoxedStoreCopy
  , AxisSizing (..)
  , axisSizing
  , sizingTag
  , readAxisSizing
  , getParent
  , getFirstChild
  , getNextSibling
  , getNodeType
  , getDirection
  , getWidthSizing
  , getHeightSizing
  , getPadding
  , getScrollContentW
  , setScrollContentW
  , parentIsRow
  , getAlignX
  , getAlignY
  , getFlow
  , isPinnedNode
  , hasPinnedBelow
  , hasScrollerBelow
  , getPointerMode
  , getNodeRect
  , setRect
  , getClipRect
  , getClipBounds
  , setClipRect
  , getText
  , ImageNode (..)
  , noImageNode
  , getImageNode
  , getImageId
  , getOptions
  , getWidgetId
  , getIdSuperseded
  , getNodeValue
  , setSolvedValue
  , getNodeFontSize
  , getNodeFontColor
  , getNodeScope
  , getArenaScope
  , setArenaScope
  , getScopeSignature
  , getInputSignature
  , getStyleIdx
  ) where

import Control.Monad (mfilter)
import Data.Bits (shiftL, shiftR, xor, (.&.))
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Primitive.Array (MutableArray, copyMutableArray, newArray, readArray)
import Data.Primitive.PrimArray
  ( MutablePrimArray
  , newPrimArray
  , readPrimArray
  , resizeMutablePrimArray
  , setPrimArray
  , writePrimArray
  )
import Data.Primitive.Types (Prim)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8, Word32, Word64)
import GHC.Exts (RealWorld)
import NanoUI.Internal.Id (WidgetId (..))
import NanoUI.Internal.Image (ImageLook, defaultImageConfig, imageLook)
import NanoUI.Internal.Style (AlignX (..), AlignY, Flow (..), Padding (..), PointerMode (..), Sizing (..))
import NanoUI.Internal.StyleBits (BitField (..))
import qualified NanoUI.Internal.StyleBits as SB
import NanoUI.Internal.Types (Color (..), Rect (..), newZeroedPrimArray, rectNonEmpty)

-- | A node's position in the arena's arrays. It is valid from the 'addNode'
-- that returned it until the next 'resetNodeArena'. Where an index names an
-- optional node (a parent, a first child, a next sibling), -1 means none.
type NodeIdx = Int

-- | The arena's mutable unboxed arrays.
type IOArr = MutablePrimArray RealWorld

-- | Node kind, which selects how the solver measures the node and how paint
-- draws it. Stored as a 'Word8' ('TagNodeType') through the derived 'Enum'
-- instance, so there can be at most 256 constructors. Several types store
-- type-specific data in the node's style index ('getStyleIdx').
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
  -- a table header, a menu-bar title, a menu item, a checkbox or radio option
  -- (its box or ring painted beside its label), or a tree row.
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
  -- ^ An image. Its image id is 'getImageId', and its style index selects
  -- its look ('getImageNode').
  | NodePanel
  -- ^ A container that paints the theme's panel background and border, and
  -- clips its children to the inside of the border.
  | NodeWindow
  -- ^ A window inside the application's window. Floating.
  | NodeBox
  -- ^ A solid rectangle. Its style index is the fill colour's 32-bit word.
  | NodeColorPicker
  -- ^ One part of a colour picker. Its style index says which part.
  | NodePopup
  -- ^ A popup such as a menu or a tooltip. Floating.
  | NodeDrawing
  -- ^ A custom widget or drawing that the application paints. Its registration
  -- holds drawing behavior and keyboard claims.
  deriving (Eq, Show, Enum, Bounded)

-- | Whether the node is a control the pointer can hover: a button, slider,
-- text field, text area, select, colour picker part or drawing. Hover
-- detection and the layout of widget labels use it. Labels, images, boxes,
-- spacers, separators and containers are not.
isWidgetNode :: NodeType -> Bool
isWidgetNode nt =
  case nt of
    NodeButton -> True
    NodeSlider -> True
    NodeTextInput -> True
    NodeTextArea -> True
    NodeSelect -> True
    NodeColorPicker -> True
    NodeDrawing -> True
    _ -> False

-- | Whether the node packs a font into its style index that the solver and
-- @resolveFontFor@ read: a label or a text field. A text area packs one too,
-- read only by its own paint and spans: its face through
-- 'NanoUI.Internal.Frame.TextArea.resolveTextAreaFont', its weight, slant,
-- decoration and tone through 'NanoUI.Internal.Frame.TextInput.pushFieldText'
-- and 'NanoUI.Internal.Frame.TextInput.fieldTextColor'.
-- Every other type keeps its own data there (a colour picker part, a tab's
-- look), so its style must not be read as a font; a button or select keeps
-- its label's font above that data ('nodeFontBits').
{-# INLINE packsNodeFont #-}
packsNodeFont :: NodeType -> Bool
packsNodeFont nt = nt == NodeText || nt == NodeTextInput

-- | Where a button's or select's style index keeps its label's font, packed
-- as a text node packs one: above the 32 bits its own data uses
-- ('SB.labelFont').
{-# INLINE labelFontShift #-}
labelFontShift :: Int
labelFontShift = bfShift SB.labelFont

-- | The font bits of a node's style index @si@, in a text node's packing:
-- all of a text node's or text field's, the label font of a button or select
-- ('labelFontShift'), and none for any other type.
{-# INLINE nodeFontBits #-}
nodeFontBits :: NodeType -> Int -> Int
nodeFontBits nt si
  | packsNodeFont nt = si
  | hasCenteredLabel nt = si `shiftR` labelFontShift
  | otherwise = 0

-- | Whether the node paints one line of label text centered vertically in its
-- box: a button or select. The solver takes that line's baseline as the
-- node's baseline when a row aligns its children on their baselines, and
-- "NanoUI.Internal.Frame.Spans" caches where the label goes
-- (@computeWidgetLabel@).
hasCenteredLabel :: NodeType -> Bool
hasCenteredLabel nt = nt == NodeButton || nt == NodeSelect

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
  -- ^ Nodes a pointer hit test may stop at: controls ('isWidgetNode'),
  -- scroll containers ('isScrollNode') and nodes with 'PointerBlock'.
  | DrawingNodes
  -- ^ Widgets the application draws ('NodeDrawing').
  | FloatingNodes
  -- ^ Windows, modals and popups ('isFloatingNode'), so the passes that look
  -- only at floating panels skip the rest of the arena.
  | BackdropNodes
  -- ^ Panels and scroll containers. Each paints a backdrop and clips its
  -- content, so the damage pass tracks them
  -- ('NanoUI.Internal.Damage.updatePrevRects').
  | LayeredNodes
  -- ^ Layered containers and pinned nodes. Only here does paint draw a node
  -- over an earlier sibling ('forChildrenInPaintOrder_'), so only here can
  -- the later of two overlapping widgets get the pointer ('layeredNodeCount').
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
-- vertical. A layered container ('TagFlow') stores 'DirColumn', so code that
-- reads the direction as an axis (a separator, a text wrap) sees a column.
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
  -- ^ Layout inputs and a few other numbers, 20 floats per node. See
  -- 'StyleCol'.
  , naArrTags :: !(IOArr Word8)
  -- ^ Enum values, 16 bytes per node. See 'TagCol'.
  , naArrTree :: !(IOArr Int)
  -- ^ Tree links and other integers, 8 per node. See 'TreeCol'.
  , naArrTextStore :: !(MutableArray RealWorld Text)
  -- ^ Each node's text. 'addNode' does not clear a slot, so read it with
  -- 'getText', which checks that the node set a text.
  , naArrOptionsStore :: !(MutableArray RealWorld [Text])
  -- ^ Each node's option list ('getOptions'), empty unless the node set one.
  , naArrOptionsLast :: !(MutableArray RealWorld [Text])
  -- ^ The option list each node last hashed ('setOptions'), which 'addNode'
  -- leaves alone, so the same list next frame reuses its hash.
  , naArrFontColor :: !(IOArr Int)
  -- ^ Each node's font colour: 0 for none, and otherwise the colour's 32-bit
  -- word with bit 32 set. See 'getNodeFontColor'.
  , naArrScope :: !(IOArr Int)
  -- ^ The scope each node was added under: the theme that a @styled@ or
  -- @disabledWhen@ block around the node selected, whether the node is
  -- disabled, and whether it is inert. Bit 0 is the disabled flag, bit 1 is
  -- the inert flag, and the theme index sits above bit 1 (0 for the context's
  -- theme). See 'NanoUI.Internal.Context.Types.ThemeScopes'.
  , naArrImageId :: !(IOArr Int)
  -- ^ Each image node's image id ('setImageId'). Only image nodes set it.
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
  , naIndex :: IORef IdIndex
  -- ^ Node index by widget id, for 'lookupNodeByWidgetId'.
  , naScope :: IORef Int
  -- ^ The paint scope 'addNode' gives new nodes, encoded as in 'naArrScope'.
  , naScopeSig :: IORef Word64
  -- ^ A hash over the index and scope of every node added under a scope other
  -- than 0 since the reset. See 'getScopeSignature'.
  , naPaintSig :: IOArr Word64
  -- ^ Hash of the paint state the input signature leaves out, mixed as it is
  -- written ('getPaintSignature'). One unboxed slot, like 'naInputSig'.
  , naInputSig :: IOArr Word64
  -- ^ The frame's input signature, which 'layoutSigMatches' compares. An
  -- unboxed slot, so mixing into it allocates nothing.
  , naTextHash :: IORef (IOArr Word64)
  -- ^ Per node, the hash of the text the last 'setNodeText' stored.
  , naOptionsHash :: IORef (IOArr Word64)
  -- ^ Per node, the hash of the option list the last 'setOptions' stored.
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
  , naOffered :: IORef (IOArr Float)
  -- ^ Per node, the rect the position pass first offered it (four floats):
  -- NaN until then, and a NaN width once it is offered one again.
  -- 'captureLayoutCache' snapshots it.
  , naUnsnapped :: IORef (IOArr Float)
  -- ^ Per node, its rect as the position pass left it, before quantizing
  -- (four floats). 'captureLayoutCache' snapshots it: a parent sizes itself
  -- from its children's unsnapped extents, so a subtree the solve puts back
  -- must be put back as that.
  , naSpans :: IORef (IOArr Int)
  -- ^ Per node, one past the last node of its subtree when the captured
  -- solve placed that subtree as this solve would at the same rect, and -1
  -- otherwise. The measure pass writes it when there is a cache.
  , naClassNodes :: IORef (IOArr Int)
  -- ^ The node lists of 'NodeClass', one after another: class @c@ keeps its
  -- @i@th node at element @fromEnum c * capacity + i@. A list never holds
  -- more nodes than the arena, so each has room for 'naCapacity' of them.
  , naClassCounts :: IOArr Int
  -- ^ Nodes in each list of 'naClassNodes', by 'fromEnum' of the class.
  , naImages :: IORef (MutableArray RealWorld ImageNode)
  -- ^ Looks of the image nodes that are not plain (fitted, faded, rotated
  -- and so on), in 'setImageNode' order. The first 'naImageCount' belong to
  -- this frame. Grows on demand.
  , naImageCount :: IOArr Int
  -- ^ Entries of 'naImages' used this frame, in one slot.
  , naHighWater :: IORef Int
  -- ^ High-water node count whose boxed text/option slots have not been cleared.
  }

-- | An image node's look ('ImageLook') and the image's natural size, used on
-- an axis the layout leaves unsized. A plain image node has no entry: it
-- stretches the image, and an unsized axis takes its minimum or 32. The
-- node's style index is its 'naImages' position plus one, or 0 when plain.
data ImageNode = ImageNode
  { inLook :: !ImageLook
  , inWidth :: {-# UNPACK #-} !Float
  , inHeight :: {-# UNPACK #-} !Float
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
geomStride = 9
styleStride = 20
tagStride = 16
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
-- * 'GeomLaidW': the width the node was laid out at, before
--   'NanoUI.Internal.Layout.Solve.snapNode' snapped its edges to the device
--   pixel grid. Text decides where it wraps at this width in layout and in
--   paint, so both break it into the same lines, wherever its edges snap.
data GeomCol
  = GeomX | GeomY | GeomW | GeomH
  | GeomClipX | GeomClipY | GeomClipW | GeomClipH
  | GeomLaidW
  deriving (Enum, Bounded)

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
-- * 'StyleLineGap': the space between a wrapping container's lines.
-- * 'StylePinX', 'StylePinY': a pinned node's offset from its parent's
--   content box.
-- * 'StyleAspect': width over height for a fit height
--   ('NanoUI.aspect'), or 0 for none.
--
-- The layout cache does not compare 'StyleScrollContentW' and
-- 'StyleNodeValue', which are not layout inputs.
data StyleCol
  = StyleWVal | StyleHVal
  | StylePadL | StylePadR | StylePadT | StylePadB
  | StyleGap | StyleMinW | StyleMinH | StyleMaxW | StyleMaxH
  | StyleScrollContentW | StyleNodeValue | StyleGridMinColW | StyleFontSize
  | StyleLineGap | StylePinX | StylePinY | StyleAspect
  deriving (Enum, Bounded)

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
-- * 'TagIdSuperseded': whether a later node this frame reuses this node's
--   widget id, as a table's scrolling pane reuses its frozen pane's
--   ('setWidgetId', 'getIdSuperseded').
-- * 'TagFlow': the 'Flow'. Always 'Line' for scroll containers, grids and
--   non-containers.
-- * 'TagLineAlign': a wrapping container's
--   'NanoUI.LineAlign'.
-- * 'TagPinned': whether the node is pinned, as a 'Bool'.
-- * 'TagPinnedBelow': whether some descendant is pinned, as a 'Bool', set
--   when that node is added ('hasPinnedBelow'). The solver, paint and hit
--   tests look for pinned children only under such a node.
-- * 'TagPointer': the node's effective 'PointerMode' ('getPointerMode').
-- * 'TagScrollerBelow': whether the node or a descendant is a scroll
--   container, a text area or a 'PointerBlock' node, as a 'Bool', set when
--   that node is added ('hasScrollerBelow'). Wheel routing looks for its
--   target only under such a node.
-- * 'TagSteadyHeight': whether the node's height is the same at every width
--   it can be laid out at, as a 'Bool'. The solver's measure pass writes it
--   ('NanoUI.Internal.Layout.Solve.Measure.markSteadyHeight'); 'False' until
--   then, which only costs a refit.
data TagCol
  = TagNodeType | TagDirection | TagWSizing | TagHSizing
  | TagScrollBarSlot | TagAlignX | TagAlignY | TagIdSuperseded
  | TagFlow | TagPinned | TagPinnedBelow | TagPointer | TagLineAlign
  | TagScrollerBelow | TagSteadyHeight
  deriving (Enum, Bounded)

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
  deriving (Enum, Bounded)

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
  naArrOptionsLast <- newArray cap []
  naArrFontColor <- newPrimArray cap
  naArrScope <- newPrimArray cap
  naArrImageId <- newPrimArray cap
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
  wmTags <- newZeroedPrimArray cap
  wmSlots <- newPrimArray (cap * memoStride)
  pure WidthMemo {..}


-- | Number of entries in 'NodeClass'.
nodeClassCount :: Int
nodeClassCount = fromEnum (maxBound :: NodeClass) + 1

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

-- | Fold node @idx@'s paint state under @tag@ into 'naPaintSig'. The index
-- goes in with the tag, so the same value on another node mixes differently.
{-# INLINE mixPaintSig #-}
mixPaintSig :: NodeArena -> NodeIdx -> Word64 -> Word64 -> IO ()
mixPaintSig na idx tag v = do
  sig <- readPrimArray (naPaintSig na) 0
  writePrimArray (naPaintSig na) 0 (mixTagged sig (tag `xor` (fromIntegral idx `shiftL` 16)) v)

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

-- | Number of layered containers and pinned nodes added since the last reset
-- ('LayeredNodes'). While it is 0, the earlier of two overlapping nodes is
-- drawn on top.
{-# INLINE layeredNodeCount #-}
layeredNodeCount :: NodeArena -> IO Int
layeredNodeCount na = readPrimArray (naClassCounts na) (fromEnum LayeredNodes)

-- | Live node count. Valid indices are 0 through count minus one.
{-# INLINE arenaCount #-}
arenaCount :: NodeArena -> IO Int
arenaCount na = readIORef (naCount na)

-- | Current backing arrays. Do not retain them across arena growth or reset.
{-# INLINE arenaArrays #-}
arenaArrays :: NodeArena -> IO NodeArenaArrays
arenaArrays na = readIORef (naArrays na)

-- | Resize a mutable primitive array to @newCap@, filling slots @[cap .. newCap - 1]@ with @defVal@.
{-# NOINLINE growPrimArrayCopy #-}
growPrimArrayCopy :: Prim a => IOArr a -> Int -> Int -> a -> IO (IOArr a)
growPrimArrayCopy oldArr cap newCap defVal = do
  newArr <- resizeMutablePrimArray oldArr newCap
  setPrimArray newArr cap (newCap - cap) defVal
  pure newArr

-- | Allocate a new boxed array of size @newCap@ and copy @oldCap@ entries into it.
{-# NOINLINE growBoxedStoreCopy #-}
growBoxedStoreCopy :: a -> MutableArray RealWorld a -> Int -> Int -> IO (MutableArray RealWorld a)
growBoxedStoreCopy emptyVal arr oldCap newCap = do
  newArr <- newArray newCap emptyVal
  copyMutableArray newArr 0 arr 0 oldCap
  pure newArr

-- | Rebuild an axis's 'Sizing' from its stored tag and value.
axisSizing :: AxisSizing -> Sizing
axisSizing (AxisSizing tag val _ _) = case tag of
  SizingFixed -> Fixed val
  SizingFit -> Fit
  SizingGrow -> Grow val
  SizingShrink -> Shrink val
  SizingPercent -> Percent val

-- | Decompose a 'Sizing' into its arena tag and scalar value.
{-# INLINE sizingTag #-}
sizingTag :: Sizing -> (SizingTag, Float)
sizingTag (Fixed v) = (SizingFixed, v)
sizingTag Fit = (SizingFit, 0)
sizingTag (Grow g) = (SizingGrow, g)
sizingTag (Shrink s) = (SizingShrink, s)
sizingTag (Percent p) = (SizingPercent, p)

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

-- | How the node lays out its flow children.
{-# INLINE getFlow #-}
getFlow :: NodeArena -> NodeIdx -> IO Flow
getFlow na idx = arenaArrays na >>= \a -> readTagEnum a idx TagFlow

-- | Whether the node is pinned: out of its parent's flow, at an offset from
-- the parent's content box ('StylePinX', 'StylePinY').
{-# INLINE isPinnedNode #-}
isPinnedNode :: NodeArena -> NodeIdx -> IO Bool
isPinnedNode na idx = arenaArrays na >>= \a -> readTagEnum a idx TagPinned

-- | Whether any descendant is pinned. A pinned node can sit outside its
-- parent, so paint searches for it even under a container it would skip as
-- out of view.
{-# INLINE hasPinnedBelow #-}
hasPinnedBelow :: NodeArena -> NodeIdx -> IO Bool
hasPinnedBelow na idx = arenaArrays na >>= \a -> readTagEnum a idx TagPinnedBelow

-- | Whether the node or a descendant is a scroll container, a text area or
-- a 'PointerBlock' node: whether its subtree can take or block the wheel.
{-# INLINE hasScrollerBelow #-}
hasScrollerBelow :: NodeArena -> NodeIdx -> IO Bool
hasScrollerBelow na idx = arenaArrays na >>= \a -> readTagEnum a idx TagScrollerBelow

-- | The node's own 'PointerMode', or 'PointerPass' when an ancestor passes
-- the pointer.
{-# INLINE getPointerMode #-}
getPointerMode :: NodeArena -> NodeIdx -> IO PointerMode
getPointerMode na idx = arenaArrays na >>= \a -> readTagEnum a idx TagPointer

-- | Current x, y, width, height in logical pixels. After scroll offsets are
-- applied, the origin is in window coordinates; before layout it is unset.
{-# INLINE getNodeRect #-}
getNodeRect :: NodeArena -> NodeIdx -> IO Rect
getNodeRect na idx = do
  a <- arenaArrays na
  Rect <$> readGeom a idx GeomX <*> readGeom a idx GeomY <*> readGeom a idx GeomW <*> readGeom a idx GeomH

-- | Write x, y, width and height. The clip ('setClipRect') is left alone.
{-# INLINE setRect #-}
setRect :: NodeArena -> NodeIdx -> Float -> Float -> Float -> Float -> IO ()
setRect na idx x y w h = do
  a <- arenaArrays na
  writeGeom a idx GeomX x
  writeGeom a idx GeomY y
  writeGeom a idx GeomW w
  writeGeom a idx GeomH h

-- | Positive-area clip in logical window coordinates. 'Nothing' means the
-- stored clip is empty or unset; 'getClipBounds' tells them apart.
{-# INLINE getClipRect #-}
getClipRect :: NodeArena -> NodeIdx -> IO (Maybe Rect)
getClipRect na idx = mfilter rectNonEmpty <$> getClipBounds na idx

-- | Store a clip in logical window coordinates. A clip without area is
-- stored as empty, which differs from the unset clip 'addNode' leaves.
-- 'getClipRect' reads both as 'Nothing'; 'getClipBounds' does not.
{-# INLINE setClipRect #-}
setClipRect :: NodeArena -> NodeIdx -> Rect -> IO ()
setClipRect na idx (Rect x y w h) = do
  a <- arenaArrays na
  let empty = not (w > 0 && h > 0)
  writeGeom a idx GeomClipX x
  writeGeom a idx GeomClipY y
  writeGeom a idx GeomClipW (if empty then -1 else w)
  writeGeom a idx GeomClipH (if empty then -1 else h)

-- | The stored clip in logical window coordinates. An empty clip reads as a
-- zero-size rect. 'Nothing' means unset, as before
-- 'NanoUI.Internal.Frame.Scroll.applyScrollOffsets' runs in a frame.
{-# INLINE getClipBounds #-}
getClipBounds :: NodeArena -> NodeIdx -> IO (Maybe Rect)
getClipBounds na idx = do
  a <- arenaArrays na
  x <- readGeom a idx GeomClipX
  y <- readGeom a idx GeomClipY
  w <- readGeom a idx GeomClipW
  h <- readGeom a idx GeomClipH
  pure $
    if w > 0 && h > 0
      then Just (Rect x y w h)
      else if w < 0 then Just (Rect x y 0 0) else Nothing


-- | Node text, or empty text when no text was assigned this frame.
{-# INLINE getText #-}
getText :: NodeArena -> NodeIdx -> IO Text
getText na idx = do
  a <- arenaArrays na
  ti <- readTree a idx TreeTextIdx
  if ti < 0
    then pure T.empty
    else readArray (naArrTextStore a) ti

-- | Image node @idx@'s look and natural size, or 'Nothing' for a plain
-- image. Only valid on a 'NodeImage'; other nodes use the style index
-- differently.
{-# INLINE getImageNode #-}
getImageNode :: NodeArena -> NodeIdx -> IO (Maybe ImageNode)
getImageNode na idx = do
  si <- arenaArrays na >>= \a -> readTree a idx TreeStyleIdx
  if si <= 0 then pure Nothing else Just <$> (readIORef (naImages na) >>= \arr -> readArray arr (si - 1))

-- | Image node @idx@'s image id ('setImageId'). Only valid on a 'NodeImage'.
{-# INLINE getImageId #-}
getImageId :: NodeArena -> NodeIdx -> IO Int
getImageId na idx = arenaArrays na >>= \a -> readPrimArray (naArrImageId a) idx

-- | Filler for unused slots of 'naImages'.
noImageNode :: ImageNode
noImageNode = ImageNode (imageLook defaultImageConfig (Color 0xFFFFFFFF)) 0 0

-- | Choices stored on a select node, or an empty list when none were assigned.
{-# INLINE getOptions #-}
getOptions :: NodeArena -> NodeIdx -> IO [Text]
getOptions na idx = arenaArrays na >>= \a -> readArray (naArrOptionsStore a) idx

-- | Identity assigned to the node, or @WidgetId 0@ for an untagged node.
{-# INLINE getWidgetId #-}
getWidgetId :: NodeArena -> NodeIdx -> IO WidgetId
getWidgetId na idx = arenaArrays na >>= \a -> WidgetId . fromIntegral <$> readTree a idx TreeWidgetId

-- | Whether a later node this frame reuses this node's widget id
-- ('setWidgetId'). 'lookupNodeByWidgetId' returns the last such node, and
-- arena walks that key results by id should skip superseded nodes.
{-# INLINE getIdSuperseded #-}
getIdSuperseded :: NodeArena -> NodeIdx -> IO Bool
getIdSuperseded na idx = arenaArrays na >>= \a -> readTagEnum a idx TagIdSuperseded

-- | Node index by widget id, as an open-addressing table. Each slot is two
-- words: the id, and a value packing the epoch (high 32 bits) with the node
-- index (low 32 bits). A slot from another epoch counts as free, so bumping
-- the epoch empties the table without touching it. Entries are never
-- removed within a frame, so a probe can stop at the first free slot.
--
-- Fields: the slots, the slot count (a power of two) minus one, and the
-- live entry count for the current epoch in one slot.
data IdIndex = IdIndex {-# UNPACK #-} !(IOArr Word64) {-# UNPACK #-} !Int {-# UNPACK #-} !(IOArr Int)

-- | An empty index of @n@ slots, a power of two.
newIdIndex :: Int -> IO IdIndex
newIdIndex n = do
  slots <- newZeroedPrimArray (2 * n)
  live <- newZeroedPrimArray 1
  pure (IdIndex slots (n - 1) live)

-- | Probe for @key@ in epoch @ep@. Calls @found s v@ with the matching slot
-- and its value, or @free s@ with the free slot where the probe stopped.
-- Ids are already hashes; the multiply spreads their bits so even a small
-- table uses them all.
{-# INLINE probeSlot #-}
probeSlot :: IOArr Word64 -> Int -> Word32 -> Word64 -> (Int -> IO r) -> (Int -> Word64 -> IO r) -> IO r
probeSlot slots mask ep key free found = go (fromIntegral ((key * 0x9E3779B97F4A7C15) `shiftR` 32) .&. mask)
  where
    go !s = do
      v <- readPrimArray slots (2 * s + 1)
      if v `shiftR` 32 /= fromIntegral ep
        then free s
        else do
          k <- readPrimArray slots (2 * s)
          if k == key then found s v else go ((s + 1) .&. mask)

-- | The node index in a slot's value.
{-# INLINE slotNode #-}
slotNode :: Word64 -> Int
slotNode v = fromIntegral (v .&. 0xFFFFFFFF)

-- | Type-specific numeric value, such as selection state or a scroller's
-- solved content height. Interpret it according to 'getNodeType'.
{-# INLINE getNodeValue #-}
getNodeValue :: NodeArena -> NodeIdx -> IO Float
getNodeValue na idx = arenaArrays na >>= \a -> readStyle a idx StyleNodeValue

-- | 'setNodeValue' for a value the solver works out from the layout, such as
-- a scroller's content extent, which the layout inputs already cover.
{-# INLINE setSolvedValue #-}
setSolvedValue :: NodeArena -> NodeIdx -> Float -> IO ()
setSolvedValue na idx v = arenaArrays na >>= \a -> writeStyle a idx StyleNodeValue v

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

-- | Packed scope: theme index above bit 1, inert flag in bit 1, disabled flag in bit 0.
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

-- | Type-specific style code. Its encoding depends on 'getNodeType', such as
-- button flags, a radio option index, or packed text styling.
{-# INLINE getStyleIdx #-}
getStyleIdx :: NodeArena -> NodeIdx -> IO Int
getStyleIdx na idx = arenaArrays na >>= \a -> readTree a idx TreeStyleIdx
