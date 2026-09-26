{-# LANGUAGE StrictData #-}

-- | Record types behind 'Context': interaction, damage, overlay, animation,
-- scroll and drawing-cache state, theme scopes, and frame messages.
module NanoUI.Internal.Context.Types
  ( Context (..)
  , GenCache (..)
  , emptyGenCache
  , insertGen
  , cachedGen
  , MeasureCache
  , WrapCache (..)
  , MetricSource (..)
  , TextInputMenu (..)
  , TextInputDrag (..)
  , TextFieldClickCell (..)
  , WindowResizeEdge (..)
  , WindowResizeDrag (..)
  , DamageRequest (..)
  , DamageState (..)
  , initialDamageState
  , PrevFrame (..)
  , emptyPrevFrame
  , OverlayState (..)
  , initialOverlayState
  , ExplainState (..)
  , initialExplainState
  , ExplainedNode (..)
  , AnimationState (..)
  , initialAnimationState
  , ScrollTuning (..)
  , defaultScrollTuning
  , ScrollAxes (..)
  , ScrollGlide (..)
  , ScrollState (..)
  , initialScrollState
  , DrawFitCache (..)
  , DrawingEntry (..)
  , DrawingCacheState (..)
  , PopupConfig (..)
  , DrawOpCacheEntry (..)
  , CustomDrawingEntry (..)
  , CustomDrawOpCacheEntry (..)
  , SpanCacheEntry (..)
  , SpanLines (..)
  , WidgetTextCacheEntry (..)
  , WidgetTextPlacement (..)
  , initialDrawingCacheState
  , InteractionState (..)
  , PointerRoute (..)
  , initialInteractionState
  , CustomMeasureFn
  , CustomDrawContext (..)
  , CustomDrawBuild
  , ThemeScopes (..)
  , FrameMsg (..)
  , decodeMessages
  , reduceMessages
  , reduceUpdates
  , intKey
  ) where

import Data.Dynamic (Dynamic)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import Data.IORef (IORef, modifyIORef', readIORef, writeIORef)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.Map.Strict (Map)
import Data.Primitive.PrimArray (MutablePrimArray)
import Data.Primitive.SmallArray (SmallArray, SmallMutableArray)
import Data.Word (Word64)
import Data.Text (Text)
import Data.Typeable (TypeRep, Typeable, cast)
import GHC.Exts (RealWorld)

import NanoUI.Internal.Animation (Animation)
import NanoUI.Internal.Atlas (ImageAtlas)
import NanoUI.Internal.Draw.Types (DrawArena, DrawOp, DrawingBuild)
import NanoUI.Internal.Font (CustomMeasureFn, FontMetrics, WrapResult)
import NanoUI.Internal.Frame.SpanArena (SpanArena)
import NanoUI.Internal.Id (IdContext, WidgetId, hashWidgetId)
import NanoUI.Internal.Input (UiCursorKind)
import NanoUI.Internal.Layout.Arena (DirTag, LayoutCache, NodeArena)
import NanoUI.Internal.Store (WidgetStore)
import NanoUI.Internal.Style (FontStyle, FontVariant, FontWeight, Layout, Padding, Theme)
import NanoUI.Widgets.TextBuffer (Cursor)
import NanoUI.Widgets.TextCommand (TextCommand)
import NanoUI.Internal.Types

-- | Themes the view's @styled@ scopes pushed this frame, and last frame's, to
-- tell whether a frame changed only how its scopes look. A node's scope holds
-- an index into 'tsThemes' plus one; index 0 is the context theme.
data ThemeScopes = ThemeScopes
  { tsCount :: {-# UNPACK #-} !Int
  , tsThemes :: !(SmallMutableArray RealWorld Theme)
  -- ^ What each scope is drawn with.
  , tsRaw :: !(SmallMutableArray RealWorld Theme)
  -- ^ Each scope's theme before a disabled scope faded it, which nested
  -- @styled@ scopes modify.
  , tsPrevCount :: {-# UNPACK #-} !Int
  , tsPrev :: !(SmallMutableArray RealWorld Theme)
  , tsDisabled :: !Bool
  -- ^ A disabled scope was entered this pass, so some widget may be disabled.
  , tsChanged :: !Bool
  -- ^ A pushed theme differs from the one at its index last frame.
  , tsPrevSig :: {-# UNPACK #-} !Word64
  -- ^ Last frame's scope signature ('NanoUI.Internal.Layout.Arena.getScopeSignature').
  }

-- | A runtime-typed message emitted by a view. Reducers select messages by type.
data FrameMsg where
  FrameMsg :: Typeable a => a -> FrameMsg

-- | Extract messages of the requested type in traversal order; skip other types.
decodeMessages :: (Foldable f, Typeable a) => f FrameMsg -> [a]
decodeMessages = foldr (\(FrameMsg x) rest -> maybe rest (: rest) (cast x)) []

-- | Strictly fold matching messages through an update function in traversal order.
reduceMessages :: (Foldable f, Typeable msg) => (msg -> model -> model) -> model -> f FrameMsg -> model
reduceMessages update = foldl' (\model (FrameMsg x) -> maybe model (`update` model) (cast x))

-- | Apply messages whose type is @model -> model@, in traversal order.
reduceUpdates :: (Foldable f, Typeable model) => model -> f FrameMsg -> model
reduceUpdates = reduceMessages ($)

-- | A memo table in two generations: the young map, its size, and the old
-- map. A full young map replaces the old one, so an entry that stops being
-- used (a clock's text, a log line) is dropped while one used every
-- generation stays. It is a cheap stand-in for least recently used: inserts
-- go to the young map and a hit in the old map moves the entry up, so an
-- entry survives however many one-off keys (wrap probes, table cells) pass
-- through. Plain insertion order evicted the oldest key even when every frame
-- used it. Hashing a text key once beats comparing it at every level of a
-- tree.
data GenCache k v = GenCache !(HashMap k v) !Int !(HashMap k v)

emptyGenCache :: GenCache k v
emptyGenCache = GenCache HashMap.empty 0 HashMap.empty

-- | Insert an entry of weight @w@ into a cache of @cap@ per generation. The
-- young map's size is the sum of its entries' weights; an entry heavier than
-- a whole generation is not kept.
{-# INLINE insertGen #-}
insertGen :: Hashable k => Int -> Int -> k -> v -> GenCache k v -> GenCache k v
insertGen cap w k v cache@(GenCache young n old)
  | w > cap = cache
  | n + w > cap = GenCache (HashMap.singleton k v) w young
  | otherwise = GenCache (HashMap.insert k v young) (n + w) old

-- | The entry for @k@, or what @make@ returns, which is kept. Entries weigh
-- @w k@; an old one used again moves to the young map. Inlined so a hit
-- returns the stored value without boxing it in a 'Just' on the way: the SDL
-- bench's warm-lookup gate measures exactly that.
{-# INLINE cachedGen #-}
cachedGen :: Hashable k => Int -> (k -> Int) -> IORef (GenCache k v) -> k -> IO v -> IO v
cachedGen cap w ref k make = do
  cache@(GenCache young _ old) <- readIORef ref
  case HashMap.lookup k young of
    Just v -> pure v
    Nothing -> case HashMap.lookup k old of
      Just v -> do
        writeIORef ref $! insertGen cap (w k) k v cache
        pure v
      Nothing -> do
        v <- make
        modifyIORef' ref (insertGen cap (w k) k v)
        pure v

-- | Memoised measurements by text and measurement scale.
type MeasureCache = GenCache (Text, Float) (Float, Float)

-- | Wrapped text by text and font ('NanoUI.Internal.WidgetText.textNodeFontKey'),
-- each with the widths it holds for, newest first, under the metric generation
-- the results were measured in.
data WrapCache = WrapCache !Int !(GenCache (Text, Int) [WrapResult])

-- | Identity of a font/measurement configuration. Pure Context modifiers
-- replace this value; the next frame invalidates shared caches if its identity
-- differs. Holding the current inputs (not a revision counter/history) also
-- distinguishes two differently configured Contexts derived from one parent.
data MetricSource
  = InitialMetricSource
  | MetricSource
      !FontMetrics
      !FontMetrics
      !(Text -> IO (Float, Float))
      !(Float -> FontWeight -> FontStyle -> FontVariant -> IO (FontMetrics, Bool))
      !(Float -> FontWeight -> FontStyle -> FontVariant -> Text -> IO (Float, Float))

-- | Explicit damage invalidation request queued during frame evaluation.
data DamageRequest
  = ReqWidget !WidgetId !DamageBounds      -- ^ Invalidate widget layout bounds (old & new)
  | ReqKey !Int !DamageBounds              -- ^ Invalidate widget bounds by integer key
  | ReqRect !Rect                          -- ^ Invalidate an explicit window-space rectangle
  | ReqPeers ![WidgetId] !DamageBounds     -- ^ Invalidate a collection of widgets
  | ReqFull                                -- ^ Force full window invalidation
  deriving (Eq, Show)

-- | Text field owning the edit menu and its logical window-space bounds.
data TextInputMenu = TextInputMenu
  { textInputMenuWidget :: WidgetId
  , textInputMenuRect :: Rect
  }
  deriving (Eq, Show)

-- | Selection-drag anchor, a position in the field's document (row 0 in a
-- single-line field). Click count selects character, word, or line selection
-- behaviour.
data TextInputDrag = TextInputDrag
  { textInputDragWidget :: WidgetId
  , textInputDragAnchor :: {-# UNPACK #-} !Cursor
  , textInputDragClicks :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Show)

-- | Text position of the last click, used to group repeated clicks on one cell.
-- Positions are character indices, not UTF-8 byte offsets.
data TextFieldClickCell = TextFieldClickCell
  { textFieldClickWidget :: WidgetId
  , textFieldClickAt :: {-# UNPACK #-} !Cursor
  }
  deriving (Eq, Show)

-- | Edge or corner captured by a floating-window resize gesture, as the side
-- of each axis it moves, horizontal then vertical: -1 the left or top, 1 the
-- right or bottom, 0 neither.
data WindowResizeEdge = WindowResizeEdge Int Int
  deriving (Eq, Show)

-- | Resize gesture's initial pointer position, window bounds, and size limits.
-- All coordinates and lengths use logical pixels.
data WindowResizeDrag = WindowResizeDrag
  { wrdWidget :: WidgetId
  , wrdEdge :: WindowResizeEdge
  , wrdGrabX :: {-# UNPACK #-} !Float
  , wrdGrabY :: {-# UNPACK #-} !Float
  , wrdStartX :: {-# UNPACK #-} !Float
  , wrdStartY :: {-# UNPACK #-} !Float
  , wrdStartW :: {-# UNPACK #-} !Float
  , wrdStartH :: {-# UNPACK #-} !Float
  , wrdMinW :: {-# UNPACK #-} !Float
  , wrdMinH :: {-# UNPACK #-} !Float
  , wrdMaxW :: {-# UNPACK #-} !Float
  , wrdMaxH :: {-# UNPACK #-} !Float
  }
  deriving (Eq, Show)

-- | Frame scheduling and repaint state, including previous visible geometry
-- keyed by widget id. Dirty state and pixel damage are tracked separately.
data DamageState = DamageState
  { dsDirty :: !Bool
  , dsDirtyOpaque :: !Bool
  -- ^ Some request since the last clear came through 'markDirty', whose
  -- effects no diff is known to cover. 'markDirtyCovered' leaves it alone,
  -- so a covered request never downgrades an opaque one.
  , dsDamage :: !Damage
  , dsDamagePieces :: ![Rect]
  -- ^ Disjoint rects inside a 'DamageClip' that together hold all of its
  -- damage, when two or more are much smaller than the clip; otherwise empty,
  -- and the clip is the one piece.
  , dsRequests :: ![DamageRequest]
  , dsLastWindowSize :: !Size
  , dsPrev :: !PrevFrame
  }

-- | What the last finished frame laid out, by widget key
-- ('NanoUI.Internal.Damage.updatePrevRects'), for the next frame's damage to
-- diff against.
data PrevFrame = PrevFrame
  { pfRects :: !(IntMap Rect)
  , pfClips :: !(IntMap Rect)
  -- ^ The viewport clip of each key in 'pfRects' that has one.
  , pfOuterClips :: !(IntMap Rect)
  -- ^ For scroll containers and panels, the clip each is painted in: the one
  -- around it, where 'pfClips' holds the smaller one it gives its content.
  , pfTexts :: !(IntMap Text)
  -- ^ The text of text and image nodes.
  }

emptyPrevFrame :: PrevFrame
emptyPrevFrame = PrevFrame IM.empty IM.empty IM.empty IM.empty

-- | Require a first frame and full repaint, with no previous geometry.
initialDamageState :: DamageState
initialDamageState = DamageState
  { dsDirty = True
  , dsDirtyOpaque = False
  , dsDamage = DamageFull
  , dsDamagePieces = []
  , dsRequests = []
  , dsLastWindowSize = Size 0 0
  , dsPrev = emptyPrevFrame
  }

-- | Current modal nesting and previous floating-panel bounds/order used for
-- routing input before the next view is built.
data OverlayState = OverlayState
  { osModalWasActive :: {-# UNPACK #-} !Bool
  , osModalActive :: {-# UNPACK #-} !Bool
  , osModalDepth :: {-# UNPACK #-} !Int
  , osEscapeConsumed :: {-# UNPACK #-} !Bool
  , osTabConsumed :: {-# UNPACK #-} !Bool
  , osPrevFloatingRects :: !(IntMap Rect)
  , osPrevFloatingOrder :: ![Int]
  -- | Painted bounds of the open dropdowns and text-edit menu last frame.
  , osPrevMenuRects :: ![Rect]
  }

-- | No modals, floating panels, or consumed Escape or Tab event.
initialOverlayState :: OverlayState
initialOverlayState = OverlayState
  { osModalWasActive = False
  , osModalActive = False
  , osModalDepth = 0
  , osEscapeConsumed = False
  , osTabConsumed = False
  , osPrevFloatingRects = IM.empty
  , osPrevFloatingOrder = []
  , osPrevMenuRects = []
  }

-- | The layout overlay ('NanoUI.Internal.Frame.Explain'): whether it is on,
-- and what the last frame drew with it on, to diff the next frame against.
data ExplainState = ExplainState
  { esOn :: !Bool
  , esLayers :: ![(Int, [(Rect, Rect, Int)])]
  -- ^ Each layer, the page's (keyed -1) first and then the floating panels'
  -- (keyed by their root node) in paint order, with the outlines drawn over
  -- it: each node's rect, the clip it was cut to, and its depth in the layer.
  , esHover :: !(Maybe (ExplainedNode, Rect))
  -- ^ The node under the pointer, and the clip its highlight was cut to.
  }

-- | The overlay off.
initialExplainState :: ExplainState
initialExplainState = ExplainState {esOn = False, esLayers = [], esHover = Nothing}

-- | The layout node under the pointer while the layout overlay is on: the
-- innermost node there in the topmost layer, the page or a floating panel.
data ExplainedNode = ExplainedNode
  { explainedKind :: !Text
  -- ^ What the node is: @Container@, @Text@, @Button@, @ScrollContainer@
  -- and so on, with how a container lays out its children after a comma:
  -- its direction (@Container, row@), @stack@ for a stack
  -- (@Container, stack@), and @wrap@ after the direction of one that wraps
  -- (@Container, row, wrap@).
  , explainedDepth :: !Int
  -- ^ How many nodes it is inside, counted from the root of its layer, which
  -- is 0. The overlay colours its outline by this.
  , explainedRect :: !Rect
  -- ^ Where it was laid out, in logical window coordinates after scrolling.
  , explainedPadding :: !Padding
  -- ^ Its padding. The content box is the rect less this.
  }
  deriving (Eq, Show)

-- | Running animations, settled values with their leases, and per-frame
-- keep-alive requests, keyed by widget or animation id.
data AnimationState = AnimationState
  { asAnimations :: !(IntMap Animation)
  , asAnimRest :: !(IntMap Float)
  -- ^ Settled nonzero values.
  , asRestHeld :: !IntSet
  -- ^ Resting keys read or set since the last sweep of 'asAnimRest'. The
  -- sweep keeps these and those of laid-out widgets, drops the rest, and
  -- starts the set again.
  , asRestFrames :: {-# UNPACK #-} !Int
  -- ^ Frames with resting values since the last sweep.
  , asAnimSettled :: {-# UNPACK #-} !Bool
  , asKeepAlive :: !IntSet
  -- ^ Keys whose perpetual animation a @keepAnimating@ call holds open.
  , asKeepTouched :: !IntSet
  -- ^ The keys that asked again this frame. A held key missing from it at
  -- the end of the frame lapses: its widget is no longer built, and nothing
  -- else would ever end its animation.
  }

-- | Empty animation maps with no pending animation frame.
initialAnimationState :: AnimationState
initialAnimationState = AnimationState
  { asAnimations = IM.empty
  , asAnimRest = IM.empty
  , asRestHeld = IS.empty
  , asRestFrames = 0
  , asAnimSettled = False
  , asKeepAlive = IS.empty
  , asKeepTouched = IS.empty
  }

-- | How far one wheel notch scrolls, and how long a scroll takes to settle.
-- One setting for the whole context; a single scroller can take its own step
-- (see @setScrollStep@).
data ScrollTuning = ScrollTuning
  { scrollWheelStep :: Float
  -- ^ Pixels one wheel notch scrolls. The default is three text lines, which
  -- is what Windows and most desktops send a notch as.
  , scrollSmoothTime :: Float
  -- ^ Seconds a scroll takes to cover most of the distance to its target.
  -- @0@ (the default) lands on it in the same frame.
  }
  deriving (Eq, Show)

-- | Immediate scrolling by 60 logical pixels per wheel step. A backend may
-- choose a font-relative step when configuring its context.
defaultScrollTuning :: ScrollTuning
defaultScrollTuning =
  ScrollTuning
    { scrollWheelStep = 60
    , scrollSmoothTime = 0
    }

-- | Which axes a scroller moves on, and how an offset in window axes (x
-- rightwards, y downwards) maps onto its stored offset. A 1D row scroller
-- keeps its offset in the main-axis slot, so its horizontal offset is the one
-- that needs swapping.
data ScrollAxes
  = ScrollAxisY
  | ScrollAxisX
  | ScrollAxisXY
  deriving (Eq, Show, Enum)

-- | A scroller on its way to an offset it has not reached yet. The target is
-- in window axes and already clamped to the scroller's range.
data ScrollGlide = ScrollGlide
  { sgWidget :: WidgetId
  , sgTarget :: V2
  , sgAxes :: ScrollAxes
  }
  deriving (Eq, Show)

-- | Context-wide scroll tuning, active glides, and geometry-publication keys.
data ScrollState = ScrollState
  { ssTuning :: !ScrollTuning
  , ssGlides :: !(IntMap ScrollGlide)
  , ssCached :: !IntSet
  -- ^ Scrollers whose geometry has been published this frame. Two scroll
  -- nodes can share a widget id (a table's frozen pane and its body), and
  -- without this the second would overwrite the first every frame, churning
  -- the store and flipping the geometry the commands read.
  }

-- | Default tuning with no glides or published scroller geometry.
initialScrollState :: ScrollState
initialScrollState =
  ScrollState
    { ssTuning = defaultScrollTuning
    , ssGlides = IM.empty
    , ssCached = IS.empty
    }

-- | Drawing's fitted layout keyed by envelope size, line height, content key,
-- and the caller's input layout.
data DrawFitCache = DrawFitCache
  { dfcDw :: {-# UNPACK #-} !Double
  , dfcDh :: {-# UNPACK #-} !Double
  , dfcLh :: {-# UNPACK #-} !Float
  , dfcContent :: {-# UNPACK #-} !Int
  , dfcIn :: !Layout
  , dfcOut :: !Layout
  }

-- | Cached text-span layout for one arena node. Key fields are every input
-- that changes the produced spans; 'sceSpans' is the shared result. The whole
-- cache is dropped on theme or font-scale changes.
data SpanCacheEntry = SpanCacheEntry
  { sceText :: !Text
  , sceFg :: {-# UNPACK #-} !Color
  , sceBg :: {-# UNPACK #-} !Color
  , sceStyle :: {-# UNPACK #-} !Int
  , sceFontSize :: {-# UNPACK #-} !Float
  , sceAlign :: {-# UNPACK #-} !Int
  , sceWidthTag :: {-# UNPACK #-} !Int
  , sceRect :: !Rect
  , sceEffMaxW :: {-# UNPACK #-} !Float
  , sceRowChild :: {-# UNPACK #-} !Bool
  , sceInset :: {-# UNPACK #-} !Float
  , sceFont :: !FontMetrics
  -- ^ The font the text is set in.
  , sceLines :: !SpanLines
  -- ^ The text laid out for the rect's width, before it is placed, so a node
  -- that only moved places the same lines again.
  , sceSpans :: ![(Rect, Text, Color, Color)]
  }

-- | A text node's lines for its width, with the metrics prepared for each.
data SpanLines
  = SpanWrapped ![(Text, FontMetrics)]
  -- ^ Wrapped lines, stacked from the node's top.
  | SpanSingle !Text !FontMetrics
  -- ^ One line, possibly truncated, centred in the node's height.

-- | A cacheable widget label is a single line (or absent for close buttons).
-- Coordinates are relative to the node origin; paint translates them without
-- rebuilding a list or invalidating the cache when a widget scrolls.
data WidgetTextPlacement = WidgetTextPlacement
  !Text
  {-# UNPACK #-} !Float
  {-# UNPACK #-} !Float
  {-# UNPACK #-} !Float
  {-# UNPACK #-} !Float

-- | Widget-label measurement inputs and optional node-relative placement.
-- Position is excluded so scrolling can reuse the measurement.
data WidgetTextCacheEntry = WidgetTextCacheEntry
  { wtcNodeType :: {-# UNPACK #-} !Int
  , wtcStyle :: {-# UNPACK #-} !Int
  , wtcFontSize :: {-# UNPACK #-} !Float
  , wtcText :: !Text
  , wtcWidth :: {-# UNPACK #-} !Float
  , wtcHeight :: {-# UNPACK #-} !Float
  , wtcAlign :: {-# UNPACK #-} !Int
  , wtcPlacement :: {-# NOUNPACK #-} !(Maybe WidgetTextPlacement)
  }

-- | Interaction and styling snapshot supplied to a custom widget's painter.
-- Theme includes scoped/disabled styling; font metrics describe its text.
data CustomDrawContext = CustomDrawContext
  { cdcHovered  :: {-# UNPACK #-} !Bool
  , cdcPressed  :: {-# UNPACK #-} !Bool
  , cdcFocused  :: {-# UNPACK #-} !Bool
  , cdcActive   :: {-# UNPACK #-} !Bool
  , cdcDisabled :: {-# UNPACK #-} !Bool
  , cdcTheme    :: !Theme
  , cdcFont     :: !FontMetrics
  }

-- | Pure draw-op builder for a solved rectangle in logical window coordinates.
-- Include changing external inputs in the widget's content key when caching.
type CustomDrawBuild = CustomDrawContext -> Rect -> SmallArray DrawOp

-- | A registered custom drawing: its content key plus the op builder. A
-- non-zero key is the author's promise that the ops follow it, so a frame
-- whose key is unchanged neither rebuilds nor repaints them. Key 0 means the
-- drawing carries no key and is rebuilt every frame and compared.
data CustomDrawingEntry = CustomDrawingEntry
  { cdrContent :: {-# UNPACK #-} !Int
  , cdrBuild :: !CustomDrawBuild
  , cdrCursor :: !(Maybe (CustomDrawContext -> UiCursorKind))
  , cdrDamageSlop :: {-# UNPACK #-} !Float
    -- ^ Repaint margin in logical pixels; anything but a positive value
    -- leaves the default margin in place.
  , cdrTracked :: !Bool
    -- ^ Whether the widget wants a frame for every pointer move over it.
  }

-- | A registered drawing: content version plus the op builder. The version
-- participates in the draw-op cache key, so a builder whose output changes
-- without its size changing must bump the version to invalidate.
data DrawingEntry = DrawingEntry
  { deContent :: {-# UNPACK #-} !Int
  , deBuild :: !DrawingBuild
  }

-- | Per-widget drawing registrations and compiled-op/layout caches. Builders
-- register during view construction; paint consumes them after layout.
data DrawingCacheState = DrawingCacheState
  { dcsPopupConfigs :: !(IntMap PopupConfig)
  , dcsDrawings :: !(IntMap DrawingEntry)
  , dcsCustomDrawings :: !(IntMap CustomDrawingEntry)
  , dcsCustomMeasures :: !(IntMap CustomMeasureFn)
  , dcsDrawOpCache :: !(IntMap DrawOpCacheEntry)
  , dcsCustomDrawOpCache :: !(IntMap CustomDrawOpCacheEntry)
  , dcsDrawFitCache :: !(IntMap DrawFitCache)
  , dcsHoverZones :: ![Rect]
  -- ^ Rects the pointer coming onto or leaving needs a frame for, though no
  -- widget the hover probe finds may be there: tooltip targets.
  }

-- | Strict cache entry for a popup's anchor configuration.
data PopupConfig = PopupConfig
  { pcAnchor :: !PopupAnchor
  , pcPlacement :: !PopupPlacement
  , pcOffset :: {-# UNPACK #-} !Float
  , pcFollowsPointer :: !Bool
  -- ^ Placed from where the pointer is, so while it is up every pointer move
  -- needs a frame to move it.
  }

-- | Strict cache entry for a drawing's compiled draw ops.
data DrawOpCacheEntry = DrawOpCacheEntry
  { doeContent :: {-# UNPACK #-} !Int
  , doeBounds :: !Rect
  , doeOps :: !(SmallArray DrawOp)
  }

-- | Strict cache entry for a custom drawing's compiled draw ops. Every input
-- the ops can depend on is part of the key: the content key, the rect, the
-- interaction state the draw context exposes, and the metric generation, which
-- a theme or font change bumps.
data CustomDrawOpCacheEntry = CustomDrawOpCacheEntry
  { cdeContent :: {-# UNPACK #-} !Int
  , cdeBounds :: !Rect
  , cdeHovered :: {-# UNPACK #-} !Bool
  , cdePressed :: {-# UNPACK #-} !Bool
  , cdeFocused :: {-# UNPACK #-} !Bool
  , cdeDisabled :: {-# UNPACK #-} !Bool
  , cdeGen :: {-# UNPACK #-} !Int
  , cdeOps :: !(SmallArray DrawOp)
  }

-- | No registered drawings, popup configurations, or cached results.
initialDrawingCacheState :: DrawingCacheState
initialDrawingCacheState = DrawingCacheState
  { dcsPopupConfigs = IM.empty
  , dcsDrawings = IM.empty
  , dcsCustomDrawings = IM.empty
  , dcsCustomMeasures = IM.empty
  , dcsDrawOpCache = IM.empty
  , dcsCustomDrawOpCache = IM.empty
  , dcsDrawFitCache = IM.empty
  , dcsHoverZones = []
  }

-- | Where a frame's pointer events go: the press, the drag and release that
-- follow it, the wheel, and hover. The frame decides this once, before the
-- view runs, from what was on top under the pointer, and a held button keeps
-- the route it went down with. Everything else sees a frame without a pointer
-- ('NanoUI.Internal.Input.withoutPointer'), so nothing has to check whether an event
-- was meant for it.
data PointerRoute
  = -- | The widgets of one layer: 0 is the page, anything else the key of the
    -- floating panel (window, popup or modal) on top under the pointer.
    RouteLayer !Int
  | -- | The text-edit context menu, which the frame draws over every layer.
    RouteTextMenu
  | -- | An open select or combo dropdown, drawn the same way, and the widget
    -- it belongs to.
    RouteDropdown !WidgetId
  deriving (Eq, Show)

-- | Pointer route and active drag/menu gestures shared by the view and frame passes.
data InteractionState = InteractionState
  { isScrollDrag :: !(Maybe (WidgetId, DirTag, Float))
  -- | The scrollbar under the pointer, or whose thumb is being dragged: its
  -- scroller's id, its axis and its track. Its thumb paints brighter.
  , isScrollHover :: !(Maybe (WidgetId, DirTag, Rect))
  , isTextInputDrag :: !(Maybe TextInputDrag)
  , isTextFieldClickCell :: !(Maybe TextFieldClickCell)
  , isTextInputMenu :: !(Maybe TextInputMenu)
  , isTextEditLastAction :: !(Maybe (WidgetId, TextCommand))
  , isPointerRoute :: !PointerRoute
  -- | A button was down when the route was last decided, so it stands.
  , isPointerHeld :: {-# UNPACK #-} !Bool
  , isWindowDrag :: !(Maybe (WidgetId, Float, Float))
  , isWindowResize :: !(Maybe WindowResizeDrag)
  -- | A table header is resizing a column. Set by the table on the frames it
  -- does, cleared when the pointer is let go.
  , isColumnResize :: {-# UNPACK #-} !Bool
  }
  deriving (Eq, Show)

-- | Pointer routed to the page, with no held gesture, menu, or pending edit command.
initialInteractionState :: InteractionState
initialInteractionState = InteractionState
  { isScrollDrag = Nothing
  , isScrollHover = Nothing
  , isTextInputDrag = Nothing
  , isTextFieldClickCell = Nothing
  , isTextInputMenu = Nothing
  , isTextEditLastAction = Nothing
  , isPointerRoute = RouteLayer 0
  , isPointerHeld = False
  , isWindowDrag = Nothing
  , isWindowResize = Nothing
  , isColumnResize = False
  }

-- | Mutable state for one UI session. Construct with @newContext@ and use it
-- serially on the UI thread. Record copies share arenas, stores, and IORefs;
-- a font-configured copy is not an independent session.
data Context = Context
  { ctxNodeArena :: NodeArena
  , ctxDrawArena :: DrawArena
  , ctxHotId :: IORef WidgetId
  , ctxLastHotId :: IORef WidgetId
  -- | The widgets, by 'intKey', that the pointer is over
  -- in the frame the user saw but does not reach, since a stack or a pinned
  -- node draws another widget over them there
  -- ('NanoUI.Internal.Frame.Input.recordCoveredWidgets'). Found before the
  -- view runs, which then gives them no pointer.
  , ctxPointerCovered :: !(IORef IntSet)
  , ctxActiveId :: IORef WidgetId
  , ctxClickedId :: IORef WidgetId
  , ctxReleaseClickedId :: IORef WidgetId
  -- | Where the held left, right and middle buttons went down, cleared when
  -- they come up. A click belongs to the widget the press landed on, so a
  -- widget hit-tests this point as well as the release point. 'Nothing' (a
  -- release with no press behind it) lets the release stand on its own.
  , ctxPressPos :: IORef (Maybe V2)
  , ctxRightPressPos :: IORef (Maybe V2)
  , ctxMiddlePressPos :: IORef (Maybe V2)
  , ctxFocusId :: IORef WidgetId
  -- | Focus last moved by keyboard, so the focused widget shows its ring. A
  -- pointer press hides it again.
  , ctxFocusVisible :: IORef Bool
  , ctxStore :: IORef WidgetStore
  , ctxDamageState :: IORef DamageState
  , ctxOverlayState :: IORef OverlayState
  , ctxAnimationState :: IORef AnimationState
  , ctxScrollState :: !(IORef ScrollState)
  , ctxDrawingCache :: IORef DrawingCacheState
  , ctxIdContext :: IORef IdContext
  , ctxFontMetrics :: FontMetrics
  -- ^ Base proportional-font metrics used by layout and drawing.
  , ctxMonoFontMetrics :: FontMetrics
  , ctxMeasureText :: Text -> IO (Float, Float)
  , ctxResolveFont :: !(Float -> FontWeight -> FontStyle -> FontVariant -> IO (FontMetrics, Bool))
  , ctxResolveMeasure :: !(Float -> FontWeight -> FontStyle -> FontVariant -> Text -> IO (Float, Float))
  , ctxMeasureCache :: Maybe (IORef MeasureCache)
  , ctxWrapCache :: !(IORef WrapCache)
  -- ^ Wrapped text, shared by the solve and the text spans across frames.
  -- See 'NanoUI.Internal.Context.cachedWrapText'.
  , ctxSpanCache :: !(IORef (IntMap SpanCacheEntry))
  , ctxWidgetTextCache :: !(IORef (IntMap WidgetTextCacheEntry))
  -- | What widgets derive from their arguments and keep between frames
  -- (a table's column widths and sort), by widget key. It is not state, so
  -- a write neither damages nor wakes the loop. Cleared with the text caches.
  , ctxDerivedCache :: !(IORef (IntMap Dynamic))
  -- Whole-layout reuse cache: cached signature and solved rects,
  -- with the window size and font/theme generation it was captured under.
  , ctxLayoutCache :: !(IORef (Maybe (LayoutCache, Size, Int)))
  , ctxMetricGen :: !(IORef Int)
  , ctxMetricSource :: {-# NOUNPACK #-} !MetricSource
  , ctxLastMetricSource :: !(IORef (Maybe MetricSource))
  -- | True when the next present must repaint the whole window (fresh retain
  -- texture, forced full, continuous present, or window expose). When False,
  -- a DamageClip frame culls the paint pass to the damaged region.
  , ctxPaintFull :: !(IORef Bool)
  -- | The layout overlay, which outlines every node ('ExplainState').
  , ctxExplain :: !(IORef ExplainState)
  , ctxTheme :: !(IORef Theme)
  -- ^ Base theme; scoped themes live in 'ctxThemeScopes'.
  , ctxThemeScopes :: !(IORef ThemeScopes)
  , ctxContainerStack :: IORef [Int]
  , ctxMessages :: IORef [FrameMsg]
  , ctxFocusables :: IORef (MutablePrimArray RealWorld WidgetId)
  , ctxFocusablesCount :: IORef Int
  , ctxSpanBase :: SpanArena
  -- ^ Base-layer text spans for the current frame, reused on the next frame.
  , ctxSpanOverlay :: SpanArena
  -- ^ Overlay text spans for the current frame, reused on the next frame.
  , ctxInteractionState :: !(IORef InteractionState)
  -- | Rects the view asked a cursor for this build, newest first: a table's
  -- column edges. Each build starts empty.
  , ctxCursorZones :: !(IORef [(Rect, UiCursorKind)])
  -- | The node ranges of this build's 'NanoUI.Internal.Widgets.Cursor.withCursorShape'
  -- scopes, from the first index up to but not including the second, newest
  -- first: a scope ends after the scopes inside it, so it comes before them.
  -- Each build starts empty.
  , ctxCursorRegions :: !(IORef [(Int, Int, UiCursorKind)])
  , ctxClipboardGet :: IO (Maybe Text)
  , ctxClipboardSet :: Text -> IO Bool
  , ctxImageAtlas :: ImageAtlas
  , ctxWakeLoop :: IORef (Maybe (IO ()))
  , ctxWakeAt :: !(IORef Double)
  -- ^ Monotonic time ('GHC.Clock.getMonotonicTime') of the earliest frame
  -- anything asked for without input to cause it, or 0 for none. Each frame
  -- starts from 0 and whatever still needs a later frame asks again, so the
  -- loop sleeps until then instead of polling.
  , ctxHost :: IORef (Map TypeRep Dynamic)
  }

-- | Convert a widget's hash to its store key. The representation assumes a
-- 64-bit 'Int' when preserving all identity bits.
{-# INLINE intKey #-}
intKey :: WidgetId -> Int
intKey = fromIntegral . hashWidgetId
