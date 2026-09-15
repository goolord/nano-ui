{-# LANGUAGE StrictData #-}

module NanoUI.Context.Types
  ( Context (..)
  , MeasureCacheKey
  , MetricSource (..)
  , TextInputMenu (..)
  , TextInputDrag (..)
  , TextFieldClickCell (..)
  , WindowResizeEdge (..)
  , WindowResizeDrag (..)
  , DamageRequest (..)
  , DamageState (..)
  , initialDamageState
  , OverlayState (..)
  , initialOverlayState
  , AnimationState (..)
  , initialAnimationState
  , DrawFitCache (..)
  , DrawingEntry (..)
  , DrawingCacheState (..)
  , PopupConfig (..)
  , DrawOpCacheEntry (..)
  , CustomDrawOpCacheEntry (..)
  , SpanCacheEntry (..)
  , WidgetTextCacheEntry (..)
  , WidgetTextPlacement (..)
  , initialDrawingCacheState
  , InteractionState (..)
  , initialInteractionState
  , CustomMeasureFn
  , CustomDrawContext (..)
  , CustomDrawBuild
  , FrameMsg (..)
  , decodeMessages
  , reduceMessages
  , reduceUpdates
  , intKey
  ) where

import Data.Dynamic (Dynamic)
import Data.HashMap.Strict (HashMap)
import Data.IORef (IORef)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Map.Strict (Map)
import Data.Primitive.PrimArray (MutablePrimArray)
import Data.Text (Text)
import Data.Typeable (TypeRep, Typeable, cast)
import Data.Vector (Vector)
import GHC.Exts (RealWorld)

import NanoUI.Animation (Animation)
import NanoUI.Atlas (ImageAtlas)
import NanoUI.Draw.Types (DrawArena, DrawOp, DrawingBuild)
import NanoUI.Font (CustomMeasureFn, FontMetrics)
import NanoUI.Frame.SpanArena (SpanArena)
import NanoUI.Id (IdContext, WidgetId, hashWidgetId)
import NanoUI.Input (UiCursorKind)
import NanoUI.Layout.Arena (DirTag, LayoutCache, NodeArena, NodeType)
import NanoUI.Store (WidgetStore)
import NanoUI.Style (FontStyle, FontVariant, FontWeight, Layout, Theme)
import NanoUI.Types
  ( Color
  , Damage (..)
  , DamageBounds
  , PopupAnchor
  , PopupPlacement
  , Rect
  , Size (..)
  , V2
  )

data FrameMsg where
  FrameMsg :: Typeable a => a -> FrameMsg

decodeMessages :: (Foldable f, Typeable a) => f FrameMsg -> [a]
decodeMessages = foldr (\(FrameMsg x) rest -> maybe rest (: rest) (cast x)) []

reduceMessages :: (Foldable f, Typeable msg) => (msg -> model -> model) -> model -> f FrameMsg -> model
reduceMessages update = foldl' (\model (FrameMsg x) -> maybe model (`update` model) (cast x))

reduceUpdates :: (Foldable f, Typeable model) => model -> f FrameMsg -> model
reduceUpdates = reduceMessages ($)

type MeasureCacheKey = (Text, Float)

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

data TextInputMenu = TextInputMenu
  { textInputMenuWidget :: WidgetId
  , textInputMenuRect :: Rect
  }
  deriving (Eq, Show)

data TextInputDrag = TextInputDrag
  { textInputDragWidget :: WidgetId
  , textInputDragAnchor :: {-# UNPACK #-} !Int
  , textInputDragAnchorRow :: {-# UNPACK #-} !Int
  , textInputDragAnchorCol :: {-# UNPACK #-} !Int
  , textInputDragMultiline :: {-# UNPACK #-} !Bool
  , textInputDragClicks :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Show)

data TextFieldClickCell = TextFieldClickCell
  { textFieldClickWidget :: WidgetId
  , textFieldClickFlat :: {-# UNPACK #-} !Int
  , textFieldClickRow :: {-# UNPACK #-} !Int
  , textFieldClickCol :: {-# UNPACK #-} !Int
  , textFieldClickMultiline :: {-# UNPACK #-} !Bool
  }
  deriving (Eq, Show)

data WindowResizeEdge
  = ResizeN
  | ResizeS
  | ResizeE
  | ResizeW
  | ResizeNE
  | ResizeNW
  | ResizeSE
  | ResizeSW
  deriving (Eq, Show)

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

data DamageState = DamageState
  { dsDirty :: !Bool
  , dsDamage :: !Damage
  , dsRequests :: ![DamageRequest]
  , dsLastWindowSize :: !Size
  , dsPrevRects :: !(IntMap Rect)
  , dsPrevClips :: !(IntMap Rect)
  , dsPrevNodeTexts :: !(IntMap Text)
  }

initialDamageState :: DamageState
initialDamageState = DamageState
  { dsDirty = True
  , dsDamage = DamageFull
  , dsRequests = []
  , dsLastWindowSize = Size 0 0
  , dsPrevRects = IM.empty
  , dsPrevClips = IM.empty
  , dsPrevNodeTexts = IM.empty
  }

data OverlayState = OverlayState
  { osModalWasActive :: {-# UNPACK #-} !Bool
  , osModalActive :: {-# UNPACK #-} !Bool
  , osModalDepth :: {-# UNPACK #-} !Int
  , osEscapeConsumed :: {-# UNPACK #-} !Bool
  , osPrevFloatingRects :: !(IntMap Rect)
  , osPrevFloatingOrder :: ![Int]
  , osTopmostCache :: !(Maybe (V2, Maybe WidgetId))
  , osCurrentFloatingId :: !(Maybe WidgetId)
  , osLastPointerBlocked :: {-# UNPACK #-} !Bool
  , osFloatingAncestor :: !(Maybe (IntMap (Maybe NodeType)))
  }

initialOverlayState :: OverlayState
initialOverlayState = OverlayState
  { osModalWasActive = False
  , osModalActive = False
  , osModalDepth = 0
  , osEscapeConsumed = False
  , osPrevFloatingRects = IM.empty
  , osPrevFloatingOrder = []
  , osTopmostCache = Nothing
  , osCurrentFloatingId = Nothing
  , osLastPointerBlocked = False
  , osFloatingAncestor = Nothing
  }

data AnimationState = AnimationState
  { asAnimations :: !(IntMap Animation)
  , asAnimRest :: !(IntMap Float)
  , asAnyAnimating :: {-# UNPACK #-} !Bool
  , asAnimSettled :: {-# UNPACK #-} !Bool
  , asRectless :: !(IntMap Int)
  }

initialAnimationState :: AnimationState
initialAnimationState = AnimationState
  { asAnimations = IM.empty
  , asAnimRest = IM.empty
  , asAnyAnimating = False
  , asAnimSettled = False
  , asRectless = IM.empty
  }

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
  , sceSpans :: ![(Rect, Text, Color, Color)]
  }

-- | A cacheable widget label is a single line (or absent for close buttons).
-- Coordinates are relative to the node origin; paint translates them without
-- rebuilding a list or invalidating the cache when a widget scrolls.
data WidgetTextPlacement = WidgetTextPlacement
  !Text
  {-# UNPACK #-} !Float
  {-# UNPACK #-} !Float
  {-# UNPACK #-} !Float
  {-# UNPACK #-} !Float

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

data CustomDrawContext = CustomDrawContext
  { cdcHovered  :: {-# UNPACK #-} !Bool
  , cdcPressed  :: {-# UNPACK #-} !Bool
  , cdcFocused  :: {-# UNPACK #-} !Bool
  , cdcActive   :: {-# UNPACK #-} !Bool
  , cdcDisabled :: {-# UNPACK #-} !Bool
  , cdcTheme    :: !Theme
  , cdcFont     :: !FontMetrics
  }

type CustomDrawBuild = CustomDrawContext -> Rect -> Vector DrawOp

-- | A registered drawing: content version plus the op builder. The version
-- participates in the draw-op cache key, so a builder whose output changes
-- without its size changing must bump the version to invalidate.
data DrawingEntry = DrawingEntry
  { deContent :: {-# UNPACK #-} !Int
  , deBuild :: !DrawingBuild
  }

data DrawingCacheState = DrawingCacheState
  { dcsPopupConfigs :: !(IntMap PopupConfig)
  , dcsDrawings :: !(IntMap DrawingEntry)
  , dcsCustomDrawings :: !(IntMap CustomDrawBuild)
  , dcsCustomMeasures :: !(IntMap CustomMeasureFn)
  , dcsCustomCursors :: !(IntMap (CustomDrawContext -> UiCursorKind))
  , dcsCustomDamageSlop :: !(IntMap Float)
  , dcsDrawOpCache :: !(IntMap DrawOpCacheEntry)
  , dcsCustomDrawOpCache :: !(IntMap CustomDrawOpCacheEntry)
  , dcsDrawFitCache :: !(IntMap DrawFitCache)
  , dcsWidgetNodeTypes :: !(Maybe (IntMap NodeType))
  }

-- | Strict cache entry for a popup's anchor configuration.
data PopupConfig = PopupConfig
  { pcAnchor :: !PopupAnchor
  , pcPlacement :: !PopupPlacement
  , pcOffset :: {-# UNPACK #-} !Float
  }

-- | Strict cache entry for a drawing's compiled draw ops.
data DrawOpCacheEntry = DrawOpCacheEntry
  { doeContent :: {-# UNPACK #-} !Int
  , doeBounds :: !Rect
  , doeOps :: !(Vector DrawOp)
  }

-- | Strict cache entry for a custom drawing's compiled draw ops.
data CustomDrawOpCacheEntry = CustomDrawOpCacheEntry
  { cdeBounds :: !Rect
  , cdeHovered :: {-# UNPACK #-} !Bool
  , cdePressed :: {-# UNPACK #-} !Bool
  , cdeFocused :: {-# UNPACK #-} !Bool
  , cdeOps :: !(Vector DrawOp)
  }

initialDrawingCacheState :: DrawingCacheState
initialDrawingCacheState = DrawingCacheState
  { dcsPopupConfigs = IM.empty
  , dcsDrawings = IM.empty
  , dcsCustomDrawings = IM.empty
  , dcsCustomMeasures = IM.empty
  , dcsCustomCursors = IM.empty
  , dcsCustomDamageSlop = IM.empty
  , dcsDrawOpCache = IM.empty
  , dcsCustomDrawOpCache = IM.empty
  , dcsDrawFitCache = IM.empty
  , dcsWidgetNodeTypes = Nothing
  }

data InteractionState = InteractionState
  { isScrollDrag :: !(Maybe (WidgetId, DirTag, Float))
  , isTextInputDrag :: !(Maybe TextInputDrag)
  , isTextFieldClickCell :: !(Maybe TextFieldClickCell)
  , isTextInputMenu :: !(Maybe TextInputMenu)
  , isTextEditLastAction :: !(Maybe (WidgetId, Int))
  , isSelectDropPress :: {-# UNPACK #-} !Bool
  , isOpenSelectDrop :: !(Maybe (WidgetId, Rect))
  , isMenuPointerGesture :: {-# UNPACK #-} !Bool
  , isWindowDrag :: !(Maybe (WidgetId, Float, Float))
  , isWindowResize :: !(Maybe WindowResizeDrag)
  }
  deriving (Eq, Show)

initialInteractionState :: InteractionState
initialInteractionState = InteractionState
  { isScrollDrag = Nothing
  , isTextInputDrag = Nothing
  , isTextFieldClickCell = Nothing
  , isTextInputMenu = Nothing
  , isTextEditLastAction = Nothing
  , isSelectDropPress = False
  , isOpenSelectDrop = Nothing
  , isMenuPointerGesture = False
  , isWindowDrag = Nothing
  , isWindowResize = Nothing
  }

data Context = Context
  { ctxNodeArena :: NodeArena
  , ctxDrawArena :: DrawArena
  , ctxHotId :: IORef WidgetId
  , ctxLastHotId :: IORef WidgetId
  , ctxActiveId :: IORef WidgetId
  , ctxClickedId :: IORef WidgetId
  , ctxReleaseClickedId :: IORef WidgetId
  , ctxFocusId :: IORef WidgetId
  , ctxStore :: IORef WidgetStore
  , ctxDamageState :: IORef DamageState
  , ctxOverlayState :: IORef OverlayState
  , ctxAnimationState :: IORef AnimationState
  , ctxDrawingCache :: IORef DrawingCacheState
  , ctxIdContext :: IORef IdContext
  , ctxFontMetrics :: FontMetrics
  , ctxMonoFontMetrics :: FontMetrics
  , ctxMeasureText :: Text -> IO (Float, Float)
  , ctxResolveFont :: !(Float -> FontWeight -> FontStyle -> FontVariant -> IO (FontMetrics, Bool))
  , ctxResolveMeasure :: !(Float -> FontWeight -> FontStyle -> FontVariant -> Text -> IO (Float, Float))
  , ctxMeasureCache :: Maybe (IORef (HashMap MeasureCacheKey (Float, Float)))
  , ctxSpanCache :: !(IORef (IntMap SpanCacheEntry))
  , ctxWidgetTextCache :: !(IORef (IntMap WidgetTextCacheEntry))
  -- Whole-layout reuse cache (Phase 5A): cached signature + solved rects,
  -- with the window size and font/theme generation it was captured under.
  , ctxLayoutCache :: !(IORef (Maybe (LayoutCache, Size, Int)))
  , ctxMetricGen :: !(IORef Int)
  , ctxMetricSource :: {-# NOUNPACK #-} !MetricSource
  , ctxLastMetricSource :: !(IORef (Maybe MetricSource))
  -- True when the next present must repaint the whole window (fresh retain
  -- texture, forced full, continuous present, or window expose). When False,
  -- a DamageClip frame culls the paint pass to the damaged region.
  , ctxPaintFull :: !(IORef Bool)
  , ctxExternalText :: Bool
  , ctxTheme :: !(IORef Theme)
  , ctxContainerStack :: IORef [Int]
  , ctxMessages :: IORef [FrameMsg]
  , ctxFocusables :: IORef (MutablePrimArray RealWorld WidgetId)
  , ctxFocusablesCount :: IORef Int
  , ctxFocusablesCap :: IORef Int
  , ctxSpanBase :: SpanArena
  , ctxSpanOverlay :: SpanArena
  , ctxInteractionState :: !(IORef InteractionState)
  , ctxClipboardGet :: IO (Maybe Text)
  , ctxClipboardSet :: Text -> IO Bool
  , ctxImageAtlas :: ImageAtlas
  , ctxWakeLoop :: IORef (Maybe (IO ()))
  , ctxHost :: IORef (Map TypeRep Dynamic)
  , ctxDefaultLayout :: IORef Layout
  }

{-# INLINE intKey #-}
intKey :: WidgetId -> Int
intKey = fromIntegral . hashWidgetId
