{-# LANGUAGE StrictData #-}

module NanoUI.Context.Types
  ( Context (..)
  , MeasureCacheKey
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
  , TextOpKey (..)
  , TextOpCache (..)
  , DrawingCacheState (..)
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
import Data.Maybe (mapMaybe)
import Data.Primitive.PrimArray (MutablePrimArray)
import Data.Text (Text)
import Data.Typeable (TypeRep, Typeable, cast)
import Data.Vector (Vector)
import Data.Word (Word64)
import GHC.Exts (RealWorld)

import NanoUI.Animation (Animation)
import NanoUI.Atlas (ImageAtlas)
import NanoUI.Draw (DrawArena, DrawOp, DrawingBuild)
import NanoUI.Font (FontMetrics)
import NanoUI.Frame.SpanArena (SpanArena)
import NanoUI.Icons (Icons)
import NanoUI.Id (IdContext, WidgetId, hashWidgetId)
import NanoUI.Input (UiCursorKind)
import NanoUI.Layout.Arena (DirTag, NodeArena, NodeType)
import NanoUI.Store (WidgetStore)
import NanoUI.Style (AlignX, FontStyle, FontVariant, FontWeight, Layout, Theme)
import NanoUI.Types
  ( Color
  , Damage (..)
  , DamageBounds
  , HostProfile
  , PopupAnchor
  , PopupPlacement
  , Rect
  , Size (..)
  , V2
  )

data FrameMsg where
  FrameMsg :: Typeable a => a -> FrameMsg

decodeMessages :: Typeable a => [FrameMsg] -> [a]
decodeMessages = mapMaybe (\(FrameMsg x) -> cast x)

reduceMessages :: Typeable msg => (msg -> model -> model) -> model -> [FrameMsg] -> model
reduceMessages update model = foldl' (flip update) model . decodeMessages

reduceUpdates :: Typeable model => model -> [FrameMsg] -> model
reduceUpdates = reduceMessages ($)

type MeasureCacheKey = (Text, Float)

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
  , textInputDragAnchor :: Int
  , textInputDragAnchorRow :: Int
  , textInputDragAnchorCol :: Int
  , textInputDragMultiline :: Bool
  , textInputDragClicks :: Int
  }
  deriving (Eq, Show)

data TextFieldClickCell = TextFieldClickCell
  { textFieldClickWidget :: WidgetId
  , textFieldClickFlat :: Int
  , textFieldClickRow :: Int
  , textFieldClickCol :: Int
  , textFieldClickMultiline :: Bool
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
  , wrdGrabX :: Float
  , wrdGrabY :: Float
  , wrdStartX :: Float
  , wrdStartY :: Float
  , wrdStartW :: Float
  , wrdStartH :: Float
  , wrdMinW :: Float
  , wrdMinH :: Float
  , wrdMaxW :: Float
  , wrdMaxH :: Float
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
  { osModalWasActive :: !Bool
  , osModalActive :: !Bool
  , osModalDepth :: !Int
  , osEscapeConsumed :: !Bool
  , osPrevFloatingRects :: !(IntMap Rect)
  , osPrevFloatingOrder :: ![Int]
  , osTopmostCache :: !(Maybe (V2, Maybe WidgetId))
  , osCurrentFloatingId :: !(Maybe WidgetId)
  , osLastPointerBlocked :: !Bool
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
  , asAnyAnimating :: !Bool
  , asAnimSettled :: !Bool
  }

initialAnimationState :: AnimationState
initialAnimationState = AnimationState
  { asAnimations = IM.empty
  , asAnimRest = IM.empty
  , asAnyAnimating = False
  , asAnimSettled = False
  }

data DrawFitCache = DrawFitCache
  { dfcDw :: {-# UNPACK #-} !Double
  , dfcDh :: {-# UNPACK #-} !Double
  , dfcLh :: {-# UNPACK #-} !Float
  , dfcContent :: {-# UNPACK #-} !Int
  , dfcIn :: !Layout
  , dfcOut :: !Layout
  }

-- | Everything that shapes a static label's placed spans: content, font
-- selection, resolved-font identity (line height + snap scale), colors,
-- alignment, and the effective wrap width. Keys that are equal except for the
-- rect origin mean the label only moved; the cached spans translate.
data TextOpKey = TextOpKey
  { tokRect :: {-# UNPACK #-} !Rect
  , tokContent :: !Text
  , tokStyle :: {-# UNPACK #-} !Int
  , tokStripe :: !(Maybe Color)
  , tokFg :: {-# UNPACK #-} !Color
  , tokBg :: {-# UNPACK #-} !Color
  , tokFontSize :: {-# UNPACK #-} !Float
  , tokWeight :: !FontWeight
  , tokSlant :: !FontStyle
  , tokVariant :: !FontVariant
  , tokAlign :: !AlignX
  , tokWrapW :: {-# UNPACK #-} !Float
  , tokCanWrap :: !Bool
  , tokLineH :: {-# UNPACK #-} !Float
  , tokSnap :: {-# UNPACK #-} !Float
  }
  deriving (Eq, Show)

-- | Cached placed spans for one text node, stamped with the frame counter so
-- 'pruneDrawOpCache' can drop entries for labels that stopped painting.
data TextOpCache = TextOpCache
  { tocStamp :: {-# UNPACK #-} !Word64
  , tocKey :: !TextOpKey
  , tocSpans :: ![(Rect, Text, Color, Color)]
  }
  deriving (Show)

type CustomMeasureFn = HostProfile -> FontMetrics -> (Float, Float) -> (Float, Float)

data CustomDrawContext = CustomDrawContext
  { cdcHovered  :: !Bool
  , cdcPressed  :: !Bool
  , cdcFocused  :: !Bool
  , cdcActive   :: !Bool
  , cdcDisabled :: !Bool
  , cdcTheme    :: !Theme
  , cdcHost     :: !HostProfile
  , cdcFont     :: !FontMetrics
  }

type CustomDrawBuild = CustomDrawContext -> Rect -> Vector DrawOp

data DrawingCacheState = DrawingCacheState
  { dcsPopupConfigs :: !(IntMap (PopupAnchor, PopupPlacement, Float))
  , dcsDrawings :: !(IntMap DrawingBuild)
  , dcsCustomDrawings :: !(IntMap CustomDrawBuild)
  , dcsCustomMeasures :: !(IntMap CustomMeasureFn)
  , dcsCustomCursors :: !(IntMap (CustomDrawContext -> UiCursorKind))
  , dcsCustomDamageSlop :: !(IntMap Float)
  , dcsDrawOpCache :: !(IntMap (Rect, Vector DrawOp))
  , dcsCustomDrawOpCache :: !(IntMap (Rect, Bool, Bool, Bool, Vector DrawOp))
  , dcsDrawFitCache :: !(IntMap DrawFitCache)
  , dcsTextOpCache :: !(IntMap TextOpCache)
  , dcsFrame :: {-# UNPACK #-} !Word64
  , dcsWidgetNodeTypes :: !(Maybe (IntMap NodeType))
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
  , dcsTextOpCache = IM.empty
  , dcsFrame = 0
  , dcsWidgetNodeTypes = Nothing
  }

data InteractionState = InteractionState
  { isScrollDrag :: !(Maybe (WidgetId, DirTag, Float))
  , isTextInputDrag :: !(Maybe TextInputDrag)
  , isTextFieldClickCell :: !(Maybe TextFieldClickCell)
  , isTextInputMenu :: !(Maybe TextInputMenu)
  , isSelectDropPress :: !Bool
  , isOpenSelectDrop :: !(Maybe (WidgetId, Rect))
  , isMenuPointerGesture :: !Bool
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
  , ctxExternalText :: Bool
  , ctxTheme :: !(IORef Theme)
  , ctxIcons :: Icons
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
  , ctxHostProfile :: HostProfile
  , ctxDefaultLayout :: IORef Layout
  }

{-# INLINE intKey #-}
intKey :: WidgetId -> Int
intKey = fromIntegral . hashWidgetId
