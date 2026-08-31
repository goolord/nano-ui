{-# LANGUAGE StrictData #-}

module NanoUI.Context.Internal
  ( Context (..)
  , MeasureCacheKey
  , PendingTooltip (..)
  , TextInputMenu (..)
  , TextInputDrag (..)
  , WindowResizeEdge (..)
  , WindowResizeDrag (..)
  , intKey
  , markDirty
  , modifyIORefList
  , clearDirty
  , isDirty
  , setWakeLoop
  , takeDamage
  , clearTooltips
  , pushTooltip
  , readTooltips
  , getStore
  , setStore
  , isDisabled
  , setDisabled
  , getScrollOffset
  , setScrollOffset
  , getPrevRectByKey
  , getPrevRect
  , setPrevRect
  , atlasTextureId
  , registerImage
  , registerImages
  , lookupImageUv
  , atlasSnapshot
  , withFontMetrics
  , withMonoFontMetrics
  , withMeasureText
  , wrapMeasureCache
  , clearMeasureCache
  , withExternalText
  , withTheme
  , withIcons
  , withHostProfile
  , withClipboard
  , enableMeasureCache
  , setHost
  , askHostIO
  , pushMessage
  , drainMessages
  -- Constructors
  , newContext
  , newPixelHostContext
  -- Focus
  , getFocusId
  , getHotId
  , registerFocusable
  , getFocusables
  , getFocusablesPrim
  -- Modal & Overlay
  , textInputEditActive
  , modalActive
  , overlayConsumesQuit
  , markEscapeConsumed
  , pointerBlockedByModal
  , pointerBlockedByOverlay
  , seedFloatingPanel
  , beginModal
  , endModal
  , beginFrameModal
  , modalDamageFlip
  -- Animation
  , anyAnimating
  , startAnimation
  , startAnimationEase
  , startAnimationEaseDelay
  , startSpring
  , setAnimationValue
  , tickAnimations
  , getAnimationValue
  ) where

import Control.Monad (forM, when)
import Data.ByteString (ByteString)
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.Primitive.PrimArray
  ( MutablePrimArray
  , PrimArray
  , copyMutablePrimArray
  , freezePrimArray
  , newPrimArray
  , readPrimArray
  , writePrimArray
  )
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Typeable (TypeRep, Typeable, typeOf, typeRep)
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr)
import GHC.Exts (RealWorld)

import NanoUI.Animation (Animation (..), Ease (..), animInProgress, animationValue, approxEq, stepAnim, writeRest)
import NanoUI.Atlas (ImageAtlas, atlasTextureId)
import NanoUI.Atlas qualified as Atlas
import NanoUI.Draw (DrawArena, newDrawArena)
import NanoUI.Font (FontMetrics, hasMonoFontMarker, measureText, monospaceMetrics, stripWidgetMarkers)
import NanoUI.Frame.SpanArena (SpanArena, newSpanArena)
import NanoUI.Host (HostProfile (..))
import NanoUI.Icons (IconSet, Icons, asciiIcons, iconsFor)
import NanoUI.Id (IdContext, WidgetId (..), hashWidgetId, initialIdContext)
import NanoUI.Input (Input (..), Key (KeyEscape), inputKeys, inputKeysElem)
import NanoUI.Layout.Arena (NodeArena, NodeType, getRect, lookupNodeByKey, newNodeArena)
import NanoUI.Messages (FrameMsg)
import NanoUI.Spring (SpringParams, springEps)
import NanoUI.Store (WidgetStore (..), boolInt, emptyWidgetStore, intBool, slotDisabled, slotKey)
import NanoUI.Style (Theme, defaultTheme)
import NanoUI.Types (Damage (..), ImageId (..), Rect (..), Size (..), V2 (..), rectContains, rectH, rectW)

type MeasureCacheKey = (Text, Bool, Float)

data PendingTooltip = PendingTooltip
  { pendingTooltipWidget :: WidgetId
  , pendingTooltipRect :: Rect
  , pendingTooltipText :: Text
  }
  deriving (Eq, Show)

data TextInputMenu = TextInputMenu
  { textInputMenuWidget :: WidgetId
  , textInputMenuRect :: Rect
  }
  deriving (Eq, Show)

data TextInputDrag = TextInputDrag
  { textInputDragWidget :: WidgetId
  , textInputDragAnchor :: Int
  , textInputDragClicks :: Int
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

data Context = Context
  { ctxNodeArena :: NodeArena
  , ctxDrawArena :: DrawArena
  , ctxHotId :: IORef WidgetId
  , ctxLastHotId :: IORef WidgetId
  , ctxActiveId :: IORef WidgetId
  , ctxClickedId :: IORef WidgetId
  , ctxFocusId :: IORef WidgetId
  , ctxPrevRects :: IORef (IntMap Rect)
  , ctxPrevNodeTexts :: IORef (IntMap Text)
  , ctxStore :: IORef WidgetStore
  , ctxAnimations :: IORef (IntMap Animation)
  , ctxAnimRest :: IORef (IntMap Float)
  , ctxAnyAnimating :: IORef Bool
  , ctxAnimSettled :: IORef Bool
  , ctxDirty :: IORef Bool
  , ctxDamage :: IORef Damage
  , ctxLastWindowSize :: IORef Size
  , ctxIdContext :: IORef IdContext
  , ctxFontMetrics :: FontMetrics
  , ctxMonoFontMetrics :: FontMetrics
  , ctxMeasureText :: Text -> IO (Float, Float)
  , ctxMeasureCache :: Maybe (IORef (HashMap MeasureCacheKey (Float, Float)))
  , ctxExternalText :: Bool
  , ctxTheme :: Theme
  , ctxIcons :: Icons
  , ctxContainerStack :: IORef [Int]
  , ctxMessages :: IORef [FrameMsg]
  , ctxFocusables :: IORef (MutablePrimArray RealWorld WidgetId)
  , ctxFocusablesCount :: IORef Int
  , ctxFocusablesCap :: IORef Int
  , ctxSpanBase :: SpanArena
  , ctxSpanOverlay :: SpanArena
  , ctxScrollDrag :: IORef (Maybe (WidgetId, Float))
  , ctxTextInputDrag :: IORef (Maybe TextInputDrag)
  , ctxTextInputMenu :: IORef (Maybe TextInputMenu)
  , ctxClipboardGet :: IO (Maybe Text)
  , ctxClipboardSet :: Text -> IO Bool
  , ctxTooltips :: IORef [PendingTooltip]
  , ctxWidgetNodeTypes :: IORef (Maybe (IntMap NodeType))
  , ctxSelectDropPress :: IORef Bool
  , ctxModalWasActive :: IORef Bool
  , ctxModalActive :: IORef Bool
  , ctxModalDepth :: IORef Int
  , ctxEscapeConsumed :: IORef Bool
  , ctxWindowDrag :: IORef (Maybe (WidgetId, Float, Float))
  , ctxWindowResize :: IORef (Maybe WindowResizeDrag)
  , ctxPrevFloatingRects :: IORef (IntMap Rect)
  , ctxPrevFloatingOrder :: IORef [Int]
  , ctxOverlayTopmostCache :: IORef (Maybe (V2, Maybe WidgetId))
  , ctxCurrentFloatingId :: IORef (Maybe WidgetId)
  , ctxLastPointerBlocked :: IORef Bool
  , ctxFloatingAncestor :: IORef (Maybe (IntMap (Maybe NodeType)))
  , ctxImageAtlas :: ImageAtlas
  , ctxWakeLoop :: IORef (Maybe (IO ()))
  , ctxHost :: IORef (Map TypeRep Dynamic)
  , ctxHostProfile :: HostProfile
  }

{-# INLINE intKey #-}
intKey :: WidgetId -> Int
intKey = fromIntegral . hashWidgetId

{-# INLINE markDirty #-}
markDirty :: Context -> IO ()
markDirty ctx = do
  writeIORef (ctxDirty ctx) True
  wake <- readIORef (ctxWakeLoop ctx)
  case wake of
    Just act -> act
    Nothing -> pure ()

{-# INLINE modifyIORefList #-}
modifyIORefList :: IORef [a] -> (a -> [a] -> [a]) -> a -> IO ()
modifyIORefList ref cons val = do
  xs <- readIORef ref
  writeIORef ref (cons val xs)

{-# INLINE clearDirty #-}
clearDirty :: Context -> IO ()
clearDirty ctx = writeIORef (ctxDirty ctx) False

{-# INLINE isDirty #-}
isDirty :: Context -> IO Bool
isDirty ctx = readIORef (ctxDirty ctx)

{-# INLINE setWakeLoop #-}
setWakeLoop :: Context -> IO () -> IO ()
setWakeLoop ctx wake = writeIORef (ctxWakeLoop ctx) (Just wake)

{-# INLINE takeDamage #-}
takeDamage :: Context -> IO Damage
takeDamage ctx = readIORef (ctxDamage ctx)

{-# INLINE clearTooltips #-}
clearTooltips :: Context -> IO ()
clearTooltips ctx = writeIORef (ctxTooltips ctx) []

{-# INLINE pushTooltip #-}
pushTooltip :: Context -> WidgetId -> Rect -> Text -> IO ()
pushTooltip ctx wid r txt = modifyIORefList (ctxTooltips ctx) (:) (PendingTooltip wid r txt)

{-# INLINE readTooltips #-}
readTooltips :: Context -> IO [PendingTooltip]
readTooltips ctx = readIORef (ctxTooltips ctx)

{-# INLINE getStore #-}
getStore :: Context -> IO WidgetStore
getStore ctx = readIORef (ctxStore ctx)

{-# INLINE setStore #-}
setStore :: Context -> WidgetStore -> IO ()
setStore ctx store = do
  prev <- readIORef (ctxStore ctx)
  writeIORef (ctxStore ctx) store
  when (prev /= store) (markDirty ctx)

{-# INLINE isDisabled #-}
isDisabled :: Context -> WidgetId -> IO Bool
isDisabled ctx wid = do
  s <- getStore ctx
  pure (intBool (IM.findWithDefault 0 (slotKey slotDisabled (intKey wid)) (storeInt s)))

{-# INLINE setDisabled #-}
setDisabled :: Context -> WidgetId -> Bool -> IO ()
setDisabled ctx wid dis = do
  s <- getStore ctx
  setStore ctx (s {storeInt = IM.insert (slotKey slotDisabled (intKey wid)) (boolInt dis) (storeInt s)})

{-# INLINE getScrollOffset #-}
getScrollOffset :: Context -> WidgetId -> IO Float
getScrollOffset ctx wid = do
  s <- getStore ctx
  pure (IM.findWithDefault 0 (intKey wid) (storeFloat s))

{-# INLINE setScrollOffset #-}
setScrollOffset :: Context -> WidgetId -> Float -> IO ()
setScrollOffset ctx wid off = do
  store <- getStore ctx
  let key = intKey wid
      prev = IM.findWithDefault 0 key (storeFloat store)
  when (prev /= off) $
    setStore ctx (store {storeFloat = IM.insert key off (storeFloat store)})

{-# INLINE getPrevRectByKey #-}
getPrevRectByKey :: Context -> Int -> IO (Maybe Rect)
getPrevRectByKey ctx k = do
  m <- readIORef (ctxPrevRects ctx)
  pure (IM.lookup k m)

{-# INLINE getPrevRect #-}
getPrevRect :: Context -> WidgetId -> IO (Maybe Rect)
getPrevRect ctx wid = getPrevRectByKey ctx (intKey wid)

{-# INLINE setPrevRect #-}
setPrevRect :: Context -> WidgetId -> Rect -> IO ()
setPrevRect ctx wid r = do
  m <- readIORef (ctxPrevRects ctx)
  writeIORef (ctxPrevRects ctx) (IM.insert (intKey wid) r m)

{-# INLINE registerImage #-}
registerImage :: Context -> ImageId -> Int -> Int -> ByteString -> IO Bool
registerImage ctx iid w h px = do
  ok <- Atlas.registerImage (ctxImageAtlas ctx) iid w h px
  when ok (markDirty ctx)
  pure ok

{-# INLINE registerImages #-}
registerImages :: Context -> [(ImageId, Int, Int, ByteString)] -> IO Bool
registerImages ctx imgs = do
  results <- mapM (\(iid, w, h, px) -> registerImage ctx iid w h px) imgs
  pure (and results)

{-# INLINE lookupImageUv #-}
lookupImageUv :: Context -> ImageId -> IO (Maybe (Float, Float, Float, Float))
lookupImageUv ctx = Atlas.lookupImageUv (ctxImageAtlas ctx)

{-# INLINE atlasSnapshot #-}
atlasSnapshot :: Context -> IO (Maybe (Int, Int, ForeignPtr Word8, Int))
atlasSnapshot ctx = Atlas.atlasSnapshot (ctxImageAtlas ctx)

withFontMetrics :: Context -> FontMetrics -> Context
withFontMetrics ctx fm =
  let mono = ctxMonoFontMetrics ctx
   in ctx
        { ctxFontMetrics = fm
        , ctxMeasureText = \txt ->
            pure (measureText (ctxHostProfile ctx) (if hasMonoFontMarker txt then mono else fm) (stripWidgetMarkers txt))
        }

withMonoFontMetrics :: Context -> FontMetrics -> Context
withMonoFontMetrics ctx mono =
  let fm = ctxFontMetrics ctx
   in ctx
        { ctxMonoFontMetrics = mono
        , ctxMeasureText = \txt ->
            pure (measureText (ctxHostProfile ctx) (if hasMonoFontMarker txt then mono else fm) (stripWidgetMarkers txt))
        }

withMeasureText :: Context -> (Text -> IO (Float, Float)) -> Context
withMeasureText ctx fn = ctx {ctxMeasureText = fn}

cacheMeasureText ::
  IORef (HashMap MeasureCacheKey (Float, Float)) ->
  Float ->
  (Text -> IO (Float, Float)) ->
  Text ->
  IO (Float, Float)
cacheMeasureText ref scale base txt = do
  let key = (stripWidgetMarkers txt, hasMonoFontMarker txt, scale)
  m <- readIORef ref
  case HashMap.lookup key m of
    Just sz -> pure sz
    Nothing -> do
      sz <- base txt
      modifyIORef' ref (HashMap.insert key sz)
      pure sz

{-# INLINE wrapMeasureCache #-}
wrapMeasureCache :: Float -> Context -> (Text -> IO (Float, Float)) -> Context
wrapMeasureCache scale ctx measure =
  case ctxMeasureCache ctx of
    Nothing -> ctx {ctxMeasureText = measure}
    Just ref -> ctx {ctxMeasureText = cacheMeasureText ref scale measure}

clearMeasureCache :: Context -> IO ()
clearMeasureCache ctx =
  case ctxMeasureCache ctx of
    Just ref -> writeIORef ref HashMap.empty
    Nothing -> pure ()

withExternalText :: Context -> Bool -> Context
withExternalText ctx ext = ctx {ctxExternalText = ext}

withTheme :: Context -> Theme -> Context
withTheme ctx theme = ctx {ctxTheme = theme}

withIcons :: Context -> IconSet -> Context
withIcons ctx iset = ctx {ctxIcons = iconsFor iset}

withHostProfile :: Context -> HostProfile -> Context
withHostProfile ctx prof = ctx {ctxHostProfile = prof}

withClipboard :: Context -> IO (Maybe Text) -> (Text -> IO Bool) -> Context
withClipboard ctx getter setter = ctx {ctxClipboardGet = getter, ctxClipboardSet = setter}

enableMeasureCache :: Context -> IO Context
enableMeasureCache ctx =
  case ctxMeasureCache ctx of
    Just _ -> pure ctx
    Nothing -> do
      ref <- newIORef HashMap.empty
      pure ctx {ctxMeasureCache = Just ref, ctxMeasureText = cacheMeasureText ref 0 (ctxMeasureText ctx)}

{-# INLINE setHost #-}
setHost :: forall a. (Typeable a) => Context -> a -> IO ()
setHost ctx val = do
  m <- readIORef (ctxHost ctx)
  let k = typeOf val
  writeIORef (ctxHost ctx) (Map.insert k (toDyn val) m)

{-# INLINE askHostIO #-}
askHostIO :: forall a. (Typeable a) => Context -> IO (Maybe a)
askHostIO ctx = do
  m <- readIORef (ctxHost ctx)
  let k = typeRep (Proxy :: Proxy a)
  pure (Map.lookup k m >>= fromDynamic)

{-# INLINE pushMessage #-}
pushMessage :: Context -> FrameMsg -> IO ()
pushMessage ctx msg = modifyIORefList (ctxMessages ctx) (:) msg

{-# INLINE drainMessages #-}
drainMessages :: Context -> IO [FrameMsg]
drainMessages ctx = do
  msgs <- readIORef (ctxMessages ctx)
  writeIORef (ctxMessages ctx) []
  pure (reverse msgs)

-- =============================================================================
-- Constructors
-- =============================================================================

{-# INLINE newContext #-}
newContext :: IO Context
newContext = do
  nodeArena <- newNodeArena
  drawArena <- newDrawArena
  ctxHotId <- newIORef (WidgetId 0)
  ctxLastHotId <- newIORef (WidgetId 0)
  ctxActiveId <- newIORef (WidgetId 0)
  ctxClickedId <- newIORef (WidgetId 0)
  ctxFocusId <- newIORef (WidgetId 0)
  ctxPrevRects <- newIORef IM.empty
  ctxPrevNodeTexts <- newIORef IM.empty
  ctxStore <- newIORef emptyWidgetStore
  ctxAnimations <- newIORef IM.empty
  ctxAnimRest <- newIORef IM.empty
  ctxAnyAnimating <- newIORef False
  ctxAnimSettled <- newIORef False
  ctxDirty <- newIORef True
  ctxDamage <- newIORef DamageFull
  ctxLastWindowSize <- newIORef (Size 0 0)
  ctxIdContext <- newIORef initialIdContext
  ctxContainerStack <- newIORef []
  ctxMessages <- newIORef []
  let initCap = 64
  ctxFocusables <- newIORef =<< newPrimArray initCap
  ctxFocusablesCount <- newIORef 0
  ctxFocusablesCap <- newIORef initCap
  ctxSpanBase <- newSpanArena 64
  ctxSpanOverlay <- newSpanArena 64
  ctxScrollDrag <- newIORef Nothing
  ctxTextInputDrag <- newIORef Nothing
  ctxTextInputMenu <- newIORef Nothing
  ctxTooltips <- newIORef []
  ctxWidgetNodeTypes <- newIORef Nothing
  ctxSelectDropPress <- newIORef False
  ctxModalWasActive <- newIORef False
  ctxModalActive <- newIORef False
  ctxModalDepth <- newIORef 0
  ctxEscapeConsumed <- newIORef False
  ctxWindowDrag <- newIORef Nothing
  ctxWindowResize <- newIORef Nothing
  ctxPrevFloatingRects <- newIORef IM.empty
  ctxPrevFloatingOrder <- newIORef []
  ctxOverlayTopmostCache <- newIORef Nothing
  ctxCurrentFloatingId <- newIORef Nothing
  ctxLastPointerBlocked <- newIORef False
  ctxFloatingAncestor <- newIORef Nothing
  ctxImageAtlas <- Atlas.newImageAtlas
  ctxWakeLoop <- newIORef Nothing
  ctxHost <- newIORef Map.empty
  let fm0 = monospaceMetrics 12
  pure Context
    { ctxNodeArena = nodeArena
    , ctxDrawArena = drawArena
    , ctxHotId
    , ctxLastHotId
    , ctxActiveId
    , ctxClickedId
    , ctxFocusId
    , ctxPrevRects
    , ctxPrevNodeTexts
    , ctxStore
    , ctxAnimations
    , ctxAnimRest
    , ctxAnyAnimating
    , ctxAnimSettled
    , ctxDirty
    , ctxDamage
    , ctxLastWindowSize
    , ctxIdContext
    , ctxFontMetrics = fm0
    , ctxMonoFontMetrics = fm0
    , ctxMeasureText = \txt -> pure (measureText PixelHost fm0 (stripWidgetMarkers txt))
    , ctxMeasureCache = Nothing
    , ctxExternalText = False
    , ctxTheme = defaultTheme
    , ctxIcons = asciiIcons
    , ctxContainerStack
    , ctxMessages
    , ctxFocusables
    , ctxFocusablesCount
    , ctxFocusablesCap
    , ctxSpanBase
    , ctxSpanOverlay
    , ctxScrollDrag
    , ctxTextInputDrag
    , ctxTextInputMenu
    , ctxClipboardGet = pure Nothing
    , ctxClipboardSet = \_ -> pure False
    , ctxTooltips
    , ctxWidgetNodeTypes
    , ctxSelectDropPress
    , ctxModalWasActive
    , ctxModalActive
    , ctxModalDepth
    , ctxEscapeConsumed
    , ctxWindowDrag
    , ctxWindowResize
    , ctxPrevFloatingRects
    , ctxPrevFloatingOrder
    , ctxOverlayTopmostCache
    , ctxCurrentFloatingId
    , ctxLastPointerBlocked
    , ctxFloatingAncestor
    , ctxImageAtlas
    , ctxWakeLoop
    , ctxHost
    , ctxHostProfile = PixelHost
    }

{-# INLINE newPixelHostContext #-}
newPixelHostContext :: IO Context
newPixelHostContext = do
  ctx0 <- newContext
  ctx <- enableMeasureCache ctx0
  pure (withExternalText (withTheme (withFontMetrics ctx (monospaceMetrics 16)) defaultTheme) True)

-- =============================================================================
-- Focus
-- =============================================================================

{-# INLINE getFocusId #-}
getFocusId :: Context -> IO WidgetId
getFocusId ctx = readIORef (ctxFocusId ctx)

{-# INLINE getHotId #-}
getHotId :: Context -> IO WidgetId
getHotId ctx = readIORef (ctxHotId ctx)

{-# INLINE registerFocusable #-}
registerFocusable :: Context -> WidgetId -> IO ()
registerFocusable ctx wid = do
  idx <- readIORef (ctxFocusablesCount ctx)
  cap <- readIORef (ctxFocusablesCap ctx)
  arr <- readIORef (ctxFocusables ctx)
  arr' <-
    if idx >= cap
      then do
        let newCap = max 16 (cap * 2)
        newArr <- newPrimArray newCap
        copyMutablePrimArray newArr 0 arr 0 idx
        writeIORef (ctxFocusables ctx) newArr
        writeIORef (ctxFocusablesCap ctx) newCap
        pure newArr
      else pure arr
  writePrimArray arr' idx wid
  writeIORef (ctxFocusablesCount ctx) (idx + 1)

{-# INLINE getFocusables #-}
getFocusables :: Context -> IO [WidgetId]
getFocusables ctx = do
  count <- readIORef (ctxFocusablesCount ctx)
  arr <- readIORef (ctxFocusables ctx)
  forM [0 .. count - 1] (readPrimArray arr)

{-# INLINE getFocusablesPrim #-}
getFocusablesPrim :: Context -> IO (PrimArray WidgetId)
getFocusablesPrim ctx = do
  count <- readIORef (ctxFocusablesCount ctx)
  arr <- readIORef (ctxFocusables ctx)
  freezePrimArray arr 0 count

-- =============================================================================
-- Modal & Overlay
-- =============================================================================

textInputEditActive :: Context -> IO Bool
textInputEditActive ctx = do
  focus <- readIORef (ctxFocusId ctx)
  menu <- readIORef (ctxTextInputMenu ctx)
  pure (hashWidgetId focus /= 0 || menu /= Nothing)

modalActive :: Context -> IO Bool
modalActive ctx = do
  was <- readIORef (ctxModalWasActive ctx)
  now <- readIORef (ctxModalActive ctx)
  pure (was || now)

overlayConsumesQuit :: Context -> Input -> IO Bool
overlayConsumesQuit ctx inp = do
  consumed <- readIORef (ctxEscapeConsumed ctx)
  let esc = inputKeysElem KeyEscape (inputKeys inp)
  pure (esc && consumed)

markEscapeConsumed :: Context -> IO ()
markEscapeConsumed ctx = writeIORef (ctxEscapeConsumed ctx) True

pointerBlockedByModal :: Context -> IO Bool
pointerBlockedByModal ctx = do
  depth <- readIORef (ctxModalDepth ctx)
  if depth > 0 then pure False else modalActive ctx

pointerBlockedByOverlay :: Context -> V2 -> IO Bool
pointerBlockedByOverlay ctx mouse = do
  modalBlocked <- pointerBlockedByModal ctx
  blocked <-
    if modalBlocked
      then pure True
      else do
        mTop <- cachedTopmost ctx mouse
        case mTop of
          Nothing -> pure False
          Just top -> do
            mCur <- readIORef (ctxCurrentFloatingId ctx)
            pure (mCur /= Just top)
  writeIORef (ctxLastPointerBlocked ctx) blocked
  pure blocked

cachedTopmost :: Context -> V2 -> IO (Maybe WidgetId)
cachedTopmost ctx mouse = do
  cache <- readIORef (ctxOverlayTopmostCache ctx)
  case cache of
    Just (p, t) | p == mouse -> pure t
    _ -> do
      t <- topmostFloatingAtMouse ctx mouse
      writeIORef (ctxOverlayTopmostCache ctx) (Just (mouse, t))
      pure t

topmostFloatingAtMouse :: Context -> V2 -> IO (Maybe WidgetId)
topmostFloatingAtMouse ctx mouse = do
  rects <- readIORef (ctxPrevFloatingRects ctx)
  order <- readIORef (ctxPrevFloatingOrder ctx)
  if IM.null rects && null order
    then pure Nothing
    else
      let hit k = case IM.lookup k rects of
            Just r | rectW r > 0 && rectH r > 0 && rectContains r mouse -> True
            _ -> False
          picked = foldl (\acc k -> if hit k then Just k else acc) Nothing order
       in case picked of
            Just k -> pure (Just (WidgetId (fromIntegral k)))
            Nothing -> pure Nothing

seedFloatingPanel :: Context -> WidgetId -> Rect -> IO ()
seedFloatingPanel ctx wid rect
  | rectW rect <= 0 || rectH rect <= 0 = pure ()
  | otherwise = do
      let k = intKey wid
      rects <- readIORef (ctxPrevFloatingRects ctx)
      writeIORef (ctxPrevFloatingRects ctx) (IM.insert k rect rects)
      order <- readIORef (ctxPrevFloatingOrder ctx)
      writeIORef (ctxPrevFloatingOrder ctx) (filter (/= k) order ++ [k])
      writeIORef (ctxOverlayTopmostCache ctx) Nothing

beginModal :: Context -> IO ()
beginModal ctx = do
  writeIORef (ctxModalActive ctx) True
  depth <- readIORef (ctxModalDepth ctx)
  writeIORef (ctxModalDepth ctx) (depth + 1)

endModal :: Context -> IO ()
endModal ctx = do
  depth <- readIORef (ctxModalDepth ctx)
  writeIORef (ctxModalDepth ctx) (max 0 (depth - 1))

beginFrameModal :: Context -> IO ()
beginFrameModal ctx = do
  modalNow <- readIORef (ctxModalActive ctx)
  writeIORef (ctxModalWasActive ctx) modalNow
  writeIORef (ctxModalActive ctx) False
  writeIORef (ctxModalDepth ctx) 0
  writeIORef (ctxOverlayTopmostCache ctx) Nothing
  writeIORef (ctxCurrentFloatingId ctx) Nothing
  writeIORef (ctxLastPointerBlocked ctx) False

modalDamageFlip :: Context -> IO Bool
modalDamageFlip ctx = do
  was <- readIORef (ctxModalWasActive ctx)
  now <- readIORef (ctxModalActive ctx)
  pure (was /= now)

-- =============================================================================
-- Animation
-- =============================================================================

refreshAnimating :: Context -> IO ()
refreshAnimating ctx = do
  anims <- readIORef (ctxAnimations ctx)
  writeIORef (ctxAnyAnimating ctx) (any animInProgress anims)

{-# INLINE anyAnimating #-}
anyAnimating :: Context -> IO Bool
anyAnimating ctx = readIORef (ctxAnyAnimating ctx)

{-# INLINE startAnimation #-}
startAnimation :: Context -> WidgetId -> Float -> Float -> Float -> IO ()
startAnimation ctx wid start end dur = startAnimationEase ctx wid start end dur EaseLinear

{-# INLINE startAnimationEase #-}
startAnimationEase :: Context -> WidgetId -> Float -> Float -> Float -> Ease -> IO ()
startAnimationEase ctx wid start end dur ease = startAnimationEaseDelay ctx wid start end dur ease 0

{-# INLINE startAnimationEaseDelay #-}
startAnimationEaseDelay :: Context -> WidgetId -> Float -> Float -> Float -> Ease -> Float -> IO ()
startAnimationEaseDelay ctx wid start end dur ease delay = do
  let key = intKey wid
  anims <- readIORef (ctxAnimations ctx)
  if dur <= 0 || approxEq start end
    then settleKey ctx key end
    else do
      let req = max 0 delay
          (elapsed, delayLeft) = case IM.lookup key anims of
            Just (EaseAnim aStart aEnd aDur aElapsed aEase aDelay aDelayReq)
              | approxEq aStart start && approxEq aEnd end && aEase == ease && approxEq aDur dur && approxEq req aDelayReq ->
                  (aElapsed, aDelay)
            _ -> (0, req)
      rest <- readIORef (ctxAnimRest ctx)
      writeIORef (ctxAnimRest ctx) (IM.delete key rest)
      writeIORef (ctxAnimations ctx) (IM.insert key (EaseAnim start end dur elapsed ease delayLeft req) anims)
      writeIORef (ctxAnyAnimating ctx) True
      markDirtyIfOrphan ctx key

startSpring :: Context -> WidgetId -> SpringParams -> Float -> IO ()
startSpring ctx wid params target = do
  let key = intKey wid
  anims <- readIORef (ctxAnimations ctx)
  cur <- case IM.lookup key anims of
    Just a -> pure (animationValue a)
    Nothing -> do
      rest <- readIORef (ctxAnimRest ctx)
      pure (IM.findWithDefault 0 key rest)
  let (pos, vel) = case IM.lookup key anims of
        Just (SpringAnim p v _ _) -> (p, v)
        _ -> (cur, 0)
  if abs (pos - target) <= springEps && abs vel <= springEps
    then settleKey ctx key target
    else do
      rest <- readIORef (ctxAnimRest ctx)
      writeIORef (ctxAnimRest ctx) (IM.delete key rest)
      writeIORef (ctxAnimations ctx) (IM.insert key (SpringAnim pos vel target params) anims)
      writeIORef (ctxAnyAnimating ctx) True
      markDirtyIfOrphan ctx key

{-# INLINE setAnimationValue #-}
setAnimationValue :: Context -> WidgetId -> Float -> IO ()
setAnimationValue ctx wid val = settleKey ctx (intKey wid) val

{-# INLINE tickAnimations #-}
tickAnimations :: Context -> Float -> IO ()
tickAnimations ctx dt = do
  anims <- readIORef (ctxAnimations ctx)
  if IM.null anims
    then do
      writeIORef (ctxAnyAnimating ctx) False
      writeIORef (ctxAnimSettled ctx) False
    else do
      let stepped = IM.map (stepAnim dt) anims
          (live, done) = IM.partition animInProgress stepped
      writeIORef (ctxAnimations ctx) live
      rest <- readIORef (ctxAnimRest ctx)
      writeIORef (ctxAnimRest ctx) (IM.foldlWithKey' writeRest rest done)
      writeIORef (ctxAnyAnimating ctx) (not (IM.null live))
      writeIORef (ctxAnimSettled ctx) (not (IM.null done))

markDirtyIfOrphan :: Context -> Int -> IO ()
markDirtyIfOrphan ctx key = do
  mprev <- getPrevRectByKey ctx key
  hasNow <- nodeHasKey ctx key
  when (isNothing mprev && not hasNow) (markDirty ctx)

nodeHasKey :: Context -> Int -> IO Bool
nodeHasKey ctx key = do
  mIdx <- lookupNodeByKey (ctxNodeArena ctx) key
  case mIdx of
    Nothing -> pure False
    Just idx -> do
      (_, _, w, h) <- getRect (ctxNodeArena ctx) idx
      pure (w > 0 && h > 0)

settleKey :: Context -> Int -> Float -> IO ()
settleKey ctx key val = do
  anims <- readIORef (ctxAnimations ctx)
  rest <- readIORef (ctxAnimRest ctx)
  let prevRest = IM.findWithDefault 0 key rest
      prevLive = fmap animationValue (IM.lookup key anims)
      changed = case prevLive of
        Just v -> not (approxEq v val)
        Nothing -> not (approxEq prevRest val)
  writeIORef (ctxAnimations ctx) (IM.delete key anims)
  writeIORef (ctxAnimRest ctx) (if approxEq val 0 then IM.delete key rest else IM.insert key val rest)
  when changed (markDirty ctx)
  refreshAnimating ctx

{-# INLINE getAnimationValue #-}
getAnimationValue :: Context -> WidgetId -> IO Float
getAnimationValue ctx wid = do
  let key = intKey wid
  anims <- readIORef (ctxAnimations ctx)
  case IM.lookup key anims of
    Just a -> pure (animationValue a)
    Nothing -> do
      rest <- readIORef (ctxAnimRest ctx)
      pure (IM.findWithDefault 0 key rest)
