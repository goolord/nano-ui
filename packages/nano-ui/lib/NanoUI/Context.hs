module NanoUI.Context
  ( Context (..)
  , MeasureCacheKey
  , TextInputMenu (..)
  , TextInputDrag (..)
  , TextFieldClickCell (..)
  , WindowResizeEdge (..)
  , WindowResizeDrag (..)
  , DamageState (..)
  , OverlayState (..)
  , AnimationState (..)
  , DrawingCacheState (..)
  , DrawingEntry (..)
  , DrawFitCache (..)
  , SpanCacheEntry (..)
  , WidgetTextCacheEntry (..)
  , WidgetTextPlacement (..)
  , InteractionState (..)
  , initialInteractionState
  , initialDamageState
  , initialOverlayState
  , initialAnimationState
  , initialDrawingCacheState
  , getsInteraction
  , modifyInteraction
  , getsOverlay
  , modifyOverlay
  , getsDamage
  , modifyDamage
  , getScrollDrag
  , setScrollDrag
  , getTextInputDrag
  , setTextInputDrag
  , getTextFieldClickCell
  , setTextFieldClickCell
  , getTextInputMenu
  , setTextInputMenu
  , setTextEditLastAction
  , takeTextEditLastAction
  , getSelectDropPress
  , setSelectDropPress
  , setOpenSelectDrop
  , getOpenSelectDrop
  , getMenuPointerGesture
  , setMenuPointerGesture
  , getWindowDrag
  , setWindowDrag
  , getWindowResize
  , setWindowResize
  , intKey
  , markDirty
  , clearDirty
  , isDirty
  , setWakeLoop
  , takeDamage
  , getLastWindowSize
  , setDamageAndWindowSize
  , DamageRequest (..)
  , requestDamage
  , damageWidget
  , damageKey
  , damageRect
  , damagePeers
  , damageFull
  , getDamageRequests
  , registerPopupConfig
  , lookupPopupConfig
  , registerDrawing
  , lookupDrawing
  , cachedDrawingOps
  , cachedWidgetLayout
  , lookupDrawFitEnvelope
  , pruneDrawOpCache
  , CustomMeasureFn
  , CustomDrawContext (..)
  , CustomDrawBuild
  , registerCustomDrawing
  , lookupCustomDrawing
  , cachedCustomDrawingOps
  , registerCustomMeasure
  , lookupCustomMeasure
  , registerCustomCursor
  , lookupCustomCursor
  , registerCustomDamageSlop
  , lookupCustomDamageSlop
  , getWidgetNodeTypes
  , setWidgetNodeTypes
  , resetDrawingScopeCache
  , getStore
  , setStore
  , deleteWidgetStore
  , getStoreBool
  , setStoreBool
  , writeStoreInt
  , writeStoreFloat
  , writeStoreText
  , writeStoreBool
  , isDisabled
  , getScrollOffset
  , setScrollOffset
  , getScrollOffset2D
  , setScrollOffset2D
  , setScrollConfig
  , defaultScrollConfig
  , linkScrollAxes
  , getPrevRect
  , getPrevClipRect
  , getPrevRects
  , getPrevClips
  , setPrevRectsAndClips
  , getPrevNodeTexts
  , setPrevNodeTexts
  , atlasTextureId
  , registerImage
  , registerImages
  , lookupImageUv
  , atlasSnapshot
  , withFontMetrics
  , withMonoFontMetrics
  , withMeasureText
  , withFontResolver
  , wrapMeasureCache
  , clearMeasureCache
  , ensureMetricCaches
  , hasCustomLayoutInputs
  , withExternalText
  , withTheme
  , setTheme
  , getTheme
  , withClipboard
  , enableMeasureCache
  , setHost
  , setDrawSnapScale
  , setDrawSquareGeometry
  , setDrawExternalText
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
  -- Modal & Overlay
  , textInputEditActive
  , modalActive
  , overlayConsumesQuit
  , markEscapeConsumed
  , pointerBlockedByModal
  , pointerBlockedByOverlay
  , armMenuPointerCapture
  , seedFloatingPanel
  , beginModal
  , endModal
  , beginFrameModal
  , modalDamageFlip
  , getCurrentFloatingId
  , setCurrentFloatingId
  , getLastPointerBlocked
  , getPrevFloatingRects
  , setPrevFloatingPanels
  , getFloatingAncestor
  , setFloatingAncestor
  -- Animation
  , anyAnimating
  , getLiveAnimations
  , takeAnimSettled
  , lookupAnimation
  , getAnimRectless
  , setAnimRectless
  , startAnimation
  , startAnimationEase
  , startAnimationEaseDelay
  , stopAnimation
  , startSpring
  , setAnimationValue
  , tickAnimations
  , getAnimationValue
  , getAnimRest
  , pruneAnimRest
  , FrameMsg (..)
  , decodeMessages
  , reduceMessages
  , reduceUpdates
  , WidgetStore (..)
  , bumpMirror
  , slotKey
  , slotDisabled
  , slotCursor
  , slotAnchor
  , slotDrag
  , slotDragW
  , slotWinSize
  , boolInt
  , intBool
  , anySelectOpen
  , isSelectOpen
  , setSelectOpen
  , closeSelects
  , Ease (..)
  , Animation (..)
  , SpringParams (..)
  , presetBouncy
  , presetSmooth
  , presetStiff
  , applyEase
  , easeSameSpec
  , approxEq
  , animInProgress
  ) where

import Control.Monad (foldM, forM, when)
import Data.ByteString (ByteString)
import Data.Dynamic (fromDynamic, toDyn)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.IntMap.Strict qualified as IM
import Data.Map.Strict qualified as Map
import Data.Primitive.PrimArray
  ( copyMutablePrimArray
  , newPrimArray
  , readPrimArray
  , writePrimArray
  )
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Typeable (Typeable, typeOf, typeRep)
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr)

import NanoUI.Animation
  ( Animation (..)
  , Ease (..)
  , SpringParams (..)
  , animInProgress
  , applyEase
  , approxEq
  , easeSameSpec
  , presetBouncy
  , presetSmooth
  , presetStiff
  )
import NanoUI.Atlas (atlasTextureId)
import NanoUI.Atlas qualified as Atlas
import NanoUI.Context.Animation
import NanoUI.Context.Core
import NanoUI.Context.Drawing
import NanoUI.Context.Overlay
import NanoUI.Context.Scroll
import NanoUI.Context.Types
  ( AnimationState (..)
  , Context (..)
  , CustomDrawBuild
  , CustomDrawContext (..)
  , CustomMeasureFn
  , DamageRequest (..)
  , DamageState (..)
  , DrawFitCache (..)
  , DrawingCacheState (..)
  , DrawingEntry (..)
  , FrameMsg (..)
  , InteractionState (..)
  , MeasureCacheKey
  , MetricSource (..)
  , OverlayState (..)
  , SpanCacheEntry (..)
  , TextFieldClickCell (..)
  , TextInputDrag (..)
  , TextInputMenu (..)
  , WidgetTextCacheEntry (..)
  , WidgetTextPlacement (..)
  , WindowResizeDrag (..)
  , WindowResizeEdge (..)
  , decodeMessages
  , initialAnimationState
  , initialDamageState
  , initialDrawingCacheState
  , initialInteractionState
  , initialOverlayState
  , intKey
  , reduceMessages
  , reduceUpdates
  )
import NanoUI.Draw (newDrawArena)
import NanoUI.Draw qualified as Draw
import NanoUI.Font (FontMetrics, fmLineHeight, measureTextIO, monospaceMetrics, scaleFontMetrics)
import NanoUI.Frame.SpanArena (newSpanArena)
import NanoUI.Frame.Scroll.Geometry (defaultScrollConfig)
import NanoUI.Id (WidgetId (..), initialIdContext)
import NanoUI.Layout.Arena (newNodeArena)
import NanoUI.Store
  ( WidgetStore (..)
  , anySelectOpen
  , boolInt
  , bumpMirror
  , closeSelects
  , emptyWidgetStore
  , intBool
  , isSelectOpen
  , ptrEq
  , setSelectOpen
  , slotAnchor
  , slotCursor
  , slotDisabled
  , slotDrag
  , slotDragW
  , slotKey
  , slotWinSize
  )
import NanoUI.Style (FontStyle, FontVariant (..), FontWeight, Theme, defaultLayout, defaultTheme)
import NanoUI.Types (ImageId)

{-# INLINE registerImage #-}
registerImage :: Context -> ImageId -> Int -> Int -> ByteString -> IO Bool
registerImage ctx iid w h px = do
  ok <- Atlas.registerImage (ctxImageAtlas ctx) iid w h px
  when ok (markDirty ctx)
  pure ok

registerImages :: Foldable f => Context -> f (ImageId, Int, Int, ByteString) -> IO Bool
registerImages ctx = foldM register True
  where
    register ok (iid, w, h, px) = do
      result <- registerImage ctx iid w h px
      pure (ok && result)

{-# INLINE lookupImageUv #-}
lookupImageUv :: Context -> ImageId -> IO (Maybe (Float, Float, Float, Float))
lookupImageUv ctx = Atlas.lookupImageUv (ctxImageAtlas ctx)

{-# INLINE atlasSnapshot #-}
atlasSnapshot :: Context -> IO (Maybe (Int, Int, ForeignPtr Word8, Int))
atlasSnapshot ctx = Atlas.atlasSnapshot (ctxImageAtlas ctx)

-- | Metrics for a font variant scaled to line height @sz@, and the scale
-- factor applied (1 when @sz@ or the base line height is not positive).
{-# INLINE resolveScale #-}
resolveScale :: Context -> Float -> FontVariant -> (FontMetrics, Float)
resolveScale ctx sz var =
  let baseFm = if var == FontMono then ctxMonoFontMetrics ctx else ctxFontMetrics ctx
      scale =
        if sz > 0 && fmLineHeight baseFm > 0
          then sz / fmLineHeight baseFm
          else 1.0
   in (if scale /= 1.0 then scaleFontMetrics scale baseFm else baseFm, scale)

defaultResolveFont :: Context -> Float -> FontWeight -> FontStyle -> FontVariant -> IO (FontMetrics, Bool)
defaultResolveFont ctx sz _w _st var = pure (fst (resolveScale ctx sz var), False)

defaultResolveMeasure :: Context -> Float -> FontWeight -> FontStyle -> FontVariant -> Text -> IO (Float, Float)
defaultResolveMeasure ctx sz _w _st var txt
  | var == FontMono = measureTextIO textFm txt
  | scale /= 1.0 = (\(w, h) -> (w * scale, h * scale)) <$> ctxMeasureText ctx txt
  | otherwise = ctxMeasureText ctx txt
  where
    (textFm, scale) = resolveScale ctx sz var

{-# INLINE withFontResolver #-}
withFontResolver ::
  Context ->
  (Float -> FontWeight -> FontStyle -> FontVariant -> IO (FontMetrics, Bool)) ->
  (Float -> FontWeight -> FontStyle -> FontVariant -> Text -> IO (Float, Float)) ->
  Context
withFontResolver ctx rf rm = trackMetricSource ctx {ctxResolveFont = rf, ctxResolveMeasure = rm}

withFontMetrics :: Context -> FontMetrics -> Context
withFontMetrics ctx fm =
  let ctx' =
        ctx
          { ctxFontMetrics = fm
          , ctxMeasureText = measureTextIO fm
          }
   in trackMetricSource ctx'
        { ctxResolveFont = defaultResolveFont ctx'
        , ctxResolveMeasure = defaultResolveMeasure ctx'
        }

withMonoFontMetrics :: Context -> FontMetrics -> Context
withMonoFontMetrics ctx mono =
  let ctx' = ctx {ctxMonoFontMetrics = mono}
   in trackMetricSource ctx'
        { ctxResolveFont = defaultResolveFont ctx'
        , ctxResolveMeasure = defaultResolveMeasure ctx'
        }

withMeasureText :: Context -> (Text -> IO (Float, Float)) -> Context
withMeasureText ctx fn =
  let ctx' = ctx {ctxMeasureText = fn}
   in trackMetricSource ctx'
        { ctxResolveMeasure = defaultResolveMeasure ctx'
        }

-- Keep the identity boxed. No structural callback comparison or unsafe pure
-- mutation is needed, and repeated frames with the same Context do no work.
trackMetricSource :: Context -> Context
trackMetricSource ctx =
  ctx {ctxMetricSource = MetricSource
    (ctxFontMetrics ctx) (ctxMonoFontMetrics ctx) (ctxMeasureText ctx)
    (ctxResolveFont ctx) (ctxResolveMeasure ctx)}

-- | Called once before building a frame. Context configuration remains pure;
-- cache invalidation happens at the IO boundary, including when alternating
-- between differently configured Contexts that share their backing stores.
ensureMetricCaches :: Context -> IO ()
ensureMetricCaches ctx = do
  -- Compare evaluated identities: passed unevaluated, the selector application
  -- is a fresh thunk without optimisation, so the check would miss every
  -- frame and force full damage with cleared caches.
  let !current = ctxMetricSource ctx
  previous <- readIORef (ctxLastMetricSource ctx)
  case previous of
    Just source | ptrEq source current -> pure ()
    _ -> do
      clearMeasureCache ctx
      damageFull ctx
      markDirty ctx

cacheMeasureText ::
  IORef (HashMap MeasureCacheKey (Float, Float)) ->
  Float ->
  (Text -> IO (Float, Float)) ->
  Text ->
  IO (Float, Float)
cacheMeasureText ref scale base txt = do
  let key = (txt, scale)
  m <- readIORef ref
  case HashMap.lookup key m of
    Just sz -> pure sz
    Nothing -> do
      sz <- base txt
      modifyIORef' ref (HashMap.insert key sz)
      pure sz

wrapMeasureCache :: Float -> Context -> (Text -> IO (Float, Float)) -> Context
wrapMeasureCache scale ctx measure =
  case ctxMeasureCache ctx of
    Nothing -> trackMetricSource ctx {ctxMeasureText = measure}
    Just ref -> trackMetricSource ctx {ctxMeasureText = cacheMeasureText ref scale measure}

-- | Drop text span, widget text and whole-layout caches and bump the metric
-- generation, for changes that alter how text lays out.
invalidateTextCaches :: Context -> IO ()
invalidateTextCaches ctx = do
  writeIORef (ctxSpanCache ctx) IM.empty
  writeIORef (ctxWidgetTextCache ctx) IM.empty
  writeIORef (ctxLayoutCache ctx) Nothing
  modifyIORef' (ctxMetricGen ctx) (+ 1)

clearMeasureCache :: Context -> IO ()
clearMeasureCache ctx = do
  -- Store the evaluated source so 'ensureMetricCaches' can match its identity.
  let !source = ctxMetricSource ctx
  writeIORef (ctxLastMetricSource ctx) (Just source)
  invalidateTextCaches ctx
  case ctxMeasureCache ctx of
    Just ref -> writeIORef ref HashMap.empty
    Nothing -> pure ()

withExternalText :: Context -> Bool -> Context
withExternalText ctx ext = ctx {ctxExternalText = ext}

withTheme :: Context -> Theme -> IO Context
withTheme ctx theme = do
  writeIORef (ctxTheme ctx) theme
  pure ctx

setTheme :: Context -> Theme -> IO ()
setTheme ctx th = do
  cur <- readIORef (ctxTheme ctx)
  when (cur /= th) $ do
    writeIORef (ctxTheme ctx) th
    invalidateTextCaches ctx
    damageFull ctx
    markDirty ctx

getTheme :: Context -> IO Theme
getTheme ctx = readIORef (ctxTheme ctx)

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

-- | Set the device pixel scale used to snap geometry origins/endpoints to
-- whole pixels. The SDL backend calls this when the display scale is synced.
{-# INLINE setDrawSnapScale #-}
setDrawSnapScale :: Context -> Float -> IO ()
setDrawSnapScale ctx s = Draw.setDrawSnapScale (ctxDrawArena ctx) s

-- | Emit rounded shapes and AA strokes as flat, axis-aligned fills. Software
-- framebuffer hosts enable this so every primitive is a solid quad.
{-# INLINE setDrawSquareGeometry #-}
setDrawSquareGeometry :: Context -> Bool -> IO ()
setDrawSquareGeometry ctx = Draw.setDrawSquareGeometry (ctxDrawArena ctx)

-- | Skip text quads in the draw buffer. Hosts that rasterize text from the
-- collected text spans enable this.
{-# INLINE setDrawExternalText #-}
setDrawExternalText :: Context -> Bool -> IO ()
setDrawExternalText ctx = Draw.setDrawExternalText (ctxDrawArena ctx)

{-# INLINE askHostIO #-}
askHostIO :: forall a. (Typeable a) => Context -> IO (Maybe a)
askHostIO ctx = do
  m <- readIORef (ctxHost ctx)
  let k = typeRep (Proxy :: Proxy a)
  pure (Map.lookup k m >>= fromDynamic)

{-# INLINE pushMessage #-}
pushMessage :: Context -> FrameMsg -> IO ()
pushMessage ctx msg = modifyIORef' (ctxMessages ctx) (msg :)

{-# INLINE drainMessages #-}
drainMessages :: Context -> IO [FrameMsg]
drainMessages ctx = do
  msgs <- readIORef (ctxMessages ctx)
  writeIORef (ctxMessages ctx) []
  pure (reverse msgs)

-- =============================================================================
-- Constructors
-- =============================================================================

newContext :: IO Context
newContext = do
  nodeArena <- newNodeArena
  drawArena <- newDrawArena
  ctxHotId <- newIORef (WidgetId 0)
  ctxLastHotId <- newIORef (WidgetId 0)
  ctxActiveId <- newIORef (WidgetId 0)
  ctxClickedId <- newIORef (WidgetId 0)
  ctxReleaseClickedId <- newIORef (WidgetId 0)
  ctxFocusId <- newIORef (WidgetId 0)
  ctxStore <- newIORef emptyWidgetStore
  ctxDamageState <- newIORef initialDamageState
  ctxOverlayState <- newIORef initialOverlayState
  ctxAnimationState <- newIORef initialAnimationState
  ctxDrawingCache <- newIORef initialDrawingCacheState
  ctxIdContext <- newIORef initialIdContext
  ctxContainerStack <- newIORef []
  ctxMessages <- newIORef []
  let initCap = 64
  ctxFocusables <- newIORef =<< newPrimArray initCap
  ctxFocusablesCount <- newIORef 0
  ctxFocusablesCap <- newIORef initCap
  ctxSpanBase <- newSpanArena 64
  ctxSpanOverlay <- newSpanArena 64
  ctxInteractionState <- newIORef initialInteractionState
  ctxImageAtlas <- Atlas.newImageAtlas
  ctxWakeLoop <- newIORef Nothing
  ctxHost <- newIORef Map.empty
  ctxDefaultLayout <- newIORef defaultLayout
  ctxTheme <- newIORef defaultTheme
  ctxSpanCache <- newIORef IM.empty
  ctxWidgetTextCache <- newIORef IM.empty
  ctxLayoutCache <- newIORef Nothing
  ctxMetricGen <- newIORef 0
  ctxLastMetricSource <- newIORef Nothing
  ctxPaintFull <- newIORef True
  let fm0 = monospaceMetrics 12
      ctx = Context
        { ctxNodeArena = nodeArena
        , ctxDrawArena = drawArena
        , ctxHotId
        , ctxLastHotId
        , ctxActiveId
        , ctxClickedId
        , ctxReleaseClickedId
        , ctxFocusId
        , ctxStore
        , ctxDamageState
        , ctxOverlayState
        , ctxAnimationState
        , ctxDrawingCache
        , ctxIdContext
        , ctxFontMetrics = fm0
        , ctxMonoFontMetrics = fm0
        , ctxMeasureText = measureTextIO fm0
        , ctxResolveFont = defaultResolveFont ctx
        , ctxResolveMeasure = defaultResolveMeasure ctx
        , ctxMeasureCache = Nothing
        , ctxSpanCache
        , ctxWidgetTextCache
        , ctxLayoutCache
        , ctxMetricGen
        , ctxMetricSource = InitialMetricSource
        , ctxLastMetricSource
        , ctxPaintFull
        , ctxExternalText = False
        , ctxTheme
        , ctxContainerStack
        , ctxMessages
        , ctxFocusables
        , ctxFocusablesCount
        , ctxFocusablesCap
        , ctxSpanBase
        , ctxSpanOverlay
        , ctxInteractionState
        , ctxClipboardGet = pure Nothing
        , ctxClipboardSet = \_ -> pure False
        , ctxImageAtlas
        , ctxWakeLoop
        , ctxHost
        , ctxDefaultLayout
        }
  pure ctx

newPixelHostContext :: IO Context
newPixelHostContext = do
  ctx0 <- newContext
  ctx <- enableMeasureCache ctx0
  withTheme (withExternalText (withFontMetrics ctx (monospaceMetrics 16)) True) defaultTheme

-- =============================================================================
-- Focus
-- =============================================================================

{-# INLINE getFocusId #-}
getFocusId :: Context -> IO WidgetId
getFocusId ctx = readIORef (ctxFocusId ctx)

{-# INLINE getHotId #-}
getHotId :: Context -> IO WidgetId
getHotId ctx = readIORef (ctxHotId ctx)

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
