{-# LANGUAGE RecordWildCards #-}

-- | The 'Context' a view runs against, and the operations on its state:
-- focus, dirty flags and damage, the widget store, drawing and measure
-- caches, overlays, and host hooks. Backends and advanced widgets use this
-- module; views normally only need "NanoUI".
module NanoUI.Internal.Context
  ( Context (..)
  , TextInputMenu (..)
  , TextInputDrag (..)
  , TextFieldClickCell (..)
  , WindowResizeEdge (..)
  , WindowResizeDrag (..)
  , DamageState (..)
  , OverlayState (..)
  , DrawingCacheState (..)
  , DrawingEntry (..)
  , SpanCacheEntry (..)
  , SpanLines (..)
  , WidgetTextCacheEntry (..)
  , WidgetTextPlacement (..)
  , InteractionState (..)
  , PointerRoute (..)
  , getsInteraction
  , modifyInteraction
  , getsOverlay
  , modifyOverlay
  , getsDamage
  , modifyDamage
  , takeTextEditLastAction
  , pointerHeldOffLayers
  , intKey
  , markDirty
  , markDirtyCovered
  , clearDirty
  , isDirty
  , setWakeLoop
  , requestWakeAt
  , requestWakeAfter
  , getWakeAt
  , clearWakeAt
  , takeDamage
  , takeDamagePieces
  , DamageRequest (..)
  , requestDamage
  , damageWidget
  , damageKey
  , damageRect
  , damagePeers
  , damageFull
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
  , registerCustomEntry
  , lookupCustomDrawing
  , cachedCustomDrawingOps
  , refreshCustomDrawingOps
  , drawingOpsStale
  , CustomDrawingEntry (..)
  , registerCustomMeasure
  , lookupCustomMeasure
  , customMeasureHooks
  , lookupCustomCursor
  , lookupCustomDamageSlop
  , isPointerTracked
  , resetDrawingScopeCache
  , getStore
  , setStore
  , modifyStore
  , writeSlots
  , writeSlot
  , adoptSlot
  , recordSlot
  , getStoreBool
  , writeStoreBool
  , isDisabled
  , beginThemeScopes
  , pushThemeScope
  , themeScopesChanged
  , scopeTheme
  , scopeRawTheme
  , currentTheme
  , nodeTheme
  , widgetTheme
  , getScrollOffset
  , setScrollOffset
  , getScrollOffset2D
  , setScrollOffset2D
  , setScrollConfig
  , linkScrollAxes
  , ScrollTuning (..)
  , defaultScrollTuning
  , getScrollTuning
  , setScrollTuning
  , getScrollStep
  , setScrollStep
  , resolveScrollStep
  , ScrollAxes (..)
  , ScrollMetrics (..)
  , getScrollMetrics
  , cacheScrollMetrics
  , beginScrollMetrics
  , getScrollOffsetIn
  , setScrollOffsetIn
  , ScrollBehavior (..)
  , ScrollAlign (..)
  , scrollTo
  , scrollBy
  , scrollPages
  , scrollToStart
  , scrollToEnd
  , scrollIntoView
  , scrollRectIntoView
  , applyScrollTarget
  , scrollTargetOffset
  , scrollGliding
  , clampScrollOffset
  , stepScrollGlides
  , getPrevRect
  , getPrevClipRect
  , atlasTextureId
  , registerImage
  , registerImages
  , lookupImageUv
  , atlasSnapshot
  , atlasChanges
  , AtlasUpload (..)
  , withFontMetrics
  , withMonoFontMetrics
  , withMeasureText
  , withFontResolver
  , wrapMeasureCache
  , clearMeasureCache
  , cachedWrapText
  , ensureMetricCaches
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
  , hostOrInit
  , pushMessage
  , drainMessages
  -- Constructors
  , newContext
  , newPixelHostContext
  -- Focus
  , getFocusId
  , getFocusVisible
  , getHotId
  , registerFocusable
  , getFocusables
  -- Modal & Overlay
  , textInputEditActive
  , modalActive
  , overlayConsumesQuit
  , markEscapeConsumed
  , markTabConsumed
  , tabConsumed
  , pointerBlockedByModal
  , routedInput
  , floatingLayerAt
  , seedFloatingPanel
  , beginModal
  , endModal
  , beginFrameModal
  , modalDamageFlip
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
  , startSpring
  , keepAnimationAlive
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
  , Slot (..)
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
  )
where

import Control.Monad (foldM, forM, when, (<=<))
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.Dynamic (fromDynamic, toDyn)
import Data.List (find)
import Data.Maybe (fromMaybe, isJust)
import Data.HashMap.Strict qualified as HashMap
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.IntMap.Strict qualified as IM
import Data.Map.Strict qualified as Map
import Data.Primitive.PrimArray
  ( newPrimArray
  , readPrimArray
  , writePrimArray
  , getSizeofMutablePrimArray
  , resizeMutablePrimArray
  )
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Typeable (Typeable, typeOf, typeRep)
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr)

import NanoUI.Internal.Animation
  ( Animation (..)
  , Ease (..)
  , SpringParams (..)
  , applyEase
  , approxEq
  , easeSameSpec
  , presetBouncy
  , presetSmooth
  , presetStiff
  )
import NanoUI.Internal.Atlas (AtlasUpload (..), atlasTextureId)
import NanoUI.Internal.Atlas qualified as Atlas
import NanoUI.Internal.Context.Animation
import NanoUI.Internal.Context.Core
import NanoUI.Internal.Context.Drawing
import NanoUI.Internal.Context.Overlay
import NanoUI.Internal.Context.Scroll
import NanoUI.Internal.Context.Types
  ( Context (..)
  , CustomDrawBuild
  , CustomDrawContext (..)
  , CustomDrawingEntry (..)
  , CustomMeasureFn
  , DamageRequest (..)
  , DamageState (..)
  , DrawingCacheState (..)
  , DrawingEntry (..)
  , FrameMsg (..)
  , InteractionState (..)
  , MeasureCache (..)
  , MetricSource (..)
  , WrapCache (..)
  , emptyMeasureCache
  , emptyWrapCache
  , OverlayState (..)
  , PointerRoute (..)
  , SpanCacheEntry (..)
  , SpanLines (..)
  , TextFieldClickCell (..)
  , TextInputDrag (..)
  , TextInputMenu (..)
  , WidgetTextCacheEntry (..)
  , WidgetTextPlacement (..)
  , WindowResizeDrag (..)
  , WindowResizeEdge (..)
  , decodeMessages
  , initialAnimationState
  , initialScrollState
  , initialDamageState
  , initialDrawingCacheState
  , initialInteractionState
  , initialOverlayState
  , intKey
  , reduceMessages
  , reduceUpdates
  )
import NanoUI.Internal.Draw (newDrawArena)
import NanoUI.Internal.Draw qualified as Draw
import NanoUI.Internal.Font (FontMetrics, WrapResult (..), fmLineHeight, measureTextIO, monospaceMetrics, scaleFontMetrics, wrapTextIO)
import NanoUI.Internal.Frame.SpanArena (newSpanArena)
import NanoUI.Internal.Id (WidgetId (..), initialIdContext)
import NanoUI.Internal.Layout.Arena (getArenaScope, newNodeArena)
import NanoUI.Internal.Store
  ( WidgetStore (..)
  , anySelectOpen
  , bumpMirror
  , closeSelects
  , emptyWidgetStore
  , intBool
  , isSelectOpen
  , ptrEq
  , setSelectOpen
  , Slot (..)
  , slotKey
  )
import NanoUI.Internal.Style (FontStyle, FontVariant (..), FontWeight, Theme, defaultTheme)
import NanoUI.Internal.Types (ImageId)

-- | Register tightly packed RGBA8 pixels under an image id. Width and height
-- are positive pixel counts. Returns 'False' for invalid data or atlas limits;
-- success requests a full repaint and wakes the loop.
{-# INLINE registerImage #-}
registerImage :: Context -> ImageId -> Int -> Int -> ByteString -> IO Bool
registerImage ctx iid w h px = do
  ok <- Atlas.registerImage (ctxImageAtlas ctx) iid w h px
  -- New pixels under an id already on screen change no rect or text, and
  -- packing can move other images' texels, so nothing narrower is safe.
  when ok (damageFull ctx >> markDirty ctx)
  pure ok

-- | Register every image and return whether all succeeded. Successful earlier
-- registrations remain in place if another image fails.
registerImages :: Foldable f => Context -> f (ImageId, Int, Int, ByteString) -> IO Bool
registerImages ctx =
  foldM (\ok (iid, w, h, px) -> (ok &&) <$> registerImage ctx iid w h px) True

-- | Current normalised atlas UV bounds, or 'Nothing' for an unknown image.
-- Atlas growth can change these coordinates; do not cache them across uploads.
{-# INLINE lookupImageUv #-}
lookupImageUv :: Context -> ImageId -> IO (Maybe (Float, Float, Float, Float))
lookupImageUv ctx = Atlas.lookupImageUv (ctxImageAtlas ctx)

-- | What a texture of the image atlas uploaded at generation @since@ (0 for
-- none) needs, with the atlas's size, pixels and generation.
{-# INLINE atlasChanges #-}
atlasChanges :: Context -> Int -> IO (Maybe (Int, Int, ForeignPtr Word8, Int, AtlasUpload))
atlasChanges ctx = Atlas.atlasChanges (ctxImageAtlas ctx)

-- | Atlas width, height, RGBA8 buffer, and revision for backend upload.
-- 'Nothing' means no atlas pixels have been allocated. Treat the buffer as
-- borrowed mutable storage and upload it before further image registration.
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

-- | Return a context with size/weight/style/variant font and measurement
-- callbacks. Use the returned context for later frames; shared text caches
-- are invalidated at the next frame boundary.
{-# INLINE withFontResolver #-}
withFontResolver ::
  Context ->
  (Float -> FontWeight -> FontStyle -> FontVariant -> IO (FontMetrics, Bool)) ->
  (Float -> FontWeight -> FontStyle -> FontVariant -> Text -> IO (Float, Float)) ->
  Context
withFontResolver ctx rf rm = trackMetricSource ctx {ctxResolveFont = rf, ctxResolveMeasure = rm}

-- | Replace base metrics and rebuild default measurement/resolution callbacks.
-- Returns a configured context sharing the original session state.
withFontMetrics :: Context -> FontMetrics -> Context
withFontMetrics ctx fm =
  withDefaultResolvers ctx {ctxFontMetrics = fm, ctxMeasureText = measureTextIO fm}

-- | Replace monospace metrics and rebuild default font-resolution callbacks.
withMonoFontMetrics :: Context -> FontMetrics -> Context
withMonoFontMetrics ctx mono = withDefaultResolvers ctx {ctxMonoFontMetrics = mono}

-- | Rebuild the default font and measurement resolvers over @ctx@'s metrics.
withDefaultResolvers :: Context -> Context
withDefaultResolvers ctx =
  trackMetricSource ctx
    { ctxResolveFont = defaultResolveFont ctx
    , ctxResolveMeasure = defaultResolveMeasure ctx
    }

-- | Replace proportional text measurement, returning logical width/height.
-- The callback must agree with the font used for painting.
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
  IORef MeasureCache ->
  Float ->
  (Text -> IO (Float, Float)) ->
  Text ->
  IO (Float, Float)
cacheMeasureText ref scale base txt = do
  let key = (txt, scale)
      remember sz (MeasureCache young n old)
        | n >= measureCacheCap = MeasureCache (HashMap.singleton key sz) 1 young
        | otherwise = MeasureCache (HashMap.insert key sz young) (n + 1) old
  cache@(MeasureCache young _ old) <- readIORef ref
  case HashMap.lookup key young of
    Just sz -> pure sz
    Nothing -> do
      -- A hit in the old generation moves up to the young one.
      sz <- maybe (base txt) pure (HashMap.lookup key old)
      writeIORef ref $! remember sz cache
      pure sz

-- | Measurements per generation of the measure cache.
measureCacheCap :: Int
measureCacheCap = 4096

-- | 'wrapTextIO' through the context's wrap cache ('ctxWrapCache'). @font@
-- names the font @lineW@ measures in ('NanoUI.Internal.WidgetText.textNodeFontKey').
-- A text keeps the results of the last few widths it was wrapped at, each
-- with the widths it holds for, so wrapping it again at a width that breaks
-- the same, as while a window or pane edge is dragged, measures nothing,
-- and the solve and the text spans share one wrap. A change of font metrics
-- ('ctxMetricGen') drops every result.
cachedWrapText :: Context -> Int -> (Text -> IO Float) -> Text -> Float -> IO WrapResult
cachedWrapText ctx font lineW txt maxW
  | maxW <= 0 = wrapTextIO lineW txt maxW
  | otherwise = do
      gen <- readIORef (ctxMetricGen ctx)
      WrapCache cachedGen young0 n0 old0 <- readIORef (ctxWrapCache ctx)
      let key = (txt, font)
          holds r = wrFitW r <= maxW && maxW < wrBreakW r
          current = cachedGen == gen
          young = if current then young0 else HashMap.empty
          n = if current then n0 else 0
          old = if current then old0 else HashMap.empty
          mine = HashMap.lookup key young
      case mine >>= find holds of
        Just r -> pure r
        Nothing -> do
          -- A hit in the old generation moves up to the young one.
          r <- maybe (wrapTextIO lineW txt maxW) pure (HashMap.lookup key old >>= find holds)
          let entries = r : take (wrapsPerText - 1) (fromMaybe [] mine)
              cache
                | isJust mine = WrapCache gen (HashMap.insert key entries young) n old
                | n >= wrapCacheCap = WrapCache gen (HashMap.singleton key entries) 1 young
                | otherwise = WrapCache gen (HashMap.insert key entries young) (n + 1) old
          writeIORef (ctxWrapCache ctx) $! cache
          pure r

-- | Texts per generation of the wrap cache.
wrapCacheCap :: Int
wrapCacheCap = 1024

-- | Widths the wrap cache keeps for one text.
wrapsPerText :: Int
wrapsPerText = 8

-- | Install measurement under a scale-specific cache key when caching is
-- enabled. Otherwise install the callback directly.
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
  writeIORef (ctxDerivedCache ctx) IM.empty
  writeIORef (ctxLayoutCache ctx) Nothing
  modifyIORef' (ctxMetricGen ctx) (+ 1)

-- | Clear measurement, text-placement, and whole-layout caches and advance the
-- metric generation. Does not itself request a repaint or wake the loop.
clearMeasureCache :: Context -> IO ()
clearMeasureCache ctx = do
  -- Store the evaluated source so 'ensureMetricCaches' can match its identity.
  let !source = ctxMetricSource ctx
  writeIORef (ctxLastMetricSource ctx) (Just source)
  invalidateTextCaches ctx
  mapM_ (`writeIORef` emptyMeasureCache) (ctxMeasureCache ctx)

-- | Apply 'setTheme' and return the same context for configuration pipelines.
withTheme :: Context -> Theme -> IO Context
withTheme ctx theme = ctx <$ setTheme ctx theme

-- | Change the base theme, invalidate text/layout caches, and request a full
-- repaint. An equal theme is a no-op.
setTheme :: Context -> Theme -> IO ()
setTheme ctx th = do
  cur <- readIORef (ctxTheme ctx)
  when (cur /= th) $ do
    writeIORef (ctxTheme ctx) th
    invalidateTextCaches ctx
    damageFull ctx
    markDirty ctx

-- | Base session theme. Use 'currentTheme' to include the current paint scope.
getTheme :: Context -> IO Theme
getTheme ctx = readIORef (ctxTheme ctx)

-- | Install clipboard read/write callbacks. 'Nothing' means no text is
-- available; a write returns 'False' when refused or unsupported.
withClipboard :: Context -> IO (Maybe Text) -> (Text -> IO Bool) -> Context
withClipboard ctx getter setter = ctx {ctxClipboardGet = getter, ctxClipboardSet = setter}

-- | Enable memoised text measurement, or return an already cached context.
-- Use the returned context for subsequent frames.
enableMeasureCache :: Context -> IO Context
enableMeasureCache ctx =
  case ctxMeasureCache ctx of
    Just _ -> pure ctx
    Nothing -> do
      ref <- newIORef emptyMeasureCache
      pure ctx {ctxMeasureCache = Just ref, ctxMeasureText = cacheMeasureText ref 0 (ctxMeasureText ctx)}

-- | Store one host value per runtime type. Replaces only the value of that
-- type; other host entries remain available. Does not wake the loop.
{-# INLINE setHost #-}
setHost :: forall a. (Typeable a) => Context -> a -> IO ()
setHost ctx val = modifyIORef' (ctxHost ctx) (Map.insert (typeOf val) (toDyn val))

-- | Set the device pixel scale used to snap geometry origins/endpoints to
-- whole pixels. The SDL backend calls this when the window pixel density is synced.
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

-- | Retrieve the host value of the requested type, or 'Nothing' if absent.
{-# INLINE askHostIO #-}
askHostIO :: forall a. (Typeable a) => Context -> IO (Maybe a)
askHostIO ctx = (fromDynamic <=< Map.lookup (typeRep (Proxy :: Proxy a))) <$> readIORef (ctxHost ctx)

-- | The host value of the requested type, storing the one @new@ builds when
-- there is none yet: how a widget keeps a cache of its own on the context.
hostOrInit :: forall a. (Typeable a) => Context -> IO a -> IO a
hostOrInit ctx new = askHostIO ctx >>= maybe (new >>= \v -> v <$ setHost ctx v) pure

-- | Queue a message for this frame. 'drainMessages' restores emission order.
{-# INLINE pushMessage #-}
pushMessage :: Context -> FrameMsg -> IO ()
pushMessage ctx msg = modifyIORef' (ctxMessages ctx) (msg :)

-- | Read queued messages in emission order and clear the queue.
{-# INLINE drainMessages #-}
drainMessages :: Context -> IO [FrameMsg]
drainMessages ctx = do
  msgs <- readIORef (ctxMessages ctx)
  writeIORef (ctxMessages ctx) []
  pure (reverse msgs)

-- =============================================================================
-- Constructors
-- =============================================================================

-- | Fresh headless context with default theme, 12-unit monospace metrics,
-- empty state, and no native clipboard or wake callbacks. Backends configure
-- it before the first frame; each independent session needs its own context.
newContext :: IO Context
newContext = do
  nodeArena <- newNodeArena
  drawArena <- newDrawArena
  ctxHotId <- newIORef (WidgetId 0)
  ctxLastHotId <- newIORef (WidgetId 0)
  ctxActiveId <- newIORef (WidgetId 0)
  ctxClickedId <- newIORef (WidgetId 0)
  ctxReleaseClickedId <- newIORef (WidgetId 0)
  ctxPressPos <- newIORef Nothing
  ctxRightPressPos <- newIORef Nothing
  ctxFocusId <- newIORef (WidgetId 0)
  ctxFocusVisible <- newIORef False
  ctxStore <- newIORef emptyWidgetStore
  ctxDamageState <- newIORef initialDamageState
  ctxOverlayState <- newIORef initialOverlayState
  ctxAnimationState <- newIORef initialAnimationState
  ctxScrollState <- newIORef initialScrollState
  ctxDrawingCache <- newIORef initialDrawingCacheState
  ctxIdContext <- newIORef initialIdContext
  ctxContainerStack <- newIORef []
  ctxMessages <- newIORef []
  let initCap = 64
  ctxFocusables <- newIORef =<< newPrimArray initCap
  ctxFocusablesCount <- newIORef 0
  ctxSpanBase <- newSpanArena 64
  ctxSpanOverlay <- newSpanArena 64
  ctxInteractionState <- newIORef initialInteractionState
  ctxCursorZones <- newIORef []
  ctxImageAtlas <- Atlas.newImageAtlas
  ctxWakeLoop <- newIORef Nothing
  ctxWakeAt <- newIORef 0
  ctxHost <- newIORef Map.empty
  ctxTheme <- newIORef defaultTheme
  ctxThemeScopes <- newIORef =<< newThemeScopes
  ctxSpanCache <- newIORef IM.empty
  ctxWidgetTextCache <- newIORef IM.empty
  ctxDerivedCache <- newIORef IM.empty
  ctxLayoutCache <- newIORef Nothing
  ctxMetricGen <- newIORef 0
  ctxWrapCache <- newIORef emptyWrapCache
  ctxLastMetricSource <- newIORef Nothing
  ctxPaintFull <- newIORef True
  -- References above use their field names; font-dependent defaults stay
  -- explicit, including the resolvers that close over this context.
  let fm0 = monospaceMetrics 12
      ctx = Context
        { ctxNodeArena = nodeArena
        , ctxDrawArena = drawArena
        , ctxFontMetrics = fm0
        , ctxMonoFontMetrics = fm0
        , ctxMeasureText = measureTextIO fm0
        , ctxResolveFont = defaultResolveFont ctx
        , ctxResolveMeasure = defaultResolveMeasure ctx
        , ctxMeasureCache = Nothing
        , ctxMetricSource = InitialMetricSource
        , ctxClipboardGet = pure Nothing
        , ctxClipboardSet = \_ -> pure False
        , ..
        }
  pure ctx

-- | Headless context with 16-unit monospace metrics and cached measurement.
-- Used as a starting point by pixel-based hosts.
newPixelHostContext :: IO Context
newPixelHostContext = do
  ctx <- enableMeasureCache =<< newContext
  pure (withFontMetrics ctx (monospaceMetrics 16))

-- =============================================================================
-- Focus
-- =============================================================================

-- | Keyboard-focused widget, or @WidgetId 0@ when none has focus.
{-# INLINE getFocusId #-}
getFocusId :: Context -> IO WidgetId
getFocusId ctx = readIORef (ctxFocusId ctx)

-- | Whether the focused widget shows its focus ring: focus moved by keyboard
-- since the last pointer press.
{-# INLINE getFocusVisible #-}
getFocusVisible :: Context -> IO Bool
getFocusVisible ctx = readIORef (ctxFocusVisible ctx)

-- | Hovered widget selected by the frame, or @WidgetId 0@ for none.
{-# INLINE getHotId #-}
getHotId :: Context -> IO WidgetId
getHotId ctx = readIORef (ctxHotId ctx)

-- | Add @wid@ to this frame's keyboard focus order, unless it is declared in a
-- disabled scope.
registerFocusable :: Context -> WidgetId -> IO ()
registerFocusable ctx wid = do
  scope <- getArenaScope (ctxNodeArena ctx)
  when (scope .&. 1 == 0) $ do
    idx <- readIORef (ctxFocusablesCount ctx)
    arr <- readIORef (ctxFocusables ctx)
    cap <- getSizeofMutablePrimArray arr
    arr' <-
      if idx >= cap
        then do
          grown <- resizeMutablePrimArray arr (max 16 (cap * 2))
          writeIORef (ctxFocusables ctx) grown
          pure grown
        else pure arr
    writePrimArray arr' idx wid
    writeIORef (ctxFocusablesCount ctx) (idx + 1)

-- | Copy this frame's registered focus ids in declaration order. Modal
-- filtering is applied separately when moving focus.
{-# INLINE getFocusables #-}
getFocusables :: Context -> IO [WidgetId]
getFocusables ctx = do
  count <- readIORef (ctxFocusablesCount ctx)
  arr <- readIORef (ctxFocusables ctx)
  forM [0 .. count - 1] (readPrimArray arr)
