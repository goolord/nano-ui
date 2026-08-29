{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

module NanoUI.Context
  ( Context (..)
  , FrameMsg (..)
  , Animation (..)
  , WidgetStore (..)
  , newContext
  , withFontMetrics
  , withMonoFontMetrics
  , withMeasureText
  , wrapMeasureCache
  , clearMeasureCache
  , withExternalText
  , withTheme
  , withIcons
  , withHostProfile
  , enableMeasureCache
  , markDirty
  , clearDirty
  , isDirty
  , setWakeLoop
  , takeDamage
  , getHotId
  , getFocusId
  , anyAnimating
  , startAnimation
  , startAnimationEase
  , startAnimationEaseDelay
  , startSpring
  , setAnimationValue
  , tickAnimations
  , easeSameSpec
  , getPrevRect
  , getPrevRectByKey
  , setPrevRect
  , getStore
  , setStore
  , intKey
  , pushMessage
  , drainMessages
  , decodeMessages
  , reduceMessages
  , reduceUpdates
  , registerFocusable
  , getFocusables
  , isDisabled
  , setDisabled
  , getScrollOffset
  , setScrollOffset
  , getAnimationValue
  , applyEase
  , animInProgress
  , approxEq
  , Ease (..)
  , SpringParams (..)
  , presetBouncy
  , presetSmooth
  , presetStiff
  , clearTooltips
  , pushTooltip
  , readTooltips
  , PendingTooltip (..)
  , TextInputMenu (..)
  , TextInputDrag (..)
  , WindowResizeEdge (..)
  , WindowResizeDrag (..)
  , withClipboard
  , textInputEditActive
  , modalActive
  , overlayConsumesQuit
  , markEscapeConsumed
  , pointerBlockedByModal
  , beginModal
  , endModal
  , registerImage
  , registerImages
  , lookupImageUv
  , atlasSnapshot
  , atlasTextureId
  , setHost
  , askHostIO
) where

import Control.Monad (when)
import Data.Dynamic (fromDynamic, toDyn)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable, typeOf, typeRep)
import Data.Text (Text)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as Map
import qualified NanoUI.Atlas as Atlas
import NanoUI.Draw (newDrawArena)
import NanoUI.Types (Damage (..), Rect (..), Size (..))
import NanoUI.Messages (FrameMsg (..), decodeMessages, reduceMessages, reduceUpdates)
import NanoUI.Animation
  ( Animation (..)
  , Ease (..)
  , animInProgress
  , applyEase
  , approxEq
  , easeSameSpec
  )
import NanoUI.Store (WidgetStore (..), emptyWidgetStore)
import NanoUI.Font (FontMetrics, hasMonoFontMarker, measureText, monospaceMetrics, stripWidgetMarkers)
import NanoUI.Host (HostProfile (..))
import NanoUI.Icons (IconSet, asciiIcons, iconsFor)
import NanoUI.Id (WidgetId (..))
import NanoUI.Layout.Arena (newNodeArena)
import NanoUI.Spring
  ( SpringParams (..)
  , presetBouncy
  , presetSmooth
  , presetStiff
  )
import NanoUI.Style (Theme, defaultTheme)
import NanoUI.Context.Internal
  ( Context (..)
  , PendingTooltip (..)
  , TextInputDrag (..)
  , TextInputMenu (..)
  , WindowResizeDrag (..)
  , WindowResizeEdge (..)
  , getPrevRectByKey
  , intKey
  , markDirty
  , modifyIORefList
  )
import NanoUI.Context.Tooltip
  ( clearTooltips
  , pushTooltip
  , readTooltips
  )
import NanoUI.Context.Atlas
  ( atlasSnapshot
  , atlasTextureId
  , lookupImageUv
  , registerImage
  , registerImages
  )
import NanoUI.Context.Animation
  ( anyAnimating
  , getAnimationValue
  , setAnimationValue
  , startAnimation
  , startAnimationEase
  , startAnimationEaseDelay
  , startSpring
  , tickAnimations
  )
import NanoUI.Context.Modal
  ( beginModal
  , endModal
  , markEscapeConsumed
  , modalActive
  , overlayConsumesQuit
  , pointerBlockedByModal
  , textInputEditActive
  )

{-# INLINE newContext #-}
newContext :: IO Context
newContext = do
  nodeArena <- newNodeArena
  drawArena <- newDrawArena
  ctxHotId <- newIORef (WidgetId 0)
  ctxLastHotId <- newIORef (WidgetId 0)
  ctxActiveId <- newIORef (WidgetId 0)
  ctxFocusId <- newIORef (WidgetId 0)
  ctxPrevRects <- newIORef IM.empty
  ctxStore <- newIORef emptyWidgetStore
  ctxAnimations <- newIORef IM.empty
  ctxAnimRest <- newIORef IM.empty
  ctxAnyAnimating <- newIORef False
  ctxAnimSettled <- newIORef False
  ctxDirty <- newIORef True
  ctxDamage <- newIORef DamageFull
  ctxLastWindowSize <- newIORef (Size 0 0)
  ctxIdSalt <- newIORef 0
  ctxContainerStack <- newIORef []
  ctxMessages <- newIORef []
  ctxFocusables <- newIORef []
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
  ctxImageAtlas <- Atlas.newImageAtlas
  ctxWakeLoop <- newIORef Nothing
  ctxHost <- newIORef Map.empty
  let fm0 = monospaceMetrics 12
  pure
    Context
      { ctxNodeArena = nodeArena
      , ctxDrawArena = drawArena
      , ctxHotId
      , ctxLastHotId
      , ctxActiveId
      , ctxFocusId
      , ctxPrevRects
      , ctxStore
      , ctxAnimations
      , ctxAnimRest
      , ctxAnyAnimating
      , ctxAnimSettled
      , ctxDirty
      , ctxDamage
      , ctxLastWindowSize
      , ctxIdSalt
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
      , ctxImageAtlas
      , ctxWakeLoop
      , ctxHost
      , ctxHostProfile = PixelHost
      }

fontMetricsForText :: Context -> Text -> FontMetrics
fontMetricsForText ctx txt =
  if hasMonoFontMarker txt
    then ctxMonoFontMetrics ctx
    else ctxFontMetrics ctx

{-# INLINE withFontMetrics #-}
withFontMetrics :: Context -> FontMetrics -> Context
withFontMetrics ctx fm =
  ctx
    { ctxFontMetrics = fm
    , ctxMeasureText =
        \txt ->
          pure (measureText (ctxHostProfile ctx) (fontMetricsForText ctx {ctxFontMetrics = fm} txt) (stripWidgetMarkers txt))
    }

{-# INLINE withMonoFontMetrics #-}
withMonoFontMetrics :: Context -> FontMetrics -> Context
withMonoFontMetrics ctx monoFm = ctx {ctxMonoFontMetrics = monoFm}

{-# INLINE withMeasureText #-}
withMeasureText :: Context -> (Text -> IO (Float, Float)) -> Context
withMeasureText ctx measure = ctx {ctxMeasureText = measure}

{-# INLINE wrapMeasureCache #-}
wrapMeasureCache :: Float -> Context -> (Text -> IO (Float, Float)) -> Context
wrapMeasureCache scale ctx measure =
  case ctxMeasureCache ctx of
    Nothing -> ctx {ctxMeasureText = measure}
    Just cacheRef ->
      ctx
        { ctxMeasureText = \txt -> do
            let mono = hasMonoFontMarker txt
                key = (stripWidgetMarkers txt, mono, scale)
            cache <- readIORef cacheRef
            case Map.lookup key cache of
              Just wh -> pure wh
              Nothing -> do
                wh <- measure txt
                writeIORef cacheRef (Map.insert key wh cache)
                pure wh
        }

{-# INLINE clearMeasureCache #-}
clearMeasureCache :: Context -> IO ()
clearMeasureCache ctx =
  case ctxMeasureCache ctx of
    Nothing -> pure ()
    Just cacheRef -> writeIORef cacheRef Map.empty

{-# INLINE withExternalText #-}
withExternalText :: Context -> Bool -> Context
withExternalText ctx on = ctx {ctxExternalText = on}

{-# INLINE withTheme #-}
withTheme :: Context -> Theme -> Context
withTheme ctx theme = ctx {ctxTheme = theme}

{-# INLINE withIcons #-}
withIcons :: Context -> IconSet -> Context
withIcons ctx set = ctx {ctxIcons = iconsFor set}

{-# INLINE withHostProfile #-}
withHostProfile :: Context -> HostProfile -> Context
withHostProfile ctx host =
  let ctx' = ctx {ctxHostProfile = host}
   in ctx'
        { ctxMeasureText =
            \txt ->
              pure (measureText host (fontMetricsForText ctx' txt) (stripWidgetMarkers txt))
        }

{-# INLINE setHost #-}
setHost :: Typeable a => Context -> a -> IO ()
setHost ctx a = modifyIORef' (ctxHost ctx) (Map.insert (typeOf a) (toDyn a))

{-# INLINE askHostIO #-}
askHostIO :: forall a. Typeable a => Context -> IO (Maybe a)
askHostIO ctx = do
  hosts <- readIORef (ctxHost ctx)
  pure (Map.lookup (typeRep (Proxy @a)) hosts >>= fromDynamic)

{-# INLINE enableMeasureCache #-}
enableMeasureCache :: Context -> IO Context
enableMeasureCache ctx = do
  cacheRef <- newIORef Map.empty
  pure (ctx {ctxMeasureCache = Just cacheRef})

{-# INLINE clearDirty #-}
clearDirty :: Context -> IO ()
clearDirty ctx = writeIORef (ctxDirty ctx) False

{-# INLINE setWakeLoop #-}
setWakeLoop :: Context -> IO () -> IO ()
setWakeLoop ctx wake = writeIORef (ctxWakeLoop ctx) (Just wake)

{-# INLINE isDirty #-}
isDirty :: Context -> IO Bool
isDirty ctx = readIORef (ctxDirty ctx)

{-# INLINE takeDamage #-}
takeDamage :: Context -> IO Damage
takeDamage ctx = readIORef (ctxDamage ctx)

{-# INLINE getFocusId #-}
getFocusId :: Context -> IO WidgetId
getFocusId ctx = readIORef (ctxFocusId ctx)

withClipboard :: Context -> IO (Maybe String) -> (String -> IO Bool) -> Context
withClipboard ctx get set =
  ctx {ctxClipboardGet = get, ctxClipboardSet = set}

{-# INLINE getHotId #-}
getHotId :: Context -> IO WidgetId
getHotId ctx = readIORef (ctxHotId ctx)

{-# INLINE getPrevRect #-}
getPrevRect :: Context -> WidgetId -> IO (Maybe Rect)
getPrevRect ctx wid = getPrevRectByKey ctx (intKey wid)

{-# INLINE setPrevRect #-}
setPrevRect :: Context -> WidgetId -> Rect -> IO ()
setPrevRect ctx wid rect = do
  rects <- readIORef (ctxPrevRects ctx)
  writeIORef (ctxPrevRects ctx) (IM.insert (intKey wid) rect rects)

{-# INLINE getStore #-}
getStore :: Context -> IO WidgetStore
getStore ctx = readIORef (ctxStore ctx)

{-# INLINE setStore #-}
setStore :: Context -> WidgetStore -> IO ()
setStore ctx store = do
  prev <- readIORef (ctxStore ctx)
  writeIORef (ctxStore ctx) store
  when (prev /= store) (markDirty ctx)

{-# INLINE pushMessage #-}
pushMessage :: Context -> FrameMsg -> IO ()
pushMessage ctx msg = do
  msgs <- readIORef (ctxMessages ctx)
  writeIORef (ctxMessages ctx) (msg : msgs)

{-# INLINE drainMessages #-}
drainMessages :: Context -> IO [FrameMsg]
drainMessages ctx = do
  msgs <- readIORef (ctxMessages ctx)
  writeIORef (ctxMessages ctx) []
  pure (reverse msgs)

{-# INLINE registerFocusable #-}
registerFocusable :: Context -> WidgetId -> IO ()
registerFocusable ctx wid =
  modifyIORefList (ctxFocusables ctx) (wid :)

{-# INLINE getFocusables #-}
getFocusables :: Context -> IO [WidgetId]
getFocusables ctx = reverse <$> readIORef (ctxFocusables ctx)

{-# INLINE isDisabled #-}
isDisabled :: Context -> WidgetId -> IO Bool
isDisabled ctx wid = do
  store <- getStore ctx
  pure (IM.findWithDefault False (intKey wid) (storeDisabled store))

{-# INLINE setDisabled #-}
setDisabled :: Context -> WidgetId -> Bool -> IO ()
setDisabled ctx wid on = do
  store <- getStore ctx
  setStore ctx (store {storeDisabled = IM.insert (intKey wid) on (storeDisabled store)})

{-# INLINE getScrollOffset #-}
getScrollOffset :: Context -> WidgetId -> IO Float
getScrollOffset ctx wid = do
  store <- getStore ctx
  pure (IM.findWithDefault 0 (intKey wid) (storeScroll store))

{-# INLINE setScrollOffset #-}
setScrollOffset :: Context -> WidgetId -> Float -> IO ()
setScrollOffset ctx wid off = do
  store <- getStore ctx
  let key = intKey wid
      prev = IM.findWithDefault 0 key (storeScroll store)
  when (prev /= off) $ do
    setStore ctx (store {storeScroll = IM.insert key off (storeScroll store)})
    markDirty ctx
