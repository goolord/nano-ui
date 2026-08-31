{-# LANGUAGE StrictData #-}

module NanoUI.Context.Internal
  ( Context (..)
  , MeasureCacheKey
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
  )
where

import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.HashMap.Strict (HashMap)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.IntMap.Strict (IntMap)
import Data.Map.Strict (Map)
import Data.Primitive.PrimArray (MutablePrimArray)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Typeable (Typeable, TypeRep, typeOf, typeRep)
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr)
import GHC.Exts (RealWorld)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as Map
import qualified NanoUI.Atlas as Atlas
import NanoUI.Animation (Animation)
import NanoUI.Atlas (ImageAtlas, atlasTextureId)
import NanoUI.Icons (Icons)
import NanoUI.Context.Types
  ( PendingTooltip (..)
  , TextInputDrag (..)
  , TextInputMenu (..)
  , WindowResizeDrag (..)
  )
import NanoUI.Draw (DrawArena)
import NanoUI.Font (FontMetrics, hasMonoFontMarker, measureText, stripWidgetMarkers)
import NanoUI.Host (HostProfile (..))
import NanoUI.Icons (IconSet, iconsFor)
import NanoUI.Id (IdContext, WidgetId (..), hashWidgetId)
import NanoUI.Frame.SpanArena (SpanArena)
import NanoUI.Layout.Arena (NodeArena, NodeType)
import NanoUI.Messages (FrameMsg)
import NanoUI.Store
  ( WidgetStore (..)
  , boolInt
  , intBool
  , slotDisabled
  , slotKey
  )
import NanoUI.Style (Theme)
import NanoUI.Types (Damage (..), ImageId (..), Rect (..), Size (..), V2)

type MeasureCacheKey = (Text, Bool, Float)

data Context = Context
  { ctxNodeArena :: NodeArena
  , ctxDrawArena :: DrawArena
  , ctxHotId :: IORef WidgetId
  , ctxLastHotId :: IORef WidgetId
  , ctxActiveId :: IORef WidgetId
  -- ^ Widget that released over a solved rect but missed in-UI prev-rect hits.
  -- Consumed on the next frame's first UI pass, then cleared.
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
  -- ^ Widget keys in paint order. Later is on top.
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
intKey wid = fromIntegral (hashWidgetId wid)

{-# INLINE markDirty #-}
markDirty :: Context -> IO ()
markDirty ctx = do
  writeIORef (ctxDirty ctx) True
  mWake <- readIORef (ctxWakeLoop ctx)
  case mWake of
    Just wake -> wake
    Nothing -> pure ()

modifyIORefList :: IORef [a] -> ([a] -> [a]) -> IO ()
modifyIORefList ref f = readIORef ref >>= writeIORef ref . f

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

{-# INLINE clearTooltips #-}
clearTooltips :: Context -> IO ()
clearTooltips ctx = writeIORef (ctxTooltips ctx) []

{-# INLINE pushTooltip #-}
pushTooltip :: Context -> WidgetId -> Rect -> Text -> IO ()
pushTooltip ctx wid rect txt =
  modifyIORefList (ctxTooltips ctx) (PendingTooltip wid rect txt :)

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
  store <- getStore ctx
  let key = slotKey slotDisabled (intKey wid)
  pure (intBool (IM.findWithDefault 0 key (storeInt store)))

{-# INLINE setDisabled #-}
setDisabled :: Context -> WidgetId -> Bool -> IO ()
setDisabled ctx wid on = do
  store <- getStore ctx
  let key = slotKey slotDisabled (intKey wid)
  setStore
    ctx
    (store {storeInt = IM.insert key (boolInt on) (storeInt store)})

{-# INLINE getScrollOffset #-}
getScrollOffset :: Context -> WidgetId -> IO Float
getScrollOffset ctx wid = do
  store <- getStore ctx
  pure (IM.findWithDefault 0 (intKey wid) (storeFloat store))

{-# INLINE setScrollOffset #-}
setScrollOffset :: Context -> WidgetId -> Float -> IO ()
setScrollOffset ctx wid off = do
  store <- getStore ctx
  let key = intKey wid
      prev = IM.findWithDefault 0 key (storeFloat store)
  when (prev /= off) $ do
    setStore ctx (store {storeFloat = IM.insert key off (storeFloat store)})
    markDirty ctx

{-# INLINE getPrevRectByKey #-}
getPrevRectByKey :: Context -> Int -> IO (Maybe Rect)
getPrevRectByKey ctx key =
  readIORef (ctxPrevRects ctx) >>= pure . IM.lookup key

{-# INLINE getPrevRect #-}
getPrevRect :: Context -> WidgetId -> IO (Maybe Rect)
getPrevRect ctx wid = getPrevRectByKey ctx (intKey wid)

{-# INLINE setPrevRect #-}
setPrevRect :: Context -> WidgetId -> Rect -> IO ()
setPrevRect ctx wid rect = do
  rects <- readIORef (ctxPrevRects ctx)
  writeIORef (ctxPrevRects ctx) (IM.insert (intKey wid) rect rects)

registerImage :: Context -> ImageId -> Int -> Int -> ByteString -> IO Bool
registerImage ctx iid w h pixels = do
  ok <- Atlas.registerImage (ctxImageAtlas ctx) iid w h pixels
  when ok (markDirty ctx)
  pure ok

registerImages :: Context -> [(ImageId, Int, Int, ByteString)] -> IO Bool
registerImages ctx items = fmap and $ mapM (\(iid, w, h, px) -> registerImage ctx iid w h px) items

lookupImageUv :: Context -> ImageId -> IO (Maybe (Float, Float, Float, Float))
lookupImageUv ctx = Atlas.lookupImageUv (ctxImageAtlas ctx)

atlasSnapshot :: Context -> IO (Maybe (Int, Int, ForeignPtr Word8, Int))
atlasSnapshot ctx = Atlas.atlasSnapshot (ctxImageAtlas ctx)

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
            case HashMap.lookup key cache of
              Just wh -> pure wh
              Nothing -> do
                wh <- measure txt
                writeIORef cacheRef (HashMap.insert key wh cache)
                pure wh
        }

{-# INLINE clearMeasureCache #-}
clearMeasureCache :: Context -> IO ()
clearMeasureCache ctx =
  case ctxMeasureCache ctx of
    Nothing -> pure ()
    Just cacheRef -> writeIORef cacheRef HashMap.empty

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

{-# INLINE enableMeasureCache #-}
enableMeasureCache :: Context -> IO Context
enableMeasureCache ctx = do
  cacheRef <- newIORef HashMap.empty
  pure (ctx {ctxMeasureCache = Just cacheRef})

{-# INLINE withClipboard #-}
withClipboard :: Context -> IO (Maybe Text) -> (Text -> IO Bool) -> Context
withClipboard ctx get set =
  ctx {ctxClipboardGet = get, ctxClipboardSet = set}

{-# INLINE setHost #-}
setHost :: Typeable a => Context -> a -> IO ()
setHost ctx a = modifyIORef' (ctxHost ctx) (Map.insert (typeOf a) (toDyn a))

{-# INLINE askHostIO #-}
askHostIO :: forall a. Typeable a => Context -> IO (Maybe a)
askHostIO ctx = do
  hosts <- readIORef (ctxHost ctx)
  pure (Map.lookup (typeRep (Proxy @a)) hosts >>= fromDynamic)

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
