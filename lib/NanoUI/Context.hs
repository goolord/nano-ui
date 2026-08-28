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
  , setAnimationValue
  , tickAnimations
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
  , lerpColor
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
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (isNothing, mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable, TypeRep, cast, typeOf, typeRep)
import Data.IntMap.Strict (IntMap)
import Data.Text (Text)
import Data.Word (Word64, Word8)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as Map
import Data.ByteString (ByteString)
import Foreign.ForeignPtr (ForeignPtr)
import qualified NanoUI.Atlas as Atlas
import NanoUI.Atlas (ImageAtlas, atlasTextureId)
import NanoUI.Draw (DrawArena, newDrawArena)
import NanoUI.Types (Damage (..), ImageId (..), Rect (..), Size (..), lerpColor)
import NanoUI.Font (FontMetrics, hasMonoFontMarker, measureText, monospaceMetrics, stripWidgetMarkers)
import NanoUI.Host (HostProfile (..))
import NanoUI.Icons (IconSet, Icons, asciiIcons, iconsFor)
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Input (Input (..), Key (KeyEscape))
import NanoUI.Layout.Arena (NodeArena, NodeType, arenaCount, getRect, getWidgetId, newNodeArena)
import NanoUI.Style (Theme, defaultTheme)

data FrameMsg where
  FrameMsg :: Typeable a => a -> FrameMsg

-- Recover same-type messages. Other payloads (widget String tags, other
-- app types) are dropped.
decodeMessages :: Typeable a => [FrameMsg] -> [a]
decodeMessages = mapMaybe (\(FrameMsg x) -> cast x)

-- Elm-style fold: apply update to each decoded message, in emit order.
reduceMessages :: Typeable msg => (msg -> model -> model) -> model -> [FrameMsg] -> model
reduceMessages update model = foldl' (flip update) model . decodeMessages

-- Fold emitted (model -> model) functions. Same frame-end contract as
-- reduceMessages, without a named Msg type.
reduceUpdates :: Typeable model => model -> [FrameMsg] -> model
reduceUpdates = reduceMessages ($)

data Ease
  = EaseLinear
  | EaseInCubic
  | EaseOutCubic
  | EaseInOutCubic
  | EaseOutBack
  deriving (Eq, Show)

data Animation = Animation
  { animStart :: {-# UNPACK #-} !Float
  , animEnd :: {-# UNPACK #-} !Float
  , animDuration :: {-# UNPACK #-} !Float
  , animElapsed :: {-# UNPACK #-} !Float
  , animEase :: !Ease
  , animDelay :: {-# UNPACK #-} !Float
  -- Remaining wait. Counts down. Do not compare this to the call-site delay.
  , animDelayReq :: {-# UNPACK #-} !Float
  -- Requested delay. Used to decide whether a restart should reset the wait.
  }
  deriving (Eq, Show)

-- Map unit progress through an easing curve. Input is clamped to [0, 1].
-- EaseOutBack may return a value outside that range (overshoot).
applyEase :: Ease -> Float -> Float
applyEase ease t0 =
  let t = max 0 (min 1 t0)
   in case ease of
        EaseLinear -> t
        EaseInCubic -> t * t * t
        EaseOutCubic ->
          let u = 1 - t
           in 1 - u * u * u
        EaseInOutCubic
          | t < 0.5 -> 4 * t * t * t
          | otherwise ->
              let u = -2 * t + 2
               in 1 - (u * u * u) / 2
        EaseOutBack ->
          let c1 = 1.70158
              c3 = c1 + 1
              u = t - 1
           in 1 + c3 * u * u * u + c1 * u * u

approxEq :: Float -> Float -> Bool
approxEq a b = abs (a - b) <= 1e-4

{-# INLINE animInProgress #-}
animInProgress :: Animation -> Bool
animInProgress a =
  not (approxEq (animStart a) (animEnd a))
    && animDuration a > 0
    && (animDelay a > 0 || animElapsed a < animDuration a)

{-# INLINE animationValue #-}
animationValue :: Animation -> Float
animationValue a
  | not (animInProgress a) = animEnd a
  | animDelay a > 0 = animStart a
  | otherwise =
      let t = min 1 (animElapsed a / max 0.001 (animDuration a))
       in animStart a + (animEnd a - animStart a) * applyEase (animEase a) t

refreshAnimating :: Context -> IO ()
refreshAnimating ctx = do
  anims <- readIORef (ctxAnimations ctx)
  writeIORef (ctxAnyAnimating ctx) (any animInProgress anims)

data WidgetStore = WidgetStore
  { storeCheckbox :: IntMap Bool
  , storeSlider :: IntMap Float
  , storeText :: IntMap String
  , storeCursor :: IntMap Int
  , storeSelAnchor :: IntMap Int
  , storeScroll :: IntMap Float
  , storeSelect :: IntMap Int
  , storeSelectOpen :: IntMap Bool
  , storeDisabled :: IntMap Bool
  , storeFlag :: IntMap Bool
  , storeNote :: IntMap String
  , storeWindow :: IntMap (Float, Float)
  , storeWindowSize :: IntMap (Float, Float)
  }
  deriving (Eq, Show)

emptyWidgetStore :: WidgetStore
emptyWidgetStore =
  WidgetStore
    { storeCheckbox = IM.empty
    , storeSlider = IM.empty
    , storeText = IM.empty
    , storeCursor = IM.empty
    , storeSelAnchor = IM.empty
    , storeScroll = IM.empty
    , storeSelect = IM.empty
    , storeSelectOpen = IM.empty
    , storeDisabled = IM.empty
    , storeFlag = IM.empty
    , storeNote = IM.empty
    , storeWindow = IM.empty
    , storeWindowSize = IM.empty
    }

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

type MeasureCacheKey = (Text, Bool, Float)

data Context = Context
  { ctxNodeArena :: NodeArena
  , ctxDrawArena :: DrawArena
  , ctxHotId :: IORef WidgetId
  , ctxLastHotId :: IORef WidgetId
  , ctxActiveId :: IORef WidgetId
  , ctxFocusId :: IORef WidgetId
  , ctxPrevRects :: IORef (IntMap Rect)
  , ctxStore :: IORef WidgetStore
  , ctxAnimations :: IORef (IntMap Animation)
  , ctxAnimRest :: IORef (IntMap Float)
  , ctxAnyAnimating :: IORef Bool
  , ctxAnimSettled :: IORef Bool
  , ctxDirty :: IORef Bool
  , ctxDamage :: IORef Damage
  , ctxLastWindowSize :: IORef Size
  , ctxIdSalt :: IORef Word64
  , ctxFontMetrics :: FontMetrics
  , ctxMonoFontMetrics :: FontMetrics
  , ctxMeasureText :: Text -> IO (Float, Float)
  , ctxMeasureCache :: Maybe (IORef (Map.Map MeasureCacheKey (Float, Float)))
  , ctxExternalText :: Bool
  , ctxTheme :: Theme
  , ctxIcons :: Icons
  , ctxContainerStack :: IORef [Int]
  , ctxMessages :: IORef [FrameMsg]
  , ctxFocusables :: IORef [WidgetId]
  , ctxScrollDrag :: IORef (Maybe (WidgetId, Float))
  , ctxTextInputDrag :: IORef (Maybe TextInputDrag)
  , ctxTextInputMenu :: IORef (Maybe TextInputMenu)
  , ctxClipboardGet :: IO (Maybe String)
  , ctxClipboardSet :: String -> IO Bool
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
  , ctxImageAtlas :: ImageAtlas
  , ctxWakeLoop :: IORef (Maybe (IO ()))
  , ctxHost :: IORef (Map.Map TypeRep Dynamic)
  , ctxHostProfile :: HostProfile
  }

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

{-# INLINE markDirty #-}
markDirty :: Context -> IO ()
markDirty ctx = do
  writeIORef (ctxDirty ctx) True
  mWake <- readIORef (ctxWakeLoop ctx)
  case mWake of
    Just wake -> wake
    Nothing -> pure ()

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

textInputEditActive :: Context -> IO Bool
textInputEditActive ctx = do
  focus <- getFocusId ctx
  menu <- readIORef (ctxTextInputMenu ctx)
  pure (hashWidgetId focus /= 0 || menu /= Nothing)

modalActive :: Context -> IO Bool
modalActive ctx = do
  was <- readIORef (ctxModalWasActive ctx)
  now <- readIORef (ctxModalActive ctx)
  pure (was || now)

-- | True after this frame's UI consumed Escape (modal dismiss or text menu).
-- Call after 'runFrame'.
overlayConsumesQuit :: Context -> Input -> IO Bool
overlayConsumesQuit ctx inp = do
  consumed <- readIORef (ctxEscapeConsumed ctx)
  let esc = KeyEscape `elem` inputKeys inp
  pure (esc && consumed)

markEscapeConsumed :: Context -> IO ()
markEscapeConsumed ctx = writeIORef (ctxEscapeConsumed ctx) True

pointerBlockedByModal :: Context -> IO Bool
pointerBlockedByModal ctx = do
  depth <- readIORef (ctxModalDepth ctx)
  if depth > 0
    then pure False
    else modalActive ctx

beginModal :: Context -> IO ()
beginModal ctx = do
  writeIORef (ctxModalActive ctx) True
  depth <- readIORef (ctxModalDepth ctx)
  writeIORef (ctxModalDepth ctx) (depth + 1)

endModal :: Context -> IO ()
endModal ctx = do
  depth <- readIORef (ctxModalDepth ctx)
  writeIORef (ctxModalDepth ctx) (max 0 (depth - 1))

{-# INLINE getHotId #-}
getHotId :: Context -> IO WidgetId
getHotId ctx = readIORef (ctxHotId ctx)

{-# INLINE anyAnimating #-}
anyAnimating :: Context -> IO Bool
anyAnimating ctx = readIORef (ctxAnyAnimating ctx)

{-# INLINE startAnimation #-}
startAnimation :: Context -> WidgetId -> Float -> Float -> Float -> IO ()
startAnimation ctx wid start end dur = startAnimationEase ctx wid start end dur EaseLinear

{-# INLINE startAnimationEase #-}
startAnimationEase :: Context -> WidgetId -> Float -> Float -> Float -> Ease -> IO ()
startAnimationEase ctx wid start end dur ease =
  startAnimationEaseDelay ctx wid start end dur ease 0

{-# INLINE startAnimationEaseDelay #-}
startAnimationEaseDelay :: Context -> WidgetId -> Float -> Float -> Float -> Ease -> Float -> IO ()
startAnimationEaseDelay ctx wid start end dur ease delay = do
  let key = intKey wid
  anims <- readIORef (ctxAnimations ctx)
  if dur <= 0 || approxEq start end
    then settleKey ctx key end
    else do
      let req = max 0 delay
          (elapsed, delayLeft) =
            case IM.lookup key anims of
              Just a
                | approxEq (animStart a) start
                    && approxEq (animEnd a) end
                    && animEase a == ease
                    && approxEq (animDuration a) dur
                    && approxEq req (animDelayReq a) ->
                    (animElapsed a, animDelay a)
              _ -> (0, req)
      rest <- readIORef (ctxAnimRest ctx)
      writeIORef (ctxAnimRest ctx) (IM.delete key rest)
      writeIORef
        (ctxAnimations ctx)
        ( IM.insert
            key
            ( Animation
                { animStart = start
                , animEnd = end
                , animDuration = dur
                , animElapsed = elapsed
                , animEase = ease
                , animDelay = delayLeft
                , animDelayReq = req
                }
            )
            anims
        )
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
      mapM_ (markDirtyIfOrphan ctx) (IM.keys live)

writeRest :: IntMap Float -> Int -> Animation -> IntMap Float
writeRest rest key a
  | approxEq (animEnd a) 0 = IM.delete key rest
  | otherwise = IM.insert key (animEnd a) rest

markDirtyIfOrphan :: Context -> Int -> IO ()
markDirtyIfOrphan ctx key = do
  mprev <- getPrevRectByKey ctx key
  hasNow <- nodeHasKey ctx key
  when (isNothing mprev && not hasNow) (markDirty ctx)

-- Call-site tweens have no node. Hover ids exist in the arena this frame
-- before prev rects are written; those must not force Full.
nodeHasKey :: Context -> Int -> IO Bool
nodeHasKey ctx key = do
  n <- arenaCount (ctxNodeArena ctx)
  let go i
        | i >= n = pure False
        | otherwise = do
            wid <- getWidgetId (ctxNodeArena ctx) i
            if intKey wid == key && hashWidgetId wid /= 0
              then do
                (_, _, w, h) <- getRect (ctxNodeArena ctx) i
                if w > 0 && h > 0 then pure True else go (i + 1)
              else go (i + 1)
  go 0

settleKey :: Context -> Int -> Float -> IO ()
settleKey ctx key val = do
  anims <- readIORef (ctxAnimations ctx)
  rest <- readIORef (ctxAnimRest ctx)
  let prevRest = IM.findWithDefault 0 key rest
      prevLive = fmap animationValue (IM.lookup key anims)
      changed =
        case prevLive of
          Just v -> not (approxEq v val)
          Nothing -> not (approxEq prevRest val)
  writeIORef (ctxAnimations ctx) (IM.delete key anims)
  writeIORef
    (ctxAnimRest ctx)
    ( if approxEq val 0
        then IM.delete key rest
        else IM.insert key val rest
    )
  when changed (markDirty ctx)
  refreshAnimating ctx

stepAnim :: Float -> Animation -> Animation
stepAnim dt a
  | not (animInProgress a) = a
  | animDelay a > 0 =
      let remain = animDelay a - dt
       in if remain > 0
            then a {animDelay = remain}
            else stepAnim (negate remain) (a {animDelay = 0})
  | otherwise =
      let elapsed = animElapsed a + dt
       in if elapsed >= animDuration a
            then a {animStart = animEnd a, animElapsed = 0, animDuration = 0, animDelay = 0, animDelayReq = 0}
            else a {animElapsed = elapsed}

{-# INLINE intKey #-}
intKey :: WidgetId -> Int
intKey wid = fromIntegral (hashWidgetId wid)

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

modifyIORefList :: IORef [a] -> ([a] -> [a]) -> IO ()
modifyIORefList ref f = readIORef ref >>= writeIORef ref . f

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

registerImage :: Context -> ImageId -> Int -> Int -> ByteString -> IO Bool
registerImage ctx iid w h pixels = do
  ok <- Atlas.registerImage (ctxImageAtlas ctx) iid w h pixels
  when ok (markDirty ctx)
  pure ok

registerImages :: Context -> [(ImageId, Int, Int, ByteString)] -> IO Bool
registerImages ctx = fmap and . mapM (\(iid, w, h, px) -> registerImage ctx iid w h px)

lookupImageUv :: Context -> ImageId -> IO (Maybe (Float, Float, Float, Float))
lookupImageUv ctx = Atlas.lookupImageUv (ctxImageAtlas ctx)

atlasSnapshot :: Context -> IO (Maybe (Int, Int, ForeignPtr Word8, Int))
atlasSnapshot ctx = Atlas.atlasSnapshot (ctxImageAtlas ctx)
