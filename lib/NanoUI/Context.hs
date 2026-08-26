module NanoUI.Context
  ( Context (..)
  , FrameMsg (..)
  , Animation (..)
  , WidgetStore (..)
  , newContext
  , withFontMetrics
  , withMeasureText
  , withExternalText
  , withTheme
  , newTerminalContext
  , newSdlContext
  , markDirty
  , isDirty
  , getHotId
  , getFocusId
  , anyAnimating
  , startAnimation
  , setAnimationValue
  , tickAnimations
  , getPrevRect
  , setPrevRect
  , getStore
  , setStore
  , intKey
  , pushMessage
  , drainMessages
  , registerFocusable
  , getFocusables
  , isDisabled
  , setDisabled
  , getScrollOffset
  , setScrollOffset
  , getAnimationValue
  , lerpColor
  , clearTooltips
  , pushTooltip
  , readTooltips
  , PendingTooltip (..)
  , TextInputMenu (..)
  , TextInputDrag (..)
  , withClipboard
  , textInputEditActive
  , modalActive
  , overlayConsumesQuit
  , markEscapeConsumed
  , pointerBlockedByModal
  , beginModal
  , endModal
) where

import Data.Bits (shiftR, shiftL, (.&.), (.|.))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.IntMap.Strict (IntMap)
import Data.Text (Text)
import Data.Word (Word64)
import qualified Data.IntMap.Strict as IM
import NanoUI.Draw (DrawArena, newDrawArena)
import NanoUI.Font (FontMetrics, measureText, monospaceMetrics)
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Input (Input (..), Key (KeyEscape))
import NanoUI.Layout.Arena (NodeArena, NodeType, newNodeArena)
import NanoUI.Style (Theme, defaultTheme, sdlTheme, terminalTheme)
import NanoUI.Types (Rect (..), Color (..))

data FrameMsg where
  FrameMsg :: a -> FrameMsg

data Animation = Animation
  { animStart :: Float
  , animEnd :: Float
  , animDuration :: Float
  , animElapsed :: Float
  }
  deriving (Eq, Show)

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
  , ctxAnyAnimating :: IORef Bool
  , ctxDirty :: IORef Bool
  , ctxIdSalt :: IORef Word64
  , ctxFontMetrics :: FontMetrics
  , ctxMeasureText :: Text -> IO (Float, Float)
  , ctxExternalText :: Bool
  , ctxTheme :: Theme
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
  ctxAnyAnimating <- newIORef False
  ctxDirty <- newIORef True
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
      , ctxAnyAnimating
      , ctxDirty
      , ctxIdSalt
      , ctxFontMetrics = fm0
      , ctxMeasureText = \txt -> pure (measureText fm0 txt)
      , ctxExternalText = False
      , ctxTheme = defaultTheme
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
      }

{-# INLINE withFontMetrics #-}
withFontMetrics :: Context -> FontMetrics -> Context
withFontMetrics ctx fm =
  ctx
    { ctxFontMetrics = fm
    , ctxMeasureText = \txt -> pure (measureText fm txt)
    }

{-# INLINE withMeasureText #-}
withMeasureText :: Context -> (Text -> IO (Float, Float)) -> Context
withMeasureText ctx measure = ctx {ctxMeasureText = measure}

{-# INLINE withExternalText #-}
withExternalText :: Context -> Bool -> Context
withExternalText ctx on = ctx {ctxExternalText = on}

{-# INLINE withTheme #-}
withTheme :: Context -> Theme -> Context
withTheme ctx theme = ctx {ctxTheme = theme}

{-# INLINE newTerminalContext #-}
newTerminalContext :: IO Context
newTerminalContext = do
  ctx <- newContext
  pure (withExternalText (withTheme (withFontMetrics ctx (monospaceMetrics 1)) terminalTheme) True)

{-# INLINE newSdlContext #-}
newSdlContext :: IO Context
newSdlContext = do
  ctx <- newContext
  pure (withExternalText (withTheme (withFontMetrics ctx (monospaceMetrics 16)) sdlTheme) True)

{-# INLINE markDirty #-}
markDirty :: Context -> IO ()
markDirty ctx = writeIORef (ctxDirty ctx) True

{-# INLINE isDirty #-}
isDirty :: Context -> IO Bool
isDirty ctx = readIORef (ctxDirty ctx)

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
startAnimation ctx wid start end dur = do
  let key = intKey wid
  anims <- readIORef (ctxAnimations ctx)
  let elapsed =
        case IM.lookup key anims of
          Just a | animStart a == start && animEnd a == end -> animElapsed a
          _ -> 0
  writeIORef (ctxAnimations ctx) (IM.insert key (Animation start end dur elapsed) anims)
  writeIORef (ctxAnyAnimating ctx) True

{-# INLINE setAnimationValue #-}
setAnimationValue :: Context -> WidgetId -> Float -> IO ()
setAnimationValue ctx wid val = do
  let key = intKey wid
      v = max 0 (min 1 val)
  anims <- readIORef (ctxAnimations ctx)
  writeIORef (ctxAnimations ctx) (IM.insert key (Animation v v 0 0) anims)
  writeIORef (ctxAnyAnimating ctx) False

{-# INLINE tickAnimations #-}
tickAnimations :: Context -> Float -> IO ()
tickAnimations ctx dt = do
  anims <- readIORef (ctxAnimations ctx)
  if IM.null anims
    then writeIORef (ctxAnyAnimating ctx) False
    else do
      let updated = IM.map (\a -> a {animElapsed = animElapsed a + dt}) anims
          finished =
            IM.filter
              (\a -> animStart a /= animEnd a && animElapsed a >= animDuration a)
              updated
          remaining = IM.difference updated finished
      writeIORef (ctxAnimations ctx) remaining
      writeIORef (ctxAnyAnimating ctx) (not (IM.null remaining))
      if not (IM.null finished) then markDirty ctx else pure ()

{-# INLINE intKey #-}
intKey :: WidgetId -> Int
intKey wid = fromIntegral (hashWidgetId wid)

{-# INLINE getPrevRect #-}
getPrevRect :: Context -> WidgetId -> IO (Maybe Rect)
getPrevRect ctx wid =
  readIORef (ctxPrevRects ctx) >>= pure . IM.lookup (intKey wid)

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
  writeIORef (ctxStore ctx) store
  markDirty ctx

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
  setStore ctx (store {storeScroll = IM.insert (intKey wid) off (storeScroll store)})

{-# INLINE getAnimationValue #-}
getAnimationValue :: Context -> WidgetId -> IO Float
getAnimationValue ctx wid = do
  anims <- readIORef (ctxAnimations ctx)
  case IM.lookup (intKey wid) anims of
    Nothing -> pure 0
    Just a ->
      let dur = max 0.001 (animDuration a)
          t = min 1 (animElapsed a / dur)
       in pure (animStart a + (animEnd a - animStart a) * t)

{-# INLINE lerpColor #-}
lerpColor :: Color -> Color -> Float -> Color
lerpColor (Color a) (Color b) t =
  let u = max 0 (min 1 t)
      ch i =
        round $
          fromIntegral ((a `shiftR` i) .&. 0xFF) * (1 - u)
            + fromIntegral ((b `shiftR` i) .&. 0xFF) * u
   in Color
        ( (ch 24 `shiftL` 24)
            .|. (ch 16 `shiftL` 16)
            .|. (ch 8 `shiftL` 8)
            .|. ch 0
        )

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
