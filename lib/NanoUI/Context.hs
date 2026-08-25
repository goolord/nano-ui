module NanoUI.Context
  ( Context (..)
  , FrameMsg (..)
  , Animation (..)
  , WidgetStore (..)
  , newContext
  , markDirty
  , isDirty
  , anyAnimating
  , startAnimation
  , tickAnimations
  , getPrevRect
  , setPrevRect
  , getStore
  , setStore
  , intKey
  , pushMessage
  , drainMessages
  ) where

import Data.Bits (xor)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.IntMap.Strict (IntMap)
import Data.Word (Word64)
import qualified Data.IntMap.Strict as IM
import NanoUI.Draw (DrawArena, newDrawArena)
import NanoUI.Font (FontMetrics, monospaceMetrics)
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Layout.Arena (NodeArena, newNodeArena)
import NanoUI.Style (Theme, defaultTheme)
import NanoUI.Types (Rect (..))

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
  }
  deriving (Eq, Show)

emptyWidgetStore :: WidgetStore
emptyWidgetStore =
  WidgetStore
    { storeCheckbox = IM.empty
    , storeSlider = IM.empty
    , storeText = IM.empty
    , storeCursor = IM.empty
    }

data Context = Context
  { ctxNodeArena :: NodeArena
  , ctxDrawArena :: DrawArena
  , ctxHotId :: IORef WidgetId
  , ctxActiveId :: IORef WidgetId
  , ctxFocusId :: IORef WidgetId
  , ctxPrevRects :: IORef (IntMap Rect)
  , ctxStore :: IORef WidgetStore
  , ctxAnimations :: IORef (IntMap Animation)
  , ctxAnyAnimating :: IORef Bool
  , ctxDirty :: IORef Bool
  , ctxIdSalt :: IORef Word64
  , ctxFontMetrics :: FontMetrics
  , ctxTheme :: Theme
  , ctxContainerStack :: IORef [Int]
  , ctxMessages :: IORef [FrameMsg]
  }

{-# INLINE newContext #-}
newContext :: IO Context
newContext = do
  nodeArena <- newNodeArena
  drawArena <- newDrawArena
  ctxHotId <- newIORef (WidgetId 0)
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
  pure
    Context
      { ctxNodeArena = nodeArena
      , ctxDrawArena = drawArena
      , ctxHotId
      , ctxActiveId
      , ctxFocusId
      , ctxPrevRects
      , ctxStore
      , ctxAnimations
      , ctxAnyAnimating
      , ctxDirty
      , ctxIdSalt
      , ctxFontMetrics = monospaceMetrics 12
      , ctxTheme = defaultTheme
      , ctxContainerStack
      , ctxMessages
      }

{-# INLINE markDirty #-}
markDirty :: Context -> IO ()
markDirty ctx = writeIORef (ctxDirty ctx) True

{-# INLINE isDirty #-}
isDirty :: Context -> IO Bool
isDirty ctx = readIORef (ctxDirty ctx)

{-# INLINE anyAnimating #-}
anyAnimating :: Context -> IO Bool
anyAnimating ctx = readIORef (ctxAnyAnimating ctx)

{-# INLINE startAnimation #-}
startAnimation :: Context -> WidgetId -> Float -> Float -> Float -> IO ()
startAnimation ctx wid start end dur = do
  let key = intKey wid
  anims <- readIORef (ctxAnimations ctx)
  writeIORef (ctxAnimations ctx) (IM.insert key (Animation start end dur 0) anims)
  writeIORef (ctxAnyAnimating ctx) True

{-# INLINE tickAnimations #-}
tickAnimations :: Context -> Float -> IO ()
tickAnimations ctx dt = do
  anims <- readIORef (ctxAnimations ctx)
  if IM.null anims
    then writeIORef (ctxAnyAnimating ctx) False
    else do
      let updated = IM.map (\a -> a {animElapsed = animElapsed a + dt}) anims
          finished = IM.filter (\a -> animElapsed a >= animDuration a) updated
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
setStore ctx store = writeIORef (ctxStore ctx) store

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

{-# INLINE mix64 #-}
mix64 :: Word64 -> Word64 -> Word64
mix64 h k = (h `xor` k) * 1099511628211
