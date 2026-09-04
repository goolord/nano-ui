{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Rgfw.Debug
  ( RgfwDebugSnapshot (..)
  , RgfwDebugSampler (..)
  , RgfwDebugHost (..)
  , newRgfwDebugSampler
  , noteLoop
  , notePresent
  , readRgfwDebug
  , emptyRgfwDebug
  , askRgfwDebug
  , debugWindowBody
  , allDebugRows
  , frameRows
  , layoutRows
  , displayRows
  , rtsRows
  ) where

import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32, Word64)
import Effectful (Eff, (:>))
import GHC.Clock (getMonotonicTime)
import Text.Printf (printf)

import NanoUI
  ( Size (..)
  , Ui
  , V2 (..)
  , heading
  , inputMousePos
  , inputWindowSize
  , kvBlock
  , sep
  , uiIO
  )
import NanoUI.Monad
  ( askHost
  , askInput
  )
import NanoUI.Debug
  ( RtsStatsSnapshot (..)
  , blend
  , debugRefreshSec
  , formatRtsRows
  , readRtsSnapshot
  )

data RgfwDebugSnapshot = RgfwDebugSnapshot
  { dbgPresentFps :: !Double
  , dbgLoopFps :: !Double
  , dbgFrameMs :: !Double
  , dbgUiMs :: !Double
  , dbgRenderMs :: !Double
  , dbgBlitMs :: !Double
  , dbgPresents :: !Word64
  , dbgNodes :: !Int
  , dbgContentW :: !Float
  , dbgContentH :: !Float
  , dbgWinW :: !Float
  , dbgWinH :: !Float
  , dbgPhysW :: !Int
  , dbgPhysH :: !Int
  , dbgMouseX :: !Float
  , dbgMouseY :: !Float
  , dbgScale :: !Float
  , dbgMonScale :: !Float
  , dbgScaleMode :: !Text
  , dbgRtsOn :: !Bool
  , dbgGcs :: !Word32
  , dbgMajorGcs :: !Word32
  , dbgAllocMb :: !Double
  , dbgLiveMb :: !Double
  , dbgMaxMemMb :: !Double
  , dbgCopiedMb :: !Double
  , dbgGcPct :: !Double
  , dbgLastGcGen :: !Word32
  , dbgLastGcMs :: !Double
  , dbgCaps :: !Int
  , dbgCpus :: !Int
  }
  deriving (Eq, Show)

data RgfwDebugSampler = RgfwDebugSampler
  { smPresentEma :: !Double
  , smLoopEma :: !Double
  , smLastPresentT :: !Double
  , smLastLoopT :: !Double
  , smLastDebugT :: !Double
  , smPresents :: !Word64
  , smUiMs :: !Double
  , smRenderMs :: !Double
  , smBlitMs :: !Double
  , smFrameMs :: !Double
  , smNodes :: !Int
  , smContentW :: !Float
  , smContentH :: !Float
  , smPhysW :: !Int
  , smPhysH :: !Int
  , smScale :: !Float
  , smMonScale :: !Float
  , smSnapshot :: !RgfwDebugSnapshot
  }

newtype RgfwDebugHost = RgfwDebugHost {rgfwDebugSampler :: IORef RgfwDebugSampler}

type SamplerRef = IORef RgfwDebugSampler

newRgfwDebugSampler :: IO SamplerRef
newRgfwDebugSampler = do
  now <- getMonotonicTime
  newIORef
    RgfwDebugSampler
      { smPresentEma = 0
      , smLoopEma = 0
      , smLastPresentT = now
      , smLastLoopT = now
      , smLastDebugT = 0
      , smPresents = 0
      , smUiMs = 0
      , smRenderMs = 0
      , smBlitMs = 0
      , smFrameMs = 0
      , smNodes = 0
      , smContentW = 0
      , smContentH = 0
      , smPhysW = 0
      , smPhysH = 0
      , smScale = 1
      , smMonScale = 1
      , smSnapshot = emptyRgfwDebug
      }

emptyRgfwDebug :: RgfwDebugSnapshot
emptyRgfwDebug =
  RgfwDebugSnapshot
    { dbgPresentFps = 0
    , dbgLoopFps = 0
    , dbgFrameMs = 0
    , dbgUiMs = 0
    , dbgRenderMs = 0
    , dbgBlitMs = 0
    , dbgPresents = 0
    , dbgNodes = 0
    , dbgContentW = 0
    , dbgContentH = 0
    , dbgWinW = 0
    , dbgWinH = 0
    , dbgPhysW = 0
    , dbgPhysH = 0
    , dbgMouseX = 0
    , dbgMouseY = 0
    , dbgScale = 1
    , dbgMonScale = 1
    , dbgScaleMode = "None"
    , dbgRtsOn = False
    , dbgGcs = 0
    , dbgMajorGcs = 0
    , dbgAllocMb = 0
    , dbgLiveMb = 0
    , dbgMaxMemMb = 0
    , dbgCopiedMb = 0
    , dbgGcPct = 0
    , dbgLastGcGen = 0
    , dbgLastGcMs = 0
    , dbgCaps = 0
    , dbgCpus = 0
    }

noteLoop :: SamplerRef -> IO ()
noteLoop ref = do
  now <- getMonotonicTime
  atomicModifyIORef' ref $ \s ->
    let dt = now - smLastLoopT s
        fps = if dt > 1e-4 then 1 / dt else 0
     in ( s
            { smLoopEma = blend (smLoopEma s) fps
            , smLastLoopT = now
            }
        , ()
        )

notePresent ::
  SamplerRef ->
  Double ->
  Double ->
  Double ->
  Double ->
  Int ->
  Float ->
  Float ->
  Int ->
  Int ->
  Float ->
  Float ->
  IO ()
notePresent ref uiMs renderMs blitMs frameMs nodes contentW contentH physW physH scale monScale = do
  now <- getMonotonicTime
  atomicModifyIORef' ref $ \s ->
    let dt = now - smLastPresentT s
        fps = if dt > 1e-4 then 1 / dt else 0
     in ( s
            { smPresentEma = blend (smPresentEma s) fps
            , smLastPresentT = now
            , smPresents = smPresents s + 1
            , smUiMs = uiMs
            , smRenderMs = renderMs
            , smBlitMs = blitMs
            , smFrameMs = frameMs
            , smNodes = nodes
            , smContentW = contentW
            , smContentH = contentH
            , smPhysW = physW
            , smPhysH = physH
            , smScale = scale
            , smMonScale = monScale
            }
        , ()
        )

readRgfwDebug :: SamplerRef -> Size -> V2 -> IO RgfwDebugSnapshot
readRgfwDebug ref (Size lw lh) (V2 mx my) = do
  now <- getMonotonicTime
  (refresh, cur) <-
    atomicModifyIORef' ref $ \s ->
      let elapsed = now - smLastDebugT s
          refresh = smLastDebugT s <= 0 || elapsed >= debugRefreshSec
       in (s, (refresh, s))
  if not refresh
    then pure (smSnapshot cur)
    else do
      rts <- readRtsSnapshot
      let curScale = smScale cur
          scaleMode
            | curScale <= 1.0 = "1x (Direct 1:1)"
            | abs (curScale - 2.0) < 0.01 = "2x (Scale2x algorithm)"
            | abs (curScale - fromIntegral (round curScale :: Int)) < 0.01 =
                T.pack (printf "%.0fx (Integer pixel scale)" curScale)
            | otherwise =
                T.pack (printf "%.2fx (Fractional bilinear)" curScale)
          snap' =
            RgfwDebugSnapshot
              { dbgPresentFps = smPresentEma cur
              , dbgLoopFps = smLoopEma cur
              , dbgFrameMs = smFrameMs cur
              , dbgUiMs = smUiMs cur
              , dbgRenderMs = smRenderMs cur
              , dbgBlitMs = smBlitMs cur
              , dbgPresents = smPresents cur
              , dbgNodes = smNodes cur
              , dbgContentW = smContentW cur
              , dbgContentH = smContentH cur
              , dbgWinW = lw
              , dbgWinH = lh
              , dbgPhysW = smPhysW cur
              , dbgPhysH = smPhysH cur
              , dbgMouseX = mx
              , dbgMouseY = my
              , dbgScale = curScale
              , dbgMonScale = smMonScale cur
              , dbgScaleMode = scaleMode
              , dbgRtsOn = rtsEnabled rts
              , dbgGcs = rtsGcs rts
              , dbgMajorGcs = rtsMajorGcs rts
              , dbgAllocMb = rtsAllocMb rts
              , dbgLiveMb = rtsLiveMb rts
              , dbgMaxMemMb = rtsMaxMemMb rts
              , dbgCopiedMb = rtsCopiedMb rts
              , dbgGcPct = rtsGcPct rts
              , dbgLastGcGen = rtsLastGcGen rts
              , dbgLastGcMs = rtsLastGcMs rts
              , dbgCaps = rtsCaps rts
              , dbgCpus = rtsCpus rts
              }
      atomicModifyIORef' ref $ \s ->
        (s {smLastDebugT = now, smSnapshot = snap'}, ())
      pure snap'

askRgfwDebug :: Ui :> es => Eff es RgfwDebugSnapshot
askRgfwDebug = do
  inp <- askInput
  mhost <- askHost @RgfwDebugHost
  case mhost of
    Nothing -> pure emptyRgfwDebug
    Just (RgfwDebugHost ref) ->
      uiIO (readRgfwDebug ref (inputWindowSize inp) (inputMousePos inp))

frameRows :: RgfwDebugSnapshot -> [(Text, Text)]
frameRows s =
  let totalHaskellMs = dbgUiMs s + dbgRenderMs s
   in [ ("present", T.pack (printf "%.1f fps" (dbgPresentFps s)))
      , ("loop", T.pack (printf "%.1f fps" (dbgLoopFps s)))
      , ("frame cpu", T.pack (printf "%.2f ms" (dbgFrameMs s)))
      , ("haskell", T.pack (printf "%.2f ms" totalHaskellMs))
      , ("  ui+layout", T.pack (printf "%.2f ms" (dbgUiMs s)))
      , ("  render", T.pack (printf "%.2f ms" (dbgRenderMs s)))
      , ("blit surface", T.pack (printf "%.2f ms" (dbgBlitMs s)))
      , ("frames", T.pack (printf "%d" (dbgPresents s)))
      ]

layoutRows :: RgfwDebugSnapshot -> [(Text, Text)]
layoutRows s =
  [ ("nodes", T.pack (printf "%d" (dbgNodes s)))
  , ("content", T.pack (printf "%.0fx%.0f" (dbgContentW s) (dbgContentH s)))
  ]

displayRows :: RgfwDebugSnapshot -> [(Text, Text)]
displayRows s =
  [ ("logical win", T.pack (printf "%.0fx%.0f" (dbgWinW s) (dbgWinH s)))
  , ("physical win", T.pack (printf "%dx%d" (dbgPhysW s) (dbgPhysH s)))
  , ("scale active", T.pack (printf "%.2fx" (dbgScale s)))
  , ("scale monitor", T.pack (printf "%.2fx" (dbgMonScale s)))
  , ("scale mode", dbgScaleMode s)
  , ("mouse pos", T.pack (printf "%.0f, %.0f" (dbgMouseX s) (dbgMouseY s)))
  ]

rtsRows :: RgfwDebugSnapshot -> [(Text, Text)]
rtsRows s =
  formatRtsRows
    RtsStatsSnapshot
      { rtsEnabled = dbgRtsOn s
      , rtsGcs = dbgGcs s
      , rtsMajorGcs = dbgMajorGcs s
      , rtsAllocMb = dbgAllocMb s
      , rtsLiveMb = dbgLiveMb s
      , rtsMaxMemMb = dbgMaxMemMb s
      , rtsCopiedMb = dbgCopiedMb s
      , rtsGcPct = dbgGcPct s
      , rtsLastGcGen = dbgLastGcGen s
      , rtsLastGcMs = dbgLastGcMs s
      , rtsCaps = dbgCaps s
      , rtsCpus = dbgCpus s
      }

allDebugRows :: RgfwDebugSnapshot -> [(Text, Text)]
allDebugRows s = frameRows s ++ layoutRows s ++ displayRows s ++ rtsRows s

debugWindowBody :: Ui :> es => RgfwDebugSnapshot -> Eff es ()
debugWindowBody snap = do
  heading "Frame"
  kvBlock (frameRows snap)
  sep
  heading "Layout & Arena"
  kvBlock (layoutRows snap)
  sep
  heading "Display & Scale"
  kvBlock (displayRows snap)
  sep
  heading "RTS Runtime"
  kvBlock (rtsRows snap)
