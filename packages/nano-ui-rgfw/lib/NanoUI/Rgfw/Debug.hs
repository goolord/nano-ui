{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Rgfw.Debug
  ( RgfwDebugSnapshot (..)
  , RgfwDebugSampler
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
  , dbgPresentFps
  , dbgLoopFps
  , dbgFrameMs
  , dbgUiMs
  , dbgRenderMs
  , dbgPresents
  , dbgWinW
  , dbgWinH
  , dbgMouseX
  , dbgMouseY
  , dbgRtsOn
  , dbgGcs
  , dbgMajorGcs
  , dbgAllocMb
  , dbgLiveMb
  , dbgMaxMemMb
  , dbgCopiedMb
  , dbgGcPct
  , dbgLastGcGen
  , dbgLastGcMs
  , dbgCaps
  , dbgCpus
  ) where

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
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
  ( CoreDebugSnapshot
  , DebugSampler (..)
  , debugRefreshSec
  , emptyCoreDebugSnapshot
  , formatCoreRtsRows
  , makeCoreDebugSnapshot
  , newDebugSampler
  , noteDebugLoop
  , noteDebugPresent
  , readRtsSnapshot
  )
import qualified NanoUI.Debug as D

data RgfwDebugSnapshot = RgfwDebugSnapshot
  { dbgCore      :: !CoreDebugSnapshot
  , dbgBlitMs    :: !Double
  , dbgNodes     :: !Int
  , dbgContentW  :: !Float
  , dbgContentH  :: !Float
  , dbgPhysW     :: !Int
  , dbgPhysH     :: !Int
  , dbgScale     :: !Float
  , dbgMonScale  :: !Float
  , dbgScaleMode :: !Text
  }
  deriving (Eq, Show)

dbgPresentFps :: RgfwDebugSnapshot -> Double
dbgPresentFps = D.dbgPresentFps . dbgCore

dbgLoopFps :: RgfwDebugSnapshot -> Double
dbgLoopFps = D.dbgLoopFps . dbgCore

dbgFrameMs :: RgfwDebugSnapshot -> Double
dbgFrameMs = D.dbgFrameMs . dbgCore

dbgUiMs :: RgfwDebugSnapshot -> Double
dbgUiMs = D.dbgUiMs . dbgCore

dbgRenderMs :: RgfwDebugSnapshot -> Double
dbgRenderMs = D.dbgRenderMs . dbgCore

dbgPresents :: RgfwDebugSnapshot -> Word64
dbgPresents = D.dbgPresents . dbgCore

dbgWinW :: RgfwDebugSnapshot -> Float
dbgWinW = D.dbgWinW . dbgCore

dbgWinH :: RgfwDebugSnapshot -> Float
dbgWinH = D.dbgWinH . dbgCore

dbgMouseX :: RgfwDebugSnapshot -> Float
dbgMouseX = D.dbgMouseX . dbgCore

dbgMouseY :: RgfwDebugSnapshot -> Float
dbgMouseY = D.dbgMouseY . dbgCore

dbgRtsOn :: RgfwDebugSnapshot -> Bool
dbgRtsOn = D.dbgRtsOn . dbgCore

dbgGcs :: RgfwDebugSnapshot -> Word32
dbgGcs = D.dbgGcs . dbgCore

dbgMajorGcs :: RgfwDebugSnapshot -> Word32
dbgMajorGcs = D.dbgMajorGcs . dbgCore

dbgAllocMb :: RgfwDebugSnapshot -> Double
dbgAllocMb = D.dbgAllocMb . dbgCore

dbgLiveMb :: RgfwDebugSnapshot -> Double
dbgLiveMb = D.dbgLiveMb . dbgCore

dbgMaxMemMb :: RgfwDebugSnapshot -> Double
dbgMaxMemMb = D.dbgMaxMemMb . dbgCore

dbgCopiedMb :: RgfwDebugSnapshot -> Double
dbgCopiedMb = D.dbgCopiedMb . dbgCore

dbgGcPct :: RgfwDebugSnapshot -> Double
dbgGcPct = D.dbgGcPct . dbgCore

dbgLastGcGen :: RgfwDebugSnapshot -> Word32
dbgLastGcGen = D.dbgLastGcGen . dbgCore

dbgLastGcMs :: RgfwDebugSnapshot -> Double
dbgLastGcMs = D.dbgLastGcMs . dbgCore

dbgCaps :: RgfwDebugSnapshot -> Int
dbgCaps = D.dbgCaps . dbgCore

dbgCpus :: RgfwDebugSnapshot -> Int
dbgCpus = D.dbgCpus . dbgCore

data RgfwDebugSamplerState = RgfwDebugSamplerState
  { smSampler    :: !(IORef DebugSampler)
  , smSnapshot   :: !(IORef RgfwDebugSnapshot)
  , smLastLoopT  :: !Double
  , smBlitMs     :: !Double
  , smNodes      :: !Int
  , smContentW   :: !Float
  , smContentH   :: !Float
  , smPhysW      :: !Int
  , smPhysH      :: !Int
  , smScale      :: !Float
  , smMonScale   :: !Float
  }

type RgfwDebugSampler = IORef RgfwDebugSamplerState

newtype RgfwDebugHost = RgfwDebugHost {rgfwDebugSampler :: RgfwDebugSampler}

type SamplerRef = RgfwDebugSampler

newRgfwDebugSampler :: IO SamplerRef
newRgfwDebugSampler = do
  now <- getMonotonicTime
  sRef <- newDebugSampler
  snapRef <- newIORef emptyRgfwDebug
  newIORef
    RgfwDebugSamplerState
      { smSampler    = sRef
      , smSnapshot   = snapRef
      , smLastLoopT  = now
      , smBlitMs     = 0
      , smNodes      = 0
      , smContentW   = 0
      , smContentH   = 0
      , smPhysW      = 0
      , smPhysH      = 0
      , smScale      = 1
      , smMonScale   = 1
      }

emptyRgfwDebug :: RgfwDebugSnapshot
emptyRgfwDebug =
  RgfwDebugSnapshot
    { dbgCore      = emptyCoreDebugSnapshot
    , dbgBlitMs    = 0
    , dbgNodes     = 0
    , dbgContentW  = 0
    , dbgContentH  = 0
    , dbgPhysW     = 0
    , dbgPhysH     = 0
    , dbgScale     = 1
    , dbgMonScale  = 1
    , dbgScaleMode = "None"
    }

noteLoop :: SamplerRef -> IO ()
noteLoop ref = do
  now <- getMonotonicTime
  s <- readIORef ref
  let dt = realToFrac (now - smLastLoopT s) :: Float
  writeIORef ref (s {smLastLoopT = now})
  noteDebugLoop (smSampler s) dt

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
  s <- readIORef ref
  noteDebugPresent (smSampler s) uiMs renderMs 0 frameMs 0 0 0
  atomicModifyIORef' ref $ \st ->
    ( st
        { smBlitMs   = blitMs
        , smNodes    = nodes
        , smContentW = contentW
        , smContentH = contentH
        , smPhysW    = physW
        , smPhysH    = physH
        , smScale    = scale
        , smMonScale = monScale
        }
    , ()
    )

readRgfwDebug :: SamplerRef -> Size -> V2 -> IO RgfwDebugSnapshot
readRgfwDebug ref (Size lw lh) (V2 mx my) = do
  now <- getMonotonicTime
  st <- readIORef ref
  (refresh, cur) <-
    atomicModifyIORef' (smSampler st) $ \s ->
      let elapsed = now - smLastDebugT s
          refresh = smLastDebugT s <= 0 || elapsed >= debugRefreshSec
       in (s, (refresh, s))
  if not refresh
    then readIORef (smSnapshot st)
    else do
      rts <- readRtsSnapshot
      let curScale = smScale st
          scaleMode
            | curScale <= 1.0 = "1x (Direct 1:1)"
            | abs (curScale - 2.0) < 0.01 = "2x (Scale2x algorithm)"
            | abs (curScale - fromIntegral (round curScale :: Int)) < 0.01 =
                T.pack (printf "%.0fx (Integer pixel scale)" curScale)
            | otherwise =
                T.pack (printf "%.2fx (Fractional bilinear)" curScale)
          core = makeCoreDebugSnapshot cur lw lh mx my rts
          snap' =
            RgfwDebugSnapshot
              { dbgCore      = core
              , dbgBlitMs    = smBlitMs st
              , dbgNodes     = smNodes st
              , dbgContentW  = smContentW st
              , dbgContentH  = smContentH st
              , dbgPhysW     = smPhysW st
              , dbgPhysH     = smPhysH st
              , dbgScale     = curScale
              , dbgMonScale  = smMonScale st
              , dbgScaleMode = scaleMode
              }
      atomicModifyIORef' (smSampler st) $ \s ->
        (s {smLastDebugT = now}, ())
      writeIORef (smSnapshot st) snap'
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
rtsRows = formatCoreRtsRows . dbgCore

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
