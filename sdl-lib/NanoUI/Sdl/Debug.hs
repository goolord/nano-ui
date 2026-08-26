module NanoUI.Sdl.Debug
  ( SdlDebugSnapshot (..)
  , SdlDebugSampler
  , newSdlDebugSampler
  , noteLoop
  , notePresent
  , noteSkip
  , takeDebugLive
  , readSdlDebug
  , emptySdlDebug
  ) where

import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Word (Word32, Word64)
import GHC.Clock (getMonotonicTime)
import GHC.Conc (getNumCapabilities, getNumProcessors)
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats, getRTSStatsEnabled)
import NanoUI (DrawData (..), Size (..), V2 (..), drawCommands)

data SdlDebugSnapshot = SdlDebugSnapshot
  { dbgPresentFps :: Double
  , dbgLoopFps :: Double
  , dbgFrameMs :: Double
  , dbgUiMs :: Double
  , dbgPresents :: Word64
  , dbgSkips :: Word64
  , dbgVerts :: Int
  , dbgIndices :: Int
  , dbgCmds :: Int
  , dbgWinW :: Float
  , dbgWinH :: Float
  , dbgMouseX :: Float
  , dbgMouseY :: Float
  , dbgScale :: Float
  , dbgFontPath :: FilePath
  , dbgRenderer :: String
  , dbgRtsOn :: Bool
  , dbgGcs :: Word32
  , dbgMajorGcs :: Word32
  , dbgAllocMb :: Double
  , dbgLiveMb :: Double
  , dbgMaxMemMb :: Double
  , dbgCopiedMb :: Double
  , dbgGcPct :: Double
  , dbgLastGcGen :: Word32
  , dbgLastGcMs :: Double
  , dbgCaps :: Int
  , dbgCpus :: Int
  }
  deriving (Eq, Show)

data SdlDebugSampler = SdlDebugSampler
  { smPresentEma :: Double
  , smLoopEma :: Double
  , smLastPresentT :: Double
  , smPresents :: Word64
  , smSkips :: Word64
  , smUiMs :: Double
  , smFrameMs :: Double
  , smVerts :: Int
  , smIndices :: Int
  , smCmds :: Int
  , smWantFrame :: Bool
  }

type SamplerRef = IORef SdlDebugSampler

newSdlDebugSampler :: IO SamplerRef
newSdlDebugSampler = do
  now <- getMonotonicTime
  newIORef
    SdlDebugSampler
      { smPresentEma = 0
      , smLoopEma = 0
      , smLastPresentT = now
      , smPresents = 0
      , smSkips = 0
      , smUiMs = 0
      , smFrameMs = 0
      , smVerts = 0
      , smIndices = 0
      , smCmds = 0
      , smWantFrame = False
      }

emptySdlDebug :: SdlDebugSnapshot
emptySdlDebug =
  SdlDebugSnapshot
    { dbgPresentFps = 0
    , dbgLoopFps = 0
    , dbgFrameMs = 0
    , dbgUiMs = 0
    , dbgPresents = 0
    , dbgSkips = 0
    , dbgVerts = 0
    , dbgIndices = 0
    , dbgCmds = 0
    , dbgWinW = 0
    , dbgWinH = 0
    , dbgMouseX = 0
    , dbgMouseY = 0
    , dbgScale = 1
    , dbgFontPath = ""
    , dbgRenderer = ""
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

noteLoop :: SamplerRef -> Float -> IO ()
noteLoop ref dt =
  atomicModifyIORef' ref $ \s ->
    let fps = if dt > 1e-4 then 1 / realToFrac dt else 0
     in (s {smLoopEma = blend (smLoopEma s) fps}, ())

noteSkip :: SamplerRef -> IO ()
noteSkip ref =
  atomicModifyIORef' ref $ \s -> (s {smSkips = smSkips s + 1}, ())

takeDebugLive :: SamplerRef -> IO Bool
takeDebugLive ref =
  atomicModifyIORef' ref $ \s -> (s {smWantFrame = False}, smWantFrame s)

notePresent :: SamplerRef -> Double -> DrawData -> IO ()
notePresent ref uiMs dd = do
  now <- getMonotonicTime
  atomicModifyIORef' ref $ \s ->
    let dt = now - smLastPresentT s
        fps = if dt > 1e-4 then 1 / dt else 0
        frameMs = dt * 1000
     in
      ( s
          { smPresentEma = blend (smPresentEma s) fps
          , smLastPresentT = now
          , smPresents = smPresents s + 1
          , smUiMs = uiMs
          , smFrameMs = frameMs
          , smVerts = drawVertexCount dd
          , smIndices = drawIndexCount dd
          , smCmds = length (drawCommands dd)
          }
      , ()
      )

readSdlDebug :: SamplerRef -> Size -> V2 -> FilePath -> Float -> String -> IO SdlDebugSnapshot
readSdlDebug ref (Size ww wh) (V2 mx my) fontPath scale renderer = do
  s <- atomicModifyIORef' ref $ \cur -> (cur {smWantFrame = True}, cur)
  caps <- getNumCapabilities
  cpus <- getNumProcessors
  rtsOn <- getRTSStatsEnabled
  rts <-
    if rtsOn
      then rtsFields <$> getRTSStats
      else pure (0, 0, 0, 0, 0, 0, 0, 0, 0)
  let (gcs, major, alloc, live, maxMem, copied, gcPct, lastGen, lastMs) = rts
  pure
    SdlDebugSnapshot
      { dbgPresentFps = smPresentEma s
      , dbgLoopFps = smLoopEma s
      , dbgFrameMs = smFrameMs s
      , dbgUiMs = smUiMs s
      , dbgPresents = smPresents s
      , dbgSkips = smSkips s
      , dbgVerts = smVerts s
      , dbgIndices = smIndices s
      , dbgCmds = smCmds s
      , dbgWinW = ww
      , dbgWinH = wh
      , dbgMouseX = mx
      , dbgMouseY = my
      , dbgScale = scale
      , dbgFontPath = fontPath
      , dbgRenderer = renderer
      , dbgRtsOn = rtsOn
      , dbgGcs = gcs
      , dbgMajorGcs = major
      , dbgAllocMb = alloc
      , dbgLiveMb = live
      , dbgMaxMemMb = maxMem
      , dbgCopiedMb = copied
      , dbgGcPct = gcPct
      , dbgLastGcGen = lastGen
      , dbgLastGcMs = lastMs
      , dbgCaps = caps
      , dbgCpus = cpus
      }

blend :: Double -> Double -> Double
blend prev sample
  | prev <= 0 = sample
  | otherwise = prev * 0.85 + sample * 0.15

bytesMb :: Word64 -> Double
bytesMb n = fromIntegral n / (1024 * 1024)

nsMs :: Integral a => a -> Double
nsMs n = fromIntegral n / 1.0e6

rtsFields :: RTSStats -> (Word32, Word32, Double, Double, Double, Double, Double, Word32, Double)
rtsFields st =
  let tot = elapsed_ns st
      gcNs = gc_elapsed_ns st
      pct = if tot > 0 then 100 * fromIntegral gcNs / fromIntegral tot else 0
      lastGc = gc st
   in
    ( gcs st
    , major_gcs st
    , bytesMb (allocated_bytes st)
    , bytesMb (gcdetails_live_bytes lastGc)
    , bytesMb (max_mem_in_use_bytes st)
    , bytesMb (copied_bytes st)
    , pct
    , gcdetails_gen lastGc
    , nsMs (gcdetails_elapsed_ns lastGc)
    )
