module NanoUI.Debug
  ( debugRefreshSec
  , blend
  , bytesMb
  , nsMs
  , rtsFields
  , RtsStatsSnapshot (..)
  , readRtsSnapshot
  , formatRtsRows
  , CoreDebugSnapshot (..)
  , emptyCoreDebugSnapshot
  , DebugSampler (..)
  , DebugSamplerRef
  , newDebugSampler
  , noteDebugLoop
  , noteDebugSkip
  , isDebugActive
  , takeDebugLive
  , noteDebugPresent
  , makeCoreDebugSnapshot
  , formatFpsRows
  , formatDrawRows
  , formatCoreRtsRows
  ) where

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32, Word64)
import GHC.Clock (getMonotonicTime)
import GHC.Conc (getNumCapabilities, getNumProcessors)
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats, getRTSStatsEnabled)
import Text.Printf (printf)

debugRefreshSec :: Double
debugRefreshSec = 0.25

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

data RtsStatsSnapshot = RtsStatsSnapshot
  { rtsEnabled :: !Bool
  , rtsGcs :: !Word32
  , rtsMajorGcs :: !Word32
  , rtsAllocMb :: !Double
  , rtsLiveMb :: !Double
  , rtsMaxMemMb :: !Double
  , rtsCopiedMb :: !Double
  , rtsGcPct :: !Double
  , rtsLastGcGen :: !Word32
  , rtsLastGcMs :: !Double
  , rtsCaps :: !Int
  , rtsCpus :: !Int
  }
  deriving (Eq, Show)

readRtsSnapshot :: IO RtsStatsSnapshot
readRtsSnapshot = do
  caps <- getNumCapabilities
  cpus <- getNumProcessors
  rtsOn <- getRTSStatsEnabled
  if not rtsOn
    then
      pure
        RtsStatsSnapshot
          { rtsEnabled = False
          , rtsGcs = 0
          , rtsMajorGcs = 0
          , rtsAllocMb = 0
          , rtsLiveMb = 0
          , rtsMaxMemMb = 0
          , rtsCopiedMb = 0
          , rtsGcPct = 0
          , rtsLastGcGen = 0
          , rtsLastGcMs = 0
          , rtsCaps = caps
          , rtsCpus = cpus
          }
    else do
      st <- getRTSStats
      let (gcsVal, major, alloc, live, maxMem, copied, gcPct, lastGen, lastMs) = rtsFields st
      pure
        RtsStatsSnapshot
          { rtsEnabled = True
          , rtsGcs = gcsVal
          , rtsMajorGcs = major
          , rtsAllocMb = alloc
          , rtsLiveMb = live
          , rtsMaxMemMb = maxMem
          , rtsCopiedMb = copied
          , rtsGcPct = gcPct
          , rtsLastGcGen = lastGen
          , rtsLastGcMs = lastMs
          , rtsCaps = caps
          , rtsCpus = cpus
          }

formatRtsRows :: RtsStatsSnapshot -> [(Text, Text)]
formatRtsRows s
  | not (rtsEnabled s) =
      [ ("rts", "stats off (need +RTS -T)")
      , ("haskell", T.pack (printf "%2d cap / %2d cpu" (rtsCaps s) (rtsCpus s)))
      ]
  | otherwise =
      [ ("haskell", T.pack (printf "%2d cap / %2d cpu" (rtsCaps s) (rtsCpus s)))
      , ("gc total", T.pack (printf "%10d" (rtsGcs s)))
      , ("gc major", T.pack (printf "%10d" (rtsMajorGcs s)))
      , ("last gen", T.pack (printf "%10d" (rtsLastGcGen s)))
      , ("last gc", T.pack (printf "%7.2f ms" (rtsLastGcMs s)))
      , ("heap live", T.pack (printf "%6.1f MiB" (rtsLiveMb s)))
      , ("heap alloc", T.pack (printf "%6.1f MiB" (rtsAllocMb s)))
      , ("copied", T.pack (printf "%6.1f MiB" (rtsCopiedMb s)))
      , ("rss max", T.pack (printf "%6.1f MiB" (rtsMaxMemMb s)))
      , ("gc time", T.pack (printf "%9.1f%%" (rtsGcPct s)))
      ]

data CoreDebugSnapshot = CoreDebugSnapshot
  { dbgPresentFps :: !Double
  , dbgLoopFps    :: !Double
  , dbgFrameMs    :: !Double
  , dbgUiMs       :: !Double
  , dbgRenderMs   :: !Double
  , dbgPresentMs  :: !Double
  , dbgPresents   :: !Word64
  , dbgSkips      :: !Word64
  , dbgVerts      :: !Int
  , dbgIndices    :: !Int
  , dbgCmds       :: !Int
  , dbgWinW       :: !Float
  , dbgWinH       :: !Float
  , dbgMouseX     :: !Float
  , dbgMouseY     :: !Float
  , dbgRtsOn      :: !Bool
  , dbgGcs        :: !Word32
  , dbgMajorGcs   :: !Word32
  , dbgAllocMb    :: !Double
  , dbgLiveMb     :: !Double
  , dbgMaxMemMb   :: !Double
  , dbgCopiedMb   :: !Double
  , dbgGcPct      :: !Double
  , dbgLastGcGen  :: !Word32
  , dbgLastGcMs   :: !Double
  , dbgCaps       :: !Int
  , dbgCpus       :: !Int
  }
  deriving (Eq, Show)

emptyCoreDebugSnapshot :: CoreDebugSnapshot
emptyCoreDebugSnapshot =
  CoreDebugSnapshot
    { dbgPresentFps = 0
    , dbgLoopFps = 0
    , dbgFrameMs = 0
    , dbgUiMs = 0
    , dbgRenderMs = 0
    , dbgPresentMs = 0
    , dbgPresents = 0
    , dbgSkips = 0
    , dbgVerts = 0
    , dbgIndices = 0
    , dbgCmds = 0
    , dbgWinW = 0
    , dbgWinH = 0
    , dbgMouseX = 0
    , dbgMouseY = 0
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

data DebugSampler = DebugSampler
  { smPresentEma   :: !Double
  , smLoopEma      :: !Double
  , smLastPresentT :: !Double
  , smLastDebugT   :: !Double
  , smLastQueryT   :: !Double
  , smPresents     :: !Word64
  , smSkips        :: !Word64
  , smUiMs         :: !Double
  , smRenderMs     :: !Double
  , smPresentMs    :: !Double
  , smFrameMs      :: !Double
  , smVerts        :: !Int
  , smIndices      :: !Int
  , smCmds         :: !Int
  , smWantFrame    :: !Bool
  }

type DebugSamplerRef = IORef DebugSampler

newDebugSampler :: IO DebugSamplerRef
newDebugSampler = do
  now <- getMonotonicTime
  newIORef
    DebugSampler
      { smPresentEma = 0
      , smLoopEma = 0
      , smLastPresentT = now
      , smLastDebugT = 0
      , smLastQueryT = 0
      , smPresents = 0
      , smSkips = 0
      , smUiMs = 0
      , smRenderMs = 0
      , smPresentMs = 0
      , smFrameMs = 0
      , smVerts = 0
      , smIndices = 0
      , smCmds = 0
      , smWantFrame = False
      }

noteDebugLoop :: DebugSamplerRef -> Float -> IO ()
noteDebugLoop ref dt =
  atomicModifyIORef' ref $ \s ->
    let dtD = realToFrac dt :: Double
        fps = if dtD > 1e-4 && dtD < 0.25 then 1 / dtD else 0
        ema' =
          if fps > 0
            then if smLoopEma s <= 0 then fps else blend (smLoopEma s) fps
            else smLoopEma s
     in (s {smLoopEma = ema'}, ())

noteDebugSkip :: DebugSamplerRef -> IO ()
noteDebugSkip ref =
  atomicModifyIORef' ref $ \s -> (s {smSkips = smSkips s + 1}, ())

isDebugActive :: DebugSamplerRef -> Bool -> IO Bool
isDebugActive ref windowOpen =
  if windowOpen
    then pure True
    else do
      now <- getMonotonicTime
      s <- readIORef ref
      pure (now - smLastQueryT s < 1.0)

takeDebugLive :: DebugSamplerRef -> Bool -> IO Bool
takeDebugLive _ False = pure False
takeDebugLive ref True = do
  now <- getMonotonicTime
  atomicModifyIORef' ref $ \s ->
    let elapsed = now - smLastDebugT s
        due = smLastDebugT s <= 0 || elapsed >= debugRefreshSec
        want = smWantFrame s || due
     in (s {smWantFrame = False}, want)

noteDebugPresent :: DebugSamplerRef -> Double -> Double -> Double -> Double -> Int -> Int -> Int -> IO ()
noteDebugPresent ref uiMs renderMs presentMs frameMs verts indices cmds = do
  now <- getMonotonicTime
  atomicModifyIORef' ref $ \s ->
    let dt = now - smLastPresentT s
        instantFps =
          if dt > 1e-4 && dt < 0.25
            then 1 / dt
            else if frameMs > 0.001
                   then 1000 / frameMs
                   else 0
        ema' =
          if instantFps > 0
            then if smPresentEma s <= 0 then instantFps else blend (smPresentEma s) instantFps
            else smPresentEma s
     in ( s
            { smPresentEma = ema'
            , smLastPresentT = now
            , smPresents = smPresents s + 1
            , smUiMs = uiMs
            , smRenderMs = renderMs
            , smPresentMs = presentMs
            , smFrameMs = frameMs
            , smVerts = verts
            , smIndices = indices
            , smCmds = cmds
            }
        , ()
        )

makeCoreDebugSnapshot :: DebugSampler -> Float -> Float -> Float -> Float -> RtsStatsSnapshot -> CoreDebugSnapshot
makeCoreDebugSnapshot s winW winH mouseX mouseY rts =
  CoreDebugSnapshot
    { dbgPresentFps = smPresentEma s
    , dbgLoopFps = smLoopEma s
    , dbgFrameMs = smFrameMs s
    , dbgUiMs = smUiMs s
    , dbgRenderMs = smRenderMs s
    , dbgPresentMs = smPresentMs s
    , dbgPresents = smPresents s
    , dbgSkips = smSkips s
    , dbgVerts = smVerts s
    , dbgIndices = smIndices s
    , dbgCmds = smCmds s
    , dbgWinW = winW
    , dbgWinH = winH
    , dbgMouseX = mouseX
    , dbgMouseY = mouseY
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

formatFpsRows :: CoreDebugSnapshot -> [(Text, Text)]
formatFpsRows s =
  [ ("fps present", T.pack (printf "%6.1f" (dbgPresentFps s)))
  , ("fps loop", T.pack (printf "%6.1f" (dbgLoopFps s)))
  , ("frame ms", T.pack (printf "%6.2f" (dbgFrameMs s)))
  , ("ui ms", T.pack (printf "%6.2f" (dbgUiMs s)))
  , ("render ms", T.pack (printf "%6.2f" (dbgRenderMs s)))
  , ("present ms", T.pack (printf "%6.2f" (dbgPresentMs s)))
  , ("presents", T.pack (printf "%10d" (dbgPresents s)))
  , ("skips", T.pack (printf "%10d" (dbgSkips s)))
  ]

formatDrawRows :: CoreDebugSnapshot -> [(Text, Text)]
formatDrawRows s =
  [ ("vertices", T.pack (printf "%10d" (dbgVerts s)))
  , ("indices", T.pack (printf "%10d" (dbgIndices s)))
  , ("commands", T.pack (printf "%10d" (dbgCmds s)))
  ]

formatCoreRtsRows :: CoreDebugSnapshot -> [(Text, Text)]
formatCoreRtsRows s
  | not (dbgRtsOn s) =
      [ ("rts", "stats off (need +RTS -T)")
      , ("haskell", T.pack (printf "%2d cap / %2d cpu" (dbgCaps s) (dbgCpus s)))
      ]
  | otherwise =
      [ ("haskell", T.pack (printf "%2d cap / %2d cpu" (dbgCaps s) (dbgCpus s)))
      , ("gc total", T.pack (printf "%10d" (dbgGcs s)))
      , ("gc major", T.pack (printf "%10d" (dbgMajorGcs s)))
      , ("last gen", T.pack (printf "%10d" (dbgLastGcGen s)))
      , ("last gc", T.pack (printf "%7.2f ms" (dbgLastGcMs s)))
      , ("heap live", T.pack (printf "%6.1f MiB" (dbgLiveMb s)))
      , ("heap alloc", T.pack (printf "%6.1f MiB" (dbgAllocMb s)))
      , ("copied", T.pack (printf "%6.1f MiB" (dbgCopiedMb s)))
      , ("rss max", T.pack (printf "%6.1f MiB" (dbgMaxMemMb s)))
      , ("gc time", T.pack (printf "%9.1f%%" (dbgGcPct s)))
      ]

