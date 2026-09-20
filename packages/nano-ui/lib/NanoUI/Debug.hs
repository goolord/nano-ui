-- | Debug readout sampling shared by the backends: frame timing and skip
-- counts, RTS statistics, draw counts, and the rows the debug windows show.
module NanoUI.Debug
  ( debugRefreshSec
  , blend
  , RtsStatsSnapshot (..)
  , readRtsSnapshot
  , CoreDebugSnapshot (..)
  , emptyCoreDebugSnapshot
  , DebugSampler (..)
  , DebugSamplerRef
  , newDebugSampler
  , noteDebugLoop
  , noteDebugSkip
  , isDebugActive
  , debugRefreshDue
  , noteDebugPresent
  , refreshDebugSnapshot
  , formatFpsRows
  , formatDrawRows
  , formatCoreRtsRows
  ) where

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32, Word64)
import GHC.Clock (getMonotonicTime)
import GHC.Conc (getNumCapabilities, getNumProcessors)
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats, getRTSStatsEnabled)
import Text.Printf (printf)

-- | Minimum interval between published snapshots, in seconds (0.25).
debugRefreshSec :: Double
debugRefreshSec = 0.25

-- | Exponential moving average with 15% weight on the sample. A non-positive
-- previous value starts a new average at the sample.
blend :: Double -> Double -> Double
blend prev sample
  | prev <= 0 = sample
  | otherwise = prev * 0.85 + sample * 0.15

-- | Runtime counters. Memory fields use MiB, GC duration uses milliseconds,
-- and GC percentage is elapsed GC time divided by elapsed runtime time.
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

emptyRtsSnapshot :: RtsStatsSnapshot
emptyRtsSnapshot =
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
    , rtsCaps = 0
    , rtsCpus = 0
    }

-- | Sample RTS statistics when enabled with @+RTS -T@. Otherwise report only
-- capability/processor counts and leave 'rtsEnabled' false.
readRtsSnapshot :: IO RtsStatsSnapshot
readRtsSnapshot = do
  caps <- getNumCapabilities
  cpus <- getNumProcessors
  rtsOn <- getRTSStatsEnabled
  if not rtsOn
    then pure emptyRtsSnapshot {rtsCaps = caps, rtsCpus = cpus}
    else do
      st <- getRTSStats
      let tot = elapsed_ns st
          lastGc = gc st
          bytesMb n = fromIntegral n / (1024 * 1024)
      pure
        RtsStatsSnapshot
          { rtsEnabled = True
          , rtsGcs = gcs st
          , rtsMajorGcs = major_gcs st
          , rtsAllocMb = bytesMb (allocated_bytes st)
          , rtsLiveMb = bytesMb (gcdetails_live_bytes lastGc)
          , rtsMaxMemMb = bytesMb (max_mem_in_use_bytes st)
          , rtsCopiedMb = bytesMb (copied_bytes st)
          , rtsGcPct =
              if tot > 0 then 100 * fromIntegral (gc_elapsed_ns st) / fromIntegral tot else 0
          , rtsLastGcGen = gcdetails_gen lastGc
          , rtsLastGcMs = fromIntegral (gcdetails_elapsed_ns lastGc) / 1.0e6
          , rtsCaps = caps
          , rtsCpus = cpus
          }

-- | Published frame rates, latest phase durations in milliseconds, cumulative
-- present/skip counts, geometry counts, and backend-supplied window coordinates.
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
  , dbgRts        :: !RtsStatsSnapshot
  }
  deriving (Eq, Show)

-- | Zeroed placeholder before a backend publishes a frame sample.
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
    , dbgRts = emptyRtsSnapshot
    }

-- | Mutable sampler contents. Timestamp fields use monotonic seconds; phase
-- durations use milliseconds. Backends update this through the @noteDebug*@ functions.
data DebugSampler = DebugSampler
  { smPresentEma   :: {-# UNPACK #-} !Double
  , smLoopEma      :: {-# UNPACK #-} !Double
  , smLastPresentT :: {-# UNPACK #-} !Double
  , smLastDebugT   :: {-# UNPACK #-} !Double
  , smLastQueryT   :: {-# UNPACK #-} !Double
  , smPresents     :: {-# UNPACK #-} !Word64
  , smSkips        :: {-# UNPACK #-} !Word64
  , smUiMs         :: {-# UNPACK #-} !Double
  , smRenderMs     :: {-# UNPACK #-} !Double
  , smPresentMs    :: {-# UNPACK #-} !Double
  , smFrameMs      :: {-# UNPACK #-} !Double
  , smVerts        :: {-# UNPACK #-} !Int
  , smIndices      :: {-# UNPACK #-} !Int
  , smCmds         :: {-# UNPACK #-} !Int
  , smRatePresents :: {-# UNPACK #-} !Word64
  , smRateT        :: {-# UNPACK #-} !Double
  }

-- | Session-owned sampler reference, updated atomically by sampling operations.
type DebugSamplerRef = IORef DebugSampler

-- | Empty sampler with its rate interval starting at the current monotonic time.
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
      , smRatePresents = 0
      , smRateT = now
      }

-- | Record loop delta time in seconds. Intervals outside 0.0001-0.25 seconds
-- do not contribute to the loop-rate moving average.
noteDebugLoop :: DebugSamplerRef -> Float -> IO ()
noteDebugLoop ref dt =
  atomicModifyIORef' ref $ \s ->
    let dtD = realToFrac dt :: Double
        fps = if dtD > 1e-4 && dtD < 0.25 then 1 / dtD else 0
        ema' =
          if fps > 0
            then blend (smLoopEma s) fps
            else smLoopEma s
     in (s {smLoopEma = ema'}, ())

-- | Increment the count of loop passes that skipped presentation.
noteDebugSkip :: DebugSamplerRef -> IO ()
noteDebugSkip ref =
  atomicModifyIORef' ref $ \s -> (s {smSkips = smSkips s + 1}, ())

-- | Debug HUD cadence is driven by actual snapshot consumption: a snapshot
-- query ('refreshDebugSnapshot') refreshes 'smLastQueryT', so the 4 Hz refresh
-- loop only runs while a stats window is being built. An open window alone
-- does not count as activity, or the event loop would wake every refresh
-- period while any floating window is open.
isDebugActive :: DebugSamplerRef -> IO Bool
isDebugActive ref = do
  now <- getMonotonicTime
  s <- readIORef ref
  pure (now - smLastQueryT s < 1.0)

-- | Whether the published snapshot is older than 'debugRefreshSec'.
debugRefreshDue :: DebugSamplerRef -> IO Bool
debugRefreshDue ref = do
  now <- getMonotonicTime
  s <- readIORef ref
  pure (snapshotDue now s)

snapshotDue :: Double -> DebugSampler -> Bool
snapshotDue now s = smLastDebugT s <= 0 || now - smLastDebugT s >= debugRefreshSec

-- | Record UI, render, present, and total frame durations in milliseconds,
-- followed by vertex, index, and command counts. Increments the present count.
noteDebugPresent :: DebugSamplerRef -> Double -> Double -> Double -> Double -> Int -> Int -> Int -> IO ()
noteDebugPresent ref uiMs renderMs presentMs frameMs verts indices cmds = do
  now <- getMonotonicTime
  atomicModifyIORef' ref $ \s ->
    let dt = now - smLastPresentT s
        instantFps =
          if dt > 1e-4 && dt < 0.25
            then 1 / dt
            else 0
        ema' =
          if instantFps > 0
            then blend (smPresentEma s) instantFps
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

-- | The published snapshot, rebuilt at most every 'debugRefreshSec' and cached
-- in between. A due query samples the core stats and hands them to @build@,
-- which adds the backend's fields: window size and mouse position are left 0
-- for it to fill. Every query marks the readout active ('isDebugActive').
refreshDebugSnapshot :: DebugSamplerRef -> IORef s -> (CoreDebugSnapshot -> IO s) -> IO s
refreshDebugSnapshot ref cache build = do
  now <- getMonotonicTime
  due <- atomicModifyIORef' ref $ \cur -> (cur {smLastQueryT = now}, snapshotDue now cur)
  if not due
    then readIORef cache
    else do
      rts <- readRtsSnapshot
      core <- atomicModifyIORef' ref $ \cur ->
        -- Actual presents per second since the previous refresh. Unlike the
        -- per-present EMA this stays truthful when presents are sparse (idle
        -- app: ~4/s with the HUD open, not the theoretical fps of one fast
        -- frame).
        let elapsed = now - smRateT cur
            rate
              | elapsed > 1e-3 = fromIntegral (smPresents cur - smRatePresents cur) / elapsed
              | otherwise = 0
            cur' = cur {smLastDebugT = now, smRatePresents = smPresents cur, smRateT = now}
         in (cur', (coreDebugSnapshot cur' rts) {dbgPresentFps = rate})
      snap <- build core
      writeIORef cache snap
      pure snap

coreDebugSnapshot :: DebugSampler -> RtsStatsSnapshot -> CoreDebugSnapshot
coreDebugSnapshot s rts =
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
    , dbgWinW = 0
    , dbgWinH = 0
    , dbgMouseX = 0
    , dbgMouseY = 0
    , dbgRts = rts
    }

-- | Label/value rows for frame rates, durations, and cumulative counts.
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

-- | Label/value rows for vertex, index, and draw-command counts.
formatDrawRows :: CoreDebugSnapshot -> [(Text, Text)]
formatDrawRows s =
  [ ("vertices", T.pack (printf "%10d" (dbgVerts s)))
  , ("indices", T.pack (printf "%10d" (dbgIndices s)))
  , ("commands", T.pack (printf "%10d" (dbgCmds s)))
  ]

-- | Runtime-stat rows, or instructions to enable @+RTS -T@ when statistics are off.
formatCoreRtsRows :: CoreDebugSnapshot -> [(Text, Text)]
formatCoreRtsRows core
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
  where
    s = dbgRts core
