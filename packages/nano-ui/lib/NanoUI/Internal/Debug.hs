-- | Debug readout sampling shared by the backends: frame timing and skip
-- counts, RTS statistics, draw counts, and the rows the debug windows show.
module NanoUI.Internal.Debug
  ( debugRefreshSec
  , CoreDebugSnapshot (..)
  , emptyCoreDebugSnapshot
  , DebugSamplerRef
  , newDebugSampler
  , noteDebugLoop
  , noteDebugSkip
  , debugCadence
  , noteDebugPresent
  , refreshDebugSnapshot
  , formatFpsRows
  , formatDrawRows
  ) where

import Data.Dynamic (Dynamic, Typeable, fromDynamic, toDyn)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
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

-- | Fold the rate of an interval of @dt@ seconds into the exponential moving
-- average @ema@, with 15% weight on the sample. A non-positive @ema@ starts
-- over at the sample. Intervals outside 0.0001-0.25 seconds leave it unchanged.
blendRate :: Double -> Double -> Double
blendRate ema dt
  | dt > 1e-4 && dt < 0.25 = if ema <= 0 then 1 / dt else ema * 0.85 + (1 / dt) * 0.15
  | otherwise = ema

-- | Published frame rates, latest phase durations in milliseconds, cumulative
-- present/skip counts, geometry counts, backend-supplied window coordinates,
-- and runtime statistics.
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
  , dbgRts        :: ![(Text, Text)]
  -- ^ Label/value rows of runtime statistics, or instructions to enable
  -- @+RTS -T@ when they are off. Memory is in MiB, the last collection's
  -- duration in milliseconds, and GC time is a share of elapsed run time.
  }
  deriving (Eq, Show)

-- | Zeroed placeholder before a backend publishes a frame sample.
emptyCoreDebugSnapshot :: CoreDebugSnapshot
emptyCoreDebugSnapshot = CoreDebugSnapshot 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 []

-- | Sample the rows of 'dbgRts'.
readRtsRows :: IO [(Text, Text)]
readRtsRows = do
  caps <- getNumCapabilities
  cpus <- getNumProcessors
  rtsOn <- getRTSStatsEnabled
  let haskell = ("haskell", T.pack (printf "%2d cap / %2d cpu" caps cpus))
  if not rtsOn
    then pure [("rts", "stats off (need +RTS -T)"), haskell]
    else do
      st <- getRTSStats
      let lastGc = gc st
          count :: Word32 -> Text
          count = T.pack . printf "%10d"
          mb :: Word64 -> Text
          mb n = T.pack (printf "%6.1f MiB" (fromIntegral n / (1024 * 1024) :: Double))
          gcPct :: Double
          gcPct
            | elapsed_ns st > 0 = 100 * fromIntegral (gc_elapsed_ns st) / fromIntegral (elapsed_ns st)
            | otherwise = 0
          gcMs = fromIntegral (gcdetails_elapsed_ns lastGc) / 1.0e6 :: Double
      pure
        [ haskell
        , ("gc total", count (gcs st))
        , ("gc major", count (major_gcs st))
        , ("last gen", count (gcdetails_gen lastGc))
        , ("last gc", T.pack (printf "%7.2f ms" gcMs))
        , ("heap live", mb (gcdetails_live_bytes lastGc))
        , ("heap alloc", mb (allocated_bytes st))
        , ("copied", mb (copied_bytes st))
        , ("rss max", mb (max_mem_in_use_bytes st))
        , ("gc time", T.pack (printf "%9.1f%%" gcPct))
        ]

-- | Mutable sampler contents. Timestamp fields use monotonic seconds. Backends
-- update it through the @noteDebug*@ functions.
data DebugSampler = DebugSampler
  { smCore :: !CoreDebugSnapshot
  -- ^ The next snapshot, but for what a refresh samples: the present rate,
  -- window coordinates and runtime statistics.
  , smPublished :: !Dynamic
  -- ^ The backend's snapshot the last refresh built.
  , smLastQueryT :: !Double
  , smRatePresents :: !Word64
  , smLastDebugT :: !Double
  -- ^ When the last refresh was, or the sampler was made.
  }

-- | Session-owned sampler reference, updated atomically by sampling operations.
type DebugSamplerRef = IORef DebugSampler

-- | Empty sampler with its rate interval starting at the current monotonic time.
newDebugSampler :: IO DebugSamplerRef
newDebugSampler = newIORef . DebugSampler emptyCoreDebugSnapshot (toDyn ()) 0 0 =<< getMonotonicTime

noteCore :: DebugSamplerRef -> (CoreDebugSnapshot -> CoreDebugSnapshot) -> IO ()
noteCore ref f = atomicModifyIORef' ref $ \s -> (s {smCore = f (smCore s)}, ())

-- | Record loop delta time in seconds. Intervals outside 0.0001-0.25 seconds
-- do not contribute to the loop-rate moving average.
noteDebugLoop :: DebugSamplerRef -> Float -> IO ()
noteDebugLoop ref dt =
  noteCore ref $ \c -> c {dbgLoopFps = blendRate (dbgLoopFps c) (realToFrac dt)}

-- | Increment the count of loop passes that skipped presentation.
noteDebugSkip :: DebugSamplerRef -> IO ()
noteDebugSkip ref = noteCore ref $ \c -> c {dbgSkips = dbgSkips c + 1}

-- | Whether the readout is active, and whether it is and its published
-- snapshot is older than 'debugRefreshSec'. Activity is driven by actual
-- snapshot consumption: a snapshot query ('refreshDebugSnapshot') refreshes
-- 'smLastQueryT', so the 4 Hz refresh loop only runs while a stats window is
-- being built. An open window alone does not count as activity, or the event
-- loop would wake every refresh period while any floating window is open.
debugCadence :: DebugSamplerRef -> IO (Bool, Bool)
debugCadence ref = do
  now <- getMonotonicTime
  s <- readIORef ref
  let active = now - smLastQueryT s < 1.0
  pure (active, active && snapshotDue now s)

snapshotDue :: Double -> DebugSampler -> Bool
snapshotDue now s = now - smLastDebugT s >= debugRefreshSec

-- | Record UI, render, present, and total frame durations in milliseconds,
-- followed by vertex, index, and command counts. Increments the present count.
noteDebugPresent :: DebugSamplerRef -> Double -> Double -> Double -> Double -> Int -> Int -> Int -> IO ()
noteDebugPresent ref uiMs renderMs presentMs frameMs verts indices cmds =
  noteCore ref $ \c ->
    c
      { dbgPresents = dbgPresents c + 1
      , dbgUiMs = uiMs
      , dbgRenderMs = renderMs
      , dbgPresentMs = presentMs
      , dbgFrameMs = frameMs
      , dbgVerts = verts
      , dbgIndices = indices
      , dbgCmds = cmds
      }

-- | The published snapshot, rebuilt at most every 'debugRefreshSec' and kept
-- by the sampler in between. A due query, or the first, samples the core
-- stats and hands them to @build@, which adds the backend's fields: window
-- size and mouse position are left 0 for it to fill. Every query marks the
-- readout active ('debugCadence').
refreshDebugSnapshot :: Typeable s => DebugSamplerRef -> (CoreDebugSnapshot -> IO s) -> IO s
refreshDebugSnapshot ref build = do
  now <- getMonotonicTime
  cached <- atomicModifyIORef' ref $ \cur ->
    (cur {smLastQueryT = now}, if snapshotDue now cur then Nothing else fromDynamic (smPublished cur))
  case cached of
    Just snap -> pure snap
    Nothing -> do
      rts <- readRtsRows
      core <- atomicModifyIORef' ref $ \cur ->
        -- Actual presents per second since the previous refresh, which stays
        -- truthful when presents are sparse (idle app: ~4/s with the HUD
        -- open, not the theoretical fps of one fast frame).
        let elapsed = now - smLastDebugT cur
            presents = dbgPresents (smCore cur)
            rate
              | elapsed > 1e-3 = fromIntegral (presents - smRatePresents cur) / elapsed
              | otherwise = 0
         in ( cur {smLastDebugT = now, smRatePresents = presents}
            , (smCore cur) {dbgPresentFps = rate, dbgRts = rts}
            )
      snap <- build core
      snap <$ atomicModifyIORef' ref (\cur -> (cur {smPublished = toDyn snap}, ()))

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
