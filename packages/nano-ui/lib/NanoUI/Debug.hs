module NanoUI.Debug
  ( debugRefreshSec
  , blend
  , bytesMb
  , nsMs
  , rtsFields
  , RtsStatsSnapshot (..)
  , readRtsSnapshot
  , formatRtsRows
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32, Word64)
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
      , ("haskell", T.pack (printf "%d cap / %d cpu" (rtsCaps s) (rtsCpus s)))
      ]
  | otherwise =
      [ ("haskell", T.pack (printf "%d cap / %d cpu" (rtsCaps s) (rtsCpus s)))
      , ("gc total", T.pack (printf "%d" (rtsGcs s)))
      , ("gc major", T.pack (printf "%d" (rtsMajorGcs s)))
      , ("last gen", T.pack (printf "%d" (rtsLastGcGen s)))
      , ("last gc", T.pack (printf "%.2f ms" (rtsLastGcMs s)))
      , ("heap live", T.pack (printf "%.1f MiB" (rtsLiveMb s)))
      , ("heap alloc", T.pack (printf "%.1f MiB" (rtsAllocMb s)))
      , ("copied", T.pack (printf "%.1f MiB" (rtsCopiedMb s)))
      , ("rss max", T.pack (printf "%.1f MiB" (rtsMaxMemMb s)))
      , ("gc time", T.pack (printf "%.1f%%" (rtsGcPct s)))
      ]
