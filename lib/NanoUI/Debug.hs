module NanoUI.Debug
  ( debugRefreshSec
  , blend
  , bytesMb
  , nsMs
  , rtsFields
  ) where

import Data.Word (Word32, Word64)
import GHC.Stats (GCDetails (..), RTSStats (..))

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
