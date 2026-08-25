-- | The contract each platform driver implements. Everything above this line
-- is platform independent.
module NanoUI.Term.Driver.Types
  ( Driver (..)
  ) where

import Data.ByteString.Builder (Builder)
import NanoUI.Term.Event (TermEvent)

data Driver = Driver
  { -- | Visible terminal size in cells, as (columns, rows).
    drvSize :: IO (Int, Int)
  , -- | Wait up to the given number of milliseconds for input. Returns an
    -- empty list on timeout, so callers can use it as both a poll and a block.
    drvRead :: Int -> IO [TermEvent]
  , -- | Queue output bytes.
    drvWrite :: Builder -> IO ()
  , -- | Push queued output to the terminal.
    drvFlush :: IO ()
  }
