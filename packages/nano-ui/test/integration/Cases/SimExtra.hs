-- | Sim helpers the input, keyboard and focus cases share.
module Cases.SimExtra (above) where

import GHC.Stack (HasCallStack)
import NanoUI.Testing.Sim

-- | Check that the value is greater than @lo@.
above :: (HasCallStack, Ord b, Show b) => b -> b -> Sim a ()
above lo x = checkGt x lo
