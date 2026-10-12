
-- | The mutable cell behind a 'NanoUI.Var.Var' and the stamps that say who
-- saw it when. Stamps are ticks from one process-wide clock, so a cell
-- shared by two sessions never mistakes one session's pass for another's.
module NanoUI.Internal.Var.Cell
  ( Cell (..)
  , newCell
  , newInertCell
  , isInert
  , readStamp
  , writeStamp
  , obsSlot
  , seenSlot
  , chgSlot
  , nextTick
  , newStamps
  , passSlot
  , baseSlot
  , passNoSlot
  , flagSlot
  , lastPassesSlot
  , frameSlots
  )
where

import Control.Monad.Primitive (RealWorld)
import Data.IORef (IORef, newIORef)
import Data.Primitive.ByteArray (MutableByteArray (..), newByteArray, writeByteArray)
import Data.Primitive.PrimArray (MutablePrimArray, newPrimArray, readPrimArray, sameMutablePrimArray, setPrimArray, writePrimArray)
import Data.Word (Word64)
import GHC.Exts (Int (..), fetchAddIntArray#, (+#))
import GHC.IO (IO (..))
import System.IO.Unsafe (unsafePerformIO)

-- | A value and its stamps:
--
-- * 'obsSlot': the pass that last observed it, by a tracked read or a
--   widget bound to it.
-- * 'seenSlot': the pass that last read it with a tracked read.
-- * 'chgSlot': the tick of its last change.
data Cell s = Cell {-# UNPACK #-} !(IORef s) {-# UNPACK #-} !(MutablePrimArray RealWorld Word64)

obsSlot, seenSlot, chgSlot :: Int
obsSlot = 0
seenSlot = 1
chgSlot = 2

-- | Slots of a context's frame record ('NanoUI.Internal.Context.ctxVarFrame'):
-- the current pass's tick (0 outside a pass), the tick changes in this frame
-- are newer than, the pass number, request flags, and the last frame's pass
-- count.
passSlot, baseSlot, passNoSlot, flagSlot, lastPassesSlot, frameSlots :: Int
passSlot = 0
baseSlot = 1
passNoSlot = 2
flagSlot = 3
lastPassesSlot = 4
frameSlots = 5

-- | @n@ zeroed stamps.
newStamps :: Int -> IO (MutablePrimArray RealWorld Word64)
newStamps n = do
  stamps <- newPrimArray n
  setPrimArray stamps 0 n 0
  pure stamps

newCell :: s -> IO (Cell s)
newCell s = Cell <$> newIORef s <*> newStamps 3

-- | Stamps shared by every inert cell and never written: an inert cell
-- ('NanoUI.Var.withValue') takes part in no rerun, damage or wake.
{-# NOINLINE inertStamps #-}
inertStamps :: MutablePrimArray RealWorld Word64
inertStamps = unsafePerformIO (newStamps 3)

newInertCell :: s -> IO (Cell s)
newInertCell s = do
  ref <- newIORef s
  pure (Cell ref inertStamps)

{-# INLINE isInert #-}
isInert :: Cell s -> Bool
isInert (Cell _ stamps) = sameMutablePrimArray stamps inertStamps

{-# INLINE readStamp #-}
readStamp :: Cell s -> Int -> IO Word64
readStamp (Cell _ stamps) = readPrimArray stamps

{-# INLINE writeStamp #-}
writeStamp :: Cell s -> Int -> Word64 -> IO ()
writeStamp (Cell _ stamps) = writePrimArray stamps

-- | The last tick handed out, in one machine word.
{-# NOINLINE tickClock #-}
tickClock :: MutableByteArray RealWorld
tickClock = unsafePerformIO $ do
  arr <- newByteArray 8
  writeByteArray arr 0 (1 :: Int)
  pure arr

-- | A tick later than every tick handed out before it. An atomic add, which
-- allocates nothing, where an 'IORef' would box every tick.
{-# INLINE nextTick #-}
nextTick :: IO Word64
nextTick = case tickClock of
  MutableByteArray arr -> IO $ \s -> case fetchAddIntArray# arr 0# 1# s of
    (# s', old #) -> (# s', fromIntegral (I# (old +# 1#)) #)
