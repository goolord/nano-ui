
-- | Mutable cell backing a 'NanoUI.Var.Var' and its per-pass read/mutation
-- bitmasks. Timestamps come from a single process-wide monotonic counter so
-- cells shared across sessions keep distinct pass stamps.
module NanoUI.Internal.Var.Cell
  ( Cell (..)
  , newCell
  , newInertCell
  , isInert
  , Sig (..)
  , allBits
  , obsSet
  , seenSet
  , setHits
  , seenWhole
  , addToSet
  , orWord
  , nextTick
  , countChange
  , varChangeCount
  , newStamps
  , passSlot
  , passNoSlot
  , flagSlot
  , lastPassesSlot
  , frameSlots
  )
where

import Control.Monad (when)
import Control.Monad.Primitive (RealWorld)
import Data.Bits (complement, (.&.), (.|.))
import Data.IORef (IORef, newIORef)
import Data.Primitive.ByteArray (MutableByteArray (..), newByteArray, writeByteArray)
import Data.Primitive.PrimArray (MutablePrimArray, readPrimArray, sameMutablePrimArray, setPrimArray, writePrimArray)
import Data.Word (Word64)
import GHC.Exts (Int (..), atomicReadIntArray#, fetchAddIntArray#, (+#))
import GHC.IO (IO (..))
import System.IO.Unsafe (unsafePerformIO)
import NanoUI.Internal.Types (newZeroedPrimArray)

-- | Mutable state, and its 'obsSet' and 'seenSet' in a UI-thread stamp
-- array.
data Cell s = Cell
  {-# UNPACK #-} !(IORef s)
  {-# UNPACK #-} !(MutablePrimArray RealWorld Word64)

-- | Two-level bit signature of the portion of a cell addressed by a lens path:
-- the first-level record field or list key bitmask, the second-level bitmask
-- ('allBits' when the path stops at one level), and whether the path is still
-- open, so that a further step can narrow it.
-- An opaque step such as 'NanoUI.Var.zoomVar' covers all bits beneath its
-- prefix and marks the signature closed so further steps leave it unchanged.
--
-- Because hashes map multiple fields or keys into 64 bits, bitmask intersection
-- is conservative: non-overlapping paths may collide, while overlapping paths
-- always intersect.
data Sig = Sig {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64 !Bool
  deriving (Eq)

-- | A second-level mask with every bit set, meaning the path stops at the
-- first level ('Sig').
allBits :: Word64
allBits = complement 0

-- | Whether @sig@ overlaps a set of paths, given as the first-level bits of
-- those that stop at the first level (@whole@), and the first- and
-- second-level bits of those that go on (@firsts@, @seconds@).
{-# INLINE sigHits #-}
sigHits :: Word64 -> Word64 -> Word64 -> Sig -> Bool
sigHits whole firsts seconds (Sig a b _)
  | b == allBits = (whole .|. firsts) .&. a /= 0
  | otherwise = whole .&. a /= 0 || (firsts .&. a /= 0 && seconds .&. b /= 0)

-- | Where each set starts: a tick, then the @whole@, @firsts@ and
-- @seconds@ bits ('sigHits').
obsSet, seenSet, stampWords :: Int
obsSet = 0
seenSet = 4
stampWords = 8

-- | Slots of a context's frame record ('NanoUI.Internal.Context.ctxVarFrame'):
-- the current pass's tick (0 outside a pass), the pass number, request
-- flags, and the last frame's pass count.
passSlot, passNoSlot, flagSlot, lastPassesSlot, frameSlots :: Int
passSlot = 0
passNoSlot = 1
flagSlot = 2
lastPassesSlot = 3
frameSlots = 4

-- | A zeroed array of @n@ stamp words.
newStamps :: Int -> IO (MutablePrimArray RealWorld Word64)
newStamps = newZeroedPrimArray

-- | A cell holding @s@, with its own zeroed stamps.
newCell :: s -> IO (Cell s)
newCell s = Cell <$> newIORef s <*> newStamps stampWords

-- | Stamps shared by every inert cell and never written: an inert cell
-- ('NanoUI.Var.withValue') takes part in no rerun, damage or wake.
{-# NOINLINE inertStamps #-}
inertStamps :: MutablePrimArray RealWorld Word64
inertStamps = unsafePerformIO (newStamps stampWords)

-- | A cell holding @s@ that uses 'inertStamps', so it records no
-- observation.
newInertCell :: s -> IO (Cell s)
newInertCell s = do
  ref <- newIORef s
  pure (Cell ref inertStamps)

-- | Whether the cell was made by 'newInertCell'.
{-# INLINE isInert #-}
isInert :: Cell s -> Bool
isInert (Cell _ stamps) = sameMutablePrimArray stamps inertStamps

{-# INLINE readStamp #-}
readStamp :: Cell s -> Int -> IO Word64
readStamp (Cell _ stamps) = readPrimArray stamps

{-# INLINE writeStamp #-}
writeStamp :: Cell s -> Int -> Word64 -> IO ()
writeStamp (Cell _ stamps) = writePrimArray stamps

-- | Whether the set at @set@, stamped with tick @t@, holds a path that
-- overlaps @sig@. A set stamped with another tick is empty.
{-# INLINE setHits #-}
setHits :: Cell s -> Int -> Word64 -> Sig -> IO Bool
setHits cell set t sig = do
  t0 <- readStamp cell set
  if t0 /= t then pure False else bitsHit cell (set + 1) sig

bitsHit :: Cell s -> Int -> Sig -> IO Bool
bitsHit cell at sig =
  sigHits <$> readStamp cell at <*> readStamp cell (at + 1) <*> readStamp cell (at + 2) <*> pure sig

-- | Whether 'seenSet' for pass @p@ already covers all first-level bits of a
-- single-level @sig@, so a repeat read records nothing new.
{-# INLINE seenWhole #-}
seenWhole :: Cell s -> Word64 -> Sig -> IO Bool
seenWhole cell p (Sig a b _)
  | b /= allBits = pure False
  | otherwise = do
      t0 <- readStamp cell seenSet
      if t0 /= p then pure False else (\w -> w .&. a == a) <$> readStamp cell (seenSet + 1)

-- | Add the path to the set at @set@ under tick @t@, emptying it first if it
-- holds another tick's paths.
{-# INLINE addToSet #-}
addToSet :: Cell s -> Int -> Word64 -> Sig -> IO ()
addToSet cell set t sig = do
  t0 <- readStamp cell set
  when (t0 /= t) (writeStamp cell set t)
  addBits cell (set + 1) (t0 /= t) sig

-- | Add a path's bits to the three bit slots from @at@, clearing them first
-- when @fresh@.
addBits :: Cell s -> Int -> Bool -> Sig -> IO ()
addBits (Cell _ stamps) at fresh (Sig a b _) = do
  when fresh (setPrimArray stamps at 3 0)
  if b == allBits
    then orWord stamps at a
    else orWord stamps (at + 1) a >> orWord stamps (at + 2) b

-- | Set bits in one word of a prim array, writing only when that changes it.
{-# INLINE orWord #-}
orWord :: MutablePrimArray RealWorld Word64 -> Int -> Word64 -> IO ()
orWord arr i x = do
  old <- readPrimArray arr i
  when (old .|. x /= old) (writePrimArray arr i (old .|. x))

-- | Process-wide monotonic tick counter, then the count of changes any cell
-- has had ('varChangeCount'), each in a machine word.
{-# NOINLINE tickClock #-}
tickClock :: MutableByteArray RealWorld
tickClock = unsafePerformIO $ do
  arr <- newByteArray 16
  writeByteArray arr 0 (1 :: Int)
  writeByteArray arr 1 (0 :: Int)
  pure arr

-- | How many changes any cell has had, process-wide. A window that ran its
-- view when this was lower may show a value since changed, which is how a
-- backend with several windows knows to run the others' views again.
varChangeCount :: IO Word64
varChangeCount = case tickClock of
  MutableByteArray arr -> IO $ \s -> case atomicReadIntArray# arr 1# s of
    (# s', n #) -> (# s', fromIntegral (I# n) #)

-- | Count a change to a cell ('varChangeCount').
{-# INLINE countChange #-}
countChange :: IO ()
countChange = case tickClock of
  MutableByteArray arr -> IO $ \s -> case fetchAddIntArray# arr 1# 1# s of
    (# s', _ #) -> (# s', () #)

-- | Atomically increment and return the next tick without boxing an 'IORef'.
{-# INLINE nextTick #-}
nextTick :: IO Word64
nextTick = case tickClock of
  MutableByteArray arr -> IO $ \s -> case fetchAddIntArray# arr 0# 1# s of
    (# s', old #) -> (# s', fromIntegral (I# (old +# 1#)) #)
