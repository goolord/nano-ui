
-- | The mutable cell behind a 'NanoUI.Var.Var' and the stamps that say who
-- saw which part of it when. Stamps are ticks from one process-wide clock,
-- so a cell shared by two sessions never mistakes one session's pass for
-- another's.
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
  , passHits
  , seenWhole
  , addToSet
  , changedSince
  , addChange
  , orWord
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

import Control.Monad (when)
import Control.Monad.Primitive (RealWorld)
import Data.Bits (complement, (.&.), (.|.))
import Data.IORef (IORef, newIORef)
import Data.Primitive.ByteArray (MutableByteArray (..), newByteArray, writeByteArray)
import Data.Primitive.PrimArray (MutablePrimArray, newPrimArray, readPrimArray, sameMutablePrimArray, setPrimArray, writePrimArray)
import Data.Word (Word64)
import GHC.Exts (Int (..), fetchAddIntArray#, (+#))
import GHC.IO (IO (..))
import System.IO.Unsafe (unsafePerformIO)

-- | A value and its stamps, in three sets of the parts of the value that
-- were touched:
--
-- * 'obsSet': parts observed in the pass stamped there, by a tracked read
--   or a widget bound to them.
-- * 'seenSet': parts read with a tracked read in the pass stamped there.
-- * the change set at 'chgSlot': parts changed since the tick after it.
data Cell s = Cell {-# UNPACK #-} !(IORef s) {-# UNPACK #-} !(MutablePrimArray RealWorld Word64)

-- | Which part of a cell's value a path touches, as two levels of bits: the
-- record field or list key the path takes first, then the one it takes
-- next ('allBits' for all of them). A path whose steps are unknown, such as
-- one through 'NanoUI.Var.zoomVar', touches all of what it has reached,
-- and is closed: steps after it say nothing more.
--
-- Two paths overlap when their bits meet at both levels. Bits stand for
-- many fields or keys at once, so paths that share none of a part may still
-- overlap; ones that share a part always do.
data Sig = Sig {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64 !Bool
  deriving (Eq)

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
-- @seconds@ bits ('sigHits'). The change set has one more slot, the tick
-- its bits start from ('changedSince').
obsSet, seenSet, chgSlot, stampWords :: Int
obsSet = 0
seenSet = 4
chgSlot = 8
stampWords = 13

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
newCell s = Cell <$> newIORef s <*> newStamps stampWords

-- | Stamps shared by every inert cell and never written: an inert cell
-- ('NanoUI.Var.withValue') takes part in no rerun, damage or wake.
{-# NOINLINE inertStamps #-}
inertStamps :: MutablePrimArray RealWorld Word64
inertStamps = unsafePerformIO (newStamps stampWords)

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

-- | Whether the obs and seen sets, stamped @p@, hold a path overlapping
-- @sig@: whether something observed the part in this pass, and whether a
-- tracked read saw it.
{-# INLINE passHits #-}
passHits :: Cell s -> Word64 -> Sig -> IO (Bool, Bool)
passHits cell p sig = (,) <$> setHits cell obsSet p sig <*> setHits cell seenSet p sig

-- | Whether the seen set, stamped @p@, already holds the whole of the part
-- a one-level path names, so a read of it again records nothing new.
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

-- | Whether a part @sig@ overlaps changed after tick @base@. When the
-- change set starts later than @base@, as after another session's change,
-- what changed in between is unknown, so it counts as changed.
changedSince :: Cell s -> Word64 -> Sig -> IO Bool
changedSince cell base sig = do
  chg <- readStamp cell chgSlot
  if chg < base
    then pure False
    else do
      from <- readStamp cell (chgSlot + 1)
      if from > base then pure True else bitsHit cell (chgSlot + 2) sig

-- | Record a change to the part @sig@ at tick @t@. A set that holds only
-- changes from before @base@, the session's frame start, starts over from
-- it. A write from IO passes 0, so its set only grows.
addChange :: Cell s -> Word64 -> Word64 -> Sig -> IO ()
addChange cell base t sig = do
  chg <- readStamp cell chgSlot
  writeStamp cell chgSlot t
  let fresh = chg < base
  when fresh (writeStamp cell (chgSlot + 1) base)
  addBits cell (chgSlot + 2) fresh sig

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
