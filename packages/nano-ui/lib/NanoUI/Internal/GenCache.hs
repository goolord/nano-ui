-- | Two-generation 'Data.IntMap.Strict.IntMap' cache bounded to a small multiple of the entries
-- touched in a frame. Companion caches (@MarkdownCache@, @PlotCache@) use it
-- so churning or growing widget ids never retain unbounded stale entries.
module NanoUI.Internal.GenCache
  ( IntGenCache
  , newIntGenCache
  , intGenCacheStartFrame
  , intGenCacheLookup
  , intGenCacheTouch
  , intGenCacheInsert
  , intGenCacheSize
  ) where

import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.IntMap.Strict qualified as IM

-- | Mutable two-generation cache keyed by 'Int' (typically @intKey@).
newtype IntGenCache a = IntGenCache (IORef (GenState a))

data GenState a = GenState
  { gsMinBound :: !Int
  , gsBoundNow :: !Int
  , gsFrame :: !Int
  , gsCount :: !Int
  , gsMost :: !Int
  , gsCur :: !(IM.IntMap a)
  , gsOld :: !(IM.IntMap a)
  }

-- | Create an empty generational cache whose generation rotates after at least
-- @minBoundEntries@ insertions (clamped to at least 16).
newIntGenCache :: Int -> IO (IntGenCache a)
newIntGenCache minEntries = do
  let !b = max 16 minEntries
  IntGenCache <$> newIORef (GenState b b (-1) 0 0 IM.empty IM.empty)

-- | Mark the start of frame @frame@, resetting the per-frame active count
-- once per frame number while preserving the high-water mark since the last
-- generation turn.
intGenCacheStartFrame :: IntGenCache a -> Int -> IO ()
intGenCacheStartFrame (IntGenCache ref) frame = modifyIORef' ref $ \gs ->
  if gsFrame gs == frame
    then gs
    else gs {gsFrame = frame, gsCount = 0, gsMost = max (gsCount gs) (gsMost gs)}

-- | Look up key @k@ in the current generation first, falling back to the
-- previous generation.
intGenCacheLookup :: IntGenCache a -> Int -> IO (Maybe a)
intGenCacheLookup (IntGenCache ref) k = do
  gs <- readIORef ref
  pure (maybe (IM.lookup k (gsOld gs)) Just (IM.lookup k (gsCur gs)))

-- | Record a hit for @(k, val)@ in the current frame, promoting @val@ from the
-- older generation into the current one when needed.
intGenCacheTouch :: IntGenCache a -> Int -> a -> IO ()
intGenCacheTouch (IntGenCache ref) k val = do
  gs <- readIORef ref
  writeIORef ref $!
    if IM.member k (gsCur gs)
      then gs {gsCount = gsCount gs + 1}
      else insertGen gs k val

-- | Insert or update @(k, val)@ in the current generation, rotating
-- generations when the current bound is reached.
intGenCacheInsert :: IntGenCache a -> Int -> a -> IO ()
intGenCacheInsert (IntGenCache ref) k val = do
  gs <- readIORef ref
  writeIORef ref $! insertGen gs k val

insertGen :: GenState a -> Int -> a -> GenState a
insertGen gs k val
  | IM.size (gsCur gs) < gsBoundNow gs =
      gs {gsCount = gsCount gs + 1, gsCur = IM.insert k val (gsCur gs)}
  | otherwise =
      gs
        { gsCount = gsCount gs + 1
        , gsBoundNow = max (gsMinBound gs) (2 * max (gsCount gs) (gsMost gs))
        , gsMost = 0
        , gsCur = IM.singleton k val
        , gsOld = gsCur gs
        }

-- | Distinct entries retained across both generations, for diagnostics.
intGenCacheSize :: IntGenCache a -> IO Int
intGenCacheSize (IntGenCache ref) = do
  gs <- readIORef ref
  pure (IM.size (IM.union (gsCur gs) (gsOld gs)))
