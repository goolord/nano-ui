module Main (main) where

import Control.Monad (forM_, replicateM_, void, when)
import GHC.Conc (getAllocationCounter)
import NanoUI
import NanoUI.Backend (burstNextIds, emptyInput)
import NanoUI.Testing (Context, newContext, runFrame)
import System.Exit (exitFailure)
import System.Mem (performGC)
import Test.Tasty.Bench

benchInput :: Input
benchInput = emptyInput {inputWindowSize = Size 100 100}

-- A layout root is required or runFrame overflows.
idBurst :: NanoUI ()
idBurst = column (burstNextIds 4096)

scopedWidgets :: NanoUI ()
scopedWidgets = rows (void nextId)

-- | 32 rows of 32 @cell@s.
rows :: NanoUI () -> NanoUI ()
rows cell =
  columnWith (gap 2)
    $ replicateM_ 32
    $ rowWith (gap 2)
    $ replicateM_ 32 cell

-- | A context that has run @ui@, so its next frame is warm.
warmContext :: NanoUI a -> IO Context
warmContext ui = do
  ctx <- newContext
  replicateM_ 2 (runFrame ctx benchInput ui)
  pure ctx

-- | Bytes a warm frame of @ui@ allocates.
frameAlloc :: NanoUI a -> IO Integer
frameAlloc ui = do
  ctx <- warmContext ui
  performGC
  -- The thread's allocation counter is current even if the frame never
  -- fills the nursery; RTSStats.allocated_bytes only catches up at a GC.
  before <- getAllocationCounter
  _ <- runFrame ctx benchInput ui
  after <- getAllocationCounter
  pure (fromIntegral (before - after))

main :: IO ()
main = do
  -- Each scene against the same nodes without the ids: issuing ids
  -- allocates nothing, so a boxed id cursor (16 bytes or more an id) trips
  -- this.
  let scenes = [("burst4096", idBurst, column (pure ())), ("scopedWidgets", scopedWidgets, rows (pure ()))]
  forM_ scenes $ \(name, ui, base) -> do
    extra <- (-) <$> frameAlloc ui <*> frameAlloc base
    when (extra > 1024) $ do
      putStrLn ("FAIL: " ++ name ++ " allocated " ++ show extra ++ " bytes more than the same nodes without ids")
      exitFailure
  ctxs <- mapM (\(_, ui, _) -> warmContext ui) scenes
  defaultMain
    [ bgroup
        "id/nextId"
        [ bench name $ whnfIO (void (runFrame ctx benchInput ui))
        | ((name, ui, _), ctx) <- zip scenes ctxs
        ]
    ]
