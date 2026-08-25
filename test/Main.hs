module Main (main) where

import Control.Monad (replicateM, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import NanoUI

main :: IO ()
main = do
  failed <- newIORef 0
  ctx <- newContext

  runIdStabilityTest ctx failed
  runWithKeyTest ctx failed
  runLayoutTest ctx failed
  runDrawTest ctx failed
  runOverlayTest ctx failed
  runInteractionTest ctx failed
  runIdleTest ctx failed
  runAnimationIdleTest ctx failed
  runAsciiTest ctx failed

  n <- readIORef failed
  if n == 0
    then putStrLn "All tests passed."
    else do
      putStrLn $ show n ++ " test(s) failed."
      fail "tests failed"

bump :: IORef Int -> IO ()
bump r = modifyIORef r (+ 1)

modifyIORef :: IORef Int -> (Int -> Int) -> IO ()
modifyIORef r f = readIORef r >>= writeIORef r . f

runIdStabilityTest :: Context -> IORef Int -> IO ()
runIdStabilityTest ctx failed = do
  let inp = emptyInput {inputWindowSize = Size 100 100}
  (ids, _, _) <-
    runFrame
      ctx
      inp
      (column defaultLayout (replicateM 3 currentId))
  case ids of
    [a, b, c] -> when (a /= b || b /= c) $ bump failed
    _ -> bump failed

runWithKeyTest :: Context -> IORef Int -> IO ()
runWithKeyTest ctx failed = do
  let inp = emptyInput {inputWindowSize = Size 200 200}
  (_, _, _) <-
    runFrame
      ctx
      inp
      ( withKey (0 :: Int) $ do
          _ <- button "A"
          withKey (1 :: Int) $ button "A"
      )
  pure ()

runLayoutTest :: Context -> IORef Int -> IO ()
runLayoutTest ctx failed = do
  let inp = emptyInput {inputWindowSize = Size 400 300}
  (_, _, draw) <-
    runFrame
      ctx
      inp
      ( column
          (defaultLayout {layoutWidth = Grow 1, layoutHeight = Grow 1, layoutGap = 8})
          ( do
              row
                (defaultLayout {layoutWidth = Grow 1})
                ( do
                    _ <- spacer (Grow 1) Fit
                    label "grow test"
                )
              label "nested"
          )
      )
  when (drawVertexCount draw <= 0) $ bump failed

runDrawTest :: Context -> IORef Int -> IO ()
runDrawTest ctx failed = do
  let inp = emptyInput {inputWindowSize = Size 100 100}
  (_, _, draw) <-
    runFrame
      ctx
      inp
      (column defaultLayout (label "draw"))
  when (drawIndexCount draw < 6) $ bump failed
  when (null (drawCommands draw)) $ bump failed

runOverlayTest :: Context -> IORef Int -> IO ()
runOverlayTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 200 80}
  -- Establish button rect
  (_, _, _) <-
    runFrame
      ctx
      inp0
      (column defaultLayout (button "Hover"))
  let inp1 =
        inp0
          { inputMousePos = V2 10 10
          , inputMouseDown = False
          , inputMousePressed = False
          , inputMouseReleased = False
          }
  (_, _, draw) <-
    runFrame
      ctx
      inp1
      ( do
          resp <- button "Hover"
          tooltip "tip" resp
      )
  let hasOverlay = any ((== LayerOverlay) . cmdLayer) (drawCommands draw)
  when (not hasOverlay) $ bump failed

runInteractionTest :: Context -> IORef Int -> IO ()
runInteractionTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 200 100}
  -- Frame 1: layout, store prev rects
  runFrame ctx inp0 (column defaultLayout (button "Click"))
  -- Frame 2: press on button
  let inpPress =
        inp0
          { inputMousePos = V2 10 10
          , inputMousePressed = True
          , inputMouseDown = True
          , inputMouseReleased = False
          }
  runFrame ctx inpPress (column defaultLayout (button "Click"))
  -- Frame 3: release => click
  let inpRelease =
        inpPress
          { inputMousePressed = False
          , inputMouseDown = False
          , inputMouseReleased = True
          }
  (_, msgs, _) <- runFrame ctx inpRelease (column defaultLayout (button "Click"))
  when (null msgs) $ bump failed

runIdleTest :: Context -> IORef Int -> IO ()
runIdleTest ctx failed = do
  let inp = emptyInput {inputWindowSize = Size 100 100}
  runFrame ctx inp (label "idle")
  need <- needsRedraw ctx inp inp
  when need $ bump failed

runAnimationIdleTest :: Context -> IORef Int -> IO ()
runAnimationIdleTest ctx failed = do
  let inp = emptyInput {inputWindowSize = Size 100 100, inputDeltaTime = 0.05}
  runFrame ctx inp (label "anim")
  startAnimation ctx (WidgetId 42) 0 1 0.5
  need <- needsRedraw ctx inp inp
  when (not need) $ bump failed

runAsciiTest :: Context -> IORef Int -> IO ()
runAsciiTest ctx failed = do
  let inp = emptyInput {inputWindowSize = Size 40 10}
  (_, _, draw) <-
    runFrame
      ctx
      inp
      (column (defaultLayout {layoutWidth = Grow 1, layoutHeight = Grow 1}) (label "snap"))
  let ascii = renderASCII 40 10 draw
  when (length ascii /= 10) $ bump failed
  when (all (all (== ' ')) ascii) $ bump failed
