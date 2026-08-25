module Main (main) where

import Control.Monad (replicateM, when)
import Data.ByteString.Builder (toLazyByteString)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (isInfixOf)
import NanoUI
import NanoUI.Term.Ansi (frameBytes)
import NanoUI.Term.Cells (cellRows, narrowChar, rasterize)
import NanoUI.Term.Event (MouseAction (..), MouseBtn (..), TermEvent (..), noMods)
import NanoUI.Term.Vt (decode, flushPending)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

main :: IO ()
main = do
  failed <- newIORef 0
  ctx <- newContext

  let run name test = do
        before <- readIORef failed
        test ctx failed
        after <- readIORef failed
        when (after > before) $ putStrLn ("FAIL: " ++ name)

  run "id-stability" runIdStabilityTest
  run "id-uniqueness" runIdUniquenessTest
  run "fit-sizing" runFitSizingTest
  run "with-key" runWithKeyTest
  run "layout" runLayoutTest
  run "draw" runDrawTest
  run "overlay" runOverlayTest
  run "interaction" runInteractionTest
  run "hover" runHoverTest
  run "text-input-focus" runTextInputFocusTest
  run "idle" runIdleTest
  run "animation-idle" runAnimationIdleTest
  run "ascii" runAsciiTest
  run "vt-decode" runVtTest
  run "cells-and-diff" runCellsTest

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
  (ids, _, _, _) <-
    runFrame
      ctx
      inp
      (column defaultLayout (replicateM 3 currentId))
  case ids of
    [a, b, c] -> when (a /= b || b /= c) $ bump failed
    _ -> bump failed

-- Widgets at different call sites must not collapse onto one id, or they all
-- share hover, focus and stored state.
runIdUniquenessTest :: Context -> IORef Int -> IO ()
runIdUniquenessTest ctx failed = do
  let inp = emptyInput {inputWindowSize = Size 100 100}
  (ids, _, _, _) <-
    runFrame
      ctx
      inp
      ( column defaultLayout $ do
          a <- currentId
          b <- currentId
          c <- currentId
          pure [a, b, c]
      )
  case ids of
    [a, b, c] -> when (a == b || b == c || a == c) $ bump failed
    _ -> bump failed

-- Fit sizing must follow intrinsic content size, not the available width.
runFitSizingTest :: Context -> IORef Int -> IO ()
runFitSizingTest ctx failed = do
  let inp = emptyInput {inputWindowSize = Size 400 100}
      -- respRect reports the previous frame's rect, so each case needs two runs.
      measure ui = do
        _ <- runFrame ctx inp ui
        (resp, _, _, _) <- runFrame ctx inp ui
        pure (rectW (respRect resp))
  w1 <- measure (column defaultLayout (label "hi"))
  w2 <- measure (column defaultLayout (label "a much longer label"))
  when (w1 <= 0 || w1 >= 400) $ bump failed
  when (w2 <= w1) $ bump failed

runWithKeyTest :: Context -> IORef Int -> IO ()
runWithKeyTest ctx _failed = do
  let inp = emptyInput {inputWindowSize = Size 200 200}
  (_, _, _, _) <-
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
  (_, _, draw, _) <-
    runFrame
      ctx
      inp
      ( column
          (defaultLayout {layoutWidth = Grow 1, layoutHeight = Grow 1, layoutGap = 8})
          ( do
              _ <-
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
  (_, _, draw, _) <-
    runFrame
      ctx
      inp
      (column defaultLayout (label "draw"))
  when (drawIndexCount draw < 6) $ bump failed
  when (null (drawCommands draw)) $ bump failed

-- Widget identity is the call site, so every frame must run the same `ui`
-- binding; re-typing the widget on another line makes a different widget.
runOverlayTest :: Context -> IORef Int -> IO ()
runOverlayTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 200 80}
      ui = column defaultLayout (button "Hover" >>= tooltip "tip")
  (_, _, _, _) <- runFrame ctx inp0 ui
  let inp1 =
        inp0
          { inputMousePos = V2 10 10
          , inputMouseDown = False
          , inputMousePressed = False
          , inputMouseReleased = False
          }
  (_, _, draw, _) <- runFrame ctx inp1 ui
  let hasOverlay = any ((== LayerOverlay) . cmdLayer) (drawCommands draw)
  when (not hasOverlay) $ bump failed

runInteractionTest :: Context -> IORef Int -> IO ()
runInteractionTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 200 100}
      ui = column defaultLayout (button "Click")
  -- Frame 1: layout, store prev rects
  _ <- runFrame ctx inp0 ui
  -- Frame 2: press on button
  let inpPress =
        inp0
          { inputMousePos = V2 10 10
          , inputMousePressed = True
          , inputMouseDown = True
          , inputMouseReleased = False
          }
  _ <- runFrame ctx inpPress ui
  -- Frame 3: release => click
  let inpRelease =
        inpPress
          { inputMousePressed = False
          , inputMouseDown = False
          , inputMouseReleased = True
          }
  (_, msgs, _, _) <- runFrame ctx inpRelease ui
  when (null msgs) $ bump failed

-- Hover uses solved layout rects after the first frame stores prev positions.
runHoverTest :: Context -> IORef Int -> IO ()
runHoverTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 200 100}
      ui = column defaultLayout (button "Hover")
  _ <- runFrame ctx inp0 ui
  let inp1 =
        inp0
          { inputMousePos = V2 10 10
          , inputMouseDown = False
          , inputMousePressed = False
          , inputMouseReleased = False
          }
  _ <- runFrame ctx inp1 ui
  hot <- getHotId ctx
  when (hashWidgetId hot == 0) $ bump failed

-- Text input focus is finalized against solved rects on first press.
runTextInputFocusTest :: Context -> IORef Int -> IO ()
runTextInputFocusTest _ failed = do
  ctx <- newTerminalContext
  let inp0 = emptyInput {inputWindowSize = Size 200 100}
      ui = column defaultLayout (textInput "Name" "")
  _ <- runFrame ctx inp0 ui
  ((resp, _), _, _, _) <- runFrame ctx inp0 ui
  let Rect rx ry _ _ = respRect resp
      click = V2 (rx + 1) (ry + 0.5)
  let inp1 =
        inp0
          { inputMousePos = click
          , inputMouseDown = True
          , inputMousePressed = True
          , inputMouseReleased = False
          }
  (_, _, _, _) <- runFrame ctx inp1 ui
  spans <- collectTextSpans ctx
  let hasCursor = any (\(_, txt, _, _) -> T.isInfixOf "\x2502" txt) spans
  when (not hasCursor) $ bump failed

runIdleTest :: Context -> IORef Int -> IO ()
runIdleTest ctx failed = do
  let inp = emptyInput {inputWindowSize = Size 100 100}
  _ <- runFrame ctx inp (label "idle")
  need <- needsRedraw ctx inp inp
  when need $ bump failed

runAnimationIdleTest :: Context -> IORef Int -> IO ()
runAnimationIdleTest ctx failed = do
  let inp = emptyInput {inputWindowSize = Size 100 100, inputDeltaTime = 0.05}
  _ <- runFrame ctx inp (label "anim")
  startAnimation ctx (WidgetId 42) 0 1 0.5
  need <- needsRedraw ctx inp inp
  when (not need) $ bump failed

runAsciiTest :: Context -> IORef Int -> IO ()
runAsciiTest ctx failed = do
  let inp = emptyInput {inputWindowSize = Size 40 10}
  (_, _, draw, _) <-
    runFrame
      ctx
      inp
      (column (defaultLayout {layoutWidth = Grow 1, layoutHeight = Grow 1}) (label "snap"))
  let ascii = renderASCII 40 10 draw
  when (length ascii /= 10) $ bump failed
  when (all (all (== ' ')) ascii) $ bump failed

-- The terminal backend owns its input parsing because vty could not report
-- hover at all, and discarded pending bytes when it met a report it did not
-- recognise, which swallowed clicks. Both properties are pinned here.
runVtTest :: Context -> IORef Int -> IO ()
runVtTest _ failed = do
  let ck cond = when (not cond) (bump failed)
      evs s = fst (decode (BS8.pack s))
      leftover s = snd (decode (BS8.pack s))
  -- Bare motion with no button held: hover. Coordinates are one-based on the
  -- wire and zero-based in the event.
  ck (evs "\ESC[<35;10;5M" == [EvMouse MouseMove 9 4 noMods])
  -- The regression: a click arriving in the same read as a motion report must
  -- still be delivered.
  ck
    ( evs "\ESC[<35;10;5M\ESC[<0;12;6M"
        == [ EvMouse MouseMove 9 4 noMods
           , EvMouse (MousePress BtnLeft) 11 5 noMods
           ]
    )
  -- An unrecognised sequence consumes only itself.
  ck (evs "\ESC[?1;2p\ESC[<0;1;1M" == [EvMouse (MousePress BtnLeft) 0 0 noMods])
  ck (evs "\ESC[<0;12;6m" == [EvMouse (MouseRelease (Just BtnLeft)) 11 5 noMods])
  ck (evs "\ESC[<32;3;4M" == [EvMouse (MouseDrag BtnLeft) 2 3 noMods])
  ck (evs "\ESC[<64;3;4M" == [EvMouse MouseScrollUp 2 3 noMods])
  ck (evs "\ESC[<65;3;4M" == [EvMouse MouseScrollDown 2 3 noMods])
  ck (evs "\ESC[<66;3;4M" == [])
  ck (evs "\ESC[<67;3;4M" == [])
  -- X10 fallback, for terminals that ignore the SGR request.
  ck (evs "\ESC[MC*%" == [EvMouse MouseMove 9 4 noMods])
  ck (evs "\ESC[M *%" == [EvMouse (MousePress BtnLeft) 9 4 noMods])
  ck (evs "\ESC[M#*%" == [EvMouse (MouseRelease Nothing) 9 4 noMods])
  -- A sequence split across reads is held whole, never half-read.
  ck (evs "\ESC[<35;10" == [])
  ck (leftover "\ESC[<35;10" == BS8.pack "\ESC[<35;10")
  -- A lone ESC stays ambiguous until input goes idle.
  ck (evs "\ESC" == [])
  ck (leftover "\ESC" == BS8.pack "\ESC")
  ck (flushPending (BS8.pack "\ESC") == [EvKey KeyEscape noMods])
  ck (evs "\ESC[A" == [EvKey KeyUp noMods])
  ck (evs "\ESCOB" == [EvKey KeyDown noMods])
  ck (evs "\ESC[3~" == [EvKey KeyDelete noMods])
  ck (evs "\ESC[H" == [EvKey KeyHome noMods])
  ck (evs "hi" == [EvChar 'h' noMods, EvChar 'i' noMods])
  ck (evs "\r" == [EvKey KeyEnter noMods])
  ck (evs "\DEL" == [EvKey KeyBackspace noMods])
  -- Multi-byte input decodes as one character.
  ck (evs "\xc3\xa9" == [EvChar '\233' noMods])
  ck (evs "\xc3" == [])

runCellsTest :: Context -> IORef Int -> IO ()
runCellsTest ctx failed = do
  let ck cond = when (not cond) (bump failed)
      inp = emptyInput {inputWindowSize = Size 40 10}
      ui = column (defaultLayout {layoutWidth = Grow 1, layoutHeight = Grow 1}) (label "hello")
  (_, _, draw, _) <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  cells <- rasterize 40 10 draw spans
  let rows = cellRows cells
  ck (length rows == 10)
  ck (any (isInfixOf "hello") rows)
  -- An unchanged frame must produce no output, or hover would repaint the
  -- whole screen on every pointer movement.
  ck (BL.null (toLazyByteString (frameBytes (Just cells) cells)))
  ck (not (BL.null (toLazyByteString (frameBytes Nothing cells))))
  -- Box-drawing and block glyphs used by sliders and the text-input caret
  -- must survive the narrow-char filter.
  ck (narrowChar '\x2502')
  ck (narrowChar '\x2588')
  ck (narrowChar '\x2591')
  ck (not (narrowChar '\x4E00'))
