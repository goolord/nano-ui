-- | The SDL demo on a hidden window, driven through its tabs and main widget
-- interactions. With @continuous@ set it presents direct to the window.
module DemoSelftest
  ( selftest
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Data.Char (isDigit)
import Data.Foldable (for_)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (sortOn)
import Data.Text qualified as T
import Demo qualified
import DemoApp (withHiddenWindow)
import Drive
import NanoUI
import NanoUI.Backend
import NanoUI.Backend.Sdl
import NanoUI.Testing.Harness (findRightmost, spanCenter, spanLabel)

selftest :: Bool -> IO ()
selftest continuous = do
  state <- Demo.newDemo
  sampled <- newIORef 0
  -- The debug readout's draws counter, as the backend last refreshed it.
  let view = do
        snapshot <- askSdlDebug
        liftIO (mapM_ (writeIORef sampled . dbgPresents . dbgCore) snapshot)
        Demo.demoUi state
  withHiddenWindow 1280 800 (V2 640 400) (\o -> o {sdlAppContinuous = continuous}) $ \ctx0 env idle -> do
    (ctx, base) <- syncDisplay ctx0 env idle
    _ <- sdlDrawFrame ctx view env base True
    runDrive ctx env base view $ do
      expect "Mute"
      expect "Live widget values"
      -- A field value wider than the glyph atlas draws glyph by glyph, and
      -- the text drawn before and after it in the frame survives.
      before <- length <$> spans
      clickAt . nudge 80 0 =<< rightmost "Name"
      typeText (T.replicate 400 "f")
      expect "Mute"
      after <- length <$> spans
      check "long field text collapsed the span set" (after >= before - 1)
      click "Table"
      expect "David"
      clickAt =<< header "Name"
      expect "descending"
      expect "(desc)"
      dept <- header "Dept"
      dragTo dept (nudge 180 0 dept)
      expect "Sonia"
      click "List"
      click "README.md"
      expect "Selected item 7"
      click "Typography"
      expect "Live Playground"
      size <- rightmost "Size"
      wheelAt (V2 640 400) (V2 0 (-350))
      for_ [20, 60, 100, 140, 180, 50, 120, -60, -100, 0] $ \dx -> dragTo size (nudge dx 0 size)
      expect "Live Playground"
      -- The Graphics tab decodes its GIF in a useTask worker. The GIF is
      -- below the fold, so each wait scrolls a notch over the tab's right
      -- side, away from the image viewer (which takes wheel events).
      click "Graphics"
      let awaitGif :: Int -> Drive ()
          awaitGif n = do
            loaded <- any (\(_, t, _, _, _) -> spanLabel t == "lick.gif") <$> spans
            unless loaded $ do
              check "the Graphics tab's GIF never loaded" (n > 0)
              liftIO (threadDelay 20000)
              frameWith (\i -> i {inputMousePos = V2 1100 400, inputScroll = V2 0 1})
              awaitGif (n - 1)
      awaitGif 250
      wheelAt (V2 1100 400) (V2 0 (-1000))
      click "Diagnostics"
      expect "Present FPS"
      click "Panes"
      panes
      click "Controls"
      mute <- rightmost "Mute"
      clickAt mute
      expect "muted"
      clickAt mute
      expectNot "muted"
      -- Theme is a radio fieldset whose options all stay visible.
      click "Tomorrow Light"
      theme <- liftIO (getTheme ctx)
      check "the context theme was not updated to Tomorrow Light" (theme == tomorrowMinLightTheme)
      -- "Follow system" picks the default theme's light or dark version per
      -- SDL's reported desktop appearance.
      click "Follow system"
      appearance <- liftIO (getSystemAppearance ctx)
      followed <- liftIO (getTheme ctx)
      check "following the system did not pick the default theme" $
        followed == lightDark defaultLightTheme defaultTheme appearance
      click "Tomorrow Light"
      clickAt . nudge 80 0 =<< rightmost "Volume"
      click "About"
      expect "Immediate-mode"
      -- Font metrics can make the body overflow the modal's viewport; the
      -- footer must be reachable by scrolling.
      closeShown <- any (\(_, t, _, _, _) -> "Close" `T.isInfixOf` t) <$> overlay
      unless closeShown $ (`wheelAt` V2 0 10) =<< find "nano-ui"
      expect "Close"
      pressKey KeyEscape
      expectNot "Immediate-mode"
      -- F1 is the About button's shortcut.
      pressKey (KeyF 1)
      expect "Immediate-mode"
      pressKey KeyEscape
      click "Debug"
      expect "Frame"
      expect "Runtime"
      -- Formatted rows may be reused between samples, but must track the
      -- next backend refresh rather than freezing the first snapshot.
      frame
      previous <- liftIO (readIORef sampled)
      liftIO (threadDelay 300000)
      frame
      current <- liftIO (readIORef sampled)
      check "the Debug draws counter did not advance" (current > previous)
      _ <- find (T.pack (show current))
      pure ()
  putStrLn ("demo selftest" <> (if continuous then " (continuous)" else "") <> ": ok")
  where
    rightmost txt = maybe (fail ("no text " <> show txt)) pure . findRightmost txt =<< spans
    -- A table header: the span labelled @txt@ on the "Age" header's row.
    header txt = do
      ss <- spans
      let at t = [r | (r, s, _, _, _) <- ss, spanLabel s == t]
      case [spanCenter r | r <- at txt, any ((== rectY r) . rectY) (at "Age")] of
        p : _ -> pure p
        [] -> fail ("no header " <> show txt)

nudge :: Float -> Float -> V2 -> V2
nudge dx dy (V2 x y) = V2 (x + dx) (y + dy)

-- | The pane grid: split, maximise, restore, close, and three drops.
panes :: Drive ()
panes = do
  let -- Pane titles, left to right.
      titles = do
        ss <- spans
        pure $
          sortOn
            (rectX . fst)
            [ (r, l)
            | (r, txt, _, _, _) <- ss
            , let l = spanLabel txt
            , Just rest <- [T.stripPrefix "Pane " l]
            , not (T.null rest)
            , T.all isDigit (T.takeWhile (/= ' ') rest)
            ]
      count n = titles >>= \ts -> check ("expected " <> show n <> " panes, saw " <> show ts) (length ts == n)
      centre = spanCenter . fst
  count 1
  click "+"
  count 2
  click "M"
  expect "maximized"
  click "R"
  count 2
  click "x"
  count 1
  click "+"
  [left, right] <- titles
  -- An edge drop: the right pane dropped on the left pane's left edge
  -- becomes the leftmost.
  dragTo (centre right) (V2 (rectX (fst left) - 10) (rectY (fst left) + 150))
  [left', right'] <- titles
  check "the edge drop did not land on the left side" (snd left' /= snd left)
  -- A centre drop on the other pane swaps the two.
  let halfGap = (rectX (fst right') - rectX (fst left')) / 2
  dragTo (centre left') (V2 (rectX (fst right') + halfGap) (rectY (fst right') + 160))
  [swapped, _] <- titles
  check "the pane drag did not swap the panes" (snd swapped /= snd left')
  -- A drop of the middle of three panes on the grid's outer left edge wraps
  -- the grid at the top level: one pane on the left half and two side by
  -- side on the right, so the first gap (~half) exceeds the second
  -- (~quarter), where a flat third column keeps them equal.
  click "+"
  [l3, m3, _] <- titles
  dragTo (nudge 70 100 (centre m3)) (V2 (rectX (fst l3) - 10) (rectY (fst l3) + 150))
  [a, b, c] <- titles
  check "the top-level drop did not move the middle pane left" (snd a == snd m3)
  check "the top-level drop did not collapse the others onto one side" $
    rectX (fst b) - rectX (fst a) > rectX (fst c) - rectX (fst b) + 10
  -- Without headers the titles go, but the panes and their content stay.
  click "Pane headers"
  count 0
  expect "Contents of"
