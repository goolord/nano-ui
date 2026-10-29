{-# OPTIONS_GHC -O0 #-}
module Cases.Modal (tests) where

import Spec
import Data.Maybe (isJust, listToMaybe)
import Data.Text qualified as T
import NanoUI.Internal.Context (setDrawSquareGeometry)
import NanoUI.Testing.Sim

tests :: [Spec]
tests =
  [ simSpec "modal-constant" 800 200 constantUi modalConstant
  , spec "modal-var" runModalVarTest
  , spec "modal-nested-dismiss" runModalNestedDismissTest
  , simSpec "modal-over-inner-window" 500 400 innerWindowUi modalOverInnerWindow
  , spec "modal-fractional-scale-no-scroll" runModalFractionalScaleNoScrollTest
  , spec "modal-fill-label-fits" runModalFillLabelFitsTest
  , spec "modal-tree-selection" runModalTreeSelectionTest
  ]

-- | Whether the frame's Escape went to an overlay rather than quitting.
escapeTaken :: Sim a Bool
escapeTaken = onContext (`overlayConsumesQuit` keyInp KeyEscape emptyInput)

sentence :: T.Text
sentence = "A sentence that is wider than the smallest modal allows."

constantUi :: NanoUI (Response, Maybe Response)
constantUi = column $ do
  _ <- button "Outside"
  modal' (constVar True) "Title" $ do
    r <- button' "Inside"
    muted sentence
    r <$ forM_ [1 .. 40 :: Int] (\i -> label (T.pack ("Row " <> show i)))

-- | A modal on a constant fits the window but widens for a label, has no
-- close button, and reports a press beside it and Escape while staying open.
modalConstant :: Sim (Response, Maybe Response) ()
modalConstant = do
  (dlg, _) <- result
  check (rectH (respRect dlg) <= 200)
  whole <- (\ss -> [r | (r, t, _, _, _) <- ss, t == sentence]) <$> spans
  checkEq [True] (map ((< rectW (respRect dlg) + 0.5) . rectW) whole)
  checkEq Nothing =<< findSpan "X"
  check . maybe False respClicked . snd =<< click "Inside"
  check . respClicked . fst =<< press (At (V2 4 4))
  _ <- release
  check . respClicked . fst =<< pressKey KeyEscape
  check =<< escapeTaken
  expect "Title"

-- | A modal on a Var closes itself (the Escape frame already draws it closed),
-- and opening and closing each repaint the whole window once.
runModalVarTest :: Context -> IORef Int -> IO ()
runModalVarTest ctx failed = do
  open <- newVar False
  let ui = column $ do
        whenM (button "Open") (writeVar open True)
        modal open "Title" (whenM (button "Close") (writeVar open False))
      damage = frameDamage
  runSim ctx failed (withInputOff 320 240) ui $ do
    _ <- pressKey KeyEscape
    checkEq False =<< escapeTaken
    _ <- click "Open"
    expect "Title"
    _ <- click "Close"
    checkEq False =<< readV open
    _ <- damage >> click "Open"
    checkEq DamageFull =<< damage
    -- The idle frame after settles the button's release, as a clip.
    check . (\d -> d /= DamageFull && not (damageIsEmpty d)) =<< (advance 1 >> damage)
    _ <- pressKey KeyEscape
    checkEq False =<< readV open
    checkEq 2 =<< onContext lastFramePasses
    check =<< escapeTaken
    expectNot "Title"
    checkEq DamageFull =<< damage
    -- The second pass no longer built it, so the next frame repaints no more.
    _ <- frame
    check . (/= DamageFull) =<< damage

-- | A newly opened modal closes the edit menu it hides, and a modal declared in
-- another's body closes first, on Escape or a click on the outer one.
runModalNestedDismissTest :: Context -> IORef Int -> IO ()
runModalNestedDismissTest ctx failed = do
  outer <- newVar False
  inner <- newVar True
  let ui = column $ do
        field <- fst <$> withValue "text" textInput'
        (dlg, _) <- modal' outer "Settings" $ do
          forM_ [1 .. 10 :: Int] (\i -> label (T.pack ("Setting " <> show i)))
          modal inner "Confirm" (label "confirm body")
        pure (field, dlg)
      states = (,) <$> readV outer <*> readV inner
  runSim ctx failed (withInput 640 480) ui $ do
    _ <- rightClick . On . fst =<< result
    expect "Paste"
    _ <- writeV outer True >> settle
    _ <- pressKey KeyEscape
    checkEq (True, False) =<< states
    Rect dx dy dw _ <- respRect . snd <$> (writeV inner True >> settle)
    _ <- click (At (V2 (dx + dw / 2) (dy + 8)))
    checkEq (True, False) =<< states

innerWindowUi :: NanoUI (Maybe (Response, Maybe Response))
innerWindowUi = fmap snd . modalWith' (fixedWH 460 380) (constVar True) "Settings" $ do
  (_, hidden) <- window' (constVar True) "Inspector" (buttonWith' (fixedWH 420 200) "Hidden")
  ok <- button' "OK"
  pure (ok, hidden)

-- | Where the modal panel covers a window in its body, clicks, title-bar drags
-- and edge drags (and the resize cursor) go to the modal, not the window.
modalOverInnerWindow :: Sim (Maybe (Response, Maybe Response)) ()
modalOverInnerWindow = do
  Just (ok, Just hidden) <- result
  check (isJust (rectIntersect (respRect hidden) (respRect ok)))
  checkEq (Just (True, False)) . fmap (\(o, h) -> (respClicked o, any respClicked h)) =<< click (On ok)
  let windows = map snd <$> onContext (nodesOfType NodeWindow)
  before@[Rect x y w h] <- windows
  _ <- drag (At (V2 (x + w / 2) (y + 10))) (At (V2 (x + w / 2 - 40) (y + 40))) 1
  checkEq before =<< windows
  let edge = V2 (x + 3) (y + h / 2)
  check . (/= UiCursorEwResize) =<< cursorAt (At edge)
  _ <- drag (At edge) (At (V2 (x - 37) (y + h / 2))) 1
  checkEq before =<< windows

-- At fractional display scales, fixed-size content must fit a content-sized
-- modal without accumulating rounding error through nested containers.
runModalFractionalScaleNoScrollTest :: Context -> IORef Int -> IO ()
runModalFractionalScaleNoScrollTest _ failed = forM_ cases $ \(s, n, nested) -> do
  let field i = rowWith (fillW . fixedH 30 . alignMid) (label (T.pack ("Field " <> show i)))
      section = columnWith (fillW . gap 6) $ do
        label "Section"
        columnWith (fillW . gap 0) (spacer Fit (Fixed 3) >> forM_ [1 .. n] field >> spacer Fit (Fixed 3))
      body = columnWith (gap 10) (section >> section >> section >> button "Close")
      dialog = fst <$> modal' (constVar True) "Details" body
      -- As the arena's root, and inside other content as an app opens one.
      ui = if nested then column (label "Behind" >> dialog) else dialog
      scrolls winH = do
        ctx <- (`withFontMetrics` (monospaceMetrics 12) {fmSnapScale = s}) <$> newContext
        runSim ctx failed (withInputOff 1000 winH) ui $ do
          y0 <- rectOf "Field 1"
          dlg <- result
          _ <- wheel (On dlg) (V2 0 3)
          (/= Just (rectY y0)) . fmap rectY <$> findSpan "Field 1"
  assertEq failed False =<< scrolls 1000
  -- The same body in a short window does scroll, so the check can fail.
  assertEq failed True =<< scrolls 300
  where
    cases = [(s, n, nested) | s <- [1, 1.25, 1.5, 1.75], n <- [4 .. 8 :: Int], nested <- [False, True]]

-- | A modal measures each label in its requested font size and keeps the last
-- row visible when the measured body fits.
runModalFillLabelFitsTest :: Context -> IORef Int -> IO ()
runModalFillLabelFitsTest _ failed = forM_ [12, 17] $ \base -> do
  ctx <- (`withFontMetrics` monospaceMetrics base) <$> newContext
  -- Wide enough for the label on one line in its own font, not the base one.
  let body = columnWith (gap 14 . minW 560 . \l -> l {layoutPadding = Padding 0 0 0 0}) $ do
        labelWith (tight . fillW) "How cabal should log in to Hackage for uploads:"
        let logins = ["cabal's config file (no login found in it)", "A username and password", "An API token"]
        _ <- withValue (0 :: Int) (radio logins)
        labelWith (tight . fillW . fontSize 14) $
          "Kept in memory for this session only. The password goes to cabal on its standard input."
        separator
        rowWith (fillW . gap 8 . alignMid . tight) (flex >> button "Cancel" >> button' "Use this login")
  runSim ctx failed (withInput 2000 800) (modal' (constVar True) "Hackage login" body) $ do
    (dlg, mOk) <- result
    y0 <- rectY <$> rectOf "An API token"
    _ <- wheel (On dlg) (V2 0 3)
    checkEq y0 . rectY =<< rectOf "An API token"
    let bottom r = rectY (respRect r) + rectH (respRect r)
    checkJust mOk $ \ok -> checkGt (bottom dlg + 0.5) (bottom ok)

-- | A tree in a modal draws its selected row apart, as one on the page does.
runModalTreeSelectionTest :: Context -> IORef Int -> IO ()
runModalTreeSelectionTest ctx failed = do
  setDrawSquareGeometry ctx True
  let inp0 = withInput 320 200
      items = [TreeItem "alpha" [], TreeItem "beta" []]
      -- The last fill under a row's whole label is that row's background.
      rowFills sel = do
        (_, draw) <- warmupDraw ctx inp0 (modal' (constVar True) "Pick" (tree' "t" items (constVar sel)))
        quads <- drawQuads draw
        spans' <- collectOverlayTextSpans ctx inp0
        let under lbl = spanRectOf lbl spans' >>= \r -> listToMaybe [c | (q, c) <- reverse quads, covers q r]
        pure (under "alpha", under "beta")
  (alpha0, beta0) <- rowFills 0
  (alpha1, beta1) <- rowFills 1
  assert failed (isJust alpha0 && isJust beta0)
  assert failed (alpha0 /= alpha1 && beta0 /= beta1)
