module Cases.Terminal
  ( runTerminalButtonBracketTest
  , runTerminalCloseButtonTest
  , runTerminalDefaultGapTest
  , runTerminalIconChromeTest
  , runTerminalIconCloseTest
  , runTerminalModalOpenRedrawTest
  , runTerminalModalOverlayTest
  , runTerminalModalScrollTest
  , runTerminalModalTightTest
  , runTerminalSeparatorSpanTest
  , runTerminalSliderTrackTest
  , runTerminalTextInputDisplayTest
  , runTerminalThemeContrastTest
  , runTerminalWideClearBracketTest
  , runTerminalWideCursorCupTest
  , runTerminalWidePairTest
  , runTerminalWideTransitionTest
  , runTerminalWindowDragIconTest
  , runTerminalWindowDragTest
  , runTerminalWindowOverlayTest
  ) where

import Control.Monad (forM_, void)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy qualified as BL
import Data.IORef (IORef)
import Data.List (isInfixOf, sort)
import Data.Text qualified as T
import NanoUI
import NanoUI.Testing
import NanoUI.Testing.Assert (assert, assertEq, assertLt, withInput)
import NanoUI.Testing.Harness
  ( assertWheelTitlePinned
  , clickPair
  , closeSpanBottom
  , closeSpanCenter
  , closeSpanPos
  , closeSpanStart
  , runDragFrom
  , spanLabelYs
  , terminalAboutModalMaxFooter
  , terminalAboutModalMaxH
  , warmup2
  , warmupDraw
  , withInputOff
  )
import NanoUI.Testing.Term

runTerminalDefaultGapTest :: Context -> IORef Int -> IO ()
runTerminalDefaultGapTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let fm = ctxFontMetrics ctx
      expectedStep = fmLineHeight fm + resolveLayoutGap (ctxHostProfile ctx) fm (layoutGap defaultLayout)
      inp = withInput 20 10
      ui = column (label "A" >> label "B" >> pure ())
  _ <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  let ys = sort [y | (Rect _ y _ _, txt, _, _, _) <- spans, txt == "A" || txt == "B"]
  case ys of
    yA : yB : _ -> assert failed (yB - yA <= expectedStep + 0.25)
    _ -> assert failed False

runTerminalSliderTrackTest :: Context -> IORef Int -> IO ()
runTerminalSliderTrackTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let inp0 = withInput 60 10
      ui = columnWith fillW (slider "Vol" 0 100 0)
  (resp, _) <- warmup2 ctx inp0 ui
  let Rect rx ry rw rh = respRect resp
      fm = ctxFontMetrics ctx
      track = sliderTrackBounds (ctxHostProfile ctx) fm "Vol" rx ry rw rh
  assert failed (rectW track < rw - 1 && rectW track >= 10)
  let endDrag = inp0 {inputMousePos = V2 (rectX track + rectW track - 0.5) (rectY track + rectH track / 2), inputMouseDown = True, inputMousePressed = True}
  ((_, val), _, _, _) <- runFrame ctx endDrag ui
  assert failed (val >= 90)

runTerminalModalOverlayTest :: Context -> IORef Int -> IO ()
runTerminalModalOverlayTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let inp0 = withInput 80 24
      ui = column $ do
        _ <- label "Behind"
        (dlg, _) <- modal True "About" $ do
          heading "nano-ui"
          muted "Immediate-mode GUI for Haskell."
          muted "Terminal backend demo."
          row' (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fit}) (flex >> clickButton "Close" (pure ()))
          pure ()
        pure dlg
  (dlg, drawData) <- warmupDraw ctx inp0 ui
  overlays <- collectOverlayTextSpans ctx inp0
  base <- collectTextSpans ctx
  let hasTitle = any (\(_, txt, _, _, _) -> "About" `T.isInfixOf` txt) overlays
      hasBody = any (\(_, txt, _, _, _) -> "Immediate-mode" `T.isInfixOf` txt) overlays
      hasClose = any (\(_, txt, _, _, _) -> "Close" `T.isInfixOf` txt) overlays
      inGrid (Rect x y w h, _, _, _, _) = x >= -0.5 && y >= -0.5 && x + w <= 80.5 && y + h <= 24.5
  assert failed (hasTitle && hasBody && hasClose)
  assert failed (all inGrid overlays)
  let Rect _ _ mw mh = respRect dlg
  assert failed (mw <= 80 && mh <= 24 && mw >= 8 && mh >= 2)
  cells <- rasterizeLayered 80 24 drawData base overlays
  let blob = concat (cellRows cells)
  assert failed ("About" `isInfixOf` blob && "Immediate-mode" `isInfixOf` blob && "Behind" `isInfixOf` blob && '\x2500' `elem` blob)
  assert failed (any (\c -> cmdTextureId c == backdropDimTextureId) (drawCmdElems drawData))

runTerminalModalScrollTest :: Context -> IORef Int -> IO ()
runTerminalModalScrollTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let inp0 = withInput 80 16
      line1 = T.pack "line 1"
      title = T.pack "About"
      ui = fmap fst $ modal True "About" $
             column (mapM_ (\i -> label (T.pack ("line " <> show (i :: Int)))) [1 .. 24])
  dlg <- warmup2 ctx inp0 ui
  let Rect mx _ mw mh = respRect dlg
  assert failed (mw > 0 && mh > 0 && mh <= 16)
  spans0 <- collectOverlayTextSpans ctx inp0
  case spanLabelYs line1 spans0 of
    [] -> assert failed False
    b0 : _ -> assertWheelTitlePinned failed ctx inp0 ui title line1 (V2 (mx + mw / 2) (b0 + 0.5)) (Just 16.5)

runTerminalModalTightTest :: Context -> IORef Int -> IO ()
runTerminalModalTightTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let inp0 = withInput 80 24
      ui = fmap fst $ modal True "About" $ do
        heading "nano-ui"
        muted "Immediate-mode GUI for Haskell."
        muted "Terminal backend demo."
        row' (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fit}) (flex >> clickButton "Close" (pure ()))
        pure ()
  (dlg, _, _, _) <- runFrame ctx inp0 ui >>= \_ -> runFrame ctx inp0 ui
  overlays <- collectOverlayTextSpans ctx inp0
  let fm = ctxFontMetrics ctx
      Rect _ my _ mh = respRect dlg
      maxH = terminalAboutModalMaxH (ctxHostProfile ctx) fm
      maxFooter = terminalAboutModalMaxFooter (ctxHostProfile ctx) fm
  case closeSpanBottom overlays of
    Nothing -> assert failed False
    Just bottom -> do
      let footer = my + mh - bottom
      assert failed (mh <= maxH && footer <= maxFooter)

runTerminalModalOpenRedrawTest :: Context -> IORef Int -> IO ()
runTerminalModalOpenRedrawTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let inp0 = withInputOff 80 24
      ui = do
        (open, setOpen) <- useFlag False
        resp <- button "Open"
        onClick resp (setOpen True)
        _ <- modal open "About" (label "body")
        pure resp
  _ <- runFrame ctx inp0 ui
  (resp, _, _, _) <- runFrame ctx inp0 ui
  let (press, release) = clickPair inp0 (V2 (rectX (respRect resp) + rectW (respRect resp) / 2) (rectY (respRect resp) + rectH (respRect resp) / 2))
  _ <- runFrame ctx press ui
  _ <- runFrame ctx release ui
  let idle = inp0 {inputDeltaTime = 0}
  need <- needsRedrawIdle ctx release idle
  assert failed need
  _ <- runFrame ctx idle ui
  overlays <- collectOverlayTextSpans ctx idle
  assert failed (any (\(_, txt, _, _, _) -> "About" `T.isInfixOf` txt) overlays)
  dmg <- takeDamage ctx
  assert failed (not (damageIsEmpty dmg))

runTerminalWindowOverlayTest :: Context -> IORef Int -> IO ()
runTerminalWindowOverlayTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let inp0 = withInput 80 24
      ui = window True "Debug" (label "Floating window overlay." >> pure ())
  ((win, _), _, drawData, _) <- runFrame ctx inp0 ui >>= \_ -> runFrame ctx inp0 ui
  overlays <- collectOverlayTextSpans ctx inp0
  base <- collectTextSpans ctx
  let hasTitle = any (\(_, txt, _, _, _) -> "Debug" `T.isInfixOf` txt) overlays
      hasBody = any (\(_, txt, _, _, _) -> "Floating window" `T.isInfixOf` txt) overlays
      inGrid (Rect x y w h, _, _, _, _) = x >= -0.5 && y >= -0.5 && x + w <= 80.5 && y + h <= 24.5
  assert failed (hasTitle && hasBody && all inGrid overlays)
  let Rect _ _ ww wh = respRect win
  assert failed (ww <= 80 && wh <= 24 && ww >= 8 && wh >= 2)
  cells <- rasterizeLayered 80 24 drawData base overlays
  let blob = concat (cellRows cells)
  assert failed ("Debug" `isInfixOf` blob && "Floating window" `isInfixOf` blob)

runTerminalWindowDragTest :: Context -> IORef Int -> IO ()
runTerminalWindowDragTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let inp0 = withInput 80 24
      ui = fmap fst (window True "Debug" (label "Body"))
  win0 <- warmup2 ctx inp0 ui
  let Rect x0 y0 _ _ = respRect win0
      dest = V2 (x0 + 4 - 8) (y0 + 0.5 + 4)
  runDragFrom ctx inp0 ui (V2 (x0 + 4) (y0 + 0.5)) dest
  (win1, _, _, _) <- runFrame ctx (inp0 {inputMousePos = dest}) ui
  let Rect x1 y1 _ _ = respRect win1
  assert failed (x1 < x0 - 2)
  assert failed (y1 > y0 + 1)

runTerminalWindowDragIconTest :: Context -> IORef Int -> IO ()
runTerminalWindowDragIconTest _ failed = do
  term <- newAdaptiveTerminalContext
  let ctx = withIcons term IconsNerd
      inp0 = withInput 80 24
      ui = fmap fst (window True "Debug" (label "Body"))
  _ <- runFrame ctx inp0 ui
  (_, _, draw0, _) <- runFrame ctx inp0 ui
  overlays0 <- collectOverlayTextSpans ctx inp0
  (win0, _, _, _) <- runFrame ctx inp0 ui
  let Rect x0 y0 _ _ = respRect win0
      grab = V2 (x0 + 4) (y0 + 0.5)
      press = inp0 {inputMousePos = grab, inputMouseDown = True, inputMousePressed = True}
  _ <- runFrame ctx press ui
  let moved = press {inputMousePos = V2 (x0 + 4 - 10) (y0 + 0.5 + 4), inputMousePressed = False}
  (_, _, draw1, _) <- runFrame ctx moved ui
  overlays1 <- collectOverlayTextSpans ctx moved
  let Size tw th = inputWindowSize inp0
  cells0 <- rasterizeLayered (round tw) (round th) draw0 [] overlays0
  cells1 <- rasterizeLayered (round tw) (round th) draw1 [] overlays1
  case closeSpanPos overlays0 of
    Nothing -> assert failed False
    Just (closeCol, rowY) -> do
      let maxX = case cellRows cells1 of (r : _) -> length r - 1; [] -> -1
          trailGhost = any (\x -> x >= 0 && x <= maxX && rowY >= 0 && rowY < cellsH cells1 && let c1 = cellChar cells1 x rowY in fontAwesomeIcon c1 || c1 == wideTrailChar) [closeCol, closeCol + 1]
      case closeSpanStart overlays1 of
        Nothing -> assert failed False
        Just x1 -> do
          assertLt failed x1 (closeCol - 2)
          assert failed (not trailGhost)
  let bytes = toLazyByteString (frameBytes (Just cells0) cells1)
  assert failed (not (BL.null bytes))

runTerminalButtonBracketTest :: Context -> IORef Int -> IO ()
runTerminalButtonBracketTest _ failed = do
  term <- newAdaptiveTerminalContext
  let ctx = withIcons term IconsNerd
      inp0 = withInput 50 20
      ui = fmap fst $ modal True "Long modal title for clip" $ do
        row (void (button "OK") >> void (button "Cancel"))
        label_ "Body"
      Size tw th = inputWindowSize inp0
  _ <- runFrame ctx inp0 ui
  (_, _, draw0, _) <- runFrame ctx inp0 ui
  overlays0 <- collectOverlayTextSpans ctx inp0
  assert failed (not (null (terminalBracketSpans overlays0)))
  cells0 <- rasterizeLayered (round tw) (round th) draw0 [] overlays0
  assert failed (terminalBracketsOk cells0 overlays0)
  case closeSpanPos overlays0 of
    Nothing -> assert failed False
    Just (closeCol, closeY) -> do
      let hover = inp0 {inputMousePos = V2 (fromIntegral closeCol + 0.5) (fromIntegral closeY + 0.5), inputMouseDown = False}
      (_, _, draw1, _) <- runFrame ctx hover ui
      overlays1 <- collectOverlayTextSpans ctx hover
      cells1 <- rasterizeLayered (round tw) (round th) draw1 [] overlays1
      assert failed (terminalBracketsOk cells1 overlays1)
  let pageUi = column $ row $ do
        void (button "OK")
        void (button "Cancel")
        void (checkbox "Feature" False)
  _ <- runFrame ctx inp0 pageUi
  (_, _, drawP, _) <- runFrame ctx inp0 pageUi
  baseP <- collectTextSpans ctx
  cellsP <- rasterizeLayered (round tw) (round th) drawP baseP []
  assert failed (not (null (terminalBracketSpans baseP)))
  assert failed (terminalBracketsOk cellsP baseP)

runTerminalWideClearBracketTest :: Context -> IORef Int -> IO ()
runTerminalWideClearBracketTest ctx failed = do
  let inp = withInput 2 1
      clip = Rect 0 0 2 1
      fg = colorRGBA 220 220 220 255
      bg = colorRGBA 20 20 24 255
  (_, _, draw, _) <- runFrame ctx inp (column (pure ()))
  cellsA <- rasterize 2 1 draw [(clip, "[O", fg, bg, clip)]
  cellsB <- rasterize 2 1 draw [(clip, iconClose glyphIcons, fg, bg, clip)]
  let bytes = toLazyByteString (frameBytes (Just cellsA) cellsB)
      packed = BL.unpack bytes
  assertEq failed (cellChar cellsA 0 0) '['
  assertEq failed (cellChar cellsA 1 0) 'O'
  assert failed (fontAwesomeIcon (cellChar cellsB 0 0))
  assertEq failed (cellChar cellsB 1 0) wideTrailChar
  assert failed (not (BL.null bytes))
  assert failed (0x20 `elem` packed)
  assert failed (BS.isInfixOf "\ESC[1;1H" (BL.toStrict bytes))

runTerminalWideCursorCupTest :: Context -> IORef Int -> IO ()
runTerminalWideCursorCupTest ctx failed = do
  let inp = withInput 4 1
      clip = Rect 0 0 4 1
      fg = colorRGBA 220 220 220 255
      bg = colorRGBA 20 20 24 255
  (_, _, draw, _) <- runFrame ctx inp (column (pure ()))
  cells <- rasterize 4 1 draw [(Rect 0 0 2 1, iconClose glyphIcons, fg, bg, clip), (Rect 2 0 2 1, "[O", fg, bg, clip)]
  assert failed (fontAwesomeIcon (cellChar cells 0 0))
  assertEq failed (cellChar cells 1 0) wideTrailChar
  assertEq failed (cellChar cells 2 0) '['
  let bytes = BL.toStrict (toLazyByteString (frameBytes Nothing cells))
  assert failed (BS.isInfixOf "\ESC[1;3H" bytes)

runTerminalWideTransitionTest :: Context -> IORef Int -> IO ()
runTerminalWideTransitionTest _ failed = do
  term <- newAdaptiveTerminalContext
  let ctx = withIcons term IconsNerd
      inp0 = withInput 80 24
      page = void (button "OK") >> void (button "Cancel") >> void (checkbox "Feature" False)
      windowUi = page >> fmap fst (window True "Debug" (label_ "Body"))
      modalUi open = page >> fmap fst (modal open "About" (label_ "Body"))
      Size tw th = inputWindowSize inp0
  _ <- runFrame ctx inp0 page
  (_, _, draw0, _) <- runFrame ctx inp0 page
  base0 <- collectTextSpans ctx
  cells0 <- rasterizeLayered (round tw) (round th) draw0 base0 []
  assert failed (terminalPairsOk cells0 base0 && terminalBracketsOk cells0 base0)
  case terminalBracketSpans base0 of
    [] -> assert failed False
    (Rect bx by _ _ : _) -> do
      let hover = inp0 {inputMousePos = V2 (fromIntegral (round bx :: Int) + 1.5) (fromIntegral (round by :: Int) + 0.5)}
      (_, _, drawH, _) <- runFrame ctx hover page
      baseH <- collectTextSpans ctx
      cellsHov <- rasterizeLayered (round tw) (round th) drawH baseH []
      assert failed (terminalPairsOk cellsHov baseH && terminalBracketsOk cellsHov baseH)
      assertEq failed (cellChar cellsHov (round bx) (round by)) '['
  _ <- runFrame ctx inp0 (void (modalUi False))
  (_, _, drawC, _) <- runFrame ctx inp0 (void (modalUi False))
  (baseC, overC) <- collectRasterSpans ctx inp0
  cellsC <- rasterizeLayered (round tw) (round th) drawC baseC overC
  _ <- runFrame ctx inp0 (void (modalUi True))
  (_, _, drawM, _) <- runFrame ctx inp0 (void (modalUi True))
  (baseM, overM) <- collectRasterSpans ctx inp0
  cellsM <- rasterizeLayered (round tw) (round th) drawM baseM overM
  assert failed (terminalPairsOk cellsM (baseM ++ overM))
  assert failed (closeSpanPos overM /= Nothing)
  assert failed (terminalBracketsOk cellsC baseC)
  _ <- warmup2 ctx inp0 windowUi
  overW0 <- collectOverlayTextSpans ctx inp0
  (win0, _, _, _) <- runFrame ctx inp0 windowUi
  let Rect wx wy _ _ = respRect win0
      press = inp0 {inputMousePos = V2 (wx + 4) (wy + 0.5), inputMouseDown = True, inputMousePressed = True}
  _ <- runFrame ctx press windowUi
  let moved = press {inputMousePos = V2 (wx + 4 - 10) (wy + 0.5 + 4), inputMousePressed = False}
  (_, _, drawW1, _) <- runFrame ctx moved windowUi
  overW1 <- collectOverlayTextSpans ctx moved
  cellsW1 <- rasterizeLayered (round tw) (round th) drawW1 [] overW1
  assert failed (terminalPairsOk cellsW1 overW1)
  case (closeSpanPos overW0, closeSpanStart overW1) of
    (Just (cx, cy), Just x1) -> do
      assertLt failed x1 (cx - 2)
      let leftover = any (\x -> x >= 0 && x < terminalGridW cellsW1 && cy >= 0 && cy < cellsH cellsW1 && let c1 = cellChar cellsW1 x cy in fontAwesomeIcon c1 || c1 == wideTrailChar) [cx, cx + 1]
      assert failed (not leftover)
    _ -> assert failed False

runTerminalWidePairTest :: Context -> IORef Int -> IO ()
runTerminalWidePairTest _ failed = do
  term <- newAdaptiveTerminalContext
  let ctx = withIcons term IconsNerd
      inp = withInput 80 24
      ui = button "OK" >> button "Cancel" >> checkbox "Feature" False >> fmap fst (window True "Debug" (label_ "Body"))
  _ <- runFrame ctx inp ui
  (_, _, draw, _) <- runFrame ctx inp ui
  (base, overlay) <- collectRasterSpans ctx inp
  cells <- rasterizeLayered 80 24 draw base overlay
  assert failed (terminalPairsOk cells (base ++ overlay))

runTerminalCloseButtonTest :: Context -> IORef Int -> IO ()
runTerminalCloseButtonTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let inp0 = withInput 80 24
      modalUi = column (fmap fst (modal True "About" (label_ "Body")))
      windowUi = fmap fst (window True "Debug" (label_ "Body"))
      testClose ui = do
        _ <- warmup2 ctx inp0 ui
        overlays <- collectOverlayTextSpans ctx inp0
        case closeSpanCenter overlays of
          Nothing -> assert failed False
          Just (V2 cx cy) -> do
            let (press, release) = clickPair inp0 (V2 (cx - 1.0) cy)
            _ <- runFrame ctx press ui
            (outer, _, _, _) <- runFrame ctx release ui
            assert failed (respClicked outer)
  testClose modalUi
  testClose windowUi

runTerminalIconChromeTest :: Context -> IORef Int -> IO ()
runTerminalIconChromeTest _ failed = do
  term <- newAdaptiveTerminalContext
  let ctx = withIcons term IconsNerd
      inp = withInput 40 30
      rows = columnWith fillW (forM_ [1 .. 40 :: Int] (label_ . T.pack . show))
      ui = columnWith fillW $ do
        _ <- checkbox "Feature" False
        _ <- select "Quality" ["Low", "High"] 0
        scrollArea ((fillW defaultLayout) {layoutHeight = Fixed 20}) rows
  _ <- runFrame ctx inp ui
  (_, _, drawData, _) <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  let texts = [txt | (_, txt, _, _, _) <- spans]
  assertEq failed (length (filter (== iconUnchecked glyphIcons <> "Feature") texts)) 1
  assert failed (any (T.isInfixOf (iconSelectClosed glyphIcons)) texts)
  let Size tw th = inputWindowSize inp
  cells <- rasterizeLayered (round tw) (round th) drawData spans []
  let blob = concat (cellRows cells)
  assert failed (any (`elem` blob) (concatMap T.unpack [iconChecked glyphIcons]))

runTerminalIconCloseTest :: Context -> IORef Int -> IO ()
runTerminalIconCloseTest _ failed = do
  term <- newAdaptiveTerminalContext
  let ctx = withIcons term IconsNerd
      inp = withInput 60 20
      ui = fmap fst (window True "Debug" (label_ "Body"))
  _ <- warmup2 ctx inp ui
  overlays <- collectOverlayTextSpans ctx inp
  let texts = [T.strip txt | (_, txt, _, _, _) <- overlays]
  assert failed (iconClose glyphIcons `elem` texts)
  assert failed (any (T.isPrefixOf (iconWindowTitle glyphIcons)) texts)

runTerminalThemeContrastTest :: Context -> IORef Int -> IO ()
runTerminalThemeContrastTest _ failed = do
  checkThemeContrast "terminalTheme-fallback" (terminalThemeFromColors terminalDefaultFg terminalDefaultBg) failed
  checkThemeContrast "terminalTheme-light" (terminalThemeFromColors (colorRGBA 0 0 0 255) (colorRGBA 255 255 255 255)) failed
  (fg, bg) <- queryTerminalColors
  checkThemeContrast "terminalTheme-adaptive" (terminalThemeFromColors fg bg) failed
  checkThemeContrast "defaultTheme" defaultTheme failed

checkThemeContrast :: String -> Theme -> IORef Int -> IO ()
checkThemeContrast _ theme failed = mapM_ check (themeContrastPairs theme)
 where
  aa = 4.5
  check (_, fg, bg) = do
    let ratio = contrastRatio fg bg
    assert failed (ratio >= aa)

themeContrastPairs :: Theme -> [(String, Color, Color)]
themeContrastPairs theme = concat
  [ styleStates "panel" (themePanel theme)
  , styleStates "floating-window" (themeFloatingWindow theme)
  , styleStates "button" (themeButton theme)
  , styleStates "input" (themeInput theme)
  , [ ("panel-fg/window", styleFg (themePanel theme), themeWindow theme)
    , ("accent/panel", themeAccent theme, styleBg (themePanel theme))
    , ("accent/window", themeAccent theme, themeWindow theme)
    , ("muted/panel", themeMuted theme, styleBg (themePanel theme))
    , ("muted/window", themeMuted theme, themeWindow theme)
    , ("modal-fg/dim", styleFg (themePanel theme), themeOverlayDim theme)
    , ("modal-muted/dim", themeMuted theme, themeOverlayDim theme)
    , ("modal-accent/dim", themeAccent theme, themeOverlayDim theme)
    , ( "scroll-thumb/floating"
      , scrollBarThumbColor (themeFloatingWindow theme) theme True
      , styleBg (themeFloatingWindow theme)
      )
    , ( "scroll-thumb/track"
      , scrollBarThumbColor (themeFloatingWindow theme) theme True
      , scrollBarTrackColor (themeFloatingWindow theme) theme True
      )
    ]
  ]
 where
  styleStates name style =
    [ (name ++ "-fg/bg", styleFg style, styleBg style)
    , (name ++ "-fg/hover", styleFg style, styleHoverBg style)
    , (name ++ "-fg/active", styleFg style, styleActiveBg style)
    ]

runTerminalTextInputDisplayTest :: Context -> IORef Int -> IO ()
runTerminalTextInputDisplayTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let inp = withInput 40 10
      ui = column (textInput "Name" "hello")
  _ <- warmup2 ctx inp ui
  spans <- collectTextSpans ctx
  case [txt | (_, txt, _, _, _) <- spans, "Name:" `T.isPrefixOf` txt] of
    [one] -> do
      assertEq failed one "Name: hello"
      assert failed (not ("Name: hello: hello" `T.isInfixOf` one))
    _ -> assert failed False

runTerminalSeparatorSpanTest :: Context -> IORef Int -> IO ()
runTerminalSeparatorSpanTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let inp = withInput 40 8
      ui = columnWith fillW (label_ "A" >> separator >>= \r -> label_ "B" >> pure r)
  _ <- runFrame ctx inp ui
  (resp, _, drawData, _) <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  let Rect _ _ w h = respRect resp
  assert failed (w >= 20 && h <= 2)
  let Size tw th = inputWindowSize inp
  cells <- rasterizeLayered (round tw) (round th) drawData spans []
  let blob = concat (cellRows cells)
  assert failed ('\x2500' `elem` blob && "A" `isInfixOf` blob && "B" `isInfixOf` blob)
