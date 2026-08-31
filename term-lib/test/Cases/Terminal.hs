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

import Control.Monad (forM_, void, when)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy qualified as BL
import Data.IORef (IORef)
import Data.List (isInfixOf, sort)
import Data.Text qualified as T
import NanoUI
import NanoUI.Testing
import NanoUI.Testing.Assert (bump, failWhen, withInput)
import NanoUI.Testing.Harness
  ( assertWheelTitlePinned
  , clickPair
  , closeSpanBottom
  , closeSpanPos
  , closeSpanStart
  , spanLabelYs
  , terminalAboutModalMaxFooter
  , terminalAboutModalMaxH
  , terminalBracketsOk
  , terminalBracketSpans
  , terminalGridW
  , terminalPairsOk
  , warmup2
  , warmupDraw
  , withInputOff
  )
import NanoUI.Testing.Term

runTerminalDefaultGapTest :: Context -> IORef Int -> IO ()
runTerminalDefaultGapTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let
    fm = ctxFontMetrics ctx
    expectedStep =
      fmLineHeight fm
        + resolveLayoutGap (ctxHostProfile ctx) fm (layoutGap defaultLayout)
    inp = withInput 20 10
    ui =
      column defaultLayout $ do
        _ <- label "A"
        _ <- label "B"
        pure ()
  _ <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  let
    ys = sort [y | (Rect _ y _ _, txt, _, _, _) <- spans, txt == "A" || txt == "B"]
  case ys of
    yA : yB : _ -> when (yB - yA > expectedStep + 0.25) $ bump failed
    _ -> bump failed

runTerminalSliderTrackTest :: Context -> IORef Int -> IO ()
runTerminalSliderTrackTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let
    inp0 = withInput 60 10
    ui = column (fillW defaultLayout) (slider "Vol" 0 100 0)
  (resp, _) <- warmup2 ctx inp0 ui
  let
    Rect rx ry rw rh = respRect resp
    fm = ctxFontMetrics ctx
    track = sliderTrackBounds (ctxHostProfile ctx) fm "Vol" rx ry rw rh
  when (rectW track >= rw - 1) $ bump failed
  when (rectW track < 10) $ bump failed
  let
    endDrag =
      inp0
        { inputMousePos =
            V2 (rectX track + rectW track - 0.5) (rectY track + rectH track / 2)
        , inputMouseDown = True
        , inputMousePressed = True
        }
  ((_, val), _, _, _) <- runFrame ctx endDrag ui
  when (val < 90) $ bump failed

runTerminalModalOverlayTest :: Context -> IORef Int -> IO ()
runTerminalModalOverlayTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let
    inp0 = withInput 80 24
    ui =
      column defaultLayout $ do
        _ <- label "Behind"
        (dlg, _) <-
          modal True "About" $ do
            heading "nano-ui"
            muted "Immediate-mode GUI for Haskell."
            muted "Terminal backend demo."
            row (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fit}) $ do
              flex
              clickButton "Close" (pure ())
            pure ()
        pure dlg
  (dlg, drawData) <- warmupDraw ctx inp0 ui
  overlays <- collectOverlayTextSpans ctx inp0
  base <- collectTextSpans ctx
  let
    hasTitle = any (\(_, txt, _, _, _) -> "About" `T.isInfixOf` txt) overlays
    hasBody = any (\(_, txt, _, _, _) -> "Immediate-mode" `T.isInfixOf` txt) overlays
    hasClose = any (\(_, txt, _, _, _) -> "Close" `T.isInfixOf` txt) overlays
    inGrid (Rect x y w h, _, _, _, _) =
      x >= -0.5 && y >= -0.5 && x + w <= 80.5 && y + h <= 24.5
  when (not (hasTitle && hasBody && hasClose)) $ bump failed
  when (any (not . inGrid) overlays) $ bump failed
  let
    Rect _ _ mw mh = respRect dlg
  when (mw > 80 || mh > 24 || mw < 8 || mh < 2) $ bump failed
  cells <- rasterizeLayered 80 24 drawData base overlays
  let
    blob = concat (cellRows cells)
  when (not ("About" `isInfixOf` blob)) $ bump failed
  when (not ("Immediate-mode" `isInfixOf` blob)) $ bump failed
  when (not ("Behind" `isInfixOf` blob)) $ bump failed
  when (not ('\x2500' `elem` blob)) $ bump failed
  when
    (not (any (\c -> cmdTextureId c == backdropDimTextureId) (drawCmdElems drawData))) $
    bump failed

runTerminalModalScrollTest :: Context -> IORef Int -> IO ()
runTerminalModalScrollTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let
    inp0 = withInput 80 16
    line1 = T.pack "line 1"
    title = T.pack "About"
    ui = do
      (dlg, _) <-
        modal True "About"
          $ column defaultLayout
          $ mapM_ (\i -> label (T.pack ("line " <> show (i :: Int)))) [1 .. 24]
      pure dlg
  dlg <- warmup2 ctx inp0 ui
  let
    Rect mx _ mw mh = respRect dlg
  failWhen failed (mw <= 0 || mh <= 0 || mh > 16)
  spans0 <- collectOverlayTextSpans ctx inp0
  case spanLabelYs line1 spans0 of
    [] -> bump failed
    b0 : _ ->
      assertWheelTitlePinned
        failed
        ctx
        inp0
        ui
        title
        line1
        (V2 (mx + mw / 2) (b0 + 0.5))
        (Just 16.5)

runTerminalModalTightTest :: Context -> IORef Int -> IO ()
runTerminalModalTightTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let
    inp0 = withInput 80 24
    ui = do
      (dlg, _) <-
        modal True "About" $ do
          heading "nano-ui"
          muted "Immediate-mode GUI for Haskell."
          muted "Terminal backend demo."
          row (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fit}) $ do
            flex
            clickButton "Close" (pure ())
          pure ()
      pure dlg
  _ <- runFrame ctx inp0 ui
  (dlg, _, _, _) <- runFrame ctx inp0 ui
  overlays <- collectOverlayTextSpans ctx inp0
  let
    fm = ctxFontMetrics ctx
    Rect _ my _ mh = respRect dlg
    maxH = terminalAboutModalMaxH (ctxHostProfile ctx) fm
    maxFooter = terminalAboutModalMaxFooter (ctxHostProfile ctx) fm
  case closeSpanBottom overlays of
    Nothing -> bump failed
    Just bottom ->
      let
        footer = my + mh - bottom
       in
        when (mh > maxH || footer > maxFooter) $ bump failed

runTerminalModalOpenRedrawTest :: Context -> IORef Int -> IO ()
runTerminalModalOpenRedrawTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let
    inp0 = withInputOff 80 24
    ui = do
      (readOpen, setOpen) <- useFlag False
      resp <- button "Open"
      onClick resp (setOpen True)
      open <- readOpen
      _ <- modal open "About" (label "body")
      pure resp
  _ <- runFrame ctx inp0 ui
  (resp, _, _, _) <- runFrame ctx inp0 ui
  let
    Rect rx ry rw rh = respRect resp
    press =
      inp0
        { inputMousePos = V2 (rx + rw / 2) (ry + rh / 2)
        , inputMouseDown = True
        , inputMousePressed = True
        }
  _ <- runFrame ctx press ui
  let
    release =
      press
        { inputMouseDown = False
        , inputMousePressed = False
        , inputMouseReleased = True
        }
  _ <- runFrame ctx release ui
  let
    idle = inp0 {inputDeltaTime = 0}
  need <- needsRedrawIdle ctx release idle
  failWhen failed (not need)
  _ <- runFrame ctx idle ui
  overlays <- collectOverlayTextSpans ctx idle
  let
    hasAbout = any (\(_, txt, _, _, _) -> "About" `T.isInfixOf` txt) overlays
  failWhen failed (not hasAbout)
  dmg <- takeDamage ctx
  when (damageIsEmpty dmg) $ bump failed

runTerminalWindowOverlayTest :: Context -> IORef Int -> IO ()
runTerminalWindowOverlayTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let
    inp0 = withInput 80 24
    ui =
      window True "Debug" $ do
        _ <- label "Floating window overlay."
        pure ()
  _ <- runFrame ctx inp0 ui
  ((win, _), _, drawData, _) <- runFrame ctx inp0 ui
  overlays <- collectOverlayTextSpans ctx inp0
  base <- collectTextSpans ctx
  let
    hasTitle = any (\(_, txt, _, _, _) -> "Debug" `T.isInfixOf` txt) overlays
    hasBody = any (\(_, txt, _, _, _) -> "Floating window" `T.isInfixOf` txt) overlays
    inGrid (Rect x y w h, _, _, _, _) =
      x >= -0.5 && y >= -0.5 && x + w <= 80.5 && y + h <= 24.5
  when (not (hasTitle && hasBody)) $ bump failed
  when (any (not . inGrid) overlays) $ bump failed
  let
    Rect _ _ ww wh = respRect win
  when (ww > 80 || wh > 24 || ww < 8 || wh < 2) $ bump failed
  cells <- rasterizeLayered 80 24 drawData base overlays
  let
    blob = concat (cellRows cells)
  when (not ("Debug" `isInfixOf` blob)) $ bump failed
  when (not ("Floating window" `isInfixOf` blob)) $ bump failed
  when (not ('\x2500' `elem` blob)) $ bump failed

runTerminalWindowDragTest :: Context -> IORef Int -> IO ()
runTerminalWindowDragTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let
    inp0 = withInput 80 24
    ui = do
      (win, _) <- window True "Debug" (label "Body")
      pure win
  _ <- runFrame ctx inp0 ui
  (win0, _, _, _) <- runFrame ctx inp0 ui
  let
    Rect x0 y0 _ _ = respRect win0
    grab = V2 (x0 + 4) (y0 + 1.5)
    press =
      inp0
        { inputMousePos = grab
        , inputMouseDown = True
        , inputMousePressed = True
        }
  _ <- runFrame ctx press ui
  let
    moved =
      press
        { inputMousePos = V2 (x0 + 4 - 8) (y0 + 1.5 + 4)
        , inputMousePressed = False
        }
  _ <- runFrame ctx moved ui
  (win1, _, _, _) <- runFrame ctx moved ui
  let
    Rect x1 y1 _ _ = respRect win1
  when (x1 >= x0 - 2) $ bump failed
  when (y1 <= y0 + 1) $ bump failed

runTerminalWindowDragIconTest :: Context -> IORef Int -> IO ()
runTerminalWindowDragIconTest _ failed = do
  term <- newAdaptiveTerminalContext
  let
    ctx = withIcons term IconsNerd
    inp0 = withInput 80 24
    ui = do
      (win, _) <- window True "Debug" (label "Body")
      pure win
  _ <- runFrame ctx inp0 ui
  (_, _, draw0, _) <- runFrame ctx inp0 ui
  overlays0 <- collectOverlayTextSpans ctx inp0
  (win0, _, _, _) <- runFrame ctx inp0 ui
  let
    Rect x0 y0 _ _ = respRect win0
    grab = V2 (x0 + 4) (y0 + 1.5)
    press =
      inp0
        { inputMousePos = grab
        , inputMouseDown = True
        , inputMousePressed = True
        }
  _ <- runFrame ctx press ui
  let
    moved =
      press
        { inputMousePos = V2 (x0 + 4 - 10) (y0 + 1.5 + 4)
        , inputMousePressed = False
        }
  (_, _, draw1, _) <- runFrame ctx moved ui
  overlays1 <- collectOverlayTextSpans ctx moved
  let
    Size tw th = inputWindowSize inp0
  cells0 <- rasterizeLayered (round tw) (round th) draw0 [] overlays0
  cells1 <- rasterizeLayered (round tw) (round th) draw1 [] overlays1
  case closeSpanPos overlays0 of
    Nothing -> bump failed
    Just (closeCol, rowY) -> do
      let
        maxX =
          case cellRows cells1 of
            (r : _) -> length r - 1
            [] -> -1
        trailGhost =
          any
            ( \x ->
                x >= 0
                  && x <= maxX
                  && rowY >= 0
                  && rowY < cellsH cells1
                  && let
                       c1 = cellChar cells1 x rowY
                      in
                       fontAwesomeIcon c1 || c1 == wideTrailChar
            )
            [closeCol, closeCol + 1]
      case closeSpanStart overlays1 of
        Nothing -> bump failed
        Just x1 -> do
          when (x1 >= closeCol - 2) $ bump failed
          when (x1 < closeCol - 2 && trailGhost) $ bump failed
  let
    bytes = toLazyByteString (frameBytes (Just cells0) cells1)
  when (BL.null bytes) $ bump failed

runTerminalButtonBracketTest :: Context -> IORef Int -> IO ()
runTerminalButtonBracketTest _ failed = do
  term <- newAdaptiveTerminalContext
  let
    ctx = withIcons term IconsNerd
    inp0 = withInput 50 20
    ui = do
      (dlg, _) <-
        modal True "Long modal title for clip" $ do
          row defaultLayout $ do
            void (button "OK")
            void (button "Cancel")
          label_ "Body"
      pure dlg
    Size tw th = inputWindowSize inp0
  _ <- runFrame ctx inp0 ui
  (_, _, draw0, _) <- runFrame ctx inp0 ui
  overlays0 <- collectOverlayTextSpans ctx inp0
  let spans0 = terminalBracketSpans overlays0
  when (null spans0) $ bump failed
  cells0 <- rasterizeLayered (round tw) (round th) draw0 [] overlays0
  when (not (terminalBracketsOk cells0 overlays0)) $ bump failed
  case closeSpanPos overlays0 of
    Nothing -> bump failed
    Just (closeCol, closeY) -> do
      let
        hover =
          inp0
            { inputMousePos = V2 (fromIntegral closeCol + 0.5) (fromIntegral closeY + 0.5)
            , inputMouseDown = False
            }
      (_, _, draw1, _) <- runFrame ctx hover ui
      overlays1 <- collectOverlayTextSpans ctx hover
      cells1 <- rasterizeLayered (round tw) (round th) draw1 [] overlays1
      when (not (terminalBracketsOk cells1 overlays1)) $ bump failed
  let
    pageUi =
      column defaultLayout $
        row defaultLayout $ do
          void (button "OK")
          void (button "Cancel")
          void (checkbox "Feature" False)
  _ <- runFrame ctx inp0 pageUi
  (_, _, drawP, _) <- runFrame ctx inp0 pageUi
  baseP <- collectTextSpans ctx
  cellsP <- rasterizeLayered (round tw) (round th) drawP baseP []
  let pageBrackets = terminalBracketSpans baseP
  when (null pageBrackets) $ bump failed
  when (not (terminalBracketsOk cellsP baseP)) $ bump failed

runTerminalWideClearBracketTest :: Context -> IORef Int -> IO ()
runTerminalWideClearBracketTest ctx failed = do
  let
    inp = withInput 2 1
    clip = Rect 0 0 2 1
    fg = colorRGBA 220 220 220 255
    bg = colorRGBA 20 20 24 255
  (_, _, draw, _) <- runFrame ctx inp (pure ())
  cellsA <- rasterize 2 1 draw [(clip, "[O", fg, bg, clip)]
  cellsB <- rasterize 2 1 draw [(clip, iconClose glyphIcons, fg, bg, clip)]
  let
    bytes = toLazyByteString (frameBytes (Just cellsA) cellsB)
    packed = BL.unpack bytes
  when (cellChar cellsA 0 0 /= '[') $ bump failed
  when (cellChar cellsA 1 0 /= 'O') $ bump failed
  when (not (fontAwesomeIcon (cellChar cellsB 0 0))) $ bump failed
  when (cellChar cellsB 1 0 /= wideTrailChar) $ bump failed
  when (BL.null bytes) $ bump failed
  when (0x20 `notElem` packed) $ bump failed
  when (not (BS.isInfixOf "\ESC[1;1H" (BL.toStrict bytes))) $ bump failed

runTerminalWideCursorCupTest :: Context -> IORef Int -> IO ()
runTerminalWideCursorCupTest ctx failed = do
  let
    inp = withInput 4 1
    clip = Rect 0 0 4 1
    fg = colorRGBA 220 220 220 255
    bg = colorRGBA 20 20 24 255
  (_, _, draw, _) <- runFrame ctx inp (pure ())
  cells <-
    rasterize
      4
      1
      draw
      [ (Rect 0 0 2 1, iconClose glyphIcons, fg, bg, clip)
      , (Rect 2 0 2 1, "[O", fg, bg, clip)
      ]
  when (not (fontAwesomeIcon (cellChar cells 0 0))) $ bump failed
  when (cellChar cells 1 0 /= wideTrailChar) $ bump failed
  when (cellChar cells 2 0 /= '[') $ bump failed
  let
    bytes = BL.toStrict (toLazyByteString (frameBytes Nothing cells))
  -- 1-based CUP to column 3 (grid x=2), after the 2-col icon.
  when (not (BS.isInfixOf "\ESC[1;3H" bytes)) $ bump failed

runTerminalWideTransitionTest :: Context -> IORef Int -> IO ()
runTerminalWideTransitionTest _ failed = do
  term <- newAdaptiveTerminalContext
  let
    ctx = withIcons term IconsNerd
    inp0 = withInput 80 24
    page = do
      void (button "OK")
      void (button "Cancel")
      void (checkbox "Feature" False)
    windowUi = do
      page
      (win, _) <- window True "Debug" (label_ "Body")
      pure win
    modalUi open = do
      page
      (dlg, _) <- modal open "About" (label_ "Body")
      pure dlg
  -- Button hover on the page row (checkbox FA sits on the same wrap row).
  _ <- runFrame ctx inp0 page
  (_, _, draw0, _) <- runFrame ctx inp0 page
  base0 <- collectTextSpans ctx
  let
    Size tw th = inputWindowSize inp0
  cells0 <- rasterizeLayered (round tw) (round th) draw0 base0 []
  when (not (terminalPairsOk cells0 base0)) $ bump failed
  when (not (terminalBracketsOk cells0 base0)) $ bump failed
  case terminalBracketSpans base0 of
    [] -> bump failed
    (Rect bx by _ _ : _) -> do
      let
        ibx = round bx :: Int
        iby = round by :: Int
        hover =
          inp0
            { inputMousePos = V2 (fromIntegral ibx + 1.5) (fromIntegral iby + 0.5)
            }
      (_, _, drawH, _) <- runFrame ctx hover page
      baseH <- collectTextSpans ctx
      cellsHov <- rasterizeLayered (round tw) (round th) drawH baseH []
      when (not (terminalPairsOk cellsHov baseH)) $ bump failed
      when (not (terminalBracketsOk cellsHov baseH)) $ bump failed
      when (cellChar cellsHov (round bx) (round by) /= '[') $ bump failed
  -- Modal open: close icon is present, page brackets stay clean.
  _ <- runFrame ctx inp0 (void (modalUi False))
  (_, _, drawC, _) <- runFrame ctx inp0 (void (modalUi False))
  (baseC, overC) <- collectRasterSpans ctx inp0
  cellsC <- rasterizeLayered (round tw) (round th) drawC baseC overC
  _ <- runFrame ctx inp0 (void (modalUi True))
  (_, _, drawM, _) <- runFrame ctx inp0 (void (modalUi True))
  (baseM, overM) <- collectRasterSpans ctx inp0
  cellsM <- rasterizeLayered (round tw) (round th) drawM baseM overM
  when (not (terminalPairsOk cellsM (baseM ++ overM))) $ bump failed
  when (closeSpanPos overM == Nothing) $ bump failed
  when (not (terminalBracketsOk cellsC baseC)) $ bump failed
  -- Window drag: old close columns must not keep FA/trail.
  _ <- runFrame ctx inp0 windowUi
  (_, _, _, _) <- runFrame ctx inp0 windowUi
  overW0 <- collectOverlayTextSpans ctx inp0
  (win0, _, _, _) <- runFrame ctx inp0 windowUi
  let
    Rect wx wy _ _ = respRect win0
    grab = V2 (wx + 4) (wy + 1.5)
    press =
      inp0
        { inputMousePos = grab
        , inputMouseDown = True
        , inputMousePressed = True
        }
  _ <- runFrame ctx press windowUi
  let
    moved =
      press
        { inputMousePos = V2 (wx + 4 - 10) (wy + 1.5 + 4)
        , inputMousePressed = False
        }
  (_, _, drawW1, _) <- runFrame ctx moved windowUi
  overW1 <- collectOverlayTextSpans ctx moved
  cellsW1 <- rasterizeLayered (round tw) (round th) drawW1 [] overW1
  when (not (terminalPairsOk cellsW1 overW1)) $ bump failed
  case closeSpanPos overW0 of
    Nothing -> bump failed
    Just (cx, cy) ->
      case closeSpanStart overW1 of
        Nothing -> bump failed
        Just x1 -> do
          when (x1 >= cx - 2) $ bump failed
          let
            leftover =
              any
                ( \x ->
                    x >= 0
                      && x < terminalGridW cellsW1
                      && cy >= 0
                      && cy < cellsH cellsW1
                      && let
                           c1 = cellChar cellsW1 x cy
                          in
                           fontAwesomeIcon c1 || c1 == wideTrailChar
                )
                [cx, cx + 1]
          when leftover $ bump failed

runTerminalWidePairTest :: Context -> IORef Int -> IO ()
runTerminalWidePairTest _ failed = do
  term <- newAdaptiveTerminalContext
  let
    ctx = withIcons term IconsNerd
    inp = withInput 80 24
    ui = do
      _ <- button "OK"
      _ <- button "Cancel"
      _ <- checkbox "Feature" False
      (win, _) <- window True "Debug" (label_ "Body")
      pure win
  _ <- runFrame ctx inp ui
  (_, _, draw, _) <- runFrame ctx inp ui
  (base, overlay) <- collectRasterSpans ctx inp
  cells <- rasterizeLayered 80 24 draw base overlay
  when (not (terminalPairsOk cells (base ++ overlay))) $ bump failed

runTerminalCloseButtonTest :: Context -> IORef Int -> IO ()
runTerminalCloseButtonTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let
    inp0 = withInput 80 24
    modalUi =
      column defaultLayout $ do
        (dlg, _) <- modal True "About" (label_ "Body")
        pure dlg
    windowUi = do
      (win, _) <- window True "Debug" (label_ "Body")
      pure win
    testClose ui = do
      _ <- runFrame ctx inp0 ui
      _ <- runFrame ctx inp0 ui
      overlays <- collectOverlayTextSpans ctx inp0
      case closeSpanCenter overlays of
        Nothing -> bump failed
        Just (V2 cx cy) -> do
          -- Left edge of the 3-cell close slot, not the centered glyph.
          let
            edge = V2 (cx - 1.0) cy
            (press, release) = clickPair inp0 edge
          _ <- runFrame ctx press ui
          (outer, _, _, _) <- runFrame ctx release ui
          when (not (respClicked outer)) $ bump failed
  testClose modalUi
  testClose windowUi

runTerminalIconChromeTest :: Context -> IORef Int -> IO ()
runTerminalIconChromeTest _ failed = do
  let
    ck cond = when (not cond) (bump failed)
  term <- newAdaptiveTerminalContext
  let
    ctx = withIcons term IconsNerd
    inp = withInput 40 30
    rows =
      column (fillW defaultLayout) (forM_ [1 .. 40 :: Int] (label_ . T.pack . show))
    ui = column (fillW defaultLayout) $ do
      _ <- checkbox "Feature" False
      _ <- select "Quality" ["Low", "High"] 0
      _ <- scrollArea ((fillW defaultLayout) {layoutHeight = Fixed 20}) rows
      pure ()
  _ <- runFrame ctx inp ui
  (_, _, drawData, _) <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  let
    texts = [txt | (_, txt, _, _, _) <- spans]
    hasGlyph g = any (T.isInfixOf g) texts
  ck (length (filter (== iconUnchecked glyphIcons <> "Feature") texts) == 1)
  ck (hasGlyph (iconSelectClosed glyphIcons))
  -- Scroll caps fit a 1-cell track (lone FA paints one column).
  let
    Size tw th = inputWindowSize inp
  cells <- rasterizeLayered (round tw) (round th) drawData spans []
  let
    blob = concat (cellRows cells)
  ck (any (`elem` blob) (concatMap T.unpack [iconChecked glyphIcons]))

runTerminalIconCloseTest :: Context -> IORef Int -> IO ()
runTerminalIconCloseTest _ failed = do
  term <- newAdaptiveTerminalContext
  let
    ctx = withIcons term IconsNerd
    inp = withInput 60 20
    ui = do
      (win, _) <- window True "Debug" (label_ "Body")
      pure win
  _ <- runFrame ctx inp ui
  _ <- runFrame ctx inp ui
  overlays <- collectOverlayTextSpans ctx inp
  let
    texts = [T.strip txt | (_, txt, _, _, _) <- overlays]
  when (iconClose glyphIcons `notElem` texts) $ bump failed
  when (not (any (T.isPrefixOf (iconWindowTitle glyphIcons)) texts)) $ bump failed

closeSpanCenter :: [(Rect, T.Text, Color, Color, Rect)] -> Maybe V2
closeSpanCenter spans =
  case [Rect x y w h | (Rect x y w h, txt, _, _, _) <- spans, T.strip txt == "X"] of
    (Rect x y w h : _) -> Just (V2 (x + w / 2) (y + h / 2))
    [] -> Nothing

runTerminalThemeContrastTest :: Context -> IORef Int -> IO ()
runTerminalThemeContrastTest _ failed = do
  checkThemeContrast
    "terminalTheme-fallback"
    (terminalThemeFromColors terminalDefaultFg terminalDefaultBg)
    failed
  checkThemeContrast
    "terminalTheme-light"
    (terminalThemeFromColors (colorRGBA 0 0 0 255) (colorRGBA 255 255 255 255))
    failed
  (fg, bg) <- queryTerminalColors
  checkThemeContrast
    "terminalTheme-adaptive"
    (terminalThemeFromColors fg bg)
    failed
  checkThemeContrast "defaultTheme" defaultTheme failed

checkThemeContrast :: String -> Theme -> IORef Int -> IO ()
checkThemeContrast themeName theme failed =
  mapM_ check (themeContrastPairs theme)
 where
  aa = 4.5
  check (pairName, fg, bg) = do
    let
      ratio = contrastRatio fg bg
    when (ratio < aa) $ do
      putStrLn $
        "  contrast "
          ++ themeName
          ++ " "
          ++ pairName
          ++ ": "
          ++ show ratio
          ++ " < "
          ++ show aa
      bump failed

themeContrastPairs :: Theme -> [(String, Color, Color)]
themeContrastPairs theme =
  concat
    [ styleStates "panel" (themePanel theme)
    , styleStates "floating-window" (themeFloatingWindow theme)
    , styleStates "button" (themeButton theme)
    , styleStates "input" (themeInput theme)
    ,
      [ ("panel-fg/window", styleFg (themePanel theme), themeWindow theme)
      , ("accent/panel", themeAccent theme, styleBg (themePanel theme))
      , ("accent/window", themeAccent theme, themeWindow theme)
      , ("muted/panel", themeMuted theme, styleBg (themePanel theme))
      , ("muted/window", themeMuted theme, themeWindow theme)
      , ("modal-fg/dim", styleFg (themePanel theme), themeOverlayDim theme)
      , ("modal-muted/dim", themeMuted theme, themeOverlayDim theme)
      , ("modal-accent/dim", themeAccent theme, themeOverlayDim theme)
      ,
        ( "scroll-thumb/floating"
        , scrollBarThumbColor (themeFloatingWindow theme) theme True
        , styleBg (themeFloatingWindow theme)
        )
      ,
        ( "scroll-thumb/track"
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
  let
    inp = withInput 40 10
    ui = column defaultLayout (textInput "Name" "hello")
  _ <- runFrame ctx inp ui
  _ <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  let
    shown = [txt | (_, txt, _, _, _) <- spans, "Name:" `T.isPrefixOf` txt]
  case shown of
    [one] -> do
      when (one /= "Name: hello") $ bump failed
      when ("Name: hello: hello" `T.isInfixOf` one) $ bump failed
    _ -> bump failed
runTerminalSeparatorSpanTest :: Context -> IORef Int -> IO ()
runTerminalSeparatorSpanTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let
    inp = withInput 40 8
    ui =
      column (fillW defaultLayout) $ do
        label_ "A"
        resp <- separator
        label_ "B"
        pure resp
  _ <- runFrame ctx inp ui
  (resp, _, drawData, _) <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  let
    Rect _ _ w h = respRect resp
  when (w < 20) $ bump failed
  when (h > 2) $ bump failed
  let
    Size tw th = inputWindowSize inp
  cells <- rasterizeLayered (round tw) (round th) drawData spans []
  let
    blob = concat (cellRows cells)
  when (not ('\x2500' `elem` blob)) $ bump failed
  when (not ("A" `isInfixOf` blob && "B" `isInfixOf` blob)) $ bump failed
