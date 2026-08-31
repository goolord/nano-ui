module Cases
  ( module Cases.Animation
  , module Cases.Demo
  , module Cases.Modal
  , module Cases.Scroll
  , module Cases.Select
  , module Cases.Tabs
  , module Cases.Terminal
  , module Cases.TextInput
  , module Cases.Window
  , runAsciiTest
  , runAspectLayoutTest
  , runCellsTest
  , runCheckboxTest
  , runColumnCardWrapTest
  , runCompactHostTest
  , runDemoWrapWideOrderTest
  , runDrawTest
  , runEmbedStateTest
  , runFitSizingTest
  , runFlexShrinkTest
  , runFlexWrapTest
  , runGrowFitsWindowTest
  , runGrowWrapPushesSiblingTest
  , runHostProfileGapTest
  , runHostProfileMeasureTest
  , runHostSlotTest
  , runHoverDamageTest
  , runHoverSkipTest
  , runHoverTest
  , runIconSetTest
  , runIdKeyedListTest
  , runIdleTest
  , runIdStabilityTest
  , runIdUniquenessTest
  , runIdZeroAllocTest
  , runImageTest
  , runInteractionTest
  , runLabelAlignEndTest
  , runLayoutTest
  , runOverlayTest
  , runPanelPaintsTest
  , runPercentLayoutTest
  , runPointerCursorCheckboxTest
  , runPointerCursorTest
  , runReduceClickTest
  , runReduceIdentityTest
  , runReduceMessagesTest
  , runReduceUpdatesTest
  , runRowPanelLayoutTest
  , runSliderFillWidthTest
  , runSliderTest
  , runTabFocusTest
  , runTextMultilineTest
  , runTextWrapAssignedTest
  , runTextWrapTest
  , runTwoCardWrapTest
  , runUseFlagClickTest
  , runVtTest
  , runWidgetNoStringEmitTest
  , runWithKeyTest
  ) where

import Cases.Animation
import Cases.Demo
import Cases.Modal
import Cases.Scroll
import Cases.Select
import Cases.Tabs
import Cases.Terminal
import Cases.TextInput
import Cases.Window
import Control.Monad (replicateM, void, when)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.IORef (IORef)
import Data.List (isInfixOf, nub, sort)
import Data.Text qualified as T
import Effectful.State.Static.Local (State, evalState, get, modify)
import GHC.Stats (RTSStats (..), getRTSStats, getRTSStatsEnabled)
import NanoUI
import NanoUI.Testing
import NanoUI.Testing.Assert (bump, failWhen, measureRespW, runClickReduce, withInput)
import NanoUI.Testing.Harness
  ( centerOf
  , checkLabelAlignEnd
  , spanXOf
  , spanYOf
  , vertUv
  , warmup2
  , warmupDraw
  , withInputOff
  )
import NanoUI.Testing.Term
import System.Mem (performGC)
runHostProfileGapTest :: Context -> IORef Int -> IO ()
runHostProfileGapTest _ failed = do
  let
    defaultGap = layoutGap defaultLayout
    cellGap = resolveLayoutGap CellHost (monospaceMetrics 1) defaultGap
    pixelGap = resolveLayoutGap PixelHost (monospaceMetrics 16) defaultGap
  when (cellGap /= 1) $ bump failed
  when (pixelGap /= defaultGap) $ bump failed

runHostProfileMeasureTest :: Context -> IORef Int -> IO ()
runHostProfileMeasureTest _ failed = do
  let
    txt = "abcde"
    fmCell = monospaceMetrics 1
    fmPixel = monospaceMetrics 16
    cellW = textDisplayWidth CellHost fmCell txt
    pixelW = textDisplayWidth PixelHost fmPixel txt
  when (cellW /= fromIntegral (terminalPaintColumns txt)) $ bump failed
  when (pixelW /= fromIntegral (T.length txt) * fmAdvance fmPixel ' ') $
    bump failed

runIdStabilityTest :: Context -> IORef Int -> IO ()
runIdStabilityTest ctx failed = do
  let
    inp = withInput 100 100
    sample = runFrame ctx inp (column defaultLayout (replicateM 3 nextId))
  (ids1, _, _, _) <- sample
  (ids2, _, _, _) <- sample
  when (ids1 /= ids2) $ bump failed
  case ids1 of
    [a, b, c] -> when (a == b || b == c || a == c) $ bump failed
    _ -> bump failed

runIdUniquenessTest :: Context -> IORef Int -> IO ()
runIdUniquenessTest ctx failed = do
  let
    inp = withInput 100 100
  (ids, _, _, _) <-
    runFrame
      ctx
      inp
      ( column defaultLayout $ do
          a <- nextId
          b <- nextId
          c <- nextId
          pure [a, b, c]
      )
  case ids of
    [a, b, c] -> when (a == b || b == c || a == c) $ bump failed
    _ -> bump failed

runIdZeroAllocTest :: Context -> IORef Int -> IO ()
runIdZeroAllocTest ctx failed = do
  enabled <- getRTSStatsEnabled
  when enabled $ do
    let
      inp = withInput 1 1
    _ <- runFrame ctx inp (pure ())
    performGC
    before <- getRTSStats
    _ <- runFrame ctx inp (void (replicateM 4096 nextId))
    after <- getRTSStats
    when (allocated_bytes after > allocated_bytes before) $ bump failed

runIdKeyedListTest :: Context -> IORef Int -> IO ()
runIdKeyedListTest ctx failed = do
  let
    inp = withInput 200 200
    keyedIds :: [String] -> IO ([WidgetId], [FrameMsg], DrawData, Bool)
    keyedIds keys =
      runFrame ctx inp
        $ column defaultLayout
        $ mapM (\k -> keyed k nextId) keys
    idFor :: String -> [String] -> [WidgetId] -> Maybe WidgetId
    idFor key keys ids = lookup key (zip keys ids)
  (idsA, _, _, _) <- keyedIds ["a", "b", "c"]
  (idsPrep, _, _, _) <- keyedIds ["x", "a", "b", "c"]
  (idsApp, _, _, _) <- keyedIds ["a", "b", "c", "y"]
  (idsRev, _, _, _) <- keyedIds ["c", "b", "a"]
  let
    aBase = idFor "a" ["a", "b", "c"] idsA
    bBase = idFor "b" ["a", "b", "c"] idsA
    cBase = idFor "c" ["a", "b", "c"] idsA
  when (aBase /= idFor "a" ["x", "a", "b", "c"] idsPrep) $ bump failed
  when (bBase /= idFor "b" ["a", "b", "c", "y"] idsApp) $ bump failed
  when (cBase /= idFor "c" ["c", "b", "a"] idsRev) $ bump failed

runFitSizingTest :: Context -> IORef Int -> IO ()
runFitSizingTest ctx failed = do
  let inp = withInput 400 100
  w1 <- measureRespW ctx inp (column defaultLayout (label "hi"))
  w2 <- measureRespW ctx inp (column defaultLayout (label "a much longer label"))
  failWhen failed (w1 <= 0 || w1 >= 400)
  failWhen failed (w2 <= w1)

runWithKeyTest :: Context -> IORef Int -> IO ()
runWithKeyTest ctx failed = do
  let
    inp = withInput 200 200
  (idsA, _, _, _) <-
    runFrame
      ctx
      inp
      ( column defaultLayout $ do
          a <- keyed (0 :: Int) nextId
          b <- keyed (1 :: Int) nextId
          pure [a, b]
      )
  (idsB, _, _, _) <-
    runFrame
      ctx
      inp
      ( column defaultLayout $ do
          a <- keyed (0 :: Int) nextId
          b <- keyed (1 :: Int) nextId
          pure [a, b]
      )
  case (idsA, idsB) of
    ([a0, a1], [b0, b1]) ->
      when (a0 /= b0 || a1 /= b1 || a0 == a1) $ bump failed
    _ -> bump failed

runLayoutTest :: Context -> IORef Int -> IO ()
runLayoutTest ctx failed = do
  let
    inp = withInput 400 300
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

runRowPanelLayoutTest :: Context -> IORef Int -> IO ()
runRowPanelLayoutTest ctx failed = do
  let
    inp = withInput 800 600
    ui =
      row (tight . fillW $ defaultLayout) $ do
        panel (minW 200 . fillH $ defaultLayout) $ void (label "Side")
        panel (grow . fillW . fillH $ defaultLayout) $
          row (tight . gap 8 $ defaultLayout) $ do
            void (button "Left")
            btn <- button "Right"
            pure btn
  _ <- runFrame ctx inp ui
  (resp, _, _, _) <- runFrame ctx inp ui
  let
    Rect bx _ _ _ = respRect resp
  when (bx < 190) $ bump failed

runColumnCardWrapTest :: Context -> IORef Int -> IO ()
runColumnCardWrapTest ctx failed = do
  let
    inp = withInput 520 800
    ui =
      row (tight . gap 8 . wrap . fillW $ defaultLayout) $ do
        column (tight . gap 8 . fillW $ defaultLayout) $ do
          card $ void (label "LeftTop")
          card $ void (label "LeftBot")
        card $ void (label "Right")
  _ <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  let yOf lbl = spanYOf lbl spans
  case (yOf "Right", yOf "LeftTop") of
    ([ry], [ly]) -> when (ry <= ly + 1) $ bump failed
    got -> do
      putStrLn ("column-card-wrap bad " ++ show got)
      bump failed

runTwoCardWrapTest :: Context -> IORef Int -> IO ()
runTwoCardWrapTest ctx failed = do
  let
    inp = withInput 520 800
    ui =
      row (tight . gap 8 . wrap . fillW $ defaultLayout) $ do
        card $ void (label "CardA")
        card $ void (label "CardB")
  _ <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  let yOf lbl = spanYOf lbl spans
  case (yOf "CardA", yOf "CardB") of
    ([ay], [by]) -> when (by <= ay + 1) $ bump failed
    _ -> bump failed

runDemoWrapWideOrderTest :: Context -> IORef Int -> IO ()
runDemoWrapWideOrderTest ctx failed = do
  let
    inp = withInput 1200 800
    ui =
      row (tight . gap 8 . wrap . fillW $ defaultLayout) $ do
        column (tight . gap 8 . fillW $ defaultLayout) $ do
          card $ void (label "State")
          card $ void (label "Gallery")
        card $ void (label "Controls")
  _ <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  let xOf lbl = spanXOf lbl spans
  case (xOf "State", xOf "Controls") of
    ([sx], [cx]) -> when (cx <= sx + 1) $ bump failed
    _ -> bump failed

runDrawTest :: Context -> IORef Int -> IO ()
runDrawTest ctx failed = do
  let
    inp = withInput 100 100
  (_, _, draw, _) <-
    runFrame
      ctx
      inp
      (column defaultLayout (label "draw"))
  when (drawIndexCount draw < 6) $ bump failed
  when (drawCmdNull draw) $ bump failed

runOverlayTest :: Context -> IORef Int -> IO ()
runOverlayTest ctx failed = do
  let
    inp0 = withInput 200 80
    ui = column defaultLayout (button "Hover" >>= tooltip "tip")
  (_, _, _, _) <- runFrame ctx inp0 ui
  let
    inp1 =
      inp0
        { inputMousePos = V2 10 10
        , inputMouseDown = False
        , inputMousePressed = False
        , inputMouseReleased = False
        }
  (_, _, draw, _) <- runFrame ctx inp1 ui
  let
    hasOverlay = any ((== LayerOverlay) . cmdLayer) (drawCmdElems draw)
  failWhen failed (not hasOverlay)

runInteractionTest :: Context -> IORef Int -> IO ()
runInteractionTest ctx failed = do
  let
    inp0 = withInput 200 100
    ui = column defaultLayout (button "Click")
  -- Frame 1: layout, store prev rects
  _ <- runFrame ctx inp0 ui
  -- Frame 2: press on button
  let
    inpPress =
      inp0
        { inputMousePos = V2 10 10
        , inputMousePressed = True
        , inputMouseDown = True
        , inputMouseReleased = False
        }
  _ <- runFrame ctx inpPress ui
  -- Frame 3: release => click
  let
    inpRelease =
      inpPress
        { inputMousePressed = False
        , inputMouseDown = False
        , inputMouseReleased = True
        }
  (resp, msgs, _, _) <- runFrame ctx inpRelease ui
  when (not (respClicked resp) || not (null msgs)) $ bump failed

runHoverTest :: Context -> IORef Int -> IO ()
runHoverTest ctx failed = do
  let
    inp0 = withInput 200 100
    ui = column defaultLayout (button "Hover")
  _ <- runFrame ctx inp0 ui
  let
    inp1 =
      inp0
        { inputMousePos = V2 10 10
        , inputMouseDown = False
        , inputMousePressed = False
        , inputMouseReleased = False
        }
  _ <- runFrame ctx inp1 ui
  hot <- getHotId ctx
  when (hashWidgetId hot == 0) $ bump failed

runPointerCursorTest :: Context -> IORef Int -> IO ()
runPointerCursorTest ctx failed = do
  let
    inp0 = withInput 200 100
    ui = column defaultLayout (button "Click")
  _ <- runFrame ctx inp0 ui
  let
    inp1 = inp0 {inputMousePos = V2 10 10}
  _ <- runFrame ctx inp1 ui
  want <- pointerCursorWanted ctx inp1
  failWhen failed (not want)
  let
    inp2 = inp0 {inputMousePos = V2 (-1) (-1)}
  _ <- runFrame ctx inp2 ui
  want2 <- pointerCursorWanted ctx inp2
  when want2 $ bump failed

runPointerCursorCheckboxTest :: Context -> IORef Int -> IO ()
runPointerCursorCheckboxTest ctx failed = do
  let
    inp0 = withInput 200 100
    ui = column defaultLayout (checkbox "Feature" False)
  (resp, _) <- warmup2 ctx inp0 ui
  let
    Rect rx ry rw rh = respRect resp
    hover = inp0 {inputMousePos = V2 (rx + rw / 2) (ry + rh / 2)}
  _ <- runFrame ctx hover ui
  want <- pointerCursorWanted ctx hover
  failWhen failed (not want)
  let
    click =
      hover
        { inputMouseDown = True
        , inputMousePressed = True
        , inputMouseReleased = False
        }
  _ <- runFrame ctx click ui
  wantClick <- pointerCursorWanted ctx click
  failWhen failed (not wantClick)

runImageTest :: Context -> IORef Int -> IO ()
runImageTest ctx failed = do
  let
    px a b c = BS.pack (concat (replicate 16 [a, b, c, 255]))
  ok1 <- registerImage ctx (ImageId 1) 4 4 (px 255 0 0)
  ok7 <- registerImage ctx (ImageId 7) 4 4 (px 0 0 255)
  when (not (ok1 && ok7)) $ bump failed
  let
    inp0 = withInput 320 200
    ui =
      row defaultLayout $ do
        _ <-
          image
            ( defaultLayout
                { layoutWidth = Fixed 40
                , layoutHeight = Fixed 24
                }
            )
            (ImageId 1)
        image
          ( defaultLayout
              { layoutWidth = Fixed 40
              , layoutHeight = Fixed 24
              }
          )
          (ImageId 7)
  (resp, drawData) <- warmupDraw ctx inp0 ui
  let
    Rect _ _ w h = respRect resp
  when (abs (w - 40) > 0.5 || abs (h - 24) > 0.5) $ bump failed
  let
    texCmds = filter (\c -> cmdTextureId c == atlasTextureId) (drawCmdElems drawData)
  when (length texCmds /= 1) $ bump failed
  when (not (any (\c -> cmdIndexCount c == 12) texCmds)) $ bump failed
  (u0, _) <- vertUv drawData 0
  (u4, _) <- vertUv drawData 4
  when (abs (u0 - u4) < 1e-6) $ bump failed
  let
    missing =
      image
        ( defaultLayout
            { layoutWidth = Fixed 40
            , layoutHeight = Fixed 24
            }
        )
        (ImageId 0)
  _ <- runFrame ctx inp0 missing
  (_, _, missingData, _) <- runFrame ctx inp0 missing
  when (any (\c -> cmdTextureId c > 0) (drawCmdElems missingData)) $ bump failed

runIdleTest :: Context -> IORef Int -> IO ()
runIdleTest _ failed = do
  ctx <- newContext
  let inp = withInputOff 100 100
  _ <- runFrame ctx inp (label "idle")
  need <- needsRedraw ctx inp inp
  when need $ bump failed

runHoverSkipTest :: Context -> IORef Int -> IO ()
runHoverSkipTest _ failed = do
  ctx <- newContext
  let
    ui = column defaultLayout (button "OK")
    inp0 =
      withInputOff 240 80
  _ <- runFrame ctx inp0 ui
  (resp, _, _, _) <- runFrame ctx inp0 ui
  let
    Rect rx ry rw rh = respRect resp
    inside = V2 (rx + rw / 2) (ry + rh / 2)
    inside2 = V2 (rx + rw / 2 + 1) (ry + rh / 2)
    inp1 = inp0 {inputMousePos = inside}
    inp2 = inp0 {inputMousePos = inside2}
  needEnter <- needsRedraw ctx inp0 inp1
  failWhen failed (not needEnter)
  _ <- runFrame ctx inp1 ui
  let
    drain = inp1 {inputDeltaTime = 1}
  _ <- runFrame ctx drain ui
  needStay <- needsRedraw ctx drain inp2
  when needStay $ bump failed
  let
    inpClick = inp1 {inputMouseDown = True, inputMousePressed = True}
  needClick <- needsRedraw ctx drain inpClick
  failWhen failed (not needClick)

runHoverDamageTest :: Context -> IORef Int -> IO ()
runHoverDamageTest _ failed = do
  ctx <- newContext
  let
    ui = column defaultLayout (button "OK")
    inp0 =
      withInputOff 240 80
  _ <- runFrame ctx inp0 ui
  d0 <- takeDamage ctx
  when (d0 /= DamageFull) $ bump failed
  (resp, _, _, _) <- runFrame ctx inp0 ui
  let
    Rect rx ry rw rh = respRect resp
    inside = V2 (rx + rw / 2) (ry + rh / 2)
    inp1 = inp0 {inputMousePos = inside}
  _ <- runFrame ctx inp1 ui
  d1 <- takeDamage ctx
  case d1 of
    DamageFull -> bump failed
    DamageClip (Rect _ _ w h) ->
      when (w * h >= 240 * 80 * 0.5) $ bump failed
  let
    inpClick = inp1 {inputMouseDown = True, inputMousePressed = True}
  _ <- runFrame ctx inpClick ui
  d2 <- takeDamage ctx
  when (d2 /= DamageFull) $ bump failed


runAsciiTest :: Context -> IORef Int -> IO ()
runAsciiTest ctx failed = do
  let
    inp = withInput 40 10
  (_, _, draw, _) <-
    runFrame
      ctx
      inp
      ( column
          (defaultLayout {layoutWidth = Grow 1, layoutHeight = Grow 1})
          (label "snap")
      )
  let
    ascii = renderASCII 40 10 draw
  when (length ascii /= 10) $ bump failed
  when (all (all (== ' ')) ascii) $ bump failed

runVtTest :: Context -> IORef Int -> IO ()
runVtTest _ failed = do
  let
    ck cond = when (not cond) (bump failed)
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
  let
    ck cond = when (not cond) (bump failed)
    inp = withInput 200 80
    ui =
      column
        (defaultLayout {layoutWidth = Grow 1, layoutHeight = Grow 1})
        (label "hello")
  (_, _, draw, _) <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  cells <- rasterize 40 10 draw spans
  let
    rows = cellRows cells
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
  -- Nerd font / Font Awesome icons live in the private use area and must pass
  -- the same filter, or the glyph tier would render blanks.
  ck (all narrowChar (concatMap T.unpack (glyphIconTexts glyphIcons)))
  ck (terminalTextColumns "\xf046" == 2)
  ck (terminalPaintColumns "\xf046" == 1)
  ck (terminalPaintColumns (iconClose glyphIcons) == 1)
  ck (terminalTextColumns (iconClose glyphIcons) == 2)
  ck (terminalTextColumns (iconChecked glyphIcons) == 3)
  ck (terminalPaintColumns (iconChecked glyphIcons) == 3)
  ck (terminalTextColumns (iconUnchecked glyphIcons) == 3)

glyphIconTexts :: Icons -> [T.Text]
glyphIconTexts icons =
  [ iconChecked icons
  , iconUnchecked icons
  , iconClose icons
  , iconSelectOpen icons
  , iconSelectClosed icons
  , iconScrollUp icons
  , iconScrollDown icons
  , iconWindowTitle icons
  , iconModalTitle icons
  ]

runIconSetTest :: Context -> IORef Int -> IO ()
runIconSetTest _ failed = do
  let
    ck cond = when (not cond) (bump failed)
  ck (parseIconSet "nerd" == Just IconsNerd)
  ck (parseIconSet "FontAwesome" == Just IconsFontAwesome)
  ck (parseIconSet " ascii " == Just IconsAscii)
  ck (parseIconSet "auto" == Nothing)
  ck (iconsFor IconsAscii == asciiIcons)
  ck (iconsFor IconsNerd == glyphIcons)
  ck (iconsFor IconsFontAwesome == glyphIcons)
  ck (checkboxMark glyphIcons True == iconChecked glyphIcons)
  ck
    ( terminalTextColumns (iconChecked glyphIcons)
        == terminalTextColumns (iconUnchecked glyphIcons)
    )

runCheckboxTest :: Context -> IORef Int -> IO ()
runCheckboxTest ctx failed = do
  let
    inp0 = withInput 200 100
    ui = column defaultLayout (checkbox "Opt" False)
  (resp, _) <- warmup2 ctx inp0 ui
  let
    Rect rx ry _ _ = respRect resp
    click = V2 (rx + 1) (ry + 0.5)
  let
    inpPress =
      inp0
        { inputMousePos = click
        , inputMouseDown = True
        , inputMousePressed = True
        }
  _ <- runFrame ctx inpPress ui
  let
    inpRelease =
      inpPress
        { inputMouseDown = False
        , inputMousePressed = False
        , inputMouseReleased = True
        }
  ((_, checked), _, _, _) <- runFrame ctx inpRelease ui
  failWhen failed (not checked)

runSliderTest :: Context -> IORef Int -> IO ()
runSliderTest ctx failed = do
  let
    inp0 = withInput 300 80
    ui = column defaultLayout (slider "Vol" 0 100 10)
  (resp, _) <- warmup2 ctx inp0 ui
  let
    Rect rx ry rw rh = respRect resp
    track = sliderTrackBounds (ctxHostProfile ctx) (ctxFontMetrics ctx) "Vol" rx ry rw rh
    drag = V2 (rectX track + rectW track * 0.75) (rectY track + rectH track / 2)
  let
    inpDrag =
      inp0
        { inputMousePos = drag
        , inputMouseDown = True
        , inputMousePressed = True
        }
  ((_, val), _, _, _) <- runFrame ctx inpDrag ui
  when (val <= 10) $ bump failed

runSliderFillWidthTest :: Context -> IORef Int -> IO ()
runSliderFillWidthTest ctx failed = do
  let
    inp0 = withInput 400 120
    ui = column (fillW defaultLayout) (slider "Vol" 0 100 0)
  (resp, _) <- warmup2 ctx inp0 ui
  let
    Rect rx ry rw rh = respRect resp
  when (rw < 300) $ bump failed
  let
    track = sliderTrackBounds (ctxHostProfile ctx) (ctxFontMetrics ctx) "Vol" rx ry rw rh
    endDrag =
      V2 (rectX track + rectW track - 2) (rectY track + rectH track / 2)
    inpDrag =
      inp0
        { inputMousePos = endDrag
        , inputMouseDown = True
        , inputMousePressed = True
        }
  ((_, val), _, _, _) <- runFrame ctx inpDrag ui
  when (val < 90) $ bump failed

runTabFocusTest :: Context -> IORef Int -> IO ()
runTabFocusTest ctx failed = do
  let
    inp0 = withInput 200 120
    ui =
      column defaultLayout $ do
        _ <- button "One"
        _ <- button "Two"
        pure ()
  _ <- runFrame ctx inp0 ui
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyTab]}) ui
  focus1 <- getFocusId ctx
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyTab]}) ui
  focus2 <- getFocusId ctx
  when (focus1 == WidgetId 0 || focus2 == WidgetId 0 || focus1 == focus2) $
    bump failed

runTextWrapTest :: Context -> IORef Int -> IO ()
runTextWrapTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let
    inp = withInput 40 10
    long = T.replicate 24 (T.pack "x")
    ui = labelEx (defaultLayout {layoutMaxW = 8}) long
  _ <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  when (length spans < 3) $ bump failed

runTextWrapAssignedTest :: Context -> IORef Int -> IO ()
runTextWrapAssignedTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let
    inp = withInput 20 12
    long = T.replicate 24 (T.pack "x")
    ui =
      column
        ( defaultLayout
            { layoutWidth = Fixed 8
            , layoutPadding = Padding 0 0 0 0
            , layoutGap = 0
            }
        )
        $ labelEx (defaultLayout {layoutWidth = Grow 1}) long
  _ <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  when (length spans < 3) $ bump failed

runTextMultilineTest :: Context -> IORef Int -> IO ()
runTextMultilineTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let
    inp = withInput 40 10
    ui = labelEx (tight defaultLayout) (monoFontMarker <> "aa\nbb\ncc")
  _ <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  let
    rows = sort (map (\(Rect _ y _ _, txt, _, _, _) -> (round y :: Int, txt)) spans)
  when (map snd rows /= ["aa", "bb", "cc"]) $ bump failed
  case map fst rows of
    [a, b, c] -> when (b /= a + 1 || c /= b + 1) $ bump failed
    _ -> bump failed

runFlexWrapTest :: Context -> IORef Int -> IO ()
runFlexWrapTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let
    inp = withInput 30 10
    ui =
      row
        ( defaultLayout
            { layoutWrap = True
            , layoutWidth = Fixed 4
            , layoutGap = 0
            , layoutPadding = Padding 0 0 0 0
            }
        )
        ( do
            _ <- label "AA"
            _ <- label "BB"
            _ <- label "CC"
            _ <- label "DD"
            pure ()
        )
  _ <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  let
    ys = nub (map (\(Rect _ y _ _, _, _, _, _) -> (round y :: Int)) spans)
  when (length ys < 2) $ bump failed

runFlexShrinkTest :: Context -> IORef Int -> IO ()
runFlexShrinkTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let
    inp = withInput 20 10
    ui =
      row
        ( defaultLayout
            { layoutWidth = Fixed 5
            , layoutGap = 0
            , layoutPadding = Padding 0 0 0 0
            }
        )
        ( do
            _ <- labelEx (defaultLayout {layoutWidth = Shrink 1}) "AA"
            _ <- labelEx (defaultLayout {layoutWidth = Shrink 1}) "BB"
            _ <- labelEx (defaultLayout {layoutWidth = Shrink 1}) "CC"
            pure ()
        )
  _ <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  when (length spans /= 3) $ bump failed
  let
    lastX = maximum (map (\(Rect x _ _ _, _, _, _, _) -> x) spans)
  when (lastX > 3.5) $ bump failed

runGrowFitsWindowTest :: Context -> IORef Int -> IO ()
runGrowFitsWindowTest ctx failed = do
  let
    inp = withInput 10 10
    ui =
      row
        ( defaultLayout
            { layoutWidth = Grow 1
            , layoutHeight = Grow 1
            , layoutPadding = Padding 0 0 0 0
            , layoutGap = 0
            }
        )
        $ do
          a <- spacer (Grow 1) (Fixed 8)
          b <- spacer (Grow 1) (Fixed 8)
          pure (a, b)
  (ra, rb) <- warmup2 ctx inp ui
  let
    Rect x1 _ w1 _ = respRect ra
    Rect x2 _ w2 _ = respRect rb
  when (w1 <= 0 || w2 <= 0) $ bump failed
  when (x1 < -0.01 || x2 + w2 > 10.01) $ bump failed
  when (abs (w1 - w2) > 0.5) $ bump failed

runPercentLayoutTest :: Context -> IORef Int -> IO ()
runPercentLayoutTest ctx failed = do
  let
    inp = withInput 200 80
    ui =
      row (fixedW 200 . tight . gap 0 $ defaultLayout) $ do
        a <- labelEx (percent 25 . tight $ defaultLayout) "A"
        b <- labelEx (percent 75 . tight $ defaultLayout) "B"
        pure (a, b)
  (a, b) <- warmup2 ctx inp ui
  let
    Rect _ _ wa _ = respRect a
    Rect _ _ wb _ = respRect b
  when (abs (wa - 50) > 1) $ bump failed
  when (abs (wb - 150) > 1) $ bump failed

runLabelAlignEndTest :: Context -> IORef Int -> IO ()
runLabelAlignEndTest _ failed = do
  checkLabelAlignEnd failed =<< newAdaptiveTerminalContext
  checkLabelAlignEnd failed =<< newPixelContext

runAspectLayoutTest :: Context -> IORef Int -> IO ()
runAspectLayoutTest ctx failed = do
  let
    inp = withInput 320 240
    ui =
      column (fixedW 160 . tight $ defaultLayout) $
        labelEx (fillW . aspect 2 . tight $ defaultLayout) "X"
  resp <- warmup2 ctx inp ui
  let
    Rect _ _ w h = respRect resp
  when (abs (w - 160) > 1) $ bump failed
  when (abs (h - 80) > 1) $ bump failed

runGrowWrapPushesSiblingTest :: Context -> IORef Int -> IO ()
runGrowWrapPushesSiblingTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let
    inp = withInput 6 20
    ui =
      column
        ( defaultLayout
            { layoutWidth = Grow 1
            , layoutHeight = Grow 1
            , layoutPadding = Padding 0 0 0 0
            , layoutGap = 0
            }
        )
        $ do
          row
            ( defaultLayout
                { layoutWidth = Grow 1
                , layoutWrap = True
                , layoutPadding = Padding 0 0 0 0
                , layoutGap = 0
                }
            )
            $ do
              _ <- label "AAAA"
              _ <- label "BBBB"
              pure ()
          label "BELOW"
  _ <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  let
    ysFor t = [y | (Rect _ y _ _, txt, _, _, _) <- spans, txt == t]
  case (ysFor "BBBB", ysFor "BELOW") of
    ([by], [sy]) -> when (sy < by + 0.5) $ bump failed
    _ -> bump failed


runHostSlotTest :: Context -> IORef Int -> IO ()
runHostSlotTest ctx failed = do
  let
    inp = withInput 80 80
  (miss, _, _, _) <- runFrame ctx inp (askHost :: NanoUI (Maybe String))
  setHost ctx ("ok" :: String)
  setHost ctx (1 :: Int)
  (hitS, _, _, _) <- runFrame ctx inp (askHost :: NanoUI (Maybe String))
  (hitI, _, _, _) <- runFrame ctx inp (askHost :: NanoUI (Maybe Int))
  when (miss /= Nothing || hitS /= Just "ok" || hitI /= Just 1) $ bump failed

runCompactHostTest :: Context -> IORef Int -> IO ()
runCompactHostTest ctx failed = do
  let
    payload = [0 .. 9999] :: [Int]
  _ <- compactHost ctx payload
  let
    inp = withInput 80 80
  (got, _, _, _) <- runFrame ctx inp (askCompact :: NanoUI (Maybe [Int]))
  case got of
    Just xs | length xs == 10000 && last xs == 9999 -> pure ()
    _ -> bump failed

runEmbedStateTest :: Context -> IORef Int -> IO ()
runEmbedStateTest ctx failed = do
  let
    inp = withInput 80 80
    ui :: Eff '[Ui, State Int, IOE] Int
    ui = do
      modify (+ (1 :: Int))
      modify (+ (1 :: Int))
      get
  (n, _, _, _) <- runFrameEff (runEff . evalState (0 :: Int)) ctx inp ui
  when (n /= 2) $ bump failed

data CounterMsg = Inc | Dec
  deriving (Eq, Show)

data Counter = Counter {counterN :: Int}
  deriving (Eq, Show)

updateCounter :: CounterMsg -> Counter -> Counter
updateCounter Inc m = m {counterN = counterN m + 1}
updateCounter Dec m = m {counterN = counterN m - 1}

runReduceMessagesTest :: Context -> IORef Int -> IO ()
runReduceMessagesTest ctx failed = do
  let
    inp = withInput 80 80
    model0 = Counter 0
    view _ = do
      emit Inc
      emit Dec
      emit Inc
      emit ("noise" :: String)
  ((), model1, msgs, _, dirty) <- runFrameReduce updateCounter ctx inp model0 view
  when (msgs /= [Inc, Dec, Inc] || model1 /= Counter 1 || not dirty) $ bump failed

runReduceUpdatesTest :: Context -> IORef Int -> IO ()
runReduceUpdatesTest ctx failed = do
  let
    inp = withInput 80 80
    ui = do
      emit (updateCounter Inc)
      emit (updateCounter Dec)
      emit (updateCounter Inc)
  (_, msgs, _, _) <- runFrame ctx inp ui
  let
    model1 = reduceUpdates (Counter 0) msgs
  when (model1 /= Counter 1) $ bump failed

runReduceClickTest :: Context -> IORef Int -> IO ()
runReduceClickTest ctx failed = do
  let
    inp0 = withInput 240 120
    view m = do
      resp <- button "Go"
      onClick resp (emit Inc)
      label_ (T.pack (show (counterN m)))
      pure resp
  (resp, model0, _, _, _) <-
    runFrameReduce updateCounter ctx inp0 (Counter 0) view
  when (model0 /= Counter 0) $ bump failed
  (modelR, msgs, dirty) <-
    runClickReduce updateCounter ctx inp0 (Counter 0) view (centerOf resp)
  when (msgs /= [Inc] || modelR /= Counter 1 || not dirty) $ bump failed
  (_, model1, _, _, _) <- runFrameReduce updateCounter ctx inp0 modelR view
  when (model1 /= Counter 1) $ bump failed

runReduceIdentityTest :: Context -> IORef Int -> IO ()
runReduceIdentityTest ctx failed = do
  let
    inp = withInput 80 80
    view _ = do
      emit Inc
      emit Dec
  ((), model1, msgs, _, dirty) <-
    runFrameReduce updateCounter ctx inp (Counter 0) view
  when (msgs /= [Inc, Dec] || model1 /= Counter 0 || dirty) $ bump failed

runWidgetNoStringEmitTest :: Context -> IORef Int -> IO ()
runWidgetNoStringEmitTest ctx failed = do
  let
    inp0 = withInput 240 120
    ui = button "Go"
  (resp, _, _, _) <- runFrame ctx inp0 ui
  let
    Rect x y w h = respRect resp
    pos = V2 (x + w / 2) (y + h / 2)
    press =
      inp0
        { inputMousePos = pos
        , inputMouseDown = True
        , inputMousePressed = True
        , inputMouseReleased = False
        }
    release =
      press
        { inputMousePressed = False
        , inputMouseDown = False
        , inputMouseReleased = True
        }
  _ <- runFrame ctx press ui
  (_, msgs, _, _) <- runFrame ctx release ui
  when (not (null (decodeMessages msgs :: [String]))) $ bump failed

runUseFlagClickTest :: Context -> IORef Int -> IO ()
runUseFlagClickTest ctx failed = do
  let
    inp0 = withInput 240 120
    ui = do
      (readOpen, setOpen) <- useFlag False
      (readNote, setNote) <- useText ""
      resp <- button "Go"
      onClick resp $ do
        setOpen True
        setNote "hi"
      open <- readOpen
      note <- readNote
      pure (open, note, resp)
  (open0, note0, resp) <- warmup2 ctx inp0 ui
  when (open0 || note0 /= "") $ bump failed
  let
    Rect x y w h = respRect resp
    pos = V2 (x + w / 2) (y + h / 2)
    press =
      inp0
        { inputMousePos = pos
        , inputMouseDown = True
        , inputMousePressed = True
        , inputMouseReleased = False
        }
    release =
      press
        { inputMousePressed = False
        , inputMouseDown = False
        , inputMouseReleased = True
        }
  _ <- runFrame ctx press ui
  ((open1, note1, _), _, _, _) <- runFrame ctx release ui
  failWhen failed (not open1 || note1 /= "hi")

runPanelPaintsTest :: Context -> IORef Int -> IO ()
runPanelPaintsTest ctx failed = do
  let
    inp = withInput 200 200
    fat = padAll 16 (fillW defaultLayout)
  (_, _, colDraw, _) <- runFrame ctx inp (column fat (label "x"))
  (_, _, panDraw, _) <- runFrame ctx inp (panel fat (label "x"))
  when (drawVertexCount panDraw <= drawVertexCount colDraw) $ bump failed



