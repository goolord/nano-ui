{-# OPTIONS_GHC -O0 #-}
module Cases.Window (tests) where

import Spec
import Control.Exception (evaluate)
import Data.IntMap.Strict qualified as IM
import Data.Maybe (isJust, isNothing, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import NanoUI.Internal.Context (Context (..))
import NanoUI.Internal.Layout.Arena (LayoutCache (..))
import NanoUI.Testing.Sim
import System.Mem.StableName (makeStableName)

tests :: [Spec]
tests =
  [ spec "fit-header-no-shrink" runFitHeaderNoShrinkTest
  , pixelSimSpec "window-scroll-gutter" 640 360 gutterUi windowScrollGutter
  , simSpec "window-fit-scroll-gutter" 1200 300 fitGutterUi windowFitScrollGutter
  , simSpec "heading-mono-truncate" 1600 600 monoUi headingMonoTruncate
  , spec "window-overlay" runWindowOverlayTest
  , spec "overlay-sibling-state" runOverlaySiblingStateTest
  , spec "window-damage" runWindowDamageTest
  , simSpec "window-drag" 640 400 (fst <$> window' (constVar True) "Debug" (label "Body")) windowDrag
  , simSpec "window-resize" 640 400 resizeUi windowResize
  , simSpec "window-resize-halo-scrolled" 640 400 haloUi haloScrolled
  , spec "window-layout-reuse" runWindowLayoutReuseTest
  , spec "window-custom-measure-placement" runWindowCustomMeasurePlacementTest
  , spec "page-window-scroll" runPageWindowScrollTest
  , simSpec "scrolled-debug-toggle" 640 400 debugToggleUi $
      click "Debug" >> expect "fps" >> frame >> expect "fps"
  , spec "span-beside-window" runSpanBesideWindowTest
  , spec "caption-drag-spans" runDragSpansTest
  , simSpec "caption-buttons" 400 200 captionUi captionButtonsClose
  ]

-- | Drag from @a@ to @b@, settle, and give @wid@'s rect.
dragRect :: WidgetId -> V2 -> V2 -> Sim a Rect
dragRect wid a b = drag (At a) (At b) 1 >> settle >> prevRect wid

rightOf :: Rect -> Float
rightOf (Rect x _ w _) = x + w

-- | Labels "line 1" to "line n".
numbered :: Int -> NanoUI ()
numbered n = forM_ [1 .. n] $ \i -> label (T.pack ("line " <> show i))

openWindow :: Text -> NanoUI a -> NanoUI (Response, Maybe a)
openWindow = window' (constVar True)

-- | A fit-height header keeps its height when a scroller below takes the rest.
runFitHeaderNoShrinkTest :: Context -> IORef Int -> IO ()
runFitHeaderNoShrinkTest ctx failed = do
  let header = panelWith (padXY 16 12 . fillW) (label' "nano-ui SDL3 demo")
      withBody = columnWith (padAll 12 . gap 8 . grow) $
        header <* scrollWith (tight . grow) (columnWith fillW (numbered 40))
  alone <- warmup2 ctx (withInput 400 800) (columnWith (padAll 12 . grow) header)
  squeezed <- warmup2 ctx (withInput 400 200) withBody
  assert failed (rectH (respRect squeezed) + 0.5 >= rectH (respRect alone))

gutterUi :: NanoUI (Response, Maybe Response)
gutterUi = openWindow "GutterWin" $ do
  wide <- labelWith' fillW "WWWW"
  kv "Key" (T.replicate 48 "M")
  wide <$ numbered 24

-- | A scrolling window's body keeps the window's side padding before the bar.
windowScrollGutter :: Sim (Response, Maybe Response) ()
windowScrollGutter = do
  (win, mwide) <- result
  ctx <- simContext
  failed <- simFailures
  checkJust mwide $ \wide -> liftIO $
    assertScrollGutterPad failed ctx (respId win) wide (scrollBarGutter ScrollBarWindow 0) (padR windowPad)

fitGutterUi :: NanoUI Response
fitGutterUi = fmap fst . openWindow "Debug" . columnWith (tight . gap 4 . minW 300 . fillW) $
  forM_ [1 .. 30 :: Int] $ \i -> kvMono (T.pack ("row " <> show i)) (T.pack ("value" <> show i))

-- | A scrolling window fitted to a minimum-width body ends right-aligned values before its bar.
windowFitScrollGutter :: Sim Response ()
windowFitScrollGutter = do
  win <- respRect <$> result
  values <- (\ss -> [rightOf r | (r, t, _, _, _) <- ss, "value" `T.isPrefixOf` t]) <$> spans
  let contentRight = rightOf win - padR windowPad - scrollBarGutter ScrollBarWindow 0
  check (not (null values) && all (<= contentRight + 0.5) values)

longPath :: Text
longPath = "C:\\Users\\zach\\AppData\\Local\\Microsoft\\Windows\\Fonts\\JetBrainsMono-Regular.ttf"

monoUi :: NanoUI Response
monoUi = fst <$> openWindow "Debug" (heading "Draw" >> label "NormalLabel" >> kvMono "font" longPath)

-- | A long mono value is cut with "..." at the content edge, and whole there once widened.
headingMonoTruncate :: Sim Response ()
headingMonoTruncate = do
  let fontSpan = do
        ss <- spans
        case [(r, t) | (r, t, _, _, _) <- ss, "JetBrainsMono" `T.isInfixOf` t || "..." `T.isInfixOf` t] of
          [found] -> pure found
          other -> fail ("font spans: " <> show other)
      contentRight r = rightOf r - padR windowPad
  win <- result
  r0@(Rect x0 y0 _ h0) <- prevRect (respId win)
  (f0, t0) <- fontSpan
  check ("..." `T.isSuffixOf` t0 && abs (rightOf f0 - contentRight r0) < 2)
  r1 <- dragRect (respId win) (V2 (x0 - 4) (y0 + h0 / 2)) (V2 (x0 - 1100) (y0 + h0 / 2))
  checkGt (rectW r1) (rectW r0 + 800)
  (f1, t1) <- fontSpan
  checkEq longPath t1
  check (abs (rightOf f1 - contentRight r1) < 2)

-- | A point inside @cover@ below its title bar and clear of @kids@.
safePoint :: Rect -> [Rect] -> Maybe V2
safePoint cover@(Rect x y w h) kids =
  listToMaybe [p | p <- candidates, rectContains cover p, not (any (`rectContains` p) kids)]
  where
    candidates =
      [V2 (x + 6) (y + h * 0.72), V2 (x + w - 6) (y + h * 0.72), V2 (x + w / 2) (y + h - 6)]
        ++ [V2 (x + 6) (y + h - 6), V2 (x + w - 6) (y + 46)]

-- | An open window shows its title and body, leaves the page clickable, ignores
-- Escape and reports its close button; a closed one draws nothing. A press on a
-- window never reaches a lower window; its own button clicks.
runWindowOverlayTest :: Context -> IORef Int -> IO ()
runWindowOverlayTest ctx failed = do
  openRef <- newIORef False
  let ui = do
        outside <- button' "Outside"
        open <- liftIO (readIORef openRef)
        (win, body) <- window' (constVar open) "Debug" (label "Body")
        pure (outside, win, body)
  runSim ctx failed (withInput 640 400) ui $ do
    (_, closed, noBody) <- result
    check (not (respClicked closed) && isNothing noBody)
    expectNot "Debug"
    liftIO (writeIORef openRef True)
    (_, win, body) <- settle
    expect "Debug" >> expect "Body"
    checkEq Nothing =<< findSpan "X"
    check (rectW (respRect win) >= 100 && rectH (respRect win) >= 20 && isJust body)
    (outsideHit, _, _) <- click "Outside"
    check (respClicked outsideHit)
    (_, afterEsc, _) <- pressKey KeyEscape
    check (not (respClicked afterEsc))
    Rect px py pw _ <- maybe (respRect win) fst . IM.minView <$> liftIO (floatingPanelRects ctx)
    (_, closing, _) <- click (at (px + pw - padR windowPad - 12.5) (py + padT windowPad + 19.5))
    check (respClicked closing)
  let stacked = do
        (_, lo) <- openWindow "Low" (button' "LowBtn")
        (hi, hiBtn) <- openWindow "High" (button' "HighBtn")
        pure (lo, hi, hiBtn)
  c <- newContext
  runSim c failed (withInput 300 220) stacked $ do
    (lo, cover, inside) <- result
    checkJust ((,) <$> lo <*> inside) $ \(low, btn) -> do
      checkJust (safePoint (respRect cover) [respRect btn, respRect low]) $ \p -> do
        (through, _, _) <- click (At p)
        check (not (maybe False respClicked through))
      (_, _, hit) <- click (On btn)
      check (maybe False respClicked hit)

-- | Opening a modal or window keeps the ids, and so the state, of later widgets.
runOverlaySiblingStateTest :: Context -> IORef Int -> IO ()
runOverlaySiblingStateTest _ failed = do
  let inp = withInput 640 400
      ui overlay open edit = do
        _ <- overlay (constVar open) "About" (label "body")
        txtVar <- useVar ("start" :: Text)
        when edit (writeVar txtVar "edited")
        readVar txtVar
  forM_ [modal, window] $ \overlay -> do
    ctx <- newContext
    _ <- runFrame ctx inp (ui overlay False True)
    forM_ [False, True] $ \open -> assertEq failed "edited" =<< warmup2 ctx inp (ui overlay open False)

-- | Static overlays repaint nothing; closing one or a dirtied modal repaints all, a
-- scroll repaints a clip over the window, and churning content less than all.
runWindowDamageTest :: Context -> IORef Int -> IO ()
runWindowDamageTest _ failed = do
  let inp = withInputOff 640 400
      debug open body = fst <$> window' (constVar open) "Debug" body
      about = void (modal' (constVar True) "About" (label "body"))
      fresh ui = newContext >>= \ctx -> (,) ctx <$> warmup2 ctx inp ui
      damageOf ctx i ui = runFrame ctx i ui >> takeDamage ctx
  forM_ [void (debug True (label "fps 0")), about] $ \ui -> do
    (ctx, _) <- fresh ui
    assert failed . not =<< needsRedraw ctx inp inp
    assert failed . damageIsEmpty =<< damageOf ctx inp ui
  (closing, _) <- fresh (debug True (label "Body"))
  assertEq failed DamageFull =<< damageOf closing inp (debug False (label "Body"))
  assert failed =<< needsRedraw closing inp inp {inputDeltaTime = 1}
  dirtied <- newContext
  _ <- runFrame dirtied inp about
  markDirty dirtied
  assert failed =<< needsRedraw dirtied inp inp
  assertEq failed DamageFull =<< damageOf dirtied inp about
  let scrolling = debug True (column (numbered 30))
      visible (Rect _ _ w h) = w > 0 && h > 0
  (scrolled, win) <- fresh scrolling
  damageOf scrolled inp {inputMousePos = centerOf win, inputScroll = V2 0 1} scrolling >>= \case
    DamageClip r -> assert failed (maybe False visible (rectIntersect r (respRect win)))
    _ -> assert failed False
  let churn k = button "Outside" >> debug True (columnWith (tight . gap 4 . minW 300 . fillW) $ do
        label (T.replicate (1 + k `mod` 9) "M")
        label "static row")
  (churning, _) <- fresh (churn 0)
  forM_ [1 .. 30 :: Int] $ \k -> assert failed . (/= DamageFull) =<< damageOf churning inp (churn k)

-- | A title drag moves the window; held still past the left edge it stops there, idle.
windowDrag :: Sim Response ()
windowDrag = do
  r0 <- respRect <$> result
  let V2 gx gy = windowTitleGrab r0
  _ <- press (at gx gy) >> moveTo (at (gx - 50) (gy + 30))
  checkEq DamageFull =<< frameDamage
  r1 <- respRect <$> (release >> settle)
  check (rectX r1 < rectX r0 - 10 && rectY r1 > rectY r0 + 10)
  let V2 hx hy = windowTitleGrab r1
      edge = at (hx - rectX r1 - 50) hy
  _ <- press (at hx hy) >> moveTo edge >> moveTo edge
  checkEq 0 . rectX . respRect =<< moveTo edge
  check . not =<< wantsFrame

resizeUi :: NanoUI (Response, Response)
resizeUi = (,) <$> button' "Hit" <*> (fst <$> openWindow "Resize" (label "Body"))

-- | A window's margins, out to a halo and in to its edges, show resize cursors and
-- drag to resize it, down to its title bar. A page button in the halo keeps its cursor.
windowResize :: Sim (Response, Response) ()
windowResize = do
  (btn, win) <- result
  Rect x0 y0 w0 h0 <- prevRect (respId win)
  let (r, b, cx, cy) = (x0 + w0, y0 + h0, x0 + w0 / 2, y0 + h0 / 2)
      edge = dragRect (respId win)
  forM_
    [ (UiCursorNwseResize, [(r + 4, b + 4), (x0 - 4, y0 - 4), (r - 5, b - 5), (x0 + 5, y0 + 5), (x0 - 4, y0 + 8)])
    , (UiCursorNeswResize, [(r + 4, y0 - 4), (x0 - 4, b + 4), (x0 + 5, b - 5)])
    , (UiCursorNsResize, [(cx, y0 - 4), (cx, b + 4), (cx, b - 5), (cx, y0 + 3)])
    , (UiCursorEwResize, [(x0 - 4, cy), (r + 4, cy), (r - 5, cy), (x0 + 5, cy)])
    ]
    $ \(kind, ps) -> forM_ ps $ \(x, y) -> checkEq ((x, y), kind) . (,) (x, y) =<< cursorAt (at x y)
  check . (/= UiCursorNsResize) =<< cursorAt (at cx (y0 + 20))
  check . (/= UiCursorEwResize) =<< cursorAt (at (r - padR windowPad - 4) cy)
  Rect x1 y1 w1 h1 <- edge (V2 (r + 4) (b + 4)) (V2 (r + 40) (b + 30))
  check (w1 > w0 + 20 && h1 > h0 + 15)
  Rect xw yw ww hw <- edge (V2 (x1 - 4) (y1 + h1 / 2)) (V2 (x1 - 36) (y1 + h1 / 2))
  check (ww > w1 + 15 && xw < x1 - 10)
  Rect xn yn wn hn <- edge (V2 (xw + ww / 2) (yw - 4)) (V2 (xw + ww / 2) (yw - 20))
  check (hn > hw + 8 && yn < yw - 5)
  Rect xs ys ws hs <- edge (V2 (xn + wn / 2) (yn + hn + 4)) (V2 (xn + wn / 2) (yn + 4))
  check (hs + 0.01 >= 39 + padT windowPad + padB windowPad)
  -- A press in the left margin, inside the window, resizes.
  Rect xi yi wi _ <- edge (V2 (xs + 5) (ys + hs / 2)) (V2 (xs - 25) (ys + hs / 2))
  check (wi > ws + 20 && xi < xs - 20)
  -- Dragged by its title to 4 px right of the page button, its halo covers the button.
  let Rect bx by bw bh = respRect btn
      destX = bx + bw + 4
      hit@(V2 hx hy) = V2 (bx + bw - 2) (by + bh - 2)
  Rect x2 y2 _ h2 <- edge (V2 (xi + 24) (yi + 22)) (V2 (destX + 24) (yi + 22))
  check (abs (x2 - destX) <= 8 && hx < x2 && hx >= x2 - 12 && hy >= y2 - 12 && hy <= y2 + h2 + 12)
  kind <- cursorAt (At hit)
  check (kind `notElem` [UiCursorEwResize, UiCursorNsResize, UiCursorNwseResize, UiCursorNeswResize])

haloUi :: NanoUI WidgetId
haloUi = do
  (page, ()) <- scrollArea (fillW . fillH) . columnWith fillW $ do
    spacer Fit (Fixed 400)
    replicateM_ 10 (buttonWith (fillW . fixedH 100) "Page")
  page <$ window (constVar True) "Tool" (label "Body")

-- | A page button in a window's halo keeps the press, hit where the scrolled page draws it.
haloScrolled :: Sim WidgetId ()
haloScrolled = do
  let windows = map snd <$> onContext (nodesOfType NodeWindow)
  page <- result
  onContext (\c -> setScrollOffset c page 400)
  before <- settle >> windows
  checkJust (listToMaybe before) $ \(Rect x y _ h) -> do
    _ <- drag (at (x - 6) (y + h / 2)) (at (x - 60) (y + h / 2)) 1
    checkEq before =<< (settle >> windows)

-- | Reusing the layout cache with floating panels open lays out as a fresh solve: still,
-- with a popup moving in the window, mid-drag, resized, at another size, under a modal.
runWindowLayoutReuseTest :: Context -> IORef Int -> IO ()
runWindowLayoutReuseTest ctx failed = do
  fresh <- newContext
  let inp0 = withInput 640 400
      ui modalOpen anchor = do
        column $ forM_ [1 .. 20 :: Int] $ \i -> void (button (T.pack ("row " <> show i)))
        win <- fmap fst $ openWindow "Tools" $ columnWith (maxW 260) $ do
          forM_ [1 .. 12 :: Int] $ \i -> labelWith fillW (T.replicate i "wrapping words ")
          void (button "ok")
          let cfg = (defaultPopupConfig (AnchorPoint anchor)) {cfgDismissable = False}
          void (popup' (constVar True) cfg (label "tip"))
        void (modal' (constVar modalOpen) "Note" (label (T.replicate 12 "a note that wraps ")))
        pure win
      -- A frame on this context, which reuses what it placed, and on one that never does.
      same i u = respRect <$> sameAsFreshSolve failed fresh ctx i u
      still = ui False (V2 100 100)
  r0 <- same inp0 still >> same inp0 still
  -- The window and the popup in it are both kept for the next frame.
  _ <- same inp0 still
  cache <- readIORef (ctxLayoutCache ctx)
  assertEq failed 2 . IM.size =<< maybe (pure IM.empty) (\(c, _, _) -> readIORef (lcPlaced c)) cache
  forM_ [1 .. 3 :: Int] $ \k -> forM_ [0, 3] $ \d ->
    same inp0 (ui False (V2 (100 + 7 * fromIntegral k + d) 120))
  let V2 gx gy = windowTitleGrab r0
  _ <- same (pressAt inp0 (V2 gx gy)) still
  forM_ [1 .. 4 :: Int] $ \k ->
    same (holdAt inp0 (V2 (gx - 20 * fromIntegral k) (gy + 10 * fromIntegral k))) still
  r1@(Rect x1 y1 w1 h1) <- same (releaseAt inp0 {inputMousePos = V2 (gx - 80) (gy + 40)}) still
  assert failed (x1 < rectX r0 - 40 && y1 > rectY r0)
  let edgeAt = V2 (x1 + w1 + 4) (y1 + h1 / 2)
      dest = v2Add edgeAt (V2 30 0)
  mapM_ (`same` still) [pressAt inp0 edgeAt, holdAt inp0 dest, inp0 {inputMousePos = dest}]
  r2 <- same inp0 {inputMousePos = dest} still
  assertGt failed (rectW r2) (rectW r1 + 20)
  let small = withInput 520 360
  forM_ [(small, still), (small, ui True (V2 100 100))] $ \(i, u) -> replicateM_ 3 (same i u)

-- | A custom widget's measure changing only at a window's inner width reuses the root
-- solve and re-places the window instead of keeping its cached placement.
runWindowCustomMeasurePlacementTest :: Context -> IORef Int -> IO ()
runWindowCustomMeasurePlacementTest ctx failed = do
  let inp = withInput 640 400
      ui h = column $ fmap snd $ openWindow "Tools" $ column $ do
        spacer (Fixed 240) (Fixed 1)
        fst <$> customWidget defaultCustomWidgetSpec
          { widgetMeasure = Just (\_ (w, _) -> if w >= 1e8 then (100, 20) else (w, h))
          , widgetLayout = fillW defaultLayout
          }
      heightOf = maybe 0 (rectH . respRect)
      cacheName = readIORef (ctxLayoutCache ctx) >>= evaluate >>= makeStableName
  assertEq failed 30 . heightOf =<< warmup2 ctx inp (ui 30)
  cache <- cacheName
  _ <- runFrame ctx inp (ui 60)
  assert failed . (== cache) =<< cacheName
  fresh <- newContext
  _ <- warmup2 fresh inp (ui 60)
  assertEq failed 60 . heightOf =<< sameAsFreshSolve failed fresh ctx inp (ui 60)

-- | The wheel over a window scrolls it, not the page, declared in a page scroller or beside.
runPageWindowScrollTest :: Context -> IORef Int -> IO ()
runPageWindowScrollTest _ failed = do
  let debugWindow = fst <$> openWindow "Debug" (column (numbered 30))
      nested = snd <$> scrollArea (tight . grow) (button "OK" >> debugWindow)
      sibling = scrollWith (tight . grow) (label "page") >> debugWindow
  forM_ [(withInput 320 220, nested), (withInput 640 400, sibling)] $ \(inp, ui) -> do
    ctx <- newContext
    Rect wx _ ww _ <- respRect <$> warmup2 ctx inp ui
    spanYOf "line 1" <$> collectOverlayTextSpans ctx inp >>= \case
      y : _ -> assertWheelTitlePinned failed ctx inp ui "Debug" "line 1" (V2 (wx + ww / 2) (y + 2))
      [] -> assert failed False

-- | A button in a page scroller opens a window that stays open.
debugToggleUi :: NanoUI ()
debugToggleUi = do
  openVar <- useVar False
  open <- readVar openVar
  scrollWith (tight . grow) (whenM (button "Debug") (writeVar openVar (not open)))
  when open (void (openWindow "Debug" (label "fps")))

-- | A span beside a window, even an empty field's, is kept whether the window is open or not.
runSpanBesideWindowTest :: Context -> IORef Int -> IO ()
runSpanBesideWindowTest ctx failed = do
  let inp = withInput 640 400
      ui open = do
        columnWith (padXY 20 320) (void (withValue "" textInput))
        void (window' (constVar open) "Elsewhere" (label "Body"))
      emptySpans = (\ss -> [(r, clip) | (r, t, _, _, clip) <- ss, T.null t]) <$> collectTextSpans ctx
  closed <- warmup2 ctx inp (ui False) >> emptySpans
  opened <- warmup2 ctx inp (ui True) >> emptySpans
  panels <- IM.elems <$> floatingPanelRects ctx
  assertEq failed 1 (length closed)
  assert failed (not (null panels) && and [isNothing (rectIntersect p clip) | p <- panels, (_, clip) <- closed])
  assertEq failed closed opened

-- | A title bar drags by the gaps between its widgets, in any order, overlapping or not.
runDragSpansTest :: Context -> IORef Int -> IO ()
runDragSpansTest _ failed = do
  let bar = Rect 0 0 400 30
      menus = Rect 0 0 100 30
      buttons = Rect 280 0 120 30
  forM_
    [ (bar, [menus, buttons], [Rect 100 0 180 30])
    , (bar, [buttons, menus], [Rect 100 0 180 30])
    , (bar, [], [bar])
    , (bar, [menus, Rect 150 4 50 22, buttons], [Rect 100 0 50 30, Rect 200 0 80 30])
    , (bar, [Rect 0 0 200 30, Rect 100 0 150 30], [Rect 250 0 150 30])
    , (bar, [Rect 0 0 400 30], [])
    , (bar, [Rect 150 40 50 20], [bar])
    , (Rect 100 0 300 30, [Rect 0 0 60 30], [Rect 100 0 300 30])
    ]
    $ \(b, taken, want) -> assertEq failed want (dragSpans b taken)

captionUi :: NanoUI (Maybe CaptionAction, Rect)
captionUi = rowWith (tight . fillW . fixedH captionBarHeight) (flex >> captionButtons False)

-- | The caption buttons' rect spans all three at the right end; the last one closes.
captionButtonsClose :: Sim (Maybe CaptionAction, Rect) ()
captionButtonsClose = do
  (action, Rect bx by bw bh) <- result
  let closeW = capButtonW defaultCaptionConfig
  checkEq Nothing action
  check (bw >= 3 * closeW && bx > 0 && bx + bw <= 400)
  checkEq (capButtonH defaultCaptionConfig) bh
  checkEq (Just CaptionClose) . fst =<< click (Within (Rect (bx + bw - closeW) by closeW bh))
