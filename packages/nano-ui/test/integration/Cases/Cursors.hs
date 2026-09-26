module Cases.Cursors (tests) where

import Spec
import Data.List (find)

tests :: [Spec]
tests =
  [ spec "cursor-shape-scope" runCursorShapeScopeTest
  , spec "cursor-shape-inner-widgets" runCursorShapeInnerWidgetsTest
  , spec "cursor-shape-nested" runCursorShapeNestedTest
  , spec "cursor-shape-disabled" runCursorShapeDisabledTest
  , spec "cursor-shape-ids-layout" runCursorShapeIdsLayoutTest
  , spec "cursor-shape-floating" runCursorShapeFloatingTest
  , spec "cursor-shape-clipped" runCursorShapeClippedTest
  , spec "cursor-shape-repeated-pass" runCursorShapeRepeatedPassTest
  , spec "cursor-shape-names" runCursorShapeNamesTest
  , spec "cursor-shape-modal" runCursorShapeModalTest
  , spec "cursor-shape-in-window" runCursorShapeInWindowTest
  , spec "cursor-shape-layers-and-pin" runCursorShapeLayersAndPinTest
  ]

-- | A plain drawing: a node with no cursor of its own.
plainArea :: Float -> Float -> NanoUI Response
plainArea w h = drawing (fixedWH w h) (const mempty)

inp :: Input
inp = withInputOff 640 480

-- | Check the cursor after a frame with the pointer at each point.
shapesAt :: IORef Int -> Context -> NanoUI a -> [(V2, UiCursorKind)] -> IO ()
shapesAt failed ctx ui = mapM_ $ \(p, want) -> cursorOver ctx inp ui p >>= assertEq failed want

-- | Warm the view up, then check the cursor at point @at@ of each response it returns.
shapesOver :: IORef Int -> Context -> (Response -> V2) -> NanoUI [Response] -> [UiCursorKind] -> IO [Response]
shapesOver failed ctx at ui wants = do
  rs <- warmup2 ctx inp ui
  assertEq failed (length wants) (length rs)
  rs <$ shapesAt failed ctx ui (zip (map at rs) wants)

-- | The scope's shape shows over its widgets only, also over the solved arena without a frame.
runCursorShapeScopeTest :: Context -> IORef Int -> IO ()
runCursorShapeScopeTest ctx failed = do
  let ui = column (sequence [withCursorShape UiCursorCrosshair (plainArea 120 80), plainArea 120 80])
  rs <- shapesOver failed ctx centerOf ui [UiCursorCrosshair, UiCursorDefault]
  forM_ (zip rs [UiCursorCrosshair, UiCursorDefault]) $ \(r, want) ->
    uiCursorKind ctx inp {inputMousePos = centerOf r} >>= assertEq failed want
  -- A frame without the scope drops its shape.
  shapesAt failed ctx (column (plainArea 120 80 >> plainArea 120 80)) [(centerOf r, UiCursorDefault) | r <- take 1 rs]

-- | Widgets inside keep their own cursors; the scope fills in the rest, container gaps included.
runCursorShapeInnerWidgetsTest :: Context -> IORef Int -> IO ()
runCursorShapeInnerWidgetsTest ctx failed = do
  let ui = withCursorShape UiCursorMove . columnWith (gap 24) $ sequence [button' "Press", fst <$> textInput' "text", plainArea 120 60]
  rs <- shapesOver failed ctx centerOf ui [UiCursorPointer, UiCursorText, UiCursorMove]
  shapesAt failed ctx ui [(V2 (bx + 4) (by + bh + 12), UiCursorMove) | Rect bx by _ bh <- map respRect (take 1 rs)]

-- | The innermost scope around a widget wins, the default arrow included.
runCursorShapeNestedTest :: Context -> IORef Int -> IO ()
runCursorShapeNestedTest ctx failed =
  void . shapesOver failed ctx centerOf ui $ [UiCursorMove, UiCursorCrosshair, UiCursorMove, UiCursorCell, UiCursorDefault]
  where
    ui = withCursorShape UiCursorMove . column $ sequence [a, s UiCursorCrosshair a, a, s UiCursorAllScroll (s UiCursorCell a), s UiCursorDefault a]
    a = plainArea 100 50
    s = withCursorShape

-- | A disabled widget has no cursor of its own, so the scope's shows over it.
runCursorShapeDisabledTest :: Context -> IORef Int -> IO ()
runCursorShapeDisabledTest ctx failed =
  forM_ [(True, [UiCursorNotAllowed, UiCursorNotAllowed]), (False, [UiCursorPointer, UiCursorText])] $ \(off, wants) ->
    shapesOver failed ctx centerOf (ui off) wants
  where
    ui off = withCursorShape UiCursorNotAllowed . disabledWhen off . column $ sequence [button' "Delete", fst <$> textInput' "name"]

-- | The scope takes no id and adds no node: widgets keep their ids, state and layout.
runCursorShapeIdsLayoutTest :: Context -> IORef Int -> IO ()
runCursorShapeIdsLayoutTest ctx failed = do
  let ui scoped = row $ do
        a <- (if scoped then withCursorShape UiCursorHelp else id) (button' "a")
        (n, setN) <- useInt 0
        b <- button' "b"
        when (respClicked b) (setN (n + 1))
        pure (a, b, n)
      ids (a, b) = (respId a, respId b, respRect a, respRect b)
  (a0, b0, _) <- warmup2 ctx inp (ui False)
  rects0 <- arenaRects ctx
  (_, _, n1) <- runClick ctx inp (ui False) (centerOf b0)
  (a1, b1, n2) <- warmup2 ctx inp (ui True)
  rects1 <- arenaRects ctx
  assertEq failed (ids (a1, b1), rects1, n1, n2) (ids (a0, b0), rects0, 1, 1)

-- | A floating panel over the scope's widgets hides the shape, unless declared inside the scope.
runCursorShapeFloatingTest :: Context -> IORef Int -> IO ()
runCursorShapeFloatingTest ctx failed = do
  let tools = fst <$> window True "Tools" (label "Body")
      over = (,) <$> tools <*> withCursorShape UiCursorMove (plainArea 620 380)
      within = withCursorShape UiCursorMove (tools >> plainArea 620 380)
  (w, d) <- warmup2 ctx inp over
  assertJustM failed (getPrevRect ctx (respId w)) $ \wr -> do
    let Rect dx dy dw dh = respRect d
        body = spanCenter wr
        corners = [V2 x y | x <- [dx + 12, dx + dw - 12], y <- [dy + 12, dy + dh - 12]]
    assert failed (rectContains (respRect d) body)
    shapesAt failed ctx over [(body, UiCursorDefault)]
    assertJust failed (find (not . rectContains (rectInflate 8 wr)) corners) $ \bare ->
      shapesAt failed ctx over [(bare, UiCursorMove)]
    void (warmup2 ctx inp within)
    shapesAt failed ctx within [(body, UiCursorMove)]

-- | A scope's widget scrolled out of its viewport shows no shape where it would lie.
runCursorShapeClippedTest :: Context -> IORef Int -> IO ()
runCursorShapeClippedTest ctx failed = do
  let ui = column . scrollWith (fixedH 100 . fixedW 200) . column $
        plainArea 150 40 >> withCursorShape UiCursorMove (plainArea 150 80)
  Rect dx dy dw dh <- respRect <$> warmup2 ctx inp ui
  shapesAt failed ctx ui [(V2 (dx + dw / 2) (dy + 4), UiCursorMove), (V2 (dx + dw / 2) (dy + dh - 4), UiCursorDefault)]

-- | Only the scopes of a frame's last view pass count.
runCursorShapeRepeatedPassTest :: Context -> IORef Int -> IO ()
runCursorShapeRepeatedPassTest ctx failed = do
  let ui = do
        (n, setN) <- useInt 0
        when (n == 0) (setN 1)
        d <- column ((if n == 0 then withCursorShape UiCursorMove else id) (plainArea 120 80))
        pure (d, n)
  (d, n) <- evalUi ctx inp ui
  assertEq failed 1 n
  assertJustM failed (getPrevRect ctx (respId d)) $ \r ->
    uiCursorKind ctx inp {inputMousePos = spanCenter r} >>= assertEq failed UiCursorDefault

-- | The view API's names are the cursor kinds the backends map.
runCursorShapeNamesTest :: Context -> IORef Int -> IO ()
runCursorShapeNamesTest _ failed =
  assertEq failed (33, True) (length shapes, all (`elem` shapes) [UiCursorNotAllowed, UiCursorCrosshair, UiCursorMove, UiCursorWait, UiCursorNwResize, UiCursorHidden])
  where
    shapes = [minBound .. maxBound] :: [UiCursorKind]

-- | While a modal is open only scopes in it show, and a popup in it covers a scope there.
runCursorShapeModalTest :: Context -> IORef Int -> IO ()
runCursorShapeModalTest ctx failed = do
  let ui open anchor = do
        page <- withCursorShape UiCursorMove (plainArea 620 460)
        (_, inner) <- modal open "Dialog" $ do
          area <- withCursorShape UiCursorCrosshair (plainArea 200 120)
          (_, body) <- popup True (defaultPopupConfig (AnchorPoint anchor)) {cfgPlacement = PlacementBelow, cfgDismissable = False} (plainArea 60 30)
          pure (area, body)
        pure (page, inner)
  (page, _) <- warmup2 ctx inp (ui False (V2 0 0))
  let corner = V2 (rectX (respRect page) + 8) (rectY (respRect page) + 8)
  shapesAt failed ctx (ui False (V2 0 0)) [(corner, UiCursorMove)]
  -- Where the scope is, to anchor the popup in it.
  assertJustM failed (fmap (respRect . fst) . snd <$> warmup2 ctx inp (ui True (V2 0 0))) $ \(Rect ax ay aw ah) -> do
    let view = ui True (V2 (ax + aw / 2) (ay + 10))
    assertJustM failed ((>>= sequence) . snd <$> warmup2 ctx inp view) $ \(area, body) -> do
      assert failed (not (rectContains (rectInflate 8 (respRect area)) corner) && rectContains (respRect area) (centerOf body))
      shapesAt failed ctx view [(corner, UiCursorDefault), (centerOf body, UiCursorDefault), (V2 (ax + aw - 8) (ay + ah - 8), UiCursorCrosshair)]

-- | A scope in a window's body shows over its widgets, not over the title bar.
runCursorShapeInWindowTest :: Context -> IORef Int -> IO ()
runCursorShapeInWindowTest ctx failed = do
  let ui = window True "Tools" (withCursorShape UiCursorCrosshair (plainArea 160 80))
  (w, mInner) <- warmup2 ctx inp ui
  assertJust failed mInner $ \inner -> assertJustM failed (getPrevRect ctx (respId w)) $ \(Rect wx wy ww _) ->
    shapesAt failed ctx ui [(centerOf inner, UiCursorCrosshair), (V2 (wx + ww / 2) (wy + 12), UiCursorDefault)]

-- | The shape follows the node drawn on top: a pinned one, or a later layer.
runCursorShapeLayersAndPinTest :: Context -> IORef Int -> IO ()
runCursorShapeLayersAndPinTest ctx failed = do
  let pinned = drawing (pinAt 20 20 . fixedWH 60 40) (const mempty)
      farCorner r = let Rect x y w h = respRect r in V2 (x + w - 10) (y + h - 10)
      check ui = void . shapesOver failed ctx farCorner ui
      col = columnWith (tight . gap 0) . sequence
  check (col [withCursorShape UiCursorCrosshair pinned, plainArea 200 120]) [UiCursorCrosshair, UiCursorDefault]
  check (col [pinned, withCursorShape UiCursorCrosshair (plainArea 200 120)]) [UiCursorDefault, UiCursorCrosshair]
  check (layersWith tight (sequence [withCursorShape UiCursorMove pinned, withCursorShape UiCursorHelp (plainArea 200 120), plainArea 100 60]))
    [UiCursorMove, UiCursorHelp, UiCursorDefault]
