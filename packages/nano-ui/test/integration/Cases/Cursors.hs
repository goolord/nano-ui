module Cases.Cursors (tests) where

import Spec
import Data.Maybe (fromMaybe, isNothing)
import Data.Text qualified as T
import NanoUI.Testing.Sim

tests :: [Spec]
tests =
  [ spec "cursor-shapes" runCursorShapesTest
  , spec "cursor-shape-ids-layout" runCursorShapeIdsLayoutTest
  , spec "cursor-shape-repeated-pass" runCursorShapeRepeatedPassTest
  , simSpec "cursor-drag-owner" 640 480 dragOwnerUi cursorDragOwner
  ]

-- | A plain drawing: a node with no cursor of its own.
plainArea :: Float -> Float -> NanoUI Response
plainArea w h = drawing (fixedWH w h) (const mempty)

-- | The cursor for the pointer at @p@, from the last frame's solve.
cursorNow :: V2 -> Sim a UiCursorKind
cursorNow p = do
  ctx <- simContext
  i <- getInput
  liftIO (uiCursorKind ctx i {inputMousePos = p})

-- | Views that return points, and the cursor each point shows.
shapeCases :: [(NanoUI [V2], [UiCursorKind])]
shapeCases =
  [ -- The innermost scope wins, even when it sets the default arrow. Widgets
    -- keep their own cursors, and a disabled one, having none, shows the
    -- scope's. The scope covers container gaps, and nothing outside it.
    ( row $ do
        rs <- withCursorShape UiCursorMove . columnWith (gap 12) $ sequence
          [ a 30
          , s UiCursorCrosshair (a 30)
          , s UiCursorAllScroll (s UiCursorCell (a 30))
          , s UiCursorDefault (a 30)
          , button' "Press"
          , fst <$> withValue "text" textInput'
          , s UiCursorNotAllowed (disabledWhen True (button' "Delete"))
          , s UiCursorNotAllowed (disabledWhen True (fst <$> withValue "name" textInput'))
          ]
        outside <- a 30
        let Rect bx by _ bh = respRect (rs !! 4)
        pure (map centerOf (rs <> [outside]) <> [V2 (bx + 4) (by + bh + 6)])
    , [ UiCursorMove, UiCursorCrosshair, UiCursorCell, UiCursorDefault, UiCursorPointer, UiCursorText
      , UiCursorNotAllowed, UiCursorNotAllowed, UiCursorDefault, UiCursorMove
      ]
    )
  , -- The shape follows the topmost node: a pinned one, or a later layer.
    (corners <$> col [s UiCursorCrosshair pinned, a 120], [UiCursorCrosshair, UiCursorDefault])
  , (corners <$> col [pinned, s UiCursorCrosshair (a 120)], [UiCursorDefault, UiCursorCrosshair])
  , ( corners <$> layersWith tight (sequence [s UiCursorMove pinned, s UiCursorHelp (a 120), a 60])
    , [UiCursorMove, UiCursorHelp, UiCursorDefault]
    )
  , -- A scoped widget scrolled out of the viewport shows no shape where it
    -- would be.
    ( ends . respRect <$> column (scrollWith (fixedH 100 . fixedW 200) clipped)
    , [UiCursorMove, UiCursorDefault]
    )
  , -- A floating panel over a scope hides its shape. A scope in a window body
    -- applies to its widgets, not the title bar.
    ( column $ do
        (w, inner) <- window' (constVar True) "Tools" ((,) <$> label' "Body" <*> s UiCursorCrosshair (a 80))
        d@(Rect dx dy dw dh) <- respRect <$> s UiCursorMove (plainArea 620 380)
        wr@(Rect wx wy ww _) <- rectNow w
        let inset = [V2 x y | x <- [dx + 12, dx + dw - 12], y <- [dy + 12, dy + dh - 12]]
        pure $ case (inner, filter (not . rectContains (rectInflate 8 wr)) inset) of
          (Just (body, scoped), bare : _) | rectContains d (centerOf body) ->
            [bare, centerOf body, centerOf scoped, V2 (wx + ww / 2) (wy + 12)]
          _ -> []
    , [UiCursorMove, UiCursorDefault, UiCursorCrosshair, UiCursorDefault]
    )
  , -- A scope much smaller than the page, whose nodes alone are tested for
    -- the pointer, shows on itself and in a window declared in it, and not on
    -- the list beside it, its labels or the gaps between its rows.
    ( row $ do
        labels <- columnWith (gap 6) . forM [1 .. 30 :: Int] $ \i ->
          row (label' (T.pack ("row " <> show i)) <* plainArea 60 8)
        (area, w) <- s UiCursorCrosshair $
          (,) <$> plainArea 80 60 <*> (fst <$> window' (constVar True) "Tools" (label "Body"))
        wr <- rectNow w
        pure $ case [respRect r | r <- labels, isNothing (rectIntersect (rectInflate 8 wr) (respRect r))] of
          l1 : Rect x y _ h : _ -> [spanCenter l1, V2 (x + 2) (y + h + 3), centerOf area, spanCenter wr]
          _ -> []
    , [UiCursorDefault, UiCursorDefault, UiCursorCrosshair, UiCursorCrosshair]
    )
  , -- With a modal open only scopes inside it apply, and a popup in the modal
    -- covers a scope beneath it.
    (modalCase False, [UiCursorMove])
  , (modalCase True, [UiCursorDefault, UiCursorDefault, UiCursorCrosshair])
  ]
  where
    modalCase open = do
      Rect px py _ _ <- respRect <$> s UiCursorMove (plainArea 620 460)
      (_, inner) <- modal' (constVar open) "Dialog" $ do
        area <- s UiCursorCrosshair (a 120)
        let Rect ax ay aw _ = respRect area
            anchor = AnchorPoint (V2 (ax + aw / 2) (ay + 10))
            cfg = (defaultPopupConfig anchor) {cfgPlacement = PlacementBelow, cfgDismissable = False}
        (area,) . snd <$> popup' (constVar True) cfg (plainArea 60 30)
      let corner = V2 (px + 8) (py + 8)
          apart area = not (rectContains (rectInflate 8 (respRect area)) corner)
      pure . (corner :) $ case inner of
        Just (area, Just body) | apart area && rectContains (respRect area) (centerOf body) ->
          centerOf body : corners [area]
        _ -> []
    rectNow r = askContext >>= \ctx -> liftIO (fromMaybe (Rect 0 0 0 0) <$> getPrevRect ctx (respId r))
    s = withCursorShape
    a = plainArea 200
    pinned = drawing (pinAt 20 20 . fixedWH 60 40) (const mempty)
    col = columnWith (tight . gap 0) . sequence
    corners = map ((\(Rect x y w h) -> V2 (x + w - 10) (y + h - 10)) . respRect)
    clipped = column (plainArea 150 40 >> s UiCursorMove (plainArea 150 80))
    ends (Rect x y w h) = [V2 (x + w / 2) (y + 4), V2 (x + w / 2) (y + h - 4)]

runCursorShapesTest :: Context -> IORef Int -> IO ()
runCursorShapesTest _ failed = forM_ shapeCases $ \(ui, wants) -> do
  ctx <- newContext
  runSim ctx failed (withInputOff 640 600) ui (checkEq wants =<< mapM (cursorAt . At) =<< result)

-- | A scope takes no id and adds no node, so widget ids, state and layout are
-- unchanged. Its shape shows over its node, even read from the solved arena
-- without running a frame, and a frame without the scope drops it.
runCursorShapeIdsLayoutTest :: Context -> IORef Int -> IO ()
runCursorShapeIdsLayoutTest ctx failed = do
  scoped <- newIORef False
  let ui = row $ do
        on <- liftIO (readIORef scoped)
        a <- (if on then withCursorShape UiCursorHelp else id) (plainArea 60 30)
        nVar <- useVar (0 :: Int)
        n <- readVar nVar
        b <- button' "b"
        when (respClicked b) (writeVar nVar (n + 1))
        pure (a, b, n)
      snapshot = do
        (a, b, n) <- result
        rects <- liftIO (arenaRects ctx)
        pure (respId a, respId b, respRect a, respRect b, rects, n)
      setScoped on = liftIO (writeIORef scoped on) >> settle
  runSim ctx failed (withInputOff 640 480) ui $ do
    _ <- click "b"
    before <- snapshot
    (a, _, _) <- setScoped True
    checkEq before =<< snapshot
    checkEq UiCursorHelp =<< cursorNow (centerOf a)
    checkEq UiCursorHelp =<< cursorAt (On a)
    _ <- setScoped False
    checkEq UiCursorDefault =<< cursorAt (On a)

-- | Only scopes from the frame's last view pass count.
runCursorShapeRepeatedPassTest :: Context -> IORef Int -> IO ()
runCursorShapeRepeatedPassTest ctx failed = do
  let inp = withInputOff 640 480
      ui = do
        nVar <- useVar (0 :: Int)
        n <- readVar nVar
        when (n == 0) (writeVar nVar 1)
        d <- column ((if n == 0 then withCursorShape UiCursorMove else id) (plainArea 120 80))
        pure (d, n)
  (d, n) <- evalUi ctx inp ui
  assertEq failed 1 n
  assertJustM failed (getPrevRect ctx (respId d)) $ \r ->
    uiCursorKind ctx inp {inputMousePos = spanCenter r} >>= assertEq failed UiCursorDefault

dragOwnerUi :: NanoUI [Response]
dragOwnerUi = row $ do
  s <- withValue 5 (slider' 0 10)
  f <- withValue "text" textInput'
  g <- withValue "more" textInput'
  b <- button' "Go"
  pure [fst s, fst f, fst g, b]

-- | A drag keeps its owner's cursor over other fields (a slider's grabbing
-- hand, a field's I-beam) and lights up no button it crosses.
cursorDragOwner :: Sim [Response] ()
cursorDragOwner = do
  [s, f, g, b] <- result
  let dragOver from to = (press (On from) >> cursorAt (On to)) <* release
  checkEq UiCursorGrabbing =<< dragOver s f
  checkEq UiCursorText =<< dragOver f g
  _ <- press (On s) >> moveTo (On b)
  check . (/= respId b) =<< liftIO . getHotId =<< simContext
