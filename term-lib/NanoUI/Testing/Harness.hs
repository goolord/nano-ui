-- | Shared helpers for integration tests: input gestures, spans, scroll checks.
module NanoUI.Testing.Harness
  ( clickPair
  , rightClickPair
  , pressAt
  , releaseAt
  , withInputOff
  , withDelta
  , centerOf
  , warmup
  , warmup2
  , warmupDraw
  , runClick
  , runRightClick
  , runClickMsgs
  , runClickPair
  , runClickRelease
  , withAnimCtx
  , assertSpansHas
  , spanYs
  , spanLabelYs
  , closeSpanBottom
  , closeSpanCenter
  , spansHas
  , spanYOf
  , spanXOf
  , assertScrollGutter
  , assertScrollGutterPad
  , assertWheelTitlePinned
  , terminalAboutModalMaxH
  , terminalAboutModalMaxFooter
  , findGrabHover
  , dragWindowEdge
  , vertUv
  , modifyIORef
  , checkLabelAlignEnd
  , checkIdleFullDamage
  , oneColFaOrigins
  , closeSpanPos
  , closeSpanStart
  , terminalGridW
  , terminalBracketSpans
  , terminalPairsOk
  , terminalBracketsOk
  , terminalBracketHasTrail
  , windowTitleGrab
  , runDragFrom
  ) where

import Control.Monad (void, when)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Text qualified as T
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (peekByteOff)
import NanoUI
import NanoUI.Term.Cells (Cells, cellChar, cellRows, cellsH)
import NanoUI.Testing
import NanoUI.Testing.Assert (bump, failWhen, withInput)

clickPair :: Input -> V2 -> (Input, Input)
clickPair inp pos =
  let
    press = pressAt inp pos
    release = releaseAt press
   in
    (press, release)

rightClickPair :: Input -> V2 -> (Input, Input)
rightClickPair inp pos =
  let
    press =
      inp
        { inputMousePos = pos
        , inputMouseRightDown = True
        , inputMouseRightPressed = True
        }
    release =
      press
        { inputMouseRightDown = False
        , inputMouseRightPressed = False
        , inputMouseRightReleased = True
        }
   in
    (press, release)

pressAt :: Input -> V2 -> Input
pressAt inp pos =
  inp
    { inputMousePos = pos
    , inputMouseDown = True
    , inputMousePressed = True
    , inputMouseReleased = False
    }

releaseAt :: Input -> Input
releaseAt press =
  press
    { inputMouseDown = False
    , inputMousePressed = False
    , inputMouseReleased = True
    }

withInputOff :: Float -> Float -> Input
withInputOff w h =
  let inp = withInput w h
   in inp {inputMousePos = V2 (-10) (-10)}

withDelta :: Float -> Float -> Float -> Input
withDelta w h dt =
  let inp = withInput w h
   in inp {inputDeltaTime = dt}

centerOf :: Response -> V2
centerOf resp =
  let
    Rect rx ry rw rh = respRect resp
   in
    V2 (rx + rw / 2) (ry + rh / 2)

warmup :: Context -> Input -> NanoUI a -> IO ()
warmup ctx inp ui = void (runFrame ctx inp ui)

warmup2 :: Context -> Input -> NanoUI a -> IO a
warmup2 ctx inp ui = do
  _ <- runFrame ctx inp ui
  (a, _, _, _) <- runFrame ctx inp ui
  pure a

warmupDraw :: Context -> Input -> NanoUI a -> IO (a, DrawData)
warmupDraw ctx inp ui = do
  _ <- runFrame ctx inp ui
  (a, _, draw, _) <- runFrame ctx inp ui
  pure (a, draw)

runClick :: Context -> Input -> NanoUI a -> V2 -> IO ()
runClick ctx inp0 ui pos = do
  let
    (press, release) = clickPair inp0 pos
  _ <- runFrame ctx press ui
  void (runFrame ctx release ui)

runRightClick :: Context -> Input -> NanoUI a -> V2 -> IO ()
runRightClick ctx inp0 ui pos = do
  let (press, release) = rightClickPair inp0 pos
  _ <- runFrame ctx press ui
  void (runFrame ctx release ui)

runClickMsgs :: Context -> Input -> NanoUI a -> V2 -> IO [FrameMsg]
runClickMsgs ctx inp0 ui pos = do
  let
    (press, release) = clickPair inp0 pos
  _ <- runFrame ctx press ui
  (_, msgs, _, _) <- runFrame ctx release ui
  pure msgs

runClickPair :: Context -> Input -> NanoUI a -> V2 -> IO a
runClickPair ctx inp0 ui pos = do
  let
    (press, release) = clickPair inp0 pos
  _ <- runFrame ctx press ui
  (a, _, _, _) <- runFrame ctx release ui
  pure a

runClickRelease :: Context -> Input -> NanoUI a -> V2 -> IO Input
runClickRelease ctx inp0 ui pos = do
  let
    (press, release) = clickPair inp0 pos
  _ <- runFrame ctx press ui
  _ <- runFrame ctx release ui
  pure release

withAnimCtx ::
  Float
  -> Float
  -> Float
  -> (Context -> Input -> IORef Int -> IO ())
  -> IORef Int
  -> IO ()
withAnimCtx w h dt body failed = do
  ctx <- newContext
  body ctx (withDelta w h dt) failed

assertSpansHas :: IORef Int -> T.Text -> [(Rect, T.Text, a, b, c)] -> IO ()
assertSpansHas failed needle spans = failWhen failed (not (spansHas needle spans))

spanYs :: T.Text -> [(Rect, T.Text, a, b, c)] -> [Float]
spanYs needle spans = [rectY r | (r, txt, _, _, _) <- spans, needle `T.isInfixOf` txt]

spanLabelYs :: T.Text -> [(Rect, T.Text, a, b, c)] -> [Float]
spanLabelYs needle spans = [rectY r | (r, txt, _, _, _) <- spans, txt == needle]

closeSpanBottom :: [(Rect, T.Text, a, b, c)] -> Maybe Float
closeSpanBottom spans =
  case
    [ rectY r + rectH r
    | (r, txt, _, _, _) <- spans
    , "Close" `T.isInfixOf` txt
    , T.strip txt /= "X"
    ] of
    [] -> Nothing
    bs -> Just (maximum bs)

closeSpanCenter :: [(Rect, T.Text, a, b, c)] -> Maybe V2
closeSpanCenter spans =
  case [r | (r, txt, _, _, _) <- spans, T.strip txt == "X"] of
    (Rect x y w h : _) -> Just (V2 (x + w / 2) (y + h / 2))
    [] -> Nothing

spansHas :: T.Text -> [(Rect, T.Text, a, b, c)] -> Bool
spansHas needle spans = any (\(_, txt, _, _, _) -> needle `T.isInfixOf` txt) spans

spanYOf :: T.Text -> [(Rect, T.Text, a, b, c)] -> [Float]
spanYOf lbl spans = [y | (Rect _ y _ _, txt, _, _, _) <- spans, txt == lbl]

spanXOf :: T.Text -> [(Rect, T.Text, a, b, c)] -> [Float]
spanXOf lbl spans = [x | (Rect x _ _ _, txt, _, _, _) <- spans, txt == lbl]

assertScrollGutter ::
  IORef Int
  -> Context
  -> WidgetId
  -> Response
  -> Float
  -> IO ()
assertScrollGutter failed ctx sid child gutter =
  assertScrollGutterPad failed ctx sid child gutter 0

assertScrollGutterPad ::
  IORef Int
  -> Context
  -> WidgetId
  -> Response
  -> Float
  -> Float
  -> IO ()
assertScrollGutterPad failed ctx sid child gutter endPad = do
  mrect <- getPrevRect ctx sid
  case mrect of
    Nothing -> bump failed
    Just (Rect sx _ sw _) -> do
      let
        Rect cx _ cw _ = respRect child
        contentRight = sx + sw - endPad - gutter
      failWhen failed (cx + cw < contentRight - 0.5)
      failWhen failed (cx + cw > contentRight + 0.01)

assertWheelTitlePinned ::
  IORef Int
  -> Context
  -> Input
  -> NanoUI a
  -> T.Text
  -> T.Text
  -> V2
  -> Maybe Float
  -> IO ()
assertWheelTitlePinned failed ctx inp0 ui title line1 wheelAt mClipMax = do
  spans0 <- collectOverlayTextSpans ctx inp0
  let
    titleYs0 = spanYs title spans0
    line1Ys0 = spanLabelYs line1 spans0
  failWhen failed (null titleYs0)
  case line1Ys0 of
    [] -> bump failed
    b0 : _ -> do
      let
        wheel = inp0 {inputMousePos = wheelAt, inputScroll = V2 0 1}
      _ <- runFrame ctx wheel ui
      spans1 <- collectOverlayTextSpans ctx wheel
      let
        titleYs1 = spanYs title spans1
        line1Ys1 = spanLabelYs line1 spans1
      case (titleYs0, titleYs1) of
        (y0 : _, y1 : _) -> failWhen failed (y1 /= y0)
        _ -> bump failed
      case line1Ys1 of
        [] -> pure ()
        b1 : _ -> failWhen failed (b1 >= b0)
      case mClipMax of
        Nothing -> pure ()
        Just maxY ->
          failWhen failed (any (\(Rect _ y _ h, _, _, _, _) -> y < 0 || y + h > maxY) spans1)

terminalAboutModalMaxH :: HostProfile -> FontMetrics -> Float
terminalAboutModalMaxH host fm =
  let
    pad = resolveLayoutPadding host fm (Padding 4 4 4 4)
    modalGap = resolveLayoutGap host fm 8
    bodyGap = resolveLayoutGap host fm (layoutGap defaultLayout)
    line = fmLineHeight fm
    titleH = if host == CellHost then 1 else 28
    sepH = 1
    bodyRows = (4 :: Int)
    bodyH =
      fromIntegral bodyRows * line
        + bodyGap * fromIntegral (pred bodyRows)
    chromeH = titleH + sepH + bodyH + modalGap * 2
   in
    padT pad + padB pad + chromeH + 0.5

terminalAboutModalMaxFooter :: HostProfile -> FontMetrics -> Float
terminalAboutModalMaxFooter host fm =
  let
    pad = resolveLayoutPadding host fm (Padding 4 4 4 4)
   in
    padB pad + fmLineHeight fm

findGrabHover ::
  Context -> NanoUI a -> Input -> Float -> [Float] -> IO (Maybe Input)
findGrabHover ctx ui inp0 thumbX = go
 where
  go [] = pure Nothing
  go (y : ys) = do
    let
      hover = inp0 {inputMousePos = V2 thumbX y}
    _ <- runFrame ctx hover ui
    kind <- uiCursorKind ctx hover
    if kind == UiCursorGrab then pure (Just hover) else go ys

dragWindowEdge ::
  Context
  -> Input
  -> NanoUI Response
  -> V2
  -> V2
  -> IO (Maybe Rect)
dragWindowEdge ctx inp0 ui grab dest = do
  let
    press = pressAt inp0 grab
  _ <- runFrame ctx press ui
  let
    dragged =
      press
        { inputMousePos = dest
        , inputMousePressed = False
        }
  _ <- runFrame ctx dragged ui
  let
    idle = inp0 {inputMousePos = dest}
  _ <- runFrame ctx idle ui
  (win, _, _, _) <- runFrame ctx idle ui
  getPrevRect ctx (respId win)

vertUv :: DrawData -> Int -> IO (Float, Float)
vertUv dd i =
  withForeignPtr (drawVertices dd) $ \p -> do
    let
      off = i * vertexSize
    u <- peekByteOff p (off + 24) :: IO Float
    v <- peekByteOff p (off + 28) :: IO Float
    pure (u, v)

modifyIORef :: IORef Int -> (Int -> Int) -> IO ()
modifyIORef r f = readIORef r >>= writeIORef r . f

checkIdleFullDamage ::
  IORef Int -> Context -> Input -> Input -> NanoUI a -> IO ()
checkIdleFullDamage failed ctx inpAfter inpIdle ui = do
  need <- needsRedraw ctx inpAfter inpIdle
  failWhen failed (not need)
  _ <- runFrame ctx inpIdle ui
  dmg <- takeDamage ctx
  failWhen failed (dmg /= DamageFull)

oneColFaOrigins :: [(Rect, T.Text, a, b, c)] -> [(Int, Int)]
oneColFaOrigins spans =
  [ (round (rectX r), round (rectY r))
  | (r, txt, _, _, _) <- spans
  , rectW r < 2
  , loneFontAwesome (T.strip txt)
  ]

closeSpanPos :: [(Rect, T.Text, a, b, c)] -> Maybe (Int, Int)
closeSpanPos spans =
  case
    [ (round (rectX r), round (rectY r))
    | (r, txt, _, _, _) <- spans
    , T.strip txt == iconClose glyphIcons
    ] of
    (p : _) -> Just p
    [] -> Nothing

closeSpanStart :: [(Rect, T.Text, a, b, c)] -> Maybe Int
closeSpanStart spans = fmap fst (closeSpanPos spans)

terminalGridW :: Cells -> Int
terminalGridW cells =
  case cellRows cells of
    (r : _) -> length r
    [] -> 0

terminalBracketSpans :: [(Rect, T.Text, a, b, c)] -> [Rect]
terminalBracketSpans spans =
  [r | (r, txt, _, _, _) <- spans, T.isPrefixOf "[ " txt]

terminalPairsOk :: Cells -> [(Rect, T.Text, a, b, c)] -> Bool
terminalPairsOk cells spans =
  let
    skip = oneColFaOrigins spans
    gw = terminalGridW cells
   in
    all
      ( \(x, y) ->
          let
            c = cellChar cells x y
           in
            not (fontAwesomeIcon c)
              || (x, y) `elem` skip
              || ( x + 1 < gw
                     && cellChar cells (x + 1) y == wideTrailChar
                 )
      )
      [ (x, y)
      | y <- [0 .. cellsH cells - 1]
      , x <- [0 .. gw - 1]
      ]

terminalBracketsOk :: Cells -> [(Rect, T.Text, a, b, c)] -> Bool
terminalBracketsOk cells spans =
  all
    ( \(Rect x y w h) ->
        let
          x0 = max 0 (round x)
          y0 = max 0 (round y)
          x1 = min (terminalGridW cells - 1) (round (x + w - 1))
          y1 = min (cellsH cells - 1) (round (y + h - 1))
         in
          all
            ( \cy ->
                all
                  (\cx -> cellChar cells cx cy /= wideTrailChar)
                  [x0 .. x1]
            )
            [y0 .. y1]
    )
    (terminalBracketSpans spans)

terminalBracketHasTrail :: Cells -> Rect -> Bool
terminalBracketHasTrail cells (Rect x y w h) =
  let
    x0 = max 0 (round x)
    y0 = max 0 (round y)
    x1 = min (terminalGridW cells - 1) (round (x + w - 1))
    y1 = min (cellsH cells - 1) (round (y + h - 1))
   in
    any
      ( \cy ->
          any (\cx -> cellChar cells cx cy == wideTrailChar) [x0 .. x1]
      )
      [y0 .. y1]

checkLabelAlignEnd :: IORef Int -> Context -> IO ()
checkLabelAlignEnd failed ctx = do
  let
    fm = ctxFontMetrics ctx
    (ix, _) = labelContentInset (ctxHostProfile ctx) fm
    tw = fmAdvance fm ' ' * 2
    boxW = tw + 2 * ix + 4
    inp = emptyInput {inputWindowSize = Size (boxW + 8) 8}
    ui =
      row (fixedW boxW . tight . gap 0 $ defaultLayout) $
        labelEx (fillW . alignEnd . tight $ defaultLayout) "ab"
  _ <- runFrame ctx inp ui
  (lab, _, _, _) <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  let
    Rect bx _ bw _ = respRect lab
    hits = [r | (r, txt, _, _, _) <- spans, T.isInfixOf (T.pack "ab") txt]
  case hits of
    [] -> bump failed
    Rect x _ w _ : _ -> do
      when (abs ((x + w) - (bx + bw - ix)) > 0.6) $ bump failed
      when (abs (w - tw) > 0.6) $ bump failed

windowTitleGrab :: Rect -> V2
windowTitleGrab (Rect x0 y0 _ _) = V2 (x0 + 24) (y0 + 22)

runDragFrom :: Context -> Input -> NanoUI a -> V2 -> V2 -> IO ()
runDragFrom ctx inp0 ui grab dest = do
  let
    press = pressAt inp0 grab
  _ <- runFrame ctx press ui
  let
    moved = press {inputMousePos = dest, inputMousePressed = False}
  void (runFrame ctx moved ui)
