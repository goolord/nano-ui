-- | Shared helpers for integration tests: input gestures, spans, scroll checks.
module NanoUI.Testing.Harness
  ( clickPair
  , rightClickPair
  , pressAt
  , releaseAt
  , keyInp
  , tabInp
  , withInputOff
  , withDelta
  , centerOf
  , warmup
  , warmup2
  , warmupDraw
  , runClick
  , runRightClick
  , runClickPair
  , runClickRelease
  , withAnimCtx
  , assertSpansHas
  , spanYOf
  , spanXOf
  , assertScrollGutter
  , assertScrollGutterPad
  , assertWheelTitlePinned
  , findGrabHover
  , dragWindowEdge
  , vertUv
  , checkLabelAlignEndInk
  , checkIdleFullDamage
  , windowTitleGrab
  , runDragFrom
  , DemoSpan
  , spanCenter
  , hasText
  , spanLabel
  , findExact
  , findHeader
  , findRightmost
  , requireSpan
  , clickPos
  , clickTab
  , dragPos
  , drawQuads
  ) where

import Control.Monad (forM, void, when)
import Data.IORef (IORef)
import Data.Text qualified as T
import Data.Word (Word32, Word8)
import Foreign.C.Types (CSize (..))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peekByteOff)
import GHC.Stack (HasCallStack)
import NanoUI
import NanoUI.Font (alignedTextPen, textInkEnd)
import NanoUI.Testing
import NanoUI.Testing.Assert (assert, assertEq, assertLt, bump, withInput)

type DemoSpan = (Rect, T.Text, Color, Color, Rect)

foreign import ccall unsafe "string.h memcpy" c_memcpy :: Ptr Word8 -> Ptr Word8 -> CSize -> IO ()

-- | Decode the quads a frame actually rasterised: one @(rect, color)@ per
-- six-index quad, in draw order. Span and arena queries cannot see chrome
-- (scroller wells, scrollbar lanes); this can. Every rasterised op in the
-- draw arena is emitted as 4 vertices / 6 indices — a command that breaks
-- that packing fails loudly here instead of decoding garbage.
drawQuads :: DrawData -> IO [(Rect, Color)]
drawQuads dd =
  fmap concat $
    forM (drawCmdElems dd) $ \c -> do
      let ioff = fromIntegral (cmdIndexOffset c)
          icnt = fromIntegral (cmdIndexCount c)
      when (icnt `rem` 6 /= 0) $
        error ("drawQuads: draw command packs " ++ show icnt ++ " indices; not quad-packed")
      sequence
        [ decodeQuad (ioff + q)
        | q <- [0, 6 .. icnt - 1]
        ]
  where
    verts = drawVertices dd
    idxs = drawIndices dd
    peekWord32 :: Ptr Word8 -> IO Word32
    peekWord32 off = allocaBytes 4 $ \tmp -> do
      c_memcpy tmp off 4
      peekByteOff tmp 0
    peekVertex :: Ptr Word8 -> Int -> IO (Float, Float, Float, Float, Float, Float)
    peekVertex vp vi =
      allocaBytes vertexSize $ \tmp -> do
        c_memcpy tmp (vp `plusPtr` (vi * vertexSize)) (fromIntegral vertexSize)
        x <- peekByteOff tmp 0
        y <- peekByteOff tmp 4
        r <- peekByteOff tmp 8
        g <- peekByteOff tmp 12
        b <- peekByteOff tmp 16
        a <- peekByteOff tmp 20
        pure (x, y, r, g, b, a)
    decodeQuad iStart =
      withForeignPtr verts $ \vp ->
        withForeignPtr idxs $ \ip -> do
          vis <-
            forM [iStart .. iStart + 3] $ \ii -> do
              vi <- fromIntegral <$> peekWord32 (ip `plusPtr` (ii * indexSize))
              peekVertex vp vi
          case vis of
            [] -> pure (Rect 0 0 0 0, colorRGBA 0 0 0 0)
            (x0, y0, r0, g0, b0, a0) : rest -> do
              let xs = x0 : map (\(x, _, _, _, _, _) -> x) rest
                  ys = y0 : map (\(_, y, _, _, _, _) -> y) rest
                  toW8 f = max 0 (min 255 (round (f * 255)))
              pure
                ( Rect (minimum xs) (minimum ys) (maximum xs - minimum xs) (maximum ys - minimum ys)
                , colorRGBA (toW8 r0) (toW8 g0) (toW8 b0) (toW8 a0)
                )

spanCenter :: Rect -> V2
spanCenter (Rect x y w h) = V2 (x + w / 2) (y + h / 2)

hasText :: T.Text -> [(Rect, T.Text, a, b, c)] -> Bool
hasText needle = any (\(_, txt, _, _, _) -> needle `T.isInfixOf` txt)

-- Blank-glyph markers some span labels carry in front of their text (blanked
-- sort arrows, flags, sort-reserve padding).
dropSpanMarkers :: T.Text -> T.Text
dropSpanMarkers = T.dropWhile (`elem` ['\x01', '\x02', '\x05'])

spanLabel :: T.Text -> T.Text
spanLabel txt = dropSpanMarkers (T.strip txt)

findExact :: T.Text -> [DemoSpan] -> Maybe V2
findExact needle spans =
  pickRight
    [ (x, spanCenter r)
    | (r@(Rect x _ w h), txt, _, _, _) <- spans
    , w > 1 && h > 1
    , spanLabel txt == needle
    ]

findHeader :: T.Text -> [DemoSpan] -> Maybe V2
findHeader needle spans =
  -- Header spans keep their sort-reserve padding ("Name   " with the arrow
  -- glyph blanked when unsorted), while every other "Name" label is trimmed.
  -- Match the raw, untrimmed text so the header wins over right-aligned kv
  -- values that happen to repeat the column name.
  let marked =
        [ (x, spanCenter r)
        | (r@(Rect x _ w h), txt, _, _, _) <- spans
        , w > 1 && h > 1
        , T.isPrefixOf (needle <> " ") (dropSpanMarkers txt)
        ]
      exact =
        [ (x, spanCenter r)
        | (r@(Rect x _ w h), txt, _, _, _) <- spans
        , w > 1 && h > 1
        , spanLabel txt == needle
        ]
   in pickRight (if null marked then exact else marked)

findRightmost :: T.Text -> [DemoSpan] -> Maybe V2
findRightmost needle spans =
  pickRight [(x, spanCenter r) | (r@(Rect x _ _ _), txt, _, _, _) <- spans, needle `T.isInfixOf` txt]

pickRight :: [(Float, V2)] -> Maybe V2
pickRight [] = Nothing
pickRight (p : ps) = Just (go p ps)
 where
  go acc [] = snd acc
  go acc@(ax, _) (q@(qx, _) : qs) = go (if qx >= ax then q else acc) qs

requireSpan :: String -> Maybe V2 -> IO V2
requireSpan msg = maybe (fail msg) pure

clickAt :: Input -> V2 -> (Input, Input, Input)
clickAt base pos =
  let press = base {inputMousePos = pos, inputMouseDown = True, inputMousePressed = True}
      hold = press {inputMousePressed = False}
      release = hold {inputMouseDown = False, inputMouseReleased = True}
   in (press, hold, release)

clickPos :: (Input -> IO ()) -> Input -> V2 -> IO ()
clickPos drawFrame base pos = do
  let (press, hold, release) = clickAt base pos
  mapM_ drawFrame [press, hold, release, base, base]

clickTab :: (Context -> IO [DemoSpan]) -> (Input -> IO ()) -> Context -> Input -> T.Text -> IO ()
clickTab getSpans drawFrame ctx base name = do
  spans <- getSpans ctx
  pos <- requireSpan ("selftest: tab " <> T.unpack name) (findExact name spans)
  clickPos drawFrame base pos

dragPos :: (Input -> IO ()) -> Input -> V2 -> V2 -> IO ()
dragPos drawFrame base from to = do
  let press = base {inputMousePos = from, inputMouseDown = True, inputMousePressed = True}
      hold = press {inputMousePressed = False, inputMousePos = to}
      release = hold {inputMouseDown = False, inputMouseReleased = True, inputMousePos = to}
  mapM_ drawFrame [press, hold, release, base, base]

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

-- | A single key-down frame.
keyInp :: Key -> Input -> Input
keyInp k inp = inp {inputKeys = inputKeysFromList [k]}

-- | Step the tab focus to the next focusable.
tabInp :: Input -> Input
tabInp = keyInp KeyTab

withInputOff :: Float -> Float -> Input
withInputOff w h =
  let inp = withInput w h
   in inp {inputMousePos = V2 (-10) (-10)}

withDelta :: Float -> Float -> Float -> Input
withDelta w h dt =
  let inp = withInput w h
   in inp {inputDeltaTime = dt}

centerOf :: Response -> V2
centerOf = spanCenter . respRect

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

assertSpansHas :: HasCallStack => IORef Int -> T.Text -> [(Rect, T.Text, a, b, c)] -> IO ()
assertSpansHas failed needle spans = assert failed (hasText needle spans)

spanYs :: T.Text -> [(Rect, T.Text, a, b, c)] -> [Float]
spanYs needle spans = [rectY r | (r, txt, _, _, _) <- spans, needle `T.isInfixOf` txt]

spanYOf :: T.Text -> [(Rect, T.Text, a, b, c)] -> [Float]
spanYOf lbl spans = [y | (Rect _ y _ _, txt, _, _, _) <- spans, txt == lbl]

spanXOf :: T.Text -> [(Rect, T.Text, a, b, c)] -> [Float]
spanXOf lbl spans = [x | (Rect x _ _ _, txt, _, _, _) <- spans, txt == lbl]

assertScrollGutter ::
  HasCallStack
  => IORef Int
  -> Context
  -> WidgetId
  -> Response
  -> Float
  -> IO ()
assertScrollGutter failed ctx sid child gutter =
  assertScrollGutterPad failed ctx sid child gutter 0

assertScrollGutterPad ::
  HasCallStack
  => IORef Int
  -> Context
  -> WidgetId
  -> Response
  -> Float
  -> Float
  -> IO ()
assertScrollGutterPad failed ctx sid child gutter endPad = do
  mrect <- getPrevRect ctx sid
  case mrect of
    Nothing -> assert failed False
    Just (Rect sx _ sw _) -> do
      let
        Rect cx _ cw _ = respRect child
        contentRight = sx + sw - endPad - gutter
      assert failed (cx + cw >= contentRight - 0.5)
      assert failed (cx + cw <= contentRight + 0.01)

assertWheelTitlePinned ::
  HasCallStack
  => IORef Int
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
    line1Ys0 = spanYOf line1 spans0
  assert failed (not (null titleYs0))
  case line1Ys0 of
    [] -> assert failed False
    b0 : _ -> do
      let
        wheel = inp0 {inputMousePos = wheelAt, inputScroll = V2 0 1}
      _ <- runFrame ctx wheel ui
      spans1 <- collectOverlayTextSpans ctx wheel
      let
        titleYs1 = spanYs title spans1
        line1Ys1 = spanYOf line1 spans1
      case (titleYs0, titleYs1) of
        (y0 : _, y1 : _) -> assertEq failed y1 y0
        _ -> assert failed False
      case line1Ys1 of
        [] -> pure ()
        b1 : _ -> assertLt failed b1 b0
      case mClipMax of
        Nothing -> pure ()
        Just maxY ->
          assert failed (not (any (\(Rect _ y _ h, _, _, _, _) -> y < 0 || y + h > maxY) spans1))

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

checkIdleFullDamage ::
  HasCallStack => IORef Int -> Context -> Input -> Input -> NanoUI a -> IO ()
checkIdleFullDamage failed ctx inpAfter inpIdle ui = do
  need <- needsRedraw ctx inpAfter inpIdle
  assert failed need
  _ <- runFrame ctx inpIdle ui
  dmg <- takeDamage ctx
  assert failed (dmg == DamageFull)

-- AlignEnd pins last-glyph ink, so "10" / "1i" / "1." share one right edge.
-- Lives here rather than with its test case because the pen and ink helpers
-- are internal to the library.
checkLabelAlignEndInk :: IORef Int -> IO ()
checkLabelAlignEndInk failed = do
  let
    gq xoff gw =
      GlyphQuad
        { gqX = xoff
        , gqY = 0
        , gqW = gw
        , gqH = 10
        , gqU0 = 0
        , gqV0 = 0
        , gqU1 = 1
        , gqV1 = 1
        }
    fm =
      (monospaceMetrics 10)
        { fmAdvance = \c -> case c of
            'i' -> 4
            '.' -> 4
            _ -> 10
        , fmGlyph = \c -> case c of
            'i' -> Just (gq 0.5 3)
            '.' -> Just (gq 1 1)
            _ -> Just (gq 1 8)
        }
    boxW = 100
    visualRight txt =
      let (tx, _) = alignedTextPen AlignEnd 0 boxW 0 fm txt
       in tx + textInkEnd fm txt
    r0 = visualRight "10"
    ri = visualRight "1i"
    rd = visualRight "1."
  when (abs (r0 - boxW) > 0.01) $ bump failed
  when (abs (ri - boxW) > 0.01) $ bump failed
  when (abs (rd - boxW) > 0.01) $ bump failed

windowTitleGrab :: Rect -> V2
windowTitleGrab (Rect x0 y0 _ _) = V2 (x0 + 24) (y0 + padT windowPad + 19.5)

runDragFrom :: Context -> Input -> NanoUI a -> V2 -> V2 -> IO ()
runDragFrom ctx inp0 ui grab dest = do
  let
    press = pressAt inp0 grab
  _ <- runFrame ctx press ui
  let
    moved = press {inputMousePos = dest, inputMousePressed = False}
  void (runFrame ctx moved ui)
