module Cases.RichText (tests) where

import Spec
import Control.Exception (evaluate)
import Data.Foldable (toList)
import Data.IntMap.Strict qualified as IM
import Data.List (groupBy)
import Data.Text qualified as T
import NanoUI.Internal.Context (Context (..), CustomDrawingEntry (..), DrawingCacheState (..), intKey, lookupCustomDrawing)
import NanoUI.Internal.Context.Types (CustomDrawOpCacheEntry (..))
import NanoUI.Internal.Widgets.Custom (mkCustomDrawContext)

tests :: [Spec]
tests =
  [ spec "rich-text-wrap" runRichTextWrapTest
  , spec "rich-text-link" runRichTextLinkTest
  , spec "rich-text-align" runRichTextAlignTest
  , spec "rich-text-many" runRichTextManyTest
  , spec "rich-text-background" runRichTextBackgroundTest
  , spec "rich-text-scroll-cull" runRichTextScrollCullTest
  , spec "rich-text-resize" runRichTextResizeTest
  , spec "rich-text-capped" runRichTextCappedTest
  ]

-- | A paragraph wraps at its column's width, taking a line's height per line,
-- and mixed pieces share a line.
runRichTextWrapTest :: Context -> IORef Int -> IO ()
runRichTextWrapTest ctx failed = do
  let inp = withInput 400 400
      paragraph = [inlineText (T.replicate 12 "word "), strong "bold", " end"]
      ui = columnWith (fixedW 200) $ do
        one <- fst <$> richText' ["word"]
        wrapped <- fst <$> richText' paragraph
        pure (one, wrapped)
  (one, wrapped) <- warmup2 ctx inp ui
  let Rect _ _ _ lineH = respRect one
      Rect _ _ w h = respRect wrapped
  assert failed (lineH > 0)
  assert failed (w <= 200)
  -- Twelve words and two more pieces cannot fit on one 200px line.
  assert failed (h >= 2 * lineH)
  assertEq failed 0 (round h `mod` round lineH :: Int)

-- | A link reports its target when clicked and shows the pointer cursor;
-- text beside it reports nothing.
runRichTextLinkTest :: Context -> IORef Int -> IO ()
runRichTextLinkTest ctx failed = do
  let inp0 = withInput 400 400
      ui = column (richText' ["Go to ", hyperlink "docs-target" "the docs", " now"])
      fm = ctxFontMetrics ctx
  (resp, _) <- warmup2 ctx inp0 ui
  prefixW <- sum <$> mapM (lineWidthIO fm) ["Go", " ", "to", " "]
  linkW <- sum <$> mapM (lineWidthIO fm) ["the", " ", "docs"]
  let Rect rx ry _ rh = respRect resp
      onLink = V2 (rx + prefixW + linkW / 2) (ry + rh / 2)
      onText = V2 (rx + 2) (ry + rh / 2)
      clickAt pos = do
        let (press, release) = clickPair inp0 pos
        void (runFrame ctx inp0 {inputMousePos = pos} ui)
        void (runFrame ctx press ui)
        ((_, clicked), _, _, _) <- runFrame ctx release ui
        pure clicked
  linkClick <- clickAt onLink
  assertEq failed (Just "docs-target") linkClick
  handShown <- cursorKindIs ctx inp0 {inputMousePos = onLink} UiCursorPointer
  assert failed handShown
  -- The hovered link is underlined once, across its words and the space
  -- between them.
  (_, _, dd, _) <- runFrame ctx inp0 {inputMousePos = onLink} ui
  quads <- drawQuads dd
  let underlines = [r | (r@(Rect _ _ w h), _) <- quads, h < 3, abs (w - linkW) < 0.5]
  assertEq failed 1 (length underlines)
  textClick <- clickAt onText
  assertEq failed Nothing textClick
  plainCursor <- cursorKindIs ctx inp0 {inputMousePos = onText} UiCursorPointer
  assert failed (not plainCursor)

-- | Each line of a paragraph, wrapped or not, aligns within its box: flush
-- right for 'alignEnd', centred for 'alignCenter'. A fit-width paragraph is
-- placed in its column by the same alignment at its wrapped width, so its
-- lines land where a full-width paragraph's do.
runRichTextAlignTest :: Context -> IORef Int -> IO ()
runRichTextAlignTest ctx failed = do
  let inp = withInput 400 400
      fm = ctxFontMetrics ctx
      paragraph = [inlineText "a few words of different lengths ", strong "wrapping", " over several lines here"]
      -- The paragraph's box and each drawn line's left and right edges.
      linesOf width align = do
        resp <- warmup2 ctx inp (columnWith (fixedW 200) (fst <$> richTextWith' (width . align) paragraph))
        let wid = respId resp
            r = respRect resp
        Just entry <- lookupCustomDrawing ctx wid
        cdc <- mkCustomDrawContext ctx fm wid
        extents <- forM [(x, y, t) | DrawTextStyled x y _ t _ <- toList (cdrBuild entry cdc r)] $ \(x, y, t) ->
          (\w -> (y, x, x + w)) <$> lineWidthIO fm t
        let lines' = groupBy (\(a, _, _) (b, _, _) -> a == b) extents
        pure (r, [(minimum [x0 | (_, x0, _) <- l], maximum [x1 | (_, _, x1) <- l]) | l <- lines'])
  forM_ [(alignEnd, 1), (alignCenter, 0.5)] $ \(align, at :: Float) -> do
    (Rect rx _ rw _, full) <- linesOf fillW align
    (_, fitted) <- linesOf id align
    assert failed (length full > 1)
    forM_ full $ \(x0, x1) ->
      assertEq failed (round (rx + at * rw) :: Int) (round (x0 + at * (x1 - x0)))
    assertEq failed full fitted

-- | A view with more paragraphs than the cache bound still keeps them all:
-- an unchanged frame measures nothing.
runRichTextManyTest :: Context -> IORef Int -> IO ()
runRichTextManyTest base failed = do
  measured <- newIORef (0 :: Int)
  recording <- newIORef False
  let fm = ctxFontMetrics base
      prepare _ = readIORef recording >>= \on -> fm <$ when on (modifyIORef' measured (+ 1))
  -- Evaluated once in IO so every frame shares this context and its metrics
  -- source.
  ctx <- evaluate (withFontMetrics base fm {fmBackend = Just (FontBackend prepare (const (pure Nothing)))})
  let inp = withInput 400 400
      ui = column (forM_ [1 .. 4500 :: Int] (\i -> richText [inlineText (T.pack (show i))]))
      frame = runFrame ctx inp (liftIO (writeIORef recording True) *> ui <* liftIO (writeIORef recording False))
  replicateM_ 3 frame
  writeIORef measured 0
  replicateM_ 2 frame
  assertEq failed 0 =<< readIORef measured

-- | A piece's background is painted under its words, spanning inner spaces
-- but not leading or trailing ones, once per line. Changing the colour
-- counts as a new paragraph.
runRichTextBackgroundTest :: Context -> IORef Int -> IO ()
runRichTextBackgroundTest ctx failed = do
  let inp = withInput 400 400
      fm = ctxFontMetrics ctx
      tint = colorRGBA 1 2 3 255
      ui c = column (fst <$> richText' ["Run ", inlineBackground c (inlineCode " cabal build "), " first"])
      opsOf c = do
        resp <- warmup2 ctx inp (ui c)
        Just entry <- lookupCustomDrawing ctx (respId resp)
        cdc <- mkCustomDrawContext ctx fm (respId resp)
        pure (respRect resp, toList (cdrBuild entry cdc (respRect resp)))
  (Rect rx _ _ _, ops) <- opsOf tint
  prefixW <- sum <$> mapM (lineWidthIO fm) ["Run", " ", " "]
  codeW <- sum <$> mapM (lineWidthIO fm) ["cabal", " ", "build"]
  let fills = [r | FillRect r c <- ops, c == tint]
      firstText = length (takeWhile (\case DrawTextStyled {} -> False; _ -> True) ops)
  assertEq failed 1 (length fills)
  forM_ fills $ \(Rect x _ w h) ->
    assert failed (abs (x - (rx + prefixW)) < 0.5 && abs (w - codeW) < 0.5 && h > 0)
  -- Under the words: the fill comes before the first text op.
  assert failed (firstText >= 1)
  (_, plain) <- opsOf (colorRGBA 9 9 9 255)
  assertEq failed [] [r | FillRect r c <- plain, c == tint]

-- | A paragraph far taller than its scroller draws only the words near the
-- viewport, and every row of the viewport inside its padding still shows
-- one, wherever the scroller is. Monospace glyphs are boxes a line tall.
runRichTextScrollCullTest :: Context -> IORef Int -> IO ()
runRichTextScrollCullTest ctx failed = do
  let inp0 = withInputOff 300 220
      paragraph = [inlineText (T.pack ("word" <> show i <> " ")) | i <- [1 .. 2000 :: Int]]
      ui = fmap fst $ scrollArea (fillW . fixedH 100) (richTextWith fillW paragraph)
  sid <- warmup2 ctx inp0 ui
  forM_ [0, 3333, 1000000] $ \off -> do
    setScrollOffset ctx sid off
    _ <- runFrame ctx inp0 ui
    (_, _, draw, _) <- runFrame ctx inp0 ui
    assertJustM failed (getPrevRect ctx sid) $ \(Rect rx ry rw rh) -> do
      quads <- drawQuads draw
      -- Glyph boxes, not the scroller's backdrop, border or bar.
      let glyphs = [r | (r@(Rect qx _ qw qh), _) <- quads, qw < 40, qh < 40, qx < rx + rw / 2]
          shown y = any (\(Rect _ gy _ gh) -> gy <= y && y < gy + gh) glyphs
      assert failed (length quads < 1000)
      assert failed (all shown [ry + fromIntegral k | k <- [12, 17 .. floor rh - 12 :: Int]])
-- | A paragraph whose width changes every frame draws what one laid out at
-- each width from the start draws.
runRichTextResizeTest :: Context -> IORef Int -> IO ()
runRichTextResizeTest ctx failed = do
  let paragraph = [inlineText "a few words of different lengths ", strong "wrapping", " over several lines ", hyperlink "x" "here"]
      ui = column (respId . fst <$> richTextWith' fillW paragraph)
      -- The ops the paragraph was drawn with in a frame at width @w@.
      opsAt c w = do
        wid <- evalUi c (withInputOff w 400) ui
        fmap (toList . cdeOps) . IM.lookup (intKey wid) . dcsCustomDrawOpCache <$> readIORef (ctxDrawingCache c)
  void (warmup2 ctx (withInputOff 400 400) ui)
  forM_ [180, 260, 150, 320, 400] $ \w -> do
    resized <- opsAt ctx w
    fresh <- newContext >>= \c -> warmup c (withInputOff w 400) ui >> opsAt c w
    assert failed (resized /= Nothing && resized == fresh)

-- | A paragraph capped at a width it stays under, hovered on and off, edited
-- and resized, draws at the rect and with the ops one laid out from the start
-- draws, and is as tall as one laid out at the width it takes: what it keeps
-- between frames at the capped width and at its own never goes stale.
runRichTextCappedTest :: Context -> IORef Int -> IO ()
runRichTextCappedTest ctx failed = do
  let cap = 170
      paragraph t = [inlineText t, strong "wrapping", " over several lines ", hyperlink "x" "here"]
      ui width t = column ((\r -> (respId r, respRect r)) . fst <$> richTextWith' width (paragraph t))
      -- The rect and ops the paragraph was drawn with in a frame with this
      -- input.
      drawnAt c inp t = do
        (wid, _) <- evalUi c inp (ui (maxW cap) t)
        entry <- IM.lookup (intKey wid) . dcsCustomDrawOpCache <$> readIORef (ctxDrawingCache c)
        pure ((\e -> (cdeBounds e, toList (cdeOps e))) <$> entry)
      step inp t = do
        drawn <- drawnAt ctx inp t
        fresh <- newContext >>= \c -> warmup c inp (ui (maxW cap) t) >> drawnAt c inp t
        assert failed (drawn /= Nothing && drawn == fresh)
        forM_ drawn $ \(Rect _ _ w h, _) -> do
          (_, Rect _ _ _ laidOutH) <- newContext >>= \c -> warmup2 c inp (ui (fixedW w) t)
          assertEq failed laidOutH h
      short = "a few words "
      long = "other words, a much longer run of them this time, enough for more lines "
  (_, Rect rx ry rw rh) <- warmup2 ctx (withInputOff 400 400) (ui (maxW cap) short)
  -- Under its cap, so the solver offers it a width it does not take.
  assert failed (rw > 0 && rw < cap)
  let over = (withInputOff 400 400) {inputMousePos = V2 (rx + rw / 2) (ry + rh / 2)}
  step over short
  step (withInputOff 400 400) short
  step over short
  step over long
  step (withInputOff 120 400) long
  step (withInputOff 400 400) long
  step (withInputOff 400 400) short
