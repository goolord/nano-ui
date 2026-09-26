module Cases.RichText (tests) where

import Spec
import Control.Exception (evaluate)
import Data.Foldable (toList)
import Data.List (groupBy)
import Data.Text qualified as T
import NanoUI.Internal.Context (CustomDrawingEntry (..), lookupCustomDrawing)
import NanoUI.Internal.Widgets.Custom (mkCustomDrawContext)

tests :: [Spec]
tests =
  [ spec "rich-text-wrap" runRichTextWrapTest
  , spec "rich-text-link" runRichTextLinkTest
  , spec "rich-text-align" runRichTextAlignTest
  , spec "rich-text-many" runRichTextManyTest
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

-- | A paragraph's lines, wrapped or not, sit in its box as its alignment
-- says: each ends at the box's right edge for 'alignEnd', and is centred in
-- it for 'alignCenter'. One as wide as its text sits in its column as the
-- same alignment says, at the width it wraps to there, so its lines land
-- where a full-width one's do.
runRichTextAlignTest :: Context -> IORef Int -> IO ()
runRichTextAlignTest ctx failed = do
  let inp = withInput 400 400
      fm = ctxFontMetrics ctx
      paragraph = [inlineText "a few words of different lengths ", strong "wrapping", " over several lines here"]
      -- Each drawn line's left and right edges, and the paragraph's box.
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

-- | Past the cache's bound, a view that draws more paragraphs than it keeps
-- them all: a frame that changes nothing measures nothing.
runRichTextManyTest :: Context -> IORef Int -> IO ()
runRichTextManyTest base failed = do
  measured <- newIORef (0 :: Int)
  recording <- newIORef False
  let fm = ctxFontMetrics base
      prepare _ = readIORef recording >>= \on -> fm <$ when on (modifyIORef' measured (+ 1))
  -- Bound in IO, so that every frame gets this one context, and with it the
  -- same metrics source.
  ctx <- evaluate (withFontMetrics base fm {fmBackend = Just (FontBackend prepare (const (pure Nothing)))})
  let inp = withInput 400 400
      ui = column (forM_ [1 .. 4500 :: Int] (\i -> richText [inlineText (T.pack (show i))]))
      frame = runFrame ctx inp (uiIO (writeIORef recording True) *> ui <* uiIO (writeIORef recording False))
  replicateM_ 3 frame
  writeIORef measured 0
  replicateM_ 2 frame
  assertEq failed 0 =<< readIORef measured
