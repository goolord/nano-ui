{-# OPTIONS_GHC -O0 #-}
module Cases.RichText (tests) where

import Spec
import Control.Exception (evaluate)
import Data.Foldable (toList)
import Data.IntMap.Strict qualified as IM
import Data.List (groupBy, sortOn)
import Data.Maybe (listToMaybe)
import Data.Primitive.PrimArray (primArrayFromList)
import Data.Text qualified as T
import NanoUI.Internal.Context (Context (..), CustomDrawingEntry (..), DrawingCacheState (..), hoverZoneCrossed, intKey, lookupCustomDrawing)
import NanoUI.Internal.Context.Types (CustomDrawOpCacheEntry (..))
import NanoUI.Internal.Widgets.Custom (mkCustomDrawContext)
import NanoUI.Internal.Store (fieldRichSelection, lookupSlot, ptrEq)
import NanoUI.Shortcut (cmdOrCtrl, key)
import NanoUI.Testing.Sim

tests :: [Spec]
tests =
  [ spec "rich-text-wrap" runRichTextWrapTest
  , spec "rich-text-link" runRichTextLinkTest
  , spec "rich-text-align" runRichTextAlignTest
  , spec "rich-text-many" runRichTextManyTest
  , spec "rich-text-background" runRichTextBackgroundTest
  , spec "rich-text-scroll-cull" runRichTextScrollCullTest
  , spec "rich-text-relayout" runRichTextRelayoutTest
  , spec "rich-text-scroll-popup" runRichTextScrollPopupTest
  , spec "rich-text-selection" runSelectionTest
  ]

inp400 :: Input
inp400 = withInput 400 400

-- | The ops the rich text @resp@ draws in its last laid-out rect.
drawnOps :: Context -> Response -> IO [DrawOp]
drawnOps ctx resp = do
  Just entry <- lookupCustomDrawing ctx (respId resp)
  cdc <- mkCustomDrawContext ctx (ctxFontMetrics ctx) (respId resp)
  pure (toList (cdrBuild entry cdc (respRect resp)))

-- | The paragraph's rect and ops once the view settles.
settledOps :: Context -> Input -> NanoUI Response -> IO (Rect, [DrawOp])
settledOps ctx inp ui = warmup2 ctx inp ui >>= \resp -> (respRect resp,) <$> drawnOps ctx resp

-- | Each text op's position and text.
texts :: [DrawOp] -> [(Float, Float, T.Text)]
texts ops = [(x, y, t) | DrawTextStyled x y _ t _ <- ops]

-- | The ops a widget was last painted with, and their bounds.
cachedOps :: Context -> WidgetId -> IO (Maybe (Rect, [DrawOp]))
cachedOps c wid =
  fmap (\e -> (cdeBounds e, toList (cdeOps e))) . IM.lookup (intKey wid) . dcsCustomDrawOpCache <$> readIORef (ctxDrawingCache c)

-- | Focus @r@ and select all of it.
selectAll :: Context -> Input -> NanoUI a -> Response -> IO ()
selectAll c inp ui r = setFocus c (respId r) >> void (evalUi c (chordInp (cmdOrCtrl <> key 'a') inp) ui)

-- | Wrapping at the column's width, leading-space indents, 'textBreak'
-- policies (whole words, or anywhere but inside a flag), and newlines
-- beside joiners and marks under 'breakAnywhere'.
runRichTextWrapTest :: Context -> IORef Int -> IO ()
runRichTextWrapTest ctx failed = do
  let fm = ctxFontMetrics ctx
      heightOf f pieces = rectH . respRect . fst <$> warmup2 ctx (withInput 600 400) (column (richTextWith' f pieces))
      flag = "\x1F1FA\x1F1F8"
  lineH <- heightOf id ["word"]
  let paragraph = [inlineText (T.replicate 12 "word "), strong "bold", " end"]
  Rect _ _ w h <- respRect <$> warmup2 ctx inp400 (columnWith (fixedW 200) (fst <$> richText' paragraph))
  assert failed (lineH > 0 && w <= 200 && h >= 2 * lineH)
  assertEq failed 0 (round h `mod` round lineH :: Int)
  (Rect rx _ _ _, indented) <- settledOps ctx inp400 (column (fst <$> richText' ["  where\n    go = 1"]))
  case ([x - rx | (x, _, "where") <- texts indented], [x - rx | (x, _, "go") <- texts indented]) of
    ([wx], [gx]) -> assert failed (wx > 0 && abs (gx - 2 * wx) < 0.5)
    _ -> assert failed False
  advA <- lineWidthIO fm "a"
  advFlag <- lineWidthIO fm (T.take 1 flag)
  monoH <- heightOf fontMono ["a"]
  assert failed (monoH > 0 && advA > 0 && advFlag > 0)
  let word = [inlineText "abcdefghijklmnop"]
  assertEq failed [monoH, 2 * monoH]
    =<< sequence [heightOf (fontMono . fixedW (8 * advA)) word, heightOf (fontMono . breakAnywhere . fixedW (8.5 * advA)) word]
  -- Each flag is two regional indicators that fit a line only together.
  flagH <- heightOf id [inlineText flag]
  assertEq failed (3 * flagH) =<< heightOf (breakAnywhere . fixedW (1.5 * advFlag)) [inlineText (T.replicate 3 flag)]
  forM_ ["one\n\x200Dtwo", "one\x200D\ntwo", "one\n\x0301two"] $ \txt -> do
    (_, ops) <- settledOps ctx inp400 (column (fst <$> richTextWith' breakAnywhere [inlineText txt]))
    let drawn = [(y, t) | (_, y, t) <- texts ops]
    assertEq failed (txt, 2, T.filter (/= '\n') txt) $
      (txt, length (groupBy (\a b -> fst a == fst b) drawn), T.concat (map snd drawn))

-- | A link reports its target when clicked, shows the pointer and one
-- underline when hovered; text beside it, or a drag from there onto it,
-- reports nothing. A move along the link runs no frame, one onto it does.
runRichTextLinkTest :: Context -> IORef Int -> IO ()
runRichTextLinkTest ctx failed = do
  let fm = ctxFontMetrics ctx
      ui = column (richText' ["Go to ", hyperlink "docs-target" "the docs", " now"])
  prefixW <- sum <$> mapM (lineWidthIO fm) ["Go", " ", "to", " "]
  linkW <- sum <$> mapM (lineWidthIO fm) ["the", " ", "docs"]
  docsW <- lineWidthIO fm "docs"
  runSim ctx failed inp400 ui $ do
    Rect rx ry _ rh <- respRect . fst <$> result
    let pt dx = V2 (rx + dx) (ry + rh / 2)
        (onLink, onLinkStart, onText) = (pt (prefixW + linkW / 2), pt (prefixW + 1), pt 2)
        onLinkEnd = pt (prefixW + linkW - docsW / 2)
        clickAt p = moveTo (At p) >> snd <$> click (At p)
        cursorIsPointer p = (== UiCursorPointer) <$> cursorAt (At p)
    checkEq (Just "docs-target") =<< clickAt onLink
    check =<< cursorIsPointer onLink
    (_, dd, _) <- liftIO . (\i -> runFrame ctx i ui) =<< getInput
    quads <- liftIO (drawQuads dd)
    checkEq 1 (length [r | (r@(Rect _ _ w h), _) <- quads, h < 3, abs (w - linkW) < 0.5])
    checkEq Nothing =<< clickAt onText
    check . not =<< cursorIsPointer onText
    checkEq Nothing . snd =<< drag (At onText) (At onLink) 1
    _ <- moveTo (At onLinkStart)
    check . not =<< liftIO (hoverZoneCrossed ctx onLinkStart onLinkEnd)
    check =<< liftIO (hoverZoneCrossed ctx onText onLinkStart)

-- | Each line aligns within the box, a fit-width paragraph lands where a
-- full-width one does, and a word wider than the box starts at its left edge.
runRichTextAlignTest :: Context -> IORef Int -> IO ()
runRichTextAlignTest ctx failed = do
  let fm = ctxFontMetrics ctx
      paragraph = [inlineText "a few words of different lengths ", strong "wrapping", " over several lines here"]
      -- The paragraph's box and each drawn line's left and right edges.
      linesOf width align = do
        (r, ops) <- settledOps ctx inp400 (columnWith (fixedW 200) (fst <$> richTextWith' (width . align) paragraph))
        extents <- forM (texts ops) $ \(x, y, t) -> (\w -> (y, x, x + w)) <$> lineWidthIO fm t
        let lines' = groupBy (\(a, _, _) (b, _, _) -> a == b) extents
        pure (r, [(minimum [x0 | (_, x0, _) <- l], maximum [x1 | (_, _, x1) <- l]) | l <- lines'])
  forM_ [(alignEnd, 1), (alignCenter, 0.5 :: Float)] $ \(align, pt) -> do
    (Rect rx _ rw _, full) <- linesOf fillW align
    (_, fitted) <- linesOf id align
    assert failed (length full > 1)
    forM_ full $ \(x0, x1) -> assertEq failed (round (rx + pt * rw) :: Int) (round (x0 + pt * (x1 - x0)))
    assertEq failed full fitted
    (Rect ox _ _ _, ops) <-
      settledOps ctx inp400 (columnWith (fixedW 40) (fst <$> richTextWith' (fillW . align) ["supercalifragilistic", " short"]))
    let xs = [x | (x, _, _) <- texts ops]
    assert failed (not (null xs) && all (>= ox - 0.5) xs)

-- | A view with more paragraphs than the cache bound still keeps them all:
-- an unchanged frame measures nothing.
runRichTextManyTest :: Context -> IORef Int -> IO ()
runRichTextManyTest base failed = do
  measured <- newIORef (0 :: Int)
  recording <- newIORef False
  let fm = ctxFontMetrics base
      prepare _ = readIORef recording >>= \on -> fm <$ when on (modifyIORef' measured (+ 1))
  -- Evaluated once so every frame shares this context and its metrics source.
  ctx <- evaluate (withFontMetrics base fm {fmBackend = Just (FontBackend prepare (const (pure Nothing)))})
  let ui = column (forM_ [1 .. 4500 :: Int] (\i -> richText [inlineText (T.pack (show i))]))
      frame' = runFrame ctx inp400 (liftIO (writeIORef recording True) *> ui <* liftIO (writeIORef recording False))
  replicateM_ 3 frame'
  writeIORef measured 0
  replicateM_ 2 frame'
  assertEq failed 0 =<< readIORef measured

-- | A piece's background spans its words and inner spaces, under them, the
-- line box's height. Changing the colour counts as a new paragraph.
runRichTextBackgroundTest :: Context -> IORef Int -> IO ()
runRichTextBackgroundTest ctx failed = do
  let fm = ctxFontMetrics ctx
      tint = colorRGBA 1 2 3 255
      opsOf c = settledOps ctx inp400 . column $
        fst <$> richText' ["Run ", inlineBackground c (inlineCode " cabal build "), " first"]
  (Rect rx ry _ rh, ops) <- opsOf tint
  prefixW <- sum <$> mapM (lineWidthIO fm) ["Run", " ", " "]
  codeW <- sum <$> mapM (lineWidthIO fm) ["cabal", " ", "build"]
  -- One line, so its box is the paragraph's.
  case [r | FillRect r c <- ops, c == tint] of
    [Rect x y w h] -> assert failed (all ((< 0.5) . abs) [x - (rx + prefixW), w - codeW, y - ry, h - rh])
    fills -> assertEq failed 1 (length fills)
  -- Under the words: the fill comes before the first text op.
  assert failed (length (takeWhile (\case DrawTextStyled {} -> False; _ -> True) ops) >= 1)
  (_, plain) <- opsOf (colorRGBA 9 9 9 255)
  assertEq failed [] [r | FillRect r c <- plain, c == tint]

-- | A paragraph far taller than its scroller draws only the words near the
-- viewport, and every row of the viewport inside its padding still shows
-- one, wherever the scroller is. Monospace glyphs are boxes a line tall.
runRichTextScrollCullTest :: Context -> IORef Int -> IO ()
runRichTextScrollCullTest ctx failed = do
  let inp = withInputOff 300 220
      paragraph = [inlineText (T.pack ("word" <> show i <> " ")) | i <- [1 .. 2000 :: Int]]
      ui = fmap fst $ scrollArea (fillW . fixedH 100) (richTextWith fillW paragraph)
  sid <- warmup2 ctx inp ui
  forM_ [0, 3333, 1000000] $ \off -> do
    setScrollOffset ctx sid off
    (_, draw) <- warmupDraw ctx inp ui
    assertJustM failed (getPrevRect ctx sid) $ \(Rect rx ry rw rh) -> do
      quads <- drawQuads draw
      -- Glyph boxes, not the scroller's backdrop, border or bar.
      let glyphs = [r | (r@(Rect qx _ qw qh), _) <- quads, qw < 40, qh < 40, qx < rx + rw / 2]
          shown y = any (\(Rect _ gy _ gh) -> gy <= y && y < gy + gh) glyphs
      assert failed (length quads < 1000 && all shown [ry + fromIntegral k | k <- [12, 17 .. floor rh - 12 :: Int]])

-- | A paragraph resized every frame, or capped under a width while hovered,
-- edited and resized, draws as one laid out from the start. The same pieces
-- (a Markdown document's closed blocks) still follow layout and theme.
runRichTextRelayoutTest :: Context -> IORef Int -> IO ()
runRichTextRelayoutTest ctx failed = do
  let paragraph t = [inlineText t, strong "wrapping", " over several lines ", hyperlink "x" "here"]
      ui width t = column (fst <$> richTextWith' width (paragraph t))
      sameAsFresh c inp view = do
        drawn <- evalUi c inp view >>= cachedOps c . respId
        fresh <- newContext >>= \f -> warmup f inp view >> evalUi f inp view >>= cachedOps f . respId
        assert failed (drawn /= Nothing && drawn == fresh)
        pure drawn
      resized = ui fillW "a few words of different lengths "
  void (warmup2 ctx (withInputOff 400 400) resized)
  forM_ [180, 260, 150, 320, 400] $ \w -> sameAsFresh ctx (withInputOff w 400) resized
  capCtx <- newContext
  let cap = 170
      step inp t =
        sameAsFresh capCtx inp (ui (maxW cap) t) >>= mapM_ (\(Rect _ _ w h, _) -> do
          laidOut <- newContext >>= \c -> warmup2 c inp (ui (fixedW w) t)
          assertEq failed (rectH (respRect laidOut)) h)
      (short, long) = ("a few words ", "other words, a much longer run of them this time, enough for more lines ")
  Rect rx ry rw rh <- respRect <$> warmup2 capCtx (withInputOff 400 400) (ui (maxW cap) short)
  -- Under its cap, so the solver offers it a width it does not take.
  assert failed (rw > 0 && rw < cap)
  let over = (withInputOff 400 400) {inputMousePos = V2 (rx + rw / 2) (ry + rh / 2)}
  mapM_ (uncurry step) [(over, short), (withInputOff 400 400, short), (over, short), (over, long), (withInputOff 120 400, long)]
  mapM_ (step (withInputOff 400 400)) [long, short]
  let pieces = ["kept ", strong "pieces", hyperlink "x" " here"]
      (red, blue) = (colorRGBA 200 10 10 255, colorRGBA 10 10 200 255)
      colorsOf theme f = do
        (_, ops) <- settledOps ctx inp400 (column (setUiTheme theme >> fst <$> richTextWith' f pieces))
        pure [c | DrawTextStyled _ _ _ "kept" c <- ops]
  assertEq failed [[red], [blue]] =<< mapM (colorsOf defaultTheme . fontColor) [red, blue]
  dark <- colorsOf defaultTheme id
  light <- colorsOf defaultLightTheme id
  assert failed (length dark == 1 && dark /= light)

-- | A scroll area in a fixed-width popup is as tall as its wrapped paragraphs,
-- as a column is, up to the popup's height cap, past which it scrolls.
runRichTextScrollPopupTest :: Context -> IORef Int -> IO ()
runRichTextScrollPopupTest _ failed = do
  let cfg = (defaultPopupConfig (AnchorPoint (V2 20 20))) {cfgPlacement = PlacementBelow, cfgDismissable = False}
      paragraph = [inlineText (T.replicate 60 "word ")]
      body = do
        one <- fst <$> richTextWith' (tight . fillW . fontMono) ["word"]
        _ <- richTextWith' (tight . fillW . fontMono) paragraph
        b <- fst <$> richTextWith' (tight . fillW) paragraph
        pure (rectH (respRect one), respRect b)
      -- The popup's rect and the body's result, laid out from the start.
      inPopup inner = do
        c <- newContext
        (resp, r) <- warmup2 c (withInput 800 800) (popupWith' (constVar True) cfg (fixedW 560 . maxH 420) inner)
        pure (c, respRect resp, r)
      bottom r = rectY r + rectH r
  (_, columnPopup, Just (lineH, wrapped)) <- inPopup (columnWith fillW body)
  -- Each paragraph wraps to several lines, and all of them stay under the cap.
  assert failed (rectH wrapped >= 3 * lineH && rectH columnPopup < 400)
  forM_ [id, columnWith (tight . fillW)] $ \holder -> do
    (c, scrollPopup, Just (sid, (_, last'))) <- inPopup (scrollArea fillW (holder body))
    assertEq failed (round (rectH columnPopup) :: Int) (round (rectH scrollPopup))
    assertJustM failed (getPrevRect c sid) $ \view -> assert failed (bottom last' <= bottom view + 0.5)
  (c, cappedPopup, Just (sid, rects)) <- inPopup (scrollArea fillW (replicateM 4 body))
  assertEq failed 420 (round (rectH cappedPopup) :: Int)
  -- Inside the popup and its padding, with more below than it shows.
  assertJustM failed (getPrevRect c sid) $ \view ->
    assert failed (rectH view > 0 && bottom view <= bottom cappedPopup - 6 + 0.5 && bottom (snd (last rects)) > bottom view)

-- | A selection shows only while focused, with no focus ring, and survives
-- styling, streaming and cache eviction. A shaped 'breakAnywhere' line
-- highlights without a gap.
runSelectionTest :: Context -> IORef Int -> IO ()
runSelectionTest base failed = do
  theme <- readIORef (ctxTheme base)
  let inp = withInputOff 500 300
      selection ops = [r | FillRect r c <- ops, c == themeSelection theme]
      paragraph t = fst <$> selectableRichText' [t]
      two = column ((,) <$> paragraph "first paragraph" <*> paragraph "second paragraph")
  (a, b) <- warmup2 base inp two
  selectAll base inp two a
  selected <- drawnOps base a
  assert failed (not (null (selection selected)) && null [() | StrokeRoundedRect {} <- selected])
  runClick base inp two (centerOf b) >> warmup base inp two
  assert failed . null . selection =<< drawnOps base a
  copied <- newIORef Nothing
  ctx <- (\c -> withClipboard c (readIORef copied) (\t -> True <$ writeIORef copied (Just t))) <$> newContext
  let pieces suffix = ["one ", strong "two\n", inlineText ("three" <> suffix)]
      ui style suffix = selectableRichTextWith' (fixedW 240 . style) (pieces suffix)
      copyFrom c view = do
        writeIORef copied Nothing
        _ <- evalUi c (chordInp (cmdOrCtrl <> key 'c') inp) view
        assertEq failed (Just "one two\nthree") =<< readIORef copied
  (r, _) <- warmup2 ctx inp (ui id "")
  selectAll ctx inp (ui id "") r
  forM_ [fontColor (colorRGB 200 20 20), fontSize 24, alignEnd] $ \style ->
    evalUi ctx inp (ui style "") >> copyFrom ctx (ui style "")
  -- A stream appending text preserves the existing range rather than clearing it.
  copyFrom ctx (ui id " appended")
  -- Evict all measurement state, keeping widget identity and interaction state.
  evicted <- (\p -> ctx {ctxParagraphs = p}) <$> newHost
  copyFrom evicted (ui id " appended")
  let slot = lookupSlot fieldRichSelection (intKey (respId r)) <$> getStore evicted
  before <- slot
  replicateM_ 3 (evalUi evicted inp (ui id " appended"))
  after <- slot
  assert failed (maybe False id (ptrEq <$> before <*> after))
  let txt = "a\x65E5\&e\x0301\&b"
      fm = monospaceMetrics 10
      shapedTxt = ShapedText 47 47 (primArrayFromList [0, 10, 27, 32, 37, 47])
      prepare t = pure fm {fmShape = \t' -> if t' == t && t == txt then Just shapedTxt else Nothing}
      shapedUi = column (fst <$> selectableRichTextWith' breakAnywhere [inlineText txt])
  shaped <- newContext >>= \c -> evaluate (withFontMetrics c fm {fmBackend = Just (FontBackend prepare (const (pure Nothing)))})
  resp <- warmup2 shaped inp shapedUi
  let Rect rx _ rw _ = respRect resp
  assertEq failed 47 rw
  selectAll shaped inp shapedUi resp
  ranges <- sortOn fst . map (\(Rect x _ w _) -> (x - rx, x - rx + w)) . selection <$> drawnOps shaped resp
  assertEq failed (Just 0, 47) (fst <$> listToMaybe ranges, maximum (0 : map snd ranges))
  assert failed (and (zipWith (\(_, e) (s, _) -> s <= e + 0.01) ranges (drop 1 ranges)))
