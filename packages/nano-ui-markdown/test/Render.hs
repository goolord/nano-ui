-- | Drawing documents in a headless context.
module Render (spec) where

import Control.Concurrent (threadDelay)
import Control.Monad (filterM, forM, replicateM_, void, when)
import Data.Foldable (toList)
import Data.Function (on)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (find, groupBy, nub, sortOn)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (peekByteOff)
import NanoUI
  ( Color, DrawOp (..), ImageId (..), Input (..), NanoUI, NanoUIEs, Rect (..), Size (..), Style (..), TextFont (..), Theme (..)
  , V2 (..), WidgetId, background, checkboxBoxSize, colorA, colorB, colorG, colorR, colorRGBA, columnWith, drawCheckbox, fillW, fixedH, fixedW, fontColor, foreground
  , getScrollOffset, label, padAll, panel, rectIntersect, runCanvasFor, scrollArea, uiIO
  )
import NanoUI.Backend (FontBackend (..), FontMetrics (..), monospaceMetrics)
import NanoUI.Internal.Context (Context (..), CustomDrawingEntry (..), lookupCustomDrawing)
import NanoUI.Internal.Layout.Arena (NodeType (..), arenaCount, getNodeRect, getNodeType, getWidgetId)
import NanoUI.Internal.Widgets.Custom (mkCustomDrawContext)
import NanoUI.Markdown
import NanoUI.Testing
  ( Damage (..), DrawData (..), collectOverlayTextSpans, collectTextSpans, newContext, runFrame, takeDamage, withClipboard
  , withFontMetrics
  )
import NanoUI.Testing.Assert (withInput)
import NanoUI.Testing.Harness (covers, hasText, runClick, spanCenter, spanRect, spanRectOf, warmup, warmup2)
import Test.Hspec

-- | A word of rich text as drawn: its widget, where its line box starts, its
-- font size, its text and its colour.
data Word' = Word' {wWidget :: WidgetId, wPos :: V2, wSize :: Float, wText :: Text, wColor :: Color}
  deriving (Show)

-- | The frame's layout nodes of a type: their ids and rects, in document
-- order.
nodesOf :: NodeType -> Context -> IO [(WidgetId, Rect)]
nodesOf wanted ctx = do
  let na = ctxNodeArena ctx
  n <- arenaCount na
  matching <- filterM (fmap (== wanted) . getNodeType na) [0 .. n - 1]
  forM matching $ \i -> (,) <$> getWidgetId na i <*> getNodeRect na i

-- | The frame's custom drawings, rich text and canvases: each one's id,
-- content key and ops, in document order.
drawings :: Context -> IO [(WidgetId, Int, [DrawOp])]
drawings ctx = do
  nodes <- nodesOf NodeDrawing ctx
  fmap concat . forM nodes $ \(wid, r) -> do
    entry <- lookupCustomDrawing ctx wid
    cdc <- mkCustomDrawContext ctx (ctxFontMetrics ctx) wid
    pure [(wid, cdrContent e, toList (cdrBuild e cdc r)) | Just e <- [entry]]

-- | Every word the frame's rich-text widgets draw, in document order.
drawnWords :: Context -> IO [Word']
drawnWords ctx = do
  ds <- drawings ctx
  pure [Word' wid (V2 tx ty) (textFontSize font) t c | (wid, _, ops) <- ds, DrawTextStyled tx ty font t c <- ops]

-- | The frame's list markers, in document order: the drawings with no text.
markers :: Context -> IO [(WidgetId, Int, [DrawOp])]
markers ctx = filter (\(_, _, ops) -> not (any isText ops)) <$> drawings ctx
  where
    isText = \case DrawTextStyled {} -> True; DrawText {} -> True; _ -> False

wordNamed :: Text -> [Word'] -> Maybe Word'
wordNamed t = find ((== t) . wText)

-- | The rectangles the frame's rich-text widgets fill in a colour: their
-- pieces' backgrounds and decorations.
drawnFills :: Color -> Context -> IO [Rect]
drawnFills c ctx = do
  ds <- drawings ctx
  pure [rect | (_, _, ops) <- ds, FillRect rect c' <- ops, c' == c]

-- | Whether a frame, painted in full, draws anything in a colour.
paints :: Color -> NanoUI a -> IO Bool
paints c ui = do
  ctx <- drawn 600 400 ui
  writeIORef (ctxPaintFull ctx) True
  (_, _, dd, _) <- runFrame ctx (withInput 600 400) ui
  withForeignPtr (drawVertices dd) $ \vp ->
    fmap or . forM [0 .. drawVertexCount dd - 1] $ \i -> do
      rgba <- forM [2 .. 5] $ \k -> peekByteOff vp (i * 32 + 4 * k) :: IO Float
      pure (map (round . (* 255)) rgba == map (fromIntegral :: Word8 -> Int) [colorR c, colorG c, colorB c, colorA c])

-- | A fresh context that has drawn a view twice in a window @w@ by @h@.
drawn :: Float -> Float -> NanoUI a -> IO Context
drawn w h ui = newContext >>= \ctx -> ctx <$ warmup2 ctx (withInput w h) ui

-- | Draw a document, append text to it and draw it again: each pair's first
-- word, drawn before, keeps its id as the second after, and the new words
-- are drawn.
idsAcross :: Text -> Text -> [(Text, Text)] -> [Text] -> Expectation
idsAcross start more same new = do
  let doc = parseMarkdown start
  ctx <- drawn 600 600 (view doc)
  was <- drawnWords ctx
  _ <- warmup2 ctx (withInput 600 600) (view (appendMarkdown more doc))
  now <- drawnWords ctx
  let idOf w ws = wWidget <$> wordNamed w ws
  mapM_ (\(w, w') -> (w', isJust (idOf w was), idOf w' now) `shouldBe` (w', True, idOf w was)) same
  mapM_ (\w -> map wText now `shouldContain` [w]) new

view :: MarkdownDoc -> NanoUI (Maybe Text)
view doc = columnWith (fixedW 500 . padAll 10) (markdown doc)

-- | Click a drawn word of a view in a 600 by 400 window, and return the
-- link the view reports.
clickWord :: NanoUI (Maybe Text) -> Text -> IO (Maybe Text)
clickWord ui w = do
  let inp = withInput 600 400
  ctx <- drawn 600 400 ui
  Just (Word' _ (V2 x y) _ _ _) <- wordNamed w <$> drawnWords ctx
  let pos = V2 (x + 3) (y + fmLineHeight (ctxFontMetrics ctx) / 2)
  warmup ctx inp {inputMousePos = pos} ui
  runClick ctx inp ui pos

-- | Draws cat.png at 40 by 30 and no other image.
catImages :: MarkdownConfig es
catImages = defaultMarkdownConfig {mdImage = \src -> if src == "cat.png" then Just (ImageId 7, Size 40 30) else Nothing}

-- | Draws a code block in Haskell as a label of its own, and every other
-- block as the widget does.
customCode :: MarkdownConfig NanoUIEs
customCode =
  defaultMarkdownConfig
    { mdBlock = \own -> \case
        CodeBlock "hs" code -> Nothing <$ label ("custom " <> code)
        b -> own b
    }

spec :: Spec
spec = do
  it "draws every kind of block's text" $ do
    ctx <-
      drawn 600 800 . view . parseMarkdown $
        "# Title\n\nHello *world*, see [the docs](https://example.com).\n\n\
        \- one\n- [x] two\n\n1. first\n\n> quoted\n\n| h1 | h2 |\n|---|---|\n| c1 | c2 |\n\n\
        \```hs\ncode here\n```\n\n---\n\n![alt text](missing.png)"
    ws <- map wText <$> drawnWords ctx
    mapM_ (\w -> ws `shouldContain` [w]) ["Title", "Hello", "world", "docs", "one", "two", "first", "quoted", "h1", "c2", "alt", "text"]
    -- Code is a label, so it is among the frame's text spans.
    spans <- collectTextSpans ctx
    map (`hasText` spans) ["code here", "hs"] `shouldBe` [True, True]

  it "draws headings larger than body text" $ do
    ws <- drawnWords =<< drawn 600 400 (view (parseMarkdown "# Big\n\nsmall"))
    let size w = maybe 0 wSize (wordNamed w ws)
    size "Big" `shouldSatisfy` (> max 16 (size "small"))

  it "scales headings and small text from the backend's default font size" $ do
    -- The default resolver's text is as tall as its size: this one's is 20.
    ctx <- (`withFontMetrics` monospaceMetrics 20) <$> newContext
    _ <- warmup2 ctx (withInput 600 800) . view . parseMarkdown $
      "# h1\n\n## h2\n\n### h3\n\n#### h4\n\n##### h5\n\n###### h6\n\nbody\n\n```\ncode\n```"
    ws <- drawnWords ctx
    let size w = maybe 0 wSize (wordNamed w ws)
    map (round . (* 100) . size) ["body", "h1", "h2", "h3", "h4", "h5", "h6"] `shouldBe` [2000, 3200, 2700, 2400, 2100, 2000, 1800 :: Int]
    -- The copy button's text is 0.8 of the body text's.
    fmap (\(Rect _ _ _ h) -> h) . spanRect "Copy" <$> collectTextSpans ctx `shouldReturn` Just 16

  it "returns the destination of a clicked link" $ do
    let ui = view (parseMarkdown "Read [the docs](https://example.com/docs) or <https://auto.link> now.")
    clickWord ui "docs" `shouldReturn` Just "https://example.com/docs"
    clickWord ui "https://auto.link" `shouldReturn` Just "https://auto.link"
    clickWord ui "Read" `shouldReturn` Nothing

  it "keeps earlier blocks' ids while text streams in" $
    -- The block that grew keeps its id too.
    idsAcross "para one\n\n- item two\n\nstrea" "ming on\n\n# new block" [("one", "one"), ("two", "two"), ("strea", "streaming")] ["new"]

  it "keeps the ids of the items and paragraphs inside a growing list or quote" $ do
    -- A list gains a nested list and an item while its second item grows; a
    -- quote's second paragraph grows, and a third follows.
    idsAcross "- first\n- sec" "ond\n  - nested\n- third" [("first", "first"), ("sec", "second")] ["nested", "third"]
    idsAcross "> quoted\n>\n> gro" "wing\n>\n> after" [("quoted", "quoted"), ("gro", "growing")] ["after"]

  it "measures and repaints only the block that grew when text is appended" $ do
    base <- newContext
    measured <- newIORef []
    inView <- newIORef False
    -- The body font records every text measured in it while the view runs,
    -- which is when rich text lays its words out; painting comes after.
    let fm = ctxFontMetrics base
        prepare t = readIORef inView >>= \recording -> fm <$ when recording (modifyIORef' measured (t :))
        ctx = withFontMetrics base fm {fmBackend = Just (FontBackend prepare (const (pure Nothing)))}
        inp = withInput 600 600
        doc1 = parseMarkdown "first alpha\n\nsecond beta\n\nthird gam"
        doc2 = appendMarkdown "ma delta" doc1
        measuring doc = do
          writeIORef measured []
          void (runFrame ctx inp (uiIO (writeIORef inView True) *> view doc <* uiIO (writeIORef inView False)))
          readIORef measured
    measuring doc1 >>= (`shouldContain` ["first"])
    _ <- measuring doc1
    writeIORef (ctxPaintFull ctx) False
    words' <- measuring doc2
    words' `shouldContain` ["delta"]
    mapM_ (\w -> words' `shouldNotContain` [w]) ["first", "alpha", "second", "beta"]
    -- The damage covers the line that grew and neither line before it.
    damage <- takeDamage ctx
    ws <- drawnWords ctx
    let hit w = [isJust (rectIntersect r (Rect 0 y 600 (fmLineHeight fm))) | DamageClip r <- [damage], Word' _ (V2 _ y) _ _ _ <- toList (wordNamed w ws)]
    map hit ["alpha", "beta", "gamma"] `shouldBe` [[False], [False], [True]]
    -- Drawing the same document again measures nothing.
    measuring doc2 `shouldReturn` []

  it "copies a code block's code" $ do
    copied <- newIORef Nothing
    base <- newContext
    let ctx = withClipboard base (pure Nothing) (\t -> True <$ writeIORef copied (Just t))
        ui = view (parseMarkdown "```\nlet x = 1\n  in x\n```")
    _ <- warmup2 ctx (withInput 600 400) ui
    Just r <- spanRect "Copy" <$> collectTextSpans ctx
    void (runClick ctx (withInput 600 400) ui (spanCenter r))
    readIORef copied `shouldReturn` Just "let x = 1\n  in x"

  it "takes a code block's language up to the first space or tab" $ do
    ctx <- drawn 600 400 (view (parseMarkdown "```hs\tlinenos\ncode\n```"))
    isJust . spanRectOf "hs" <$> collectTextSpans ctx `shouldReturn` True

  it "wraps a code line wider than the document and shows all of it" $ do
    let long = "a_long_identifier_much_wider_than_the_column_it_is_drawn_in"
    -- No ancestor has a fixed width: the column fills the window.
    ctx <- drawn 400 400 (columnWith (fillW . padAll 10) (markdown (parseMarkdown ("```\nfirst\n" <> long <> "\nlast\n```"))))
    spans <- collectTextSpans ctx
    let code = [(r, t, clip) | (r, t, _, _, clip) <- spans, t /= "Copy"]
        texts = [t | (_, t, _) <- code]
        middle = takeWhile (/= "last") (drop 1 (dropWhile (/= "first") texts))
    mapM_ (\t -> texts `shouldContain` [t]) ["first", "last"]
    T.concat middle `shouldBe` long
    length middle `shouldSatisfy` (> 1)
    -- Every line is inside the window and inside what the block shows.
    mapM_ (\(r@(Rect x _ w _), t, clip) -> (t, x + w <= 390, covers clip r) `shouldBe` (t, True, True)) code

  it "keeps a wrapped code line's indent and the spaces inside it" $ do
    let line = "    x  =  alpha  beta  gamma  delta  epsilon  zeta  eta  theta"
    ctx <- drawn 300 400 (columnWith (fillW . padAll 10) (markdown (parseMarkdown ("```\n" <> line <> "\n```"))))
    texts <- map (\(_, t, _, _, _) -> t) <$> collectTextSpans ctx
    let code = filter (/= "Copy") texts
    length code `shouldSatisfy` (> 1)
    take 1 code `shouldSatisfy` all ("    x  =  " `T.isPrefixOf`)
    -- Each line is a piece of the source line, spaces and all.
    mapM_ (\t -> (t, t `T.isInfixOf` line) `shouldBe` (t, True)) code

  it "lets the wheel over a code block scroll the page" $ do
    ctx <- newContext
    let inp = withInput 400 300
        paras n = T.concat ["para " <> T.pack (show i) <> "\n\n" | i <- [1 .. n :: Int]]
        doc = parseMarkdown (paras 3 <> "```\ncode\n```\n\n" <> paras 30)
        ui = scrollArea (fillW . fixedH 280) (columnWith (fillW . padAll 10) (markdown doc))
    (sid, _) <- warmup2 ctx inp ui
    Just r <- spanRect "code" <$> collectTextSpans ctx
    let over = inp {inputMousePos = spanCenter r}
    void (runFrame ctx over {inputScroll = V2 0 3} ui)
    -- Let the glide settle.
    replicateM_ 30 (runFrame ctx over {inputDeltaTime = 0.05} ui)
    getScrollOffset ctx sid `shouldNotReturn` 0

  it "draws a resolved image, which returns its source when clicked" $ do
    let ui = columnWith (fixedW 500) (markdownConfigured catImages (parseMarkdown "![a cat](cat.png)\n\n![a dog](dog.png)"))
    ctx <- drawn 600 400 ui
    images <- map snd <$> nodesOf NodeImage ctx
    case images of
      [r@(Rect _ _ w h)] -> do
        (w, h) `shouldBe` (40, 30)
        runClick ctx (withInput 600 400) ui (spanCenter r) `shouldReturn` Just "cat.png"
      _ -> expectationFailure ("expected one image, got " <> show images)
    -- The image without one shows its alt text.
    ws <- map wText <$> drawnWords ctx
    ws `shouldContain` ["dog"]
    ws `shouldNotContain` ["cat"]

  it "sends a click on an image in a link to the link, and shows the link's title over it" $ do
    let ui = columnWith (fixedW 500) (markdownConfigured catImages (parseMarkdown "[![a cat](cat.png)](https://cats.example \"All about cats\")"))
        inp = withInput 600 400
    ctx <- drawn 600 400 ui
    [r] <- map snd <$> nodesOf NodeImage ctx
    let over = inp {inputMousePos = spanCenter r}
    warmup ctx over ui
    hasText "All about cats" <$> collectOverlayTextSpans ctx over `shouldReturn` False
    -- Rest on it past the tooltip's delay.
    threadDelay 600000
    warmup ctx over ui
    hasText "All about cats" <$> collectOverlayTextSpans ctx over `shouldReturn` True
    runClick ctx inp ui (spanCenter r) `shouldReturn` Just "https://cats.example"

  it "gives a paragraph that turns into a drawn image fresh ids" $ do
    let ui = columnWith (fixedW 500) . markdownConfigured catImages
        unfinished = parseMarkdown "![a cat](cat.png"
    ctx <- drawn 600 400 (ui unfinished)
    textIds <- nub . map wWidget <$> drawnWords ctx
    _ <- warmup2 ctx (withInput 600 400) (ui (appendMarkdown ")" unfinished))
    imageIds <- map fst <$> nodesOf NodeImage ctx
    (length textIds, length imageIds) `shouldBe` (1, 1)
    imageIds `shouldNotBe` textIds

  it "shows an image it does not draw as its alt text, a link to its source" $ do
    let ui = view (parseMarkdown "See ![the diagram](diagram.png) here.")
    ctx <- drawn 600 400 ui
    theme <- readIORef (ctxTheme ctx)
    fmap wColor . wordNamed "diagram" <$> drawnWords ctx `shouldReturn` Just (themeLink theme)
    clickWord ui "diagram" `shouldReturn` Just "diagram.png"

  it "sizes an ordered list's markers by its widest number" $ do
    -- A font whose "1" is narrow, so "10." is wider than "11.", the last.
    base <- newContext
    let fm = (monospaceMetrics 16) {fmAdvance = \c -> case c of '1' -> 2; 'x' -> 7; _ -> 8}
        ctx = withFontMetrics base fm
    _ <- warmup2 ctx (withInput 600 400) (view (parseMarkdown "10. ten\n11. eleven\n"))
    fmap (\(Rect _ _ w _) -> w) . spanRectOf "10." <$> collectTextSpans ctx `shouldReturn` Just 18

  it "lines a table cell's wrapped lines up as its column is aligned" $ do
    let cellText = "words that wrap over a few lines of a narrow column"
        row = "| " <> T.intercalate " | " (replicate 3 cellText) <> " |"
    ctx <- drawn 400 600 (columnWith (fixedW 380 . padAll 10) (markdown (parseMarkdown ("| l | r | c |\n|---|--:|:-:|\n" <> row))))
    ws <- drawnWords ctx
    -- The monospace test font's characters are all 12 wide.
    let extent w = let V2 x _ = wPos w in (x, x + 12 * fromIntegral (T.length (wText w)))
        cells = sortOn (map (fst . extent) . take 1) (filter (any ((== "narrow") . wText)) (groupBy ((==) `on` wWidget) ws))
        lineExtents cell = [(minimum (map (fst . extent) l), maximum (map (snd . extent) l)) | l <- groupBy ((==) `on` (\w -> let V2 _ y = wPos w in y)) cell]
    case map lineExtents cells of
      [left, right, centre] -> do
        map length [left, right, centre] `shouldSatisfy` all (> 1)
        length (nub (map fst left)) `shouldBe` 1
        length (nub (map snd right)) `shouldBe` 1
        length (nub [round (x0 + x1) :: Int | (x0, x1) <- centre]) `shouldBe` 1
      other -> expectationFailure ("expected three body cells, got " <> show (length other))

  it "draws a quote's list markers in its muted text colour" $ do
    ctx <- drawn 600 400 (view (parseMarkdown "> - bullet\n>\n> text\n>\n> - [x] done\n"))
    theme <- readIORef (ctxTheme ctx)
    ms <- markers ctx
    case map (\(_, _, ops) -> ops) ms of
      [[FillCircle _ _ _ bullet], FillRoundedRect _ _ boxFill : _] -> (bullet, boxFill) `shouldBe` (themeMuted theme, themeMuted theme)
      _ -> expectationFailure ("expected a bullet and a checked box, got " <> show (length ms) <> " markers")

  it "draws a task item's box as nano-ui draws a checkbox, at the bullet's id and under another version" $ do
    ctx <- drawn 600 400 (view (parseMarkdown "- a\n"))
    [(bulletId, bulletVersion, _)] <- markers ctx
    _ <- warmup2 ctx (withInput 600 400) (view (parseMarkdown "- [ ] a\n"))
    [(boxId, boxVersion, ops)] <- markers ctx
    (boxId, boxVersion /= bulletVersion) `shouldBe` (bulletId, True)
    theme <- readIORef (ctxTheme ctx)
    let side = checkboxBoxSize (ctxFontMetrics ctx)
    case ops of
      [_, StrokeRoundedRect r@(Rect _ _ w h) _ _ _] -> do
        (w, h) `shouldBe` (side, side)
        cdc <- mkCustomDrawContext ctx (ctxFontMetrics ctx) boxId
        ops == toList (runCanvasFor cdc (drawCheckbox theme r False)) `shouldBe` True
      _ -> expectationFailure ("expected an unchecked box's two ops, got " <> show (length ops))

  it "draws the blocks mdBlock draws, at every depth, and the rest as it would" $ do
    ctx <-
      drawn 600 600 . columnWith (fixedW 500) . markdownConfigured customCode . parseMarkdown $
        "```hs\ntop\n```\n\n> ```hs\n> quoted\n> ```\n\n- item\n\n  ```hs\n  listed\n  ```\n\n```py\nother\n```\n\nafter"
    spans <- collectTextSpans ctx
    map (`hasText` spans) ["custom top", "custom quoted", "custom listed", "other", "py"] `shouldBe` replicate 5 True
    -- Only the Python block is the widget's own, with its copy button.
    length [() | (_, "Copy", _, _, _) <- spans] `shouldBe` 1
    map wText <$> drawnWords ctx `shouldReturn` ["item", "after"]

  it "wraps the widget's own drawing in chrome of its own, where the block is, and returns its link" $ do
    let cfg :: MarkdownConfig NanoUIEs
        cfg =
          defaultMarkdownConfig
            { mdBlock = \own b -> case b of
                Paragraph _ -> panel (own b)
                _ -> own b
            }
        ui = columnWith (fixedW 500) (markdownConfigured cfg (parseMarkdown "See [the docs](/docs).\n\n> Quoted [link](/quoted)."))
    clickWord ui "docs" `shouldReturn` Just "/docs"
    clickWord ui "link" `shouldReturn` Just "/quoted"
    -- The quoted paragraph is drawn as in the quote: muted.
    ctx <- drawn 600 400 ui
    theme <- readIORef (ctxTheme ctx)
    fmap wColor . wordNamed "Quoted" <$> drawnWords ctx `shouldReturn` Just (themeMuted theme)

  it "styles inline code, quotes, table cells and code blocks over their own look" $ do
    let tint = colorRGBA 1 2 3 255
        codeInk = colorRGBA 40 50 60 255
        quoteInk = colorRGBA 70 80 90 255
        cellInk = colorRGBA 100 110 120 255
        cfg =
          (defaultMarkdownConfig :: MarkdownConfig NanoUIEs)
            { mdInlineCode = fontColor codeInk
            , mdInlineCodeBackground = Just tint
            , mdQuote = fontColor quoteInk
            , mdTableCell = \header -> if header then id else foreground cellInk
            }
        ui c = columnWith (fixedW 500) (markdownConfigured c (parseMarkdown "Run `build` now.\n\n> quoted\n\n| head |\n|---|\n| cell |"))
    ctx <- drawn 600 400 (ui cfg)
    ws <- drawnWords ctx
    theme <- readIORef (ctxTheme ctx)
    map (fmap wColor . (`wordNamed` ws)) ["build", "quoted", "cell", "head", "Run"]
      `shouldBe` map Just [codeInk, quoteInk, cellInk, styleFgOf theme, styleFgOf theme]
    -- The background is under the code alone.
    Just code <- pure (wordNamed "build" ws)
    [Rect x _ _ _] <- drawnFills tint ctx
    x `shouldBe` let V2 cx _ = wPos code in cx
    (drawnFills tint =<< drawn 600 400 (ui defaultMarkdownConfig)) `shouldReturn` []
    let block c = columnWith (fixedW 500) (markdownConfigured (defaultMarkdownConfig :: MarkdownConfig NanoUIEs) {mdCodeBlock = background c} (parseMarkdown "```\ncode\n```"))
    paints tint (block tint) `shouldReturn` True
    paints tint (block (colorRGBA 9 9 9 255)) `shouldReturn` False
  where
    styleFgOf theme = styleFg (themePanel theme)
