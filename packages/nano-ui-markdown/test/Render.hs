-- | Drawing documents in a headless context.
module Render (spec) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM, forM_, replicateM_, void, when)
import Data.Foldable (toList)
import Data.Function (on)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (find, groupBy, nub, sortOn)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import NanoUI
  ( Color, DrawOp (..), ImageId (..), NanoUI, Rect (..), Size (..), Style (..), TextFont (..), Theme (..)
  , V2 (..), WidgetId, background, checkboxBoxSize, colorRGBA, columnWith, drawCheckbox, fillW, fixedH, fixedW, fontColor, foreground
  , label, padAll, panel, rectIntersect, runCanvasFor, scrollArea, liftIO, withKey
  )
import NanoUI.Backend (FontBackend (..), FontMetrics (..), monospaceMetrics)
import NanoUI.Internal.Context (Context (..), CustomDrawingEntry (..), getTheme, lookupCustomDrawing)
import NanoUI.Internal.Layout.Arena (NodeType (..))
import NanoUI.Internal.Widgets.Custom (mkCustomDrawContext)
import NanoUI.Markdown
import NanoUI.Testing
  ( Damage (..), collectTextSpans, newContext, runFrame, takeDamage, withClipboard
  , lineWidth, withFontMetrics
  )
import NanoUI.Testing.Assert (withInput)
import NanoUI.Testing.Harness
  (drawnVertices, hasText, nodesOfType, runClick, spanCenter, spanRect, spanRectOf, vertexColor, warmup2)
import NanoUI.Testing.Sim hiding (spans)
import NanoUI.Shortcut (cmdOrCtrl, key)
import Test.Hspec

-- | A drawn rich-text word: widget, line box origin, font size, text and
-- colour.
data Word' = Word' {wWidget :: WidgetId, wPos :: V2, wSize :: Float, wText :: Text, wColor :: Color}
  deriving (Show)

-- | The frame's custom drawings (rich text and canvases) as id, content key
-- and ops, in document order.
drawings :: Context -> IO [(WidgetId, Int, [DrawOp])]
drawings ctx = do
  nodes <- nodesOfType NodeDrawing ctx
  fmap concat . forM nodes $ \(wid, r) -> do
    entry <- lookupCustomDrawing ctx wid
    cdc <- mkCustomDrawContext ctx (ctxFontMetrics ctx) wid
    pure [(wid, cdrContent e, toList (cdrBuild e cdc r)) | Just e <- [entry]]

-- | Every word the frame's rich-text widgets draw, in document order.
drawnWords :: Context -> IO [Word']
drawnWords ctx = do
  ds <- drawings ctx
  pure [Word' wid (V2 tx ty) (textFontSize font) t c | (wid, _, ops) <- ds, DrawTextStyled tx ty font t c <- ops]

-- | The frame's list markers (drawings with no text), in document order.
markers :: Context -> IO [(WidgetId, Int, [DrawOp])]
markers ctx = filter (\(_, _, ops) -> not (any isText ops)) <$> drawings ctx
  where
    isText = \case DrawTextStyled {} -> True; DrawText {} -> True; _ -> False

wordNamed :: Text -> [Word'] -> Maybe Word'
wordNamed t = find ((== t) . wText)

-- | The texts of the frame's drawn words.
wordTexts :: Context -> IO [Text]
wordTexts ctx = map wText <$> drawnWords ctx

-- | A point @dx@ in from the left of the drawn word, half a line down.
inWord :: Context -> Float -> Text -> IO V2
inWord ctx dx w = do
  Just (Word' _ (V2 x y) _ _ _) <- wordNamed w <$> drawnWords ctx
  pure (V2 (x + dx) (y + fmLineHeight (ctxFontMetrics ctx) / 2))

-- | Rects that rich-text widgets fill in a colour: backgrounds and
-- decorations.
drawnFills :: Color -> Context -> IO [Rect]
drawnFills c ctx = do
  ds <- drawings ctx
  pure [rect | (_, _, ops) <- ds, FillRect rect c' <- ops, c' == c]

-- | Whether a fully painted frame draws anything in the colour.
paints :: Color -> NanoUI a -> IO Bool
paints c ui = do
  ctx <- drawn 600 400 ui
  writeIORef (ctxPaintFull ctx) True
  (_, dd, _) <- runFrame ctx (withInput 600 400) ui
  elem c . map vertexColor <$> drawnVertices dd

-- | A fresh context that has drawn the view twice in a @w@ by @h@ window.
drawn :: Float -> Float -> NanoUI a -> IO Context
drawn w h ui = newContext >>= \ctx -> ctx <$ warmup2 ctx (withInput w h) ui

-- | Run a script against the view in a @w@ by @h@ window of the context,
-- stopping at the first failed check.
sim :: Context -> Float -> Float -> NanoUI a -> Sim a r -> IO r
sim ctx w h ui script = newIORef 0 >>= \failed -> runSimWith opts ctx failed (withInput w h) ui script
  where
    opts = defaultSimOptions {simFailFast = True}

-- | A fresh context with a clipboard the returned ref holds.
copying :: IO (Context, IORef (Maybe Text))
copying = do
  copied <- newIORef Nothing
  base <- newContext
  pure (withClipboard base (readIORef copied) (\t -> True <$ writeIORef copied (Just t)), copied)

-- | Ctrl (Cmd on macOS) with each key in turn.
chords :: [Char] -> Sim a ()
chords = mapM_ (chord . (cmdOrCtrl <>) . key)

-- | 'inWord' in the script's frame.
wordAt :: Float -> Text -> Sim a Target
wordAt dx w = At <$> onContext (\ctx -> inWord ctx dx w)

-- | Draw a document, append text and draw again. For each pair, the first
-- word's id before must equal the second word's id after. The @new@ words
-- must be drawn.
idsAcross :: Text -> Text -> [(Text, Text)] -> [Text] -> Expectation
idsAcross start more same new = do
  cache <- newMarkdownCache
  let doc = parseMarkdown start
  ctx <- drawn 600 600 (view cache doc)
  was <- drawnWords ctx
  _ <- warmup2 ctx (withInput 600 600) (view cache (appendMarkdown more doc))
  now <- drawnWords ctx
  let idOf w ws = wWidget <$> wordNamed w ws
  mapM_ (\(w, w') -> (w', isJust (idOf w was), idOf w' now) `shouldBe` (w', True, idOf w was)) same
  mapM_ (\w -> map wText now `shouldContain` [w]) new

view :: MarkdownCache -> MarkdownDoc -> NanoUI (Maybe Text)
view cache doc = columnWith (fixedW 500 . padAll 10) (markdown cache doc)

-- | Click a drawn word in a 600 by 400 window and return the reported link.
clickWord :: NanoUI (Maybe Text) -> Text -> IO (Maybe Text)
clickWord ui w = newContext >>= \ctx -> sim ctx 600 400 ui $ do
  p <- wordAt 3 w
  _ <- moveTo p
  click p

-- | Resolves only cat.png, at 40 by 30.
catImages :: MarkdownConfig
catImages = defaultMarkdownConfig {mdImage = \src -> if src == "cat.png" then Just (ImageId 7, Size 40 30) else Nothing}

-- | Draws Haskell code blocks as a plain label, and every other block as the
-- default widget does.
customCode :: MarkdownConfig
customCode =
  defaultMarkdownConfig
    { mdBlock = \own -> \case
        CodeBlock "hs" code -> Nothing <$ label ("custom " <> code)
        b -> own b
    }

spec :: Spec
spec = before newMarkdownCache $ do
  it "holds a few passes' blocks however many keys churn, and every block a frame draws" $ \cache -> do
    ctx <- newContext
    let
      doc = parseMarkdown (T.intercalate "\n\n" ["paragraph " <> T.pack (show i) | i <- [1 .. 50 :: Int]])
      frameOf ui = void (runFrame ctx (withInput 600 400) ui)
    -- Fresh keys every frame: 6000 blocks over the run, 50 of them live.
    forM_ [1 .. 120 :: Int] $ \n -> frameOf (void (withKey n (markdown cache doc)))
    markdownCacheSize cache >>= (`shouldSatisfy` (\n -> n >= 50 && n <= 2048))
    -- Sixty documents sharing the blocks.
    replicateM_ 4 (frameOf (forM_ [1 .. 60 :: Int] $ \i -> withKey i (markdown cache doc)))
    markdownCacheSize cache >>= (`shouldSatisfy` (>= 3000))

  it "scales headings and small text from the backend's default font size" $ \cache -> do
    -- Text height equals font size in this resolver; the body size is 20.
    ctx <- (`withFontMetrics` monospaceMetrics 20) <$> newContext
    _ <- warmup2 ctx (withInput 600 800) . view cache . parseMarkdown $
      "# h1\n\n## h2\n\n### h3\n\n#### h4\n\n##### h5\n\n###### h6\n\nbody\n\n```\ncode\n```"
    ws <- drawnWords ctx
    let size w = maybe 0 wSize (wordNamed w ws)
    map (round . (* 100) . size) ["body", "h1", "h2", "h3", "h4", "h5", "h6"]
      `shouldBe` [2000, 3200, 2700, 2400, 2100, 2000, 1800 :: Int]
    -- The copy button's text is 0.8 times the body size.
    fmap (\(Rect _ _ _ h) -> h) . spanRect "Copy" <$> collectTextSpans ctx `shouldReturn` Just 16

  it "selects and copies styled Markdown text" $ \cache -> do
    (ctx, copied) <- copying
    sim ctx 600 400 (view cache (parseMarkdown "Select **these** words")) $ do
      start <- wordAt 1 "Select"
      finish <- wordAt (lineWidth (ctxFontMetrics ctx) "these" - 1) "these"
      _ <- drag start finish 1
      selectionColor <- themeSelection <$> liftIO (getTheme ctx)
      check . not . null =<< liftIO (drawnFills selectionColor ctx)
      chords ['c']
      checkEq (Just "Select these") =<< liftIO (readIORef copied)
      chords ['a', 'c']
      checkEq (Just "Select these words") =<< liftIO (readIORef copied)

  it "copies code blocks, whitespace kept, by selection and by the copy button" $ \cache -> do
    (ctx, copied) <- copying
    sim ctx 600 400 (view cache (parseMarkdown "```hs\tlinenos\n  let x = 1\n```")) $ do
      -- The language runs up to the first space or tab.
      check =<< labelShown "hs"
      _ <- click =<< wordAt 1 "  let x = 1"
      chords ['a', 'c']
      checkEq (Just "  let x = 1") =<< liftIO (readIORef copied)
    sim ctx 600 400 (view cache (parseMarkdown "```\nlet x = 1\n  in x\n```")) $ do
      _ <- click "Copy"
      checkEq (Just "let x = 1\n  in x") =<< liftIO (readIORef copied)

  it "keeps earlier blocks' ids while text streams in" $ \_ -> do
    -- The growing block keeps its id too.
    idsAcross "para one\n\n- item two\n\nstrea" "ming on\n\n# new block" [("one", "one"), ("two", "two"), ("strea", "streaming")] ["new"]
    -- A list's second item grows and gains a nested list and a sibling. A
    -- quote's second paragraph grows and a third follows.
    idsAcross "- first\n- sec" "ond\n  - nested\n- third" [("first", "first"), ("sec", "second")] ["nested", "third"]
    idsAcross "> quoted\n>\n> gro" "wing\n>\n> after" [("quoted", "quoted"), ("gro", "growing")] ["after"]

  it "measures and repaints only the block that grew when text is appended" $ \cache -> do
    base <- newContext
    measured <- newIORef []
    inView <- newIORef False
    -- Record texts measured while the view runs. Rich text lays out its words
    -- then; painting comes later.
    let fm = ctxFontMetrics base
        prepare t = readIORef inView >>= \recording -> fm <$ when recording (modifyIORef' measured (t :))
        ctx = withFontMetrics base fm {fmBackend = Just (FontBackend prepare (const (pure Nothing)))}
        inp = withInput 600 600
        doc1 = parseMarkdown "first alpha\n\nsecond beta\n\nthird gam"
        doc2 = appendMarkdown "ma delta" doc1
        measuring doc = do
          writeIORef measured []
          void (runFrame ctx inp (liftIO (writeIORef inView True) *> view cache doc <* liftIO (writeIORef inView False)))
          readIORef measured
    measuring doc1 >>= (`shouldContain` ["first"])
    _ <- measuring doc1
    writeIORef (ctxPaintFull ctx) False
    words' <- measuring doc2
    words' `shouldContain` ["delta"]
    mapM_ (\w -> words' `shouldNotContain` [w]) ["first", "alpha", "second", "beta"]
    -- The damage covers only the line that grew.
    damage <- takeDamage ctx
    ws <- drawnWords ctx
    let hit w = [isJust (rectIntersect r (Rect 0 y 600 (fmLineHeight fm))) | DamageClip r <- [damage], Word' _ (V2 _ y) _ _ _ <- toList (wordNamed w ws)]
    map hit ["alpha", "beta", "gamma"] `shouldBe` [[False], [False], [True]]
    -- Redrawing the same document measures nothing.
    measuring doc2 `shouldReturn` []

  it "wraps a code line wider than the document, keeping all of it, its indent and its spaces" $ \cache -> do
    let
      long = "a_long_identifier_much_wider_than_the_column_it_is_drawn_in"
      indented = "    x  =  alpha  beta  gamma  delta  epsilon  zeta  eta  theta"
      -- No fixed-width ancestor: the column fills the window.
      codeIn w src = drawn w 400 . columnWith (fillW . padAll 10) . markdown cache $
        parseMarkdown ("```\n" <> src <> "\n```")
    code <- drawnWords =<< codeIn 400 ("first\n" <> long <> "\nlast")
    let
      texts = map wText code
      middle = takeWhile (/= "last") (drop 1 (dropWhile (/= "first") texts))
    mapM_ (\t -> texts `shouldContain` [t]) ["first", "last"]
    T.concat middle `shouldBe` long
    length middle `shouldSatisfy` (> 1)
    -- Every wrapped line starts within the code block's available width.
    mapM_ (\w -> let V2 x _ = wPos w in x < 390 `shouldBe` True) code
    wrapped <- wordTexts =<< codeIn 300 indented
    length wrapped `shouldSatisfy` (> 1)
    take 1 wrapped `shouldSatisfy` all ("    x  =  " `T.isPrefixOf`)
    -- Each line is a substring of the source line, spaces included.
    mapM_ (\t -> (t, t `T.isInfixOf` indented) `shouldBe` (t, True)) wrapped

  it "lets the wheel over a code block scroll the page" $ \cache -> do
    ctx <- newContext
    let paras n = T.concat ["para " <> T.pack (show i) <> "\n\n" | i <- [1 .. n :: Int]]
        doc = parseMarkdown (paras 3 <> "```\ncode\n```\n\n" <> paras 30)
        ui = scrollArea (fillW . fixedH 280) (columnWith (fillW . padAll 10) (markdown cache doc))
    sim ctx 400 300 ui $ do
      (sid, _) <- result
      _ <- (`wheel` V2 0 3) =<< wordAt 0 "code"
      -- Let the glide settle.
      replicateM_ 30 (advance 0.05)
      check . (/= 0) =<< scrollOffsetOf sid

  it "draws a resolved image, which returns its source when clicked, and the rest as alt text" $ \cache -> do
    let ui = columnWith (fixedW 500) (markdownConfigured cache catImages (parseMarkdown "![a cat](cat.png)\n\n![a dog](dog.png)"))
    ctx <- drawn 600 400 ui
    images <- map snd <$> nodesOfType NodeImage ctx
    case images of
      [r@(Rect _ _ w h)] -> do
        (w, h) `shouldBe` (40, 30)
        runClick ctx (withInput 600 400) ui (spanCenter r) `shouldReturn` Just "cat.png"
      _ -> expectationFailure ("expected one image, got " <> show images)
    ws <- wordTexts ctx
    ws `shouldContain` ["dog"]
    ws `shouldNotContain` ["cat"]
    -- An image it does not draw is a link to its source.
    let alt = view cache (parseMarkdown "See ![the diagram](diagram.png) here.")
    actx <- drawn 600 400 alt
    theme <- readIORef (ctxTheme actx)
    fmap wColor . wordNamed "diagram" <$> drawnWords actx `shouldReturn` Just (themeLink theme)
    clickWord alt "diagram" `shouldReturn` Just "diagram.png"

  it "sends a click on an image in a link to the link, and shows the link's title over it" $ \cache -> do
    let ui = columnWith (fixedW 500) (markdownConfigured cache catImages (parseMarkdown "[![a cat](cat.png)](https://cats.example \"All about cats\")"))
    ctx <- newContext
    sim ctx 600 400 ui $ do
      [r] <- map snd <$> onContext (nodesOfType NodeImage)
      _ <- moveTo (Within r)
      expectNot "All about cats"
      -- Hover past the tooltip delay.
      liftIO (threadDelay 600000)
      _ <- frame
      expect "All about cats"
      checkEq (Just "https://cats.example") =<< click (Within r)

  it "gives a paragraph that turns into a drawn image fresh ids" $ \cache -> do
    let ui = columnWith (fixedW 500) . markdownConfigured cache catImages
        unfinished = parseMarkdown "![a cat](cat.png"
    ctx <- drawn 600 400 (ui unfinished)
    textIds <- nub . map wWidget <$> drawnWords ctx
    _ <- warmup2 ctx (withInput 600 400) (ui (appendMarkdown ")" unfinished))
    imageIds <- map fst <$> nodesOfType NodeImage ctx
    (length textIds, length imageIds) `shouldBe` (1, 1)
    imageIds `shouldNotBe` textIds

  it "sizes an ordered list's markers by its widest number" $ \cache -> do
    -- "1" is narrow, so "10." is wider than the last marker, "11.".
    base <- newContext
    let fm = (monospaceMetrics 16) {fmAdvance = \c -> case c of '1' -> 2; 'x' -> 7; _ -> 8}
        ctx = withFontMetrics base fm
    _ <- warmup2 ctx (withInput 600 400) (view cache (parseMarkdown "10. ten\n11. eleven\n"))
    fmap (\(Rect _ _ w _) -> w) . spanRectOf "10." <$> collectTextSpans ctx `shouldReturn` Just 18

  it "keeps wrapped table text inside its rows on the first frame and after resizing" $ \cache -> do
    let doc = parseMarkdown
          "| Category | Keys |\n|---|---|\n\
          \| Motions | `h j k l`, `w b e`, with counts and words that wrap across several lines |\n\
          \| Operators | `d c y` with a motion, a text object, or doubled |\n\nAfter"
        ui = columnWith fillW (markdown cache doc)
        bottom (Rect _ y _ h) = y + h
        rectTop (Rect _ y _ _) = y
    ctx <- newContext
    forM_ [400, 800, 280, 400, 400] $ \width -> do
      _ <- runFrame ctx (withInput width 1200) ui
      panels <- map snd <$> nodesOfType NodePanel ctx
      ws <- drawnWords ctx
      case panels of
        [table, h1, h2, m1, m2, o1, o2] -> do
          let cells = [h1, h2, m1, m2, o1, o2]
          forM_ ws $ \word -> do
            let V2 x y = wPos word
            if wText word == "After"
              then y `shouldSatisfy` (>= bottom table)
              else cells `shouldSatisfy` any (\(Rect cx cy cw ch) -> x >= cx && x < cx + cw && y >= cy && y + wSize word <= cy + ch)
          bottom h1 `shouldSatisfy` (<= rectTop m1)
          bottom m2 `shouldSatisfy` (<= rectTop o2)
          bottom table `shouldSatisfy` (\b -> abs (b - bottom o1 - 1) < 0.1)
          (rectTop m1, bottom m1, bottom o1) `shouldBe` (rectTop m2, bottom m2, bottom o2)
        _ -> expectationFailure ("expected a table and six cells, got " <> show panels)

  it "lines a table cell's wrapped lines up as its column is aligned" $ \cache -> do
    let cellText = "words that wrap over a few lines of a narrow column"
        row = "| " <> T.intercalate " | " (replicate 3 cellText) <> " |"
    ctx <- drawn 400 600 (columnWith (fixedW 380 . padAll 10) (markdown cache (parseMarkdown ("| l | r | c |\n|---|--:|:-:|\n" <> row))))
    ws <- drawnWords ctx
    -- The monospace test font is 12 wide per character.
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

  it "draws a quote's list markers in its muted text colour" $ \cache -> do
    ctx <- drawn 600 400 (view cache (parseMarkdown "> - bullet\n>\n> text\n>\n> - [x] done\n"))
    theme <- readIORef (ctxTheme ctx)
    ms <- markers ctx
    case map (\(_, _, ops) -> ops) ms of
      [[FillCircle _ _ _ bullet], FillRoundedRect _ _ boxFill : _] -> (bullet, boxFill) `shouldBe` (themeMuted theme, themeMuted theme)
      _ -> expectationFailure ("expected a bullet and a checked box, got " <> show (length ms) <> " markers")

  it "draws a task item's box as nano-ui draws a checkbox, at the bullet's id and under another version" $ \cache -> do
    ctx <- drawn 600 400 (view cache (parseMarkdown "- a\n"))
    [(bulletId, bulletVersion, _)] <- markers ctx
    _ <- warmup2 ctx (withInput 600 400) (view cache (parseMarkdown "- [ ] a\n"))
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

  it "draws the blocks mdBlock draws, at every depth, and the rest as it would" $ \cache -> do
    ctx <-
      drawn 600 600 . columnWith (fixedW 500) . markdownConfigured cache customCode . parseMarkdown $
        "```hs\ntop\n```\n\n> ```hs\n> quoted\n> ```\n\n- item\n\n  ```hs\n  listed\n  ```\n\n```py\nother\n```\n\nafter"
    spans <- collectTextSpans ctx
    map (`hasText` spans) ["custom top", "custom quoted", "custom listed"] `shouldBe` replicate 3 True
    -- The language label exactly: the block's "Copy" button contains "py".
    isJust (spanRectOf "py" spans) `shouldBe` True
    wordTexts ctx `shouldReturn` ["item", "other", "after"]
    -- Only the Python block uses the default widget, with its copy button.
    length [() | (_, "Copy", _, _, _) <- spans] `shouldBe` 1

  it "wraps the widget's own drawing in chrome of its own, where the block is, and returns its link" $ \cache -> do
    let cfg :: MarkdownConfig
        cfg =
          defaultMarkdownConfig
            { mdBlock = \own b -> case b of
                Paragraph _ -> panel (own b)
                _ -> own b
            }
        ui = columnWith (fixedW 500) . markdownConfigured cache cfg . parseMarkdown $
          "See [the docs](/docs) or <https://auto.link>.\n\n> Quoted [link](/quoted)."
    clickWord ui "docs" `shouldReturn` Just "/docs"
    clickWord ui "https://auto.link" `shouldReturn` Just "https://auto.link"
    clickWord ui "link" `shouldReturn` Just "/quoted"
    clickWord ui "See" `shouldReturn` Nothing
    -- The quoted paragraph keeps the quote's muted colour.
    ctx <- drawn 600 400 ui
    theme <- readIORef (ctxTheme ctx)
    fmap wColor . wordNamed "Quoted" <$> drawnWords ctx `shouldReturn` Just (themeMuted theme)

  it "styles inline code, quotes, table cells and code blocks over their own look" $ \cache -> do
    let tint = colorRGBA 1 2 3 255
        codeInk = colorRGBA 40 50 60 255
        quoteInk = colorRGBA 70 80 90 255
        cellInk = colorRGBA 100 110 120 255
        cfg =
          (defaultMarkdownConfig :: MarkdownConfig)
            { mdInlineCode = fontColor codeInk
            , mdInlineCodeBackground = Just tint
            , mdQuote = fontColor quoteInk
            , mdTableCell = \header -> if header then id else foreground cellInk
            }
        ui c = columnWith (fixedW 500) (markdownConfigured cache c (parseMarkdown "Run `build` now.\n\n> quoted\n\n| head |\n|---|\n| cell |"))
    ctx <- drawn 600 400 (ui cfg)
    ws <- drawnWords ctx
    theme <- readIORef (ctxTheme ctx)
    map (fmap wColor . (`wordNamed` ws)) ["build", "quoted", "cell", "head", "Run"]
      `shouldBe` map Just [codeInk, quoteInk, cellInk, styleFgOf theme, styleFgOf theme]
    -- The background covers only the code.
    Just code <- pure (wordNamed "build" ws)
    [Rect x _ _ _] <- drawnFills tint ctx
    x `shouldBe` let V2 cx _ = wPos code in cx
    (drawnFills tint =<< drawn 600 400 (ui defaultMarkdownConfig)) `shouldReturn` []
    let block c = columnWith (fixedW 500) (markdownConfigured cache (defaultMarkdownConfig :: MarkdownConfig) {mdCodeBlock = background c} (parseMarkdown "```\ncode\n```"))
    paints tint (block tint) `shouldReturn` True
    paints tint (block (colorRGBA 9 9 9 255)) `shouldReturn` False
  where
    styleFgOf theme = styleFg (themePanel theme)
