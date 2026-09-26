-- | Appending text gives the same parse as parsing the whole text.
module Incremental (spec) where

import Data.List (inits, sort)
import Data.Text (Text)
import Data.Text qualified as T
import NanoUI.Markdown
import Test.Hspec
import Test.QuickCheck

-- | A document of parser-stressing lines, each with its own line ending:
-- container markers before block starts, lines that start a block only in
-- some contexts, inline text dense with delimiters, brackets and links, link
-- reference definitions, HTML, and blank or indented lines.
newtype Doc = Doc [(Text, Text)]

instance Show Doc where
  show = show . docText

docText :: Doc -> Text
docText (Doc ls) = T.concat [l <> end | (l, end) <- ls]

instance Arbitrary Doc where
  arbitrary = do
    ls <- upTo 16 genLine
    ends <- vectorOf (length ls - 1) genEnding
    end <- oneof [pure "", genEnding]
    pure (Doc (zip ls (ends ++ [end])))
  shrink (Doc ls) = map Doc (shrinkList (const []) ls)

genEnding :: Gen Text
genEnding = frequency [(6, pure "\n"), (2, pure "\r\n"), (2, pure "\r")]

genLine :: Gen Text
genLine = do
  prefixes <- upTo 2 (elements ["> ", ">", "- ", "* ", "1. ", "2) ", "  ", "   ", "    ", "\t", "- [ ] ", "- [x] "])
  indent <- elements ["", "", "", " ", "  ", "   ", "    "]
  body <-
    frequency
      [ (6, genInline)
      , (3, pure "")
      , (1, elements ["   ", "#", "# Title", "## Sub ##", "###### six", "=====", "===", "---", "***", "- - -", "___"])
      , (2, elements ["```", "```haskell", "~~~", "````", "``` x`y"])
      , (2, elements ["| a | b |", "|---|:-:|", "|:-:|", "a | b", "--|--", "| `x\\|y` | *z* |", "|:--|"])
      , (1, elements ["2. ", "10. ", "2. b", "10. b", "1. [x] a"])
      , (2, elements ["[ref]: /url \"t\"", "[Other]: <u v>", "[ref]:", "[ref]", "[x][ref]", "[other][]", "[OTHER]"])
      , (2, elements ["<div>", "</div>", "<a href=\"x\">", "<span>", "<x-y>", "<pre>", "<!-- c", "-->", "<!-- c -->", "a <!-- c --> b"])
      , (1, (<>) <$> elements ["", "    "] <*> genInline)
      ]
  pure (T.concat prefixes <> indent <> body)

-- | Between 0 and @k@ generated values.
upTo :: Int -> Gen a -> Gen [a]
upTo k g = chooseInt (0, k) >>= (`vectorOf` g)

genInline :: Gen Text
genInline = T.concat <$> (chooseInt (1, 8) >>= (`vectorOf` genToken))

genToken :: Gen Text
genToken =
  frequency
    [ (8, elements ["word", "text", "a", "b", "snake_case", "x*y", "!", "(", ")", "  ", " ", "'"])
    , (6, elements ["*", "**", "***", "_", "__", "~", "~~", "~~~"])
    , (3, elements ["`", "``", "`code`", "\\", "\\*", "\\`", "&amp;", "&#65;", "&nope;", "&"])
    , (3, elements ["[", "]", "![", "](", "(/u)", "[a](/u \"t\")", "[b]", "<", ">"])
    , (2, elements ["<https://a.b>", "<me@x.io>", "https://x.org/p.", "www.a.co", "http://h", " https://q.r/(s)"])
    , (1, elements ["  ", "\\"] >>= \end -> pure (end <> "\n"))
    ]

-- | A document and its cut positions; both shrink.
data Cut = Cut Doc [Int]

instance Show Cut where
  show (Cut d cuts) = show (chunksAt cuts (docText d))

instance Arbitrary Cut where
  arbitrary = do
    d <- arbitrary
    Cut d <$> upTo 12 (chooseInt (0, T.length (docText d)))
  shrink (Cut d cuts) = [Cut d cuts' | cuts' <- shrinkList shrink cuts] ++ [Cut d' cuts | d' <- shrink d]

-- | Split the text at the given positions (clamped to its length), keeping
-- empty pieces.
chunksAt :: [Int] -> Text -> [Text]
chunksAt cuts t = zipWith (\from to -> T.take (to - from) (T.drop from t)) bounds (drop 1 bounds)
  where
    bounds = 0 : sort (map (min (T.length t)) cuts) ++ [T.length t]

-- | The document after each append, starting with the empty one.
streams :: [Text] -> [MarkdownDoc]
streams = scanl (flip appendMarkdown) emptyMarkdown

appendAll :: [Text] -> MarkdownDoc
appendAll = last . streams

-- | Streaming one character at a time, every step's blocks match a full
-- parse of the text so far.
streamsLikeWhole :: Text -> Expectation
streamsLikeWhole t =
  map markdownBlocks (streams (T.chunksOf 1 t)) `shouldBe` map parseMarkdownBlocks (T.inits t)

spec :: Spec
spec = do
  describe "appendMarkdown" $ do
    it "parses to what the whole text does, in any number of pieces" $
      withNumTests 3000 $ property $ \(Cut d cuts) ->
        markdownBlocks (appendAll (chunksAt cuts (docText d))) === parseMarkdownBlocks (docText d)
    it "parses to what the whole text does, a character at a time" $
      withNumTests 300 $ property $ \d ->
        markdownBlocks (appendAll (T.chunksOf 1 (docText d))) === parseMarkdownBlocks (docText d)
    it "parses any text" $
      property $ \s ->
        let t = T.pack s
         in length (show (parseMarkdownBlocks t)) `seq` markdownBlocks (appendMarkdown t (parseMarkdown t)) === parseMarkdownBlocks (t <> t)

  describe "a line right under a paragraph" $ do
    -- Such a line may underline the paragraph, make it a table header, or
    -- fail to interrupt it where it would otherwise start a block, so the
    -- paragraph must be reparsed with it.
    let underParagraph name a b = it name $ do
          markdownBlocks (appendMarkdown b (parseMarkdown a)) `shouldBe` parseMarkdownBlocks (a <> b)
          streamsLikeWhole (a <> b)
    underParagraph "underlines it into a heading" "a\n---\n" "b"
    underParagraph "makes it a table's header" "a | b\n--|--\n" "c | d"
    underParagraph "continues it with an ordered item not numbered 1" "a\n2. b\n" "c"
    underParagraph "continues it with an ordered item numbered 10" "a\n10. b\n" "c"
    underParagraph "continues it with HTML of type 7" "a\n<x-y>\n" "b"
    underParagraph "continues it with a task item with nothing after its box" "a\n- [x] \n" "b"
    underParagraph "continues link reference definitions" "[r]: /u\n- [x] \n" "b"

  describe "a streamed message" $ do
    it "keeps its finished blocks" $ do
      let msg = "# Reply\n\nFirst paragraph with *emphasis*.\n\n- one\n- two\n\n```hs\nx = 1\n```\n\nLast [link](/u)."
          whole = parseMarkdownBlocks msg
          streamed = streams (T.chunksOf 3 msg)
      markdownBlocks (last streamed) `shouldBe` whole
      -- A closed block stays unchanged while the rest streams in.
      let settled = [length (takeWhile id (zipWith (==) (markdownBlocks p) whole)) | p <- streamed]
      settled `shouldBe` sort settled
    it "continues an open code block, list, table and block quote" $
      mapM_
        streamsLikeWhole
        [ "```hs\nx = 1\n\n  y = 2\n~~~\n````\n```\nafter\n"
        , "   ~~~\n    a\n  b\n~~~ ~\n   ~~~~"
        , "- a\n- b\n\n- c\n  more\n\n  > q\n- d\ne\n\npara"
        , "- a\n\n- b\n- c\n- d"
        , "1. a\n2. b\n3) c\n4) d"
        , "- [ ] a\n- [x] b\n- c\n- [x] d"
        , "| a | b |\n|---|:-:|\n| 1 | 2 |\n| 3 |\n| 4 | 5 | 6 |\nafter"
        , "> a\n>\n> b\n> - c\n> - d\n>\n> ```\n> e\n\n> f"
        , "<!-- a -->\n<!-- b -->\n<div>\nc\n</div>\n\nd"
        ]
    it "resolves links to definitions before and after them" $
      mapM_
        streamsLikeWhole
        [ "[a] and [b]\n\ntext\n\n[a]: /u\n[b]: /v 'title'\n"
        , "[a]: /1\n\n[a]\n\n[A]: /2\n\n[a]\n\n- [a]\n- [c]\n\n> [c]: /3\n\n[c]"
        ]

  describe "MarkdownDoc" $ do
    it "is equal to another with the same text" $ do
      appendMarkdown "b" (parseMarkdown "a") `shouldBe` parseMarkdown "ab"
      parseMarkdown "a" `shouldNotBe` parseMarkdown "a\n"
      parseMarkdown "a\r\n" `shouldNotBe` parseMarkdown "a\n"
      emptyMarkdown `shouldBe` parseMarkdown ""
    it "hands back its text, however it was built" $
      withNumTests 300 $ property $ \(Cut d cuts) ->
        markdownSource (appendAll (chunksAt cuts (docText d))) === docText d
    it "lists its images' sources once each, in order, at any depth" $
      markdownImages
        ( parseMarkdown
            "![a](one.png) and [![b](two.png)](/link)\n\n> - ![c](three.png) *![d](one.png)*\n\n\
            \| h | ![e](four.png) |\n|---|---|\n| ![f](five.png) | x |\n\n```\n![not](code.png)\n```"
        )
        `shouldBe` ["one.png", "two.png", "three.png", "four.png", "five.png"]
    it "shows as the parse of its text" $
      show (appendMarkdown "b" (parseMarkdown "*a*\n")) `shouldBe` "parseMarkdown \"*a*\\nb\""
    it "keeps every token in a store that skips equal documents" $ do
      -- The store mimics nano-ui's useState. A line ending changes no block
      -- yet, but the doc must still differ or the text after it is lost.
      let tokens = ["Hello", "\n", "\n", "- one", "\n", "- two", "\n", "\n", "```", "\n", "x", "\n", "```"]
          store old new = if new == old then old else new
          kept = scanl (\doc tok -> store doc (appendMarkdown tok doc)) emptyMarkdown tokens
      map markdownBlocks kept `shouldBe` map (parseMarkdownBlocks . T.concat) (inits tokens)
