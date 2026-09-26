-- | The blocks and spans the commonmark library's parse becomes. CommonMark
-- itself is the library's to get right.
module Parse (spec) where

import NanoUI.Markdown
import Test.Hspec

spec :: Spec
spec = do
  describe "blocks" $ do
    it "parses headings" $ do
      "## Title ##" `parsesTo` Heading 2 [Str "Title"]
      "Two\nlines\n---" `parsesTo` Heading 2 [Str "Two", SoftBreak, Str "lines"]
      "#" `parsesTo` Heading 1 []
    it "parses paragraphs and thematic breaks" $
      parseMarkdownBlocks "a\nb\n\n***\n\nc" `shouldBe` [Paragraph [Str "a", SoftBreak, Str "b"], ThematicBreak, para "c"]
    it "parses code with its info string, without the last line ending" $ do
      "```hs x\nmain = pure ()\n```" `parsesTo` CodeBlock "hs x" "main = pure ()"
      "    one\n\n    two\n\n" `parsesTo` CodeBlock "" "one\n\ntwo"
      "```\r\na\r\nb" `parsesTo` CodeBlock "" "a\nb"
    it "parses block quotes" $
      "> a\n> > b" `parsesTo` BlockQuote [para "a", BlockQuote [para "b"]]
    it "parses bullet and ordered lists, tight or loose" $ do
      "- a\n  - b\n- c"
        `parsesTo` List (Bullet '-') True [item [para "a", List (Bullet '-') True [item [para "b"]]], item [para "c"]]
      "3) c\n\n4) d" `parsesTo` List (Ordered 3 ')') False [item [para "c"], item [para "d"]]
    it "parses task lists, which an item without a box ends" $ do
      "- [ ] todo\n- [x] done"
        `parsesTo` List (Bullet '-') True [ListItem (Just False) [para "todo"], ListItem (Just True) [para "done"]]
      parseMarkdownBlocks "- [x] done\n- plain"
        `shouldBe` [List (Bullet '-') True [ListItem (Just True) [para "done"]], List (Bullet '-') True [item [para "plain"]]]
    it "gives only bullet items check boxes" $
      "1. [x] done" `parsesTo` List (Ordered 1 '.') True [item [para "[x] done"]]
    it "parses tables with their alignment, one cell a column" $
      "| a | b | c | d |\n|---|:--|:-:|--:|\n| 1 | *2* |\n| 1 | 2 | 3 | 4 | 5 |"
        `parsesTo` Table
          [CellDefault, CellLeft, CellCenter, CellRight]
          [[Str "a"], [Str "b"], [Str "c"], [Str "d"]]
          [[[Str "1"], [Emph [Str "2"]], [], []], [[Str "1"], [Str "2"], [Str "3"], [Str "4"]]]
    it "keeps raw HTML as text, without its comments" $ do
      "<div>\n*hi*\n</div>" `parsesTo` para "<div>\n*hi*\n</div>"
      "a <b>bold</b>" `inlineOf` [Str "a <b>bold</b>"]
      "<div>\n<!-- note -->\n</div>" `parsesTo` para "<div>\n\n</div>"
      "a <!-- note --> b" `inlineOf` [Str "a  b"]
      parseMarkdownBlocks "<!-- note -->\n<!--\nlonger\n-->\n\nshown" `shouldBe` [para "shown"]

  describe "inlines" $ do
    it "parses emphasis, strong emphasis and strikethrough" $
      "*a* **b** ***c*** ~~d~~"
        `inlineOf` [Emph [Str "a"], Str " ", Strong [Str "b"], Str " ", Emph [Strong [Str "c"]], Str " ", Strike [Str "d"]]
    it "parses code spans and line breaks" $
      "`a`  \nb\\\nc\nd" `inlineOf` [Code "a", HardBreak, Str "b", HardBreak, Str "c", SoftBreak, Str "d"]
    it "parses links and images" $
      "[a *b*](/u \"t\") ![alt](/i.png)"
        `inlineOf` [Link "/u" "t" [Str "a ", Emph [Str "b"]], Str " ", Image "/i.png" "" [Str "alt"]]
    it "resolves reference links" $
      parseMarkdownBlocks "[a] [text][B]\n\n[a]: /1\n[b]: </2> \"two\""
        `shouldBe` [Paragraph [Link "/1" "" [Str "a"], Str " ", Link "/2" "two" [Str "text"]]]
    it "links autolinks and bare web and email addresses" $ do
      "<https://a.b/c> https://x.org/a. www.x.org"
        `inlineOf` [ Link "https://a.b/c" "" [Str "https://a.b/c"]
                   , Str " "
                   , Link "https://x.org/a" "" [Str "https://x.org/a"]
                   , Str ". "
                   , Link "http://www.x.org" "" [Str "www.x.org"]
                   ]
      "mail a.b+c@x.org." `inlineOf` [Str "mail ", Link "mailto:a.b+c@x.org" "" [Str "a.b+c@x.org"], Str "."]
    it "decodes escapes and entities into one piece of text" $
      "\\*a\\* &amp; &ouml; &#65; &bogus;" `inlineOf` [Str "*a* & \246 A &bogus;"]
  where
    -- The one block a text parses to, and the spans of the one paragraph it
    -- parses to.
    parsesTo src b = parseMarkdownBlocks src `shouldBe` [b]
    inlineOf src spans = parsesTo src (Paragraph spans)
    para t = Paragraph [Str t]
    item = ListItem Nothing
