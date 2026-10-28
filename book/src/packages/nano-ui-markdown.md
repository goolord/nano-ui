# Markdown Rendering (`nano-ui-markdown`)

`nano-ui-markdown` parses Markdown with [commonmark](https://hackage.haskell.org/package/commonmark) and draws it with nano-ui's rich text. It is designed for two jobs: static documentation, and streamed replies where text arrives a token at a time.

## A document

Parse once and keep the `MarkdownDoc`; drawing it every frame is cheap.

```haskell
import NanoUI.Markdown

readme :: MarkdownDoc
readme = parseMarkdown "# Hello\n\nSome *emphasis*, `code` and [a link](https://example.com)."

view :: MarkdownCache -> NanoUI ()
view cache = do
  lastLink <- useVar ""
  clicked <- markdown cache readme
  for_ clicked (writeVar lastLink)
  link <- readVar lastLink
  unless (T.null link) $ label ("Clicked " <> link)
```

`markdown` takes the cache and the document and returns the destination of a link clicked this frame, if any. Allocate the `MarkdownCache` once in setup with `newMarkdownCache`; it caches block drawing across frames.

The renderer supports headings, emphasis, inline code, fenced code blocks, blockquotes, ordered and unordered lists, task lists, tables, strikethrough, and autolinks. Paragraphs, headings, table cells, and code blocks can be selected and copied, with Select All and Copy acting on the focused block. Raw HTML is shown as text with comments removed.

## Streaming

`appendMarkdown` adds text without re-parsing the whole document:

```haskell
-- On the UI thread, append a frame's tokens at once:
onTokens :: [Text] -> MarkdownDoc -> MarkdownDoc
onTokens tokens = appendMarkdown (T.concat tokens)
```

On a worker thread, stream tokens into the view with `useStream`:

```haskell
replyView :: MarkdownCache -> Stream Int MarkdownDoc -> Client -> Int -> NanoUI ()
replyView cache stream client replyId = do
  doc <- useStream stream replyId emptyMarkdown $ \update ->
    onToken client (\token -> update (appendMarkdown token))
  void (markdown cache doc)
```

When appending on the UI thread, append a frame's batch in **one** call. Appending a token at a time costs a parse per token; a batched append parses the last block once. Appending to a plain final paragraph word-by-word is the exception that avoids a parse for ordinary words.

### What re-parses

`appendMarkdown new old` keeps the blocks the new text cannot change and re-parses the rest: the last top-level block, or — when the text ends inside a list, table, fenced code block, or blockquote — only its last item, row, line, or block. Two cases still cost a whole-document parse:

- the reply ends in one long block that cannot be split this way (a paragraph, a list or code block inside a list item or blockquote, indented code, or an HTML block);
- an appended link reference definition changes earlier blocks that may link to it.

Joining the reparsed part onto a long list or table still copies the earlier part, which is cheap next to parsing but grows with the block.

## Custom block drawing

`MarkdownConfig` has an `mdBlock` hook that overrides block drawing at any depth. It receives the built-in drawing for the block, so a renderer can wrap or replace it:

```haskell
highlighted :: MarkdownConfig
highlighted = defaultMarkdownConfig
  { mdBlock = \own -> \case
      CodeBlock "haskell" code -> Nothing <$ panel (richText (highlight code))
      block -> own block
  }
```

`markdownConfigured cache highlighted doc` draws with the config.

## Related

- The rich-text widget itself: `NanoUI.Widgets.RichText`.
- Streaming primitives: [Asynchronous Work & Streams](../advanced/background-work.md).
- A complete streamed view: [Live Data & Background Work](../tutorials/live-data.md).
