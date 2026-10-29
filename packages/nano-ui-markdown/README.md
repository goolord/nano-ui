# nano-ui-markdown

Markdown rendering for [nano-ui](https://github.com/goolord/nano-ui). Parsing
uses [commonmark](https://hackage.haskell.org/package/commonmark). Documents
support incremental append for streamed text.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (unless)
import Data.Foldable (for_)
import Data.Text qualified as T
import NanoUI (NanoUI, label, readVar, useVar, writeVar)
import NanoUI.Markdown

readme :: MarkdownDoc
readme = parseMarkdown "# Hello\n\nSome *emphasis*, `code` and [a link](https://example.com)."

-- cache <- newMarkdownCache, once at component setup
view :: MarkdownCache -> NanoUI ()
view cache = do
  lastLinkVar <- useVar ""
  clicked <- markdown cache readme
  for_ clicked (writeVar lastLinkVar)
  lastLink <- readVar lastLinkVar
  unless (T.null lastLink) $ label ("Clicked " <> lastLink)
```

`markdown` returns the destination of a link clicked this frame. Parse once
and store the `MarkdownDoc`; do not parse per frame.

## Syntax

CommonMark plus GitHub tables, task lists, `~~strikethrough~~` and autolinks
from `commonmark-extensions`.

- Task lists apply to bullet lists only. `1. [x] a` is an ordered item with
  text `[x] a`.
- Raw HTML renders as text. HTML comments are dropped.
- `markdownBlocks` returns the AST defined in `NanoUI.Markdown.Syntax`.

## Streaming

`appendMarkdown t doc` produces the same blocks as reparsing the full source,
but reparses only the tail: the last top-level block, or the last item, row,
line or block when the text ends inside a list, table, fenced code or block
quote. Plain words appended to a trailing paragraph skip the parser.

Larger reparses:

| Case | Reparsed |
| --- | --- |
| doc ends in a paragraph, indented code, HTML block, or list/code nested in a list item or quote | that whole block |
| append adds or changes a link reference definition after other blocks | the whole document |

Every call reparses the tail, so append a frame's tokens in one call.

Tokens from a worker thread, via `useStream` (`stream <- newStream` at setup):

```haskell
replyView :: MarkdownCache -> Stream Int MarkdownDoc -> Client -> Int -> NanoUI ()
replyView cache stream client replyId = do
  doc <- useStream stream replyId emptyMarkdown $ \update ->
    onToken client (\token -> update (appendMarkdown token))
  void (markdown cache doc)
```

Tokens on the UI thread:

```haskell
onTokens :: [Text] -> MarkdownDoc -> MarkdownDoc
onTokens tokens = appendMarkdown (T.concat tokens)
```

`MarkdownDoc` equality compares source text (`markdownSource`). Blocks before
the changed one keep their ids and are not re-measured, laid out or
repainted. For long chat logs, draw only visible messages
(`getScrollMetricsUi`).

## Rendering

`markdownConfigured cache config doc`. Each modifier applies over the default
look; colours left `Nothing` come from the theme.

| Field | Controls |
| --- | --- |
| `mdLayout` | document column |
| `mdText` | body font |
| `mdHeading` | heading font by level (1 to 6), over the body font |
| `mdLinkColor` | link colour |
| `mdInlineCode`, `mdInlineCodeBackground` | inline code font and background |
| `mdCodeBlock` | code block panel style |
| `mdQuote` | block quote text |
| `mdTableCell` | table cell style; the `Bool` is true for header cells |
| `mdCopyCode` | copy button on code blocks |
| `mdImage` | image source to `(ImageId, Size)` |
| `mdBlock` | custom drawing for any block, at any depth |

```haskell
markdownConfigured
  cache
  defaultMarkdownConfig
    { mdText = fontSize 15
    , mdImage = \src -> lookup src loadedImages
    , mdInlineCodeBackground = Just (colorRGBA 60 64 72 255)
    , mdCodeBlock = background (colorRGBA 30 32 36 255)
    }
  doc
```

Behaviour:

- Images render only when alone in a paragraph (or alone in a link there) and
  resolved by `mdImage`. A click returns the link, else the source. The
  tooltip is the image title, else the link title. Other images render their
  alt text as a link. `markdownImages` lists a document's image sources for
  preloading.
- Long code lines wrap and keep their indentation.
- Heading sizes scale from the backend's default font size.
- Task items use nano-ui's checkbox.
- Text in paragraphs, headings, table cells and code blocks is selectable.
  Ctrl+A / Ctrl+C (Cmd on macOS) select and copy within the focused block.

### Custom blocks

`mdBlock` receives the default renderer, to fall back to or wrap. Return the
clicked link, if any.

Syntax highlighting:

```haskell
highlighted :: MarkdownConfig
highlighted =
  defaultMarkdownConfig
    { mdBlock = \own -> \case
        CodeBlock "haskell" code -> do
          panel (richTextWith (fontMono . fillW) (highlight code))
          pure Nothing
        b -> own b
    }
```

Lazy image loading with `useTask`, one `newTask` handle per source allocated
at setup (`decodePng` is application code):

```haskell
lazyImages :: (Text -> Task Text (ImageId, Size)) -> MarkdownConfig
lazyImages taskFor =
  defaultMarkdownConfig
    { mdBlock = \own -> \case
        Paragraph [Image src _ alt] -> do
          loaded <- useTask (taskFor src) src (decodePng src)
          case loaded of
            Just (iid, Size w h) -> void (image (fixedWH w h) iid)
            Nothing -> label ("Loading " <> spansText alt)
          pure Nothing
        b -> own b
    }
```

## Build

Depend on `nano-ui`, `nano-ui-markdown` and `text`. The library has no
backend dependency.

`cabal run nano-ui-markdown-example` streams a reply into a chat view. It
requires the `sdl` flag (off by default, on in this repository's
`cabal.project`).

## Documentation

[The nano-ui Book](https://goolord.github.io/nano-ui/index.html)
