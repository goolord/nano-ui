# nano-ui-markdown

CommonMark rendering for [nano-ui](https://github.com/goolord/nano-ui), powered by [commonmark](https://hackage.haskell.org/package/commonmark) with fast incremental appends for streamed text.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (unless)
import Data.Foldable (for_)
import Data.Text qualified as T
import NanoUI (NanoUI, label, readVar, useVar, writeVar)
import NanoUI.Markdown

readme :: MarkdownDoc
readme = parseMarkdown "# Hello\n\nSome *emphasis*, `code`, and [a link](https://example.com)."

-- Allocate cache <- newMarkdownCache once during setup.
view :: MarkdownCache -> NanoUI ()
view cache = do
  lastLinkVar <- useVar ""
  clicked <- markdown cache readme
  for_ clicked (writeVar lastLinkVar)
  lastLink <- readVar lastLinkVar
  unless (T.null lastLink) $ label ("Clicked " <> lastLink)
```

`markdown` returns `Just url` on the frame a link is clicked. Parse static Markdown once and store the `MarkdownDoc`; avoid reparsing on every frame.

## Supported syntax

Standard CommonMark plus GitHub extensions from `commonmark-extensions`: tables, task lists, `~~strikethrough~~`, and autolinks.

- Task checkboxes apply to bullet lists only (`1. [x] a` stays a plain ordered item).
- Raw HTML renders literally as text, and HTML comments are stripped.
- Inspect the parsed block AST with `markdownBlocks` (types in `NanoUI.Markdown.Syntax`).

## Incremental streaming

`appendMarkdown chunk doc` produces the exact same AST as reparsing the entire document, while only reparsing the trailing block (or the final list item, table row, fenced-code line, or nested block). Plain words appended to a trailing paragraph bypass the parser altogether.

Two edge cases trigger broader reparses:

| Case | Scope reparsed |
| --- | --- |
| Document ends in a paragraph, indented code block, HTML block, or nested list/code inside a quote or list item | That trailing top-level block |
| Appended text introduces or changes a link-reference definition | The full document |

Because every call reparses the tail, concatenate all tokens that arrive during a single frame into one `appendMarkdown` call.

Streaming tokens from a worker thread via `useStream` (`stream <- newStream` during setup):

```haskell
replyView :: MarkdownCache -> Stream Int MarkdownDoc -> Client -> Int -> NanoUI ()
replyView cache stream client replyId = do
  doc <- useStream stream replyId emptyMarkdown $ \update ->
    onToken client (\token -> update (appendMarkdown token))
  void (markdown cache doc)
```

Batching tokens on the UI thread:

```haskell
onTokens :: [Text] -> MarkdownDoc -> MarkdownDoc
onTokens tokens = appendMarkdown (T.concat tokens)
```

`MarkdownDoc` compares equality by source text (`markdownSource`). Unchanged blocks before the tail keep their widget IDs and skip re-measurement, layout, and repainting. For very long chat histories, combine this with `getScrollMetrics` to build only visible messages.

## Customizing appearance

Use `markdownConfigured cache config doc` to override fonts, colors, and block renderers. Any color left as `Nothing` falls back to the active theme:

| Config field | Controls |
| --- | --- |
| `mdLayout` | Outer document column layout |
| `mdText` | Base body text font |
| `mdHeading` | Heading font by level (1 to 6), layered over `mdText` |
| `mdLinkColor` | Hyperlink color |
| `mdInlineCode`, `mdInlineCodeBackground` | Inline code font and pill background |
| `mdCodeBlock` | Fenced/indented code block panel style |
| `mdQuote` | Blockquote text style |
| `mdTableCell` | Table cell style (`Bool` is `True` for header cells) |
| `mdCopyCode` | Copy-to-clipboard button on code blocks |
| `mdImage` | Resolves an image URL/path to `(ImageId, Size)` |
| `mdBlock` | Custom renderer for any block at any nesting depth |

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

Rendering details:

- Standalone images (alone in a paragraph, or wrapped directly in a link) render when resolved by `mdImage`. Clicking returns the link target or image source, and tooltips show the image or link title. Unresolved or inline images fall back to clickable alt text. Use `markdownImages` to discover image URLs ahead of time for preloading.
- Long lines inside code blocks wrap while preserving leading indentation.
- Heading sizes scale relative to the backend's default font size.
- Task list items render with `nano-ui`'s native checkbox widget.
- Paragraphs, headings, table cells, and code blocks support text selection and Ctrl+A / Ctrl+C (Cmd on macOS) within the focused block.

### Custom block renderers

`mdBlock` receives the default block renderer (`own`) so you can override specific cases and delegate everything else. Return `Just url` if a link was clicked.

Adding syntax highlighting to Haskell code blocks:

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

Lazy-loading images with `useTask` (using one `Task` handle per source allocated during setup):

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

## Building and running the example

Add `nano-ui`, `nano-ui-markdown`, and `text` to `build-depends`; the library itself requires no backend.

Run the streaming chat demo (requires the `sdl` Cabal flag, enabled by default in this repo's `cabal.project`):

```sh
cabal run nano-ui-markdown-example
```

## Documentation

- [The nano-ui Book: Markdown](https://goolord.github.io/nano-ui/book/packages/index.html#markdown)
