# nano-ui-markdown

Markdown for [nano-ui](https://github.com/goolord/nano-ui): documents parsed
by the [commonmark](https://hackage.haskell.org/package/commonmark) library,
which text can be appended to without parsing the whole document again, and a
widget that draws them with nano-ui's rich text.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (unless)
import Data.Foldable (for_)
import Data.Text (Text)
import Data.Text qualified as T
import NanoUI (NanoUI, label, useState)
import NanoUI.Markdown

readme :: MarkdownDoc
readme = parseMarkdown "# Hello\n\nSome *emphasis*, `code` and [a link](https://example.com)."

view :: NanoUI ()
view = do
  (lastLink, setLastLink) <- useState ("" :: Text)
  clicked <- markdown readme
  for_ clicked setLastLink
  unless (T.null lastLink) $ label ("Clicked " <> lastLink)
```

`markdown` returns the destination of a link clicked this frame. Parse a
document once and keep it in your model, rather than parsing it every frame.

## Streaming

A reply that arrives on a thread of its own, from a network client, streams
into the view with nano-ui's `useStream`: the producer appends each token to
the document on its thread, and the view draws the document so far. No
`IORef` is needed, and the UI thread does no parsing:

```haskell
replyView :: Client -> Int -> NanoUI ()
replyView client replyId = do
  doc <- useStream replyId emptyMarkdown $ \update ->
    onToken client (\token -> update (appendMarkdown token))
  void (markdown doc)
```

A reply kept in a model instead takes the tokens that arrived in a frame at
once, joined:

```haskell
newtype Model = Model {reply :: MarkdownDoc}

onTokens :: [Text] -> Model -> Model
onTokens tokens m = m {reply = appendMarkdown (T.concat tokens) (reply m)}
```

`appendMarkdown b (parseMarkdown a)` has the blocks of `parseMarkdown (a <> b)`,
but it keeps the blocks that `b` cannot change and parses again only the rest
of the text: the last top-level block, or, when the text ends inside a list, a
table, fenced code or a block quote, only its last item, row, line or block. So
a token costs about as much at the end of a long reply as of a short one,
unless the reply ends in one long block that is not split this way, which is
parsed whole: a paragraph, a list or code block inside a list item or a block
quote, indented code, or an HTML block. Every append parses that rest again, so
on the UI thread, appending the tokens of a frame one at a time costs more for
nothing.

Text after link reference definitions is parsed with them. An append that adds
or changes a definition after other blocks parses the whole reply again, as
those blocks may link to it: definitions at the end of a reply cost a whole
parse an append.

`markdownSource` gives a document's text back. Two documents are equal when
their texts are, so one kept in `useState`
changes with every append. The widget keeps the ids of the blocks before the
one that changed, so they are not measured, laid out or repainted again. A chat
log of thousands of paragraphs should draw only the messages in view, using
`getScrollMetrics`.

`cabal run nano-ui-markdown-example` streams a reply into a chat view with
`useStream`. It
needs `nano-ui-sdl`, behind this package's `sdl` flag (on by default); the
library itself does not depend on a backend.

## What it parses

CommonMark with GitHub's tables, task lists, `~~strikethrough~~`, and bare web
and email links, from `commonmark-extensions`. Task lists are bullet lists
only: `1. [x] a` is an ordered item with the text `[x] a`. Raw HTML stays text,
and HTML comments are left out. `NanoUI.Markdown.Syntax` has the parsed form,
which `markdownBlocks` returns.

## Drawing

`markdownConfigured` takes a `MarkdownConfig`: the column's layout, the body
and heading fonts, the link colour, style modifiers over the look of inline
code (with a background, if you like), code blocks, quotes and table cells,
whether code blocks have a copy button, and `mdImage`, which resolves an
image's source to a registered image and a size. An image alone in its
paragraph, or alone in a link, is drawn when `mdImage` resolves it; a click
goes to the link, else to its source, and its title (else the link's) shows as
a tooltip. Any other image shows its alt text as a link. `markdownImages`
lists a document's image sources, to load them before they are drawn. Long
code lines wrap, keeping their indentation, and headings scale the backend's
default font size. Task items draw nano-ui's checkbox.

```haskell
markdownConfigured
  defaultMarkdownConfig
    { mdText = fontSize 15
    , mdImage = \src -> lookup src loadedImages
    , mdInlineCodeBackground = Just (colorRGBA 60 64 72 255)
    , mdCodeBlock = background (colorRGBA 30 32 36 255)
    }
  doc
```

`mdBlock` draws blocks your own way: it is asked of every block, in quotes
and lists too, and `Nothing` leaves a block to the widget. `markdownBlock`
draws one as the widget would, to fall back to or to wrap. The configuration
carries the view's effect row, as nano-ui's `PaneGridConfig` does, since
`mdBlock` runs widgets of yours. A code block with syntax highlighting, where
`highlight` stands for a highlighter of your own that returns rich-text
pieces:

```haskell
highlighted :: MarkdownConfig NanoUIEs
highlighted =
  defaultMarkdownConfig
    { mdBlock = \case
        CodeBlock "haskell" code -> Just $ do
          panel (richTextWith (fontMono . fillW) (highlight code))
          pure Nothing
        _ -> Nothing
    }
```

An image can load as it is drawn, with `useTask` (`decodePng` stands for a
decoder of yours, which registers the image and returns its id and size):

```haskell
lazyImages :: MarkdownConfig NanoUIEs
lazyImages =
  defaultMarkdownConfig
    { mdBlock = \case
        Paragraph [Image src _ alt] -> Just $ do
          loaded <- useTask src (decodePng src)
          case loaded of
            Just (iid, Size w h) -> void (image (fixedWH w h) iid)
            Nothing -> label ("Loading " <> spansText alt)
          pure Nothing
        _ -> Nothing
    }
```

## Build

Add `nano-ui`, `nano-ui-markdown` and `text` to your application's
`build-depends`. `cabal build lib:nano-ui-markdown` builds the library without
SDL.
