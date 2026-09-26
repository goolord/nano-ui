-- |
-- Module      : NanoUI.Markdown
-- Description : Markdown documents drawn with nano-ui
-- Copyright   : (c) 2026 Zachary Churchill
-- License     : MIT
-- Maintainer  : zacharyachurchill@gmail.com
--
-- Markdown drawn with nano-ui's rich text and layout. Parse a document once,
-- keep it in your model, and draw it every frame. 'markdown' returns the
-- link clicked this frame, if any:
--
-- > view :: MarkdownDoc -> NanoUI ()
-- > view doc = do
-- >   (lastLink, setLastLink) <- useState ("" :: Text)
-- >   clicked <- markdown doc
-- >   for_ clicked setLastLink
-- >   unless (T.null lastLink) $ label ("Clicked " <> lastLink)
--
-- Grow a streamed reply with 'appendMarkdown'. When tokens arrive on a
-- worker thread, nano-ui's @useStream@ can append each one there:
--
-- > doc <- useStream replyId emptyMarkdown $ \update ->
-- >   onToken client (\token -> update (appendMarkdown token))
--
-- On the UI thread, append a frame's tokens in one call:
--
-- > onTokens :: [Text] -> MarkdownDoc -> MarkdownDoc
-- > onTokens tokens = appendMarkdown (T.concat tokens)
--
-- Appending reparses only the last top-level block, or less when the text
-- ends inside a list, table, fenced code or block quote.
-- "NanoUI.Markdown.Document" describes when more is reparsed.
--
-- 'mdBlock' overrides block drawing at any depth, given the widget's own
-- drawing to fall back to or wrap:
--
-- > highlighted :: MarkdownConfig NanoUIEs
-- > highlighted = defaultMarkdownConfig {mdBlock = \own -> \case
-- >   CodeBlock "haskell" code -> Nothing <$ panel (richText (highlight code))
-- >   b -> own b}
--
-- Parsing uses @commonmark@ with GitHub tables, task lists (in bullet
-- lists), strikethrough and autolinks from @commonmark-extensions@. Raw HTML
-- is shown as text, with comments removed.
module NanoUI.Markdown
  ( -- * Documents
    MarkdownDoc
  , emptyMarkdown
  , parseMarkdown
  , appendMarkdown
  , markdownBlocks
  , markdownSource
  , markdownImages
  , parseMarkdownBlocks

    -- * Drawing
  , markdown
  , markdownConfigured
  , MarkdownConfig (..)
  , defaultMarkdownConfig

    -- * Syntax
  , module NanoUI.Markdown.Syntax
  ) where

import NanoUI.Markdown.Document
import NanoUI.Markdown.Syntax
import NanoUI.Markdown.Widget
