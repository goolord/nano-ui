-- |
-- Module      : NanoUI.Markdown
-- Description : Markdown documents drawn with nano-ui
-- Copyright   : (c) 2026 Zachary Churchill
-- License     : MIT
-- Maintainer  : zacharyachurchill@gmail.com
--
-- Markdown drawn with nano-ui's rich text and layout. Parse a document once,
-- keep it in your model, and draw it every frame. 'markdown' returns the
-- destination of a link clicked this frame:
--
-- > view :: MarkdownDoc -> NanoUI ()
-- > view doc = do
-- >   (lastLink, setLastLink) <- useState ("" :: Text)
-- >   clicked <- markdown doc
-- >   for_ clicked setLastLink
-- >   unless (T.null lastLink) $ label ("Clicked " <> lastLink)
--
-- A reply that streams in grows with 'appendMarkdown'. One that arrives on
-- a thread of its own streams in with nano-ui's @useStream@, which appends
-- each token on that thread:
--
-- > doc <- useStream replyId emptyMarkdown $ \update ->
-- >   onToken client (\token -> update (appendMarkdown token))
--
-- On the UI thread, append the tokens that arrived in a frame at once:
--
-- > onTokens :: [Text] -> MarkdownDoc -> MarkdownDoc
-- > onTokens tokens = appendMarkdown (T.concat tokens)
--
-- Appending keeps the blocks that the new text cannot change and parses
-- again only the rest: the last top-level block, or less when the text ends
-- inside a list, a table, fenced code or a block quote, down to its last
-- item, row, line or block. "NanoUI.Markdown.Document" says when more is
-- parsed again.
--
-- 'mdBlock' draws blocks your own way, at any depth, falling back to
-- 'markdownBlock', the widget's own drawing:
--
-- > highlighted :: MarkdownConfig NanoUIEs
-- > highlighted = defaultMarkdownConfig {mdBlock = \case
-- >   CodeBlock "haskell" code -> Just (Nothing <$ panel (richText (highlight code)))
-- >   _ -> Nothing}
--
-- The @commonmark@ library parses the text, with GitHub's tables, task lists
-- (in bullet lists), strikethrough and bare web and email links from
-- @commonmark-extensions@. Raw HTML stays text, without its comments.
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
  , markdownBlock

    -- * Syntax
  , module NanoUI.Markdown.Syntax
  ) where

import NanoUI.Markdown.Document
import NanoUI.Markdown.Syntax
import NanoUI.Markdown.Widget
