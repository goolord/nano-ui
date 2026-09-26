# Changelog

## Unreleased

### Added

- First release: `MarkdownDoc`, CommonMark with GitHub's tables, task lists
  (in bullet lists), strikethrough and autolinks, parsed by `commonmark`, raw
  HTML shown as text without its comments; `appendMarkdown`, which keeps the
  blocks that appended text cannot change and parses again only the rest, down
  to the last item of a list, row of a table, line of fenced code or block of a
  quote; and `markdown`, which draws one with rich text and returns the link
  clicked. Documents are equal when their texts are. `NFData` instances for
  the syntax types. Task items draw nano-ui's checkbox, headings scale the
  backend's default font size, and a drawn image shows its title as a tooltip.
- `MarkdownConfig es` carries the view's effect row, for `mdBlock`, which
  draws any block, at any depth, your own way (syntax highlighting, images
  loaded as they are drawn, chrome of your own), and `markdownBlock`, the
  widget's own drawing of a block, to fall back to or wrap. Style modifiers
  over the look of inline code (`mdInlineCode`, with a background from
  `mdInlineCodeBackground`), code blocks (`mdCodeBlock`), quotes (`mdQuote`)
  and table cells (`mdTableCell`).
- `markdownSource`, a document's text, and `markdownImages`, its images'
  sources, for loading them ahead.
- The example streams its reply with nano-ui's `useStream`, appending tokens
  on the producer's thread.
