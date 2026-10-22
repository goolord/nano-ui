# Changelog

## 0.1.0.0 -- Unreleased

### Added

- First release: `MarkdownDoc`, CommonMark with GitHub's tables, task lists
  (in bullet lists), strikethrough and autolinks, parsed by `commonmark`, raw
  HTML shown as text without its comments; `appendMarkdown`, which keeps the
  blocks that appended text cannot change and parses again only the rest, down
  to the last item of a list, row of a table, line of fenced code or block of a
  quote, and adds plain words to a paragraph without parsing it; and
  `markdown`, which draws one with rich text and returns the link clicked.
  Documents are equal when their texts are. `NFData` instances for the syntax
  types. Task items draw nano-ui's checkbox, headings scale the backend's
  default font size, and a drawn image shows its title as a tooltip.
- `MarkdownConfig`'s `mdBlock` draws any block, at any depth, your own way
  (syntax highlighting, images loaded as they are drawn, chrome of your own),
  given the widget's own drawing of a block to fall back to or wrap. Style
  modifiers over the look of inline code (`mdInlineCode`, with a background from
  `mdInlineCodeBackground`), code blocks (`mdCodeBlock`), quotes (`mdQuote`)
  and table cells (`mdTableCell`).
- `markdownSource`, a document's text, and `markdownImages`, its images'
  sources, for loading them ahead.
- Blocks are 16 pixels apart, as are the items of a loose list; a tight
  list's items are 6 apart.
- The widget keeps the rich-text pieces of blocks that are unchanged since
  the last frame, so a long document costs little a frame while a reply
  streams into its last block: its paragraphs are neither rebuilt nor
  hashed again.
- `markdown` and `markdownConfigured` keep those pieces in a `MarkdownCache`,
  allocated once with `newMarkdownCache`. It sizes itself from the blocks one
  pass draws, not from its history, so keys that churn (a document redrawn
  under new keys) do not grow it. `markdownCacheSize` reports what it holds.
- The example streams its reply with nano-ui's `useStream`, appending tokens
  on the producer's thread. The `sdl` flag that builds it is off by default,
  so depending on the library needs neither nano-ui-sdl nor SDL3.
- The library imports `NanoUI.Internal` modules, which change without
  notice, so it allows only nano-ui 0.2.0.x (`>=0.2.0 && <0.2.1`).
