# Changelog

## 0.2.0.0 -- Unreleased

Initial release.

- CommonMark rendering (`MarkdownDoc`, `parseMarkdown`, `markdown`, `markdownConfigured`) with GitHub tables, task lists, strikethrough, autolinks, code blocks, quotes, headings, and clickable links.
- Incremental streaming support (`appendMarkdown`): re-parses only the trailing blocks affected by appended text and caches unchanged block rich-text segments in a bounded `MarkdownCache` (`newMarkdownCache`).
- Customizable block and inline styling via `MarkdownConfig` (`mdBlock`, `mdInlineCode`, `mdInlineCodeBackground`, `mdCodeBlock`, `mdQuote`, `mdTableCell`), plus `markdownSource` and `markdownImages` helpers.
- The `sdl` Cabal flag (for the streaming example) is off by default so the library requires neither `nano-ui-sdl` nor SDL3.
- Builds with GHC 9.10 through 9.14 (`base >=4.20 && <4.23`).
