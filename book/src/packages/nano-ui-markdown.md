# nano-ui-markdown: Markdown Rendering

`nano-ui-markdown` provides rich Markdown rendering optimized for immediate-mode GUIs and streaming responses.

## Key Capabilities

- **Incremental Append**: Fast incremental parsing designed specifically for streaming LLM tokens and chat replies without re-parsing the entire document on every token.
- **Rich Elements**: Headings, bold/italic, inline code, fenced code blocks with syntax highlighting, blockquotes, ordered/unordered lists, and tables.
- **Theme Awareness**: Text colors, code backgrounds, and table borders automatically conform to the active `Theme`.

```haskell
import NanoUI
import NanoUI.Markdown

chatMessage :: Text -> NanoUI ()
chatMessage markdownText = markdown markdownText
```
