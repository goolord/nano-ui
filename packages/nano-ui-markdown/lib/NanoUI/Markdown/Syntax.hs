-- | Parsed Markdown: blocks and their inline spans. "NanoUI.Markdown" draws
-- it; applications can also walk it, for a table of contents or a
-- plain-text copy.
module NanoUI.Markdown.Syntax
  ( Block (..)
  , Span (..)
  , ListType (..)
  , ListItem (..)
  , CellAlign (..)
  , spansText
  ) where

import Control.DeepSeq (NFData)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

-- | A block of a document.
data Block
  = -- | Inline text. Raw HTML blocks also become paragraphs, with comments
    -- removed; a block of only comments is dropped.
    Paragraph ![Span]
  | -- | Level 1 to 6: @# Title@, or a line underlined with @===@ (1) or
    -- @---@ (2).
    Heading !Int ![Span]
  | -- | A horizontal rule: @---@, @***@ or @___@.
    ThematicBreak
  | -- | Fenced or four-space-indented code: the info string (empty for
    -- indented code) and the code, without a trailing newline.
    CodeBlock !Text !Text
  | -- | Blocks quoted with @>@.
    BlockQuote ![Block]
  | -- | Kind, tightness (no blank lines between or inside items), items.
    List !ListType !Bool ![ListItem]
  | -- | GitHub table: column alignments, header cells, and rows with one
    -- cell per column.
    Table ![CellAlign] ![[Span]] ![[[Span]]]
  deriving (Eq, Show, Generic)

instance NFData Block

-- | Inline content.
data Span
  = -- | Plain text with escapes and entities decoded. Inline raw HTML stays
    -- as text; HTML comments are dropped.
    Str !Text
  | -- | A plain line break inside a paragraph.
    SoftBreak
  | -- | A line ending in two spaces or a backslash.
    HardBreak
  | -- | @*text*@ or @_text_@.
    Emph ![Span]
  | -- | @**text**@ or @__text__@.
    Strong ![Span]
  | -- | @~~text~~@.
    Strike ![Span]
  | -- | @\`code\`@.
    Code !Text
  | -- | Destination, title (empty if none), text. Covers inline and
    -- reference links, autolinks (@\<https://...\>@), and bare web and
    -- email addresses.
    Link !Text !Text ![Span]
  | -- | Source, title, alt text.
    Image !Text !Text ![Span]
  deriving (Eq, Show, Generic)

instance NFData Span

-- | A bullet list's character (@-@, @+@ or @*@), or an ordered list's start
-- number and delimiter (@.@ or @)@). A list ends when the bullet or
-- delimiter changes, or where task and non-task items meet.
data ListType
  = Bullet !Char
  | Ordered !Int !Char
  deriving (Eq, Show, Generic)

instance NFData ListType

-- | A list item. Only bullet items can be tasks: @1. [x] a@ is an ordered
-- item with the text @[x] a@.
data ListItem = ListItem
  { itemTask :: !(Maybe Bool)
  -- ^ For task items (@[ ]@ or @[x]@), whether checked.
  , itemBlocks :: ![Block]
  }
  deriving (Eq, Show, Generic)

instance NFData ListItem

-- | Column alignment, from the colons in the delimiter row.
data CellAlign = CellDefault | CellLeft | CellCenter | CellRight
  deriving (Eq, Show, Enum, Bounded, Generic)

instance NFData CellAlign

-- | Spans as plain text: breaks become spaces, images their alt text.
spansText :: [Span] -> Text
spansText = T.concat . map go
  where
    go = \case
      Str t -> t
      SoftBreak -> " "
      HardBreak -> " "
      Emph xs -> spansText xs
      Strong xs -> spansText xs
      Strike xs -> spansText xs
      Code t -> t
      Link _ _ xs -> spansText xs
      Image _ _ xs -> spansText xs
