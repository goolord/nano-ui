-- | The parsed form of a Markdown document: blocks, and the inline spans in
-- their text. "NanoUI.Markdown" draws it; an application can also walk it,
-- for a table of contents or a plain-text copy.
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
  = -- | A paragraph of inline text, or a raw HTML block as its text without
    -- its comments. An HTML block that is only comments is left out.
    Paragraph ![Span]
  | -- | A heading of level 1 to 6: @# Title@, or a paragraph underlined with
    -- @===@ (level 1) or @---@ (level 2).
    Heading !Int ![Span]
  | -- | A horizontal rule: @---@, @***@ or @___@.
    ThematicBreak
  | -- | Code, fenced with backticks or tildes or indented by four spaces:
    -- the fence's info string (empty for indented code) and the code, its
    -- lines joined by newlines without a final one.
    CodeBlock !Text !Text
  | -- | Blocks quoted with @>@.
    BlockQuote ![Block]
  | -- | A list: its kind, whether it is tight (no blank line between its
    -- items or inside one), and its items.
    List !ListType !Bool ![ListItem]
  | -- | A GitHub table: each column's alignment, the header cells, and the
    -- rows, each with one cell a column.
    Table ![CellAlign] ![[Span]] ![[[Span]]]
  deriving (Eq, Show, Generic)

instance NFData Block

-- | Inline content.
data Span
  = -- | Plain text, escapes and entities decoded. Raw HTML stays text, but
    -- an HTML comment is left out.
    Str !Text
  | -- | A line break inside a paragraph that is not a hard break.
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
  | -- | A link: its destination, its title (empty when it has none), and its
    -- text. Inline links, reference links, @\<https://...\>@, and bare web
    -- and email addresses all parse to one.
    Link !Text !Text ![Span]
  | -- | An image: its source, its title, and its alt text.
    Image !Text !Text ![Span]
  deriving (Eq, Show, Generic)

instance NFData Span

-- | A bullet list with its bullet character (@-@, @+@ or @*@), or an ordered
-- list with its first number and delimiter (@.@ or @)@). A list ends where
-- an item starts with another bullet or delimiter, or where items with task
-- check boxes and items without meet.
data ListType
  = Bullet !Char
  | Ordered !Int !Char
  deriving (Eq, Show, Generic)

instance NFData ListType

-- | A list item: its task-list check box, if it starts with @[ ]@ or
-- @[x]@, and its blocks. Only a bullet list's items have check boxes:
-- @1. [x] a@ is an ordered item with the text @[x] a@.
data ListItem = ListItem
  { itemTask :: !(Maybe Bool)
  -- ^ 'Just' whether it is checked, for a task-list item.
  , itemBlocks :: ![Block]
  }
  deriving (Eq, Show, Generic)

instance NFData ListItem

-- | A table column's alignment, from the colons in its delimiter row.
data CellAlign = CellDefault | CellLeft | CellCenter | CellRight
  deriving (Eq, Show, Enum, Bounded, Generic)

instance NFData CellAlign

-- | The text of some spans without their formatting: breaks become spaces
-- and images their alt text.
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
