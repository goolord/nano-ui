-- | Parse Markdown into "NanoUI.Markdown.Syntax" using @commonmark@ plus the
-- GitHub extensions from @commonmark-extensions@ (tables, task lists,
-- strikethrough, autolinks). Also records what "NanoUI.Markdown.Document"
-- needs to reparse only a text's tail: each block's lines, its children and
-- its link reference definitions.
module NanoUI.Markdown.Internal.Parse
  ( Parsed (..)
  , Node (..)
  , Shape (..)
  , Refs
  , parseLines
  , unixLines
  , splitLines
  , chomp
  ) where

import Commonmark qualified as C
import Commonmark.Blocks (BPState (..), BlockParser)
import Commonmark.Entity (lookupEntity)
import Commonmark.Extensions
  ( ColAlignment (..)
  , HasPipeTable (..)
  , HasStrikethrough (..)
  , HasTaskList (..)
  , autolinkSpec
  , pipeTableSpec
  , strikethroughSpec
  , taskListSpec
  )
import Commonmark.ReferenceMap (LinkInfo (..), ReferenceMap (..))
import Data.Char (isSpace)
import Data.Dynamic (toDyn)
import Data.Functor.Identity (Identity, runIdentity)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid (Endo (..))
import Data.Text (Text)
import Data.Text qualified as T
import NanoUI.Markdown.Syntax
import Text.Parsec (updateState)

data Parsed = Parsed
  { parsedText :: !Text
  -- ^ The parsed text, with line endings normalised to @\\n@.
  , parsedComplete :: !Int
  -- ^ Number of complete lines: all but a final unterminated one.
  , parsedNodes :: !(Maybe [Node])
  -- ^ Top-level blocks, or 'Nothing' if commonmark failed.
  }

-- | A block with its source lines and children.
data Node = Node
  { nodeLines :: !(Int, Int)
  -- ^ First and last line, 1-based.
  , nodeBlock :: !(Maybe Block)
  -- ^ 'Nothing' for link reference definitions and HTML comments, which
  -- render nothing.
  , nodeRefs :: !Refs
  -- ^ Link reference definitions inside the block.
  , nodeShape :: !Shape
  }
  deriving (Show)

-- | What a reparse starting inside a block needs to know about it.
data Shape
  = -- | A paragraph or its leading link reference definitions; the next line
    -- may continue it.
    Para
  | -- | Code including the final line ending.
    CodeLines !Text
  | -- | Each list item's blocks. They carry the item's line range.
    Items ![[Node]]
  | -- | A block quote's blocks.
    Quote ![Node]
  | Other
  deriving (Show)

-- | Link reference definitions: normalised label to destination and title.
-- The first definition of a label wins, matching left-biased 'M.union'.
type Refs = Map Text (Text, Text)

-- | Parse a text, given the link reference definitions from the text before
-- it. Raw HTML is kept as text, minus comments.
parseLines :: Refs -> Text -> Parsed
parseLines before src = Parsed unix (T.count "\n" unix) nodes
  where
    -- Normalise to @\\n@ so line numbers match commonmark's and code has no
    -- @\\r@. Terminate the last line: otherwise commonmark drops a final
    -- table row and adds an empty paragraph after a heading or closing fence.
    -- 'splitLines' accepts the same line endings.
    unix = unixLines src
    text = if "\n" `T.isSuffixOf` unix then unix else T.snoc unix '\n'
    nodes = case runIdentity (C.commonmarkWith syntax "" text) of
      Right (Blocks ns) -> Just ns
      Left _ -> Nothing
    syntax = extensions <> mempty {C.syntaxFinalParsers = [defined before]}

-- | Normalise @\\r\\n@ and @\\r@ line endings to @\\n@.
unixLines :: Text -> Text
unixLines = T.replace "\r" "\n" . T.replace "\r\n" "\n"

-- | CommonMark with GitHub extensions, built once rather than per parse. Task
-- items must precede core list items and tables follow core blocks.
extensions :: C.SyntaxSpec Identity Spans Blocks
extensions = taskListSpec <> strikethroughSpec <> autolinkSpec <> C.defaultSyntaxSpec <> pipeTableSpec

-- | Merge definitions from earlier text into the reference map, taking
-- priority, before commonmark parses inlines.
defined :: Monad m => Refs -> BlockParser m Spans Blocks Blocks
defined before = mempty <$ updateState add
  where
    add st = st {referenceMap = ReferenceMap (M.unionWith (++) refs (unReferenceMap (referenceMap st)))}
    refs = M.map (\(url, title) -> [toDyn (LinkInfo url title [] Nothing)]) before

-- | Split after the first @n@ lines, each ended by @\\n@, @\\r\\n@ or
-- @\\r@.
splitLines :: Int -> Text -> (Text, Text)
splitLines n t = (T.dropEnd (T.length rest) t, rest)
  where
    rest = go n t
    go k s
      | k <= 0 = s
      | otherwise = go (k - 1) (fromMaybe (T.drop 1 r) (T.stripPrefix "\r\n" r))
      where
        r = T.dropWhile (\c -> c /= '\n' && c /= '\r') s

newtype Blocks = Blocks [Node]
  deriving newtype (Semigroup, Monoid, Show)

-- | One block. Its lines are filled in by 'C.ranged' after construction.
node :: Maybe Block -> Refs -> Shape -> Blocks
node b refs shape = Blocks [Node (0, 0) b refs shape]

leaf :: Block -> Blocks
leaf b = node (Just b) mempty Other

blocks :: Blocks -> [Block]
blocks (Blocks ns) = mapMaybe nodeBlock ns

refsIn :: Blocks -> Refs
refsIn (Blocks ns) = foldMap nodeRefs ns

-- | Difference list, since the inline parser appends spans one at a time.
newtype Spans = Spans (Endo [Span])
  deriving newtype (Semigroup, Monoid)

instance Show Spans where
  showsPrec d = showsPrec d . spans

-- | The spans with adjacent 'Str's merged.
spans :: Spans -> [Span]
spans (Spans f) = go (appEndo f [])
  where
    go xs = case span isStr xs of
      ([], []) -> []
      ([], x : rest) -> x : go rest
      (strs, rest) -> [Str t | let t = T.concat [s | Str s <- strs], not (T.null t)] ++ go rest
    isStr = \case
      Str _ -> True
      _ -> False

one :: Span -> Spans
one x = Spans (Endo (x :))

wrap :: ([Span] -> Span) -> Spans -> Spans
wrap f = one . f . spans

-- | Drop one trailing line ending.
chomp :: Text -> Text
chomp t = fromMaybe t (T.stripSuffix "\n" t)

-- | Strip HTML comments, as a browser hides them. An unterminated comment
-- runs to the end.
uncomment :: Text -> Text
uncomment t = case T.breakOn "<!--" t of
  (before, "") -> before
  (before, comment) -> before <> uncomment (T.drop 3 (snd (T.breakOn "-->" (T.drop 4 comment))))

-- | A list whose items may carry task check boxes.
listOf :: C.ListType -> C.ListSpacing -> [(Maybe Bool, Blocks)] -> Blocks
listOf ty spacing items =
  node
    (Just (List kind (spacing == C.TightList) [ListItem task (blocks bs) | (task, bs) <- items]))
    (refsIn (foldMap snd items))
    (Items [ns | (_, Blocks ns) <- items])
  where
    kind = case ty of
      C.BulletList c -> Bullet c
      C.OrderedList start _ delim -> Ordered start (if delim == C.Period then '.' else ')')

instance C.Rangeable Blocks where
  -- Setext headings and tables list their defining line first, hence the
  -- minimum. End positions are exclusive. commonmark ranges an item's blocks
  -- again with the item's range, overwriting their own. Leading link
  -- reference definitions keep their own lines (their second range is
  -- empty), and the rest of the paragraph starts on the first definition's
  -- line, so a reparse never starts between them.
  ranged (C.SourceRange r) (Blocks ns) = case r of
    [] -> Blocks ns
    _ -> Blocks [n {nodeLines = (first, lst)} | n <- ns]
    where
      first = minimum (map (C.sourceLine . fst) r)
      lst = maximum [C.sourceLine end - (if C.sourceColumn end == 1 then 1 else 0) | (_, end) <- r]

instance C.Rangeable Spans where
  ranged _ = id

instance C.HasAttributes Blocks where
  addAttributes _ = id

instance C.HasAttributes Spans where
  addAttributes _ = id

instance C.IsBlock Spans Blocks where
  paragraph s = node (Just (Paragraph (spans s))) mempty Para
  plain = C.paragraph
  thematicBreak = leaf ThematicBreak
  blockQuote bs@(Blocks ns) = node (Just (BlockQuote (blocks bs))) (refsIn bs) (Quote ns)
  codeBlock info t = node (Just (CodeBlock info (chomp t))) mempty (CodeLines t)
  heading level = leaf . Heading level . spans
  rawBlock _ t = node (if T.all isSpace html then Nothing else Just (Paragraph [Str html])) mempty Other
    where
      html = chomp (uncomment t)
  referenceLinkDefinition label (!url, !title) =
    node Nothing (M.singleton (T.toCaseFold (T.unwords (T.words label))) (url, title)) Para
  list ty spacing = listOf ty spacing . map (Nothing,)

instance HasTaskList Spans Blocks where
  taskList ty spacing = listOf ty spacing . map (\(done, bs) -> (Just done, bs))

instance HasPipeTable Spans Blocks where
  pipeTable aligns header rows = leaf (Table (map align aligns) (map spans header) (map (map spans) rows))
    where
      align = \case
        LeftAlignedCol -> CellLeft
        CenterAlignedCol -> CellCenter
        RightAlignedCol -> CellRight
        DefaultAlignedCol -> CellDefault

instance C.IsInline Spans where
  lineBreak = one HardBreak
  softBreak = one SoftBreak
  str = one . Str
  entity t = one (Str (fromMaybe t (lookupEntity (T.drop 1 t))))
  escapedChar = one . Str . T.singleton
  emph = wrap Emph
  strong = wrap Strong
  link url title = wrap (Link url title)
  image src title = wrap (Image src title)
  code = one . Code
  rawInline _ t
    | "<!--" `T.isPrefixOf` t = mempty
    | otherwise = one (Str t)

instance HasStrikethrough Spans where
  strikethrough = wrap Strike
