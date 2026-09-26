-- | Markdown text to "NanoUI.Markdown.Syntax", parsed by the @commonmark@
-- library with GitHub's tables, task lists, strikethrough and bare links from
-- @commonmark-extensions@, and what "NanoUI.Markdown.Document" needs to parse
-- again only the end of a text: the lines each block is on, what is inside
-- it, and the link reference definitions.
module NanoUI.Markdown.Internal.Parse
  ( Parsed (..)
  , Node (..)
  , Shape (..)
  , Refs
  , parseLines
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
import Data.Functor.Identity (runIdentity)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid (Endo (..))
import Data.Text (Text)
import Data.Text qualified as T
import NanoUI.Markdown.Syntax
import Text.Parsec (updateState)

-- | A text, parsed.
data Parsed = Parsed
  { parsedText :: !Text
  -- ^ The text with its line endings made @\\n@, as parsed.
  , parsedComplete :: !Int
  -- ^ How many of its lines are complete: all but a last one without a line
  -- ending.
  , parsedNodes :: !(Maybe [Node])
  -- ^ Its top-level blocks, or 'Nothing' if commonmark failed.
  }

-- | A block, with the lines it is on and what is inside it.
data Node = Node
  { nodeLines :: !(Int, Int)
  -- ^ Its first and last line, counted from 1.
  , nodeBlock :: !(Maybe Block)
  -- ^ The block, or 'Nothing' for link reference definitions and HTML
  -- comments, which show nothing.
  , nodeRefs :: !Refs
  -- ^ The link reference definitions in it.
  , nodeShape :: !Shape
  }
  deriving (Show)

-- | What parsing again from inside a block needs to know about it.
data Shape
  = -- | A paragraph, or the link reference definitions a paragraph starts
    -- with: a line right under it can continue it.
    Para
  | -- | Code with its line endings, the last line's too.
    CodeLines !Text
  | -- | A list's items, each its blocks, which have the item's lines.
    Items ![[Node]]
  | -- | A block quote's blocks.
    Quote ![Node]
  | Other
  deriving (Show)

-- | Link reference definitions: each label, as commonmark compares them, with
-- its destination and title. The first definition of a label wins, which is
-- what the union of two maps keeps.
type Refs = Map Text (Text, Text)

-- | Parse a text, with the link reference definitions of the text before it.
-- Raw HTML stays text, without its comments.
parseLines :: Refs -> Text -> Parsed
parseLines before src = Parsed unix (T.count "\n" unix) nodes
  where
    -- Line endings become @\\n@, which numbers the lines as commonmark does
    -- and keeps @\\r@ out of code, and the last line gets one: without it
    -- commonmark ends no table row there, and adds an empty paragraph after a
    -- heading or a closing fence. 'splitLines' knows the same line endings.
    unix = T.replace "\r" "\n" (T.replace "\r\n" "\n" src)
    text = if "\n" `T.isSuffixOf` unix then unix else T.snoc unix '\n'
    nodes = case runIdentity (C.commonmarkWith syntax "" text) of
      Right (Blocks ns) -> Just ns
      Left _ -> Nothing
    -- Task items before the core list items, tables after the core blocks.
    syntax =
      taskListSpec <> strikethroughSpec <> autolinkSpec <> C.defaultSyntaxSpec <> pipeTableSpec
        <> mempty {C.syntaxFinalParsers = [defined before]}

-- | Adds link reference definitions made before the text to the text's own,
-- ahead of them, before commonmark parses the inline text that uses them.
defined :: Monad m => Refs -> BlockParser m Spans Blocks Blocks
defined before = mempty <$ updateState add
  where
    add st = st {referenceMap = ReferenceMap (M.unionWith (++) refs (unReferenceMap (referenceMap st)))}
    refs = M.map (\(url, title) -> [toDyn (LinkInfo url title [] Nothing)]) before

-- | A text split after its first @n@ lines, each ended by @\\n@, @\\r\\n@ or
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

-- | Blocks as commonmark builds them.
newtype Blocks = Blocks [Node]
  deriving newtype (Semigroup, Monoid, Show)

-- | One block. commonmark gives each block its lines with 'C.ranged' right
-- after making it.
node :: Maybe Block -> Refs -> Shape -> Blocks
node b refs shape = Blocks [Node (0, 0) b refs shape]

leaf :: Block -> Blocks
leaf b = node (Just b) mempty Other

blocks :: Blocks -> [Block]
blocks (Blocks ns) = mapMaybe nodeBlock ns

refsIn :: Blocks -> Refs
refsIn (Blocks ns) = foldMap nodeRefs ns

-- | Spans as a difference list: the inline parser adds them to the end one
-- at a time.
newtype Spans = Spans (Endo [Span])
  deriving newtype (Semigroup, Monoid)

instance Show Spans where
  showsPrec d = showsPrec d . spans

-- | The spans, adjacent text joined.
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

-- | Code without the line ending after its last line.
chomp :: Text -> Text
chomp t = fromMaybe t (T.stripSuffix "\n" t)

-- | Raw HTML without its comments, which a browser would not show. A comment
-- that does not end runs to the end.
uncomment :: Text -> Text
uncomment t = case T.breakOn "<!--" t of
  (before, "") -> before
  (before, comment) -> before <> uncomment (T.drop 3 (snd (T.breakOn "-->" (T.drop 4 comment))))

-- | A list; its items with their task check boxes.
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
  -- A setext heading or a table lists the line that made it first, so the
  -- first line is the least one. A range ends before its end position. An
  -- item's blocks get the item's lines after their own. The link reference
  -- definitions a paragraph starts with get their own lines, and then none;
  -- the rest of the paragraph starts on their first line, where the
  -- paragraph does, so the rest of a text never starts between them.
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
