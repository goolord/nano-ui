-- | An incrementally parsed Markdown document, for chat messages that arrive
-- a few tokens at a time.
--
-- Appending gives the same blocks as parsing the whole text:
--
-- > markdownBlocks (appendMarkdown b (parseMarkdown a)) == markdownBlocks (parseMarkdown (a <> b))
--
-- The document keeps the blocks that appended text cannot change and reparses
-- only the rest. The rest starts at the last complete line that parses the
-- same way on its own: a top-level block not directly under a paragraph, or,
-- inside a top-level block, a list item after the first, a table row, a line
-- of fenced code (reparsed after the table header or the fence), or a block
-- quote's block after the first that is not directly under a paragraph. So an
-- append costs the new text plus the last list item, table row, code line or
-- quote block, or otherwise the last block.
--
-- The rest is parsed with the link reference definitions before it. If the
-- rest's new definitions differ from those the closed blocks were parsed
-- with, the whole text is reparsed, since those blocks may use them. This
-- happens on every append while a message ends in a definition.
module NanoUI.Markdown.Document
  ( MarkdownDoc
  , emptyMarkdown
  , parseMarkdown
  , appendMarkdown
  , markdownBlocks
  , markdownSource
  , markdownImages
  , parseMarkdownBlocks
  ) where

import Control.DeepSeq (NFData, deepseq, rnf)
import Control.Monad (guard)
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (toList)
import Data.List (find)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import GHC.Generics (Generic)
import NanoUI.Markdown.Internal.Parse
import NanoUI.Markdown.Syntax

-- | A parsed document. It keeps the blocks that appending cannot change and
-- reparses the rest on append. Documents are equal when their texts are.
data MarkdownDoc = MarkdownDoc
  { docLength :: !Int
  -- ^ Length of the text.
  , docDone :: ![Text]
  -- ^ Text of the closed blocks, in pieces, newest first.
  , docClosed :: !(Seq Block)
  -- ^ The closed blocks.
  , docOpen :: !Open
  -- ^ The block the rest starts inside, up to where the rest begins.
  , docKnown :: !Refs
  -- ^ Link reference definitions before the rest.
  , docPending :: !Refs
  -- ^ Definitions in the rest whose labels are not defined before it. The
  -- closed blocks were parsed with these.
  , docRest :: !Text
  -- ^ The text after the closed blocks.
  , docLast :: ![Block]
  -- ^ Blocks of the rest; the first continues 'docOpen'.
  }

instance Eq MarkdownDoc where
  a == b = docLength a == docLength b && TL.fromChunks (pieces a) == TL.fromChunks (pieces b)

instance Show MarkdownDoc where
  showsPrec d doc = showParen (d > 10) (showString "parseMarkdown " . showsPrec 11 (markdownSource doc))

-- | The text of a document, in pieces.
pieces :: MarkdownDoc -> [Text]
pieces doc = reverse (docRest doc : docDone doc)

-- | The part of a block before the rest, when the rest starts inside it.
data Open
  = -- | The rest starts at a block boundary.
    Fresh
  | -- | A list's items so far, and whether they keep it tight.
    InList !ListType !Bool ![ListItem]
  | -- | A block quote's blocks.
    InQuote ![Block]
  | -- | A fenced code block's opening fence line, and its code so far with line endings.
    InCode !Text !Text
  | -- | A table's header and delimiter lines, and its rows so far.
    InTable !Text ![[[Span]]]
  deriving (Generic)

instance NFData Open

-- | Lines prepended to the rest when parsing it, to restart the block.
reopen :: Open -> Text
reopen = \case
  InCode fence _ -> fence
  InTable header _ -> header
  _ -> ""

-- | A document with no text.
emptyMarkdown :: MarkdownDoc
emptyMarkdown = MarkdownDoc 0 [] Seq.empty Fresh mempty mempty "" []

-- | Parse a whole document. Keep the result in your model rather than
-- parsing the text again every frame.
parseMarkdown :: Text -> MarkdownDoc
parseMarkdown t = fromMaybe unparsed (resume new t)
  where
    new = emptyMarkdown {docLength = T.length t}
    -- If commonmark fails, show the text as is.
    unparsed = new {docRest = t, docLast = [Paragraph [Str (chomp (unixLines t))]]}

-- | The blocks of a document.
markdownBlocks :: MarkdownDoc -> [Block]
markdownBlocks doc = toList (docClosed doc) ++ docLast doc

-- | The document's full text, which '==' compares.
markdownSource :: MarkdownDoc -> Text
markdownSource = T.concat . pieces

-- | Image sources in the document, without duplicates, in order of first
-- appearance, including those in quotes, lists, tables and links. Useful for
-- loading images before they scroll into view.
markdownImages :: MarkdownDoc -> [Text]
markdownImages = nubOrd . concatMap blockImages . markdownBlocks
  where
    blockImages = \case
      Paragraph xs -> spanImages xs
      Heading _ xs -> spanImages xs
      BlockQuote bs -> concatMap blockImages bs
      List _ _ items -> concatMap (concatMap blockImages . itemBlocks) items
      Table _ header rows -> concatMap spanImages (header ++ concat rows)
      _ -> []
    spanImages = concatMap $ \case
      Image src _ alt -> src : spanImages alt
      Emph xs -> spanImages xs
      Strong xs -> spanImages xs
      Strike xs -> spanImages xs
      Link _ _ xs -> spanImages xs
      _ -> []

-- | The blocks of a text: @'markdownBlocks' . 'parseMarkdown'@.
parseMarkdownBlocks :: Text -> [Block]
parseMarkdownBlocks = markdownBlocks . parseMarkdown

-- | Append text to a document. This costs the new text plus the rest after
-- the closed blocks (see "NanoUI.Markdown.Document"), or the whole text when
-- the rest changes a link reference definition. Append a frame's tokens in
-- one call rather than one at a time.
appendMarkdown :: Text -> MarkdownDoc -> MarkdownDoc
appendMarkdown new doc
  | T.null new = doc
  | otherwise =
      fromMaybe
        (parseMarkdown (markdownSource doc <> new))
        (resume doc {docLength = docLength doc + T.length new} (docRest doc <> new))

-- | Parse a new rest after the document's closed blocks. 'Nothing' means the
-- whole text must be reparsed: the rest changed a link reference definition
-- the closed blocks were parsed with, a continued table outgrew 'fits', or
-- commonmark failed.
resume :: MarkdownDoc -> Text -> Maybe MarkdownDoc
resume doc rest = do
  let open = docOpen doc
      reopened = reopen open
      skip = T.count "\n" reopened
      Parsed input complete parsed = parseLines (docKnown doc) (reopened <> rest)
  fresh <- parsed
  rnf (mapMaybe nodeBlock fresh) `seq` pure ()
  let freshRefs = foldMap nodeRefs fresh
      refs = freshRefs `M.difference` docKnown doc
  guard (null (docDone doc) || refs == docPending doc)
  nodes <- case (fresh, open) of
    (n : ns, _) -> (: ns) <$> continue open n
    ([], Fresh) -> Just []
    _ -> Nothing
  let usable (Cut line _ _ open') = line <= complete && isJust open'
      after (Cut line _ _ _) = line > skip + 1
  pure $ case find usable (takeWhile after (cuts input open nodes)) of
    Just (Cut line k closingRefs (Just open')) ->
      let (closing, opened) = splitAt k nodes
          known = docKnown doc <> foldMap nodeRefs closing <> closingRefs
          (done, rest') = splitLines (line - skip - 1) rest
       in open' `deepseq`
            doc
              { docDone = done `seq` done : docDone doc
              , docClosed = docClosed doc <> Seq.fromList (mapMaybe nodeBlock closing)
              , docOpen = open'
              , docKnown = known
              , docPending = freshRefs `M.difference` known
              , docRest = rest'
              , docLast = spine (mapMaybe nodeBlock opened)
              }
    _ -> doc {docRest = rest, docPending = refs, docLast = spine (mapMaybe nodeBlock nodes)}

-- | Force a list's spine and elements so it retains nothing of the parse.
spine :: [a] -> [a]
spine xs = foldr seq () xs `seq` xs

-- | Join the rest's first block onto the open block's earlier part. 'Nothing'
-- if a continued table outgrows 'fits', or if the block does not continue the
-- open one (which should not happen).
continue :: Open -> Node -> Maybe Node
continue open n = case (open, nodeBlock n, nodeShape n) of
  (Fresh, _, _) -> Just n
  (InList ty tight items, Just (List _ tight' items'), _) -> joined (List ty (tight && tight') (items ++ items'))
  (InQuote bs, Just (BlockQuote bs'), _) -> joined (BlockQuote (bs ++ bs'))
  (InCode _ code, Just (CodeBlock info _), CodeLines more) -> joined (CodeBlock info (chomp (code <> more)))
  (InTable _ rows, Just (Table aligns header rows'), _)
    | allRows <- rows ++ rows', fits aligns allRows -> joined (Table aligns header allRows)
  _ -> Nothing
  where
    joined b = b `seq` Just n {nodeBlock = Just b}

-- | Whether commonmark-extensions keeps all of a table's rows. It ends a table
-- after filling in 200000 missing cells, and a table parsed in parts would
-- count those per part.
fits :: [CellAlign] -> [[[Span]]] -> Bool
fits aligns rows = length rows * length aligns <= 200000

-- | A line where the rest can start.
data Cut
  = Cut
      !Int
      -- ^ Line number in the parsed text.
      !Int
      -- ^ Number of blocks closed before the line.
      Refs
      -- ^ Link reference definitions in the next block, before the line.
      (Maybe Open)
      -- ^ The next block's part before the line, or 'Nothing' if the rest
      -- cannot start there.

-- | Lines where the rest can start, last first: at each block after the
-- first, and inside blocks.
cuts :: Text -> Open -> [Node] -> [Cut]
cuts input open nodes =
  concat
    [ inside input (if k == 0 then open else Fresh) k n ++ [Cut (start n) k mempty (Just Fresh) | k > 0, opens prev n]
    | (k, prev, n) <- reverse (withPrev nodes)
    ]

-- | Lines inside a block where the rest can start, last first: a list item
-- after the first, a fenced code line, a table row, or a block quote's block
-- after the first. The block continues @before@.
inside :: Text -> Open -> Int -> Node -> [Cut]
inside input before k n = case (nodeBlock n, nodeShape n) of
  (Just (List ty _ items), Items own) ->
    [ Cut line k (foldMap nodeRefs (concat (take i own))) $ do
        tight <- tightTo line
        Just (InList ty (tight && tightBefore) (take (length items - length own + i) items))
    | (i, Node {nodeLines = (line, _)} : _) <- reverse (drop 1 (zip [0 ..] own))
    ]
  (Just CodeBlock {}, CodeLines code)
    -- Fenced code spans more lines than its code, counting the fences.
    | size <- T.count "\n" code
    , end - first >= size ->
        [ Cut line k mempty (Just (InCode (linesAt first 1) (codeBefore <> fst (splitLines (line - first - 1) code))))
        | line <- [first + size, first + size - 1 .. first + 1]
        ]
  (Just (Table aligns _ rows), _)
    | fits aligns rows ->
        [ Cut line k mempty (Just (InTable (linesAt first 2) (take (length rows - (end - line + 1)) rows)))
        | line <- [end, end - 1 .. first + 2]
        ]
  (Just (BlockQuote bs), Quote own) ->
    [ Cut (start m) k (foldMap nodeRefs (take j own)) (Just (InQuote (take (length bs - length (mapMaybe nodeBlock (drop j own))) bs)))
    | (j, prev, m) <- reverse (withPrev own)
    , j > 0
    , opens prev m
    ]
  _ -> []
  where
    (first, end) = nodeLines n
    linesAt from count = fst (splitLines count (snd (splitLines (from - 1) input)))
    -- Whether the items before the one on @line@ keep the list tight. Parsed
    -- up to that line, the list ends with that item, one line long.
    tightTo line = case parsedNodes (parseLines mempty (fst (splitLines line input))) >>= listToMaybe . reverse of
      Just Node {nodeBlock = Just (List _ tight _)} -> Just tight
      _ -> Nothing
    tightBefore = case before of
      InList _ tight _ -> tight
      _ -> True
    codeBefore = case before of
      InCode _ code -> code
      _ -> ""

-- | Whether a block parses the same on its own, given the block before it, so
-- the rest can start there. A line directly under a paragraph may continue
-- it, turn it into a heading (@===@, @---@) or a table header (a delimiter
-- row), or fail to interrupt it (an ordered item not numbered 1, an empty
-- task item, HTML of type 7). So the paragraph must end before the line
-- above. Any other previous block has ended by then, or is a list, which only
-- an item continues.
opens :: Maybe Node -> Node -> Bool
opens prev n = case prev of
  Just Node {nodeShape = Para, nodeLines = (_, end)} -> end < start n - 1
  _ -> True

start :: Node -> Int
start = fst . nodeLines

-- | Blocks numbered from 0, each paired with the one before it.
withPrev :: [Node] -> [(Int, Maybe Node, Node)]
withPrev ns = zip3 [0 ..] (Nothing : map Just ns) ns
