-- | A parsed Markdown document that text can be appended to, for a chat
-- message arriving a few tokens at a time.
--
-- Appending gives the blocks that parsing the whole text would:
--
-- > markdownBlocks (appendMarkdown b (parseMarkdown a)) == markdownBlocks (parseMarkdown (a <> b))
--
-- A document keeps the blocks that appended text cannot change, and parses
-- again only the rest of its text, from the last complete line that starts
-- the same way parsed alone: a top-level block that is not right under a
-- paragraph, or, inside a top-level block, an item of a list after the first,
-- a row of a table or a line of fenced code (parsed again after the table's
-- header or the fence), or a block of a block quote after the first that is
-- not right under a paragraph. So an append costs the new text and that
-- rest: the last list item, table row, line of code or block of a quote when
-- the text ends in one, or else the last block.
--
-- The rest is parsed with the link reference definitions before it. The
-- whole text is parsed again when the rest defines a label the closed blocks
-- were not parsed with, or defines it otherwise, as they may use it: at each
-- append to a definition at the end of a message.
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

-- | A document: its text, the blocks that appending to it cannot change,
-- and the rest, parsed again when text is appended. Two documents are equal
-- when their texts are.
data MarkdownDoc = MarkdownDoc
  { docLength :: !Int
  -- ^ The length of the text.
  , docDone :: ![Text]
  -- ^ The text of the blocks that cannot change, in pieces, the last first.
  , docClosed :: !(Seq Block)
  -- ^ Those blocks.
  , docOpen :: !Open
  -- ^ The block that the rest starts inside, up to the rest.
  , docKnown :: !Refs
  -- ^ The link reference definitions before the rest.
  , docPending :: !Refs
  -- ^ Those in the rest with a label not defined before it, which the closed
  -- blocks were parsed with.
  , docRest :: !Text
  -- ^ The rest of the text.
  , docLast :: ![Block]
  -- ^ Its blocks, the first continuing 'docOpen'.
  }

instance Eq MarkdownDoc where
  a == b = docLength a == docLength b && TL.fromChunks (pieces a) == TL.fromChunks (pieces b)

instance Show MarkdownDoc where
  showsPrec d doc = showParen (d > 10) (showString "parseMarkdown " . showsPrec 11 (T.concat (pieces doc)))

-- | The text of a document, in pieces.
pieces :: MarkdownDoc -> [Text]
pieces doc = reverse (docRest doc : docDone doc)

-- | The part of a block before the rest of the text, which starts inside
-- it.
data Open
  = -- | None: the rest starts with a block.
    Fresh
  | -- | A list's items, and whether they keep it tight.
    InList !ListType !Bool ![ListItem]
  | -- | A block quote's blocks.
    InQuote ![Block]
  | -- | A fenced code block's first line, and its code, with line endings.
    InCode !Text !Text
  | -- | A table's header and delimiter rows, and its rows.
    InTable !Text ![[[Span]]]
  deriving (Generic)

instance NFData Open

-- | The lines the rest is parsed after, which start the block again.
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
    -- If commonmark failed, the text would show as it is.
    unparsed = new {docRest = t, docLast = [Paragraph [Str (chomp (parsedText (parseLines mempty t)))]]}

-- | The blocks of a document.
markdownBlocks :: MarkdownDoc -> [Block]
markdownBlocks doc = toList (docClosed doc) ++ docLast doc

-- | A document's text, parsed or appended: what '==' compares.
markdownSource :: MarkdownDoc -> Text
markdownSource = T.concat . pieces

-- | The sources of a document's images, each once, in the order they first
-- appear, in quotes, lists, tables and links as well: for loading them
-- before they come into view.
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

-- | Add text to the end of a document. It costs the new text and the rest
-- of the document after its closed blocks (see "NanoUI.Markdown.Document"),
-- or the whole text when the rest changes a link reference definition. Append
-- the tokens that arrived in a frame at once, rather than one at a time.
appendMarkdown :: Text -> MarkdownDoc -> MarkdownDoc
appendMarkdown new doc
  | T.null new = doc
  | otherwise =
      fromMaybe
        (parseMarkdown (T.concat (pieces doc) <> new))
        (resume doc {docLength = docLength doc + T.length new} (docRest doc <> new))

-- | A document with a new rest, parsed after its closed blocks, or 'Nothing'
-- if the whole text must be parsed again: when the rest changes a link
-- reference definition that the closed blocks were parsed with, as they may
-- use it, when a table it continues gets too big for 'fits', or when
-- commonmark fails.
resume :: MarkdownDoc -> Text -> Maybe MarkdownDoc
resume doc rest = do
  let open = docOpen doc
      reopened = reopen open
      skip = T.count "\n" reopened
      Parsed input complete parsed = parseLines (docKnown doc) (reopened <> rest)
  fresh <- parsed
  rnf (mapMaybe nodeBlock fresh) `seq` pure ()
  let refs = foldMap nodeRefs fresh `M.difference` docKnown doc
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
              , docPending = foldMap nodeRefs fresh `M.difference` known
              , docRest = rest'
              , docLast = spine (mapMaybe nodeBlock opened)
              }
    _ -> doc {docRest = rest, docPending = refs, docLast = spine (mapMaybe nodeBlock nodes)}

-- | A list with its spine and elements evaluated, so that it holds on to no
-- parse.
spine :: [a] -> [a]
spine xs = foldr seq () xs `seq` xs

-- | The first block of the rest, which starts inside the open block,
-- continuing it: 'Nothing' if it does not, which it always does, or if the
-- table it continues gets too big for 'fits'.
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

-- | Whether a table is small enough that commonmark-extensions keeps all its
-- rows. It ends a table once it has filled in 200000 missing cells, which
-- a table parsed in parts would count apart.
fits :: [CellAlign] -> [[[Span]]] -> Bool
fits aligns rows = length rows * length aligns <= 200000

-- | A line the rest of a text can start on.
data Cut
  = Cut
      !Int
      -- ^ The line, in the parsed text.
      !Int
      -- ^ How many of the text's blocks close before it.
      Refs
      -- ^ The link reference definitions in the next block before the line.
      (Maybe Open)
      -- ^ That block's part before the line, if the rest can start there.

-- | Where the rest of a parsed text can start, the last line first: where
-- a block after the first starts, and inside a block.
cuts :: Text -> Open -> [Node] -> [Cut]
cuts input open nodes =
  concat
    [ inside input (if k == 0 then open else Fresh) k n ++ [Cut (start n) k mempty (Just Fresh) | k > 0, opens prev n]
    | (k, prev, n) <- reverse (withPrev nodes)
    ]

-- | Where the rest can start inside a block, the last line first: at a list
-- item after the first, a line of fenced code, a table row, or a block
-- after the first in a block quote, in a parsed text. The block continues
-- @before@.
inside :: Text -> Open -> Int -> Node -> [Cut]
inside input before k n = case (nodeBlock n, nodeShape n) of
  (Just (List ty _ items), Items own) ->
    [ Cut line k (foldMap nodeRefs (concat (take i own))) $ do
        tight <- tightTo line
        Just (InList ty (tight && tightBefore) (take (length items - length own + i) items))
    | (i, Node {nodeLines = (line, _)} : _) <- reverse (drop 1 (zip [0 ..] own))
    ]
  (Just CodeBlock {}, CodeLines code)
    -- Fenced code is on more lines than its code: the fence's.
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
    -- Whether a list's items before the one on a line keep it tight: parsed
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

-- | Whether a block starts as it would alone, after the block before it:
-- the rest of a text can start there. A line right under a paragraph can
-- continue it, underline it into a heading (@===@, @---@), make it a table's
-- header (a delimiter row), or fail to interrupt it (an ordered item not
-- numbered 1, a task item with nothing after its box, HTML of type 7), so
-- a paragraph must end before the line above. Any other block before it
-- has ended by then, or is a list, which only an item continues.
opens :: Maybe Node -> Node -> Bool
opens prev n = case prev of
  Just Node {nodeShape = Para, nodeLines = (_, end)} -> end < start n - 1
  _ -> True

start :: Node -> Int
start = fst . nodeLines

-- | Blocks counted from 0, each with the one before it.
withPrev :: [Node] -> [(Int, Maybe Node, Node)]
withPrev ns = zip3 [0 ..] (Nothing : map Just ns) ns
