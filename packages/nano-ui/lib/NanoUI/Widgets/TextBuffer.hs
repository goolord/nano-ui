{-# LANGUAGE BangPatterns #-}

-- | A text document as a finger tree of lines with a cursor. Every change is
-- a 'TextEdit': replace the text at a position with other text. An edit
-- touches only the lines it spans, so edits, cursor moves and line lookups
-- cost O(log lines) plus the size of the lines involved, however long the
-- document is.
module NanoUI.Widgets.TextBuffer
  ( -- * Types
    TextBuffer (..)
  , Cursor (..)
  , TextEdit (..)

    -- * Construction & Conversion
  , empty
  , fromText
  , fromLines
  , toText
  , splitLines
  , joinLines
  , toLines
  , lineAt

    -- * Cursor & Metrics
  , getCursor
  , getLineCount
  , withCursor
  , clampCursor
  , changedLines
  , markLinesSeen

    -- * Navigation
  , moveLeft
  , moveRight
  , moveUp
  , moveDown
  , moveToBOL
  , moveToEOL
  , moveToTop
  , moveToBottom
  , moveWordLeft
  , moveWordRight

    -- * Selection
  , selectionRange
  , selectedText
  , textRange
  , documentEnd

    -- * Edits
  , applyEdit
  , invertEdit
  , replaceEdit
  , insertableText
  )
where

import Control.Monad (when)
import Control.Monad.ST (runST)
import Data.Char (isPrint, isSpace)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Array qualified as A
import Data.Text.Internal (Text (..))

-- | Zero-indexed logical (row, column) position in the buffer. Fields are
-- row then column, so the derived 'Ord' is document order.
data Cursor = Cursor
  { cursorRow :: {-# UNPACK #-} !Int
  , cursorCol :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Ord, Show)

-- | Lines (never empty, and without their newlines) and the cursor.
data TextBuffer = TextBuffer
  { bufferLines :: !(Seq Text)
  , bufferCursor :: {-# UNPACK #-} !Cursor
  , preferredCol :: {-# UNPACK #-} !Int
  -- ^ The column vertical motion aims for, kept while moving through
  -- shorter lines.
  , bufferSeenHead :: {-# UNPACK #-} !Int
  , bufferSeenTail :: {-# UNPACK #-} !Int
  -- ^ Lines at the start and the end that no edit has touched since
  -- 'markLinesSeen', so what was derived from them (their measured widths)
  -- still holds. See 'changedLines'.
  }
  deriving (Eq, Show)

-- | Replace 'editRemoved' at 'editAt' with 'editInserted'. Both texts may
-- span lines. An edit carries what it removes, so it can be inverted without
-- looking at the document.
data TextEdit = TextEdit
  { editAt :: {-# UNPACK #-} !Cursor
  , editRemoved :: !Text
  , editInserted :: !Text
  }
  deriving (Eq, Show)

-- | A TextBuffer containing a single blank line.
empty :: TextBuffer
empty = TextBuffer (Seq.singleton T.empty) (Cursor 0 0) 0 0 0

-- | Construct a TextBuffer from raw Text. Cursor is always (0, 0).
fromText :: Text -> TextBuffer
fromText = fromLines . splitLines

-- | A buffer over lines already split, at (0, 0), with every line unseen.
-- The lines are as 'splitLines' makes them: at least one, none holding a
-- newline.
fromLines :: Seq Text -> TextBuffer
fromLines lns
  | Seq.null lns = empty
  | otherwise = TextBuffer lns (Cursor 0 0) 0 0 0

-- | The lines of a text, without their newlines. There is always at least one.
splitLines :: Text -> Seq Text
splitLines = Seq.fromList . T.splitOn "\n"

-- | All lines joined with newlines, copied once into a new text.
toText :: TextBuffer -> Text
toText = joinLines . bufferLines

-- | Lines joined with newlines, copied once into a new text.
joinLines :: Seq Text -> Text
joinLines lns =
  let !total = foldl' (\acc (Text _ _ len) -> acc + len + 1) (-1) lns
   in if total <= 0
        then T.empty
        else runST $ do
          dest <- A.new total
          let copyLine (Text arr start len) next !off = do
                A.copyI len dest off arr start
                when (off + len < total) $ A.unsafeWrite dest (off + len) 10
                next (off + len + 1)
          foldr copyLine (\_ -> pure ()) lns 0
          frozen <- A.unsafeFreeze dest
          pure (Text frozen 0 total)

toLines :: TextBuffer -> [Text]
toLines = toList . bufferLines

-- | The text of a row, or empty outside the document.
lineAt :: Int -> TextBuffer -> Text
lineAt row buf = fromMaybe T.empty (Seq.lookup row (bufferLines buf))

getCursor :: TextBuffer -> Cursor
getCursor = bufferCursor

getLineCount :: TextBuffer -> Int
getLineCount = Seq.length . bufferLines

-- | How many lines at the start and at the end are the ones there at the last
-- 'markLinesSeen'; the lines between may have changed. Whatever was derived
-- per line from the marked document can be kept for those lines and
-- rederived for the rest.
changedLines :: TextBuffer -> (Int, Int)
changedLines buf = (bufferSeenHead buf, bufferSeenTail buf)

-- | Record that every line has been seen, for 'changedLines'.
markLinesSeen :: TextBuffer -> TextBuffer
markLinesSeen buf = let n = getLineCount buf in buf {bufferSeenHead = n, bufferSeenTail = n}

-- | The nearest position inside the document.
clampCursor :: TextBuffer -> Cursor -> Cursor
clampCursor buf (Cursor row col) =
  let !r = max 0 (min (getLineCount buf - 1) row)
      !c = max 0 (min (T.length (lineAt r buf)) col)
   in Cursor r c

-- | Move to a position, clamped into the document, without changing text.
withCursor :: Cursor -> TextBuffer -> TextBuffer
withCursor cur buf =
  let c = clampCursor buf cur
   in buf {bufferCursor = c, preferredCol = cursorCol c}

--------------------------------------------------------------------------------
-- Navigation
--------------------------------------------------------------------------------

moveLeft :: TextBuffer -> TextBuffer
moveLeft buf = withCursor (positionLeft buf (getCursor buf)) buf

moveRight :: TextBuffer -> TextBuffer
moveRight buf = withCursor (positionRight buf (getCursor buf)) buf

positionLeft :: TextBuffer -> Cursor -> Cursor
positionLeft buf (Cursor row col)
  | col > 0 = Cursor row (col - 1)
  | row > 0 = Cursor (row - 1) (T.length (lineAt (row - 1) buf))
  | otherwise = Cursor 0 0

positionRight :: TextBuffer -> Cursor -> Cursor
positionRight buf (Cursor row col)
  | col < T.length (lineAt row buf) = Cursor row (col + 1)
  | row + 1 < getLineCount buf = Cursor (row + 1) 0
  | otherwise = Cursor row col

moveUp :: TextBuffer -> TextBuffer
moveUp = moveByRow (-1)

moveDown :: TextBuffer -> TextBuffer
moveDown = moveByRow 1

moveByRow :: Int -> TextBuffer -> TextBuffer
moveByRow d buf =
  let Cursor row _ = getCursor buf
      goal = preferredCol buf
   in buf {bufferCursor = clampCursor buf (Cursor (row + d) goal)}

moveToBOL :: TextBuffer -> TextBuffer
moveToBOL buf = withCursor (Cursor (cursorRow (getCursor buf)) 0) buf

moveToEOL :: TextBuffer -> TextBuffer
moveToEOL buf =
  let row = cursorRow (getCursor buf)
   in withCursor (Cursor row (T.length (lineAt row buf))) buf

moveToTop :: TextBuffer -> TextBuffer
moveToTop = withCursor (Cursor 0 0)

moveToBottom :: TextBuffer -> TextBuffer
moveToBottom buf = withCursor (documentEnd buf) buf

-- | Back over spaces (line breaks count), then over the word before them.
moveWordLeft :: TextBuffer -> TextBuffer
moveWordLeft buf = withCursor (wordLeft buf (getCursor buf)) buf

-- | Forward over spaces (line breaks count), then over the word after them.
moveWordRight :: TextBuffer -> TextBuffer
moveWordRight buf = withCursor (wordRight buf (getCursor buf)) buf

wordLeft :: TextBuffer -> Cursor -> Cursor
wordLeft buf (Cursor row col) =
  let before = T.take col (lineAt row buf)
      spaces = T.length (T.takeWhileEnd isSpace before)
      inWord = col - spaces
   in if inWord == 0 && row > 0
        -- Only spaces back to the line start: the line break is one more.
        then wordLeft buf (Cursor (row - 1) (T.length (lineAt (row - 1) buf)))
        else Cursor row (inWord - T.length (T.takeWhileEnd (not . isSpace) (T.take inWord before)))

wordRight :: TextBuffer -> Cursor -> Cursor
wordRight buf (Cursor row col) =
  let after = T.drop col (lineAt row buf)
      spaces = T.length (T.takeWhile isSpace after)
      rest = T.drop spaces after
   in if T.null rest && row + 1 < getLineCount buf
        then wordRight buf (Cursor (row + 1) 0)
        else Cursor row (col + spaces + T.length (T.takeWhile (not . isSpace) rest))

--------------------------------------------------------------------------------
-- Edits
--------------------------------------------------------------------------------

-- | Apply an edit and leave the cursor after the inserted text. The removed
-- text decides how far the edit reaches, so an edit recorded against this
-- document (or undone from one) is applied without reading the text it
-- removes.
applyEdit :: TextEdit -> TextBuffer -> TextBuffer
applyEdit (TextEdit at removed inserted) buf =
  let Cursor row col = clampCursor buf at
      Cursor endRow endCol = advance (Cursor row col) removed
      lns = bufferLines buf
      first = lineAt row buf
      lastLine = lineAt endRow buf
      prefix = T.take col first
      suffix = T.drop endCol lastLine
      newLines = case T.splitOn "\n" inserted of
        firstPiece : rest@(_ : _) ->
          Seq.fromList ((prefix <> firstPiece) : init rest ++ [last rest <> suffix])
        _ -> Seq.singleton (prefix <> inserted <> suffix)
      spliced = Seq.take row lns <> newLines <> Seq.drop (min (Seq.length lns) (endRow + 1)) lns
      end = advance (Cursor row col) inserted
      untouchedTail = Seq.length spliced - (row + Seq.length newLines)
   in TextBuffer spliced end (cursorCol end) (min row (bufferSeenHead buf)) (min untouchedTail (bufferSeenTail buf))

-- | The position after walking over @txt@ from @cur@.
advance :: Cursor -> Text -> Cursor
advance (Cursor row col) txt =
  case T.count "\n" txt of
    0 -> Cursor row (col + T.length txt)
    breaks -> Cursor (row + breaks) (T.length (T.takeWhileEnd (/= '\n') txt))

-- | The edit that takes the document back.
invertEdit :: TextEdit -> TextEdit
invertEdit (TextEdit at removed inserted) = TextEdit at inserted removed

-- | The edit replacing the text between two positions.
replaceEdit :: Text -> Cursor -> Cursor -> TextBuffer -> TextEdit
replaceEdit inserted a b buf =
  let (lo, hi) = selectionRange (clampCursor buf a) (clampCursor buf b)
   in TextEdit lo (textRange lo hi buf) inserted

-- | Text as it can enter the document: printable characters, tabs and line
-- breaks, with Windows line ends folded.
insertableText :: Text -> Text
insertableText = T.filter (\c -> isPrint c || c == '\t' || c == '\n') . T.replace "\r\n" "\n"

--------------------------------------------------------------------------------
-- Selection
--------------------------------------------------------------------------------

selectionRange :: Cursor -> Cursor -> (Cursor, Cursor)
selectionRange a b = (min a b, max a b)

selectedText :: Cursor -> Cursor -> TextBuffer -> Text
selectedText a b buf =
  let (lo, hi) = selectionRange (clampCursor buf a) (clampCursor buf b)
   in textRange lo hi buf

-- | The text between two positions in document order, reading only the lines
-- between them.
textRange :: Cursor -> Cursor -> TextBuffer -> Text
textRange (Cursor loRow loCol) (Cursor hiRow hiCol) buf
  | loRow == hiRow = T.take (hiCol - loCol) (T.drop loCol (lineAt loRow buf))
  | otherwise =
      let middle = toList (Seq.take (hiRow - loRow - 1) (Seq.drop (loRow + 1) (bufferLines buf)))
       in T.intercalate "\n" (T.drop loCol (lineAt loRow buf) : middle ++ [T.take hiCol (lineAt hiRow buf)])

documentEnd :: TextBuffer -> Cursor
documentEnd buf =
  let row = getLineCount buf - 1
   in Cursor row (T.length (lineAt row buf))
