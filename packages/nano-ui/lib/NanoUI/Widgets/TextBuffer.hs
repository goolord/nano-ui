{-# LANGUAGE BangPatterns #-}

module NanoUI.Widgets.TextBuffer
  ( -- * Types
    TextBuffer (..)
  , Cursor (..)
    -- * Construction & Conversion
  , empty
  , fromText
  , toText
  , toLines
    -- * Cursor & Metrics
  , getCursor
  , getLineCount
  , withCursor
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
  , compareCursor
  , cursorBefore
  , selectionRange
  , selectedText
  , deleteRange
  , replaceRange
  , documentEnd
    -- * Editing Operations
  , insertChar
  , insertText
  , breakLine
  , deletePrevChar
  , deleteChar
  , deletePrevWord
  , deleteNextWord
  , killToEOL
  , killToBOL
  ) where

import qualified Data.Text as T
import NanoUI.Types (clamp)
import NanoUI.Font (tabSentinelChar)
import qualified Data.Text.Zipper as TZ
import qualified Data.Text.Zipper.Generic.Words as TZW

-- | Zero-indexed logical (row, column) position in the buffer.
data Cursor = Cursor
  { cursorRow :: {-# UNPACK #-} !Int
  , cursorCol :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Show)

-- | Document state backed by a 2D text zipper.
data TextBuffer = TextBuffer
  { unTextBuffer :: TZ.TextZipper T.Text
  , preferredCol :: {-# UNPACK #-} !Int
  }
  deriving (Show)

-- | Construct an empty TextBuffer containing a single blank line.
empty :: TextBuffer
empty = fromText ""

-- | Construct a TextBuffer from raw Text. Cursor is always (0, 0).
fromText :: T.Text -> TextBuffer
fromText t =
  let validLines = case T.splitOn "\n" (encodeTabs t) of
        [] -> [""]
        lns -> lns
      z = TZ.gotoBOF (TZ.textZipper validLines Nothing)
  in TextBuffer z 0

-- | Flatten all lines into a single newline-separated Text block.
toText :: TextBuffer -> T.Text
toText = T.intercalate "\n" . toLines

-- | Extract all lines for measurement and rendering loops.
toLines :: TextBuffer -> [T.Text]
toLines = map decodeTabs . TZ.getText . unTextBuffer

-- | Query current cursor coordinates.
getCursor :: TextBuffer -> Cursor
getCursor (TextBuffer z _) =
  let (r, c) = TZ.cursorPosition z
  in Cursor {cursorRow = r, cursorCol = c}

-- | Return the total line count.
getLineCount :: TextBuffer -> Int
getLineCount = length . toLines

-- | Move to an absolute cursor position without changing document text.
withCursor :: Cursor -> TextBuffer -> TextBuffer
withCursor (Cursor row col) buf =
  let lineTexts = toLines buf
      lastRow = max 0 (length lineTexts - 1)
      r = clamp 0 lastRow row
      lineText =
        if null lineTexts
          then ""
          else lineTexts !! r
      c = clamp 0 (T.length lineText) col
      z = TZ.moveCursor (r, c) (unTextBuffer (fromText (toText buf)))
  in TextBuffer z c

zipperCol :: TZ.TextZipper T.Text -> Int
zipperCol = snd . TZ.cursorPosition

withZipper :: (TZ.TextZipper T.Text -> TZ.TextZipper T.Text) -> TextBuffer -> TextBuffer
withZipper f (TextBuffer z _) =
  let z' = f z
  in TextBuffer z' (zipperCol z')

-- | text-zipper drops non-printables, including Tab. Store Tab as a printable
-- stand-in inside the zipper and map it back at the public Text boundary.
tabSentinel :: Char
tabSentinel = tabSentinelChar

encodeTabs :: T.Text -> T.Text
encodeTabs = T.replace "\t" (T.singleton tabSentinel)

decodeTabs :: T.Text -> T.Text
decodeTabs = T.replace (T.singleton tabSentinel) "\t"

--------------------------------------------------------------------------------
-- Navigation
--------------------------------------------------------------------------------

moveLeft :: TextBuffer -> TextBuffer
moveLeft = withZipper TZ.moveLeft

moveRight :: TextBuffer -> TextBuffer
moveRight = withZipper TZ.moveRight

moveUp :: TextBuffer -> TextBuffer
moveUp = moveByRow (-1)

moveDown :: TextBuffer -> TextBuffer
moveDown = moveByRow 1

moveByRow :: Int -> TextBuffer -> TextBuffer
moveByRow d (TextBuffer z goal) =
  let (row, _) = TZ.cursorPosition z
      lineTexts = TZ.getText z
      lastRow = max 0 (length lineTexts - 1)
      newRow = clamp 0 lastRow (row + d)
      lineText = lineTexts !! newRow
      newCol = min goal (T.length lineText)
      z' = TZ.moveCursor (newRow, newCol) z
  in TextBuffer z' goal

moveToBOL :: TextBuffer -> TextBuffer
moveToBOL = withZipper TZ.gotoBOL

moveToEOL :: TextBuffer -> TextBuffer
moveToEOL = withZipper TZ.gotoEOL

moveToTop :: TextBuffer -> TextBuffer
moveToTop = withZipper TZ.gotoBOF

moveToBottom :: TextBuffer -> TextBuffer
moveToBottom = withZipper TZ.gotoEOF

moveWordLeft :: TextBuffer -> TextBuffer
moveWordLeft = withZipper TZW.moveWordLeft

moveWordRight :: TextBuffer -> TextBuffer
moveWordRight = withZipper TZW.moveWordRight

--------------------------------------------------------------------------------
-- Editing
--------------------------------------------------------------------------------

insertChar :: Char -> TextBuffer -> TextBuffer
insertChar '\n' = breakLine
insertChar '\t' = withZipper (TZ.insertChar tabSentinel)
insertChar c = withZipper (TZ.insertChar c)

insertText :: T.Text -> TextBuffer -> TextBuffer
insertText txt buf = T.foldl' (flip insertChar) buf txt

breakLine :: TextBuffer -> TextBuffer
breakLine = withZipper TZ.breakLine

deletePrevChar :: TextBuffer -> TextBuffer
deletePrevChar = withZipper TZ.deletePrevChar

deleteChar :: TextBuffer -> TextBuffer
deleteChar = withZipper TZ.deleteChar

deletePrevWord :: TextBuffer -> TextBuffer
deletePrevWord = withZipper TZW.deletePrevWord

deleteNextWord :: TextBuffer -> TextBuffer
deleteNextWord = withZipper TZW.deleteWord

killToEOL :: TextBuffer -> TextBuffer
killToEOL = withZipper TZ.killToEOL

killToBOL :: TextBuffer -> TextBuffer
killToBOL = withZipper TZ.killToBOL

compareCursor :: Cursor -> Cursor -> Ordering
compareCursor (Cursor r1 c1) (Cursor r2 c2) =
  compare r1 r2 <> compare c1 c2

cursorBefore :: Cursor -> Cursor -> Bool
cursorBefore a b = compareCursor a b == LT

selectionRange :: Cursor -> Cursor -> (Cursor, Cursor)
selectionRange a b =
  if cursorBefore a b
    then (a, b)
    else (b, a)

selectedText :: Cursor -> Cursor -> TextBuffer -> T.Text
selectedText a b buf =
  let (lo, hi) = selectionRange a b
      text = toText buf
      loOff = cursorOffset buf lo
      hiOff = cursorOffset buf hi
   in T.take (hiOff - loOff) (T.drop loOff text)

cursorOffset :: TextBuffer -> Cursor -> Int
cursorOffset buf (Cursor row col) =
  let lineTexts = toLines buf
   in sum (map ((+ 1) . T.length) (take row lineTexts)) + col

deleteRange :: Cursor -> Cursor -> TextBuffer -> TextBuffer
deleteRange a b buf =
  let text = toText buf
      (lo, hi) = selectionRange a b
      loOff = cursorOffset buf lo
      hiOff = cursorOffset buf hi
      newBuf = fromText (T.take loOff text <> T.drop hiOff text)
   in withCursor (offsetToCursor newBuf loOff) newBuf

replaceRange :: T.Text -> Cursor -> Cursor -> TextBuffer -> TextBuffer
replaceRange insert a b buf =
  let text = toText buf
      (lo, hi) = selectionRange a b
      loOff = cursorOffset buf lo
      hiOff = cursorOffset buf hi
      newText = T.take loOff text <> insert <> T.drop hiOff text
      newBuf = fromText newText
      endOff = loOff + T.length insert
   in withCursor (offsetToCursor newBuf endOff) newBuf

offsetToCursor :: TextBuffer -> Int -> Cursor
offsetToCursor buf off =
  let lineTexts = toLines buf
      go _ [] _ = Cursor 0 0
      go r (ln : rest) acc =
        let len = T.length ln
         in if off <= acc + len
              then Cursor r (off - acc)
              else go (r + 1) rest (acc + len + 1)
   in go 0 lineTexts 0

documentEnd :: TextBuffer -> Cursor
documentEnd buf =
  let lineTexts = toLines buf
      r = max 0 (length lineTexts - 1)
      c =
        if null lineTexts
          then 0
          else T.length (lineTexts !! r)
   in Cursor r c