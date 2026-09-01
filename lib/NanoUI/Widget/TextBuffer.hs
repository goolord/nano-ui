{-# LANGUAGE BangPatterns #-}

module NanoUI.Widget.TextBuffer
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

zipperCol :: TZ.TextZipper T.Text -> Int
zipperCol = snd . TZ.cursorPosition

withZipper :: (TZ.TextZipper T.Text -> TZ.TextZipper T.Text) -> TextBuffer -> TextBuffer
withZipper f (TextBuffer z _) =
  let z' = f z
  in TextBuffer z' (zipperCol z')

clamp :: Int -> Int -> Int -> Int
clamp lo hi x = max lo (min hi x)

-- | text-zipper drops non-printables, including Tab. Store Tab as a printable
-- stand-in inside the zipper and map it back at the public Text boundary.
tabSentinel :: Char
tabSentinel = '\x2409'

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
