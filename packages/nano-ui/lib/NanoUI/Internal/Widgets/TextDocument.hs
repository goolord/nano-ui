-- | Implementation of "NanoUI.Widgets.TextDocument", plus the conversions
-- between a document and the widgets' text buffer.
module NanoUI.Internal.Widgets.TextDocument
  ( TextDocument
  , textDocument
  , emptyDocument
  , documentText
  , documentLines
  , documentLine
  , documentLineCount
  , sameDocument

    -- * For the widgets
  , bufferDocument
  , documentBuffer
  , sameLines
  ) where

import Data.Functor.Classes (liftEq)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Exts (isTrue#, reallyUnsafePtrEquality#)
import NanoUI.Widgets.TextBuffer qualified as TB

-- | A text as its lines, without their newlines. There is always at least
-- one line.
newtype TextDocument = TextDocument (Seq Text)

-- | Documents with the same text are equal. A document compared with itself,
-- such as the one a text area returned last frame, is equal at once, however
-- long it is. An edited document shares its untouched lines with the one it
-- was edited from, so comparing the two reads no text before the edit.
instance Eq TextDocument where
  a == b =
    sameDocument a b
      || liftEq sameLine (documentLines a) (documentLines b)
   where
    sameLine !x !y = isTrue# (reallyUnsafePtrEquality# x y) || x == y

instance Show TextDocument where
  showsPrec d doc =
    showParen
      (d > 10)
      (showString "textDocument " . showsPrec 11 (documentText doc))

-- | Split a text into a document. O(length of the text).
textDocument :: Text -> TextDocument
textDocument = TextDocument . TB.splitLines

-- | A document holding one empty line.
emptyDocument :: TextDocument
emptyDocument = TextDocument (Seq.singleton T.empty)

-- | The lines joined with newlines. O(length of the text).
documentText :: TextDocument -> Text
documentText = TB.joinLines . documentLines

-- | Share the document's line sequence without joining or copying text. Lines
-- exclude newline separators; the sequence always contains at least one line.
documentLines :: TextDocument -> Seq Text
documentLines (TextDocument lns) = lns

-- | The text of a line, or empty outside the document. O(log lines).
documentLine :: Int -> TextDocument -> Text
documentLine row doc = fromMaybe T.empty (Seq.lookup row (documentLines doc))

-- | Number of lines in O(1), including the empty line in an empty document.
documentLineCount :: TextDocument -> Int
documentLineCount = Seq.length . documentLines

-- | Whether two documents are the same value, in O(1). Equal documents built
-- separately may still answer 'False'; '==' compares their text.
sameDocument :: TextDocument -> TextDocument -> Bool
sameDocument a b = sameLines (documentLines a) (documentLines b)

-- | The document a buffer holds, sharing its lines.
bufferDocument :: TB.TextBuffer -> TextDocument
bufferDocument = TextDocument . TB.bufferLines

-- | A buffer over the document's lines, sharing them, with the cursor at the
-- start.
documentBuffer :: TextDocument -> TB.TextBuffer
documentBuffer = TB.fromLines . documentLines

-- | Whether two line sequences are the same heap object. An edit makes new
-- lines and a cursor move keeps them, so this tells an edited buffer from a
-- moved one without reading any text. It can answer 'False' for the same
-- lines (GHC may compare an evaluated value with a reference to it before
-- evaluation), never 'True' for different ones; callers treat 'False' as
-- "maybe changed". Both arguments are forced first, so neither is a thunk.
sameLines :: Seq Text -> Seq Text -> Bool
sameLines !a !b = isTrue# (reallyUnsafePtrEquality# a b)
