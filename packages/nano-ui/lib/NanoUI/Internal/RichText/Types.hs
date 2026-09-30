-- | Paragraph values and typed layout cache, independent of view execution.
module NanoUI.Internal.RichText.Types
  ( Inline (..)
  , inlineText
  , Run (..)
  , TokenKind (..)
  , Token (..)
  , Line (..)
  , Paragraph (..)
  , Inputs (..)
  , RichSelection (..)
  , Measured (..)
  , ParagraphCache (..)
  , Paragraphs (..)
  )
where

import Data.IORef (IORef)
import Data.IntMap.Strict (IntMap)
import Data.Primitive.SmallArray (SmallArray)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import NanoUI.Internal.Draw.Types (TextFont)
import NanoUI.Internal.Font (FontMetrics)
import NanoUI.Internal.Style (Layout, Theme)
import NanoUI.Internal.Types (Color)
import NanoUI.Widgets.TextBuffer (Cursor, TextBuffer)

-- | Text with a layout-style modifier, optional link target and background.
data Inline = Inline !Text (Layout -> Layout) !(Maybe Text) !(Maybe Color)

instance IsString Inline where
  fromString = inlineText . T.pack

inlineText :: Text -> Inline
inlineText txt = Inline txt id Nothing Nothing

data Run = Run
  { runFont :: !TextFont
  , runColor :: !Color
  , runLineHeight :: !Float
  , runAscent :: !Float
  , runTarget :: !(Maybe Text)
  , runBackground :: !(Maybe Color)
  }

data TokenKind = Word | Space | Break | Glyph | GlyphSpace
  deriving Eq

data Token = Token
  { tokenText :: !Text
  , tokenRun :: !Int
  , tokenKind :: !TokenKind
  , tokenWidth :: !Float
  , tokenStart :: {-# UNPACK #-} !Int
  , tokenMetrics :: !FontMetrics
  }

data Line = Line
  { lineStart :: {-# UNPACK #-} !Int
  , lineTop :: !Float
  , lineHeight :: !Float
  , lineAscent :: !Float
  , lineWidth :: !Float
  , lineTokens :: ![(Float, Token)]
  }

-- | Disposable text/measurement data. Selection belongs to the widget store,
-- so changing presentation or evicting this cache cannot clear it.
data Paragraph = Paragraph
  { paraKey :: !Int
  , paraInputs :: !(IORef Inputs)
  , paraRuns :: !(SmallArray Run)
  , paraTokens :: ![Token]
  , paraEmptyLine :: !(Float, Float)
  , paraNatural :: (Float, Float)
  , paraWidth :: !Float
  , paraLines :: [Line]
  , paraMeasured :: !(IORef Measured)
  , paraText :: !Text
  , paraBuffer :: TextBuffer
  -- ^ Built lazily, at most once per paragraph, when a text command needs it.
  }

data Inputs = Inputs [Inline] !Layout !Theme !Int !Bool

data RichSelection = RichSelection
  { selectionText :: !Text
  , selectionAnchor :: !Cursor
  , selectionCursor :: !Cursor
  , selectionDragging :: !Bool
  , selectionMoved :: !Bool
  , selectionStart :: !Int
  , selectionEnd :: !Int
  }
  deriving Eq

data Measured = Unmeasured | Measured !Float !Float !Float

-- | Entry count, eviction threshold, and paragraphs by widget key.
data ParagraphCache = ParagraphCache !Int !Int !(IntMap Paragraph)

newtype Paragraphs = Paragraphs (IORef ParagraphCache)
