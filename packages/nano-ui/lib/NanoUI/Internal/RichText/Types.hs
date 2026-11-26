{-# OPTIONS_HADDOCK not-home #-}

-- | Paragraph values and typed layout cache, independent of view execution.
module NanoUI.Internal.RichText.Types
  ( InlineStyle (..)
  , Inline (..)
  , inlineText
  , Run (..)
  , TokenKind (..)
  , Token (..)
  , Line (..)
  , InteractivePiece (..)
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

-- | How an 'Inline' piece modifies the paragraph's base layout. Common presets
-- carry no function or 'Layout' copy so steady-frame cache checks allocate
-- nothing.
data InlineStyle
  = InlinePlain
  | InlineBold
  | InlineItalic
  | InlineMono
  | InlineCustom !(Layout -> Layout)

-- | Text with a layout-style modifier, optional link target, background,
-- and hover tooltip.
data Inline = Inline !Text !InlineStyle !(Maybe Text) !(Maybe Color) !(Maybe Text)

instance IsString Inline where
  fromString = inlineText . T.pack

-- | Plain text in the paragraph's style. A string literal is the same piece
-- ('IsString').
inlineText :: Text -> Inline
inlineText txt = Inline txt InlinePlain Nothing Nothing Nothing

-- | The style of one 'Inline' piece, resolved once when the paragraph is
-- measured.
data Run = Run
  { runFont :: !TextFont
  , runColor :: !Color
  , runLineHeight :: !Float
  -- ^ Line height of 'runFont'.
  , runAscent :: !Float
  -- ^ Distance from the top of a line box to the baseline of 'runFont'.
  , runTarget :: !(Maybe Text)
  -- ^ Link target, if the piece is a link.
  , runBackground :: !(Maybe Color)
  -- ^ Fill drawn behind the piece's text, if any.
  , runTooltip :: !(Maybe Text)
  -- ^ Optional tooltip shown when hovering the piece.
  }

-- | How a 'Token' wraps. 'Word' and 'Space' come from pieces that break at
-- spaces, 'Break' is a newline, and 'Glyph' and 'GlyphSpace' are single
-- characters from pieces that break anywhere.
data TokenKind
  = -- | A run of non-space characters, kept on one line.
    Word
  | -- | A run of spaces or tabs.
    Space
  | -- | A newline; it has zero width and ends the line.
    Break
  | -- | One character cluster of a break-anywhere piece.
    Glyph
  | -- | A space or tab inside a break-anywhere piece.
    GlyphSpace
  deriving Eq

-- | A unit of line breaking: a slice of one piece's text with its measured
-- width.
data Token = Token
  { tokenText :: !Text
  , tokenRun :: !Int
  -- ^ Index of the token's 'Run' in 'paraRuns'.
  , tokenKind :: !TokenKind
  , tokenWidth :: !Float
  , tokenStart :: {-# UNPACK #-} !Int
  -- ^ Character offset of the token in the paragraph's plain text.
  , tokenMetrics :: !FontMetrics
  -- ^ Font metrics prepared for the token's text.
  }

-- | One wrapped line of a paragraph, placed by its tokens.
data Line = Line
  { lineStart :: {-# UNPACK #-} !Int
  -- ^ Character offset of the line's first character in the paragraph text.
  , lineTop :: !Float
  -- ^ Offset of the line box from the top of the paragraph.
  , lineHeight :: !Float
  , lineAscent :: !Float
  , lineWidth :: !Float
  , lineTokens :: ![(Float, Token)]
  -- ^ Tokens with their x offset from the line's left edge.
  }

-- | Interactive span (link or tooltipped run) relative to the paragraph's
-- top-left corner, cached alongside 'paraLines' when the width settles.
data InteractivePiece = InteractivePiece
  { ipRun :: {-# UNPACK #-} !Int
  , ipX :: {-# UNPACK #-} !Float
  , ipY :: {-# UNPACK #-} !Float
  , ipW :: {-# UNPACK #-} !Float
  , ipH :: {-# UNPACK #-} !Float
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
  , paraInteractive :: ![InteractivePiece]
  -- ^ Link and tooltip spans in paragraph-local coordinates for 'paraLines'.
  , paraTooltipRuns :: ![(Int, Text)]
  -- ^ Runs carrying a hover tooltip, precomputed once per paragraph.
  , paraHasInteractive :: !Bool
  -- ^ Whether any run is a link or has a hover tooltip.
  , paraHasGlyphs :: !Bool
  -- ^ Whether any piece uses 'BreakAnywhere' (has 'Glyph' or 'GlyphSpace' tokens).
  , paraMeasured :: !(IORef Measured)
  , paraText :: !Text
  , paraBuffer :: TextBuffer
  -- ^ Built lazily, at most once per paragraph, when a text command needs it.
  }

-- | What a 'Paragraph' was built from: the pieces, the base 'Layout', the
-- 'Theme', the font metric generation, and whether it is selectable. The
-- paragraph is reused while a new frame's inputs match these.
data Inputs = Inputs [Inline] !Layout !Theme !Int !Bool

-- | The selection in a selectable paragraph, kept in the widget store.
data RichSelection = RichSelection
  { selectionText :: !Text
  -- ^ The paragraph text the offsets below index.
  , selectionAnchor :: !Cursor
  -- ^ Where the selection started.
  , selectionCursor :: !Cursor
  -- ^ The moving end of the selection.
  , selectionDragging :: !Bool
  -- ^ A mouse drag that began in the paragraph is in progress.
  , selectionMoved :: !Bool
  -- ^ The pointer has moved since the drag began.
  , selectionStart :: !Int
  -- ^ Lower end of the selection, as a character offset.
  , selectionEnd :: !Int
  -- ^ Upper end of the selection, as a character offset.
  }
  deriving Eq

-- | The last extent measured for a paragraph. @Measured width w h@ holds the
-- extent @(w, h)@ computed when offered @width@.
data Measured
  = -- | Nothing measured yet.
    Unmeasured
  | Measured !Float !Float !Float

-- | Entry count, eviction threshold, and paragraphs by widget key.
data ParagraphCache = ParagraphCache !Int !Int !(IntMap Paragraph)

-- | The 'ParagraphCache' in a mutable reference; it is the value held in the
-- context's paragraph 'NanoUI.Internal.Host.Host' slot.
newtype Paragraphs = Paragraphs (IORef ParagraphCache)
