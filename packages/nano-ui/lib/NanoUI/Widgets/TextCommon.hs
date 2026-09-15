{-# LANGUAGE BangPatterns #-}

module NanoUI.Widgets.TextCommon
  ( -- * Character classes and word boundaries
    TextCharClass (..)
  , textCharClass
  , textWordBounds
    -- * Ctrl combos and dispatch
  , isCtrlCombo
  , dispatchCtrlChar
    -- * Menu actions
  , MenuAction (..)
  , menuActionEnabled
  , dispatchMenuAction
    -- * Selection and caret helpers
  , textSelectionForClick
  , textSelectionForDrag
  , selectionBgColor
  , selectionCaretGeom
    -- * Clipboard operations on TextBuffer
  , copyBufferText
  , cutBufferText
  , pasteBufferText
  ) where

import Control.Monad (void, when)
import Data.Char (isAlphaNum, isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import NanoUI.Context (Context (..))
import NanoUI.Types (Color, clamp, lerpColor)
import qualified NanoUI.Widgets.TextBuffer as TB

-- | Character classification for double-click word selection.
data TextCharClass = TextWord | TextSpace | TextOther
  deriving (Eq)

textCharClass :: Char -> TextCharClass
textCharClass c
  | isAlphaNum c || c == '_' = TextWord
  | isSpace c = TextSpace
  | otherwise = TextOther

-- | Find the word bounds around a character position in text.
textWordBounds :: Text -> Int -> (Int, Int)
textWordBounds text raw
  | T.null text = (0, 0)
  | otherwise =
      -- Split once: repeatedly indexing UTF-8 text makes long-word selection
      -- quadratic. The clamped index guarantees a non-empty suffix.
      let i = clamp 0 (T.length text - 1) raw
          (before, after) = T.splitAt i text
          sameClass = (== textCharClass (T.head after)) . textCharClass
       in ( i - T.length (T.takeWhileEnd sameClass before)
          , i + T.length (T.takeWhile sameClass after)
          )

-- | Calculate selection span for single/double/triple click.
textSelectionForClick :: Text -> Int -> Int -> (Int, Int)
textSelectionForClick value idx clicks
  | clicks >= 3 = (0, T.length value)
  | clicks == 2 = textWordBounds value idx
  | otherwise = (idx, idx)

-- | Calculate selection span when dragging mouse across text.
textSelectionForDrag :: Text -> Int -> Int -> Int -> (Int, Int)
textSelectionForDrag value anchor idx clicks
  | clicks >= 3 = (0, T.length value)
  | clicks == 2 =
      let (a0, a1) = textWordBounds value anchor
          (c0, c1) = textWordBounds value idx
       in (min a0 c0, max a1 c1)
  | otherwise = (anchor, idx)

-- | Shared selection background color.
{-# INLINE selectionBgColor #-}
selectionBgColor :: Color -> Color -> Color
selectionBgColor accent bg = lerpColor accent bg 0.55

-- | Shared caret geometry (caretX, caretY, caretH).
{-# INLINE selectionCaretGeom #-}
selectionCaretGeom :: Float -> Float -> Float -> Float -> (Float, Float, Float)
selectionCaretGeom originX originY pw lineH =
  (originX + pw, originY + 1, max 4 (lineH - 2))

-- | Ctrl combinations that are editor/app shortcuts, never literal text. Beyond
-- the clipboard combos this lists the zoom keys the SDL backend forwards as
-- ctrl text (`=`, `+`, `-`, `0`), so focused fields do not insert them.
{-# INLINE isCtrlCombo #-}
isCtrlCombo :: Bool -> Char -> Bool
isCtrlCombo c ch = c && T.elem ch "aAcCxXvV=+-0\x01\x03\x16\x18"

-- | Dispatch standard Ctrl keystrokes (A=selectAll, C=copy, X=cut, V=paste).
dispatchCtrlChar :: Monad m => (a -> m a) -> (a -> m ()) -> (a -> m a) -> (a -> m a) -> a -> Char -> m a
dispatchCtrlChar onSelectAll onCopy onCut onPaste s ch
  | T.elem ch "aA\x01" = onSelectAll s
  | T.elem ch "cC\ETX" = onCopy s >> pure s
  | T.elem ch "xX\x18" = onCut s
  | T.elem ch "vV\x16" = onPaste s
  | otherwise = pure s

-- | Text-field context-menu actions, in menu order (their 'fromEnum' is the
-- menu row index).
data MenuAction = MenuCut | MenuCopy | MenuPaste | MenuSelectAll
  deriving (Eq, Show, Enum, Bounded)

-- | Whether a context-menu action applies: Paste needs clipboard text, the
-- others need field text.
menuActionEnabled :: Bool -> Maybe Text -> MenuAction -> Bool
menuActionEnabled hasText mclip = \case
  MenuPaste -> maybe False (not . T.null) mclip
  _ -> hasText

-- | Run a context-menu action on an editor state.
dispatchMenuAction :: Monad m => (s -> m s) -> (s -> m ()) -> (s -> m s) -> (s -> s) -> MenuAction -> s -> m s
dispatchMenuAction onCut onCopy onPaste onSelectAll action s = case action of
  MenuCut -> onCut s
  MenuCopy -> s <$ onCopy s
  MenuPaste -> onPaste s
  MenuSelectAll -> pure (onSelectAll s)

-- | Copy buffer text (either selected range or full buffer) to clipboard.
copyBufferText :: Context -> TB.Cursor -> TB.TextBuffer -> IO ()
copyBufferText ctx anc buf = do
  let cur = TB.getCursor buf
      txt = if anc /= cur then TB.selectedText anc cur buf else TB.toText buf
  when (not (T.null txt)) $
    void (ctxClipboardSet ctx txt)

-- | Cut buffer text to clipboard and delete the range, returning the updated buffer.
cutBufferText :: Context -> TB.Cursor -> TB.TextBuffer -> IO TB.TextBuffer
cutBufferText ctx anc buf = do
  copyBufferText ctx anc buf
  let cur = TB.getCursor buf
  pure (if anc /= cur then TB.deleteRange anc cur buf else TB.empty)

-- | Paste text from clipboard into buffer at selection or cursor.
pasteBufferText :: Context -> Bool -> TB.Cursor -> TB.TextBuffer -> IO (Maybe TB.TextBuffer)
pasteBufferText ctx allowNewlines anc buf = do
  mtxt <- ctxClipboardGet ctx
  case mtxt of
    Nothing -> pure Nothing
    Just rawPaste -> do
      let paste = if allowNewlines then rawPaste else T.filter (/= '\n') rawPaste
          cur = TB.getCursor buf
          buf' =
            if anc /= cur
              then TB.replaceRange paste anc cur buf
              else TB.insertText paste buf
      pure (Just buf')
