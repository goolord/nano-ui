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
  , menuActionEnabled
  , dispatchMenuAction
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
      let n = T.length text
          i = max 0 (min (n - 1) raw)
          cls = textCharClass (T.index text i)
          lo = goLeft cls i
          hi = goRight cls n i + 1
       in (lo, hi)
  where
    goLeft cls i
      | i <= 0 = 0
      | textCharClass (T.index text (i - 1)) == cls = goLeft cls (i - 1)
      | otherwise = i
    goRight cls n i
      | i + 1 >= n = i
      | textCharClass (T.index text (i + 1)) == cls = goRight cls n (i + 1)
      | otherwise = i

-- | Test if a character is part of a standard Ctrl shortcut (Ctrl+A, Ctrl+C, Ctrl+X, Ctrl+V).
{-# INLINE isCtrlCombo #-}
isCtrlCombo :: Bool -> Char -> Bool
isCtrlCombo c ch = c && T.elem ch "aAcCxXvV\x01\x03\x16\x18"

-- | Dispatch standard Ctrl keystrokes (A=selectAll, C=copy, X=cut, V=paste).
dispatchCtrlChar :: Monad m => (a -> m a) -> (a -> m ()) -> (a -> m a) -> (a -> m a) -> a -> Char -> m a
dispatchCtrlChar onSelectAll onCopy onCut onPaste s ch
  | ch `elem` ('a' : 'A' : '\x01' : []) = onSelectAll s
  | ch `elem` ('c' : 'C' : '\ETX' : []) = onCopy s >> pure s
  | ch `elem` ('x' : 'X' : '\x18' : []) = onCut s
  | ch `elem` ('v' : 'V' : '\x16' : []) = onPaste s
  | otherwise = pure s

-- | Check if a context menu action is enabled (0=Cut, 1=Copy, 2=Paste, 3=Select All).
{-# INLINE menuActionEnabled #-}
menuActionEnabled :: Bool -> Maybe Text -> Int -> Bool
menuActionEnabled hasText mclip item =
  case item of
    0 -> hasText
    1 -> hasText
    2 -> maybe False (not . T.null) mclip
    3 -> hasText
    _ -> False

-- | Dispatch context menu action (0=Cut, 1=Copy, 2=Paste, 3=Select All).
dispatchMenuAction :: Monad m => (s -> m s) -> (s -> m ()) -> (s -> m s) -> (s -> s) -> Int -> s -> m s
dispatchMenuAction onCut onCopy onPaste onSelectAll item s =
  case item of
    0 -> onCut s
    1 -> onCopy s >> pure s
    2 -> onPaste s
    3 -> pure (onSelectAll s)
    _ -> pure s

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
  let cur = TB.getCursor buf
  if anc /= cur
    then do
      let txt = TB.selectedText anc cur buf
      when (not (T.null txt)) $
        void (ctxClipboardSet ctx txt)
      pure (TB.deleteRange anc cur buf)
    else do
      let txt = TB.toText buf
      when (not (T.null txt)) $
        void (ctxClipboardSet ctx txt)
      pure (TB.fromText T.empty)

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
