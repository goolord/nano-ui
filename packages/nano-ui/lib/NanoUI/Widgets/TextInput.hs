module NanoUI.Widgets.TextInput
  ( TextInputState (..)
  , textInputLayout
  , searchFieldLayout
  , processTextInput
  , applyTextInputMenuAction
  )
where

import Data.Char (isPrint)
import Data.IORef (writeIORef)
import Data.IntMap.Strict qualified as IM
import Data.Text (Text)
import Data.Text qualified as T
import NanoUI.Context
  ( Context (..)
  , getStore
  , intKey
  , markDirty
  , setStore
  , setTextInputMenu
  )
import NanoUI.Id (WidgetId)
import NanoUI.Input
  ( Input (..)
  , Key (..)
  , Modifiers (..)
  , foldInputKeys
  , inputChars
  , inputKeys
  , inputModifiers
  )
import NanoUI.Store (WidgetStore (..), slotAnchor, slotCursor, slotKey)
import NanoUI.Style (Layout (..), Sizing (..), defaultLayout)
import NanoUI.Widgets.TextBuffer qualified as TB
import NanoUI.Widgets.TextCommon
  ( copyBufferText
  , cutBufferText
  , dispatchCtrlChar
  , dispatchMenuAction
  , isCtrlCombo
  , pasteBufferText
  )

textInputLayout :: Layout
textInputLayout =
  defaultLayout
    { layoutWidth = Grow 1
    , layoutMinW = 160
    }

-- | Layout for a caption-less search field. Grows to fill, keeps a little more
-- room for the embedded magnifier / clear chrome than a plain text input.
searchFieldLayout :: Layout
searchFieldLayout =
  defaultLayout
    { layoutWidth = Grow 1
    , layoutMinW = 180
    }

data TextInputState = TextInputState
  { tisText :: !Text
  , tisCursor :: !Int
  , tisAnchor :: !Int
  }
  deriving (Eq, Show)

toBuffer :: TextInputState -> (TB.TextBuffer, TB.Cursor)
toBuffer s =
  let
    buf0 = TB.fromText (tisText s)
    cur = TB.Cursor 0 (tisCursor s)
    anc = TB.Cursor 0 (tisAnchor s)
   in
    (TB.withCursor cur buf0, anc)

-- Editing collapses selection to the resulting cursor; navigation can retain
-- the original anchor when Shift is held.
fromBuffer :: Maybe Int -> TB.TextBuffer -> TextInputState
fromBuffer anchor buf =
  let
    c = TB.cursorCol (TB.getCursor buf)
   in
    TextInputState (TB.toText buf) c (maybe c id anchor)

selectAllTextInput :: TextInputState -> TextInputState
selectAllTextInput s =
  s {tisAnchor = 0, tisCursor = T.length (tisText s)}

textInputCopy :: Context -> TextInputState -> IO ()
textInputCopy ctx s =
  let
    (buf, anc) = toBuffer s
   in
    copyBufferText ctx anc buf

textInputCut :: Context -> TextInputState -> IO TextInputState
textInputCut ctx s = do
  let
    (buf, anc) = toBuffer s
  fromBuffer Nothing <$> cutBufferText ctx anc buf

textInputPaste :: Context -> TextInputState -> IO TextInputState
textInputPaste ctx s = do
  let
    (buf, anc) = toBuffer s
  mbuf' <- pasteBufferText ctx False anc buf
  pure (maybe s (fromBuffer Nothing) mbuf')

applyTextInputMenuAction :: Context -> WidgetId -> Int -> IO ()
applyTextInputMenuAction ctx wid item = do
  store <- getStore ctx
  let
    key = intKey wid
    text = IM.findWithDefault "" key (storeText store)
    cursor = IM.findWithDefault (T.length text) (slotKey slotCursor key) (storeInt store)
    anchor = IM.findWithDefault cursor (slotKey slotAnchor key) (storeInt store)
    s0 = TextInputState text cursor anchor
  s1 <-
    dispatchMenuAction
      (textInputCut ctx)
      (textInputCopy ctx)
      (textInputPaste ctx)
      selectAllTextInput
      item
      s0
  setStore
    ctx
    ( store
        { storeText = IM.insert key (tisText s1) (storeText store)
        , storeInt =
            IM.insert (slotKey slotCursor key) (tisCursor s1) $
              IM.insert (slotKey slotAnchor key) (tisAnchor s1) (storeInt store)
        }
    )
  -- Menu actions edit a field that may not be under the pointer; focus it so
  -- the selection highlight and caret become visible.
  writeIORef (ctxFocusId ctx) wid
  setTextInputMenu ctx Nothing
  markDirty ctx

processTextInput :: Context -> Input -> TextInputState -> IO TextInputState
processTextInput ctx inp s0 = do
  let
    mods = inputModifiers inp
    ctrl = modCtrl mods
    alt = modAlt mods
    shift = modShift mods
    keys = inputKeys inp
    chars = inputChars inp
  s1 <-
    if ctrl
      then T.foldlM' (handleCtrlChar ctx) s0 chars
      else pure s0
  let
    filtered = T.filter (\ch -> not (isCtrlCombo ctrl ch) && isPrint ch && ch /= '\n') chars
    s2 = insertText s1 filtered
    -- Word-wise editing keys, matching the text area: Ctrl or Alt plus
    -- Backspace/Delete/Left/Right works on words instead of characters.
    word = ctrl || alt
  pure (foldInputKeys (applyKey word shift) s2 keys)

handleCtrlChar :: Context -> TextInputState -> Char -> IO TextInputState
handleCtrlChar ctx =
  dispatchCtrlChar
    (pure . selectAllTextInput)
    (textInputCopy ctx)
    (textInputCut ctx)
    (textInputPaste ctx)

-- Host text events may contain a whole IME commit. Convert the state once and
-- replace the selection once rather than rebuilding the buffer per character.
insertText :: TextInputState -> Text -> TextInputState
insertText s text | T.null text = s
insertText s text =
  let
    (buf, anc) = toBuffer s
    cur = TB.getCursor buf
    buf' =
      if anc /= cur
        then TB.replaceRange text anc cur buf
        else TB.insertText text buf
   in
    fromBuffer Nothing buf'

applyKey :: Bool -> Bool -> TextInputState -> Key -> TextInputState
applyKey word shift s key =
  let
    (buf, anc) = toBuffer s
    cur = TB.getCursor buf
    hasSel = anc /= cur
    deleteWith f =
      fromBuffer Nothing (if hasSel then TB.deleteRange anc cur buf else f buf)
   in
    case key of
      KeyBackspace
        | word -> moveWith shift buf anc TB.deletePrevWord
        | otherwise -> deleteWith TB.deletePrevChar
      KeyDelete
        | word -> moveWith shift buf anc TB.deleteNextWord
        | otherwise -> deleteWith TB.deleteChar
      KeyLeft
        | word -> moveWith shift buf anc TB.moveWordLeft
        | otherwise -> moveWith shift buf anc TB.moveLeft
      KeyRight
        | word -> moveWith shift buf anc TB.moveWordRight
        | otherwise -> moveWith shift buf anc TB.moveRight
      KeyHome -> moveWith shift buf anc TB.moveToBOL
      KeyEnd -> moveWith shift buf anc TB.moveToEOL
      _ -> s

moveWith ::
  Bool
  -> TB.TextBuffer
  -> TB.Cursor
  -> (TB.TextBuffer -> TB.TextBuffer)
  -> TextInputState
moveWith shift buf anc f =
  fromBuffer (if shift then Just (TB.cursorCol anc) else Nothing) (f buf)
