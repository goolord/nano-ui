module NanoUI.Widgets.TextInput
  ( TextInputState (..)
  , textInputLayout
  , searchFieldLayout
  , processTextInput
  , processSelectableTextInput
  , applyTextInputMenuAction
  , isSelectableTextInput
  , selectableText
  , selectableTextWith
  , selectableTextEx
  )
where

import Control.Monad (when)
import Data.Bits ((.|.))
import Data.Char (isPrint)
import Data.IORef (writeIORef)
import Data.IntMap.Strict qualified as IM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful (Eff, type (:>))
import NanoUI.Context
  ( Context (..)
  , getStore
  , intKey
  , markDirty
  , registerFocusable
  , setStore
  , setTextInputMenu
  )
import NanoUI.Frame.Hit (findNodeByWidgetId)
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
import NanoUI.Layout.Arena (NodeType (..), getNodeType, getStyleIdx)
import NanoUI.Monad (Ui, askContext, askDefaultLayout, askInput, nextId, uiIO)
import NanoUI.Store (WidgetStore (..), slotAnchor, slotCursor, slotKey)
import NanoUI.Style (Layout (..), Sizing (..), defaultLayout)
import NanoUI.WidgetText (packTextNodeStyleFull, textInputFlagSelectable, textInputSelectableMode)
import NanoUI.Widgets.Behavior (keyboardFocused)
import NanoUI.Widgets.Node (Response (..), addWidgetStyled)
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

-- | True when the widget is a text input in selectable (read-only label) mode.
isSelectableTextInput :: Context -> WidgetId -> IO Bool
isSelectableTextInput ctx wid =
  findNodeByWidgetId ctx wid >>= \case
    Nothing -> pure False
    Just idx -> do
      nt <- getNodeType (ctxNodeArena ctx) idx
      if nt /= NodeTextInput
        then pure False
        else textInputSelectableMode <$> getStyleIdx (ctxNodeArena ctx) idx

applyTextInputMenuAction :: Context -> WidgetId -> Int -> IO ()
applyTextInputMenuAction ctx wid item = do
  isSelectable <- isSelectableTextInput ctx wid
  store <- getStore ctx
  let
    key = intKey wid
    text = IM.findWithDefault "" key (storeText store)
    cursor = IM.findWithDefault (T.length text) (slotKey slotCursor key) (storeInt store)
    anchor = IM.findWithDefault cursor (slotKey slotAnchor key) (storeInt store)
    s0 = TextInputState text cursor anchor
  s1 <-
    let
      cutF = if isSelectable then pure else textInputCut ctx
      pasteF = if isSelectable then pure else textInputPaste ctx
     in
      dispatchMenuAction
        cutF
        (textInputCopy ctx)
        pasteF
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
      then T.foldlM' (handleCtrlChar ctx False) s0 chars
      else pure s0
  let
    filtered = T.filter (\ch -> not (isCtrlCombo ctrl ch) && isPrint ch && ch /= '\n') chars
    s2 = insertText s1 filtered
    -- Word-wise editing keys, matching the text area: Ctrl or Alt plus
    -- Backspace/Delete/Left/Right works on words instead of characters.
    word = ctrl || alt
  pure (foldInputKeys (applyKey word shift) s2 keys)

handleCtrlChar :: Context -> Bool -> TextInputState -> Char -> IO TextInputState
handleCtrlChar ctx isSelectable =
  dispatchCtrlChar
    (pure . selectAllTextInput)
    (textInputCopy ctx)
    (if isSelectable then pure else textInputCut ctx)
    (if isSelectable then pure else textInputPaste ctx)

-- | Read-only input processing for selectable text labels: allows select-all (Ctrl+A),
-- copying (Ctrl+C), and arrow/Home/End navigation (with Shift selection), but
-- ignores character insertion, backspace, delete, cut, and paste.
processSelectableTextInput :: Context -> Input -> TextInputState -> IO TextInputState
processSelectableTextInput ctx inp s0 = do
  let
    mods = inputModifiers inp
    ctrl = modCtrl mods
    alt = modAlt mods
    shift = modShift mods
    keys = inputKeys inp
    chars = inputChars inp
  s1 <-
    if ctrl
      then T.foldlM' (handleCtrlChar ctx True) s0 chars
      else pure s0
  let word = ctrl || alt
  pure (foldInputKeys (applyNavKeyState word shift) s1 keys)

-- | State-level navigation for a fresh buffer; delegates to the shared core.
applyNavKeyState :: Bool -> Bool -> TextInputState -> Key -> TextInputState
applyNavKeyState word shift s key =
  let (buf, anc) = toBuffer s
   in applyNavKey word shift buf anc s key

-- | Navigation keys on an already-opened buffer, shared by editable and
-- read-only text inputs so `applyKey` reuses the buffer it just built.
applyNavKey :: Bool -> Bool -> TB.TextBuffer -> TB.Cursor -> TextInputState -> Key -> TextInputState
applyNavKey word shift buf anc s key =
  case key of
    KeyLeft
      | word -> moveWith shift buf anc TB.moveWordLeft
      | otherwise -> moveWith shift buf anc TB.moveLeft
    KeyRight
      | word -> moveWith shift buf anc TB.moveWordRight
      | otherwise -> moveWith shift buf anc TB.moveRight
    KeyHome -> moveWith shift buf anc TB.moveToBOL
    KeyEnd -> moveWith shift buf anc TB.moveToEOL
    _ -> s

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
      _ -> applyNavKey word shift buf anc s key

moveWith ::
  Bool
  -> TB.TextBuffer
  -> TB.Cursor
  -> (TB.TextBuffer -> TB.TextBuffer)
  -> TextInputState
moveWith shift buf anc f =
  fromBuffer (if shift then Just (TB.cursorCol anc) else Nothing) (f buf)

-- | Selectable text label: displays text that can be highlighted/selected with the
-- mouse and copied to the clipboard with Ctrl+C, but cannot be edited.
selectableText :: Ui :> es => Text -> Eff es Response
selectableText = selectableTextWith id

-- | Selectable text label with a layout modifier.
selectableTextWith :: Ui :> es => (Layout -> Layout) -> Text -> Eff es Response
selectableTextWith f txt = do
  base <- askDefaultLayout
  selectableTextEx (f base) txt

-- | Selectable text label with an explicit layout.
selectableTextEx :: Ui :> es => Layout -> Text -> Eff es Response
selectableTextEx layout txt = do
  wid <- nextId
  ctx <- askContext
  uiIO $ registerFocusable ctx wid
  inp <- askInput
  store <- uiIO (getStore ctx)
  let key = intKey wid
      newLen = T.length txt
      oldTxt = IM.findWithDefault "" key (storeText store)
      store' =
        if oldTxt /= txt
          then
            let cur = min newLen (fromMaybe newLen (IM.lookup (slotKey slotCursor key) (storeInt store)))
                anc = min newLen (fromMaybe cur (IM.lookup (slotKey slotAnchor key) (storeInt store)))
             in store
                  { storeText = IM.insert key txt (storeText store)
                  , storeInt =
                      IM.insert (slotKey slotCursor key) cur $
                        IM.insert (slotKey slotAnchor key) anc (storeInt store)
                  }
          else store
  when (oldTxt /= txt) $ uiIO $ setStore ctx store'
  let cursor = min newLen (fromMaybe newLen (IM.lookup (slotKey slotCursor key) (storeInt store')))
      anchor = min newLen (fromMaybe cursor (IM.lookup (slotKey slotAnchor key) (storeInt store')))
  isFocus <- keyboardFocused wid
  when isFocus $ do
    newState <- uiIO (processSelectableTextInput ctx inp (TextInputState txt cursor anchor))
    let newCursor = min newLen (tisCursor newState)
        newAnchor = min newLen (tisAnchor newState)
    when (newCursor /= cursor || newAnchor /= anchor) $
      uiIO $ setStore ctx (store'
        { storeInt = IM.insert (slotKey slotCursor key) newCursor
                   $ IM.insert (slotKey slotAnchor key) newAnchor (storeInt store')
        })
  let styleIdx =
        textInputFlagSelectable
          .|. packTextNodeStyleFull
                (layoutFontVariant layout)
                (layoutFontWeight layout)
                (layoutFontStyle layout)
                (layoutTextDecoration layout)
                0
  addWidgetStyled wid NodeTextInput txt 0 layout styleIdx Nothing

