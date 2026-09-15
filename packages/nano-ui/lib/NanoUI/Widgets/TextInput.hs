-- | Single-line text fields: editable inputs, debounced search fields, and
-- selectable read-only labels, plus the key handling they share.
module NanoUI.Widgets.TextInput
  ( TextInputState (..)
  , loadTextInputState
  , saveTextInputState
  , textInputLayout
  , searchFieldLayout
  , processTextInput
  , processSelectableTextInput
  , applyTextInputMenuAction
  , isSelectableTextInput
    -- * Text fields
  , TextInputConfig (..)
  , defaultTextInputConfig
  , textInput
  , textInputConfigured
  , textInputWithPlaceholder
  , textInputPassword
  , SearchFieldConfig (..)
  , defaultSearchFieldConfig
  , searchField
  , searchFieldConfigured
  , buildTextInput
  , editTextField
    -- * Selectable text
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
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful (Eff, type (:>))
import GHC.Clock (getMonotonicTime)
import NanoUI.Context
  ( Context (..)
  , getStore
  , intKey
  , markDirty
  , registerFocusable
  , setStore
  , setTextInputMenu
  , writeStoreText
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
import NanoUI.Store (WidgetStore (..), slotAnchor, slotCursor, slotKey, slotSearchAge, slotSearchCommitted)
import NanoUI.Style (Layout (..), Sizing (..), defaultLayout)
import NanoUI.WidgetText (packTextNodeStyleFull, textInputFlagPassword, textInputFlagSearch, textInputFlagSelectable, textInputPasswordMode, textInputSelectableMode)
import NanoUI.Widgets.Behavior (keyboardFocused)
import NanoUI.Widgets.Node (Response (..), addWidgetStyled, setChanged, setSubmitted)
import NanoUI.Widgets.TextBuffer qualified as TB
import NanoUI.Widgets.TextCommon
  ( MenuAction
  , copyBufferText
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

-- | A field's cursor and anchor for @text@; the cursor defaults to the end
-- and the anchor to the cursor.
loadTextInputState :: WidgetStore -> Int -> Text -> TextInputState
loadTextInputState store key text =
  let cursor = IM.findWithDefault (T.length text) (slotKey slotCursor key) (storeInt store)
      anchor = IM.findWithDefault cursor (slotKey slotAnchor key) (storeInt store)
   in TextInputState text cursor anchor

saveTextInputState :: Int -> TextInputState -> WidgetStore -> WidgetStore
saveTextInputState key s store =
  store
    { storeText = IM.insert key (tisText s) (storeText store)
    , storeInt =
        IM.insert (slotKey slotCursor key) (tisCursor s) $
          IM.insert (slotKey slotAnchor key) (tisAnchor s) (storeInt store)
    }

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
    TextInputState (TB.toText buf) c (fromMaybe c anchor)

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

applyTextInputMenuAction :: Context -> WidgetId -> MenuAction -> IO ()
applyTextInputMenuAction ctx wid action = do
  isSelectable <- isSelectableTextInput ctx wid
  store <- getStore ctx
  let
    key = intKey wid
    s0 = loadTextInputState store key (IM.findWithDefault "" key (storeText store))
    editing f = if isSelectable then pure else f ctx
  s1 <-
    dispatchMenuAction
      (editing textInputCut)
      (textInputCopy ctx)
      (editing textInputPaste)
      selectAllTextInput
      action
      s0
  setStore ctx (saveTextInputState key s1 store)
  -- Menu actions edit a field that may not be under the pointer; focus it so
  -- the selection highlight and caret become visible.
  writeIORef (ctxFocusId ctx) wid
  setTextInputMenu ctx Nothing
  markDirty ctx

-- | Key handling shared by editable and read-only fields: Ctrl shortcuts
-- first, then @keysPass@ with the word-motion flag (Ctrl or Alt) and Shift.
-- @canCopy@ is off for password fields and @canEdit@ for read-only labels;
-- cut needs both.
runTextInputKeys ::
  Context ->
  Bool ->
  Bool ->
  Input ->
  (Bool -> Bool -> TextInputState -> TextInputState) ->
  TextInputState ->
  IO TextInputState
runTextInputKeys ctx canCopy canEdit inp keysPass s0 = do
  let
    mods = inputModifiers inp
    ctrl = modCtrl mods
    ctrlChar =
      dispatchCtrlChar
        (pure . selectAllTextInput)
        (if canCopy then textInputCopy ctx else const (pure ()))
        (if canCopy && canEdit then textInputCut ctx else pure)
        (if canEdit then textInputPaste ctx else pure)
  s1 <-
    if ctrl
      then T.foldlM' ctrlChar s0 (inputChars inp)
      else pure s0
  pure (keysPass (ctrl || modAlt mods) (modShift mods) s1)

-- | Editable field input; @canCopy@ is off for password fields.
processTextInput :: Context -> Bool -> Input -> TextInputState -> IO TextInputState
processTextInput ctx canCopy inp =
  runTextInputKeys ctx canCopy True inp $ \word shift s1 ->
    let
      ctrl = modCtrl (inputModifiers inp)
      typed = T.filter (\ch -> not (isCtrlCombo ctrl ch) && isPrint ch && ch /= '\n') (inputChars inp)
     in
      foldInputKeys (applyKey word shift) (insertText s1 typed) (inputKeys inp)

-- | Read-only input processing for selectable text labels: allows select-all (Ctrl+A),
-- copying (Ctrl+C), and arrow/Home/End navigation (with Shift selection), but
-- ignores character insertion, backspace, delete, cut, and paste.
processSelectableTextInput :: Context -> Input -> TextInputState -> IO TextInputState
processSelectableTextInput ctx inp =
  runTextInputKeys ctx True False inp $ \word shift s1 ->
    foldInputKeys (applyNavKeyState word shift) s1 (inputKeys inp)

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

-- -----------------------------------------------------------------------------
-- Text fields
-- -----------------------------------------------------------------------------

data TextInputConfig = TextInputConfig
  { ticPlaceholder :: !Text
  , ticPassword :: !Bool
  , ticLayout :: !Layout
  }
  deriving (Eq, Show)

defaultTextInputConfig :: TextInputConfig
defaultTextInputConfig =
  TextInputConfig
    { ticPlaceholder = ""
    , ticPassword = False
    , ticLayout = textInputLayout
    }

textInput :: Ui :> es => Text -> Eff es (Response, Text)
textInput = textInputConfigured defaultTextInputConfig

textInputWithPlaceholder :: Ui :> es => Text -> Text -> Eff es (Response, Text)
textInputWithPlaceholder placeholder initial =
  textInputConfigured (defaultTextInputConfig {ticPlaceholder = placeholder}) initial

textInputPassword :: Ui :> es => Text -> Eff es (Response, Text)
textInputPassword initial =
  textInputConfigured (defaultTextInputConfig {ticPassword = True}) initial

textInputConfigured :: Ui :> es => TextInputConfig -> Text -> Eff es (Response, Text)
textInputConfigured cfg initial =
  buildTextInput
    (if ticPassword cfg then textInputFlagPassword else 0)
    (ticLayout cfg)
    (ticPlaceholder cfg)
    initial
    Nothing

-- | One frame of a single-line field's text state: load the text (seeding
-- @initial@ on first use) with its cursor and anchor, run the editor while
-- focused, and save any change. While unfocused, @unfocusedText@ (when given)
-- replaces the stored text, so a field that mirrors another value follows it.
-- A @password@ field never copies or cuts to the clipboard.
-- Returns the text before and after this frame, and whether it is focused.
editTextField :: Ui :> es => WidgetId -> Bool -> Text -> Maybe Text -> Eff es (Text, Text, Bool)
editTextField wid password initial unfocusedText = do
  ctx <- askContext
  uiIO $ registerFocusable ctx wid
  inp <- askInput
  store <- uiIO (getStore ctx)
  let
    key = intKey wid
    stored = IM.lookup key (storeText store)
    s0 = loadTextInputState store key (fromMaybe initial stored)
  when (isNothing stored) $
    uiIO $ writeStoreText ctx wid key initial
  isFocus <- keyboardFocused wid
  s1 <-
    if isFocus
      then uiIO (processTextInput ctx (not password) inp s0)
      else pure (maybe s0 (\t -> s0 {tisText = t}) unfocusedText)
  when (s1 /= s0) $
    uiIO $ setStore ctx (saveTextInputState key s1 store)
  pure (tisText s0, tisText s1, isFocus)

-- | Shared single-line field builder. @styleIdx@ may carry the search or
-- password flag on a @NodeTextInput@; when @mDebounceMs@ is present the returned
-- change pulse is delayed until the text has been idle for that long (immediate
-- for clear clicks).
buildTextInput ::
  Ui :> es =>
  Int ->
  Layout ->
  Text ->
  Text ->
  Maybe Float ->
  Eff es (Response, Text)
buildTextInput styleIdx layout placeholder initial mDebounceMs = do
  wid <- nextId
  (oldText, newText, isFocus) <- editTextField wid (textInputPasswordMode styleIdx) initial Nothing
  ctx <- askContext
  inp <- askInput
  let submitted = isFocus && KeyEnter `elem` inputKeys inp
  changed <- case mDebounceMs of
    Nothing -> pure (newText /= oldText)
    Just ms -> uiIO (debounceSearchChanged ctx (intKey wid) isFocus (newText /= oldText) ms)
  resp <- addWidgetStyled wid NodeTextInput placeholder 0 layout styleIdx Nothing
  pure (setSubmitted submitted (setChanged changed resp), newText)

-- | Debounced change pulse for a search field. Fires when the text differs from
-- the last committed query and either the field is empty, lost focus, or has
-- been idle for @ms@ (trailing edge). Field text lives under @key@; the last
-- committed query under 'slotSearchCommitted'.
debounceSearchChanged :: Context -> Int -> Bool -> Bool -> Float -> IO Bool
debounceSearchChanged ctx key focused rawChanged ms = do
  store <- getStore ctx
  let
    committedKey = slotKey slotSearchCommitted key
    ageKey = slotKey slotSearchAge key
    fieldText = IM.findWithDefault "" key (storeText store)
    committedMissing = not (IM.member committedKey (storeText store))
    committed = IM.findWithDefault fieldText committedKey (storeText store)
    dirty = fieldText /= committed
    needClock = rawChanged || dirty
  now <- if needClock then getMonotonicTime else pure 0
  let
    -- Debounce timing stays in Double: wall-clock seconds as Float lose
    -- resolution at long uptimes (~125 ms at 12 days), which would shift
    -- (or skip) the trailing-edge window.
    lastEdit = IM.findWithDefault now ageKey (storeDouble store)
    deadline = realToFrac ms :: Double
    idleMs = (now - lastEdit) * 1000
    commit =
      not rawChanged
        && dirty
        && (T.null fieldText || not focused || idleMs >= deadline)
  when (rawChanged || commit || committedMissing) $
    getStore ctx >>= \st -> setStore ctx $
      st
        { storeText =
            if commit || committedMissing
              then IM.insert committedKey fieldText (storeText st)
              else storeText st
        , storeDouble =
            if rawChanged || commit
              then IM.insert ageKey now (storeDouble st)
              else storeDouble st
        }
  pure commit

-- | Search field: a caption-less 'NodeTextInput' with an embedded magnifier and
-- clear button. The label acts as the placeholder. Change pulses are debounced
-- (trailing edge); clearing with the embedded button fires immediately.
data SearchFieldConfig = SearchFieldConfig
  { sfcPlaceholder :: !Text
  , sfcDebounceMs :: !Float
  , sfcLayout :: !Layout
  }
  deriving (Eq, Show)

defaultSearchFieldConfig :: SearchFieldConfig
defaultSearchFieldConfig =
  SearchFieldConfig
    { sfcPlaceholder = "Search…"
    , sfcDebounceMs = 300
    , sfcLayout = searchFieldLayout
    }

searchField :: Ui :> es => Text -> Text -> Eff es (Response, Text)
searchField placeholder initial =
  searchFieldConfigured (defaultSearchFieldConfig {sfcPlaceholder = placeholder}) initial

searchFieldConfigured ::
  Ui :> es => SearchFieldConfig -> Text -> Eff es (Response, Text)
searchFieldConfigured cfg initial =
  buildTextInput
    textInputFlagSearch
    (sfcLayout cfg)
    (sfcPlaceholder cfg)
    initial
    (Just (sfcDebounceMs cfg))

-- -----------------------------------------------------------------------------
-- Selectable text
-- -----------------------------------------------------------------------------

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
      clampToText s = s {tisCursor = min newLen (tisCursor s), tisAnchor = min newLen (tisAnchor s)}
      s0 = clampToText (loadTextInputState store key txt)
  -- The caller owns the text: store it (with the clamped caret) when it changes.
  when (IM.findWithDefault "" key (storeText store) /= txt) $
    uiIO $ setStore ctx (saveTextInputState key s0 store)
  isFocus <- keyboardFocused wid
  when isFocus $ do
    s1 <- clampToText <$> uiIO (processSelectableTextInput ctx inp s0)
    when (s1 /= s0) $
      uiIO $ getStore ctx >>= setStore ctx . saveTextInputState key s1
  let styleIdx =
        textInputFlagSelectable
          .|. packTextNodeStyleFull
                (layoutFontVariant layout)
                (layoutFontWeight layout)
                (layoutFontStyle layout)
                (layoutTextDecoration layout)
                0
  addWidgetStyled wid NodeTextInput txt 0 layout styleIdx Nothing
