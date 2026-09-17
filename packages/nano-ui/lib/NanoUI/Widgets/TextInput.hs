-- | Single-line text fields: editable inputs, debounced search fields, and
-- selectable read-only labels, plus the key handling they share.
module NanoUI.Widgets.TextInput
  ( TextInputState (..)
  , loadTextInputState
  , saveTextInputState
  , textInputLayout
  , searchFieldLayout
  , textInputEditor
  , editorTextState
  , saveTextEditor
  , inputTextCommands
  , editTextInput
  , textInputMode
  , applyTextInputCommand
  , isSelectableTextInput
    -- * Text fields
  , TextInputConfig (..)
  , defaultTextInputConfig
  , textInput
  , textInput'
  , textInputConfigured
  , textInputConfigured'
  , SearchFieldConfig (..)
  , defaultSearchFieldConfig
  , searchField
  , searchField'
  , searchFieldConfigured
  , searchFieldConfigured'
  , buildTextInput
  , editTextField
    -- * Selectable text
  , selectableText
  , selectableText'
  , selectableTextWith
  , selectableTextWith'
  )
where

import Control.Monad (foldM, void, when)
import Data.Bits ((.|.))
import Data.Char (isPrint)
import Data.IntMap.Strict qualified as IM
import Data.Dynamic (fromDynamic, toDyn)
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful (Eff, type (:>))
import GHC.Clock (getMonotonicTime)
import NanoUI.Context
  ( Context (..)
  , adoptStoreText
  , getStore
  , intKey
  , markDirty
  , recordStoreText
  , registerFocusable
  , setStore
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
import NanoUI.Store (WidgetStore (..), slotAnchor, slotCursor, slotKey, slotSearchAge, slotSearchCommitted, slotTextAreaChanged, slotTextHistory, slotTextMode)
import NanoUI.Style (Layout (..), Sizing (..), defaultLayout)
import NanoUI.WidgetText (packTextNodeStyleFull, textInputFlagPassword, textInputFlagSearch, textInputFlagSelectable, textInputPasswordMode, textInputSelectableMode)
import NanoUI.Widgets.Behavior (keyboardFocused)
import NanoUI.Widgets.Node (Response (..), addWidgetStyled, setChanged, setSubmitted)
import NanoUI.Widgets.TextBuffer qualified as TB
import NanoUI.Widgets.TextEditor
  ( Editor (..)
  , EditorMode (..)
  , TextCommand (..)
  , ctrlCharCommand
  , editorModeCode
  , emptyHistory
  , keyCommand
  , runCommandIO
  , sealHistory
  , singleLineMode
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

-- | The editor for a field's state, with the undo history stored for it. A
-- history recorded against other text (the caller replaced the value) is
-- dropped.
textInputEditor :: WidgetStore -> Int -> TextInputState -> Editor
textInputEditor store key s =
  let buf = TB.withCursor (TB.Cursor 0 (tisCursor s)) (TB.fromText (tisText s))
      history = case IM.lookup (slotKey slotTextHistory key) (storeDyn store) >>= fromDynamic of
        Just (text, h) | text == tisText s -> h
        _ -> emptyHistory
   in Editor buf (TB.clampCursor buf (TB.Cursor 0 (tisAnchor s))) history

editorTextState :: Editor -> TextInputState
editorTextState ed =
  let buf = editorBuffer ed
   in TextInputState (TB.toText buf) (TB.cursorCol (TB.getCursor buf)) (TB.cursorCol (editorAnchor ed))

-- | Store an editor's text, selection and history.
saveTextEditor :: Int -> Editor -> WidgetStore -> WidgetStore
saveTextEditor key ed store =
  let s = editorTextState ed
      saved = saveTextInputState key s store
   in saved {storeDyn = IM.insert (slotKey slotTextHistory key) (toDyn (tisText s, editorHistory ed)) (storeDyn saved)}

-- | This frame's typing and shortcuts as commands, typed characters first.
-- Ctrl turns characters into shortcuts; a line break never enters a
-- single-line field.
inputTextCommands :: EditorMode -> Input -> [TextCommand]
inputTextCommands mode inp =
  let mods = inputModifiers inp
      ctrl = modCtrl mods
      chars
        | ctrl = mapMaybe (ctrlCharCommand mode mods) (T.unpack (inputChars inp))
        | otherwise =
            [InsertText (T.singleton ch) | ch <- T.unpack (inputChars inp), isPrint ch, ch /= '\n']
   in chars ++ foldInputKeys (\acc k -> acc ++ maybe [] pure (keyCommand mode mods k)) [] (inputKeys inp)

-- | Run this frame's commands on a field, or 'Nothing' when it had none.
editTextInput :: Context -> EditorMode -> Input -> WidgetStore -> Int -> TextInputState -> IO (Maybe Editor)
editTextInput ctx mode inp store key s0 =
  case inputTextCommands mode inp of
    [] -> pure Nothing
    cmds -> Just <$> foldM (flip (runCommandIO ctx mode)) (textInputEditor store key s0) cmds

-- | The editor mode of a single-line field with these style flags.
textInputMode :: Int -> EditorMode
textInputMode si =
  singleLineMode
    { modeEditable = not (textInputSelectableMode si)
    , modeCopyable = not (textInputPasswordMode si)
    }

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

-- | Run a command on a single-line field outside its frame (a context menu
-- row, an app's Edit menu). A change to the text pulses 'respChanged' on the
-- field's next frame.
applyTextInputCommand :: Context -> WidgetId -> EditorMode -> TextCommand -> IO ()
applyTextInputCommand ctx wid mode cmd = do
  store <- getStore ctx
  let
    key = intKey wid
    s0 = loadTextInputState store key (IM.findWithDefault "" key (storeText store))
  let ed0 = textInputEditor store key s0
  ed <- runCommandIO ctx mode cmd ed0 {editorHistory = sealHistory (editorHistory ed0)}
  let s1 = editorTextState ed
      saved = saveTextEditor key ed store
  setStore ctx $
    if tisText s1 /= tisText s0
      then saved {storeInt = IM.insert (slotKey slotTextAreaChanged key) 1 (storeInt saved)}
      else saved
  markDirty ctx

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

-- | Single-line text field. Pass the current text; the result is the text
-- after this frame's typing, pastes, and menu edits.
{-# INLINE textInput #-}
textInput :: Ui :> es => Text -> Eff es Text
textInput value = snd <$> textInputConfigured' defaultTextInputConfig value

{-# INLINE textInput' #-}
textInput' :: Ui :> es => Text -> Eff es (Response, Text)
textInput' = textInputConfigured' defaultTextInputConfig

-- | 'textInput' with a placeholder, password masking, or its own layout.
--
-- @
-- secret' <- textInputConfigured defaultTextInputConfig {ticPassword = True} secret
-- @
{-# INLINE textInputConfigured #-}
textInputConfigured :: Ui :> es => TextInputConfig -> Text -> Eff es Text
textInputConfigured cfg value = snd <$> textInputConfigured' cfg value

textInputConfigured' :: Ui :> es => TextInputConfig -> Text -> Eff es (Response, Text)
textInputConfigured' cfg value =
  buildTextInput
    (if ticPassword cfg then textInputFlagPassword else 0)
    (ticLayout cfg)
    (ticPlaceholder cfg)
    value
    Nothing

-- | One frame of a single-line field's text state: load the text (seeding
-- @initial@ on first use) with its cursor and anchor, run the editor while
-- focused, and save any change. While unfocused, @unfocusedText@ (when given)
-- replaces the stored text, so a field that mirrors another value follows it.
-- A @password@ field never copies or cuts to the clipboard.
-- Returns the text before and after this frame, whether it is focused, and
-- whether a command run from outside the frame changed it.
editTextField :: Ui :> es => WidgetId -> Bool -> Text -> Maybe Text -> Eff es (Text, Text, Bool, Bool)
editTextField wid password initial unfocusedText = do
  ctx <- askContext
  uiIO $ registerFocusable ctx wid
  inp <- askInput
  store <- uiIO (getStore ctx)
  let
    key = intKey wid
    mode = singleLineMode {modeCopyable = not password}
    modeKey = slotKey slotTextMode key
    pulseKey = slotKey slotTextAreaChanged key
    stored = IM.lookup key (storeText store)
    text0 = fromMaybe initial stored
    -- Text replaced from outside the field can be shorter than the caret.
    len0 = T.length text0
    loaded = loadTextInputState store key text0
    s0 = loaded {tisCursor = min len0 (tisCursor loaded), tisAnchor = min len0 (tisAnchor loaded)}
    pulse = IM.member pulseKey (storeInt store)
  when (isNothing stored || IM.lookup modeKey (storeInt store) /= Just (editorModeCode mode) || pulse) $
    uiIO $ getStore ctx >>= \st -> setStore ctx st
      { storeText = if isNothing stored then IM.insert key initial (storeText st) else storeText st
      , storeInt = IM.delete pulseKey (IM.insert modeKey (editorModeCode mode) (storeInt st))
      }
  isFocus <- keyboardFocused wid
  mEdited <- if isFocus then uiIO (editTextInput ctx mode inp store key s0) else pure Nothing
  let s1 = case mEdited of
        Just ed -> editorTextState ed
        Nothing -> maybe s0 (\t -> s0 {tisText = t}) unfocusedText
  when (s1 /= s0) $
    uiIO $ getStore ctx >>= setStore ctx . maybe (saveTextInputState key s1) (saveTextEditor key) mEdited
  pure (tisText s0, tisText s1, isFocus, pulse)

-- | Shared single-line field builder. The caller's @value@ is adopted as by
-- 'NanoUI.Context.adoptStoreText'. @styleIdx@ may carry the search or password
-- flag on a @NodeTextInput@; when @mDebounceMs@ is present the returned change
-- pulse is delayed until the text has been idle for that long (immediate for
-- clear clicks).
buildTextInput ::
  Ui :> es =>
  Int ->
  Layout ->
  Text ->
  Text ->
  Maybe Float ->
  Eff es (Response, Text)
buildTextInput styleIdx layout placeholder value mDebounceMs = do
  wid <- nextId
  ctx <- askContext
  let key = intKey wid
  uiIO $ adoptStoreText ctx wid key value
  (oldText, newText, isFocus, pulse) <- editTextField wid (textInputPasswordMode styleIdx) value Nothing
  uiIO $ recordStoreText ctx key newText
  inp <- askInput
  let submitted = isFocus && KeyEnter `elem` inputKeys inp
      edited = pulse || newText /= oldText
  changed <- case mDebounceMs of
    Nothing -> pure edited
    Just ms -> uiIO (debounceSearchChanged ctx key isFocus edited ms)
  resp <- addWidgetStyled wid NodeTextInput placeholder 0 layout styleIdx
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

-- | Search box with a magnifier and a clear button; the first argument is the
-- placeholder. Pass the current text; the result is the text after this
-- frame. 'respChanged' on 'searchField'' is debounced: it fires once typing
-- pauses, or at once when the field is cleared.
{-# INLINE searchField #-}
searchField :: Ui :> es => Text -> Text -> Eff es Text
searchField placeholder value = snd <$> searchField' placeholder value

{-# INLINE searchField' #-}
searchField' :: Ui :> es => Text -> Text -> Eff es (Response, Text)
searchField' placeholder =
  searchFieldConfigured' (defaultSearchFieldConfig {sfcPlaceholder = placeholder})

{-# INLINE searchFieldConfigured #-}
searchFieldConfigured :: Ui :> es => SearchFieldConfig -> Text -> Eff es Text
searchFieldConfigured cfg value = snd <$> searchFieldConfigured' cfg value

searchFieldConfigured' ::
  Ui :> es => SearchFieldConfig -> Text -> Eff es (Response, Text)
searchFieldConfigured' cfg value =
  buildTextInput
    textInputFlagSearch
    (sfcLayout cfg)
    (sfcPlaceholder cfg)
    value
    (Just (sfcDebounceMs cfg))

-- -----------------------------------------------------------------------------
-- Selectable text
-- -----------------------------------------------------------------------------

-- | Read-only text that can be selected with the mouse and copied with Ctrl+C.
{-# INLINE selectableText #-}
selectableText :: Ui :> es => Text -> Eff es ()
selectableText = selectableTextWith id

{-# INLINE selectableText' #-}
selectableText' :: Ui :> es => Text -> Eff es Response
selectableText' = selectableTextWith' id

{-# INLINE selectableTextWith #-}
selectableTextWith :: Ui :> es => (Layout -> Layout) -> Text -> Eff es ()
selectableTextWith f txt = void (selectableTextWith' f txt)

selectableTextWith' :: Ui :> es => (Layout -> Layout) -> Text -> Eff es Response
selectableTextWith' f txt = do
  layout <- f <$> askDefaultLayout
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
  let modeKey = slotKey slotTextMode key
      modeCode = editorModeCode singleLineMode {modeEditable = False}
  when (IM.findWithDefault "" key (storeText store) /= txt || IM.lookup modeKey (storeInt store) /= Just modeCode) $
    uiIO $ setStore ctx (saveTextInputState key s0 store) {storeInt = IM.insert modeKey modeCode (storeInt (saveTextInputState key s0 store))}
  isFocus <- keyboardFocused wid
  when isFocus $ do
    let mode = singleLineMode {modeEditable = False}
    mEdited <- uiIO (editTextInput ctx mode inp store key s0)
    let s1 = maybe s0 (clampToText . editorTextState) mEdited
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
  addWidgetStyled wid NodeTextInput txt 0 layout styleIdx
