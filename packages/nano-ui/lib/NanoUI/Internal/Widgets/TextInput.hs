-- | Single-line text fields: editable inputs, debounced search fields, and
-- selectable read-only labels, plus the key handling they share.
module NanoUI.Internal.Widgets.TextInput
  ( TextInputState (..)
  , loadTextInputState
  , saveTextInputState
  , textInputLayout
  , searchInputLayout
  , textInputEditor
  , editorTextState
  , saveTextEditor
  , editTextInput
  , textInputMode
  , applyTextInputCommand
    -- * Text fields
  , TextInputConfig (..)
  , defaultTextInputConfig
  , textInput
  , textInput'
  , textInputConfigured
  , textInputConfigured'
  , SearchInputConfig (..)
  , defaultSearchInputConfig
  , searchInput
  , searchInput'
  , searchInputConfigured
  , searchInputConfigured'
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
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful (Eff, type (:>))
import GHC.Clock (getMonotonicTime)
import NanoUI.Internal.Context
  ( Context (..)
  , adoptStoreText
  , getStore
  , intKey
  , markDirty
  , recordStoreText
  , registerFocusable
  , requestWakeAt
  , setStore
  , modifyStore
  )
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Input
  ( Input (..)
  , Key (..)
  , inputKeys
  )
import NanoUI.Internal.Layout.Arena (NodeType (..))
import NanoUI.Internal.Monad (Ui, askContext, askDefaultLayout, askInput, nextId, uiIO)
import NanoUI.Internal.Store
  ( Slot (..)
  , WidgetStore
  , deleteSlot
  , fieldDouble
  , fieldInt
  , fieldText
  , findSlot
  , insertDyn
  , insertSlot
  , lookupDyn
  , lookupSlot
  , memberSlot
  , slotKey
  )
import NanoUI.Internal.Style (Layout (..), defaultLayout, fillW, minW)
import NanoUI.Internal.WidgetText (packTextNodeStyleFull, textInputFlagPassword, textInputFlagSearch, textInputFlagSelectable, textInputPasswordMode, textInputSelectableMode)
import NanoUI.Internal.Widgets.Behavior (keyboardFocused)
import NanoUI.Internal.Widgets.Node (Response (..), addWidgetStyled, setChanged, setSubmitted)
import NanoUI.Widgets.TextBuffer qualified as TB
import NanoUI.Internal.Widgets.TextEditor
  ( Editor (..)
  , EditorMode (..)
  , TextCommand (..)
  , inputTextCommands
  , editorModeCode
  , emptyHistory
  , runCommandIO
  , sealHistory
  , singleLineMode
  )

textInputLayout :: Layout
textInputLayout = minW 160 (fillW defaultLayout)

-- | Layout for a caption-less search field. Grows to fill, keeps a little more
-- room for the embedded magnifier / clear chrome than a plain text input.
searchInputLayout :: Layout
searchInputLayout = minW 180 (fillW defaultLayout)

data TextInputState = TextInputState
  { tisText :: !Text
  , tisCursor :: !Int
  , tisAnchor :: !Int
  }
  deriving (Eq, Show)

-- | A field's cursor and anchor for @text@; the cursor defaults to the end
-- and the anchor to the cursor. Both are clamped to the text, which can have
-- been replaced from outside the field with a shorter one.
loadTextInputState :: WidgetStore -> Int -> Text -> TextInputState
loadTextInputState store key text =
  let len = T.length text
      cursor = min len (findSlot fieldInt len (slotKey SlotCursor key) store)
      anchor = min len (findSlot fieldInt cursor (slotKey SlotAnchor key) store)
   in TextInputState text cursor anchor

saveTextInputState :: Int -> TextInputState -> WidgetStore -> WidgetStore
saveTextInputState key s =
  insertSlot fieldText key (tisText s)
    . insertSlot fieldInt (slotKey SlotCursor key) (tisCursor s)
    . insertSlot fieldInt (slotKey SlotAnchor key) (tisAnchor s)

-- | The editor for a field's state, with the undo history stored for it. A
-- history recorded against other text (the caller replaced the value) is
-- dropped.
textInputEditor :: WidgetStore -> Int -> TextInputState -> Editor
textInputEditor store key s =
  let buf = TB.withCursor (TB.Cursor 0 (tisCursor s)) (TB.fromText (tisText s))
      history = case lookupDyn (slotKey SlotTextHistory key) store of
        Just (text, h) | text == tisText s -> h
        _ -> emptyHistory
   in Editor buf (TB.clampCursor buf (TB.Cursor 0 (tisAnchor s))) history

editorTextState :: Editor -> TextInputState
editorTextState ed =
  let buf = editorBuffer ed
   in TextInputState (TB.toText buf) (TB.cursorCol (TB.getCursor buf)) (TB.cursorCol (editorAnchor ed))

-- | Store an editor's text, selection and history.
saveTextEditor :: Int -> Editor -> WidgetStore -> WidgetStore
saveTextEditor key ed =
  let s = editorTextState ed
   in insertDyn (slotKey SlotTextHistory key) (tisText s, editorHistory ed) . saveTextInputState key s

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

-- | Run a command on a single-line field outside its frame (a context menu
-- row, an app's Edit menu). A change to the text pulses @respChanged@ on the
-- field's next frame.
applyTextInputCommand :: Context -> WidgetId -> EditorMode -> TextCommand -> IO ()
applyTextInputCommand ctx wid mode cmd = do
  store <- getStore ctx
  let
    key = intKey wid
    s0 = loadTextInputState store key (findSlot fieldText "" key store)
  let ed0 = textInputEditor store key s0
  ed <- runCommandIO ctx mode cmd ed0 {editorHistory = sealHistory (editorHistory ed0)}
  let s1 = editorTextState ed
      saved = saveTextEditor key ed store
  setStore ctx $
    if tisText s1 /= tisText s0
      then insertSlot fieldInt (slotKey SlotTextAreaChanged key) 1 saved
      else saved
  markDirty ctx

-- -----------------------------------------------------------------------------
-- Text fields
-- -----------------------------------------------------------------------------

-- | Placeholder, password-display masking, and layout for a single-line field.
-- Masking affects display; the caller and widget store still hold the original text.
data TextInputConfig = TextInputConfig
  { ticPlaceholder :: !Text
  , ticPassword :: !Bool
  , ticLayout :: !Layout
  }
  deriving (Eq, Show)

-- | No placeholder, no password masking, and the standard text-input layout.
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

-- | 'textInput' returning @(response, updatedText)@, including change/submit flags.
{-# INLINE textInput' #-}
textInput' :: Ui :> es => Text -> Eff es (Response, Text)
textInput' = textInputConfigured' defaultTextInputConfig

-- | 'textInput' with a placeholder, password masking, or its own layout.
--
-- > secret' <- textInputConfigured defaultTextInputConfig {ticPassword = True} secret
{-# INLINE textInputConfigured #-}
textInputConfigured :: Ui :> es => TextInputConfig -> Text -> Eff es Text
textInputConfigured cfg value = snd <$> textInputConfigured' cfg value

-- | 'textInputConfigured' returning @(response, updatedText)@.
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
-- Returns the text before and after this frame, whether it is focused, and
-- whether a command run from outside the frame changed it.
editTextField :: Ui :> es => WidgetId -> EditorMode -> Text -> Maybe Text -> Eff es (Text, Text, Bool, Bool)
editTextField wid mode initial unfocusedText = do
  ctx <- askContext
  uiIO $ registerFocusable ctx wid
  inp <- askInput
  store <- uiIO (getStore ctx)
  let
    key = intKey wid
    modeKey = slotKey SlotTextMode key
    pulseKey = slotKey SlotTextAreaChanged key
    stored = lookupSlot fieldText key store
    s0 = loadTextInputState store key (fromMaybe initial stored)
    pulse = memberSlot fieldInt pulseKey store
  when (isNothing stored || lookupSlot fieldInt modeKey store /= Just (editorModeCode mode) || pulse) $
    uiIO . modifyStore ctx $
      (if isNothing stored then insertSlot fieldText key initial else id)
        . deleteSlot fieldInt pulseKey
        . insertSlot fieldInt modeKey (editorModeCode mode)
  isFocus <- keyboardFocused wid
  mEdited <- if isFocus then uiIO (editTextInput ctx mode inp store key s0) else pure Nothing
  let s1 = case mEdited of
        Just ed -> editorTextState ed
        Nothing -> maybe s0 (\t -> s0 {tisText = t}) unfocusedText
  when (s1 /= s0) $
    uiIO $ modifyStore ctx (maybe (saveTextInputState key s1) (saveTextEditor key) mEdited)
  pure (tisText s0, tisText s1, isFocus, pulse)

-- | Shared single-line field builder. The caller's @value@ is adopted as by
-- 'NanoUI.Internal.Context.adoptStoreText'. @styleIdx@ may carry the search or password
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
  _ <- uiIO $ adoptStoreText ctx wid key value
  -- Both modes are constants, so an idle field allocates no mode record.
  let mode = if textInputPasswordMode styleIdx then singleLineMode {modeCopyable = False} else singleLineMode
  (oldText, newText, isFocus, pulse) <- editTextField wid mode value Nothing
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
-- committed query under 'SlotSearchCommitted'. While an edit waits to commit,
-- the frame that will commit it is scheduled with 'requestWakeAt'.
debounceSearchChanged :: Context -> Int -> Bool -> Bool -> Float -> IO Bool
debounceSearchChanged ctx key focused rawChanged ms = do
  store <- getStore ctx
  let
    committedKey = slotKey SlotSearchCommitted key
    ageKey = slotKey SlotSearchAge key
    current = findSlot fieldText "" key store
    committedMissing = not (memberSlot fieldText committedKey store)
    committed = findSlot fieldText current committedKey store
    dirty = current /= committed
    needClock = rawChanged || dirty
  now <- if needClock then getMonotonicTime else pure 0
  let
    -- Debounce timing stays in Double: wall-clock seconds as Float lose
    -- resolution at long uptimes (~125 ms at 12 days), which would shift
    -- (or skip) the trailing-edge window.
    lastEdit = findSlot fieldDouble now ageKey store
    deadline = realToFrac ms :: Double
    idleMs = (now - lastEdit) * 1000
    commit =
      not rawChanged
        && dirty
        && (T.null current || not focused || idleMs >= deadline)
    -- Text the caller changed, in a field nobody has typed in, has no edit
    -- time to age from. Its pause starts now: left unstamped, every frame
    -- would see a fresh edit, and the commit would never come.
    stamp = rawChanged || commit || (dirty && not (memberSlot fieldDouble ageKey store))
  when (stamp || committedMissing) $
    modifyStore ctx $
      (if commit || committedMissing then insertSlot fieldText committedKey current else id)
        . (if stamp then insertSlot fieldDouble ageKey now else id)
  -- An uncommitted edit commits once typing has paused for the deadline, and
  -- no input arrives to mark that moment. Ask for the frame that will see it.
  when (dirty && not commit) $
    requestWakeAt ctx ((if rawChanged then now else lastEdit) + deadline / 1000 + 0.001)
  pure commit

-- | Search field: a caption-less 'NodeTextInput' with an embedded magnifier and
-- clear button. The label acts as the placeholder. Change pulses are debounced
-- (trailing edge); clearing with the embedded button fires immediately.
data SearchInputConfig = SearchInputConfig
  { sicPlaceholder :: !Text
  , sicDebounceMs :: !Float
  , sicLayout :: !Layout
  }
  deriving (Eq, Show)

-- | Search placeholder, a 300 ms trailing debounce, and the standard search layout.
defaultSearchInputConfig :: SearchInputConfig
defaultSearchInputConfig =
  SearchInputConfig
    { sicPlaceholder = "Search…"
    , sicDebounceMs = 300
    , sicLayout = searchInputLayout
    }

-- | Search box with a magnifier and a clear button; the first argument is the
-- placeholder. Pass the current text; the result is the text after this
-- frame. @respChanged@ on 'searchInput'' is debounced: it fires once typing
-- pauses, or at once when the field is cleared.
{-# INLINE searchInput #-}
searchInput :: Ui :> es => Text -> Text -> Eff es Text
searchInput placeholder value = snd <$> searchInput' placeholder value

-- | 'searchInput' with a response. Text updates immediately; only the change
-- flag waits for the debounce interval.
{-# INLINE searchInput' #-}
searchInput' :: Ui :> es => Text -> Text -> Eff es (Response, Text)
searchInput' placeholder =
  searchInputConfigured' (defaultSearchInputConfig {sicPlaceholder = placeholder})

-- | Search field with explicit placeholder, debounce in milliseconds, and layout.
{-# INLINE searchInputConfigured #-}
searchInputConfigured :: Ui :> es => SearchInputConfig -> Text -> Eff es Text
searchInputConfigured cfg value = snd <$> searchInputConfigured' cfg value

-- | 'searchInputConfigured' returning @(response, updatedText)@. Store the
-- returned text every frame, including before the debounced change flag fires.
searchInputConfigured' ::
  Ui :> es => SearchInputConfig -> Text -> Eff es (Response, Text)
searchInputConfigured' cfg value =
  buildTextInput
    textInputFlagSearch
    (sicLayout cfg)
    (sicPlaceholder cfg)
    value
    (Just (sicDebounceMs cfg))

-- -----------------------------------------------------------------------------
-- Selectable text
-- -----------------------------------------------------------------------------

-- | Read-only text that can be selected with the mouse and copied with Ctrl+C.
{-# INLINE selectableText #-}
selectableText :: Ui :> es => Text -> Eff es ()
selectableText = selectableTextWith id

-- | 'selectableText' with its response, for hover or anchored UI.
{-# INLINE selectableText' #-}
selectableText' :: Ui :> es => Text -> Eff es Response
selectableText' = selectableTextWith' id

-- | Read-only selectable text with a layout/font modifier.
{-# INLINE selectableTextWith #-}
selectableTextWith :: Ui :> es => (Layout -> Layout) -> Text -> Eff es ()
selectableTextWith f txt = void (selectableTextWith' f txt)

-- | 'selectableTextWith' returning its response; text remains caller-owned.
selectableTextWith' :: Ui :> es => (Layout -> Layout) -> Text -> Eff es Response
selectableTextWith' f txt = do
  layout <- f <$> askDefaultLayout
  wid <- nextId
  ctx <- askContext
  -- The caller owns the text; the editor only moves the selection.
  _ <- uiIO $ adoptStoreText ctx wid (intKey wid) txt
  _ <- editTextField wid singleLineMode {modeEditable = False} txt Nothing
  let styleIdx =
        textInputFlagSelectable
          .|. packTextNodeStyleFull
                (layoutFontVariant layout)
                (layoutFontWeight layout)
                (layoutFontStyle layout)
                (layoutTextDecoration layout)
                0
  addWidgetStyled wid NodeTextInput txt 0 layout styleIdx
