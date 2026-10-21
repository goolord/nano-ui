-- | Single-line text fields: editable inputs, debounced search fields, and
-- selectable read-only labels, plus the key handling they share.
module NanoUI.Internal.Widgets.TextInput
  ( TextInputState (..)
  , loadTextInputState
  , saveTextInputState
  , searchInputLayout
  , editorTextState
  , saveTextEditor
  , editTextInput
  , fieldTextCommands
  , textInputMode
  , textInputFieldEditor
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
import Data.Char (isControl)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Clock (getMonotonicTime)
import NanoUI.Internal.Context
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Input
import NanoUI.Internal.Layout.Arena (NodeType (..))
import NanoUI.Internal.Monad (NanoUI, askContext, askDefaultLayout, askInput, freshWidget, nextId, liftIO, withContext)
import NanoUI.Internal.Store
import NanoUI.Internal.Store.Types (TextHistory (..))
import NanoUI.Internal.Style (Layout (..), Style (..), Theme (..), defaultLayout, fieldIconColor, fillW, minW)
import NanoUI.Internal.Types (Color)
import NanoUI.Internal.Var (Binding (..), bindVar)
import NanoUI.Internal.Var.Types (Var)
import NanoUI.Internal.WidgetText (hasFlag, packTextNodeStyle, textInputFlagPassword, textInputFlagSearch, textInputFlagSelectable)
import NanoUI.Internal.Widgets.Adornment (Adornments, adornWidget)
import NanoUI.Internal.Widgets.Behavior (keyboardFocused)
import NanoUI.Internal.Widgets.Node (Response (..), addWidgetStyled, inertResponse, respId, setChanged, setSubmitted)
import NanoUI.Widgets.TextBuffer qualified as TB
import NanoUI.Widgets.TextEditor

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

-- | A field's cursor and anchor for @text@ ('fieldSelection').
loadTextInputState :: WidgetStore -> Int -> Text -> TextInputState
loadTextInputState store key text =
  let (anchor, cursor) = fieldSelection store key text
   in TextInputState text cursor anchor

saveTextInputState :: Int -> TextInputState -> WidgetStore -> WidgetStore
saveTextInputState key s =
  insertSlot fieldText key (tisText s) . setFieldSelection key (tisAnchor s) (tisCursor s)

-- | The editor for a field's state, with the undo history stored for it. A
-- history recorded against other text (the caller replaced the value) is
-- dropped.
textInputEditor :: WidgetStore -> Int -> TextInputState -> Editor
textInputEditor store key s =
  let buf0 = TB.fromText (tisText s)
      buf = TB.withCursor (fieldPosition buf0 (tisCursor s)) buf0
      history = case lookupSlot fieldHistory (slotKey SlotTextHistory key) store of
        Just (FieldHistory text h) | text == tisText s -> h
        _ -> emptyHistory
   in Editor buf (fieldPosition buf (tisAnchor s)) history

editorTextState :: Editor -> TextInputState
editorTextState ed =
  let buf = editorBuffer ed
   in TextInputState (TB.toText buf) (fieldOffset buf (TB.getCursor buf)) (fieldOffset buf (editorAnchor ed))

-- | Where character @i@ of a field's value is in its editor's buffer, which
-- holds each line of a value with line breaks as a row; a field's caret and
-- anchor count characters of the whole value. Clamped into the value.
fieldPosition :: TB.TextBuffer -> Int -> TB.Cursor
fieldPosition buf = go 0
  where
    go row i
      | row + 1 < TB.getLineCount buf, i > n = go (row + 1) (i - n - 1)
      | otherwise = TB.clampCursor buf (TB.Cursor row i)
      where
        n = T.length (TB.lineAt row buf)

-- | The character a position in a field's buffer is at in its value
-- ('fieldPosition').
fieldOffset :: TB.TextBuffer -> TB.Cursor -> Int
fieldOffset buf (TB.Cursor row col) = col + sum [T.length (TB.lineAt r buf) + 1 | r <- [0 .. row - 1]]

-- | Store an editor's text, selection and history.
saveTextEditor :: Int -> Editor -> WidgetStore -> WidgetStore
saveTextEditor key ed =
  let s = editorTextState ed
   in insertSlot fieldHistory (slotKey SlotTextHistory key) (FieldHistory (tisText s) (editorHistory ed)) . saveTextInputState key s

-- | Run this frame's commands on a field, or 'Nothing' when it had none.
editTextInput :: Context -> EditorMode -> Input -> WidgetStore -> Int -> TextInputState -> IO (Maybe Editor)
editTextInput ctx mode inp store key s0 =
  fieldTextCommands ctx mode inp >>= \case
    [] -> pure Nothing
    cmds -> Just <$> foldM (flip (runCommandIO ctx mode)) (textInputEditor store key s0) cmds

-- | This frame's edit commands for the focused field ('inputTextCommands').
-- Under 'FocusComposing' the typed text is the IME commit and is inserted
-- regardless of modifiers, so a chord that ends a composition keeps it.
fieldTextCommands :: Context -> EditorMode -> Input -> IO [TextCommand]
fieldTextCommands ctx mode inp = do
  kind <- getsInteraction ctx isFocusKind
  pure $
    if kind == FocusComposing
      then
        [InsertText (T.singleton c) | c <- T.unpack (inputChars inp), not (isControl c)]
          ++ inputTextCommands mode inp {inputChars = T.empty}
      else inputTextCommands mode inp

-- | The editor mode of a single-line field with these style flags.
textInputMode :: Int -> EditorMode
textInputMode si =
  singleLineMode
    { modeEditable = not (hasFlag textInputFlagSelectable si)
    , modeCopyable = not (hasFlag textInputFlagPassword si)
    }

-- | A single-line field's stored editor, for a command run outside its frame,
-- and how to store the edited editor.
textInputFieldEditor :: WidgetStore -> Int -> (Editor, Editor -> WidgetStore -> WidgetStore)
textInputFieldEditor store key =
  (textInputEditor store key (loadTextInputState store key (findSlot fieldText "" key store)), saveTextEditor key)

-- -----------------------------------------------------------------------------
-- Text fields
-- -----------------------------------------------------------------------------

-- | Placeholder, password-display masking, layout, and adornments
-- ("NanoUI.Adornment", in a muted text colour) for a single-line field.
-- Masking affects display; the caller and widget store still hold the
-- original text.
data TextInputConfig = TextInputConfig
  { ticPlaceholder :: !Text
  , ticPassword :: !Bool
  , ticLayout :: !Layout
  , ticAdornments :: !Adornments
  }
  deriving (Show)

-- | No placeholder, no password masking, the standard text-input layout, and
-- no adornments.
defaultTextInputConfig :: TextInputConfig
defaultTextInputConfig =
  TextInputConfig
    { ticPlaceholder = ""
    , ticPassword = False
    , ticLayout = textInputLayout
    , ticAdornments = mempty
    }

-- | Single-line text field bound to mutable state ('Var'): typing, pastes and
-- menu edits update it in place.
--
-- > textInput draft
{-# INLINE textInput #-}
textInput :: Var Text -> NanoUI ()
textInput v = void (textInputConfigured' defaultTextInputConfig v)

-- | 'textInput' returning its 'Response', with change and submit flags.
{-# INLINE textInput' #-}
textInput' :: Var Text -> NanoUI Response
textInput' = textInputConfigured' defaultTextInputConfig

-- | 'textInput' with a placeholder, password masking, its own layout, or
-- adornments.
--
-- > textInputConfigured defaultTextInputConfig {ticPassword = True} secret
-- > textInputConfigured defaultTextInputConfig {ticAdornments = A.trailing (A.affix "kg")} weight
{-# INLINE textInputConfigured #-}
textInputConfigured :: TextInputConfig -> Var Text -> NanoUI ()
textInputConfigured cfg v = void (textInputConfigured' cfg v)

-- | 'textInputConfigured' returning its 'Response'.
textInputConfigured' :: TextInputConfig -> Var Text -> NanoUI Response
textInputConfigured' (TextInputConfig placeholder password lay adorns) v = do
  -- Write the field's text before running adornments so an adornment
  -- control that modifies the 'Var' overwrites the field's value.
  resp <-
    bindVar RenderFinal v $ \value ->
      buildTextInput
        (if password then textInputFlagPassword else 0)
        lay
        placeholder
        value
        Nothing
  -- Suppress the field's response when an adornment control captures the pointer.
  taken <- adornWidget (respId resp) lay (fieldAdornmentColor lay) adorns
  pure (if taken then inertResponse resp else resp)

fieldAdornmentColor :: Layout -> Theme -> Color
fieldAdornmentColor lay theme =
  let s = themeInput theme
   in fieldIconColor s {styleFg = fromMaybe (styleFg s) (layoutFontColor lay)}

-- | One frame of a single-line field's text state: load the text (seeding
-- @initial@ on first use) with its cursor and anchor, run the editor while
-- focused, and save any change. While unfocused, @unfocusedText@ (when given)
-- replaces the stored text, so a field that mirrors another value follows it.
-- Returns the text before and after this frame, whether it is focused, and
-- whether a command run from outside the frame changed it.
editTextField :: WidgetId -> EditorMode -> Text -> Maybe Text -> NanoUI (Text, Text, Bool, Bool)
editTextField wid mode initial unfocusedText = do
  ctx <- askContext
  liftIO $ registerFocusable ctx wid
  inp <- askInput
  store <- liftIO (getStore ctx)
  let
    key = intKey wid
    modeKey = slotKey SlotTextMode key
    pulseKey = slotKey SlotTextAreaChanged key
    stored = lookupSlot fieldText key store
    s0 = loadTextInputState store key (fromMaybe initial stored)
    pulse = memberSlot fieldInt pulseKey store
  when (isNothing stored || lookupSlot fieldEditorMode modeKey store /= Just mode || pulse) $
    liftIO . modifyStore ctx $
      (if isNothing stored then insertSlot fieldText key initial else id)
        . deleteSlot fieldInt pulseKey
        . insertSlot fieldEditorMode modeKey mode
  isFocus <- keyboardFocused wid
  -- Password fields request 'InputSecure' so the IME does not display or
  -- record their text.
  when (isFocus && modeEditable mode) $
    liftIO (requestInputMethod ctx wid Nothing (if modeCopyable mode then InputNormal else InputSecure))
  mEdited <- if isFocus then liftIO (editTextInput ctx mode inp store key s0) else pure Nothing
  let s1 = case mEdited of
        Just ed -> editorTextState ed
        Nothing
          -- A focused field keeps what the user typed, or a command run
          -- between frames stored, until it loses focus.
          | isFocus -> s0
          | otherwise -> maybe s0 (\t -> s0 {tisText = t}) unfocusedText
  when (s1 /= s0) $
    liftIO $ modifyStore ctx (maybe (saveTextInputState key s1) (saveTextEditor key) mEdited)
  pure (tisText s0, tisText s1, isFocus, pulse)

-- | Shared single-line field builder. The caller's @value@ is adopted as by
-- 'adoptSlot'. @styleIdx@ may carry the search or password
-- flag on a @NodeTextInput@; when @mDebounceMs@ is present the returned change
-- pulse is delayed until the text has been idle for that long (immediate for
-- clear clicks).
buildTextInput ::
  Int ->
  Layout ->
  Text ->
  Text ->
  Maybe Float ->
  NanoUI (Response, Text)
buildTextInput styleIdx layout placeholder value mDebounceMs = do
  (wid, ctx) <- freshWidget
  let key = intKey wid
  _ <- liftIO $ adoptSlot fieldText ctx wid value
  -- Both modes are constants, so an idle field allocates no mode record.
  let mode = if hasFlag textInputFlagPassword styleIdx then singleLineMode {modeCopyable = False} else singleLineMode
  (oldText, newText, isFocus, pulse) <- editTextField wid mode value Nothing
  liftIO $ recordSlot fieldText ctx key newText
  inp <- askInput
  let submitted = isFocus && pressedOnceIn KeyEnter inp
      edited = pulse || newText /= oldText
  changed <- case mDebounceMs of
    Nothing -> pure edited
    Just ms -> liftIO (debounceSearchChanged ctx key isFocus edited ms)
  -- The layout's font face, weight and slant ride in the style bits, as on
  -- a label; the flags sit above them.
  resp <- addWidgetStyled wid NodeTextInput placeholder 0 layout (styleIdx .|. packTextNodeStyle layout 0)
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
    -- An edit this frame has not been idle for any time yet.
    commit = dirty && (T.null current || not focused || (not rawChanged && idleMs >= deadline))
    -- Text the caller changed, in a field nobody has typed in, has no edit
    -- time to age from. Its pause starts now: left unstamped, every frame
    -- would see a fresh edit, and the commit would never come.
    stamp = rawChanged || commit || (dirty && not (memberSlot fieldDouble ageKey store))
  when (stamp || committedMissing) $
    modifyStore ctx $
      (if commit || committedMissing then insertSlot fieldText committedKey current else id)
        . (if stamp then insertSlot fieldDouble ageKey now else id)
  -- Schedule a wake-up at the debounce deadline since no input event will
  -- arrive when the pause expires.
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

-- | Search box with a magnifier and a clear button, bound to mutable state
-- ('Var'); the first argument is the placeholder. The 'Var' is mutated in place
-- as the user types; @respChanged@ on 'searchInput'' is debounced and fires
-- once typing pauses, or immediately when the field is cleared.
{-# INLINE searchInput #-}
searchInput :: Text -> Var Text -> NanoUI ()
searchInput placeholder v = void (searchInput' placeholder v)

-- | 'searchInput' returning its 'Response'.
{-# INLINE searchInput' #-}
searchInput' :: Text -> Var Text -> NanoUI Response
searchInput' placeholder =
  searchInputConfigured' (defaultSearchInputConfig {sicPlaceholder = placeholder})

-- | Search field with explicit placeholder, debounce in milliseconds, and layout.
{-# INLINE searchInputConfigured #-}
searchInputConfigured :: SearchInputConfig -> Var Text -> NanoUI ()
searchInputConfigured cfg v = void (searchInputConfigured' cfg v)

-- | 'searchInputConfigured' returning its 'Response'.
{-# INLINE searchInputConfigured' #-}
searchInputConfigured' :: SearchInputConfig -> Var Text -> NanoUI Response
searchInputConfigured' cfg v =
  bindVar RenderFinal v $ \value ->
    buildTextInput
      textInputFlagSearch
      (sicLayout cfg)
      (sicPlaceholder cfg)
      value
      (Just (sicDebounceMs cfg))

-- -----------------------------------------------------------------------------
-- Selectable text
-- -----------------------------------------------------------------------------

-- | Read-only text that can be selected with the mouse and copied with the
-- platform's copy shortcut (Ctrl+C, Command+C on macOS) or the context menu.
{-# INLINE selectableText #-}
selectableText :: Text -> NanoUI ()
selectableText = selectableTextWith id

-- | 'selectableText' with its response, for hover or anchored UI.
{-# INLINE selectableText' #-}
selectableText' :: Text -> NanoUI Response
selectableText' = selectableTextWith' id

-- | Read-only selectable text with a layout/font modifier.
{-# INLINE selectableTextWith #-}
selectableTextWith :: (Layout -> Layout) -> Text -> NanoUI ()
selectableTextWith f txt = void (selectableTextWith' f txt)

-- | 'selectableTextWith' returning its response; text remains caller-owned.
selectableTextWith' :: (Layout -> Layout) -> Text -> NanoUI Response
selectableTextWith' f txt = do
  layout <- f <$> askDefaultLayout
  wid <- nextId
  -- The caller owns the text; the editor only moves the selection.
  _ <- withContext (\ctx -> adoptSlot fieldText ctx wid txt)
  _ <- editTextField wid singleLineMode {modeEditable = False} txt Nothing
  let styleIdx = textInputFlagSelectable .|. packTextNodeStyle layout 0
  addWidgetStyled wid NodeTextInput txt 0 layout styleIdx
