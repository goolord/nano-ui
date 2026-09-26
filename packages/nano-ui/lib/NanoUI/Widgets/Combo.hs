-- | Combo box: a search field with a filtered, scrollable suggestion dropdown.
-- The per-frame logic is the pure 'comboStep' over a persisted 'ComboState'.
module NanoUI.Widgets.Combo
  ( comboBox
  , comboBox'
  , ComboState (..)
  , ComboInput (..)
  , ComboStep (..)
  , comboStep
  )
where

import Control.Monad (foldM, when, (<$!>))
import Data.Foldable (toList)
import Data.IORef (writeIORef)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful (Eff, type (:>))
import NanoUI.Internal.Context
import NanoUI.Internal.Font (menuItemRowH)
import NanoUI.Internal.Frame.Hit (findNodeByWidgetId)
import NanoUI.Internal.Frame.Select (comboDropPickIndex, comboDropRect, comboScrollGeom)
import NanoUI.Internal.Id (WidgetId (..))
import NanoUI.Internal.Input (Input, Key (..), MouseButton (..), Pressable (..), buttonHeld, buttonPressed, inputKeys, inputMousePos, inputScroll)
import NanoUI.Internal.Layout.Arena (setOptions)
import NanoUI.Internal.Monad (Ui, askContext, uiIO)
import NanoUI.Internal.Store (boolInt, ptrEq, fieldFloat, fieldInt, fieldText, findSlot, flagSlot, insertSlot, setFieldSelection)
import NanoUI.Internal.Types (Rect (..), V2 (..), clamp, rectContains, rectNonEmpty, v2X, v2Y)
import NanoUI.Internal.WidgetText (textInputFlagSearch)
import NanoUI.Internal.Widgets.Behavior (keyboardFocused)
import NanoUI.Internal.Widgets.Combinators (readDerived, writeDerived)
import NanoUI.Internal.Widgets.Node (Response (..), dropdownInput, setChanged)
import NanoUI.Internal.Widgets.TextInput (buildTextInput, searchInputLayout)

-- | Maximum suggestion rows the combo dropdown shows at once; Up/Down walk
-- the highlight and the wheel scrolls the list through a sliding window.
comboBoxMaxVisible :: Int
comboBoxMaxVisible = 8

-- | Rows scrolled per wheel notch.
comboBoxRowsPerNotch :: Float
comboBoxRowsPerNotch = 3

-- | Case-insensitive substring filter behind the combo's suggestion list.
comboFiltered :: Foldable f => f Text -> Text -> [Text]
comboFiltered options q
  | T.null q = opts
  | otherwise =
      let needle = T.toLower q
        in filter (T.isInfixOf needle . T.toLower) opts
  where
    opts = toList options

-- | A combo's matches for one query over one options list, with the widest
-- match's width once a focused frame has measured it.
data ComboMatches = ComboMatches ![Text] !Text [Text] !(Maybe Float)

-- | 'comboFiltered', kept in 'ctxDerivedCache' under the combo's key while the
-- options are the same list and the query the same text. A combo filters its
-- options on every frame, focused or not, and a long list (a font picker's
-- families) would otherwise lowercase every option each time.
comboMatches :: Context -> Int -> [Text] -> Text -> IO ComboMatches
comboMatches ctx key !opts q =
  readDerived ctx key >>= \case
    Just m@(ComboMatches o q' _ _) | ptrEq o opts && q' == q -> pure m
    _ -> do
      let m = ComboMatches opts q (comboFiltered opts q) Nothing
      m <$ writeDerived ctx key m

-- | A combo's state between frames.
data ComboState = ComboState
  { csHighlight :: !Int
    -- ^ Highlighted row of the filtered list; -1 for none.
  , csWindow :: !Int
    -- ^ First visible row.
  , csScrollX :: !Float
  , csContentW :: !Float
    -- ^ Widest matching row, measured while focused.
  , csDrag :: !Int
    -- ^ Scrollbar thumb drag: 0 none, 1 vertical, 2 horizontal.
  , csDragOff :: !Float
    -- ^ Pointer offset into the dragged thumb.
  , csCommitted :: !Text
    -- ^ Last committed value.
  , csLive :: !Text
    -- ^ Field text the combo last produced.
  , csFocused :: !Bool
  }
  deriving (Eq, Show)

-- | One frame's inputs to 'comboStep'.
data ComboInput = ComboInput
  { ciFocused :: !Bool
  , ciEdited :: !Bool
    -- ^ Typing changed the field text this frame.
  , ciText :: !Text
    -- ^ Field text after this frame's editing.
  , ciRows :: ![Text]
    -- ^ Options matching the field text.
  , ciContentW :: !Float
    -- ^ Width of the widest matching row.
  , ciField :: !Rect
    -- ^ The field's rect; empty before its first layout.
  , ciInput :: !Input
    -- ^ The pointer, wheel and keys as the dropdown sees them.
  }

-- | What one frame of the combo decided.
data ComboStep = ComboStep
  { stepState :: !ComboState
  , stepCommit :: !(Maybe Text)
    -- ^ The newly committed value, on the frame the committed value changes.
  , stepPicked :: !Bool
    -- ^ Enter picked the highlighted row; the caret moves to the text's end.
  , stepDismissed :: !Bool
    -- ^ Escape reverted the field to the committed value and releases focus.
  , stepRedraw :: !Bool
    -- ^ Something visible moved.
  }

-- | One frame of the combo: highlight, scrolling, thumb drags, and commits.
--
-- Typing edits the live text but never commits it: the committed value only
-- changes on Enter (which commits the highlighted row only), on a row click
-- (a field text the combo did not produce), or when the field loses focus.
-- Escape reverts the live text to the last committed value. Hover
-- highlights a row and makes it the Enter target; Up/Down move the highlight.
comboStep :: ComboInput -> ComboState -> ComboStep
comboStep ci cs0 =
  ComboStep
    { stepState =
        ComboState
          { csHighlight = hi'
          , csWindow = win
          , csScrollX = xOff
          , csContentW = contentW
          , csDrag = dragKind'
          , csDragOff = dragOff'
          , csCommitted = fromMaybe committed0 commitText
          , csLive = finalText
          , csFocused = isFocus
          }
    , stepCommit = if commitPulse then commitText else Nothing
    , stepPicked = picked
    , stepDismissed = escDismiss
    , stepRedraw =
        picked || nav /= 0 || escDismiss || wheelDelta /= 0 || xWheel /= 0
          || win /= storedWin || xOff /= storedX || hi' /= storedHi
          || dragKind' /= drag0 || commitPulse || csFocused cs0 /= isFocus
    }
  where
    isFocus = ciFocused ci
    inp = ciInput ci
    hasKey k = k `elem` inputKeys inp
    text = ciText ci
    displayed = ciRows ci
    contentW = ciContentW ci
    n = length displayed
    vis = comboBoxMaxVisible
    storedHi = csHighlight cs0
    storedWin = csWindow cs0
    storedX = csScrollX cs0
    drag0 = csDrag cs0
    dragOff0 = csDragOff cs0
    committed0 = csCommitted cs0
    -- Typing clears the highlight (-1): it never pre-selects a row.
    hi0 = if ciEdited ci then -1 else storedHi
    win0 = if ciEdited ci then 0 else storedWin
    nav
      | not isFocus || n <= 0 = 0 :: Int
      | hasKey KeyDown = 1
      | hasKey KeyUp = -1
      | otherwise = 0
    hi
      | nav == 0 = hi0
      | hi0 < 0 = if nav > 0 then 0 else n - 1
      | otherwise = clamp 0 (n - 1) (hi0 + nav)
    clampWin = clamp 0 (max 0 (n - vis))
    -- Keep the highlighted row inside the window after keyboard navigation.
    alignWin v
      | n <= vis = 0
      | hi < v = hi
      | hi >= v + vis = hi - vis + 1
      | otherwise = clampWin v
    Rect rx ry rw rh = ciField ci
    mouse = inputMousePos inp
    dropRect = comboDropRect rx ry rw rh (min vis n) n contentW
    overDrop = isFocus && rectNonEmpty (ciField ci) && rectContains dropRect mouse
    itemH = menuItemRowH
    -- Hover highlights the row under the pointer (and makes it the Enter
    -- target); it never commits by itself. Rows on screen belong to the
    -- previous frame's window, so the hit test maps through storedWin.
    hoverIdx
      | overDrop = (storedWin +) <$> comboDropPickIndex dropRect itemH (min vis n) (v2Y mouse)
      | otherwise = Nothing
    hiRaw = fromMaybe hi hoverIdx
    -- A hover mapped through a stale window can point past a shrunken list:
    -- highlight nothing then.
    hi' = if hiRaw < n then hiRaw else -1
    -- Scrollbar geometry from the pre-frame scroll state (the thumb the user
    -- is looking at when a drag starts).
    (_, vSb, hSb, usableW) = comboScrollGeom dropRect n vis storedWin storedX contentW
    maxOffX = max 0 (contentW - usableW)
    -- A missing scrollbar is an empty rect, which contains no point.
    (vTrackR, vThumbR) = fromMaybe (Rect 0 0 0 0, Rect 0 0 0 0) vSb
    (hTrackR, hThumbR) = fromMaybe (Rect 0 0 0 0, Rect 0 0 0 0) hSb
    onVThumb = rectContains vThumbR mouse
    onVTrack = rectContains vTrackR mouse
    onHThumb = rectContains hThumbR mouse
    onHTrack = rectContains hTrackR mouse
    pressed = isFocus && buttonPressed MouseLeft inp
    down = isFocus && buttonHeld MouseLeft inp
    startV = pressed && overDrop && onVTrack
    startH = pressed && overDrop && not startV && onHTrack
    vGrab = if onVThumb then v2Y mouse - rectY vThumbR else rectH vThumbR / 2
    hGrab = if onHThumb then v2X mouse - rectX hThumbR else rectW hThumbR / 2
    drag1
      | startV = 1
      | startH = 2
      | down && drag0 /= 0 = drag0
      | otherwise = 0
    -- Thumb-anchored drags move from the next frame on; track presses jump
    -- the window to the click immediately.
    draggingV = down && drag1 == 1 && ((drag0 == 1 && not startV) || (startV && not onVThumb))
    draggingH = down && drag1 == 2 && ((drag0 == 2 && not startH) || (startH && not onHThumb))
    dragWin = clampWin (round ((v2Y mouse - rectY vTrackR - dragOff0) / max 1 (rectH vTrackR - rectH vThumbR) * fromIntegral (n - vis)))
    dragX = clamp 0 maxOffX ((v2X mouse - rectX hTrackR - dragOff0) / max 1 (rectW hTrackR - rectW hThumbR) * maxOffX)
    wheelRows = round (v2Y (inputScroll inp) * comboBoxRowsPerNotch) :: Int
    wheelDelta = if overDrop then wheelRows else 0
    xWheel = if overDrop then v2X (inputScroll inp) * 20 else 0
    win
      | draggingV = dragWin
      | nav /= 0 = alignWin (win0 + wheelDelta)
      | otherwise = clampWin (win0 + wheelDelta)
    xOff
      | draggingH = dragX
      | otherwise = clamp 0 maxOffX (storedX + xWheel)
    dragKind' = if down then drag1 else 0
    dragOff' | startV = vGrab | startH = hGrab | otherwise = dragOff0
    -- Enter commits only an explicitly highlighted row (hover or Up/Down).
    picked = isFocus && n > 0 && hi' >= 0 && pressedOnceIn KeyEnter inp
    -- Only read when 'picked', so @hi'@ is a row.
    pickedText = fromMaybe text (listToMaybe (drop hi' displayed))
    escDismiss = isFocus && pressedOnceIn KeyEscape inp
    -- Commit points: Enter, a row click (the frame-side pick lands as a
    -- frame-start text the widget did not produce), and losing focus (which
    -- the blur frame after the focus clear detects). Escape is a cancel: it
    -- reverts the live text to the last committed value without committing.
    externalText = not (ciEdited ci) && text /= csLive cs0
    commitText
      | picked = Just pickedText
      | externalText = Just text
      | csFocused cs0 && not isFocus = Just text
      | otherwise = Nothing
    commitPulse = maybe False (/= committed0) commitText
    finalText
      | picked = pickedText
      | escDismiss = committed0
      | otherwise = text

-- | Combo box: the 'searchInput' with a select-style dropdown of options.
-- While the field holds focus, the shared select dropdown overlay lists the
-- options filtered by the field text (all of them while it is empty). The
-- value is free text: options are suggestions, not a closed set. See
-- 'comboStep' for when the value commits. Pass the current text; the result is
-- the text after this frame, and @respChanged@ on 'comboBox'' marks a commit.
{-# INLINE comboBox #-}
comboBox :: (Foldable f, Ui :> es) => Text -> f Text -> Text -> Eff es Text
comboBox placeholder options value = snd <$> comboBox' placeholder options value

-- | 'comboBox' returning @(response, updatedText)@. The first text argument
-- is the placeholder; the last is the controlled value.
comboBox' :: (Foldable f, Ui :> es) => Text -> f Text -> Text -> Eff es (Response, Text)
comboBox' placeholder options value = do
  (resp, text) <-
    buildTextInput textInputFlagSearch searchInputLayout placeholder value Nothing
  ctx <- askContext
  inp <- dropdownInput (rawRespId resp)
  let wid = rawRespId resp
      key = intKey wid
  isFocus <- keyboardFocused wid
  -- The dropdown only shows while the field is focused, so an unfocused
  -- combo steps with no rows. The matches stay lazy: the option window below
  -- forces only its rows, and the count is forced only on frames that store it.
  ComboMatches opts query matches cachedW <- uiIO (comboMatches ctx key (toList options) text)
  let displayed = if isFocus then matches else []
  store <- uiIO (getStore ctx)
  let cs0 =
        ComboState
          { csHighlight = findSlot fieldInt (-1) (slotKey SlotComboHighlight key) store
          , csWindow = findSlot fieldInt 0 (slotKey SlotComboScroll key) store
          , csScrollX = findSlot fieldFloat 0 (slotKey SlotComboScrollX key) store
          , csContentW = findSlot fieldFloat 0 (slotKey SlotComboContentW key) store
          , csDrag = findSlot fieldInt 0 (slotKey SlotComboDrag key) store
          , csDragOff = findSlot fieldFloat 0 (slotKey SlotComboDragOff key) store
          , csCommitted = findSlot fieldText value (slotKey SlotComboCommitted key) store
          , csLive = findSlot fieldText text (slotKey SlotComboLive key) store
          , csFocused = flagSlot (slotKey SlotComboFocus key) store
          }
  contentW <- uiIO $
    case cachedW of
      Just w | isFocus -> pure w
      _
        | isFocus && not (null displayed) -> do
            w <- foldM (\widest t -> max widest . fst <$!> ctxMeasureText ctx t) 0 displayed
            w <$ writeDerived ctx key (ComboMatches opts query matches (Just w))
        | otherwise -> pure (csContentW cs0)
  let step =
        comboStep
          ComboInput
            { ciFocused = isFocus
            , ciEdited = rawRespChanged resp
            , ciText = text
            , ciRows = displayed
            , ciContentW = contentW
            , ciField = rawRespRect resp
            , ciInput = inp
            }
          cs0
      cs1 = stepState step
      finalText = csLive cs1
  when (isFocus || stepRedraw step) $
    uiIO $ do
      let len = T.length finalText
          caretToEnd
            | stepPicked step = setFieldSelection key len len
            | otherwise = id
      modifyStore ctx $
        caretToEnd
          . insertSlot fieldInt (slotKey SlotComboHighlight key) (csHighlight cs1)
          . insertSlot fieldInt (slotKey SlotComboScroll key) (csWindow cs1)
          . insertSlot fieldInt (slotKey SlotComboCount key) (length matches)
          . insertSlot fieldInt (slotKey SlotComboFocus key) (boolInt (csFocused cs1))
          . insertSlot fieldInt (slotKey SlotComboDrag key) (csDrag cs1)
          . insertSlot fieldFloat (slotKey SlotComboScrollX key) (csScrollX cs1)
          . insertSlot fieldFloat (slotKey SlotComboContentW key) (csContentW cs1)
          . insertSlot fieldFloat (slotKey SlotComboDragOff key) (csDragOff cs1)
          . insertSlot fieldText (slotKey SlotComboLive key) finalText
          . insertSlot fieldText (slotKey SlotComboCommitted key) (csCommitted cs1)
          . insertSlot fieldText key finalText
      when (stepDismissed step) $ do
        writeIORef (ctxFocusId ctx) (WidgetId 0)
        markEscapeConsumed ctx
      when (stepRedraw step) $ markDirty ctx
  -- The dropdown overlay reads its rows from the node's option list: the
  -- visible window of the filtered list. Unfocused combos set it too, since a
  -- click that focuses the field this frame shows the dropdown this frame.
  uiIO $ do
    findNodeByWidgetId ctx wid
      >>= mapM_ (\idx -> setOptions (ctxNodeArena ctx) idx (take comboBoxMaxVisible (drop (csWindow cs1) matches)))
    recordSlot fieldText ctx key finalText
  pure (setChanged (isJust (stepCommit step)) resp, finalText)
