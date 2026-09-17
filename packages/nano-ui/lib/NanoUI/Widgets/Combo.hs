{-# LANGUAGE OverloadedStrings #-}

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
import Data.IORef (writeIORef)
import Data.IntMap.Strict qualified as IM
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful (Eff, type (:>))
import NanoUI.Context
  ( Context (..)
  , getStore
  , intKey
  , markDirty
  , markEscapeConsumed
  , recordStoreText
  , setStore
  )
import NanoUI.Font (FontMetrics, menuItemRowH)
import NanoUI.Frame.Hit (findNodeByWidgetId)
import NanoUI.Frame.Select (comboDropPickIndex, comboDropRect, comboScrollGeom)
import NanoUI.Id (WidgetId (..))
import NanoUI.Input (Key (..), inputKeys, inputMouseDown, inputMousePos, inputMousePressed, inputScroll)
import NanoUI.Layout.Arena (setOptions)
import NanoUI.Monad (Ui, askContext, askInput, uiIO)
import NanoUI.Store
  ( WidgetStore (..)
  , boolInt
  , Slot (..)
  , slotKey
  )
import NanoUI.Types (Rect (..), V2 (..), clamp, rectContains, rectNonEmpty, v2X, v2Y)
import NanoUI.WidgetText (textInputFlagSearch)
import NanoUI.Widgets.Behavior (keyboardFocused)
import NanoUI.Widgets.Node (Response (..), setChanged)
import NanoUI.Widgets.TextInput (buildTextInput, searchFieldLayout)

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
    opts = foldr (:) [] options

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
  , ciMetrics :: !FontMetrics
  , ciMouse :: !V2
  , ciPressed :: !Bool
  , ciDown :: !Bool
  , ciScroll :: !V2
  , ciKeyUp :: !Bool
  , ciKeyDown :: !Bool
  , ciEnter :: !Bool
  , ciEscape :: !Bool
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
    text = ciText ci
    displayed = ciRows ci
    contentW = ciContentW ci
    n = length displayed
    vis = max 1 comboBoxMaxVisible
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
      | ciKeyDown ci = 1
      | ciKeyUp ci = -1
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
    mouse = ciMouse ci
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
    onVThumb = maybe False (\(_, th) -> rectContains th mouse) vSb
    onVTrack = maybe False (\(t, _) -> rectContains t mouse) vSb
    onHThumb = maybe False (\(_, th) -> rectContains th mouse) hSb
    onHTrack = maybe False (\(t, _) -> rectContains t mouse) hSb
    pressed = isFocus && ciPressed ci
    down = isFocus && ciDown ci
    startV = pressed && overDrop && onVTrack
    startH = pressed && overDrop && not startV && onHTrack
    vThumbR = maybe (Rect 0 0 0 0) snd vSb
    vTrackR = maybe (Rect 0 0 0 0) fst vSb
    hThumbR = maybe (Rect 0 0 0 0) snd hSb
    hTrackR = maybe (Rect 0 0 0 0) fst hSb
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
    wheelRows = round (v2Y (ciScroll ci) * comboBoxRowsPerNotch) :: Int
    wheelDelta = if overDrop then wheelRows else 0
    xWheel = if overDrop then v2X (ciScroll ci) * 20 else 0
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
    picked = isFocus && n > 0 && hi' >= 0 && ciEnter ci
    pickedText = case drop (max 0 hi') displayed of
      chosen : _ | hi' >= 0 -> chosen
      _ -> text
    escDismiss = isFocus && ciEscape ci
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

-- | Combo box: the 'searchField' with a select-style dropdown of options.
-- While the field holds focus, the shared select dropdown overlay lists the
-- options filtered by the field text (all of them while it is empty). The
-- value is free text: options are suggestions, not a closed set. See
-- 'comboStep' for when the value commits. Pass the current text; the result is
-- the text after this frame, and 'respChanged' on 'comboBox'' marks a commit.
{-# INLINE comboBox #-}
comboBox :: (Foldable f, Ui :> es) => Text -> f Text -> Text -> Eff es Text
comboBox placeholder options value = snd <$> comboBox' placeholder options value

comboBox' :: (Foldable f, Ui :> es) => Text -> f Text -> Text -> Eff es (Response, Text)
comboBox' placeholder options value = do
  (resp, text) <-
    buildTextInput textInputFlagSearch searchFieldLayout placeholder value Nothing
  ctx <- askContext
  inp <- askInput
  let wid = rawRespId resp
      key = intKey wid
      keys = inputKeys inp
      displayed = comboFiltered options text
  isFocus <- keyboardFocused wid
  store <- uiIO (getStore ctx)
  let cs0 =
        ComboState
          { csHighlight = IM.findWithDefault (-1) (slotKey SlotComboHighlight key) (storeInt store)
          , csWindow = IM.findWithDefault 0 (slotKey SlotComboScroll key) (storeInt store)
          , csScrollX = IM.findWithDefault 0 (slotKey SlotComboScrollX key) (storeFloat store)
          , csContentW = IM.findWithDefault 0 (slotKey SlotComboContentW key) (storeFloat store)
          , csDrag = IM.findWithDefault 0 (slotKey SlotComboDrag key) (storeInt store)
          , csDragOff = IM.findWithDefault 0 (slotKey SlotComboDragOff key) (storeFloat store)
          , csCommitted = IM.findWithDefault value (slotKey SlotComboCommitted key) (storeText store)
          , csLive = IM.findWithDefault text (slotKey SlotComboLive key) (storeText store)
          , csFocused = IM.findWithDefault 0 (slotKey SlotComboFocus key) (storeInt store) /= 0
          }
  contentW <- uiIO $
    if isFocus && not (null displayed)
      then foldM (\widest t -> max widest . fst <$!> ctxMeasureText ctx t) 0 displayed
      else pure (csContentW cs0)
  let step =
        comboStep
          ComboInput
            { ciFocused = isFocus
            , ciEdited = rawRespChanged resp
            , ciText = text
            , ciRows = displayed
            , ciContentW = contentW
            , ciField = rawRespRect resp
            , ciMetrics = ctxFontMetrics ctx
            , ciMouse = inputMousePos inp
            , ciPressed = inputMousePressed inp
            , ciDown = inputMouseDown inp
            , ciScroll = inputScroll inp
            , ciKeyUp = KeyUp `elem` keys
            , ciKeyDown = KeyDown `elem` keys
            , ciEnter = KeyEnter `elem` keys
            , ciEscape = KeyEscape `elem` keys
            }
          cs0
      cs1 = stepState step
      finalText = csLive cs1
  when (isFocus || stepRedraw step) $
    uiIO $ do
      st <- getStore ctx
      let len = T.length finalText
          ints =
            IM.insert (slotKey SlotComboHighlight key) (csHighlight cs1) $
              IM.insert (slotKey SlotComboScroll key) (csWindow cs1) $
                IM.insert (slotKey SlotComboCount key) (length displayed) $
                  IM.insert (slotKey SlotComboFocus key) (boolInt (csFocused cs1)) $
                    IM.insert (slotKey SlotComboDrag key) (csDrag cs1) (storeInt st)
      setStore
        ctx
        st
          { storeInt =
              if stepPicked step
                then IM.insert (slotKey SlotCursor key) len (IM.insert (slotKey SlotAnchor key) len ints)
                else ints
          , storeFloat =
              IM.insert (slotKey SlotComboScrollX key) (csScrollX cs1) $
                IM.insert (slotKey SlotComboContentW key) (csContentW cs1) $
                  IM.insert (slotKey SlotComboDragOff key) (csDragOff cs1) (storeFloat st)
          , storeText =
              IM.insert (slotKey SlotComboLive key) finalText $
                IM.insert (slotKey SlotComboCommitted key) (csCommitted cs1) $
                  IM.insert key finalText (storeText st)
          }
      when (stepDismissed step) $ do
        writeIORef (ctxFocusId ctx) (WidgetId 0)
        markEscapeConsumed ctx
      when (stepRedraw step) $ markDirty ctx
  -- The dropdown overlay reads its rows from the node's option list: the
  -- visible window of the filtered list.
  uiIO $
    findNodeByWidgetId ctx wid
      >>= mapM_ (\idx -> setOptions (ctxNodeArena ctx) idx (take comboBoxMaxVisible (drop (csWindow cs1) displayed)))
  uiIO $ recordStoreText ctx key finalText
  pure (setChanged (isJust (stepCommit step)) resp, finalText)
