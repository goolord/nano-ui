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

import Control.Monad (foldM, forM_, mfilter, void, when, (<$!>))
import Data.Foldable (toList)
import Data.IORef (writeIORef)
import Data.Hashable (hash)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import NanoUI.Internal.Context
import NanoUI.Internal.Canvas (drawRect, drawRoundedRect, runCanvasFor, withClip)
import NanoUI.Internal.Derived (ComboMatches (..), comboCache)
import NanoUI.Internal.Font (menuItemRowH)
import NanoUI.Internal.Frame.Hit (findNodeByWidgetId)
import NanoUI.Internal.Frame.Select (comboDropPickIndex, comboDropRect, comboScrollGeom, dropdownPopupRows)
import NanoUI.Internal.Id (WidgetId (..))
import NanoUI.Internal.Input (Input, Key (..), MouseButton (..), Pressable (..), inputMousePos, inputScroll, inputWindowSize)
import NanoUI.Internal.Style (scrollBarThumbColor, scrollBarThumbHoverColor, scrollBarTrackColor, themeInput)
import NanoUI.Internal.Layout.Arena (DirTag (..), setOptions)
import NanoUI.Internal.Layout.ScrollGeometry (ScrollBarLayout (..), onScrollBar, scrollOffsetFromThumb)
import NanoUI.Internal.Monad (NanoUI, askContext, liftIO)
import NanoUI.Internal.Store (fieldCombo, fieldText, insertSlot, lookupSlot, ptrEq, setFieldSelection)
import NanoUI.Internal.Store.Types (ComboState (..))
import NanoUI.Internal.Var (Binding (..), bindVar)
import NanoUI.Internal.Var.Types (Var)
import NanoUI.Internal.Types (Rect (..), Size (..), V2 (..), clamp, rectContains, rectIntersect, rectNonEmpty, v2X, v2Y)
import NanoUI.Internal.WidgetText (textInputFlagSearch)
import NanoUI.Internal.Widgets.Behavior (keyboardFocused)
import NanoUI.Internal.Widgets.Combinators (readDerived, writeDerived)
import NanoUI.Internal.Widgets.Node (Response (..), dropdownInput, respPressed, setChanged)
import NanoUI.Internal.Widgets.Select (dropdownPopup, paintDropdownRows)
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
      let needle = T.toCaseFold q
        in filter (T.isInfixOf needle . T.toCaseFold) opts
  where
    opts = toList options

-- | 'comboFiltered', kept in 'ctxDerivedCache' under the combo's key while the
-- options are the same container and the query the same text. A combo
-- filters its options on every frame, focused or not, and a long list (a
-- font picker's families) would otherwise fold every option each time. The
-- caller's container is compared, not a list made from it, so a vector or
-- sequence hits too.
comboMatches :: Foldable f => Context -> Int -> f Text -> Text -> IO ComboMatches
comboMatches ctx key !opts q =
  readDerived comboCache ctx key >>= \case
    Just m@(ComboMatches o q' _ _) | ptrEq o opts && q' == q -> pure m
    _ -> do
      let m = ComboMatches opts q (comboFiltered opts q) Nothing
      m <$ writeDerived comboCache ctx key m

-- | One frame's inputs to 'comboStep'.
data ComboInput = ComboInput
  { ciFocused :: !Bool
    -- ^ The field holds keyboard focus, so the dropdown is open.
  , ciEdited :: !Bool
    -- ^ Typing changed the field text this frame.
  , ciText :: !Text
    -- ^ Field text after this frame's editing.
  , ciRows :: ![Text]
    -- ^ Options matching the field text.
  , ciContentW :: !Float
    -- ^ Width of the widest matching row.
  , ciField :: !Rect
    -- ^ The field's rect; empty before its first layout and while the field
    -- is clipped out of view, when no row takes the pointer.
  , ciInput :: !Input
    -- ^ Dropdown-scoped input for the frame.
  }
  deriving (Eq, Show)

-- | Result of stepping the combo state for one frame.
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
  deriving (Eq, Show)

-- | One frame of the combo: highlight, scrolling, thumb drags, and commits.
--
-- Typing edits the live text but never commits it: the committed value only
-- changes on Enter (which commits the highlighted row only), on a row click
-- (a field text the combo did not produce), or when the field loses focus.
-- Escape reverts the live text to the last committed value. Hover
-- highlights a row and makes it the Enter target; Up/Down move the highlight,
-- and a stationary pointer does not override keyboard highlight until it moves.
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
          , csWheelRem = wheelRows - fromIntegral wheelDelta
          , csCommitted = fromMaybe committed0 commitText
          , csLive = finalText
          , csFocused = isFocus
          , csNavPointer = navPointer
          }
    , stepCommit = if commitPulse then commitText else Nothing
    , stepPicked = picked
    , stepDismissed = escDismiss
    , stepRedraw =
        -- What navigation and the wheel show moves the window, the x offset
        -- or the highlight; a press at the end of the list moves nothing.
        picked || escDismiss
          || win /= storedWin || xOff /= storedX || hi' /= storedHi
          || dragKind' /= drag0 || commitPulse || csFocused cs0 /= isFocus
    }
  where
    isFocus = ciFocused ci
    inp = ciInput ci
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
      | pressedIn KeyDown inp = 1
      | pressedIn KeyUp inp = -1
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
    dropRect = comboDropRect (inputWindowSize inp) rx ry rw rh (min vis n) n contentW
    overDrop = isFocus && rectNonEmpty (ciField ci) && rectContains dropRect mouse
    itemH = menuItemRowH
    -- Visible rows come from the previous frame's scroll window, so hit-testing
    -- offsets by storedWin.
    navPointer
      | nav /= 0 = Just mouse
      | wheelDelta /= 0 = Nothing
      | otherwise = mfilter (== mouse) (csNavPointer cs0)
    hoverIdx
      | overDrop && isNothing navPointer = (storedWin +) <$> comboDropPickIndex dropRect itemH (min vis n) (v2Y mouse)
      | otherwise = Nothing
    hiRaw = fromMaybe hi hoverIdx
    -- A hover mapped through a stale window can point past a shrunken list:
    -- highlight nothing then.
    hi' = if hiRaw < n then hiRaw else -1
    -- Scrollbar geometry from the pre-frame scroll state (the thumb the user
    -- is looking at when a drag starts).
    (_, vSb, hSb, usableW) = comboScrollGeom dropRect n vis storedWin storedX contentW
    maxOffX = max 0 (contentW - usableW)
    pressed = isFocus && pressedIn MouseLeft inp
    down = isFocus && heldIn MouseLeft inp
    hitBar = maybe (False, False) (\l -> (rectContains (sbTrack l) mouse, rectContains (sbThumb l) mouse))
    (onVTrack, onVThumb) = hitBar vSb
    (onHTrack, onHThumb) = hitBar hSb
    startV = pressed && overDrop && onVTrack
    startH = pressed && overDrop && not startV && onHTrack
    grabOff dir onThumb = maybe 0 $ \l ->
      let Rect tx ty tw th = sbThumb l
       in case dir of
            DirColumn -> if onThumb then v2Y mouse - ty else th / 2
            DirRow -> if onThumb then v2X mouse - tx else tw / 2
    drag1
      | startV = 1
      | startH = 2
      | down && drag0 /= 0 = drag0
      | otherwise = 0
    dragKind' = if down then drag1 else 0
    -- This frame's grab offset: a track press jumps by the one its drag
    -- goes on with.
    dragOff' | startV = grabOff DirColumn onVThumb vSb | startH = grabOff DirRow onHThumb hSb | otherwise = dragOff0
    -- Thumb-anchored drags move from the next frame on; track presses jump
    -- the window to the click immediately.
    dragging kind started onThumb = down && drag1 == kind && ((drag0 == kind && not started) || (started && not onThumb))
    -- Off the dropdown the wheel scrolls something else, and the carried
    -- fraction is dropped.
    wheelRows = if overDrop then v2Y (inputScroll inp) * comboBoxRowsPerNotch + csWheelRem cs0 else 0
    wheelDelta = truncate wheelRows :: Int
    xWheel = if overDrop then v2X (inputScroll inp) * 20 else 0
    win
      | dragging 1 startV onVThumb = maybe 0 (\l -> clampWin (round (scrollOffsetFromThumb DirColumn l dragOff' mouse))) vSb
      | nav /= 0 = alignWin (win0 + wheelDelta)
      | otherwise = clampWin (win0 + wheelDelta)
    xOff
      | dragging 2 startH onHThumb = maybe 0 (\l -> scrollOffsetFromThumb DirRow l dragOff' mouse) hSb
      | otherwise = clamp 0 maxOffX (storedX + xWheel)
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

-- | Combo box: the 'NanoUI.searchInput' with a select-style dropdown of options.
-- While the field holds focus, a dropdown popup lists the options filtered
-- by the field text (all of them while it is empty). The
-- value is free text: options are suggestions, not a closed set. See
-- 'comboStep' for when the value commits. The 'Var' is mutated on every edit;
-- @respChanged@ on 'comboBox'' fires only when the user commits a value, not
-- when the application writes one.
{-# INLINE comboBox #-}
comboBox :: Foldable f => Text -> f Text -> Var Text -> NanoUI ()
comboBox placeholder options v = void (comboBox' placeholder options v)

-- | 'comboBox' returning its 'Response'. The first argument is the
-- placeholder.
{-# INLINE comboBox' #-}
comboBox' :: Foldable f => Text -> f Text -> Var Text -> NanoUI Response
comboBox' placeholder options v = bindVar RenderFinal v (comboBoxCore placeholder options)

-- | Load the persisted combo state for @key@, defaulting committed and live
-- text to @value@ and @text@.
loadComboState :: WidgetStore -> Int -> Text -> Text -> ComboState
loadComboState store key value text =
  fromMaybe (ComboState (-1) 0 0 0 0 0 0 value text False Nothing) (lookupSlot fieldCombo key store)

-- | Persist @cs@ for @key@, moving the caret to the end when @picked@ is 'True'.
saveComboState :: Int -> Bool -> ComboState -> WidgetStore -> WidgetStore
saveComboState key picked cs =
  (if picked then setFieldEnd key finalText else id)
    . insertSlot fieldCombo key cs
    . insertSlot fieldText key finalText
  where
    finalText = csLive cs

setFieldEnd :: Int -> Text -> WidgetStore -> WidgetStore
setFieldEnd key t = let len = T.length t in setFieldSelection key len len

comboBoxCore :: Foldable f => Text -> f Text -> Text -> NanoUI (Response, Text)
comboBoxCore placeholder options value = do
  (resp, text) <-
    buildTextInput textInputFlagSearch searchInputLayout placeholder value Nothing
  ctx <- askContext
  inp <- dropdownInput (rawRespId resp)
  let wid = rawRespId resp
      key = intKey wid
  isFocus <- keyboardFocused wid
  -- A field scrolled out of its viewport hangs no list ('settleDropdowns'), so
  -- no row of it takes the pointer.
  shown <- if isFocus then liftIO (maybe True (isJust . rectIntersect (rawRespRect resp)) <$> getPrevClipRect ctx wid) else pure True
  -- The dropdown only shows while the field is focused, so an unfocused
  -- combo steps with no rows. The matches stay lazy: the option window below
  -- forces only its rows, and the count is forced only on frames that store it.
  ComboMatches opts query matches cachedW <- liftIO (comboMatches ctx key options text)
  let displayed = if isFocus then matches else []
  store <- liftIO (getStore ctx)
  let cs0 = loadComboState store key value text
  contentW <- liftIO $
    case cachedW of
      Just w | isFocus -> pure w
      _
        | isFocus && not (null displayed) -> do
            w <- foldM (\widest t -> max widest . fst <$!> ctxMeasureText ctx t) 0 displayed
            w <$ writeDerived comboCache ctx key (ComboMatches opts query matches (Just w))
        | otherwise -> pure (csContentW cs0)
  let step =
        comboStep
          ComboInput
            { ciFocused = isFocus
            , ciEdited = rawRespChanged resp
            , ciText = text
            , ciRows = displayed
            , ciContentW = contentW
            , ciField = if shown then rawRespRect resp else Rect 0 0 0 0
            , ciInput = inp
            }
          cs0
      cs1 = stepState step
      finalText = csLive cs1
      win = csWindow cs1
      rows = take comboBoxMaxVisible (drop win matches)
      nRows = length rows
      nMatches = length matches
      mouse = inputMousePos inp
      size@(Size _ winH) = inputWindowSize inp
      Rect rx ry rw rh = rawRespRect resp
      dropRect = comboDropRect size rx ry rw rh nRows nMatches contentW
      geom r = comboScrollGeom r nMatches comboBoxMaxVisible win (csScrollX cs1) contentW
      onLane = let (_, vSb, hSb, _) = geom dropRect in any (\l -> rectContains (sbTrack l) mouse) (catMaybes [vSb, hSb])
      -- A left press on a row, not on a scrollbar lane, picks it, so that
      -- finishing a thumb drag cannot.
      clickRow
        | isFocus && shown && nRows > 0 && pressedIn MouseLeft inp && rectContains dropRect mouse && not onLane =
            comboDropPickIndex dropRect menuItemRowH nRows (v2Y mouse)
        | otherwise = Nothing
  when (isFocus || stepRedraw step) $
    liftIO $ do
      modifyStore ctx (saveComboState key (stepPicked step) cs1)
      when (stepDismissed step) $ do
        writeIORef (ctxFocusId ctx) (WidgetId 0)
        markEscapeConsumed ctx
      when (stepRedraw step) $ markDirty ctx
  -- A pick puts the row's text in the field and takes focus away, which
  -- closes the dropdown; the next frame finds a text the combo did not
  -- produce, and commits it.
  liftIO . forM_ clickRow $ \i -> do
    let picked = fromMaybe "" (listToMaybe (drop i rows))
    modifyStore ctx (insertSlot fieldText key picked . setFieldEnd key picked)
    writeIORef (ctxFocusId ctx) (WidgetId 0)
    markDirty ctx
  -- The dropdown's text spans read its rows from the node's option list: the
  -- visible window of the filtered list.
  liftIO $ do
    findNodeByWidgetId ctx wid >>= mapM_ (\idx -> setOptions (ctxNodeArena ctx) idx rows)
    recordSlot fieldText ctx key finalText
  -- Focus moves after the view, so a press that focuses the field shows the
  -- dropdown in its own frame here.
  let focusing = not isFocus && pressedIn MouseLeft inp && respPressed resp
  when ((isFocus || focusing) && shown && nRows > 0) $ do
    let dp = DropdownPopup wid (csHighlight cs1 - win) True (csScrollX cs1)
        drag = csDrag cs1
        -- As a scroller's bar: brighter under the pointer or while dragged.
        hot kind l = drag == kind || drag == 0 && onScrollBar mouse l
    void . dropdownPopup wid dp (rectH dropRect) $ \_ dropR ->
      let
        (_, vSb, hSb, _) = geom dropR
        hovered = if rectContains dropR mouse then comboDropPickIndex dropR menuItemRowH nRows (v2Y mouse) else Nothing
        content = hash ((win, dpPicked dp, hovered, csScrollX cs1, drag), (hot 1 <$> vSb, hot 2 <$> hSb, contentW, nMatches, hash rows))
        build cdc rect =
          let
            (inner, vBar, hBar, _) = geom rect
            theme = cdcTheme cdc
            base = themeInput theme
            bar kind l = do
              drawRect (sbTrack l) (scrollBarTrackColor base theme)
              drawRoundedRect (sbThumb l) 3 ((if hot kind l then scrollBarThumbHoverColor else scrollBarThumbColor) base theme)
           in
            runCanvasFor cdc $ do
              withClip inner (paintDropdownRows cdc (dropdownPopupRows (cdcFont cdc) theme winH mouse dp rows rect))
              mapM_ (bar 1) vBar
              mapM_ (bar 2) hBar
       in ((), content, build)
  -- A value the application wrote reaches the field as a text the combo did
  -- not produce, as a row click does, and becomes the committed value; but
  -- it is not the user's commit.
  let appWrote = value /= csLive cs0 && stepCommit step == Just value
  pure (setChanged (isJust (stepCommit step) && not appWrote) resp, finalText)
