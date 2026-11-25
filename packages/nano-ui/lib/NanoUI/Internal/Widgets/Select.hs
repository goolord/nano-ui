-- | Dropdown select.
module NanoUI.Internal.Widgets.Select
  ( select
  , select'
  , selectWith
  , selectWith'
  , enumSelectBy
  , enumSelectBy'
  , enumSelect
  , enumSelect'
  , dropdownPopup
  , paintDropdownRows
  )
where

import Control.Monad (forM_, join, mfilter, unless, void, when)
import Data.Foldable (toList)
import Data.Hashable (hash)
import Data.IORef (writeIORef)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import NanoUI.Internal.Canvas (CanvasM, drawRect, drawRoundedRect, drawText, runCanvasFor)
import NanoUI.Internal.Context
import NanoUI.Internal.Equality (sameEnum)
import NanoUI.Internal.Font (FontMetrics (..), centeredTextY, menuItemRowH, menuOuterPad)
import NanoUI.Internal.Frame.Select (DropdownRow (..), dropdownPopupRows, dropdownVisibleRows, selectRowAt)
import NanoUI.Internal.Frame.Hit (findNodeByWidgetId)
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Input (Input, MouseButton (..), Pressable (..), UiCursorKind (..), inputMousePos, inputWindowSize)
import NanoUI.Internal.Layout.Arena (NodeType (..), addNodeFromLayout)
import NanoUI.Internal.Monad (NanoUI, (<&&>), askContext, askDefaultLayout, askInput, freshWidget, liftIO, nextId)
import NanoUI.Internal.Store (fieldInt, insertSlot)
import NanoUI.Internal.Style (AlignX (..), AlignY (..), Layout (..), Theme (..), defaultLayout, fillW, fixedH, inheritFont, tight)
import NanoUI.Internal.Types (Rect (..), Size (..), V2 (..), clamp, rectIntersect)
import NanoUI.Internal.Var (Binding (..), bindVar, bindVarBy)
import NanoUI.Internal.Var.Types (Var)
import NanoUI.Internal.Widgets.Combinators (withBoundedIndex)
import NanoUI.Internal.Widgets.Node (Response, addWidget, addWidgetWithOptions, dropdownBodyInput, inDropdownScope, pointerOnWidget, respRect, setChanged)
import NanoUI.Internal.Widgets.Popup (floatingOverlay)

-- | Dropdown over @options@ in fold order, bound to mutable state ('Var')
-- holding the zero-based selected index.
{-# INLINE select #-}
select :: Foldable f => f Text -> Var Int -> NanoUI ()
select options v = void (selectWith' id options v)

-- | 'select' returning its 'Response'.
{-# INLINE select' #-}
select' :: Foldable f => f Text -> Var Int -> NanoUI Response
select' = selectWith' id

-- | 'select' with a layout modifier.
{-# INLINE selectWith #-}
selectWith :: Foldable f => (Layout -> Layout) -> f Text -> Var Int -> NanoUI ()
selectWith f options v = void (selectWith' f options v)

-- | 'selectWith' returning its 'Response'.
{-# INLINE selectWith' #-}
selectWith' :: Foldable f => (Layout -> Layout) -> f Text -> Var Int -> NanoUI Response
selectWith' f options v = bindVar RenderFinal v (selectCore f options)

-- | 'selectWith' on a plain index: the response and the index after this
-- frame's pick.
selectCore ::
  Foldable f =>
  (Layout -> Layout) ->
  f Text ->
  Int ->
  NanoUI (Response, Int)
selectCore f options index = do
  (wid, ctx) <- freshWidget
  liftIO $ registerFocusable ctx wid
  let
    opts = case toList options of
      [] -> [""]
      xs -> xs
    n = length opts
    key = intKey wid
  -- The store holds the caller's index until a pick replaces it, even one
  -- the options do not reach: a pick of the option it shows is a change.
  stored <- liftIO $ adoptSlot fieldInt ctx wid index
  store0 <- liftIO (getStore ctx)
  let
    current = clamp 0 (n - 1) stored
    open = isSelectOpen store0 key
  amb <- askDefaultLayout
  resp <- addWidgetWithOptions wid NodeSelect "" opts 0 (f (inheritFont amb defaultLayout))
  inp <- askInput
  let rect = respRect resp
  -- Ignore presses where layers or a pinned node cover the widget, or where
  -- it is scrolled out of its viewport. The press that opens the dropdown
  -- shows it in the same pass.
  pressed <-
    liftIO $
      pure (pressedIn MouseLeft inp)
        <&&> (findNodeByWidgetId ctx wid >>= \mIdx -> pointerOnWidget ctx mIdx wid rect (inputMousePos inp))
  -- A field scrolled out of its viewport, under a modal it is not in, or
  -- disabled hangs no dropdown.
  shown <-
    if open /= pressed
      then liftIO $
        (maybe True (isJust . rectIntersect rect) <$> getPrevClipRect ctx wid)
          <&&> (not <$> pointerBlockedByModal ctx)
          <&&> (not <$> isDisabled ctx wid)
      else pure False
  pick <- if shown then selectDropdown wid opts current else pure Nothing
  let
    -- A release on a row picks in this pass. A press picks in the store
    -- only, which the next frame reads, as a keyboard pick does.
    picked = fst <$> mfilter snd pick
    finalIdx = maybe current (clamp 0 (n - 1)) picked
    -- An index the options do not reach yet stays the caller's until a pick.
    result = if isNothing picked && stored == index then index else finalIdx
  -- Opening, closing or picking changes the store, which wakes the loop.
  liftIO $ do
    when pressed $ do
      modifyStore ctx (\st -> setSelectOpen st key (not open))
      writeIORef (ctxFocusId ctx) wid
    forM_ pick $ \(i, _) -> do
      modifyStore ctx (\st -> setSelectOpen (insertSlot fieldInt key i st) key False)
      writeIORef (ctxFocusId ctx) wid
    recordSlot fieldInt ctx key result
  -- Compare with the caller's index, not 'current': a dropdown or keyboard
  -- pick lands in the store between frames and must still report a change.
  pure (setChanged (result /= index) resp, result)

-- | The open dropdown of select @wid@ over @opts@, with row @current@
-- picked ('dropdownPopup'). Returns the row a left press or release on the
-- dropdown picks, and whether it was a release. A press on the field that is
-- released on a row picks it ('dropdownBodyInput').
selectDropdown :: WidgetId -> [Text] -> Int -> NanoUI (Maybe (Int, Bool))
selectDropdown wid opts current =
  fmap join . dropdownPopup wid dp (menuItemRowH * fromIntegral n + 2 * menuOuterPad) $ \inp dropR@(Rect _ dy _ _) ->
    let
      mouse = inputMousePos inp
      Size _ winH = inputWindowSize inp
      hovered = selectRowAt dropR n mouse
      released = releasedIn MouseLeft inp
      picked
        | hovered >= 0 && (pressedIn MouseLeft inp || released) = Just (hovered, released)
        | otherwise = Nothing
      -- The rows in the window where the dropdown was last laid out; the
      -- drawing is rebuilt when they, the hover or the pick change.
      (first, lastRow) = dropdownVisibleRows n winH (dy + menuOuterPad)
      content = hash (hovered, current, first, hash (take (lastRow - first + 1) (drop first opts)))
      build cdc rect = runCanvasFor cdc (paintDropdownRows cdc (dropdownPopupRows (cdcFont cdc) (cdcTheme cdc) winH mouse dp opts rect))
     in
      (picked, content, build)
  where
    n = length opts
    dp = DropdownPopup wid current False 0

-- | Field @owner@'s open dropdown: a popup the frame places below or above
-- the field ('NanoUI.Internal.Frame.Select.dropdownPlacement'), as wide as
-- it, holding one drawing @dropH@ tall. @body@, given the popup's input
-- ('dropdownBodyInput') and the drawing's rect last frame, returns a result,
-- the drawing's content key and its build. The popup is declared in
-- 'inDropdownScope'.
dropdownPopup :: WidgetId -> DropdownPopup -> Float -> (Input -> Rect -> (a, Int, CustomDrawBuild)) -> NanoUI (Maybe a)
dropdownPopup owner dp dropH body = inDropdownScope owner $ do
  ctx <- askContext
  let
    layout = fillW ((tight defaultLayout) {layoutGap = 0})
    addPanel pwid parent = do
      registerDropdownPopup ctx pwid dp
      addNodeFromLayout (ctxNodeArena ctx) NodePopup parent layout
  (_, r) <- floatingOverlay True Nothing addPanel (pure ()) $ do
    inp <- dropdownBodyInput owner
    dwid <- nextId
    resp <- addWidget dwid NodeDrawing T.empty 0 (fillW (fixedH dropH defaultLayout))
    let (a, content, build) = body inp (respRect resp)
    liftIO (registerCustomEntry ctx dwid (CustomDrawingEntry content build (Just (\_ _ _ -> UiCursorPointer)) 0 True KeysNone))
    pure (False, a)
  pure r

-- | Paint dropdown rows: each one's fill, the accent marker at the hovered
-- row's left edge, and its text.
paintDropdownRows :: CustomDrawContext -> [DropdownRow] -> CanvasM ()
paintDropdownRows cdc rows =
  forM_ rows $ \row -> do
    let r@(Rect rx ry _ rh) = drRect row
    mapM_ (drawRect r) (drFill row)
    when (drHovered row) $ drawRoundedRect (Rect rx (ry + 3) 2 (max 0 (rh - 6))) 1 (themeAccent (cdcTheme cdc))
    unless (T.null (drOption row)) $
      drawText (V2 (drTextX row) (centeredTextY fm ry rh (fmLineHeight fm))) AlignStart AlignTop (drOption row) (drFg row)
  where
    fm = cdcFont cdc

-- | Select over every value of a bounded enum, labeled by @encode@.
{-# INLINE enumSelectBy #-}
enumSelectBy :: (Bounded a, Enum a) => (a -> Text) -> Var a -> NanoUI ()
enumSelectBy encode v = void (enumSelectBy' encode v)

-- | 'enumSelectBy' returning its 'Response'.
{-# INLINE enumSelectBy' #-}
enumSelectBy' :: (Bounded a, Enum a) => (a -> Text) -> Var a -> NanoUI Response
enumSelectBy' encode v = bindVarBy sameEnum RenderFinal v (\value -> withBoundedIndex encode value (selectCore id))

-- | 'enumSelectBy' labeled with 'show'.
{-# INLINE enumSelect #-}
enumSelect :: (Bounded a, Enum a, Show a) => Var a -> NanoUI ()
enumSelect = enumSelectBy (T.pack . show)

-- | 'enumSelect' returning its 'Response'.
{-# INLINE enumSelect' #-}
enumSelect' :: (Bounded a, Enum a, Show a) => Var a -> NanoUI Response
enumSelect' = enumSelectBy' (T.pack . show)
