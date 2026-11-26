-- | Dropdown popups of selects and combo boxes: their placement, rows and
-- text, pointer routing to them, and the select's keys and dismissal.
module NanoUI.Internal.Frame.Select
  ( closeSelectOnOutsideClick
  , finalizeSelectKeyboard
  , collectDropdownSpans
  , dropdownOwnerAt
  , routePointer
  , comboDropRect
  , comboDropPickIndex
  , comboScrollGeom
  , focusedComboNode
  , openSelectNode
  , dropdownPlacement
  , settleDropdowns
  , DropdownRow (..)
  , dropdownPopupRows
  , selectRowAt
  , dropdownVisibleRows
  ) where

import Control.Monad (filterM, forM, forM_, unless, when)
import Data.IORef (readIORef)
import Data.IntMap.Strict qualified as IM
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import qualified Data.Text as T
import NanoUI.Internal.Context
import NanoUI.Internal.Draw (DrawArena (..))
import NanoUI.Internal.Font (FontMetrics, centeredTextY, menuItemPadX, menuItemRowH, menuOuterPad, widgetContentInset)
import NanoUI.Internal.Frame.Chrome (overlayMenuStyle)
import NanoUI.Internal.Frame.Hit (widgetOverlayAllowed)
import NanoUI.Internal.Layout.ScrollGeometry (ScrollBarLayout (..), scrollThumbSpan)
import NanoUI.Internal.Id (WidgetId (..), hashWidgetId)
import NanoUI.Internal.Input (Input (..), Key (..), MouseButton (..), Pressable (..), anyButtonPressed, anyButtonReleased, inputPointerHeld, shiftAtMost)
import NanoUI.Internal.Layout.Arena (NodeIdx, NodeType (NodeSelect, NodeTextInput), getClipBounds, getFirstChild, getNodeType, lookupNodeByKey, lookupNodeByWidgetId, getOptions, getNodeRect, getWidgetId, setClipRect, setRect, subtreeSpan)
import NanoUI.Internal.Layout.Solve (computePopupPosition)
import NanoUI.Internal.Monad (mfilterM, whenM, (<&&>))
import NanoUI.Internal.Store (fieldInt, findSlot, insertSlot)
import NanoUI.Internal.Style (Style (..), Theme (..), themeAccent)
import NanoUI.Internal.Types (Color (..), PopupAnchor (..), PopupPlacement (..), Rect (..), Size (..), V2 (..), clamp, rectContains, rectIntersect, rectNonEmpty)

-- | One placed row of an open dropdown, with its colours.
data DropdownRow = DropdownRow
  { drOption :: T.Text
  , drRect :: !Rect
  , drTextX :: !Float
  , drHovered :: !Bool
  , drFill :: !(Maybe Color)
  -- ^ The hover highlight, else the picked row's tint.
  , drFg :: !Color
  }

-- | The rows of dropdown @dp@ laid out at the rect given, over options
-- @opts@, that a window @winH@ tall shows ('dropdownVisibleRows'), with the
-- one @mouse@ is on hovered. A select's rows sit inside the dropdown's outer
-- margin. A combo's options are the rows of its window, which sit flush at
-- the top and scroll across by 'dpScrollX'. Shared by the dropdown's drawing
-- and its text spans.
dropdownPopupRows :: FontMetrics -> Theme -> Float -> V2 -> DropdownPopup -> [T.Text] -> Rect -> [DropdownRow]
dropdownPopupRows fm theme winH mouse dp opts (Rect x y w _) =
  [ DropdownRow opt row textX hovered fill (if picked then themeAccent theme else styleFg style)
  | (i, opt) <- zip [lo .. hi] (drop lo opts)
  , let row = Rect x (top + menuItemRowH * fromIntegral i) w menuItemRowH
        hovered = rectContains row mouse
        picked = i == dpPicked dp
        fill
          | hovered = Just (styleHoverBg style)
          | picked = Just (styleActiveBg style)
          | otherwise = Nothing
  ]
  where
    style = overlayMenuStyle theme
    top = if dpCombo dp then y else y + menuOuterPad
    textX = x + menuItemPadX + fst (widgetContentInset fm) - dpScrollX dp
    (lo, hi) = dropdownVisibleRows (length opts) winH top

-- | The first and last of @n@ dropdown rows, starting at @top@, that a window
-- @winH@ tall shows.
dropdownVisibleRows :: Int -> Float -> Float -> (Int, Int)
dropdownVisibleRows n winH top =
  (max 0 (floor ((0 - top) / menuItemRowH)), min (n - 1) (ceiling ((winH - top) / menuItemRowH)))

-- | The row of @n@ of a select dropdown laid out at @dropR@ that @mouse@ is
-- on, or -1.
selectRowAt :: Rect -> Int -> V2 -> Int
selectRowAt dropR@(Rect _ dy _ _) n mouse@(V2 _ my)
  | rectContains dropR mouse, i >= 0, i < n = i
  | otherwise = -1
  where
    i = floor ((my - dy - menuOuterPad) / menuItemRowH)

-- | Text of each dropdown popup ('registerDropdownPopup'), as its drawing
-- paints it, for a host that draws text from glyph quads. Under external
-- text the drawing records its text itself. One laid out empty
-- ('settleDropdowns') shows nothing.
collectDropdownSpans :: Context -> Input -> IO [(Rect, T.Text, Color, Color, Rect)]
collectDropdownSpans ctx@Context {ctxNodeArena = na, ctxFontMetrics = fm} inp = do
  drops <- dropdownPopups ctx
  external <- readIORef (daExternalText (ctxDrawArena ctx))
  if external || IM.null drops
    then pure []
    else fmap concat . forM (IM.toList drops) $ \(key, dp) -> do
      let field = dpField dp
      nodes <- (,) <$> lookupNodeByWidgetId na field <*> lookupNodeByKey na key
      case nodes of
        (Just fidx, Just pidx) -> do
          didx <- getFirstChild na pidx
          dropR <- if didx < 0 then pure (Rect 0 0 0 0) else getNodeRect na didx
          if not (rectNonEmpty dropR)
            then pure []
            else do
              opts <- getOptions na fidx
              theme <- widgetTheme ctx field
              let Size _ winH = inputWindowSize inp
                  rows = dropdownPopupRows fm theme winH (inputMousePos inp) dp opts dropR
              forM (filter (not . T.null . drOption) rows) $ \row -> do
                (tw, th) <- ctxMeasureText ctx (drOption row)
                let Rect _ ry _ rh = drRect row
                    bg = fromMaybe (styleBg (overlayMenuStyle theme)) (drFill row)
                pure (Rect (drTextX row) (centeredTextY fm ry rh th) tw th, drOption row, drFg row, bg, dropR)
        _ -> pure []

-- | The field whose dropdown popup, laid out this frame, @mouse@ is on.
dropdownOwnerAt :: Context -> V2 -> IO (Maybe WidgetId)
dropdownOwnerAt ctx@Context {ctxNodeArena = na} mouse = do
  drops <- dropdownPopups ctx
  if IM.null drops
    then pure Nothing
    else
      fmap (listToMaybe . map snd) . flip filterM (IM.toList (dpField <$> drops)) $ \(key, _) ->
        maybe (pure False) (fmap (`rectContains` mouse) . getNodeRect na) =<< lookupNodeByKey na key

-- | Decide where this frame's pointer goes, before the view runs. A button
-- that is already down keeps the route it went down with, through its
-- release, so a drag that wanders over something else stays where it began.
-- A press always starts over, even with the other button still down: it goes
-- to what is under it now, not through it to where the first one landed.
-- The floating panel on top is a dropdown when the last view registered it
-- as one ('registerDropdownPopup').
routePointer :: Context -> Input -> IO PointerRoute
routePointer ctx inp = do
  (held, old) <- getsInteraction ctx (\s -> (isPointerHeld s, isPointerRoute s))
  let pressed = anyButtonPressed inp
      released = anyButtonReleased inp
      -- A hold that ended without its release being seen is over too.
      holding = held && not pressed && (inputPointerHeld inp || released)
      mouse = inputMousePos inp
  route <-
    if holding
      then pure old
      else do
        layer <- floatingLayerAt ctx mouse
        drops <- dropdownPopups ctx
        pure (if layer /= 0 && IM.member layer drops then RouteDropdown layer else RouteLayer layer)
  -- Most frames change neither, and an idle frame should write nothing.
  when (route /= old || inputPointerHeld inp /= held) $
    modifyInteraction ctx $ \s ->
      s
        { isPointerRoute = route
        , isPointerHeld = inputPointerHeld inp
        , isColumnResize = isColumnResize s && inputPointerHeld inp
        }
  pure route

-- | Close the open select on a press of any button, or a left release, away
-- from its field and every dropdown. A right press on a field focuses it,
-- so a dropdown left open would take its arrows and Enter.
closeSelectOnOutsideClick :: Context -> Input -> IO ()
closeSelectOnOutsideClick ctx@Context {ctxNodeArena = na} inp =
  when (anyButtonPressed inp || releasedIn MouseLeft inp) $ do
    store <- getStore ctx
    when (anySelectOpen store) $ do
      let mouse = inputMousePos inp
      onField <- maybe (pure False) (fmap (`rectContains` mouse) . getNodeRect na) =<< openSelectNode ctx store
      onDropdown <- isJust <$> dropdownOwnerAt ctx mouse
      unless (onField || onDropdown) $
        setStore ctx (closeSelects store)

finalizeSelectKeyboard :: Context -> Input -> IO ()
finalizeSelectKeyboard ctx@Context {ctxNodeArena = na} inp = do
  -- Strict, so that a frame without these keys allocates no thunks for them.
  let has k = pressedIn k inp
      !wantNext = has KeyDown || has KeyRight
      !wantFirst = has KeyHome || has KeyPageUp
      !wantLast = has KeyEnd || has KeyPageDown
      !wantStep = wantNext || wantFirst || wantLast || has KeyUp || has KeyLeft
      !wantEsc = pressedOnceIn KeyEscape inp
      -- Enter or Space opens or closes the dropdown, as it presses a button.
      !wantEnter = pressedOnceIn KeyEnter inp || pressedOnceIn KeySpace inp
  -- With Ctrl, Alt or Super held, the keys are left to shortcuts.
  when (wantEsc || ((wantStep || wantEnter) && shiftAtMost (inputModifiers inp))) $ do
    focus <- readIORef (ctxFocusId ctx)
    store <- getStore ctx
    -- These keys act on the focused enabled select, open or not. Otherwise
    -- Escape, or any of them while nothing has focus, goes to the open
    -- select: a field that took focus without closing it keeps its keys.
    let enabledSelect idx = ((== NodeSelect) <$> getNodeType na idx) <&&> (not <$> isDisabled ctx focus)
    focused <-
      if wantStep || wantEnter then mfilterM enabledSelect =<< lookupNodeByWidgetId na focus else pure Nothing
    target <- case focused of
      Just idx -> pure (Just (idx, isSelectOpen store (intKey focus)))
      Nothing
        | wantEsc || hashWidgetId focus == 0 -> fmap (,True) <$> openSelectNode ctx store
        | otherwise -> pure Nothing
    forM_ target $ \(idx, open) -> do
      wid <- getWidgetId na idx
      whenM (widgetOverlayAllowed ctx wid) $
        if wantEsc || wantEnter
          then when (open || not wantEsc) $ do
            setStore ctx (setSelectOpen store (intKey wid) (not open))
            when wantEsc $ markEscapeConsumed ctx
            markDirty ctx
          else do
            n <- length <$> getOptions na idx
            when (n > 0) $ do
              -- The stored index can be one the options do not reach; they
              -- show it clamped, and a key that lands there still picks.
              let key = intKey wid
                  stored = findSlot fieldInt 0 key store
                  cur = clamp 0 (n - 1) stored
                  next
                    | wantFirst = 0
                    | wantLast = n - 1
                    | otherwise = clamp 0 (n - 1) (cur + if wantNext then 1 else -1)
              when (next /= stored) $ do
                setStore ctx (insertSlot fieldInt key next store)
                markDirty ctx

-- | The node of the select whose dropdown the store holds open.
openSelectNode :: Context -> WidgetStore -> IO (Maybe NodeIdx)
openSelectNode Context {ctxNodeArena = na} store
  | not (anySelectOpen store) = pure Nothing
  | otherwise =
      mfilterM (fmap (== NodeSelect) . getNodeType na) =<< lookupNodeByKey na (storeOpenSelect store)

-- | The focused node when it is a combo (a text input carrying options),
-- which owns an open dropdown for as long as it holds focus.
focusedComboNode :: Context -> IO (Maybe NodeIdx)
focusedComboNode ctx@Context {ctxNodeArena = na} =
  mfilterM (\idx -> ((== NodeTextInput) <$> getNodeType na idx) <&&> (not . null <$> getOptions na idx))
    =<< lookupNodeByWidgetId na =<< readIORef (ctxFocusId ctx)

-- | Vertical gap between the select widget and its dropdown menu.
selectDropGap :: Float
selectDropGap = 4

-- | A dropdown @dropH@ tall and as wide as the field at @x y w h@: below the
-- field, or above it when it does not fit below and there is more room
-- above, and kept inside the window of @size@ ('computePopupPosition'). A
-- list that fits on neither side runs past the window edge on the roomier
-- one rather than over its field, where the press that opens it would pick.
dropRect :: Size -> Float -> Float -> Float -> Float -> Float -> Rect
dropRect (Size winW winH) x y w h dropH =
  let (dx, dy) = computePopupPosition winW winH 0 w dropH (AnchorRect (Rect x y w h)) PlacementAuto selectDropGap
      dy'
        | dy >= y + h || dy + dropH <= y = dy
        | winH - y - h >= y = y + h + selectDropGap
        | otherwise = y - dropH - selectDropGap
   in Rect dx dy' w dropH

-- | The rect dropdown popup @wid@ goes at, @ih@ tall, when it is one
-- ('registerDropdownPopup'): where 'dropRect' puts it for its field's rect
-- in a window of @size@. Placement runs before the scroll offsets, so a
-- field in a scroller is where it would be unscrolled, and
-- 'settleDropdowns' moves the popup after them.
dropdownPlacement :: Context -> Size -> WidgetId -> Float -> Float -> IO (Maybe Rect)
dropdownPlacement ctx@Context {ctxNodeArena = na} size wid _ ih = do
  drops <- dropdownPopups ctx
  case IM.lookup (intKey wid) drops of
    Nothing -> pure Nothing
    Just dp -> lookupNodeByWidgetId na (dpField dp) >>= traverse (\idx -> do
      Rect x y w h <- getNodeRect na idx
      pure (dropRect size x y w h ih))

-- | Move each dropdown popup, with what it holds, to where 'dropRect' puts
-- it for its field's rect once the scroll offsets are applied. A dropdown
-- is laid out empty, so that it shows nothing and takes no pointer, when its
-- field closed it after the view (a key, Tab, a pick on press), a modal
-- declared after it hides its field, or its field is scrolled out of its
-- viewport. A select's dropdown is open while the store says so, a combo's
-- while its field has focus. The frame runs this once the scroll offsets
-- are applied and again once its input has acted.
settleDropdowns :: Context -> Size -> IO ()
settleDropdowns ctx@Context {ctxNodeArena = na} size = do
  drops <- dropdownPopups ctx
  unless (IM.null drops) $ do
    store <- getStore ctx
    focus <- readIORef (ctxFocusId ctx)
    forM_ (IM.toList drops) $ \(key, dp) -> do
      let field = dpField dp
      nodes <- (,) <$> lookupNodeByKey na key <*> lookupNodeByWidgetId na field
      case nodes of
        (Just idx, Just fidx) -> do
          frect@(Rect fx fy fw fh) <- getNodeRect na fidx
          nt <- getNodeType na fidx
          let open = if nt == NodeSelect then isSelectOpen store (intKey field) else focus == field
          shown <-
            pure open
              <&&> (maybe True (isJust . rectIntersect frect) <$> getClipBounds na fidx)
              <&&> widgetOverlayAllowed ctx field
          Rect px py _ ph <- getNodeRect na idx
          let Rect tx ty _ _ = dropRect size fx fy fw fh ph
          if shown
            then unless (tx == px && ty == py) $ eachInSubtree idx $ \(Rect x y w h) -> Rect (x + tx - px) (y + ty - py) w h
            else eachInSubtree idx $ \(Rect x y _ _) -> Rect x y 0 0
        _ -> pure ()
  where
    -- Apply @f@ to the rect and clip of each node of @idx@'s subtree.
    eachInSubtree idx f =
      subtreeSpan na idx >>= mapM_ (\n -> forM_ [idx .. idx + n - 1] $ \i -> do
        Rect x y w h <- f <$> getNodeRect na i
        setRect na i x y w h
        getClipBounds na i >>= mapM_ (setClipRect na i . f))

-- Combo dropdown scrollbar sizes: lane thickness and the shortest a thumb
-- ever gets.
comboSbW, comboSbMinThumb :: Float
comboSbW = 10
comboSbMinThumb = 24

-- | Scrollbar geometry for a combo dropdown, shared by its drawing, its pick
-- and the widget's thumb-drag gesture. The list has no outer margin: rows
-- fill the drop rect edge to edge, and a vertical lane sits on the right
-- when rows overflow the window, a horizontal one on the bottom when the
-- widest row overflows the width. Returns (inner rows area, vertical bar,
-- horizontal bar, usable content width).
comboScrollGeom ::
  Rect ->
  Int ->
  Int ->
  Int ->
  Float ->
  Float ->
  (Rect, Maybe ScrollBarLayout, Maybe ScrollBarLayout, Float)
comboScrollGeom (Rect dx dy dw dh) n vis win xOff contentW =
  (Rect dx dy usableW usableH, vSb, hSb, usableW)
  where
    vScroll = n > vis && vis > 0
    usableW = max 0 (dw - if vScroll then comboSbW else 0)
    hScroll = contentW > usableW && contentW > 0
    -- Rows fill the drop rect from the top, stopping short of the lanes.
    usableH = max 0 (dh - if hScroll then comboSbW else 0)
    maxWin = fromIntegral (max 0 (n - vis))
    maxOffX = max 0 (contentW - usableW)
    -- Lanes sit flush against the dropdown border and share the corner. A
    -- thumb shows @num / den@ of its track, @off@ of @maxOff@ along it.
    thumb track num den off = scrollThumbSpan track comboSbMinThumb num den off
    vx = dx + dw - comboSbW
    hy = dy + dh - comboSbW
    (ty, thumbH) = thumb usableH (fromIntegral vis) (fromIntegral n) (fromIntegral win) (max 1 maxWin)
    (tx, thumbW) = thumb usableW usableW contentW xOff (max 1 maxOffX)
    vSb
      | vScroll = Just (ScrollBarLayout (Rect vx dy comboSbW usableH) (Rect (vx + 2) (dy + ty) (comboSbW - 4) thumbH) maxWin)
      | otherwise = Nothing
    hSb
      | hScroll = Just (ScrollBarLayout (Rect dx hy usableW comboSbW) (Rect (dx + tx) (hy + 2) thumbW (comboSbW - 4)) maxOffX)
      | otherwise = Nothing

-- | Combo dropdown rect: like a select's ('dropRect'), but with no outer
-- margin (rows start flush at the top), and the height reserves a flush
-- bottom scrollbar lane when the widest row overflows, so the horizontal bar
-- never covers the bottommost row. Must agree with 'comboScrollGeom' on when
-- lanes appear (same inputs, same formulas).
comboDropRect :: Size -> Float -> Float -> Float -> Float -> Int -> Int -> Float -> Rect
comboDropRect size x y w h nRows nTotal contentW =
  let vLaneW = if nTotal > nRows then comboSbW else 0
      hScroll = contentW > max 0 (w - vLaneW) && contentW > 0
   in dropRect size x y w h (fromIntegral nRows * menuItemRowH + (if hScroll then comboSbW else 0))

-- | Row index at @mouseY@ for a combo dropdown, whose rows start flush at the
-- drop rect's top.
comboDropPickIndex :: Rect -> Float -> Int -> Float -> Maybe Int
comboDropPickIndex (Rect _ dy _ _) itemH nOpts mouseY =
  let rel = mouseY - dy
   in if rel < 0 || rel >= itemH * fromIntegral nOpts
        then Nothing
        else Just (clamp 0 (nOpts - 1) (floor (rel / max itemH 1)))
