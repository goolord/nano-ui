-- | Post-layout select/dropdown picking, keyboard navigation, and dismissal.
module NanoUI.Internal.Frame.Select
  ( selectDropRect
  , selectDropPickIndex
  , closeSelectOnOutsideClick
  , finalizeSelectKeyboard
  , finalizeSelectPick
  , drawSelectOverlays
  , collectSelectDropdownSpans
  , overlayMenuOwnerAt
  , overlayMenuRects
  , routePointer
  , comboDropRect
  , comboDropPickIndex
  , comboScrollGeom
  , focusedComboNode
  ) where

import Control.Monad (filterM, forM, forM_, unless, when)
import Data.IORef (readIORef, writeIORef)
import Data.List (sortOn)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, maybeToList)
import qualified Data.Text as T
import NanoUI.Internal.Context
import NanoUI.Internal.Draw (pushRect, pushRoundedRect, pushText, withClip)
import NanoUI.Internal.Font (FontMetrics, centeredTextY, menuItemPadX, menuItemRowH, menuOuterPad, widgetContentInset)
import NanoUI.Internal.Frame.Chrome (menuPanelBounds, overlayMenuStyle, paintMenuAccent, paintMenuPanel)
import NanoUI.Internal.Frame.Hit (widgetOverlayAllowed)
import NanoUI.Internal.Id (WidgetId (..))
import NanoUI.Internal.Input (Input (..), Key (..), inputKeys, inputKeysElem, inputMousePos, inputMousePressed, inputPointerHeld)
import NanoUI.Internal.Layout.Arena (NodeIdx, NodeType (NodeSelect, NodeTextInput), getNodeType, lookupNodeByKey, lookupNodeByWidgetId, getOptions, getNodeRect, getWidgetId)
import NanoUI.Internal.Monad (whenM, (<&&>))
import NanoUI.Internal.Store (fieldFloat, fieldInt, fieldText, findSlot, insertSlot, setFieldSelection)
import NanoUI.Internal.Style (Style (..), Theme (..), scrollBarThumbColor, scrollBarThumbHoverColor, scrollBarTrackColor, themeAccent, themeInput)
import NanoUI.Internal.Types (Color (..), Rect (..), V2 (..), clamp, rectContains)

-- | An open dropdown: a select with its open flag set, or a combo box (a
-- search field carrying options) exactly while it holds focus.
data Dropdown = Dropdown
  { ddWidget :: !WidgetId
  , ddCombo :: !Bool
  , ddOptions :: [T.Text]
  , ddAnchor :: !Rect
  , ddRect :: !Rect
  , ddPicked :: !Int
  -- ^ Row shown as picked: the select's value, or the combo's keyboard
  -- highlight relative to its window (-1 highlights nothing).
  , ddComboScrollX :: !Float
  , ddComboGeom :: (Rect, Maybe (Rect, Rect), Maybe (Rect, Rect), Float)
  -- ^ A combo's 'comboScrollGeom'.
  , ddComboDrag :: !Int
  -- ^ The combo scrollbar whose thumb is being dragged: 0 none, 1 vertical,
  -- 2 horizontal.
  }

-- | Painted bounds of every open dropdown and of the text-edit menu. The
-- overlays paint over the page in the retained texture, so a clip frame must
-- repaint where they are and where they were.
overlayMenuRects :: Context -> IO [Rect]
overlayMenuRects ctx = do
  dropdowns <- openDropdowns ctx
  menu <- getsInteraction ctx isTextInputMenu
  pure (map menuPanelBounds (map ddRect dropdowns ++ maybe [] (pure . textInputMenuRect) menu))

-- | Every open dropdown, in arena order.
openDropdowns :: Context -> IO [Dropdown]
openDropdowns ctx@Context {ctxNodeArena = na} = do
  store <- getStore ctx
  -- Selects open only through the store flag and combos only while focused,
  -- so at most two nodes hold a dropdown, and both are looked up directly.
  sel <- openSelectNode ctx store
  combo <- focusedComboNode ctx
  forM (sortOn fst (catMaybes [(,False) <$> sel, (,True) <$> combo])) $ \(idx, isCombo) -> do
    wid <- getWidgetId na idx
    build store idx wid isCombo
  where
    build store idx wid combo = do
      opts <- getOptions na idx
      Rect x y w h <- getNodeRect na idx
      let key = intKey wid
          slotInt slot def = findSlot fieldInt def (slotKey slot key) store
          slotFloat slot = findSlot fieldFloat 0 (slotKey slot key) store
          nOpts = length opts
          rows = slotInt SlotComboCount nOpts
          window = slotInt SlotComboScroll 0
          scrollX = slotFloat SlotComboScrollX
          contentW = slotFloat SlotComboContentW
          rect
            | combo = comboDropRect x y w h nOpts rows contentW
            | otherwise = selectDropRect x y w h nOpts
      pure
        Dropdown
          { ddWidget = wid
          , ddCombo = combo
          , ddOptions = opts
          , ddAnchor = Rect x y w h
          , ddRect = rect
          , ddPicked =
              if combo
                then slotInt SlotComboHighlight (-1) - window
                else findSlot fieldInt 0 key store
          , ddComboScrollX = scrollX
          , ddComboGeom = comboScrollGeom rect rows nOpts window scrollX contentW
          , ddComboDrag = slotInt SlotComboDrag 0
          }

-- | The open dropdowns the modal state lets be drawn and picked from, in
-- arena order.
allowedDropdowns :: Context -> IO [Dropdown]
allowedDropdowns ctx = filterM (widgetOverlayAllowed ctx . ddWidget) =<< openDropdowns ctx

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

-- | Rows of an open dropdown, shared by its painter and its text spans. Combo
-- rows sit flush at the drop rect's top edge (no outer margin) and scroll
-- horizontally; select rows keep their padded layout.
dropdownRows :: FontMetrics -> Theme -> V2 -> Dropdown -> [DropdownRow]
dropdownRows fm theme mouse dd =
  let Rect dx dy dw _ = ddRect dd
      top = if ddCombo dd then dy else dy + menuOuterPad
      textX0 = dx + menuItemPadX + fst (widgetContentInset fm)
      textX = if ddCombo dd then textX0 - ddComboScrollX dd else textX0
      style = overlayMenuStyle theme
   in [ DropdownRow opt row textX hovered fill (if picked then themeAccent theme else styleFg style)
      | (i, opt) <- zip [0 ..] (ddOptions dd)
      , let row = Rect dx (top + menuItemRowH * fromIntegral i) dw menuItemRowH
            hovered = rectContains row mouse
            picked = i == ddPicked dd
            fill
              | hovered = Just (styleHoverBg style)
              | picked = Just (styleActiveBg style)
              | otherwise = Nothing
      ]

-- | The widget whose menu or dropdown is on top at @mouse@.
overlayMenuOwnerAt :: Context -> V2 -> IO (Maybe WidgetId)
overlayMenuOwnerAt ctx mouse =
  overlayRouteAt ctx mouse >>= \case
    Just RouteTextMenu -> fmap textInputMenuWidget <$> getsInteraction ctx isTextInputMenu
    Just (RouteDropdown wid) -> pure (Just wid)
    _ -> pure Nothing

-- | The route to what the frame draws over every layer, when that is what is
-- on top at @mouse@: the text-edit menu, then an open dropdown the modal
-- state lets receive input. They are drawn rather than laid out, so nothing
-- but this knows they cover what is under them.
overlayRouteAt :: Context -> V2 -> IO (Maybe PointerRoute)
overlayRouteAt ctx mouse = do
  mMenu <- getsInteraction ctx isTextInputMenu
  case mMenu of
    Just m | rectContains (textInputMenuRect m) mouse -> pure (Just RouteTextMenu)
    _ -> do
      dropdowns <- openDropdowns ctx
      let under = [ddWidget dd | dd <- reverse dropdowns, rectContains (ddRect dd) mouse]
      fmap RouteDropdown . listToMaybe <$> filterM (widgetOverlayAllowed ctx) under

-- | Decide where this frame's pointer goes, before the view runs. A button
-- that is already down keeps the route it went down with, through its
-- release, so a drag that wanders over something else stays where it began.
-- A press always starts over, even with the other button still down: it goes
-- to what is under it now, not through it to where the first one landed.
routePointer :: Context -> Input -> IO PointerRoute
routePointer ctx inp = do
  (held, old) <- getsInteraction ctx (\s -> (isPointerHeld s, isPointerRoute s))
  let pressed = inputMousePressed inp || inputMouseRightPressed inp
      released = inputMouseReleased inp || inputMouseRightReleased inp
      -- A hold that ended without its release being seen is over too.
      holding = held && not pressed && (inputPointerHeld inp || released)
      mouse = inputMousePos inp
  -- What is on top: a menu or dropdown, then the floating panel in front,
  -- then the page.
  route <-
    if holding
      then pure old
      else maybe (RouteLayer <$> floatingLayerAt ctx mouse) pure =<< overlayRouteAt ctx mouse
  -- Most frames change neither, and an idle frame should write nothing.
  when (route /= old || inputPointerHeld inp /= held) $
    modifyInteraction ctx $ \s ->
      s
        { isPointerRoute = route
        , isPointerHeld = inputPointerHeld inp
        , isColumnResize = isColumnResize s && inputPointerHeld inp
        }
  pure route

closeSelectOnOutsideClick :: Context -> Input -> IO ()
closeSelectOnOutsideClick ctx inp =
  when (inputMousePressed inp || inputMouseReleased inp) $ do
    store <- getStore ctx
    when (anySelectOpen store) $ do
      let mouse = inputMousePos inp
      dropdowns <- openDropdowns ctx
      unless (any (\dd -> rectContains (ddAnchor dd) mouse || rectContains (ddRect dd) mouse) dropdowns) $
        setStore ctx (closeSelects store)

finalizeSelectKeyboard :: Context -> Input -> IO ()
finalizeSelectKeyboard ctx@Context {ctxNodeArena = na} inp = do
  let has k = inputKeysElem k (inputKeys inp)
      wantNext = has KeyDown || has KeyRight
      wantStep = wantNext || has KeyUp || has KeyLeft
      wantEsc = has KeyEscape
      wantEnter = has KeyEnter
  when (wantStep || wantEsc || wantEnter) $ do
    focus <- readIORef (ctxFocusId ctx)
    store <- getStore ctx
    -- Arrows step the focused enabled select, open or not. Otherwise the keys
    -- go to the open select.
    let enabledSelect idx = ((== NodeSelect) <$> getNodeType na idx) <&&> (not <$> isDisabled ctx focus)
    focused <-
      if wantStep then keepNode enabledSelect =<< lookupNodeByWidgetId na focus else pure Nothing
    target <- case focused of
      Just idx -> pure (Just (idx, isSelectOpen store (intKey focus)))
      Nothing -> fmap (,True) <$> openSelectNode ctx store
    forM_ target $ \(idx, open) -> do
      wid <- getWidgetId na idx
      whenM (widgetOverlayAllowed ctx wid) $
        if wantEsc || wantEnter
          then when open $ do
            setStore ctx (setSelectOpen store (intKey wid) False)
            when wantEsc $ markEscapeConsumed ctx
            markDirty ctx
          else do
            n <- length <$> getOptions na idx
            when (n > 0) $ do
              let key = intKey wid
                  cur = findSlot fieldInt 0 key store
                  next = clamp 0 (n - 1) (cur + if wantNext then 1 else -1)
              when (next /= cur) $ do
                setStore ctx (insertSlot fieldInt key next store)
                markDirty ctx

-- | The node of the select whose dropdown the store holds open.
openSelectNode :: Context -> WidgetStore -> IO (Maybe NodeIdx)
openSelectNode Context {ctxNodeArena = na} store
  | not (anySelectOpen store) = pure Nothing
  | otherwise =
      keepNode (fmap (== NodeSelect) . getNodeType na) =<< lookupNodeByKey na (storeOpenSelect store)

-- | The focused node when it is a combo (a text input carrying options),
-- which owns an open dropdown for as long as it holds focus.
focusedComboNode :: Context -> IO (Maybe NodeIdx)
focusedComboNode ctx@Context {ctxNodeArena = na} =
  keepNode (\idx -> ((== NodeTextInput) <$> getNodeType na idx) <&&> (not . null <$> getOptions na idx))
    =<< lookupNodeByWidgetId na =<< readIORef (ctxFocusId ctx)

-- | The node, when it satisfies @p@.
keepNode :: (NodeIdx -> IO Bool) -> Maybe NodeIdx -> IO (Maybe NodeIdx)
keepNode p = fmap listToMaybe . filterM p . maybeToList

finalizeSelectPick :: Context -> Input -> IO ()
finalizeSelectPick ctx inp =
  when (inputMousePressed inp || inputMouseReleased inp) $ do
    let mouse@(V2 _ mouseY) = inputMousePos inp
    dropdowns <- allowedDropdowns ctx
    forM_ dropdowns $ \dd ->
      when (rectContains (ddRect dd) mouse) $ do
        st <- getStore ctx
        let wid = ddWidget dd
            key = intKey wid
            nOpts = length (ddOptions dd)
        if ddCombo dd
          then do
            -- Combo: pick on press only, never from the scrollbar lanes, so
            -- finishing a thumb drag cannot commit a row. Picking commits the
            -- option text into the field and defocuses it: the combo's
            -- dropdown is visible exactly while focused, so the menu
            -- disappears with the pick.
            let (_, vSb, hSb, _) = ddComboGeom dd
                onLane = any (\(track, _) -> rectContains track mouse) (catMaybes [vSb, hSb])
            when (inputMousePressed inp && not onLane) $
              forM_ (comboDropPickIndex (ddRect dd) menuItemRowH nOpts mouseY) $ \picked -> do
                let txt = fromMaybe "" (listToMaybe (drop picked (ddOptions dd)))
                    len = T.length txt
                setStore ctx $
                  insertSlot fieldText key txt . setFieldSelection key len len $ st
                writeIORef (ctxFocusId ctx) (WidgetId 0)
                markDirty ctx
          else
            forM_ (selectDropPickIndex (ddRect dd) menuItemRowH nOpts mouseY) $ \picked -> do
              setStore ctx (setSelectOpen (insertSlot fieldInt key picked st) key False)
              writeIORef (ctxFocusId ctx) wid
              markDirty ctx

-- | Vertical gap between the select widget and its dropdown menu.
selectDropGap :: Float
selectDropGap = 4

selectDropRect :: Float -> Float -> Float -> Float -> Int -> Rect
selectDropRect x y w h nOpts =
  Rect x (y + h + selectDropGap) w (menuItemRowH * fromIntegral nOpts + 2 * menuOuterPad)

-- | Row index at @mouseY@ for a select dropdown, whose rows sit centred in
-- the drop rect.
selectDropPickIndex :: Rect -> Float -> Int -> Float -> Maybe Int
selectDropPickIndex (Rect dx dy dw dh) itemH nOpts =
  comboDropPickIndex (Rect dx (dy + max 0 ((dh - itemH * fromIntegral nOpts) / 2)) dw dh) itemH nOpts

-- Combo dropdown scrollbar sizes: lane thickness and the shortest a thumb
-- ever gets.
comboSbW, comboSbMinThumb :: Float
comboSbW = 10
comboSbMinThumb = 24

-- | Scrollbar geometry for a combo dropdown, shared by the overlay painter,
-- the pick guard, and the widget's thumb-drag gesture. The list has no outer
-- margin: rows fill the drop rect edge to edge, and a vertical lane sits on
-- the right when rows overflow the window, a horizontal one on the bottom
-- when the widest row overflows the width. Returns (inner rows area,
-- vertical (track, thumb), horizontal (track, thumb), usable content width).
comboScrollGeom ::
  Rect ->
  Int ->
  Int ->
  Int ->
  Float ->
  Float ->
  (Rect, Maybe (Rect, Rect), Maybe (Rect, Rect), Float)
comboScrollGeom (Rect dx dy dw dh) n vis win xOff contentW =
  (Rect dx dy usableW usableH, vSb, hSb, usableW)
  where
    vScroll = n > vis && vis > 0
    usableW = max 0 (dw - if vScroll then comboSbW else 0)
    hScroll = contentW > usableW && contentW > 0
    -- Rows fill the drop rect from the top, stopping short of the lanes.
    usableH = max 0 (dh - if hScroll then comboSbW else 0)
    -- Lanes sit flush against the dropdown border and share the corner. A
    -- thumb shows @num / den@ of its track, @off@ of @maxOff@ along it.
    thumb track num den off maxOff =
      let len = max 1 track
          size = clamp (min comboSbMinThumb len) len (len * num / den)
       in ((len - size) * clamp 0 maxOff off / maxOff, size)
    vx = dx + dw - comboSbW
    hy = dy + dh - comboSbW
    (ty, thumbH) =
      thumb usableH (fromIntegral vis) (fromIntegral n) (fromIntegral win) (fromIntegral (max 1 (n - vis)))
    (tx, thumbW) = thumb usableW usableW contentW xOff (max 1 (contentW - usableW))
    vSb
      | vScroll = Just (Rect vx dy comboSbW usableH, Rect (vx + 2) (dy + ty) (comboSbW - 4) thumbH)
      | otherwise = Nothing
    hSb
      | hScroll = Just (Rect dx hy usableW comboSbW, Rect (dx + tx) (hy + 2) thumbW (comboSbW - 4))
      | otherwise = Nothing

-- | Combo dropdown rect: like 'selectDropRect', but with no outer margin
-- (rows start flush at the top), and the height reserves a flush bottom
-- scrollbar lane when the widest row overflows, so the horizontal bar never
-- covers the bottommost row. Must agree with 'comboScrollGeom' on when lanes
-- appear (same inputs, same formulas).
comboDropRect :: Float -> Float -> Float -> Float -> Int -> Int -> Float -> Rect
comboDropRect x y w h nRows nTotal contentW =
  let vLaneW = if nTotal > nRows then comboSbW else 0
      hScroll = contentW > max 0 (w - vLaneW) && contentW > 0
   in Rect x (y + h + selectDropGap) w (fromIntegral nRows * menuItemRowH + (if hScroll then comboSbW else 0))

-- | Row index at @mouseY@ for a combo dropdown, whose rows start flush at the
-- drop rect's top (unlike 'selectDropPickIndex', which centers them).
comboDropPickIndex :: Rect -> Float -> Int -> Float -> Maybe Int
comboDropPickIndex (Rect _ dy _ _) itemH nOpts mouseY =
  let rel = mouseY - dy
   in if rel < 0 || rel >= itemH * fromIntegral nOpts
        then Nothing
        else Just (clamp 0 (nOpts - 1) (floor (rel / max itemH 1)))

-- | Paint each open dropdown (select or combo). The combo list clips to its
-- inner area (so x-shifted text and row fills stop at the scrollbar lanes)
-- and gets vertical / horizontal scrollbars when the filtered rows or the
-- widest row overflow the window.
drawSelectOverlays :: Context -> Input -> IO ()
drawSelectOverlays ctx@Context {ctxDrawArena = da, ctxFontMetrics = fm} inp = do
  dropdowns <- allowedDropdowns ctx
  forM_ dropdowns $ \dd -> do
    theme <- widgetTheme ctx (ddWidget dd)
    let style = overlayMenuStyle theme
        paintRows =
          forM_ (dropdownRows fm theme (inputMousePos inp) dd) $ \row -> do
            mapM_ (pushRect da (drRect row)) (drFill row)
            when (drHovered row) $ paintMenuAccent da theme (drRect row)
            unless (T.null (drOption row)) $ do
              (_, th) <- ctxMeasureText ctx (drOption row)
              let Rect _ ry _ rh = drRect row
              pushText da fm (drTextX row) (centeredTextY fm ry rh th) (drOption row) (drFg row)
    paintMenuPanel da theme style (ddRect dd)
    if ddCombo dd
      then do
        let (inner, vSb, hSb, _) = ddComboGeom dd
            base = themeInput theme
            mouse = inputMousePos inp
            drag = ddComboDrag dd
            -- As a scroller's bar: brighter under the pointer or while dragged.
            hot kind (track, thumb) =
              drag == kind || drag == 0 && (rectContains track mouse || rectContains thumb mouse)
            drawBar kind bar@(track, thumb) = do
              pushRect da track (scrollBarTrackColor base theme)
              let thumbColor = if hot kind bar then scrollBarThumbHoverColor else scrollBarThumbColor
              pushRoundedRect da thumb 3 (thumbColor base theme)
        withClip da inner paintRows
        mapM_ (drawBar 1) vSb
        mapM_ (drawBar 2) hSb
      else paintRows

collectSelectDropdownSpans :: Context -> Input -> IO [(Rect, T.Text, Color, Color, Rect)]
collectSelectDropdownSpans ctx@Context {ctxFontMetrics = fm} inp = do
  dropdowns <- allowedDropdowns ctx
  fmap concat . forM dropdowns $ \dd -> do
    theme <- widgetTheme ctx (ddWidget dd)
    let bg row = fromMaybe (styleBg (overlayMenuStyle theme)) (drFill row)
    forM (filter (not . T.null . drOption) (dropdownRows fm theme (inputMousePos inp) dd)) $ \row -> do
      (tw, th) <- ctxMeasureText ctx (drOption row)
      let Rect _ ry _ rh = drRect row
      pure (Rect (drTextX row) (centeredTextY fm ry rh th) tw th, drOption row, drFg row, bg row, ddRect dd)
