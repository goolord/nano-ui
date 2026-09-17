{-# LANGUAGE DataKinds #-}

module NanoUI.Frame.Select
  ( selectDropRect
  , selectItemH
  , selectDropPickIndex
  , closeSelectOnOutsideClick
  , finalizeSelectKeyboard
  , finalizeSelectPick
  , markSelectDropPress
  , drawSelectOverlays
  , collectSelectDropdownSpans
  , findSelectUnderMouse
  , overlayMenuOwnerAt
  , cacheOpenSelectDrop
  , tagSelectClippedSpans
  , comboDropRect
  , comboDropPickIndex
  , comboScrollGeom
  ) where

import Control.Monad (forM, forM_, unless, when)
import Data.Foldable (find)
import Data.IORef (readIORef, writeIORef)
import qualified Data.IntMap.Strict as IM
import Data.Maybe (catMaybes, listToMaybe)
import qualified Data.Text as T
import NanoUI.Context
  ( Context (..)
  , TextInputMenu (..)
  , WidgetStore (..)
  , anySelectOpen
  , closeSelects
  , getStore
  , getTextInputMenu
  , intKey
  , isSelectOpen
  , markDirty
  , markEscapeConsumed
  , setOpenSelectDrop
  , setSelectDropPress
  , setSelectOpen
  , setStore
  , widgetTheme
  , isDisabled
  )
import NanoUI.Draw (pushRect, pushRoundedRect, pushText, withClip)
import NanoUI.Font (FontMetrics, centeredTextY, menuItemPadX, menuItemRowH, menuOuterPad, widgetContentInset)
import NanoUI.Frame.Chrome (overlayMenuStyle, paintMenuAccent, paintMenuPanel)
import NanoUI.Frame.Hit (findNodeByWidgetId, widgetOverlayAllowed)
import NanoUI.Frame.Scroll.Geometry (padTextClipRect)
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Input (Input (..), Key (..), foldInputKeys, inputKeys, inputMouseDown, inputMousePos, inputMousePressed)
import NanoUI.Layout.Arena (NodeType (NodeSelect, NodeTextInput), findNodeM, foldNodeRevM, getNodeType, getOptions, getRect, getWidgetId)
import NanoUI.Store (slotAnchor, slotComboContentW, slotComboCount, slotComboHighlight, slotComboScroll, slotComboScrollX, slotCursor, slotKey)
import NanoUI.Style (Style (..), Theme (..), scrollBarThumbColor, scrollBarTrackColor, themeAccent, themeInput)
import NanoUI.Types (Color (..), Rect (..), V2 (..), rectContains, rectIntersect)
import NanoUI.WidgetText (selectChevronReserve)

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
  , ddComboRows :: !Int
  , ddComboWindow :: !Int
  , ddComboScrollX :: !Float
  , ddComboContentW :: !Float
  }

-- | Every open dropdown, in arena order.
openDropdowns :: Context -> IO [Dropdown]
openDropdowns ctx = do
  store <- getStore ctx
  focus <- readIORef (ctxFocusId ctx)
  -- Selects open only through the store flag and combos only while focused,
  -- so most frames skip the walk.
  if not (anySelectOpen store) && hashWidgetId focus == 0
    then pure []
    else foldNodeRevM na (\acc idx -> maybe acc (: acc) <$> dropdownAt store focus idx) []
  where
    na = ctxNodeArena ctx
    dropdownAt store focus idx =
      getNodeType na idx >>= \case
        NodeSelect -> do
          wid <- getWidgetId na idx
          if isSelectOpen store (intKey wid) then Just <$> build store idx wid False else pure Nothing
        NodeTextInput -> do
          wid <- getWidgetId na idx
          opts <- getOptions na idx
          if wid /= focus || null opts then pure Nothing else Just <$> build store idx wid True
        _ -> pure Nothing
    build store idx wid combo = do
      opts <- getOptions na idx
      (x, y, w, h) <- getRect na idx
      let key = intKey wid
          slotInt slot def = IM.findWithDefault def (slotKey slot key) (storeInt store)
          slotFloat slot = IM.findWithDefault 0 (slotKey slot key) (storeFloat store)
          nOpts = length opts
          rows = slotInt slotComboCount nOpts
          window = slotInt slotComboScroll 0
          contentW = slotFloat slotComboContentW
          fm = ctxFontMetrics ctx
      pure
        Dropdown
          { ddWidget = wid
          , ddCombo = combo
          , ddOptions = opts
          , ddAnchor = Rect x y w h
          , ddRect =
              if combo
                then comboDropRect fm x y w h nOpts rows contentW
                else selectDropRect fm x y w h nOpts
          , ddPicked =
              if combo
                then slotInt slotComboHighlight (-1) - window
                else IM.findWithDefault 0 key (storeInt store)
          , ddComboRows = rows
          , ddComboWindow = window
          , ddComboScrollX = slotFloat slotComboScrollX
          , ddComboContentW = contentW
          }

-- | One placed row of an open dropdown.
data DropdownRow = DropdownRow
  { drIndex :: !Int
  , drOption :: T.Text
  , drRect :: !Rect
  , drTextX :: !Float
  , drHovered :: !Bool
  }

-- | Rows of an open dropdown, shared by its painter and its text spans. Combo
-- rows sit flush at the drop rect's top edge (no outer margin) and scroll
-- horizontally; select rows keep their padded layout.
dropdownRows :: FontMetrics -> V2 -> Dropdown -> [DropdownRow]
dropdownRows fm mouse dd =
  let Rect dx dy dw _ = ddRect dd
      top = if ddCombo dd then dy else dy + menuOuterPad
      textX0 = dx + menuItemPadX + fst (widgetContentInset fm)
      textX = if ddCombo dd then textX0 - ddComboScrollX dd else textX0
   in [ DropdownRow i opt row textX (rectContains row mouse)
      | (i, opt) <- zip [0 ..] (ddOptions dd)
      , let row = Rect dx (top + menuItemRowH * fromIntegral i) dw menuItemRowH
      ]

overlayMenuOwnerAt :: Context -> V2 -> IO (Maybe WidgetId)
overlayMenuOwnerAt ctx mouse = do
  mMenu <- getTextInputMenu ctx
  case mMenu of
    Just m | rectContains (textInputMenuRect m) mouse -> pure (Just (textInputMenuWidget m))
    _ -> fmap ddWidget . find (\dd -> rectContains (ddRect dd) mouse) <$> openDropdowns ctx

cacheOpenSelectDrop :: Context -> IO ()
cacheOpenSelectDrop ctx = do
  dropdowns <- openDropdowns ctx
  setOpenSelectDrop ctx ((\dd -> (ddWidget dd, ddRect dd)) <$> listToMaybe dropdowns)

markSelectDropPress :: Context -> Input -> IO ()
markSelectDropPress ctx inp =
  when (inputMouseDown inp) $ do
    store <- getStore ctx
    when (anySelectOpen store) $ do
      let mouse = inputMousePos inp
      dropdowns <- openDropdowns ctx
      when (any (\dd -> rectContains (ddAnchor dd) mouse || rectContains (ddRect dd) mouse) dropdowns) $
        setSelectDropPress ctx True

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
finalizeSelectKeyboard ctx inp = do
  let (wantNext, wantPrev, wantEsc, wantEnter) =
        foldInputKeys
          ( \(n, p, e, r) k ->
              ( n || k == KeyDown || k == KeyRight
              , p || k == KeyUp || k == KeyLeft
              , e || k == KeyEscape
              , r || k == KeyEnter
              )
          )
          (False, False, False, False)
          (inputKeys inp)
      wantStep = wantNext || wantPrev
  when (wantStep || wantEsc || wantEnter) $ do
    focus <- readIORef (ctxFocusId ctx)
    store <- getStore ctx
    mTarget <- pickSelectKeyboardTarget ctx focus store wantStep
    forM_ mTarget $ \(wid, open) -> do
      allow <- widgetOverlayAllowed ctx wid
      when allow $
        if wantEsc || wantEnter
          then when open $ do
            setStore ctx (setSelectOpen store (intKey wid) False)
            when wantEsc $ markEscapeConsumed ctx
            markDirty ctx
          else do
            mIdx <- findNodeByWidgetId ctx wid
            forM_ mIdx $ \idx -> do
              n <- length <$> getOptions (ctxNodeArena ctx) idx
              when (n > 0) $ do
                let key = intKey wid
                    cur = IM.findWithDefault 0 key (storeInt store)
                    next = max 0 (min (n - 1) (cur + if wantNext then 1 else -1))
                when (next /= cur) $ do
                  setStore ctx (store {storeInt = IM.insert key next (storeInt store)})
                  markDirty ctx

pickSelectKeyboardTarget :: Context -> WidgetId -> WidgetStore -> Bool -> IO (Maybe (WidgetId, Bool))
pickSelectKeyboardTarget ctx focus store wantStep = do
  mFocus <- if wantStep then selectWidgetIfAny ctx focus else pure Nothing
  case mFocus of
    Just wid -> pure (Just (wid, isSelectOpen store (intKey wid)))
    Nothing -> fmap (,True) <$> findOpenSelectWidget ctx

selectWidgetIfAny :: Context -> WidgetId -> IO (Maybe WidgetId)
selectWidgetIfAny ctx wid
  | hashWidgetId wid == 0 = pure Nothing
  | otherwise = do
      mIdx <- findNodeByWidgetId ctx wid
      case mIdx of
        Nothing -> pure Nothing
        Just idx -> do
          nt <- getNodeType (ctxNodeArena ctx) idx
          disabled <- isDisabled ctx wid
          pure (if nt == NodeSelect && not disabled then Just wid else Nothing)

findOpenSelectWidget :: Context -> IO (Maybe WidgetId)
findOpenSelectWidget ctx = do
  store <- getStore ctx
  let na = ctxNodeArena ctx
  mIdx <-
    findNodeM na $ \idx -> do
      nt <- getNodeType na idx
      if nt /= NodeSelect
        then pure False
        else isSelectOpen store . intKey <$> getWidgetId na idx
  traverse (getWidgetId na) mIdx

finalizeSelectPick :: Context -> Input -> IO ()
finalizeSelectPick ctx inp =
  when (inputMousePressed inp || inputMouseReleased inp) $ do
    let mouse@(V2 _ mouseY) = inputMousePos inp
    dropdowns <- openDropdowns ctx
    forM_ dropdowns $ \dd -> do
      allow <- widgetOverlayAllowed ctx (ddWidget dd)
      when (allow && rectContains (ddRect dd) mouse) $ do
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
            let (_, vSb, hSb, _) = comboScrollGeom (ddRect dd) (ddComboRows dd) nOpts (ddComboWindow dd) (ddComboScrollX dd) (ddComboContentW dd)
                onLane = any (\(track, _) -> rectContains track mouse) (catMaybes [vSb, hSb])
            when (inputMousePressed inp && not onLane) $
              forM_ (comboDropPickIndex (ddRect dd) menuItemRowH nOpts mouseY) $ \picked -> do
                let txt = maybe "" id (listToMaybe (drop picked (ddOptions dd)))
                    len = T.length txt
                setStore
                  ctx
                  ( st
                      { storeText = IM.insert key txt (storeText st)
                      , storeInt =
                          IM.insert (slotKey slotCursor key) len $
                            IM.insert (slotKey slotAnchor key) len (storeInt st)
                      }
                  )
                writeIORef (ctxFocusId ctx) (WidgetId 0)
                markDirty ctx
          else
            forM_ (selectDropPickIndex (ddRect dd) menuItemRowH nOpts mouseY) $ \picked -> do
              setStore ctx (setSelectOpen (st {storeInt = IM.insert key picked (storeInt st)}) key False)
              writeIORef (ctxFocusId ctx) wid
              markDirty ctx

-- | Topmost open dropdown owner (in reverse arena order) whose anchor or menu
-- is under @mouse@ and that the modal state lets receive input.
findSelectUnderMouse :: Context -> V2 -> IO (Maybe WidgetId)
findSelectUnderMouse ctx mouse = do
  dropdowns <- openDropdowns ctx
  firstAllowed [dd | dd <- reverse dropdowns, rectContains (ddAnchor dd) mouse || rectContains (ddRect dd) mouse]
  where
    firstAllowed [] = pure Nothing
    firstAllowed (dd : rest) = do
      allow <- widgetOverlayAllowed ctx (ddWidget dd)
      if allow then pure (Just (ddWidget dd)) else firstAllowed rest

selectItemH :: Float -> Float
selectItemH _ = menuItemRowH

-- | Vertical gap between the select widget and its dropdown menu.
selectDropGap :: Float
selectDropGap = 4

selectDropRect :: FontMetrics -> Float -> Float -> Float -> Float -> Int -> Rect
selectDropRect _fm x y w h nOpts =
  Rect x (y + h + selectDropGap) w (menuItemRowH * fromIntegral nOpts + 2 * menuOuterPad)

selectDropPickIndex :: Rect -> Float -> Int -> Float -> Maybe Int
selectDropPickIndex dropRect itemH nOpts mouseY =
  let Rect _ dy _ dh = dropRect
      innerH = itemH * fromIntegral nOpts
      rel = mouseY - dy - max 0 ((dh - innerH) / 2)
   in if rel < 0 || rel >= innerH
        then Nothing
        else Just (max 0 (min (nOpts - 1) (floor (rel / max itemH 1))))

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
  let
    vScroll = n > vis && vis > 0
    vLaneW = if vScroll then comboSbW else 0
    usableW = max 0 (dw - vLaneW)
    hScroll = contentW > usableW && contentW > 0
    hLaneH = if hScroll then comboSbW else 0
    -- Rows fill the drop rect from the top, stopping short of the lanes.
    inner = Rect dx dy (max 0 (dw - vLaneW)) (max 0 (dh - hLaneH))
    -- Lanes sit flush against the dropdown border and share the corner.
    vTrack = Rect (dx + dw - comboSbW) dy comboSbW (max 0 (dh - hLaneH))
    hTrack = Rect dx (dy + dh - comboSbW) (max 0 (dw - vLaneW)) comboSbW
    vSb =
      if vScroll
        then
          let Rect vx vy _ vh = vTrack
              trackH = max 1 vh
              thumbH = max (min comboSbMinThumb trackH) (min trackH (trackH * fromIntegral vis / fromIntegral n))
              maxWin = max 1 (n - vis)
              ty = vy + (trackH - thumbH) * fromIntegral (max 0 (min maxWin win)) / fromIntegral maxWin
           in Just (vTrack, Rect (vx + 2) ty (comboSbW - 4) thumbH)
        else Nothing
    hSb =
      if hScroll
        then
          let Rect hx hy hw _ = hTrack
              trackW = max 1 hw
              thumbW = max (min comboSbMinThumb trackW) (min trackW (trackW * usableW / contentW))
              maxOff = max 1 (contentW - usableW)
              tx = hx + (trackW - thumbW) * max 0 (min maxOff xOff) / maxOff
           in Just (hTrack, Rect tx (hy + 2) thumbW (comboSbW - 4))
        else Nothing
   in (inner, vSb, hSb, usableW)

-- | Combo dropdown rect: like 'selectDropRect', but with no outer margin
-- (rows start flush at the top), and the height reserves a flush bottom
-- scrollbar lane when the widest row overflows, so the horizontal bar never
-- covers the bottommost row. Must agree with 'comboScrollGeom' on when lanes
-- appear (same inputs, same formulas).
comboDropRect :: FontMetrics -> Float -> Float -> Float -> Float -> Int -> Int -> Float -> Rect
comboDropRect _fm x y w h nRows nTotal contentW =
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
        else Just (max 0 (min (nOpts - 1) (floor (rel / max itemH 1))))

drawSelectOverlays :: Context -> Input -> IO ()
drawSelectOverlays ctx inp = do
  dropdowns <- openDropdowns ctx
  forM_ dropdowns $ \dd -> do
    allow <- widgetOverlayAllowed ctx (ddWidget dd)
    when allow $ do
      theme <- widgetTheme ctx (ddWidget dd)
      drawDropdownMenu ctx inp theme dd

-- | Paint one open dropdown (select or combo). The combo list clips to its
-- inner area (so x-shifted text and row fills stop at the scrollbar lanes)
-- and gets vertical / horizontal scrollbars when the filtered rows or the
-- widest row overflow the window.
drawDropdownMenu :: Context -> Input -> Theme -> Dropdown -> IO ()
drawDropdownMenu ctx inp theme dd = do
  let da = ctxDrawArena ctx
      fm = ctxFontMetrics ctx
      style = overlayMenuStyle theme
      paintRows =
        forM_ (dropdownRows fm (inputMousePos inp) dd) $ \row -> do
          let picked = drIndex row == ddPicked dd
              Rect _ ry _ rh = drRect row
          if drHovered row
            then do
              pushRect da (drRect row) (styleHoverBg style)
              paintMenuAccent da theme (drRect row)
            else when picked $ pushRect da (drRect row) (styleActiveBg style)
          unless (T.null (drOption row)) $ do
            (_, th) <- ctxMeasureText ctx (drOption row)
            pushText da fm (drTextX row) (centeredTextY fm ry rh th) (drOption row) $
              if picked then themeAccent theme else styleFg style
  paintMenuPanel da theme style (ddRect dd)
  if ddCombo dd
    then do
      let (inner, vSb, hSb, _) = comboScrollGeom (ddRect dd) (ddComboRows dd) (length (ddOptions dd)) (ddComboWindow dd) (ddComboScrollX dd) (ddComboContentW dd)
          base = themeInput theme
          drawBar (track, thumb) = do
            pushRect da track (scrollBarTrackColor base theme)
            pushRoundedRect da thumb 3 (scrollBarThumbColor base theme)
      withClip da inner paintRows
      mapM_ drawBar vSb
      mapM_ drawBar hSb
    else paintRows

collectSelectDropdownSpans :: Context -> Input -> IO [(Rect, T.Text, Color, Color, Rect)]
collectSelectDropdownSpans ctx inp = do
  dropdowns <- openDropdowns ctx
  let fm = ctxFontMetrics ctx
  fmap concat . forM dropdowns $ \dd -> do
    allow <- widgetOverlayAllowed ctx (ddWidget dd)
    style <- overlayMenuStyle <$> widgetTheme ctx (ddWidget dd)
    if not allow
      then pure []
      else fmap concat . forM (dropdownRows fm (inputMousePos inp) dd) $ \row ->
        if T.null (drOption row)
          then pure []
          else do
            (tw, th) <- ctxMeasureText ctx (drOption row)
            let Rect _ ry _ rh = drRect row
                bg
                  | drHovered row = styleHoverBg style
                  | drIndex row == ddPicked dd = styleActiveBg style
                  | otherwise = styleBg style
            pure [(Rect (drTextX row) (centeredTextY fm ry rh th) tw th, drOption row, styleFg style, bg, ddRect dd)]

tagSelectClippedSpans ::
  Rect -> Float -> Float -> Float -> Float -> FontMetrics -> [(Rect, T.Text, Color, Color)] -> [(Rect, T.Text, Color, Color, Rect)]
tagSelectClippedSpans parentClip x y w h fm spans =
  let (ix, _) = widgetContentInset fm
      textClip = padTextClipRect (Rect (x + ix) y (max 0 (w - ix - selectChevronReserve)) (max 0 h))
   in case rectIntersect parentClip textClip of
        Nothing -> []
        Just clip -> [(rect, txt, fg, bg, clip) | (rect, txt, fg, bg) <- spans]
