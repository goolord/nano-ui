{-# LANGUAGE DataKinds #-}

module NanoUI.Frame.Select
  ( selectDropRect
  , selectDropGap
  , selectItemH
  , selectDropPickIndex
  , closeSelectOnOutsideClick
  , finalizeSelectKeyboard
  , finalizeSelectPick
  , markSelectDropPress
  , openSelectHit
  , drawSelectOverlays
  , collectSelectDropdownSpans
  , findSelectUnderMouse
  , overlayMenuOwnerAt
  , cacheOpenSelectDrop
  , selectTextClip
  , tagSelectClippedSpans
  , comboDropRect
  , comboDropPickIndex
  , comboScrollGeom
  ) where


import Control.Monad (forM, forM_, unless, when)
import Data.IORef (readIORef, writeIORef)
import qualified Data.IntMap.Strict as IM
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
  )
import NanoUI.Draw (pushRect, pushRoundedRect, pushText, withClip)
import NanoUI.Font (FontMetrics, centeredTextY, widgetContentInset)
import NanoUI.Types (HostProfile, isCellHost)
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Input (Input (..), Key (..), foldInputKeys, inputKeys, inputMouseDown, inputMousePos, inputMousePressed)
import NanoUI.Layout.Arena (NodeIdx, NodeType (NodeSelect, NodeTextInput), arenaCount, findNodeRevM, getNodeType, getOptions, getRect, getWidgetId)
import NanoUI.Store (slotAnchor, slotComboContentW, slotComboCount, slotComboHighlight, slotComboScroll, slotComboScrollX, slotCursor, slotKey)
import NanoUI.Style (Style (..), Theme (..), scrollBarThumbColor, scrollBarTrackColor, themeAccent, themeInput)
import NanoUI.Types (Color (..), Rect (..), V2 (..), rectContains, rectH, rectIntersect, rectW, rectX, rectY, v2Y)
import NanoUI.Frame.Scroll.Geometry (padTextClipRect)
import NanoUI.WidgetText (selectChevronReserve)
import NanoUI.Frame.Chrome
  ( fillStyledRect
  , pushMenuShadow
  , strokeStyledRect
  , overlayMenuStyle
  , padDropText
  , textInputMenuItemPadX
  , textInputMenuOuterPad
  )
import NanoUI.Frame.Hit (findNodeByWidgetId, widgetOverlayAllowed)

overlayMenuOwnerAt :: Context -> V2 -> IO (Maybe WidgetId)
overlayMenuOwnerAt ctx mouse = do
  mMenu <- getTextInputMenu ctx
  case mMenu of
    Just m | rectContains (textInputMenuRect m) mouse ->
      pure (Just (textInputMenuWidget m))
    _ -> openSelectDropOwnerAt ctx mouse

-- | Live dropdown owners: selects with their open flag set, and combo boxes —
-- search-style text fields carrying options — exactly while they hold focus
-- (a combo's dropdown is visible whenever its field is focused).
openDropdownOwner :: Context -> NodeIdx -> NodeType -> IO (Maybe WidgetId)
openDropdownOwner ctx idx nt
  | nt == NodeSelect = do
      store <- getStore ctx
      wid <- getWidgetId (ctxNodeArena ctx) idx
      pure (if isSelectOpen store (intKey wid) then Just wid else Nothing)
  | nt == NodeTextInput = do
      wid <- getWidgetId (ctxNodeArena ctx) idx
      focus <- readIORef (ctxFocusId ctx)
      if focus /= wid
        then pure Nothing
        else do
          opts <- getOptions (ctxNodeArena ctx) idx
          pure (if null opts then Nothing else Just wid)
  | otherwise = pure Nothing

openSelectDropOwnerAt :: Context -> V2 -> IO (Maybe WidgetId)
openSelectDropOwnerAt ctx mouse = do
  store <- getStore ctx
  count <- arenaCount (ctxNodeArena ctx)
  let go idx
        | idx >= count = pure Nothing
        | otherwise = do
            nt <- getNodeType (ctxNodeArena ctx) idx
            mOwner <- openDropdownOwner ctx idx nt
            case mOwner of
              Nothing -> go (idx + 1)
              Just wid -> do
                opts <- getOptions (ctxNodeArena ctx) idx
                (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
                let dropRect = ownerDropRect ctx nt wid store opts x y w h
                if rectContains dropRect mouse
                  then pure (Just wid)
                  else go (idx + 1)
  go 0

cacheOpenSelectDrop :: Context -> IO ()
cacheOpenSelectDrop ctx = do
  store <- getStore ctx
  focus <- readIORef (ctxFocusId ctx)
  if not (anySelectOpen store) && hashWidgetId focus == 0
    then setOpenSelectDrop ctx Nothing
    else do
      count <- arenaCount (ctxNodeArena ctx)
      m <- go 0 count store
      setOpenSelectDrop ctx m
  where
    go idx n st
      | idx >= n = pure Nothing
      | otherwise = do
          nt <- getNodeType (ctxNodeArena ctx) idx
          mOwner <- openDropdownOwner ctx idx nt
          case mOwner of
            Nothing -> go (idx + 1) n st
            Just wid -> do
              opts <- getOptions (ctxNodeArena ctx) idx
              (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
              pure (Just (wid, ownerDropRect ctx nt wid st opts x y w h))

markSelectDropPress :: Context -> Input -> IO ()
markSelectDropPress ctx inp =
  when (inputMouseDown inp) $ do
    store <- getStore ctx
    when (anySelectOpen store) $ do
      let mouse = inputMousePos inp
      count <- arenaCount (ctxNodeArena ctx)
      hit <- openSelectHit ctx count mouse
      when hit $ setSelectDropPress ctx True

closeSelectOnOutsideClick :: Context -> Input -> IO ()
closeSelectOnOutsideClick ctx inp =
  when (inputMousePressed inp || inputMouseReleased inp) $ do
    store <- getStore ctx
    when (anySelectOpen store) $ do
      let mouse = inputMousePos inp
      count <- arenaCount (ctxNodeArena ctx)
      hit <- openSelectHit ctx count mouse
      unless hit $
        setStore ctx (closeSelects store)

finalizeSelectKeyboard :: Context -> Input -> IO ()
finalizeSelectKeyboard ctx inp = do
  let keys = inputKeys inp
      (wantNext, wantPrev, wantEsc, wantEnter) =
        foldInputKeys
          ( \(n, p, e, r) k ->
              ( n || k == KeyDown || k == KeyRight
              , p || k == KeyUp || k == KeyLeft
              , e || k == KeyEscape
              , r || k == KeyEnter
              )
          )
          (False, False, False, False)
          keys
      wantStep = wantNext || wantPrev
  when (wantStep || wantEsc || wantEnter) $ do
    focus <- readIORef (ctxFocusId ctx)
    store <- getStore ctx
    mTarget <- pickSelectKeyboardTarget ctx focus store wantStep
    case mTarget of
      Nothing -> pure ()
      Just (wid, open) -> do
        allow <- widgetOverlayAllowed ctx wid
        when allow $
          case () of
            _ | wantEsc || wantEnter ->
                when open $ do
                  setStore ctx (setSelectOpen store (intKey wid) False)
                  when wantEsc $ markEscapeConsumed ctx
                  markDirty ctx
            _ | wantStep -> do
                mIdx <- findNodeByWidgetId ctx wid
                case mIdx of
                  Nothing -> pure ()
                  Just idx -> do
                    opts <- getOptions (ctxNodeArena ctx) idx
                    let n = length opts
                    if n <= 0
                      then pure ()
                      else do
                        let key = intKey wid
                            cur = IM.findWithDefault 0 key (storeInt store)
                            delta = if wantNext then 1 else -1
                            next = max 0 (min (n - 1) (cur + delta))
                        when (next /= cur) $ do
                          setStore ctx (store {storeInt = IM.insert key next (storeInt store)})
                          markDirty ctx
            _ -> pure ()

pickSelectKeyboardTarget ::
  Context -> WidgetId -> WidgetStore -> Bool -> IO (Maybe (WidgetId, Bool))
pickSelectKeyboardTarget ctx focus store wantStep = do
  if wantStep
    then do
      mFocus <- selectWidgetIfAny ctx focus
      case mFocus of
        Just wid -> do
          let open = isSelectOpen store (intKey wid)
          pure (Just (wid, open))
        Nothing -> do
          mOpen <- findOpenSelectWidget ctx
          case mOpen of
            Nothing -> pure Nothing
            Just w -> pure (Just (w, True))
    else do
      mOpen <- findOpenSelectWidget ctx
      case mOpen of
        Nothing -> pure Nothing
        Just w -> pure (Just (w, True))

selectWidgetIfAny :: Context -> WidgetId -> IO (Maybe WidgetId)
selectWidgetIfAny ctx wid
  | hashWidgetId wid == 0 = pure Nothing
  | otherwise = do
      mIdx <- findNodeByWidgetId ctx wid
      case mIdx of
        Nothing -> pure Nothing
        Just idx -> do
          nt <- getNodeType (ctxNodeArena ctx) idx
          if nt == NodeSelect then pure (Just wid) else pure Nothing

findOpenSelectWidget :: Context -> IO (Maybe WidgetId)
findOpenSelectWidget ctx = do
  store <- getStore ctx
  count <- arenaCount (ctxNodeArena ctx)
  let go idx
        | idx >= count = pure Nothing
        | otherwise = do
            nt <- getNodeType (ctxNodeArena ctx) idx
            if nt /= NodeSelect
              then go (idx + 1)
              else do
                wid <- getWidgetId (ctxNodeArena ctx) idx
                if isSelectOpen store (intKey wid)
                  then pure (Just wid)
                  else go (idx + 1)
  go 0

finalizeSelectPick :: Context -> Input -> IO ()
finalizeSelectPick ctx inp =
  when (inputMousePressed inp || inputMouseReleased inp) $ do
    let mouse = inputMousePos inp
    count <- arenaCount (ctxNodeArena ctx)
    let go idx
          | idx >= count = pure ()
          | otherwise = do
              nt <- getNodeType (ctxNodeArena ctx) idx
              mOwner <- openDropdownOwner ctx idx nt
              case mOwner of
                Nothing -> go (idx + 1)
                Just wid -> do
                  allow <- widgetOverlayAllowed ctx wid
                  if not allow
                    then go (idx + 1)
                    else do
                      opts <- getOptions (ctxNodeArena ctx) idx
                      (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
                      st <- getStore ctx
                      let dropRect = ownerDropRect ctx nt wid st opts x y w h
                          itemH = selectItemH (ctxHostProfile ctx) h
                      when (rectContains dropRect mouse) $ do
                        let key = intKey wid
                        case nt of
                          NodeTextInput -> do
                            -- Combo: pick on press only, never from the
                            -- scrollbar lanes, so finishing a thumb drag
                            -- cannot commit a row. Picking commits the
                            -- option text into the field and defocuses it:
                            -- the combo's dropdown is visible exactly while
                            -- focused, so the menu disappears with the pick.
                            let cN = IM.findWithDefault (length opts) (slotKey slotComboCount key) (storeInt st)
                                cWin = IM.findWithDefault 0 (slotKey slotComboScroll key) (storeInt st)
                                cCW = IM.findWithDefault 0 (slotKey slotComboContentW key) (storeFloat st)
                                cX = IM.findWithDefault 0 (slotKey slotComboScrollX key) (storeFloat st)
                                (_, vSb, hSb, _) = comboScrollGeom dropRect cN (length opts) cWin cX cCW
                                onLane =
                                  maybe False (\(t, _) -> rectContains t mouse) vSb
                                    || maybe False (\(t, _) -> rectContains t mouse) hSb
                            when (inputMousePressed inp && not onLane) $
                              case comboDropPickIndex dropRect itemH (length opts) (v2Y mouse) of
                                Nothing -> pure ()
                                Just pickedI -> do
                                  let txt = case drop pickedI opts of
                                        (o : _) -> o
                                        _ -> ""
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
                          _ ->
                            case selectDropPickIndex dropRect itemH (length opts) (v2Y mouse) of
                              Nothing -> pure ()
                              Just pickedI -> do
                                setStore
                                  ctx
                                  ( setSelectOpen
                                      (st {storeInt = IM.insert key pickedI (storeInt st)})
                                      key
                                      False
                                  )
                                writeIORef (ctxFocusId ctx) wid
                                markDirty ctx
                      go (idx + 1)
    go 0

openSelectHit :: Context -> Int -> V2 -> IO Bool
openSelectHit ctx count mouse = do
  store <- getStore ctx
  let go idx
        | idx >= count = pure False
        | otherwise = do
            nt <- getNodeType (ctxNodeArena ctx) idx
            mOwner <- openDropdownOwner ctx idx nt
            case mOwner of
              Nothing -> go (idx + 1)
              Just wid -> do
                (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
                opts <- getOptions (ctxNodeArena ctx) idx
                let btnRect = Rect x y w h
                    dropRect = ownerDropRect ctx nt wid store opts x y w h
                if rectContains btnRect mouse || rectContains dropRect mouse
                  then pure True
                  else go (idx + 1)
  go 0

findSelectUnderMouse :: Context -> V2 -> IO (Maybe WidgetId)
findSelectUnderMouse ctx mouse = do
  store <- getStore ctx
  mIdx <-
    findNodeRevM (ctxNodeArena ctx) $ \idx -> do
      nt <- getNodeType (ctxNodeArena ctx) idx
      mOwner <- openDropdownOwner ctx idx nt
      case mOwner of
        Nothing -> pure False
        Just wid -> do
          allow <- widgetOverlayAllowed ctx wid
          if not allow
            then pure False
            else do
              (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
              opts <- getOptions (ctxNodeArena ctx) idx
              let btnRect = Rect x y w h
                  dropRect = ownerDropRect ctx nt wid store opts x y w h
              pure (rectContains btnRect mouse || rectContains dropRect mouse)
  case mIdx of
    Nothing -> pure Nothing
    Just idx -> Just <$> getWidgetId (ctxNodeArena ctx) idx

selectItemH :: HostProfile -> Float -> Float
selectItemH host rh = if isCellHost host then max 1 rh else 28

selectDropOuterPad :: HostProfile -> Float
selectDropOuterPad host = if isCellHost host then 0 else textInputMenuOuterPad

selectDropBg :: Style -> Color
selectDropBg st = styleBg st

selectDropActiveBg :: Style -> Color
selectDropActiveBg st = styleActiveBg st

selectDropHoverBg :: Style -> Color
selectDropHoverBg st = styleHoverBg st

-- | Vertical gap/margin between the select widget and its dropdown menu.
selectDropGap :: HostProfile -> Float
selectDropGap host = if isCellHost host then 0 else 4

selectDropRect :: HostProfile -> FontMetrics -> Float -> Float -> Float -> Float -> Int -> Rect
selectDropRect host _fm x y w h nOpts =
  let itemH = selectItemH host h
      pad = selectDropOuterPad host
      gap = selectDropGap host
   in Rect x (y + h + gap) w (itemH * fromIntegral nOpts + 2 * pad)

selectDropItemY :: HostProfile -> FontMetrics -> Rect -> Float -> Int -> Float
selectDropItemY host _fm dropRect itemH i =
  rectY dropRect + selectDropOuterPad host + itemH * fromIntegral i

selectDropPickIndex :: Rect -> Float -> Int -> Float -> Maybe Int
selectDropPickIndex dropRect itemH nOpts mouseY =
  let innerH = itemH * fromIntegral nOpts
      pad = max 0 ((rectH dropRect - innerH) / 2)
      rel = mouseY - rectY dropRect - pad
   in if rel < 0 || rel >= innerH
        then Nothing
        else
          Just (max 0 (min (nOpts - 1) (floor (rel / max itemH 1))))

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
comboScrollGeom dropRect n vis win xOff contentW =
  let
    vScroll = n > vis && vis > 0
    vLaneW = if vScroll then comboSbW else 0
    usableW = max 0 (rectW dropRect - vLaneW)
    hScroll = contentW > usableW && contentW > 0
    hLaneH = if hScroll then comboSbW else 0
    -- Rows fill the drop rect from the top, stopping short of the lanes.
    inner =
      Rect
        (rectX dropRect)
        (rectY dropRect)
        (max 0 (rectW dropRect - vLaneW))
        (max 0 (rectH dropRect - hLaneH))
    -- Lanes sit flush against the dropdown border and share the corner.
    vTrack =
      Rect
        (rectX dropRect + rectW dropRect - comboSbW)
        (rectY dropRect)
        comboSbW
        (max 0 (rectH dropRect - hLaneH))
    hTrack =
      Rect
        (rectX dropRect)
        (rectY dropRect + rectH dropRect - comboSbW)
        (max 0 (rectW dropRect - vLaneW))
        comboSbW
    vSb =
      if vScroll
        then
          let trackH = max 1 (rectH vTrack)
              thumbH = max (min comboSbMinThumb trackH) (min trackH (trackH * fromIntegral vis / fromIntegral n))
              maxWin = max 1 (n - vis)
              ty = rectY vTrack + (trackH - thumbH) * fromIntegral (max 0 (min maxWin win)) / fromIntegral maxWin
           in Just (vTrack, Rect (rectX vTrack + 2) ty (comboSbW - 4) thumbH)
        else Nothing
    hSb =
      if hScroll
        then
          let trackW = max 1 (rectW hTrack)
              thumbW = max (min comboSbMinThumb trackW) (min trackW (trackW * usableW / contentW))
              maxOff = max 1 (contentW - usableW)
              tx = rectX hTrack + (trackW - thumbW) * (max 0 (min maxOff xOff)) / maxOff
           in Just (hTrack, Rect tx (rectY hTrack + 2) thumbW (comboSbW - 4))
        else Nothing
   in (inner, vSb, hSb, usableW)

-- | Combo dropdown rect: like 'selectDropRect', but with no outer margin —
-- rows start flush at the top — and the height reserves a flush bottom
-- scrollbar lane when the widest row overflows, so the horizontal bar never
-- covers the bottommost row. Must agree with 'comboScrollGeom' on when lanes
-- appear (same inputs, same formulas).
comboDropRect ::
  HostProfile -> FontMetrics -> Float -> Float -> Float -> Float -> Int -> Int -> Float -> Rect
comboDropRect host _fm x y w h nRows nTotal contentW =
  let itemH = selectItemH host h
      gap = selectDropGap host
      gui = not (isCellHost host)
      vScroll = gui && nTotal > nRows
      vLaneW = if vScroll then comboSbW else 0
      usableW = max 0 (w - vLaneW)
      hScroll = gui && contentW > usableW && contentW > 0
   in Rect x (y + h + gap) w (fromIntegral nRows * itemH + (if hScroll then comboSbW else 0))

-- | Row index at @mouseY@ for a combo dropdown, whose rows start flush at the
-- drop rect's top (unlike 'selectDropPickIndex', which centers them).
comboDropPickIndex :: Rect -> Float -> Int -> Float -> Maybe Int
comboDropPickIndex dropRect itemH nOpts mouseY =
  let rel = mouseY - rectY dropRect
   in if rel < 0 || rel >= itemH * fromIntegral nOpts
        then Nothing
        else Just (max 0 (min (nOpts - 1) (floor (rel / max itemH 1))))

-- | Dropdown rect for an open dropdown owner: 'selectDropRect' for selects,
-- 'comboDropRect' for combos.
ownerDropRect :: Context -> NodeType -> WidgetId -> WidgetStore -> [T.Text] -> Float -> Float -> Float -> Float -> Rect
ownerDropRect ctx nt wid store opts x y w h = case nt of
  NodeTextInput ->
    let key = intKey wid
        cN = IM.findWithDefault (length opts) (slotKey slotComboCount key) (storeInt store)
        cCW = IM.findWithDefault 0 (slotKey slotComboContentW key) (storeFloat store)
     in comboDropRect (ctxHostProfile ctx) (ctxFontMetrics ctx) x y w h (length opts) cN cCW
  _ -> selectDropRect (ctxHostProfile ctx) (ctxFontMetrics ctx) x y w h (length opts)

terminalDropRow :: Int -> Int -> Int -> T.Text -> Color -> Color -> Rect -> (Rect, T.Text, Color, Color, Rect)
terminalDropRow x y w txt fg bg clip =
  (Rect (fromIntegral x) (fromIntegral y) (fromIntegral w) 1, txt, fg, bg, clip)

-- Title-bar rule and other column separators: glyphs, not a filled hairline.
terminalSelectDropdownSpans ::
  Int ->
  Int ->
  Int ->
  [T.Text] ->
  Int ->
  Maybe Int ->
  Color ->
  Color ->
  Color ->
  Color ->
  Rect ->
  [(Rect, T.Text, Color, Color, Rect)]
terminalSelectDropdownSpans rx ry wi opts picked hoverIdx fg dropBg dropActiveBg dropHoverBg clip =
  let innerW = max 0 (wi - 1)
      itemRow opt = T.singleton ' ' <> padDropText innerW opt
      rowBg i =
        if Just i == hoverIdx
          then dropHoverBg
          else
            if i == picked
              then dropActiveBg
              else dropBg
   in [ terminalDropRow rx (ry + i) wi rowText fg (rowBg i) clip
      | (i, opt) <- zip [0 ..] opts
      , let rowText = if T.null opt then T.replicate wi (T.singleton ' ') else itemRow opt
      ]

drawSelectOverlays :: Context -> Input -> IO ()
drawSelectOverlays ctx inp = do
  theme <- readIORef (ctxTheme ctx)
  let terminal = isCellHost (ctxHostProfile ctx)
  count <- arenaCount (ctxNodeArena ctx)
  when (not terminal) $ do
    let go idx
          | idx >= count = pure ()
          | otherwise = do
              nt <- getNodeType (ctxNodeArena ctx) idx
              mOwner <- openDropdownOwner ctx idx nt
              case mOwner of
                Nothing -> go (idx + 1)
                Just wid -> do
                  store <- getStore ctx
                  allow <- widgetOverlayAllowed ctx wid
                  if allow
                    then do
                      opts <- getOptions (ctxNodeArena ctx) idx
                      (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
                      drawDropdownMenu ctx inp theme nt wid store opts x y w h
                    else pure ()
                  go (idx + 1)
    go 0

-- | Paint one owner's dropdown menu (select or combo). Selects keep the
-- historical full-width rows; the combo list has no outer margin (rows start
-- flush at the drop rect's top), clips to the inner area (so x-shifted text
-- and row fills stop at the scrollbar lanes) and gets vertical / horizontal
-- scrollbars when the filtered rows or the widest row overflow the window.
drawDropdownMenu ::
  Context -> Input -> Theme -> NodeType -> WidgetId -> WidgetStore -> [T.Text] -> Float -> Float -> Float -> Float -> IO ()
drawDropdownMenu ctx inp theme nt wid store opts x y w h = do
  let da = ctxDrawArena ctx
      fm = ctxFontMetrics ctx
      host = ctxHostProfile ctx
      mouse = inputMousePos inp
      key = intKey wid
      isCombo = nt == NodeTextInput
      comboHi = IM.findWithDefault (-1) (slotKey slotComboHighlight key) (storeInt store)
      comboWin = IM.findWithDefault 0 (slotKey slotComboScroll key) (storeInt store)
      comboN = IM.findWithDefault (length opts) (slotKey slotComboCount key) (storeInt store)
      comboCW = IM.findWithDefault 0 (slotKey slotComboContentW key) (storeFloat store)
      comboX = IM.findWithDefault 0 (slotKey slotComboScrollX key) (storeFloat store)
      -- Combo keyboard highlight, window-relative; -1 (nothing highlighted)
      -- must stay unhighlighted.
      picked = case nt of
        NodeTextInput -> comboHi - comboWin
        _ -> IM.findWithDefault 0 key (storeInt store)
      itemH = selectItemH host h
      dropRect = ownerDropRect ctx nt wid store opts x y w h
      (inner, vSb, hSb, _) =
        if isCombo
          then comboScrollGeom dropRect comboN (length opts) comboWin comboX comboCW
          else (dropRect, Nothing, Nothing, rectW dropRect)
      dropStyle = overlayMenuStyle theme
      r = styleCornerRadius dropStyle
  pushMenuShadow da dropRect r
  fillStyledRect da False dropStyle dropRect
  strokeStyledRect da False dropStyle (rectX dropRect) (rectY dropRect) (rectW dropRect) (rectH dropRect)
  let (ix, _) = widgetContentInset host fm
      -- Combo rows sit flush at the drop rect's top edge (no outer margin);
      -- select rows keep their padded layout.
      rowY i = if isCombo then rectY dropRect + itemH * fromIntegral i else selectDropItemY host fm dropRect itemH i
      paintRows =
        forM_ (zip ([0 ..] :: [Int]) opts) $ \(i, opt) -> do
          let iy = rowY i
              itemRect = Rect (rectX dropRect) iy (rectW dropRect) itemH
              hovered = rectContains itemRect mouse
          when (hovered || i == picked) $ do
            let bg = if hovered then styleHoverBg dropStyle else styleActiveBg dropStyle
            pushRect da itemRect bg
            when hovered $ do
              let accent = themeAccent theme
                  barRect = Rect (rectX itemRect) (rectY itemRect + 3) 2 (rectH itemRect - 6)
              pushRoundedRect da barRect 1 accent
          unless (T.null opt) $ do
            (_tw, th) <- ctxMeasureText ctx opt
            let tx0 = rectX dropRect + textInputMenuItemPadX + ix
                tx = if isCombo then tx0 - comboX else tx0
                ty = centeredTextY host fm iy itemH th
                itemFg = if i == picked then themeAccent theme else styleFg dropStyle
            pushText da fm tx ty opt itemFg
  if isCombo
    then do
      withClip da inner paintRows
      let base = themeInput theme
          trackCol = scrollBarTrackColor base theme False
          thumbCol = scrollBarThumbColor base theme False
          drawBar (track, thumb) = do
            pushRect da track trackCol
            pushRoundedRect da thumb 3 thumbCol
      mapM_ drawBar vSb
      mapM_ drawBar hSb
    else paintRows

collectSelectDropdownSpans :: Context -> Input -> IO [(Rect, T.Text, Color, Color, Rect)]
collectSelectDropdownSpans ctx inp = do
  theme <- readIORef (ctxTheme ctx)
  let fm = ctxFontMetrics ctx
      mouse = inputMousePos inp
  count <- arenaCount (ctxNodeArena ctx)
  let go idx
        | idx >= count = pure []
        | otherwise = do
            nt <- getNodeType (ctxNodeArena ctx) idx
            mOwner <- openDropdownOwner ctx idx nt
            case mOwner of
              Nothing -> go (idx + 1)
              Just wid -> do
                store <- getStore ctx
                allow <- widgetOverlayAllowed ctx wid
                if not allow
                  then go (idx + 1)
                  else do
                    opts <- getOptions (ctxNodeArena ctx) idx
                    (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
                    let key = intKey wid
                        itemH = selectItemH (ctxHostProfile ctx) h
                        dropRect = ownerDropRect ctx nt wid store opts x y w h
                        comboHi =
                          IM.findWithDefault (-1) (slotKey slotComboHighlight key) (storeInt store)
                        comboWin = IM.findWithDefault 0 (slotKey slotComboScroll key) (storeInt store)
                        comboX = IM.findWithDefault 0 (slotKey slotComboScrollX key) (storeFloat store)
                        picked = case nt of
                          NodeTextInput -> comboHi - comboWin
                          _ -> IM.findWithDefault 0 key (storeInt store)
                        dropStyle = overlayMenuStyle theme
                        fg = styleFg dropStyle
                    if isCellHost (ctxHostProfile ctx)
                      then do
                        let wi = max 1 (round w)
                            rx = round (rectX dropRect)
                            ry = round (rectY dropRect)
                            dropBg = selectDropBg dropStyle
                            dropActiveBg = selectDropActiveBg dropStyle
                            dropHoverBg = selectDropHoverBg dropStyle
                            hoverIdx = selectDropPickIndex dropRect itemH (length opts) (v2Y mouse)
                        rest <- go (idx + 1)
                        pure
                          ( terminalSelectDropdownSpans rx ry wi opts picked hoverIdx fg dropBg dropActiveBg dropHoverBg dropRect
                              ++ rest
                          )
                      else do
                        let (ix, _) = widgetContentInset (ctxHostProfile ctx) fm
                            dropBg = styleBg dropStyle
                            -- Combo rows sit flush at the drop rect's top
                            -- edge (no outer margin); select rows keep
                            -- their padded layout.
                            rowY i = case nt of
                              NodeTextInput -> rectY dropRect + itemH * fromIntegral i
                              _ -> selectDropItemY (ctxHostProfile ctx) fm dropRect itemH i
                        itemSpans <-
                          forM (zip ([0 ..] :: [Int]) opts) $ \(i, opt) ->
                            if T.null opt
                              then pure []
                              else do
                                (tw, th) <- ctxMeasureText ctx opt
                                let itemY = rowY i
                                    itemRect = Rect (rectX dropRect) itemY (rectW dropRect) itemH
                                    hovered = rectContains itemRect mouse
                                    rowBg
                                      | hovered = styleHoverBg dropStyle
                                      | i == picked = styleActiveBg dropStyle
                                      | otherwise = dropBg
                                    ty = centeredTextY (ctxHostProfile ctx) fm itemY itemH th
                                    tx0 = rectX dropRect + textInputMenuItemPadX + ix
                                    tx = case nt of
                                      NodeTextInput -> tx0 - comboX
                                      _ -> tx0
                                pure [(Rect tx ty tw th, opt, fg, rowBg, dropRect)]
                        rest <- go (idx + 1)
                        pure (concat itemSpans ++ rest)
  go 0


selectTextClip :: HostProfile -> Float -> Float -> Float -> Float -> FontMetrics -> Rect
selectTextClip host x y w h fm =
  let (ix, _) = widgetContentInset host fm
   in Rect (x + ix) y (max 0 (w - ix - selectChevronReserve)) (max 0 h)

tagSelectClippedSpans ::
  HostProfile -> Rect -> Float -> Float -> Float -> Float -> FontMetrics -> [(Rect, T.Text, Color, Color)] -> [(Rect, T.Text, Color, Color, Rect)]
tagSelectClippedSpans host parentClip x y w h fm spans =
  let textClip = padTextClipRect (selectTextClip host x y w h fm)
   in case rectIntersect parentClip textClip of
        Nothing -> []
        Just clip -> map (\(rect, txt, fg, bg) -> (rect, txt, fg, bg, clip)) spans

