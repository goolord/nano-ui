{-# LANGUAGE DataKinds #-}

module NanoUI.Frame.Select
  ( selectDropRect
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
  , intKey
  , isSelectOpen
  , markDirty
  , markEscapeConsumed
  , setSelectOpen
  , setStore
  )
import NanoUI.Draw (pushRect, pushRoundedRect, pushText)
import NanoUI.Font (FontMetrics, centeredTextY, hasMonoFontMarker, stripMonoFontMarker, widgetContentInset)
import NanoUI.Types (HostProfile, isCellHost)
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Input (Input (..), Key (..), foldInputKeys, inputKeys, inputMouseDown, inputMousePos, inputMousePressed)
import NanoUI.Layout.Arena (NodeType (NodeSelect), arenaCount, findNodeRevM, getNodeType, getRect, getText, getWidgetId)
import NanoUI.Style (Style (..), Theme (..), themeAccent)
import NanoUI.Types (Color (..), Rect (..), V2 (..), rectContains, rectH, rectW, rectX, rectY, v2Y)
import NanoUI.WidgetText (selectOptions)
import NanoUI.Frame.Chrome
  ( fillStyledRect
  , pushMenuShadow
  , strokeStyledRect
  , overlayMenuStyle
  , padDropText
  , textInputMenuItemPadX
  , textInputMenuOuterPad
  )
import NanoUI.Frame.Focus (unlessHit)
import NanoUI.Frame.Hit (findNodeByWidgetId, widgetOverlayAllowed)

overlayMenuOwnerAt :: Context -> V2 -> IO (Maybe WidgetId)
overlayMenuOwnerAt ctx mouse = do
  mMenu <- readIORef (ctxTextInputMenu ctx)
  case mMenu of
    Just m | rectContains (textInputMenuRect m) mouse ->
      pure (Just (textInputMenuWidget m))
    _ -> openSelectDropOwnerAt ctx mouse

openSelectDropOwnerAt :: Context -> V2 -> IO (Maybe WidgetId)
openSelectDropOwnerAt ctx mouse = do
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
                let key = intKey wid
                if not (isSelectOpen store key)
                  then go (idx + 1)
                  else do
                    txt <- getText (ctxNodeArena ctx) idx
                    (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
                    let (_, opts) = selectOptions txt
                        dropRect =
                          selectDropRect (ctxHostProfile ctx) (ctxFontMetrics ctx) x y w h (length opts)
                    if rectContains dropRect mouse
                      then pure (Just wid)
                      else go (idx + 1)
  go 0

cacheOpenSelectDrop :: Context -> IO ()
cacheOpenSelectDrop ctx = do
  store <- getStore ctx
  if not (anySelectOpen store)
    then writeIORef (ctxOpenSelectDrop ctx) Nothing
    else do
      count <- arenaCount (ctxNodeArena ctx)
      m <- go 0 count store
      writeIORef (ctxOpenSelectDrop ctx) m
  where
    go idx n st
      | idx >= n = pure Nothing
      | otherwise = do
          nt <- getNodeType (ctxNodeArena ctx) idx
          if nt /= NodeSelect
            then go (idx + 1) n st
            else do
              wid <- getWidgetId (ctxNodeArena ctx) idx
              let key = intKey wid
              if not (isSelectOpen st key)
                then go (idx + 1) n st
                else do
                  txt <- getText (ctxNodeArena ctx) idx
                  (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
                  let (_, opts) = selectOptions txt
                      dropRect =
                        selectDropRect (ctxHostProfile ctx) (ctxFontMetrics ctx) x y w h (length opts)
                  pure (Just (wid, dropRect))

markSelectDropPress :: Context -> Input -> IO ()
markSelectDropPress ctx inp =
  when (inputMouseDown inp) $ do
    store <- getStore ctx
    when (anySelectOpen store) $ do
      let mouse = inputMousePos inp
      count <- arenaCount (ctxNodeArena ctx)
      hit <- openSelectHit ctx count mouse store
      when hit $ writeIORef (ctxSelectDropPress ctx) True

closeSelectOnOutsideClick :: Context -> Input -> IO ()
closeSelectOnOutsideClick ctx inp =
  when (inputMousePressed inp) $ do
    store <- getStore ctx
    when (anySelectOpen store) $ do
      let mouse = inputMousePos inp
      count <- arenaCount (ctxNodeArena ctx)
      hit <- openSelectHit ctx count mouse store
      unlessHit hit $
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
                    txt <- getText (ctxNodeArena ctx) idx
                    let (_, opts) = selectOptions txt
                        n = length opts
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
  when (inputMousePressed inp) $ do
    let mouse = inputMousePos inp
    count <- arenaCount (ctxNodeArena ctx)
    let go idx
          | idx >= count = pure ()
          | otherwise = do
              nt <- getNodeType (ctxNodeArena ctx) idx
              if nt /= NodeSelect
                then go (idx + 1)
                else do
                  wid <- getWidgetId (ctxNodeArena ctx) idx
                  store <- getStore ctx
                  let key = intKey wid
                  if not (isSelectOpen store key)
                    then go (idx + 1)
                    else do
                      allow <- widgetOverlayAllowed ctx wid
                      if not allow
                        then go (idx + 1)
                        else do
                          txt <- getText (ctxNodeArena ctx) idx
                          let (_, opts) = selectOptions txt
                          (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
                          let fm = ctxFontMetrics ctx
                              dropRect = selectDropRect (ctxHostProfile ctx) fm x y w h (length opts)
                          when (rectContains dropRect mouse) $
                            case selectDropPickIndex dropRect (selectItemH (ctxHostProfile ctx) h) (length opts) (v2Y mouse) of
                              Nothing -> pure ()
                              Just picked -> do
                                st <- getStore ctx
                                setStore
                                  ctx
                                  ( setSelectOpen
                                      (st {storeInt = IM.insert key picked (storeInt st)})
                                      key
                                      False
                                  )
                                writeIORef (ctxFocusId ctx) wid
                                markDirty ctx
                          go (idx + 1)
    go 0

openSelectHit :: Context -> Int -> V2 -> WidgetStore -> IO Bool
openSelectHit ctx count mouse store = go 0
  where
    go idx
      | idx >= count = pure False
      | otherwise = do
          nt <- getNodeType (ctxNodeArena ctx) idx
          if nt /= NodeSelect
            then go (idx + 1)
            else do
              wid <- getWidgetId (ctxNodeArena ctx) idx
              let key = intKey wid
              if not (isSelectOpen store key)
                then go (idx + 1)
                else do
                  (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
                  txt <- getText (ctxNodeArena ctx) idx
                  let fm = ctxFontMetrics ctx
                      (_, opts) = selectOptions txt
                      btnRect = Rect x y w h
                      dropRect = selectDropRect (ctxHostProfile ctx) fm x y w h (length opts)
                  if rectContains btnRect mouse || rectContains dropRect mouse
                    then pure True
                    else go (idx + 1)

findSelectUnderMouse :: Context -> V2 -> IO (Maybe WidgetId)
findSelectUnderMouse ctx mouse = do
  mIdx <-
    findNodeRevM (ctxNodeArena ctx) $ \idx -> do
      nt <- getNodeType (ctxNodeArena ctx) idx
      if nt /= NodeSelect
        then pure False
        else do
          wid <- getWidgetId (ctxNodeArena ctx) idx
          allow <- widgetOverlayAllowed ctx wid
          if not allow
            then pure False
            else do
              (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
              txt <- getText (ctxNodeArena ctx) idx
              store <- getStore ctx
              let key = intKey wid
                  open = isSelectOpen store key
                  fm = ctxFontMetrics ctx
                  (_, opts) = selectOptions txt
                  btnRect = Rect x y w h
                  dropRect = selectDropRect (ctxHostProfile ctx) fm x y w h (length opts)
              pure (rectContains btnRect mouse || (open && rectContains dropRect mouse))
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

-- The list hangs directly off the select, with no gap on any backend.
selectDropRect :: HostProfile -> FontMetrics -> Float -> Float -> Float -> Float -> Int -> Rect
selectDropRect host _fm x y w h nOpts =
  let itemH = selectItemH host h
      pad = selectDropOuterPad host
   in Rect x (y + h) w (itemH * fromIntegral nOpts + 2 * pad)

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
  let mouse = inputMousePos inp
      da = ctxDrawArena ctx
      theme = ctxTheme ctx
      fm = ctxFontMetrics ctx
      terminal = isCellHost (ctxHostProfile ctx)
  count <- arenaCount (ctxNodeArena ctx)
  when (not terminal) $ do
    let go idx
          | idx >= count = pure ()
          | otherwise = do
              nt <- getNodeType (ctxNodeArena ctx) idx
              if nt /= NodeSelect
                then go (idx + 1)
                else do
                  wid <- getWidgetId (ctxNodeArena ctx) idx
                  store <- getStore ctx
                  let key = intKey wid
                  if not (isSelectOpen store key)
                    then go (idx + 1)
                    else do
                      allow <- widgetOverlayAllowed ctx wid
                      if not allow
                        then go (idx + 1)
                        else do
                          txt <- getText (ctxNodeArena ctx) idx
                          let (_, opts) = selectOptions txt
                          (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
                          let picked = IM.findWithDefault 0 key (storeInt store)
                              itemH = selectItemH (ctxHostProfile ctx) h
                              dropRect = selectDropRect (ctxHostProfile ctx) fm x y w h (length opts)
                              dropStyle = overlayMenuStyle theme
                              r = styleCornerRadius dropStyle
                          pushMenuShadow da dropRect r
                          fillStyledRect da False dropStyle dropRect
                          strokeStyledRect
                            da
                            False
                            dropStyle
                            (rectX dropRect)
                            (rectY dropRect)
                            (rectW dropRect)
                            (rectH dropRect)
                          let (ix, _) = widgetContentInset (ctxHostProfile ctx) fm
                          forM_ (zip ([0 ..] :: [Int]) opts) $ \(i, opt) -> do
                            let iy = selectDropItemY (ctxHostProfile ctx) fm dropRect itemH i
                                itemRect = Rect (rectX dropRect) iy (rectW dropRect) itemH
                                hovered = rectContains itemRect mouse
                            when (hovered || i == picked) $ do
                              let bg =
                                    if hovered
                                      then styleHoverBg dropStyle
                                      else styleActiveBg dropStyle
                              pushRect da itemRect bg
                              when hovered $ do
                                let accent = themeAccent theme
                                    barRect = Rect (rectX itemRect) (rectY itemRect + 3) 2 (rectH itemRect - 6)
                                pushRoundedRect da barRect 1 accent
                            unless (T.null opt) $ do
                              (_tw, th) <- ctxMeasureText ctx opt
                              let tx = rectX dropRect + textInputMenuItemPadX + ix
                                  ty = centeredTextY (ctxHostProfile ctx) fm iy itemH th
                                  itemFg = if i == picked then themeAccent theme else styleFg dropStyle
                                  (fm', shown) = if hasMonoFontMarker opt
                                                   then (ctxMonoFontMetrics ctx, stripMonoFontMarker opt)
                                                   else (fm, opt)
                              pushText da fm' tx ty shown itemFg
                          go (idx + 1)
    go 0

collectSelectDropdownSpans :: Context -> Input -> IO [(Rect, T.Text, Color, Color, Rect)]
collectSelectDropdownSpans ctx inp = do
  let fm = ctxFontMetrics ctx
      theme = ctxTheme ctx
      mouse = inputMousePos inp
  count <- arenaCount (ctxNodeArena ctx)
  let go idx
        | idx >= count = pure []
        | otherwise = do
            nt <- getNodeType (ctxNodeArena ctx) idx
            if nt /= NodeSelect
              then go (idx + 1)
              else do
                wid <- getWidgetId (ctxNodeArena ctx) idx
                store <- getStore ctx
                let key = intKey wid
                if not (isSelectOpen store key)
                  then go (idx + 1)
                  else do
                    allow <- widgetOverlayAllowed ctx wid
                    if not allow
                      then go (idx + 1)
                      else do
                        txt <- getText (ctxNodeArena ctx) idx
                        let (_, opts) = selectOptions txt
                        (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
                        let itemH = selectItemH (ctxHostProfile ctx) h
                            dropRect = selectDropRect (ctxHostProfile ctx) fm x y w h (length opts)
                            picked = IM.findWithDefault 0 key (storeInt store)
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
                                hoverIdx =
                                  selectDropPickIndex dropRect itemH (length opts) (v2Y mouse)
                            rest <- go (idx + 1)
                            pure
                              ( terminalSelectDropdownSpans rx ry wi opts picked hoverIdx fg dropBg dropActiveBg dropHoverBg dropRect
                                  ++ rest
                              )
                          else do
                            let (ix, _) = widgetContentInset (ctxHostProfile ctx) fm
                                dropBg = styleBg dropStyle
                            itemSpans <-
                              forM (zip ([0 ..] :: [Int]) opts) $ \(i, opt) ->
                                if T.null opt
                                  then pure []
                                  else do
                                    (tw, th) <- ctxMeasureText ctx opt
                                    let itemY = selectDropItemY (ctxHostProfile ctx) fm dropRect itemH i
                                        itemRect = Rect (rectX dropRect) itemY (rectW dropRect) itemH
                                        hovered = rectContains itemRect mouse
                                        rowBg
                                          | hovered = styleHoverBg dropStyle
                                          | i == picked = styleActiveBg dropStyle
                                          | otherwise = dropBg
                                        ty = centeredTextY (ctxHostProfile ctx) fm itemY itemH th
                                        tx = rectX dropRect + textInputMenuItemPadX + ix
                                    pure [(Rect tx ty tw th, opt, fg, rowBg, dropRect)]
                            rest <- go (idx + 1)
                            pure (concat itemSpans ++ rest)
  go 0

