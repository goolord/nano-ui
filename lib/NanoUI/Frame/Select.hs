{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

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
  ) where


import Control.Monad (filterM, foldM, forM, forM_, unless, void, when)
import Data.Char (isAlphaNum, isSpace)
import Data.IORef (readIORef, writeIORef)
import Data.Typeable (Typeable)
import Data.List (findIndex)
import Data.Maybe (isJust)
import Data.Word (Word32)
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import NanoUI.Damage (floatingPanelRects, updatePrevRects, writeDamage)
import NanoUI.Context
  ( Context (..)
  , FrameMsg (..)
  , WidgetStore (..)
  , TextInputMenu (..)
  , TextInputDrag (..)
  , WindowResizeDrag (..)
  , WindowResizeEdge (..)
  , anyAnimating
  , decodeMessages
  , drainMessages
  , getFocusables
  , getScrollOffset
  , getStore
  , intKey
  , isDirty
  , isDisabled
  , markDirty
  , getHotId
  , getPrevRect
  , setScrollOffset
  , setStore
  , startAnimation
  , setAnimationValue
  , tickAnimations
  , getAnimationValue
  , animInProgress
  , clearTooltips
  , readTooltips
  , PendingTooltip (..)
  , ctxClipboardGet
  , clearMeasureCache
  , markEscapeConsumed
  , lookupImageUv
  , atlasTextureId
  )
import NanoUI.Draw
  ( DrawArena
  , DrawData
  , Layer (..)
  , beginLayer
  , currentLayer
  , finishDraw
  , pushLine
  , pushFilledTriangle
  , pushRect
  , pushBackdropDim
  , pushImage
  , pushRoundedRect
  , pushRoundedStroke
  , pushText
  , resetDrawArena
  , withClip
  )
import NanoUI.Font
  ( FontMetrics (..)
  , checkboxBoxSize
  , checkboxLeading
  , fmLineHeight
  , layoutLineHeight
  , hasHeadingMarker
  , hasMonoFontMarker
  , hasMutedMarker
  , labelContentInset
  , resolveLayoutPadding
  , stripWidgetMarkers
  , lineWidth
  , textDisplayWidth
  , ScrollBarSlot (..)
  , scrollBarGeomFor
  , scrollBarOuterGap
  , scrollLayoutGutter
  , sliderTrackBounds
  , widgetContentInset
  , centeredTextY
  , alignedTextBox
  , wrapTextLines
  , wrapTextLinesIO
  )
import NanoUI.Host (HostProfile, isCellHost)
import NanoUI.WidgetMarkers
  ( buttonDisplayText
  , closeButtonDisplayText
  , isCloseButtonText
  , stripButtonBrackets
  )
import NanoUI.Icons (Icons (..), checkboxMark, terminalPaintColumns)
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Input (Input (..), Key (..), Modifiers (..), inputInteracted, inputKeys, inputPointerHeld, inputMouseDown, inputMousePos, inputMousePressed, inputMouseReleased, inputMouseRightPressed, inputScroll, inputDeltaTime, inputWindowSize, modShift)
import NanoUI.Layout.Arena
  ( DirTag (..)
  , NodeIdx
  , NodeType (..)
  , SizingTag (..)
  , arenaCount
  , getAlignX
  , getDirection
  , getFirstChild
  , getHeightSizing
  , getMinMax
  , getNextSibling
  , getParent
  , getNodeType
  , getNodeValue
  , getPadding
  , getRect
  , getStyleIdx
  , getText
  , getWidthSizing
  , getWidgetId
  , isWidgetNode
  , isContainerNode
  , isFloatingNode
  , isScrollNode
  , NodeType (NodeButton, NodeCheckbox, NodeSelect, NodeSlider, NodeTextInput, NodeModal, NodeImage, NodePanel, NodeWindow, NodeContainer, NodeScrollContainer, NodeText, NodeSeparator, NodeSpacer, NodeBox)
  , resetNodeArena
  , setNodeText
  , setNodeValue
  , setRect
  )
import NanoUI.Layout.Solve (placeModals, placeWindows, positionWindowNode, scrollBarSlotOf, solveLayout)
import Effectful (Eff, IOE, runEff, type (:>))
import NanoUI.Monad (NanoUI, Ui, runUi)
import NanoUI.Widgets (applyTextInputMenuAction)
import NanoUI.WidgetText
  ( checkboxLabelText
  , sliderLabelText
  , sliderPackRange
  , sliderParseRange
  , sliderPackTerminal
  , sliderValueText
  , textInputFieldHeight
  , textInputFieldText
  , textInputLabelGap
  , textInputTerminalText
  , selectParseOptions
  , selectDisplayText
  , selectChevronReserve
  , selectChevronCenterX
  )
import NanoUI.Style (Padding (..), Style (..), Theme (..), scrollBarThumbColor, scrollBarTrackColor, themeAccent, themeButton, themeFloatingWindow, themeInput, themeMuted, themeOverlayDim, themePanel, themeSeparator, themeWindow)
import NanoUI.Types (Color (..), ImageId (..), Rect (..), Size (..), V2 (..), colorRGBA, lerpColor, rectContains, rectH, rectIntersect, rectOverlapArea, rectUnion, rectW, rectX, rectY, v2X, v2Y)
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

markSelectDropPress :: Context -> Input -> IO ()
markSelectDropPress ctx inp =
  when (inputMouseDown inp) $ do
    store <- getStore ctx
    when (any id (IM.elems (storeSelectOpen store))) $ do
      let mouse = inputMousePos inp
      count <- arenaCount (ctxNodeArena ctx)
      hit <- openSelectHit ctx count mouse (storeSelectOpen store)
      when hit $ writeIORef (ctxSelectDropPress ctx) True

closeSelectOnOutsideClick :: Context -> Input -> IO ()
closeSelectOnOutsideClick ctx inp =
  when (inputMousePressed inp) $ do
    store <- getStore ctx
    when (any id (IM.elems (storeSelectOpen store))) $ do
      let mouse = inputMousePos inp
      count <- arenaCount (ctxNodeArena ctx)
      hit <- openSelectHit ctx count mouse (storeSelectOpen store)
      unlessHit hit $
        setStore ctx (store {storeSelectOpen = IM.map (const False) (storeSelectOpen store)})

finalizeSelectKeyboard :: Context -> Input -> IO ()
finalizeSelectKeyboard ctx inp = do
  let keys = inputKeys inp
      wantNext = KeyDown `elem` keys || KeyRight `elem` keys
      wantPrev = KeyUp `elem` keys || KeyLeft `elem` keys
      wantEsc = KeyEscape `elem` keys
      wantEnter = KeyEnter `elem` keys
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
                  setStore ctx (store {storeSelectOpen = IM.insert (intKey wid) False (storeSelectOpen store)})
                  when wantEsc $ markEscapeConsumed ctx
                  markDirty ctx
            _ | wantStep -> do
                mIdx <- findNodeByWidgetId ctx wid
                case mIdx of
                  Nothing -> pure ()
                  Just idx -> do
                    txt <- getText (ctxNodeArena ctx) idx
                    let (_, opts) = selectParseOptions txt
                        n = length opts
                    if n <= 0
                      then pure ()
                      else do
                        let key = intKey wid
                            cur = IM.findWithDefault 0 key (storeSelect store)
                            delta = if wantNext then 1 else -1
                            next = max 0 (min (n - 1) (cur + delta))
                        when (next /= cur) $ do
                          setStore ctx (store {storeSelect = IM.insert key next (storeSelect store)})
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
          let open = IM.findWithDefault False (intKey wid) (storeSelectOpen store)
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
                if IM.findWithDefault False (intKey wid) (storeSelectOpen store)
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
                  if not (IM.findWithDefault False key (storeSelectOpen store))
                    then go (idx + 1)
                    else do
                      allow <- widgetOverlayAllowed ctx wid
                      if not allow
                        then go (idx + 1)
                        else do
                          txt <- getText (ctxNodeArena ctx) idx
                          let (_, opts) = selectParseOptions txt
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
                                  ( st
                                      { storeSelect = IM.insert key picked (storeSelect st)
                                      , storeSelectOpen = IM.insert key False (storeSelectOpen st)
                                      }
                                  )
                                writeIORef (ctxFocusId ctx) wid
                                markDirty ctx
                          go (idx + 1)
    go 0

openSelectHit :: Context -> Int -> V2 -> IM.IntMap Bool -> IO Bool
openSelectHit ctx count mouse opens = go 0
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
              if not (IM.findWithDefault False key opens)
                then go (idx + 1)
                else do
                  (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
                  txt <- getText (ctxNodeArena ctx) idx
                  let fm = ctxFontMetrics ctx
                      (_, opts) = selectParseOptions txt
                      btnRect = Rect x y w h
                      dropRect = selectDropRect (ctxHostProfile ctx) fm x y w h (length opts)
                  if rectContains btnRect mouse || rectContains dropRect mouse
                    then pure True
                    else go (idx + 1)

findSelectUnderMouse :: Context -> Int -> V2 -> IO (Maybe WidgetId)
findSelectUnderMouse ctx count mouse = go (count - 1)
  where
    go idx
      | idx < 0 = pure Nothing
      | otherwise = do
          nt <- getNodeType (ctxNodeArena ctx) idx
          if nt /= NodeSelect
            then go (idx - 1)
            else do
              wid <- getWidgetId (ctxNodeArena ctx) idx
              allow <- widgetOverlayAllowed ctx wid
              if not allow
                then go (idx - 1)
                else do
                  (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
                  txt <- getText (ctxNodeArena ctx) idx
                  store <- getStore ctx
                  let key = intKey wid
                      open = IM.findWithDefault False key (storeSelectOpen store)
                      fm = ctxFontMetrics ctx
                      (_, opts) = selectParseOptions txt
                      btnRect = Rect x y w h
                      dropRect = selectDropRect (ctxHostProfile ctx) fm x y w h (length opts)
                  if rectContains btnRect mouse || (open && rectContains dropRect mouse)
                    then pure (Just wid)
                    else go (idx - 1)

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
                  if not (IM.findWithDefault False key (storeSelectOpen store))
                    then go (idx + 1)
                    else do
                      allow <- widgetOverlayAllowed ctx wid
                      if not allow
                        then go (idx + 1)
                        else do
                          txt <- getText (ctxNodeArena ctx) idx
                          let (_, opts) = selectParseOptions txt
                          (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
                          let picked = IM.findWithDefault 0 key (storeSelect store)
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
                          forM_ (zip ([0 ..] :: [Int]) opts) $ \(i, _opt) -> do
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
                if not (IM.findWithDefault False key (storeSelectOpen store))
                  then go (idx + 1)
                  else do
                    allow <- widgetOverlayAllowed ctx wid
                    if not allow
                      then go (idx + 1)
                      else do
                        txt <- getText (ctxNodeArena ctx) idx
                        let (_, opts) = selectParseOptions txt
                        (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
                        let itemH = selectItemH (ctxHostProfile ctx) h
                            dropRect = selectDropRect (ctxHostProfile ctx) fm x y w h (length opts)
                            picked = IM.findWithDefault 0 key (storeSelect store)
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

