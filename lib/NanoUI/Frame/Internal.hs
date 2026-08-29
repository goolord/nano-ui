{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module NanoUI.Frame.Internal
  ( findNodeByWidgetId
  , widgetNodeTypeTable
  , modalTreeOpen
  , topmostModalIdx
  , nodeInTopmostModal
  , nodeInSubtree
  , modalHitAllowed
  , overlayHitAllowed
  , topmostWindowAtMouse
  , widgetOverlayAllowed
  , filterModalFocusables
  , widgetIdInModal
  , constrainFocusToModal
  , syncWidgetLabels
  , imageIdFromText
  , clamp01
  , unlessHit
  , tabNext
  , tagClippedSpans
  , padTextClipRect
  , floatingAncestor
  , buildFloatingAncestorMap
  , displayText
  , nodeLabelPaint
  , floatingLabelPaint
  , widgetVisualStyle
  , scrollContentClip
  , scrollChromeLane
  , padContentClip
  , terminalModalOuterClip
  , UiCursorKind (..)
  , grabHoverKind
  , grabDragKind
  , textInputValue
  , textInputFocused
  , fillStyledRect
  , strokeStyledRect
  , pushMenuShadow
  , overlayWindowStyle
  , overlayModalStyle
  , overlayMenuStyle
  , strokeRect
  , textInputMenuOuterPad
  , textInputMenuItemPadX
  , textInputMenuCornerR
  , textInputMenuShadowOff
  , padDropText
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

tagClippedSpans :: Rect -> [(Rect, T.Text, Color, Color)] -> [(Rect, T.Text, Color, Color, Rect)]
tagClippedSpans clip =
  concatMap
    ( \(rect, txt, fg, bg) ->
        case rectIntersect clip (padTextClipRect rect) of
          Nothing -> []
          Just clipHere -> [(rect, txt, fg, bg, clipHere)]
    )

textClipSlop :: Float
textClipSlop = 4

padTextClipRect :: Rect -> Rect
padTextClipRect (Rect x y w h) = Rect x y (w + textClipSlop) h

nodeLabelPaint :: Theme -> T.Text -> (T.Text, Color, Color)
nodeLabelPaint theme raw = labelPaintWith (themePanel theme) theme raw

labelPaintWith :: Style -> Theme -> T.Text -> (T.Text, Color, Color)
labelPaintWith style theme raw =
  labelPaintWithBg style (styleBg style) theme raw

labelPaintWithBg :: Style -> Color -> Theme -> T.Text -> (T.Text, Color, Color)
labelPaintWithBg style bg theme raw =
  let fg
        | hasHeadingMarker raw = themeAccent theme
        | hasMutedMarker raw = themeMuted theme
        | otherwise = styleFg style
   in (stripWidgetMarkers raw, fg, bg)

floatingLabelPaint ::
  IM.IntMap (Maybe NodeType) -> Context -> NodeIdx -> Theme -> T.Text -> (T.Text, Color, Color)
floatingLabelPaint floatCache ctx idx theme raw =
  let terminal = isCellHost (ctxHostProfile ctx)
   in case IM.lookup idx floatCache of
        Just (Just NodeWindow)
          | terminal -> labelPaintWith (themeFloatingWindow theme) theme raw
        Just (Just NodeModal)
          | terminal -> labelPaintWith (themeFloatingWindow theme) theme raw
        _ -> nodeLabelPaint theme raw

floatingAncestor :: Context -> NodeIdx -> IO (Maybe NodeType)
floatingAncestor ctx idx = go idx
  where
    go i
      | i < 0 = pure Nothing
      | otherwise = do
          nt <- getNodeType (ctxNodeArena ctx) i
          if isFloatingNode nt
            then pure (Just nt)
            else do
              parent <- getParent (ctxNodeArena ctx) i
              go parent

buildFloatingAncestorMap :: Context -> IO (IM.IntMap (Maybe NodeType))
buildFloatingAncestorMap ctx = do
  count <- arenaCount (ctxNodeArena ctx)
  foldM resolve IM.empty [0 .. count - 1]
  where
    resolve :: IM.IntMap (Maybe NodeType) -> Int -> IO (IM.IntMap (Maybe NodeType))
    resolve cache idx =
      if IM.member idx cache
        then pure cache
        else do
          nt <- getNodeType (ctxNodeArena ctx) idx
          if isFloatingNode nt
            then pure (IM.insert idx (Just nt) cache)
            else do
              parent <- getParent (ctxNodeArena ctx) idx
              if parent < 0
                then pure (IM.insert idx Nothing cache)
                else do
                  cache' <- resolve cache parent
                  let ancestor = IM.findWithDefault Nothing parent cache'
                  pure (IM.insert idx ancestor cache')

displayText :: Context -> NodeType -> NodeIdx -> IO T.Text
displayText ctx nt idx = do
  txt <- getText (ctxNodeArena ctx) idx
  let terminal = isCellHost (ctxHostProfile ctx)
  if terminal
    then
      case nt of
        NodeSelect -> do
          store <- getStore ctx
          let (lbl, opts) = selectParseOptions txt
          wid <- getWidgetId (ctxNodeArena ctx) idx
          let picked = IM.findWithDefault 0 (intKey wid) (storeSelect store)
              open = IM.findWithDefault False (intKey wid) (storeSelectOpen store)
              opt =
                case drop picked opts of
                  (o : _) -> o
                  _ -> ""
              icons = ctxIcons ctx
              caret = if open then iconSelectOpen icons else iconSelectClosed icons
          pure (selectDisplayText lbl opt <> caret)
        NodeSlider -> pure (T.takeWhile (/= '\US') txt)
        -- Terminal keeps bracket text for TUI affordance; SDL strips via buttonDisplayText.
        NodeButton ->
          if isCloseButtonText txt
            then pure (closeButtonDisplayText txt)
            else pure txt
        NodeTextInput -> do
          value <- textInputValue ctx idx
          focused <- textInputFocused ctx idx
          wid <- getWidgetId (ctxNodeArena ctx) idx
          store <- getStore ctx
          let cursor = IM.findWithDefault (length value) (intKey wid) (storeCursor store)
          pure (textInputTerminalText txt value cursor focused)
        _ -> pure txt
    else
      case nt of
        NodeCheckbox -> pure (checkboxLabelText txt)
        NodeTextInput -> do
          value <- textInputValue ctx idx
          focused <- textInputFocused ctx idx
          pure (textInputFieldText txt value focused)
        NodeSlider -> pure (sliderLabelText txt)
        NodeSelect -> do
          store <- getStore ctx
          let (lbl, opts) = selectParseOptions txt
          wid <- getWidgetId (ctxNodeArena ctx) idx
          let picked = IM.findWithDefault 0 (intKey wid) (storeSelect store)
              opt =
                case drop picked opts of
                  (o : _) -> o
                  _ -> ""
          pure (selectDisplayText lbl opt)
        NodeButton -> pure (buttonDisplayText txt)
        _ -> pure (stripButtonBrackets txt)

textInputValue :: Context -> NodeIdx -> IO String
textInputValue ctx idx = do
  wid <- getWidgetId (ctxNodeArena ctx) idx
  store <- getStore ctx
  let key = intKey wid
  pure (IM.findWithDefault "" key (storeText store))

textInputFocused :: Context -> NodeIdx -> IO Bool
textInputFocused ctx idx = do
  wid <- getWidgetId (ctxNodeArena ctx) idx
  focus <- readIORef (ctxFocusId ctx)
  pure (focus == wid)

data UiCursorKind
  = UiCursorDefault
  | UiCursorPointer
  | UiCursorText
  | UiCursorGrab
  | UiCursorGrabbing
  | UiCursorNsResize
  | UiCursorEwResize
  | UiCursorNwseResize
  | UiCursorNeswResize
  deriving (Eq, Show)

grabHoverKind :: Bool -> Input -> UiCursorKind
grabHoverKind onTarget inp = grabDragKind onTarget False inp

grabDragKind :: Bool -> Bool -> Input -> UiCursorKind
grabDragKind onTarget dragging inp
  | dragging = UiCursorGrabbing
  | onTarget, inputMouseDown inp = UiCursorGrabbing
  | onTarget = UiCursorGrab
  | otherwise = UiCursorDefault

widgetNodeTypeTable :: Context -> IO (IM.IntMap NodeType)
widgetNodeTypeTable ctx = do
  cached <- readIORef (ctxWidgetNodeTypes ctx)
  case cached of
    Just table -> pure table
    Nothing -> do
      count <- arenaCount (ctxNodeArena ctx)
      table <-
        if count <= 0
          then pure IM.empty
          else do
            let go idx acc
                  | idx >= count = pure acc
                  | otherwise = do
                      nt <- getNodeType (ctxNodeArena ctx) idx
                      acc' <-
                        if isWidgetNode nt
                          then do
                            wid <- getWidgetId (ctxNodeArena ctx) idx
                            pure (IM.insert (intKey wid) nt acc)
                          else pure acc
                      go (idx + 1) acc'
            go 0 IM.empty
      writeIORef (ctxWidgetNodeTypes ctx) (Just table)
      pure table

closeButtonStyle :: Theme -> Bool -> Float -> Style
closeButtonStyle theme isHot animT =
  let btn = themeButton theme
      panel = themePanel theme
      muted = lerpColor (styleFg btn) (styleBg panel) 0.42
      hot = styleFg btn
      fg
        | isHot = lerpColor muted hot (if animT > 0 then animT else 1)
        | otherwise = lerpColor muted hot animT
   in btn
        { styleBg = colorRGBA 0 0 0 0
        , styleHoverBg = colorRGBA 0 0 0 0
        , styleActiveBg = colorRGBA 0 0 0 0
        , styleBorderWidth = 0
        , styleFg = fg
        }

widgetVisualStyle :: Context -> NodeType -> NodeIdx -> IO Style
widgetVisualStyle ctx nt idx = do
  wid <- getWidgetId (ctxNodeArena ctx) idx
  hot <- readIORef (ctxHotId ctx)
  active <- readIORef (ctxActiveId ctx)
  focus <- readIORef (ctxFocusId ctx)
  animT <- getAnimationValue ctx wid
  mFloat <- floatingAncestor ctx idx
  storedText <-
    if nt == NodeButton
      then getText (ctxNodeArena ctx) idx
      else pure T.empty
  let isClose = nt == NodeButton && isCloseButtonText storedText
  let theme = ctxTheme ctx
      terminal = isCellHost (ctxHostProfile ctx)
      isFocus = focus == wid
      widKey = hashWidgetId wid
      isHot = wid == hot
      base =
        case nt of
          NodeTextInput -> themeInput theme
          NodeSelect ->
            let sel = themeButton theme
             in if isFocus
                  then sel {styleBorder = themeAccent theme}
                  else sel
          NodeSlider ->
            if terminal
              then themeInput theme
              else
                (themeInput theme)
                  { styleBg = colorRGBA 0 0 0 0
                  , styleHoverBg = colorRGBA 0 0 0 0
                  , styleActiveBg = colorRGBA 0 0 0 0
                  , styleBorderWidth = 0
                  }
          NodeCheckbox ->
            (themeButton theme)
              { styleBg = colorRGBA 0 0 0 0
              , styleHoverBg = colorRGBA 0 0 0 0
              , styleActiveBg = colorRGBA 0 0 0 0
              , styleBorderWidth = 0
              }
          NodeButton
            | isClose -> closeButtonStyle theme isHot animT
            | Just NodeWindow <- mFloat, terminal -> themeFloatingWindow theme
            | Just NodeModal <- mFloat, terminal -> themeFloatingWindow theme
          _ -> themeButton theme
      widgetBase =
        case mFloat of
          Just NodeModal
            | terminal -> base
            | nt == NodeCheckbox || nt == NodeSlider -> overlayModalStyle theme
            | otherwise -> base
          _ -> base
      bg
        | terminal, widKey == hashWidgetId active = styleActiveBg widgetBase
        | terminal, isHot = styleHoverBg widgetBase
        | terminal = styleBg widgetBase
        | nt == NodeTextInput, isFocus = styleActiveBg widgetBase
        | widKey == hashWidgetId active = styleActiveBg widgetBase
        | nt == NodeCheckbox || nt == NodeSlider || isClose = styleBg widgetBase
        | otherwise = hoverBackground widgetBase animT isHot
  pure widgetBase {styleBg = bg}

hoverBackground :: Style -> Float -> Bool -> Color
hoverBackground base val isHot
  | styleBg base == styleHoverBg base = styleBg base
  | isHot = lerpColor (styleBg base) (styleHoverBg base) (if val > 0 then val else 1)
  | otherwise = lerpColor (styleBg base) (styleHoverBg base) val

fillStyledRect :: DrawArena -> Bool -> Style -> Rect -> IO ()
fillStyledRect da terminal style rect =
  if terminal || styleCornerRadius style <= 0
    then pushRect da rect (styleBg style)
    else pushRoundedRect da rect (styleCornerRadius style) (styleBg style)

strokeStyledRect :: DrawArena -> Bool -> Style -> Float -> Float -> Float -> Float -> IO ()
strokeStyledRect da terminal style x y w h =
  when (not terminal && styleBorderWidth style > 0) $ do
    let bw = max 1 (styleBorderWidth style)
        col = styleBorder style
        r = styleCornerRadius style
    if r <= 0
      then strokeRect da x y w h bw col
      else strokeRoundedBorder da x y w h r bw col

strokeRoundedBorder ::
  DrawArena ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Color ->
  IO ()
strokeRoundedBorder da x y w h r bw col = do
  -- Half-pixel inset keeps the 1px AA fringe inside the clip. Do not snap
  -- the fill in C or this becomes a full layout pixel again.
  let inset = 0.5
      ox = x + inset
      oy = y + inset
      ow = max 0 (w - 2 * inset)
      oh = max 0 (h - 2 * inset)
      rr = min r (min (ow / 2) (oh / 2))
  pushRoundedStroke da (Rect ox oy ow oh) rr bw col

clamp01 :: Float -> Float
clamp01 v = max 0 (min 1 v)

findNodeByWidgetId :: Context -> WidgetId -> IO (Maybe NodeIdx)
findNodeByWidgetId ctx wid = do
  count <- arenaCount (ctxNodeArena ctx)
  let go idx
        | idx >= count = pure Nothing
        | otherwise = do
            w' <- getWidgetId (ctxNodeArena ctx) idx
            if w' == wid
              then pure (Just idx)
              else go (idx + 1)
  go 0

tabNext :: WidgetId -> [WidgetId] -> Bool -> WidgetId
tabNext cur ids shift =
  case ids of
    [] -> WidgetId 0
    _ ->
      let idx = findIndex (== cur) ids
          pick i = ids !! (i `mod` length ids)
       in case idx of
            Nothing -> ids !! 0
            Just i ->
              if shift
                then pick (i - 1 + length ids)
                else pick (i + 1)

unlessHit :: Bool -> IO () -> IO ()
unlessHit b act = when (not b) act

-- Hit-test widgets with solved layout rects so hover paint matches draw positions.
textInputMenuOuterPad :: Float
textInputMenuOuterPad = 6

textInputMenuItemPadX :: Float
textInputMenuItemPadX = 10

textInputMenuCornerR :: Float
textInputMenuCornerR = 2

textInputMenuShadowOff :: Float
textInputMenuShadowOff = 3

overlayMenuStyle :: Theme -> Style
overlayMenuStyle theme =
  let panel = themePanel theme
      -- SDL panel hover matches panel fill, so the row would be invisible.
      hover =
        if styleHoverBg panel == styleBg panel
          then styleHoverBg (themeButton theme)
          else styleHoverBg panel
      selected = lerpColor (styleBg panel) (themeAccent theme) 0.22
   in panel
        { styleCornerRadius = textInputMenuCornerR
        , styleBorderWidth = 1
        , styleHoverBg = hover
        , styleActiveBg = selected
        }

overlayWindowStyle :: Theme -> Style
overlayWindowStyle theme =
  let win = themeFloatingWindow theme
   in win {styleCornerRadius = 2, styleBorderWidth = 1}

overlayModalStyle :: Theme -> Style
overlayModalStyle theme =
  let base = overlayMenuStyle theme
   in base {styleCornerRadius = 2, styleBorderWidth = 1}

pushMenuShadow :: DrawArena -> Rect -> Float -> IO ()
pushMenuShadow da menuRect r =
  let off = textInputMenuShadowOff
      shadowRect =
        Rect
          (rectX menuRect + off)
          (rectY menuRect + off)
          (rectW menuRect)
          (rectH menuRect)
      shadowCol = colorRGBA 0 0 0 72
   in pushRoundedRect da shadowRect r shadowCol

syncWidgetLabels :: Context -> IO ()
syncWidgetLabels ctx = do
  store <- getStore ctx
  count <- arenaCount (ctxNodeArena ctx)
  forM_ [0 .. count - 1] $ \idx -> do
    nt <- getNodeType (ctxNodeArena ctx) idx
    wid <- getWidgetId (ctxNodeArena ctx) idx
    let key = intKey wid
    case nt of
      NodeCheckbox -> do
        txt <- getText (ctxNodeArena ctx) idx
        let body = checkboxLabelText txt
            val = IM.findWithDefault False key (storeCheckbox store)
            terminal = isCellHost (ctxHostProfile ctx)
            mark = if terminal then checkboxMark (ctxIcons ctx) val else ""
        setNodeText (ctxNodeArena ctx) idx (mark <> body)
        setNodeValue (ctxNodeArena ctx) idx (if val then 1 else 0)
      NodeSlider -> do
        let val = IM.findWithDefault 0 key (storeSlider store)
        txt <- getText (ctxNodeArena ctx) idx
        let (lbl, minV, maxV) = sliderParseRange txt
            frac = if maxV > minV then (val - minV) / (maxV - minV) else 0
            shown =
              if isCellHost (ctxHostProfile ctx)
                then sliderPackTerminal lbl frac val minV maxV
                else sliderPackRange lbl minV maxV
        setNodeText (ctxNodeArena ctx) idx shown
        setNodeValue (ctxNodeArena ctx) idx frac
      NodeButton -> do
        txt <- getText (ctxNodeArena ctx) idx
        when (not (isCellHost (ctxHostProfile ctx))) $
          setNodeText (ctxNodeArena ctx) idx (stripButtonBrackets txt)
      _ -> pure ()

modalTreeOpen :: Context -> IO Bool
modalTreeOpen ctx = do
  top <- topmostModalIdx ctx
  pure (isJust top)

topmostModalIdx :: Context -> IO (Maybe NodeIdx)
topmostModalIdx ctx = do
  count <- arenaCount (ctxNodeArena ctx)
  go (count - 1)
  where
    go idx
      | idx < 0 = pure Nothing
      | otherwise = do
          nt <- getNodeType (ctxNodeArena ctx) idx
          if nt == NodeModal then pure (Just idx) else go (idx - 1)

nodeInTopmostModal :: Context -> NodeIdx -> IO Bool
nodeInTopmostModal ctx idx = do
  mTop <- topmostModalIdx ctx
  case mTop of
    Nothing -> pure False
    Just top -> nodeInSubtree ctx idx top

nodeInSubtree :: Context -> NodeIdx -> NodeIdx -> IO Bool
nodeInSubtree ctx idx top = go idx
  where
    go i
      | i < 0 = pure False
      | i == top = pure True
      | otherwise = do
          parent <- getParent (ctxNodeArena ctx) i
          go parent

modalHitAllowed :: Context -> NodeIdx -> IO Bool
modalHitAllowed ctx idx = do
  mTop <- topmostModalIdx ctx
  case mTop of
    Nothing -> pure True
    Just top -> nodeInSubtree ctx idx top

overlayHitAllowed :: Context -> NodeIdx -> V2 -> IO Bool
overlayHitAllowed ctx idx mouse = do
  mModal <- topmostModalIdx ctx
  case mModal of
    Just _ -> modalHitAllowed ctx idx
    Nothing -> do
      mWin <- topmostWindowAtMouse ctx mouse
      case mWin of
        Nothing -> pure True
        Just widx -> nodeInSubtree ctx idx widx

topmostWindowAtMouse :: Context -> V2 -> IO (Maybe NodeIdx)
topmostWindowAtMouse ctx mouse = do
  count <- arenaCount (ctxNodeArena ctx)
  go (count - 1)
  where
    go idx
      | idx < 0 = pure Nothing
      | otherwise = do
          nt <- getNodeType (ctxNodeArena ctx) idx
          if nt /= NodeWindow
            then go (idx - 1)
            else do
              (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
              if w > 0 && h > 0 && rectContains (Rect x y w h) mouse
                then pure (Just idx)
                else go (idx - 1)

filterModalFocusables :: Context -> [WidgetId] -> IO [WidgetId]
filterModalFocusables ctx ids = do
  open <- modalTreeOpen ctx
  if not open
    then pure ids
    else filterM (widgetIdInModal ctx) ids

widgetIdInModal :: Context -> WidgetId -> IO Bool
widgetIdInModal ctx wid = do
  count <- arenaCount (ctxNodeArena ctx)
  go 0 count
  where
    go idx count
      | idx >= count = pure False
      | otherwise = do
          w' <- getWidgetId (ctxNodeArena ctx) idx
          if w' == wid
            then nodeInTopmostModal ctx idx
            else go (idx + 1) count

widgetOverlayAllowed :: Context -> WidgetId -> IO Bool
widgetOverlayAllowed ctx wid = do
  open <- modalTreeOpen ctx
  if not open then pure True else widgetIdInModal ctx wid

constrainFocusToModal :: Context -> IO ()
constrainFocusToModal ctx = do
  open <- modalTreeOpen ctx
  when open $ do
    focus <- readIORef (ctxFocusId ctx)
    when (hashWidgetId focus /= 0) $ do
      ok <- widgetIdInModal ctx focus
      unless ok $ writeIORef (ctxFocusId ctx) (WidgetId 0)

strokeRect :: DrawArena -> Float -> Float -> Float -> Float -> Float -> Color -> IO ()
strokeRect da x y w h bw col =
  let inset = 0.5
      ox = x + inset
      oy = y + inset
      ow = max 0 (w - 2 * inset)
      oh = max 0 (h - 2 * inset)
   in pushRoundedStroke da (Rect ox oy ow oh) 0 (max 1 bw) col

padDropText :: Int -> T.Text -> T.Text
padDropText n txt =
  let len = T.length txt
   in if len >= n then T.take n txt else txt <> T.replicate (n - len) (T.singleton ' ')

padContentClip :: HostProfile -> FontMetrics -> Float -> Float -> Float -> Float -> Padding -> Rect
padContentClip host fm x y w h pad0 =
  let pad = resolveLayoutPadding host fm pad0
   in Rect
        (x + padL pad)
        (y + padT pad)
        (max 0 (w - padL pad - padR pad))
        (max 0 (h - padT pad - padB pad))

-- TUI modal: title and separator stay fixed; modal/2 wraps body in scroll.
-- Outer clip is the padded panel. Inner NodeScrollContainer clips overflow.
terminalModalOuterClip :: HostProfile -> FontMetrics -> Float -> Float -> Float -> Float -> Padding -> Rect
terminalModalOuterClip = padContentClip

scrollContentClip ::
  HostProfile ->
  FontMetrics ->
  ScrollBarSlot ->
  DirTag ->
  Float ->
  Float ->
  Float ->
  Float ->
  Padding ->
  Float ->
  Rect
scrollContentClip host fm slot dir x y w h pad contentSize =
  let base = padContentClip host fm x y w h pad
      innerMain =
        case dir of
          DirColumn -> rectH base
          DirRow -> rectW base
      gutter = scrollLayoutGutter host fm slot contentSize innerMain
   in case dir of
        DirColumn -> Rect (rectX base) (rectY base) (max 0 (rectW base - gutter)) (rectH base)
        DirRow -> Rect (rectX base) (rectY base) (rectW base) (max 0 (rectH base - gutter))

-- List/page bars sit in the scroll rect. Window body hangs into the parent pad.
scrollChromeLane ::
  HostProfile -> FontMetrics -> ScrollBarSlot -> DirTag -> Float -> Float -> Float -> Float -> Padding -> Rect
scrollChromeLane host fm slot dir x y w h pad =
  let (barW, _) = scrollBarGeomFor host fm slot
      outer = scrollBarOuterGap host fm slot
      hang = slot == ScrollBarWindow
   in case dir of
        DirColumn ->
          let laneX =
                if hang
                  then x + w + outer
                  else max x (x + w - outer - barW)
           in Rect laneX (y + padT pad) barW (max 0 (h - padT pad - padB pad))
        DirRow ->
          let laneY =
                if hang
                  then y + h + outer
                  else max y (y + h - outer - barW)
           in Rect (x + padL pad) laneY (max 0 (w - padL pad - padR pad)) barW

imageIdFromText :: T.Text -> Int
imageIdFromText txt =
  case reads (T.unpack txt) of
    [(n, "")] | n > 0 -> n
    _ -> 0
