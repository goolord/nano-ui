{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module NanoUI.Frame.Paint
  ( lowerShapes
  , lowerNode
  , walkChildren
  , drawTooltipOverlays
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
import NanoUI.Frame.Internal (clamp01, displayText, fillStyledRect, imageIdFromText, nodeLabelPaint, overlayModalStyle, overlayWindowStyle, scrollContentClip, strokeStyledRect, terminalModalOuterClip, textInputFocused, textInputValue, widgetOverlayAllowed, widgetVisualStyle)
import NanoUI.Frame.Scroll (paintScrollChrome)
import NanoUI.Frame.Spans (sliderValue, widgetTextPlacements)
import NanoUI.Frame.TextInput (TextInputGeom (..), drawTextInputCaret, textInputGeom)

lowerShapes :: Context -> IO ()
lowerShapes ctx = do
  count <- arenaCount (ctxNodeArena ctx)
  when (count > 0) $ lowerNode ctx 0

lowerNode :: Context -> NodeIdx -> IO ()
lowerNode ctx idx = do
  nt <- getNodeType (ctxNodeArena ctx) idx
  (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
  let rect = Rect x y w h
      fm = ctxFontMetrics ctx
      theme = ctxTheme ctx
      terminal = isCellHost (ctxHostProfile ctx)
      da = ctxDrawArena ctx
  case nt of
    NodeContainer -> walkChildren ctx idx
    NodePanel -> do
      let style = themePanel theme
      fillStyledRect da terminal style rect
      strokeStyledRect da terminal style x y w h
      withClip da (borderContentClip style rect) $ walkChildren ctx idx
    NodeScrollContainer -> do
      let style = themeInput theme
      pad <- getPadding (ctxNodeArena ctx) idx
      (wTag, _) <- getWidthSizing (ctxNodeArena ctx) idx
      (hTag, _) <- getHeightSizing (ctxNodeArena ctx) idx
      -- Paint bounded wells; full-page Grow/Grow viewports stay on the window fill.
      -- Square corners so axis-aligned clip does not leave rounded leftover pixels.
      let paintWell = not (wTag == SizingGrow && hTag == SizingGrow)
          wellStyle = style {styleCornerRadius = 0}
      when paintWell $ do
        fillStyledRect da terminal wellStyle rect
        strokeStyledRect da terminal wellStyle x y w h
      dir <- getDirection (ctxNodeArena ctx) idx
      contentSize <- getNodeValue (ctxNodeArena ctx) idx
      slot <- scrollBarSlotOf (ctxNodeArena ctx) idx
      let inner = scrollContentClip (ctxHostProfile ctx) fm slot dir x y w h pad contentSize
      withClip da inner $ walkChildren ctx idx
      wid <- getWidgetId (ctxNodeArena ctx) idx
      paintScrollChrome ctx da idx wid x y w h pad theme terminal
    NodeText -> do
      raw <- getText (ctxNodeArena ctx) idx
      let (txt, fg, _) = nodeLabelPaint theme raw
          (ix, _) = labelContentInset (ctxHostProfile ctx) fm
      ax <- getAlignX (ctxNodeArena ctx) idx
      when (not (ctxExternalText ctx) && not (T.null txt)) $ do
        (tw, th) <- ctxMeasureText ctx raw
        let (tx, used) = alignedTextBox ax x w ix tw
        pushRect da (Rect tx (centeredTextY (ctxHostProfile ctx) fm y h th) used th) fg
    NodeSeparator -> do
      let hair = 1
      when (not terminal) $
        if w >= h
          then pushRect da (Rect x (y + (h - hair) / 2) w hair) (themeSeparator theme)
          else pushRect da (Rect (x + (w - hair) / 2) y hair h) (themeSeparator theme)
    NodeTextInput
      | not terminal -> do
          style <- widgetVisualStyle ctx nt idx
          focus <- textInputFocused ctx idx
          let geom = textInputGeom (ctxHostProfile ctx) fm x y w h
              fieldRect = tigFieldRect geom
              borderCol =
                if focus
                  then themeAccent theme
                  else styleBorder style
              fieldStyle = style {styleBorder = borderCol}
          fillStyledRect da False style fieldRect
          strokeStyledRect
            da
            False
            fieldStyle
            (rectX fieldRect)
            (rectY fieldRect)
            (rectW fieldRect)
            (rectH fieldRect)
          drawTextInputCaret da ctx idx x y w h style
    NodeSpacer -> pure ()
    NodeModal -> pure ()
    NodeWindow -> pure ()
    NodeBox -> do
      si <- getStyleIdx (ctxNodeArena ctx) idx
      -- styleIdx holds RGBA Word32 bits; see `box` in NanoUI.Widgets.
      pushRect da rect (Color (fromIntegral si :: Word32))
    NodeImage -> do
      tex <- imageIdFromText <$> getText (ctxNodeArena ctx) idx
      mUv <- lookupImageUv ctx (ImageId tex)
      case mUv of
        Just (u0, v0, u1, v1)
          | not terminal ->
              pushImage da rect atlasTextureId u0 v0 u1 v1 (colorRGBA 255 255 255 255)
        _ -> pushRect da rect (themeAccent theme)
    _ -> do
      style <- widgetVisualStyle ctx nt idx
      value <- getNodeValue (ctxNodeArena ctx) idx
      storedText <-
        if nt == NodeButton
          then getText (ctxNodeArena ctx) idx
          else pure T.empty
      let isClose = nt == NodeButton && isCloseButtonText storedText
      let opaqueBg
            | isClose = False
            | terminal, nt == NodeButton = False
            | terminal, nt == NodeCheckbox = False
            | terminal, nt == NodeSlider = False
            | terminal, nt == NodeSelect = False
            | terminal, nt == NodeTextInput = False
            | terminal, nt == NodeText = False
            | terminal = True
            | otherwise =
                nt /= NodeCheckbox && nt /= NodeSlider && nt /= NodeTextInput
      when opaqueBg $ fillStyledRect da terminal style rect
      when (not terminal) $ do
        when opaqueBg $ strokeStyledRect da terminal style x y w h
        when (nt == NodeCheckbox) $
          drawCheckbox
            (ctxHostProfile ctx)
            da
            fm
            style
            x
            y
            h
            value
            (themeAccent theme)
            (styleBg (themeInput theme))
            (colorRGBA 255 255 255 255)
        when (nt == NodeSlider) $ do
          txt <- getText (ctxNodeArena ctx) idx
          let lbl = sliderLabelText (T.takeWhile (/= '\US') txt)
              track = sliderTrackBounds (ctxHostProfile ctx) fm lbl x y w h
              tx = rectX track
              ty = rectY track
              tw = rectW track
              th = rectH track
              trackR = 3
              fillW = max 0 (tw * clamp01 value)
              outline = styleBorder (themeInput theme)
              well = colorRGBA 72 48 48 255
              fill = colorRGBA 204 102 102 255
              bw = 1
              innerR = max 0 (trackR - bw)
              innerX = tx + bw
              innerY = ty + bw
              innerW = tw - 2 * bw
              innerH = th - 2 * bw
              innerFillW = max 0 (innerW * clamp01 value)
          pushRoundedRect da track trackR outline
          when (innerW > 0 && innerH > 0) $
            pushRoundedRect da (Rect innerX innerY innerW innerH) innerR well
          when (innerFillW > 0) $ do
            let fillR =
                  if innerFillW >= innerW - 0.5
                    then innerR
                    else min innerR (innerFillW / 2)
            pushRoundedRect da (Rect innerX innerY innerFillW innerH) fillR fill
          let handleD = 18
              handleCx = tx + max (handleD / 2) (min (tw - handleD / 2) fillW)
              handleHy = ty + (th - handleD) / 2
              handle = Rect (handleCx - handleD / 2) handleHy handleD handleD
              innerD = handleD - 2
              handleInner =
                Rect
                  (handleCx - innerD / 2)
                  (handleHy + (handleD - innerD) / 2)
                  innerD
                  innerD
          pushRoundedRect da handle (handleD / 2) (styleBorder (themeInput theme))
          pushRoundedRect da handleInner (innerD / 2) (colorRGBA 255 255 255 255)
        when isClose $
          drawCloseIcon (ctxHostProfile ctx) fm da x y w h (styleFg style)
        when (nt == NodeSelect) $
          drawSelectChevron da x y w h (styleFg style)
      placements <- widgetTextPlacements ctx nt idx x y w h
      when (terminal && not (ctxExternalText ctx)) $
        forM_ placements $ \(txt, px, py, _, _) ->
          when (not (T.null txt)) $
            pushText da fm px py txt (styleFg style)

drawCheckbox ::
  HostProfile ->
  DrawArena ->
  FontMetrics ->
  Style ->
  Float ->
  Float ->
  Float ->
  Float ->
  Color ->
  Color ->
  Color ->
  IO ()
drawCheckbox host da fm style x y h value accent well markCol = do
  let (ix, _) =
        if isCellHost host
          then widgetContentInset host fm
          else labelContentInset host fm
      box = checkboxBoxSize host fm
      bx = x + ix
      by = y + (h - box) / 2
      r = min 6 (box / 3.5)
      bw = 2
      outer = Rect bx by box box
      inner = Rect (bx + bw) (by + bw) (box - 2 * bw) (box - 2 * bw)
      innerR = max 0 (r - bw)
  if value >= 0.5
    then do
      pushRoundedRect da outer r accent
      drawCheckboxMark da bx by box markCol
    else do
      pushRoundedRect da outer r (styleBorder style)
      pushRoundedRect da inner innerR well

drawCheckboxMark :: DrawArena -> Float -> Float -> Float -> Color -> IO ()
drawCheckboxMark da bx by box markCol = do
  let t = max 1.6 (box * 0.11)
      x0 = bx + box * 0.22
      y0 = by + box * 0.52
      x1 = bx + box * 0.42
      y1 = by + box * 0.72
      x2 = bx + box * 0.78
      y2 = by + box * 0.28
  pushLine da x0 y0 x1 y1 t markCol
  pushLine da x1 y1 x2 y2 t markCol

borderContentClip :: Style -> Rect -> Rect
borderContentClip style (Rect x y w h) =
  if styleBorderWidth style <= 0
    then Rect x y w h
    else
      let bw = max 1 (styleBorderWidth style)
       in Rect (x + bw) (y + bw) (max 0 (w - 2 * bw)) (max 0 (h - 2 * bw))

walkChildren :: Context -> NodeIdx -> IO ()
walkChildren ctx idx = do
  fc <- getFirstChild (ctxNodeArena ctx) idx
  go fc
  where
    go ci =
      if ci < 0
        then pure ()
        else do
          lowerNode ctx ci
          ns <- getNextSibling (ctxNodeArena ctx) ci
          go ns

drawCloseIcon :: HostProfile -> FontMetrics -> DrawArena -> Float -> Float -> Float -> Float -> Color -> IO ()
drawCloseIcon host fm da x y w h col = do
  let s = min w h * 0.72
      th = s
      ty = centeredTextY host fm y h th
      tx = x + (w - s) / 2
      inset = s * 0.32
      t = max 1.6 (s * 0.10)
      x0 = tx + inset
      y0 = ty + inset
      x1 = tx + s - inset
      y1 = ty + s - inset
  pushLine da x0 y0 x1 y1 t col
  pushLine da x0 y1 x1 y0 t col

drawSelectChevron :: DrawArena -> Float -> Float -> Float -> Float -> Color -> IO ()
drawSelectChevron da x y w h col = do
  let cx = selectChevronCenterX x w
      cy = y + h / 2
      hw = 4.2
      hh = 2.6
  pushFilledTriangle da (cx - hw) (cy - hh * 0.35) (cx + hw) (cy - hh * 0.35) cx (cy + hh) col

drawTooltipOverlays :: Context -> IO ()
drawTooltipOverlays ctx = do
  let da = ctxDrawArena ctx
      theme = ctxTheme ctx
      terminal = isCellHost (ctxHostProfile ctx)
  when (not terminal) $ do
    tips <- readTooltips ctx
    let panelStyle = themePanel theme
    forM_ tips $ \(PendingTooltip wid rect _) -> do
      allow <- widgetOverlayAllowed ctx wid
      when allow $ fillStyledRect da False panelStyle rect

