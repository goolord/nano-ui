{-# LANGUAGE DataKinds #-}

-- | Widget paint helpers: labels, styles, rects, and display text.
module NanoUI.Frame.Chrome
  ( widgetNodeTypeTable
  , floatingAncestor
  , buildFloatingAncestorMap
  , displayText
  , nodeLabelPaint
  , floatingLabelPaint
  , widgetVisualStyle
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
  , imageIdFromText
  , clamp01
  , paintTabHeader
  ) where

import Control.Monad (foldM, when)
import Data.IORef (readIORef, writeIORef)
import qualified Data.IntMap.Strict as IM
import Data.Text (Text)
import qualified Data.Text as T
import NanoUI.Context
  ( Context (..)
  , WidgetStore (..)
  , getAnimationValue
  , getStore
  , intKey
  )
import NanoUI.Draw (DrawArena, pushRect, pushRoundedRect, pushRoundedStroke)
import NanoUI.Font (hasHeadingMarker, hasMutedMarker, stripWidgetMarkers)
import NanoUI.Host (HostProfile, isCellHost)
import NanoUI.Icons (iconSelectClosed, iconSelectOpen)
import NanoUI.Id (hashWidgetId)
import NanoUI.Layout.Arena
  ( NodeIdx
  , NodeType (..)
  , arenaCount
  , getNodeType
  , getNodeValue
  , getParent
  , getStyleIdx
  , getText
  , getWidgetId
  , isFloatingNode
  , isWidgetNode
  )
import NanoUI.WidgetText
  ( buttonDisplayText
  , buttonDisplayTextFromFlags
  , buttonFlags
  , stripButtonBrackets
  , checkboxLabelText
  , radioLabelText
  , treeDisplayText
  , treeLabelText
  , treeParseRow
  , selectDisplayText
  , selectParseOptions
  , colorPickerDisplayText
  , sliderLabelText
  , textInputFieldText
  , textInputTerminalText
  )
import NanoUI.Style
  ( Style (..)
  , Theme (..)
  , themeAccent
  , themeButton
  , themeFloatingWindow
  , themeInput
  , themeMuted
  , themePanel
  , themeWindow
  )
import NanoUI.ColorPicker (colorPickerDefaultColor, widgetStoreColor)
import NanoUI.Types (Color (..), Rect (..), colorRGBA, colorR, colorG, colorB, clamp01, lerpColor, rectH, rectW, rectX, rectY)

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
  cached <- readIORef (ctxFloatingAncestor ctx)
  case cached of
    Just table -> pure table
    Nothing -> do
      table <- buildFloatingAncestorMapFresh ctx
      writeIORef (ctxFloatingAncestor ctx) (Just table)
      pure table

buildFloatingAncestorMapFresh :: Context -> IO (IM.IntMap (Maybe NodeType))
buildFloatingAncestorMapFresh ctx = do
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
        NodeColorPicker -> do
          store <- getStore ctx
          wid <- getWidgetId (ctxNodeArena ctx) idx
          let current = widgetStoreColor store wid colorPickerDefaultColor
          pure (colorPickerDisplayText txt current)
        NodeSlider -> pure (T.takeWhile (/= '\US') txt)
        NodeTree -> do
          let (_, _, depth, hasKids, expanded, lbl) = treeParseRow txt
          pure (treeDisplayText (ctxIcons ctx) depth hasKids expanded lbl)
        NodeButton ->
          let flags = buttonFlags txt
           in pure (buttonDisplayTextFromFlags flags txt)
        NodeTextInput -> do
          value <- textInputValue ctx idx
          focused <- textInputFocused ctx idx
          wid <- getWidgetId (ctxNodeArena ctx) idx
          store <- getStore ctx
          let cursor = IM.findWithDefault (T.length value) (intKey wid) (storeCursor store)
          pure (textInputTerminalText txt value cursor focused)
        _ -> pure txt
    else
      case nt of
        NodeCheckbox -> pure (checkboxLabelText txt)
        NodeRadio -> pure (radioLabelText txt)
        NodeTree -> pure (treeLabelText txt)
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
        NodeColorPicker -> pure txt
        NodeButton -> pure (buttonDisplayText txt)
        _ -> pure (stripButtonBrackets txt)

textInputValue :: Context -> NodeIdx -> IO Text
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

tabHeaderVisualStyle :: Theme -> Int -> Bool -> Bool -> Float -> Style
tabHeaderVisualStyle theme styleIdx isActive _isHot _animT =
  let panel = themePanel theme
      btn = themeButton theme
      muted = themeMuted theme
      window = themeWindow theme
      accent = themeAccent theme
      clear = colorRGBA 0 0 0 0
      hoverLift = lerpColor window (styleHoverBg btn) 0.55
   in case styleIdx of
        1 ->
          if isActive
            then panel
                { styleBg = accent
                , styleHoverBg = accent
                , styleFg = colorRGBA 255 255 255 255
                , styleBorder = accent
                , styleBorderWidth = 0
                , styleCornerRadius = 6
                }
            else
              panel
                { styleBg = clear
                , styleHoverBg = hoverLift
                , styleFg = muted
                , styleBorder = clear
                , styleBorderWidth = 0
                , styleCornerRadius = 6
                }
        2 ->
          if isActive
            then
              panel
                { styleBg = styleBg panel
                , styleHoverBg = styleBg panel
                , styleFg = styleFg panel
                , styleBorder = styleBorder panel
                , styleBorderWidth = 1
                , styleCornerRadius = 8
                }
            else
              panel
                { styleBg = clear
                , styleHoverBg = hoverLift
                , styleFg = muted
                , styleBorder = clear
                , styleBorderWidth = 0
                , styleCornerRadius = 8
                }
        _ ->
          if isActive
            then
              panel
                { styleBg = styleBg panel
                , styleHoverBg = styleBg panel
                , styleFg = styleFg panel
                , styleBorder = styleBorder panel
                , styleBorderWidth = 1
                , styleCornerRadius = 6
                }
            else
              panel
                { styleBg = clear
                , styleHoverBg = hoverLift
                , styleFg = lerpColor muted (styleFg panel) 0.78
                , styleBorder = clear
                , styleBorderWidth = 0
                , styleCornerRadius = 6
                }

paintTabHeader ::
  DrawArena ->
  HostProfile ->
  Theme ->
  Int ->
  Bool ->
  Style ->
  Float ->
  Float ->
  Float ->
  Float ->
  IO ()
paintTabHeader da host theme styleIdx isActive style x y w h =
  when (not (isCellHost host)) $ do
    let rect = Rect x y w h
        r = max 0 (styleCornerRadius style)
        accent = themeAccent theme
        panel = themePanel theme
        border = styleBorder panel
        bg = styleBg style
        clear = colorRGBA 0 0 0 0
        hasFill = bg /= clear
    if isActive
      then case styleIdx of
        1 -> pushRoundedRect da rect r bg
        2 -> do
          pushRoundedRect da rect r bg
          strokeStyledRect da False style x y w h
        _ -> do
          pushRoundedRect da rect r bg
          strokeTabHeaderSides da x y w h r border
          pushRect da (Rect x (y + h - 2) w 2) accent
      else when hasFill $ pushRoundedRect da rect r bg

strokeTabHeaderSides ::
  DrawArena -> Float -> Float -> Float -> Float -> Float -> Color -> IO ()
strokeTabHeaderSides da x y w h r col =
  let bw = 1
      inset = 0.5
      ox = x + inset
      oy = y + inset
      ow = max 0 (w - 2 * inset)
      oh = max 0 (h - 2 * inset)
      rr = min r (min (ow / 2) (oh / 2))
   in pushRoundedStroke da (Rect ox oy ow (oh + 1)) rr bw col

widgetVisualStyle :: Context -> NodeType -> NodeIdx -> IO Style
widgetVisualStyle ctx nt idx = do
  wid <- getWidgetId (ctxNodeArena ctx) idx
  val <- getNodeValue (ctxNodeArena ctx) idx
  hot <- readIORef (ctxHotId ctx)
  active <- readIORef (ctxActiveId ctx)
  focus <- readIORef (ctxFocusId ctx)
  animT <- getAnimationValue ctx wid
  mFloat <- floatingAncestor ctx idx
  storedText <-
    if nt == NodeButton
      then getText (ctxNodeArena ctx) idx
      else pure T.empty
  styleIdx <-
    if nt == NodeButton
      then getStyleIdx (ctxNodeArena ctx) idx
      else pure 0
  let (isClose, isTab) =
        if nt == NodeButton
          then buttonFlags storedText
          else (False, False)
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
          NodeColorPicker ->
            let sel = themeInput theme
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
          NodeRadio ->
            (themeButton theme)
              { styleBg = colorRGBA 0 0 0 0
              , styleHoverBg = colorRGBA 0 0 0 0
              , styleActiveBg = colorRGBA 0 0 0 0
              , styleBorderWidth = 0
              }
          NodeTree ->
            let btn = themeButton theme
                accent = themeAccent theme
             in if val > 0.5
                  then
                    btn
                      { styleBg = colorRGBA (colorR accent) (colorG accent) (colorB accent) 48
                      , styleHoverBg = colorRGBA (colorR accent) (colorG accent) (colorB accent) 72
                      , styleActiveBg = colorRGBA (colorR accent) (colorG accent) (colorB accent) 96
                      , styleBorderWidth = 0
                      }
                  else
                    btn
                      { styleBg = colorRGBA 0 0 0 0
                      , styleHoverBg = colorRGBA (colorR accent) (colorG accent) (colorB accent) 32
                      , styleActiveBg = colorRGBA (colorR accent) (colorG accent) (colorB accent) 48
                      , styleBorderWidth = 0
                      }
          NodeButton
            | isClose -> closeButtonStyle theme isHot animT
            | isTab, terminal ->
                let btn = themeButton theme
                    accent = themeAccent theme
                    muted = themeMuted theme
                 in if val > 0.5
                      then btn
                        { styleBg = accent
                        , styleHoverBg = accent
                        , styleFg = colorRGBA 255 255 255 255
                        , styleBorder = accent
                        , styleBorderWidth = 0
                        }
                      else btn
                        { styleBg = colorRGBA 0 0 0 0
                        , styleHoverBg = colorRGBA 0 0 0 0
                        , styleFg = muted
                        , styleBorderWidth = 0
                        }
            | isTab ->
                tabHeaderVisualStyle theme styleIdx (val > 0.5) isHot animT
            | not terminal && val > 0.5 ->
                let btn = themeButton theme
                 in btn
                      { styleBg = themeAccent theme
                      , styleHoverBg = themeAccent theme
                      , styleFg = colorRGBA 255 255 255 255
                      , styleBorder = themeAccent theme
                      }
            | Just NodeWindow <- mFloat, terminal -> themeFloatingWindow theme
            | Just NodeModal <- mFloat, terminal -> themeFloatingWindow theme
          _ -> themeButton theme
      widgetBase =
        case mFloat of
          Just NodeModal
            | terminal -> base
            | nt == NodeCheckbox || nt == NodeRadio || nt == NodeTree || nt == NodeSlider -> overlayModalStyle theme
            | otherwise -> base
          _ -> base
      bg
        | terminal, widKey == hashWidgetId active = styleActiveBg widgetBase
        | terminal, isHot = styleHoverBg widgetBase
        | terminal = styleBg widgetBase
        | nt == NodeTextInput, isFocus = styleActiveBg widgetBase
        | widKey == hashWidgetId active = styleActiveBg widgetBase
        | nt == NodeCheckbox || nt == NodeRadio || nt == NodeSlider || isClose = styleBg widgetBase
        | nt == NodeTree = hoverBackground widgetBase animT isHot
        | isTab = hoverBackground widgetBase animT isHot
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
  let inset = 0.5
      ox = x + inset
      oy = y + inset
      ow = max 0 (w - 2 * inset)
      oh = max 0 (h - 2 * inset)
      rr = min r (min (ow / 2) (oh / 2))
  pushRoundedStroke da (Rect ox oy ow oh) rr bw col

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

imageIdFromText :: T.Text -> Int
imageIdFromText txt =
  case reads (T.unpack txt) of
    [(n, "")] | n > 0 -> n
    _ -> 0
