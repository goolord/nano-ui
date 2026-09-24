-- | Widget paint helpers: labels, styles, rects, menu panels and display text.
module NanoUI.Internal.Frame.Chrome
  ( floatingAncestor
  , displayText
  , widgetVisualStyle
  , textInputValue
  , textInputFocused
  , fillStyledRect
  , strokeStyledRect
  , paintStyledRect
  , overlayWindowStyle
  , overlayMenuStyle
  , paintMenuPanel
  , menuPanelBounds
  , paintMenuAccent
  , paintScrollBars
  , imageIdFromText
  , paintTabHeader
  , paintTableHeader
  ) where

import Control.Monad (when)
import Data.IORef (readIORef)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import NanoUI.Internal.Context
import NanoUI.Internal.Draw (DrawArena, pushRect, pushRoundedRect, pushRoundedStroke)
import NanoUI.Internal.Frame.Scroll.Geometry (ScrollBarLayout (..))
import NanoUI.Internal.Id (WidgetId, hashWidgetId)
import NanoUI.Internal.Layout.Arena
import NanoUI.Internal.Store (fieldInt, fieldText, findSlot)
import NanoUI.Internal.Style
import NanoUI.Internal.Types (Color (..), Rect (..), clamp, colorA, colorRGBA, lerpColor)
import NanoUI.Internal.WidgetText

floatingAncestor :: Context -> NodeIdx -> IO (Maybe NodeType)
floatingAncestor ctx idx = walkFloatingAncestors (ctxNodeArena ctx) idx (\_ nt -> pure (Just nt))

displayText :: Context -> NodeType -> NodeIdx -> IO Text
displayText ctx nt idx = do
  txt <- getText (ctxNodeArena ctx) idx
  case nt of
    NodeButton -> do
      si <- getStyleIdx (ctxNodeArena ctx) idx
      pure $! if hasFlag buttonFlagTable si then tableHeaderDisplayText txt else txt
    NodeTextInput -> textInputFieldText txt <$> textInputValue ctx idx <*> textInputFocused ctx idx
    NodeTextArea -> textInputValue ctx idx
    NodeSelect -> selectDisplayText txt <$> selectCurrentOption ctx idx
    _ -> pure txt

selectCurrentOption :: Context -> NodeIdx -> IO Text
selectCurrentOption ctx idx = do
  store <- getStore ctx
  opts <- getOptions (ctxNodeArena ctx) idx
  wid <- getWidgetId (ctxNodeArena ctx) idx
  let picked = findSlot fieldInt 0 (intKey wid) store
  pure (fromMaybe "" (listToMaybe (drop picked opts)))

-- | The text a field displays: its stored value, masked one character per
-- character for password inputs so caret and selection offsets still line up.
textInputValue :: Context -> NodeIdx -> IO Text
textInputValue ctx@Context {ctxNodeArena = na} idx = do
  wid <- getWidgetId na idx
  nt <- getNodeType na idx
  si <- getStyleIdx na idx
  store <- getStore ctx
  let value = findSlot fieldText "" (intKey wid) store
  pure $
    if nt == NodeTextInput && hasFlag textInputFlagPassword si
      then T.replicate (T.length value) "*"
      else value

textInputFocused :: Context -> NodeIdx -> IO Bool
textInputFocused ctx idx = do
  wid <- getWidgetId (ctxNodeArena ctx) idx
  focus <- readIORef (ctxFocusId ctx)
  pure (focus == wid)

-- | Fully transparent black.
transparentColor :: Color
transparentColor = colorRGBA 0 0 0 0

-- | Transparent fills and no border.
clearStyle :: Style -> Style
clearStyle s = s {styleBg = transparentColor, styleHoverBg = transparentColor, styleActiveBg = transparentColor, styleBorderWidth = 0}

closeButtonStyle :: Theme -> Bool -> Float -> Style
closeButtonStyle theme isHot animT =
  let btn = themeButton theme
      muted = lerpColor (styleFg btn) (styleBg (themePanel theme)) 0.42
      t = if isHot && not (animT > 0) then 1 else animT
   in (clearStyle btn) {styleFg = lerpColor muted (styleFg btn) t}

tabHeaderVisualStyle :: Theme -> Int -> Bool -> Style
tabHeaderVisualStyle theme styleIdx isActive =
  let panel = themePanel theme
      btn = themeButton theme
      muted = themeMuted theme
      accent = themeAccent theme
      hoverLift = lerpColor (themeWindow theme) (styleHoverBg btn) 0.55
      (cr, activeBg, activeFg, activeBw, inactFg) = case styleIdx of
        1 -> (6, accent, themeOnAccent theme, 0, muted)
        2 -> (8, styleBg panel, styleFg panel, 1, muted)
        _ -> (6, styleBg panel, styleFg panel, 1, lerpColor muted (styleFg panel) 0.78)
   in if isActive
        then panel
          { styleBg = activeBg
          , styleHoverBg = activeBg
          , styleFg = activeFg
          , styleBorder = activeBg
          , styleBorderWidth = activeBw
          , styleCornerRadius = cr
          }
        else panel
          { styleBg = transparentColor
          , styleHoverBg = hoverLift
          , styleFg = inactFg
          , styleBorder = transparentColor
          , styleBorderWidth = 0
          , styleCornerRadius = cr
          }

-- | Flat menu row / menu-bar entry. Transparent at rest, a hover highlight
-- (matching the text-field context menu), and an accent-tinted fill while it
-- owns an open drop-down (@val > 0.5@, menu-bar titles only).
menuItemVisualStyle :: Theme -> Float -> Style
menuItemVisualStyle theme val =
  let menu = overlayMenuStyle theme
      accent = themeAccent theme
      openBg = lerpColor (styleBg menu) accent 0.3
      isOpen = val > 0.5
   in menu
        { styleBg = if isOpen then openBg else transparentColor
        , styleHoverBg = if isOpen then openBg else styleHoverBg menu
        , styleActiveBg = lerpColor (styleBg menu) accent 0.4
        , styleBorder = transparentColor
        , styleBorderWidth = 0
        -- The text-field context menu fills hovered rows with a square
        -- pushRect; keep the generic menu identical.
        , styleCornerRadius = 0
        }

tableHeaderVisualStyle :: Theme -> Bool -> Style
tableHeaderVisualStyle theme isSorted =
  let btn = themeButton theme
      accent = themeAccent theme
      headerBg = lerpColor (styleBg (themePanel theme)) (styleBg btn) 0.55
   in btn
        { styleBg = headerBg
        , styleHoverBg = lerpColor headerBg accent 0.18
        , styleActiveBg = lerpColor headerBg accent 0.28
        , styleFg = if isSorted then styleFg btn else themeMuted theme
        , styleBorderWidth = 0
        , styleCornerRadius = 0
        }

paintTabHeader :: DrawArena -> Theme -> Int -> Bool -> Style -> Float -> Float -> Float -> Float -> IO ()
paintTabHeader da theme tabStyle isActive style x y w h = do
  let rect = Rect x y w h
      r = max 0 (styleCornerRadius style)
      bg = styleBg style
  if isActive
    then do
      pushRoundedRect da rect r bg
      case tabStyle of
        1 -> pure ()
        2 -> strokeStyledRect da style rect
        _ -> do
          pushRoundedStroke da (Rect x y w (h + 1)) (min r (min (w / 2) (h / 2))) 1 (styleBorder (themePanel theme))
          pushRect da (Rect x (y + h - 2) w 2) (themeAccent theme)
    else when (bg /= transparentColor) $ pushRoundedRect da rect r bg

paintTableHeader :: DrawArena -> Theme -> Bool -> Style -> Float -> Float -> Float -> Float -> IO ()
paintTableHeader da theme isSorted style x y w h = do
  pushRect da (Rect x y w h) (styleBg style)
  when isSorted $
    pushRect da (Rect x (y + h - 2) w 2) (themeAccent theme)

widgetVisualStyle :: Context -> NodeType -> NodeIdx -> IO Style
widgetVisualStyle ctx nt idx = do
  wid <- getWidgetId (ctxNodeArena ctx) idx
  val <- getNodeValue (ctxNodeArena ctx) idx
  hot <- readIORef (ctxHotId ctx)
  active <- readIORef (ctxActiveId ctx)
  focus <- readIORef (ctxFocusId ctx)
  animT <- getAnimationValue ctx wid
  styleIdx <- if nt == NodeButton then getStyleIdx (ctxNodeArena ctx) idx else pure 0
  let buttonFlag flag = nt == NodeButton && hasFlag flag styleIdx
      isClose = buttonFlag buttonFlagClose
      isTab = buttonFlag buttonFlagTab
      isTable = buttonFlag buttonFlagTable
      isMenu = buttonFlag buttonFlagMenu || buttonFlag buttonFlagMenuBar
      isChoice = buttonFlag buttonFlagChoice
      isRow = buttonFlag buttonFlagRow
      -- Only these consult the floating ancestor; skip the parent walk for
      -- the common panel/text/button path.
      modalAware = isChoice || isRow || nt == NodeSlider
  mFloat <- if modalAware then floatingAncestor ctx idx else pure Nothing
  theme <- nodeTheme ctx idx
  let isFocus = focus == wid
      isHot = wid == hot
      focusBorder s = if isFocus then s {styleBorder = themeAccent theme} else s
      base =
        case nt of
          NodeTextInput -> focusBorder (themeInput theme)
          NodeTextArea -> focusBorder (themeInput theme)
          NodeSelect -> focusBorder (themeButton theme)
          NodeColorPicker -> focusBorder (themeInput theme)
          NodeSlider -> clearStyle (themeInput theme)
          NodeButton
            | isChoice -> clearStyle (themeButton theme)
            | isRow ->
                let btn = themeButton theme
                    accent = themeAccent theme
                    unselectedBg = fromMaybe (styleBg (themePanel theme)) (stripeColor theme (treeDecodeStripe styleIdx))
                    rowStyle fill hoverT activeT =
                      btn
                        { styleBg = fill
                        , styleHoverBg = lerpColor unselectedBg accent hoverT
                        , styleActiveBg = lerpColor unselectedBg accent activeT
                        , styleBorderWidth = 0
                        , styleCornerRadius = 0
                        }
                 in if val > 0.5
                      then rowStyle (lerpColor unselectedBg accent 0.25) 0.35 0.45
                      else rowStyle unselectedBg 0.12 0.22
            | isMenu -> menuItemVisualStyle theme val
            | isClose -> closeButtonStyle theme isHot animT
            | isTab -> tabHeaderVisualStyle theme (buttonVisualStyle styleIdx) (val > 0.5)
            | isTable -> tableHeaderVisualStyle theme (val > 0.5)
            | val > 0.5 ->
                (themeButton theme)
                  { styleBg = themeAccent theme
                  , styleHoverBg = themeAccent theme
                  , styleFg = themeOnAccent theme
                  , styleBorder = themeAccent theme
                  }
          _ -> themeButton theme
      widgetBase =
        case mFloat of
          Just NodeModal | modalAware -> overlayMenuStyle theme
          _ -> base
      bg
        | isFocus, nt == NodeTextInput || nt == NodeTextArea = styleActiveBg widgetBase
        | hashWidgetId wid == hashWidgetId active = styleActiveBg widgetBase
        | isChoice || isClose || nt == NodeSlider = styleBg widgetBase
        | isMenu = if isHot then styleHoverBg widgetBase else styleBg widgetBase
        | styleBg widgetBase == styleHoverBg widgetBase = styleBg widgetBase
        | otherwise = lerpColor (styleBg widgetBase) (styleHoverBg widgetBase) hotT
      hotT = if isHot && not (animT > 0) then 1 else animT
  -- Idle widgets (no hover/active tint change) reuse the base style record
  -- rather than allocating a fresh Style through a record update.
  pure $! if bg == styleBg widgetBase then widgetBase else widgetBase {styleBg = bg}

{-# INLINE fillStyledRect #-}
fillStyledRect :: DrawArena -> Style -> Rect -> IO ()
fillStyledRect da style rect =
  if styleCornerRadius style <= 0
    then pushRect da rect (styleBg style)
    else pushRoundedRect da rect (styleCornerRadius style) (styleBg style)

{-# INLINE strokeStyledRect #-}
strokeStyledRect :: DrawArena -> Style -> Rect -> IO ()
strokeStyledRect da style rect@(Rect _ _ w h) =
  when (styleBorderWidth style > 0) $ do
    let rr = clamp 0 (min (w / 2) (h / 2)) (styleCornerRadius style)
    pushRoundedStroke da rect rr (max 1 (styleBorderWidth style)) (styleBorder style)

-- | A style's fill, then its border.
{-# INLINE paintStyledRect #-}
paintStyledRect :: DrawArena -> Style -> Rect -> IO ()
paintStyledRect da style rect = do
  fillStyledRect da style rect
  strokeStyledRect da style rect

overlayMenuStyle :: Theme -> Style
overlayMenuStyle theme =
  let panel = themePanel theme
      hover =
        if styleHoverBg panel == styleBg panel
          then styleHoverBg (themeButton theme)
          else styleHoverBg panel
   in panel
        { styleCornerRadius = 2
        , styleBorderWidth = 1
        , styleHoverBg = hover
        , styleActiveBg = lerpColor (styleBg panel) (themeAccent theme) 0.22
        }

overlayWindowStyle :: Theme -> Style
overlayWindowStyle theme = (themeFloatingWindow theme) {styleCornerRadius = 2, styleBorderWidth = 1}

-- | Panel behind menus, dropdowns and floating windows: the theme's offset
-- shadow, then the styled fill and border.
paintMenuPanel :: DrawArena -> Theme -> Style -> Rect -> IO ()
paintMenuPanel da theme style rect@(Rect x y w h) = do
  when (colorA (themeShadow theme) > 0) $
    pushRoundedRect da (Rect (x + menuShadowOffset) (y + menuShadowOffset) w h) (styleCornerRadius style) (themeShadow theme)
  paintStyledRect da style rect

-- | Everything 'paintMenuPanel' can touch for a panel at @rect@: the panel,
-- its offset shadow, and a pixel of antialiasing around both.
menuPanelBounds :: Rect -> Rect
menuPanelBounds (Rect x y w h) = Rect (x - 1) (y - 1) (w + menuShadowOffset + 2) (h + menuShadowOffset + 2)

menuShadowOffset :: Float
menuShadowOffset = 3

-- | Accent marker, 2 pixels wide, at a menu row's left edge, inset 3 pixels
-- from its top and bottom.
paintMenuAccent :: DrawArena -> Theme -> Rect -> IO ()
paintMenuAccent da theme (Rect x y _ h) =
  pushRoundedRect da (Rect x (y + 3) 2 (max 0 (h - 6))) 1 (themeAccent theme)

-- | The vertical and horizontal bars of scroller @wid@ on surface @base@.
-- The one under the pointer or being dragged ('isScrollHover') paints its
-- thumb in 'scrollBarThumbHoverColor'.
paintScrollBars ::
  Context -> DrawArena -> Theme -> Style -> WidgetId -> Maybe ScrollBarLayout -> Maybe ScrollBarLayout -> IO ()
paintScrollBars ctx da theme base wid mV mH = do
  hover <- getsInteraction ctx isScrollHover
  let paint dir = mapM_ (paintScrollBarLayout da (scrollBarTrackColor base theme) (thumb dir))
      thumb dir = case hover of
        Just (hw, hd, _) | hw == wid && hd == dir -> scrollBarThumbHoverColor base theme
        _ -> scrollBarThumbColor base theme
  paint DirColumn mV
  paint DirRow mH

-- | Scrollbar track and thumb, each rounded to at most 4px.
paintScrollBarLayout :: DrawArena -> Color -> Color -> ScrollBarLayout -> IO ()
paintScrollBarLayout da trackCol thumbCol layout = do
  pill (sbTrack layout) trackCol
  pill (sbThumb layout) thumbCol
  where
    pill r@(Rect _ _ rw rh) = pushRoundedRect da r (min 4 (min rw rh / 2))

imageIdFromText :: Text -> Int
imageIdFromText txt =
  case TR.decimal txt of
    Right (n, rest) | T.null rest, n > 0 -> n
    _ -> 0
