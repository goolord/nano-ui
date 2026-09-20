{-# LANGUAGE DataKinds #-}

-- | Widget paint helpers: labels, styles, rects, menu panels and display text.
module NanoUI.Frame.Chrome
  ( floatingAncestor
  , displayText
  , widgetVisualStyle
  , textInputValue
  , textInputFocused
  , fillStyledRect
  , strokeStyledRect
  , paintStyledRect
  , overlayWindowStyle
  , overlayModalStyle
  , overlayMenuStyle
  , paintMenuPanel
  , paintMenuAccent
  , paintScrollBarLayout
  , imageIdFromText
  , paintTabHeader
  , paintTableHeader
  ) where

import Control.Monad (when)
import Data.IORef (readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import NanoUI.Context
  ( Context (..)
  , getAnimationValue
  , getStore
  , intKey
  , nodeTheme
  )
import NanoUI.Draw (DrawArena, pushRect, pushRoundedRect, pushRoundedStroke)
import NanoUI.Font (menuAccentInset, menuAccentW)
import NanoUI.Frame.Scroll.Geometry (ScrollBarLayout (..))
import NanoUI.Id (hashWidgetId)
import NanoUI.Layout.Arena
  ( NodeIdx
  , NodeType (..)
  , getNodeType
  , getNodeValue
  , getOptions
  , getStyleIdx
  , getText
  , getWidgetId
  , isFloatingNode
  , walkAncestors
  )
import NanoUI.Store (fieldInt, fieldText, findSlot)
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
  , themeOnAccent
  , themeShadow
  )
import NanoUI.Types (Color (..), Rect (..), colorA, colorRGBA, lerpColor)
import NanoUI.WidgetText
  ( buttonFlagsFromStyle
  , buttonVisualStyle
  , isMenuBarStyle
  , isMenuItemStyle
  , isTableHeaderStyle
  , selectDisplayText
  , stripeColor
  , tableHeaderDisplayText
  , textInputFieldText
  , textInputPasswordMode
  , treeDecodeStripe
  )

floatingAncestor :: Context -> NodeIdx -> IO (Maybe NodeType)
floatingAncestor ctx idx =
  walkAncestors (ctxNodeArena ctx) idx $ \i -> do
    nt <- getNodeType (ctxNodeArena ctx) i
    pure (if isFloatingNode nt then Just nt else Nothing)

displayText :: Context -> NodeType -> NodeIdx -> IO Text
displayText ctx nt idx = do
  txt <- getText (ctxNodeArena ctx) idx
  case nt of
    NodeButton -> do
      si <- getStyleIdx (ctxNodeArena ctx) idx
      pure $! if isTableHeaderStyle si then tableHeaderDisplayText txt else txt
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
  pure $ case drop picked opts of
    (o : _) -> o
    _ -> ""

-- | The text a field displays: its stored value, masked one character per
-- character for password inputs so caret and selection offsets still line up.
textInputValue :: Context -> NodeIdx -> IO Text
textInputValue ctx idx = do
  let na = ctxNodeArena ctx
  wid <- getWidgetId na idx
  nt <- getNodeType na idx
  si <- getStyleIdx na idx
  store <- getStore ctx
  let value = findSlot fieldText "" (intKey wid) store
  pure $
    if nt == NodeTextInput && textInputPasswordMode si
      then T.replicate (T.length value) "*"
      else value

textInputFocused :: Context -> NodeIdx -> IO Bool
textInputFocused ctx idx = do
  wid <- getWidgetId (ctxNodeArena ctx) idx
  focus <- readIORef (ctxFocusId ctx)
  pure (focus == wid)

-- | Transparent fills and no border.
clearStyle :: Style -> Style
clearStyle s = s {styleBg = clear, styleHoverBg = clear, styleActiveBg = clear, styleBorderWidth = 0}
  where
    clear = colorRGBA 0 0 0 0

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
      clear = colorRGBA 0 0 0 0
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
          { styleBg = clear
          , styleHoverBg = hoverLift
          , styleFg = inactFg
          , styleBorder = clear
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
      clear = colorRGBA 0 0 0 0
      openBg = lerpColor (styleBg menu) accent 0.3
      isOpen = val > 0.5
   in menu
        { styleBg = if isOpen then openBg else clear
        , styleHoverBg = if isOpen then openBg else styleHoverBg menu
        , styleActiveBg = lerpColor (styleBg menu) accent 0.4
        , styleBorder = clear
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
paintTabHeader da theme styleIdx isActive style x y w h = do
  let rect = Rect x y w h
      r = max 0 (styleCornerRadius style)
      bg = styleBg style
  if isActive
    then case styleIdx `mod` 4 of
      1 -> pushRoundedRect da rect r bg
      2 -> do
        pushRoundedRect da rect r bg
        strokeStyledRect da style rect
      _ -> do
        pushRoundedRect da rect r bg
        pushRoundedStroke da (Rect x y w (h + 1)) (min r (min (w / 2) (h / 2))) 1 (styleBorder (themePanel theme))
        pushRect da (Rect x (y + h - 2) w 2) (themeAccent theme)
    else when (bg /= colorRGBA 0 0 0 0) $ pushRoundedRect da rect r bg

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
  -- Only these node types consult the floating ancestor; skip the parent
  -- walk for the common panel/text/button path.
  let modalAware = nt == NodeCheckbox || nt == NodeRadio || nt == NodeTree || nt == NodeSlider
  mFloat <- if modalAware then floatingAncestor ctx idx else pure Nothing
  styleIdx <-
    if nt == NodeButton || nt == NodeTree
      then getStyleIdx (ctxNodeArena ctx) idx
      else pure 0
  let (isClose, isTab, isTable) =
        if nt == NodeButton
          then buttonFlagsFromStyle styleIdx
          else (False, False, False)
      isMenu = nt == NodeButton && (isMenuItemStyle styleIdx || isMenuBarStyle styleIdx)
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
          NodeCheckbox -> clearStyle (themeButton theme)
          NodeRadio -> clearStyle (themeButton theme)
          NodeTree ->
            let btn = themeButton theme
                accent = themeAccent theme
                unselectedBg =
                  case stripeColor theme (treeDecodeStripe styleIdx) of
                    Just c -> c
                    Nothing -> styleBg (themePanel theme)
             in if val > 0.5
                  then
                    btn
                      { styleBg = lerpColor unselectedBg accent 0.25
                      , styleHoverBg = lerpColor unselectedBg accent 0.35
                      , styleActiveBg = lerpColor unselectedBg accent 0.45
                      , styleBorderWidth = 0
                      , styleCornerRadius = 0
                      }
                  else
                    btn
                      { styleBg = unselectedBg
                      , styleHoverBg = lerpColor unselectedBg accent 0.12
                      , styleActiveBg = lerpColor unselectedBg accent 0.22
                      , styleBorderWidth = 0
                      , styleCornerRadius = 0
                      }
          NodeButton
            | isMenu -> menuItemVisualStyle theme val
            | isClose -> closeButtonStyle theme isHot animT
            | isTab -> tabHeaderVisualStyle theme (buttonVisualStyle styleIdx `mod` 4) (val > 0.5)
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
          Just NodeModal | modalAware -> overlayModalStyle theme
          _ -> base
      bg
        | nt == NodeTextInput, isFocus = styleActiveBg widgetBase
        | nt == NodeTextArea, isFocus = styleActiveBg widgetBase
        | hashWidgetId wid == hashWidgetId active = styleActiveBg widgetBase
        | nt == NodeCheckbox || nt == NodeRadio || nt == NodeSlider || isClose = styleBg widgetBase
        | isMenu = if isHot then styleHoverBg widgetBase else styleBg widgetBase
        | otherwise = hoverBackground widgetBase animT isHot
  -- Idle widgets (no hover/active tint change) reuse the base style record
  -- rather than allocating a fresh Style through a record update.
  pure $! if bg == styleBg widgetBase then widgetBase else widgetBase {styleBg = bg}

hoverBackground :: Style -> Float -> Bool -> Color
hoverBackground base val isHot
  | styleBg base == styleHoverBg base = styleBg base
  | isHot = lerpColor (styleBg base) (styleHoverBg base) (if val > 0 then val else 1)
  | otherwise = lerpColor (styleBg base) (styleHoverBg base) val

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
    let rr = max 0 (min (styleCornerRadius style) (min (w / 2) (h / 2)))
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

overlayModalStyle :: Theme -> Style
overlayModalStyle theme = (overlayMenuStyle theme) {styleCornerRadius = 2, styleBorderWidth = 1}

-- | Panel behind menus, dropdowns and floating windows: the theme's offset
-- shadow, then the styled fill and border.
paintMenuPanel :: DrawArena -> Theme -> Style -> Rect -> IO ()
paintMenuPanel da theme style rect@(Rect x y w h) = do
  when (colorA (themeShadow theme) > 0) $
    pushRoundedRect da (Rect (x + 3) (y + 3) w h) (styleCornerRadius style) (themeShadow theme)
  paintStyledRect da style rect

-- | Accent marker at a menu row's left edge, inset from its top and bottom.
paintMenuAccent :: DrawArena -> Theme -> Rect -> IO ()
paintMenuAccent da theme (Rect x y _ h) =
  pushRoundedRect
    da
    (Rect x (y + menuAccentInset) menuAccentW (max 0 (h - 2 * menuAccentInset)))
    1
    (themeAccent theme)

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
