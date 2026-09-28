-- | Widget paint helpers: labels, styles, rects, menu panels and display text.
module NanoUI.Internal.Frame.Chrome
  ( floatingAncestor
  , displayText
  , widgetVisualStyle
  , textInputValue
  , maskPassword
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
  , paintTabHeader
  , paintTabChrome
  , tabHeaderVisualStyle
  , paintTableHeader
  ) where

import Control.Monad (when)
import Data.IORef (readIORef)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import NanoUI.Internal.Context
import NanoUI.Internal.Draw (DrawArena, pushRect, pushRoundedRect, pushRoundedStroke, withClip)
import NanoUI.Internal.Frame.Scroll.Geometry (ScrollBarLayout (..))
import NanoUI.Internal.Id (WidgetId, hashWidgetId)
import NanoUI.Internal.Layout.Arena
import NanoUI.Internal.Store (fieldInt, fieldText, findSlot)
import NanoUI.Internal.Style
import NanoUI.Internal.Types (Color (..), Rect (..), clamp, colorA, colorLuminance, colorRGBA, lerpColor)
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
      then maskPassword value
      else value

-- | A password field's text as shown: one @*@ per character.
maskPassword :: Text -> Text
maskPassword t = T.replicate (T.length t) "*"

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

-- | Close button style; the cross goes from muted to full colour as @hotT@
-- goes from 0 to 1.
closeButtonStyle :: Theme -> Float -> Style
closeButtonStyle theme hotT =
  let btn = themeButton theme
      muted = lerpColor (styleFg btn) (styleBg (themePanel theme)) 0.42
   in (clearStyle btn) {styleFg = lerpColor muted (styleFg btn) hotT}

-- | A tab header's surface for packed tab style @packed@ ('tabDecodeStyle').
-- Fills are the label colour at a low alpha where they can, so a strip reads
-- the same on a panel, a card or the window. The label is muted until the
-- tab is selected.
tabHeaderVisualStyle :: Theme -> Int -> Bool -> Style
tabHeaderVisualStyle theme packed isActive =
  let panel = themePanel theme
      fg = styleFg panel
      idle = lerpColor (themeMuted theme) fg 0.35
      clear = fadeAlpha fg 0
      accent = themeAccent theme
      raised = tabRaisedColor theme
      flat c = panel {styleBg = c, styleHoverBg = c, styleActiveBg = c, styleBorderWidth = 0, styleBorder = clear}
      quiet tint =
        panel
          { styleBg = fadeAlpha fg tint
          , styleHoverBg = fadeAlpha fg (tint + 16)
          , styleActiveBg = fadeAlpha fg (tint + 28)
          , styleFg = idle
          , styleBorder = clear
          , styleBorderWidth = 0
          }
      radius s r = s {styleCornerRadius = r}
   in case (fst (tabDecodeStyle packed), isActive) of
        (1, True) -> radius ((flat accent) {styleFg = themeOnAccent theme}) tabPillRadius
        (1, False) -> radius (quiet 0) tabPillRadius
        (2, True) -> radius ((flat raised) {styleBorder = styleBorder (themeButton theme), styleBorderWidth = 1}) 6
        (2, False) -> radius (quiet 0) 6
        (3, True) -> radius ((flat (styleBg panel)) {styleBorder = styleBorder panel, styleBorderWidth = 1}) 7
        (3, False) -> radius (quiet 12) 7
        (_, True) -> radius ((quiet 0) {styleFg = fg}) 6
        _ -> radius (quiet 0) 6

-- | Large enough to round a pill header's ends fully; painters clamp it.
tabPillRadius :: Float
tabPillRadius = 100

-- | A selected segment's fill: whichever of the button and panel colours is
-- lighter, so it stands out of its track in light and dark themes alike.
tabRaisedColor :: Theme -> Color
tabRaisedColor theme =
  let b = styleBg (themeButton theme)
      p = styleBg (themePanel theme)
   in if colorLuminance b >= colorLuminance p then b else p

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

-- | A tab header's fill, outline and selection mark. Underlined and
-- contained headers meet the strip's rule on the side facing the body
-- ('tabEdgeStrip'): an underlined header marks the selection with an accent bar
-- there, and a contained one opens onto the body through it.
paintTabHeader :: DrawArena -> Theme -> Int -> Bool -> Style -> Rect -> IO ()
paintTabHeader da theme packed isActive style rect@(Rect x y w h) = do
  let (tabStyle, orient) = tabDecodeStyle packed
      bg = styleBg style
      r = min (styleCornerRadius style) (min w h / 2)
      hasBg = colorA bg > 0
  case tabStyle of
    1 -> when hasBg $ pushRoundedRect da rect r bg
    2 -> do
      -- A selected segment stands a pixel above its shadow, inside its rect.
      let shadow = themeShadow theme
          raised = if isActive && colorA shadow > 0 then Rect x y w (h - 1) else rect
      when (raised /= rect) $
        pushRoundedRect da rect r (fadeAlpha shadow (colorA shadow `div` 2))
      when hasBg $ pushRoundedRect da raised r bg
      when isActive $ strokeStyledRect da style raised
    3 ->
      -- Rounded on the outer corners only: the shape runs a radius past the
      -- edge facing the body, and the header's rect clips that side off.
      withClip da rect $ do
        let open = tabEdgeGrow orient (r + 1) rect
        when hasBg $ pushRoundedRect da open r bg
        when isActive $ pushRoundedStroke da open r 1 (styleBorder style)
    _ -> do
      -- The hover fill stops short of the rule and the selection bar.
      when hasBg $ pushRoundedRect da (tabEdgeGrow orient (-3) rect) r bg
      when isActive $ pushRoundedRect da (tabEdgeStrip orient 2 rect) 1 (themeAccent theme)

-- | What a tab strip's container paints under its headers or body
-- ('tabChromeDecode'): the rule the headers sit on, the segmented track, or a
-- contained body's fill and its border on the sides away from the headers.
{-# NOINLINE paintTabChrome #-}
paintTabChrome :: DrawArena -> Theme -> Int -> Rect -> IO ()
paintTabChrome da theme si rect@(Rect _ _ w h) = do
  let (part, tabStyle, orient) = tabChromeDecode si
      panel = themePanel theme
      fg = styleFg panel
  case part of
    TabChromeRule ->
      pushRect da (tabEdgeStrip orient 1 rect) (if tabStyle == 3 then styleBorder panel else themeSeparator theme)
    TabChromeTrack -> pushRoundedRect da rect (min 9 (min w h / 2)) (fadeAlpha fg 16)
    TabChromeBody -> withClip da rect $ do
      let open = tabEdgeGrow (tabEdgeOpposite orient) 9 rect
      pushRoundedRect da open 8 (styleBg panel)
      pushRoundedStroke da open 8 1 (styleBorder panel)
    TabChromeNone -> pure ()

-- | @rect@ grown by @d@ on the side facing the body of a strip with
-- orientation @orient@ (0-3: top, bottom, left, right), or shrunk for a
-- negative @d@.
tabEdgeGrow :: Int -> Float -> Rect -> Rect
tabEdgeGrow orient d (Rect x y w h) = case orient of
  1 -> Rect x (y - d) w (h + d)
  2 -> Rect x y (w + d) h
  3 -> Rect (x - d) y (w + d) h
  _ -> Rect x y w (h + d)

-- | The @t@ thick strip of @rect@ along the side facing the body.
tabEdgeStrip :: Int -> Float -> Rect -> Rect
tabEdgeStrip orient t (Rect x y w h) = case orient of
  1 -> Rect x y w t
  2 -> Rect (x + w - t) y t h
  3 -> Rect x y t h
  _ -> Rect x (y + h - t) w t

-- | The orientation whose body side is @orient@'s strip side: a body below
-- its headers opens upward.
tabEdgeOpposite :: Int -> Int
tabEdgeOpposite orient = case orient of
  0 -> 1
  1 -> 0
  2 -> 3
  _ -> 2

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
            | isClose -> closeButtonStyle theme hotT
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
strokeStyledRect da style rect@(Rect x y w h) =
  when (styleBorderWidth style > 0) $
    if styleBorderSides style == 15
      then do
        let rr = clamp 0 (min (w / 2) (h / 2)) (styleCornerRadius style)
        pushRoundedStroke da rect rr bw (styleBorder style)
      else do
        let side s r = when (styleHasSide s style) (pushRect da r (styleBorder style))
        side SideLeft (Rect x y bw h)
        side SideRight (Rect (x + w - bw) y bw h)
        side SideTop (Rect x y w bw)
        side SideBottom (Rect x (y + h - bw) w bw)
  where
    bw = max 1 (styleBorderWidth style)

-- | A style's fill, then its border.
{-# INLINE paintStyledRect #-}
paintStyledRect :: DrawArena -> Style -> Rect -> IO ()
paintStyledRect da style rect = do
  fillStyledRect da style rect
  strokeStyledRect da style rect

-- | Popups, modals, dropdowns and menus: 'themePopup', with a row hover
-- that shows on it and a pressed row tinted toward the accent.
overlayMenuStyle :: Theme -> Style
overlayMenuStyle theme =
  let popup = themePopup theme
      hover =
        if styleHoverBg popup == styleBg popup
          then styleHoverBg (themeButton theme)
          else styleHoverBg popup
   in popup
        { styleHoverBg = hover
        , styleActiveBg = lerpColor (styleBg popup) (themeAccent theme) 0.22
        }

-- | Floating windows: 'themeFloatingWindow'.
overlayWindowStyle :: Theme -> Style
overlayWindowStyle = themeFloatingWindow

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

