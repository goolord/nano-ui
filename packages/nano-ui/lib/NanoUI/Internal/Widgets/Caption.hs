-- | The chrome a frameless window draws for itself: the three caption
-- buttons, the border around the window, and the title bar's drag regions.
-- Acting on them is the backend's ('NanoUI.Sdl.Internal.Chrome.windowCaption'
-- in @nano-ui-sdl@). The glyphs are drawn as whole-pixel fills, which stay
-- crisp at any size.
module NanoUI.Internal.Widgets.Caption
  ( -- * Buttons
    CaptionGlyph (..)
  , CaptionAction (..)
  , CaptionConfig (..)
  , defaultCaptionConfig
  , captionButton
  , captionButtons
  , captionButtonsConfigured

    -- * The window's border
  , WindowFrame (..)
  , defaultWindowFrame
  , windowFrame

    -- * Geometry
  , captionBarHeight
  , dragSpans
  ) where

import Control.Monad (when)
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Effectful (Eff, type (:>))
import NanoUI.Internal.Context (Context (..), scopeRawTheme)
import NanoUI.Internal.Layout.Arena (getArenaScope)
import NanoUI.Internal.Monad (Ui, styled, themed, withContext)
import NanoUI.Internal.Style
import NanoUI.Internal.Types (Color, Rect (..), V2 (..), lerpColor, rectUnion)
import NanoUI.Widgets.Custom
import NanoUI.Internal.Widgets.Layout (panelWith)
import NanoUI.Internal.Widgets.Node (Response, respClicked, respRect)

-- | Which button, which is also which glyph it draws.
data CaptionGlyph
  = GlyphMinimize
  | GlyphMaximize
  | GlyphRestore
  -- ^ What the middle button shows while the window fills the screen.
  | GlyphClose
  deriving (Eq, Show)

-- | What the caption buttons were asked to do this frame.
data CaptionAction
  = CaptionMinimize
  | CaptionToggleMaximize
  -- ^ Maximize a window that is restored, restore one that is maximized.
  | CaptionClose
  deriving (Eq, Show)

-- | How big the buttons are.
data CaptionConfig = CaptionConfig
  { capButtonW :: !Float
  -- ^ One button's width.
  , capButtonH :: !Float
  -- ^ One button's height, which is usually the title bar's.
  , capGlyphSize :: !Float
  -- ^ The side of the square a glyph is drawn in, centred in the button.
  , capCloseColor :: !(Maybe Color)
  -- ^ What the close button lights up in, or the theme's red when unset.
  , capCornerRadius :: !Float
  -- ^ How far the close button's top right, the window's corner, is rounded.
  }

-- | 44 by 'captionBarHeight', a ten-pixel glyph, and a corner rounded by
-- eight, as the desktop rounds a window.
defaultCaptionConfig :: CaptionConfig
defaultCaptionConfig =
  CaptionConfig
    { capButtonW = 44
    , capButtonH = captionBarHeight
    , capGlyphSize = 10
    , capCloseColor = Nothing
    , capCornerRadius = 8
    }

-- | A title bar tall enough that a few pixels along its top can resize the
-- window without the rest becoming hard to aim at.
captionBarHeight :: Float
captionBarHeight = 30

-- | One caption button. It lights up under the pointer: 'GlyphClose' in red
-- (rounded at the top right by 'capCornerRadius'), the others a step up from
-- the window's colour.
captionButton :: Ui :> es => CaptionConfig -> CaptionGlyph -> Eff es Response
captionButton cfg glyph = do
  (resp, _) <-
    customWidget
      defaultCustomWidgetSpec
        { widgetLayout = tight . fixedWH (capButtonW cfg) (capButtonH cfg) $ defaultLayout
        , widgetDraw = \cdc rect -> runCanvas $ do
            let theme = cdcTheme cdc
                lit = cdcHovered cdc || cdcPressed cdc
                fg = styleFg (themePanel theme)
            when lit $
              if glyph == GlyphClose
                then cornerRect (capCornerRadius cfg) rect (fromMaybe (themeRed theme) (capCloseColor cfg))
                else drawRect rect (lerpColor (themeWindow theme) fg 0.18)
            drawGlyph glyph (glyphBox (capGlyphSize cfg) rect) (if lit then fg else themeMuted theme)
        }
  pure resp

-- | A fill rounded only at its top right.
cornerRect :: Float -> Rect -> Color -> CanvasM ()
cornerRect radius r@(Rect x y w h) col
  | radius <= 0 || radius > w || radius > h = drawRect r col
  | otherwise = do
      drawRoundedRect r radius col
      drawRect (Rect x (y + radius) w (h - radius)) col
      drawRect (Rect x y (w - radius) radius) col

-- | The three buttons in a row: what they were asked to do, and the
-- rectangle they span, which the window cannot be dragged by. Pass whether
-- the window is maximized, which decides the middle button's glyph.
captionButtons :: Ui :> es => Bool -> Eff es (Maybe CaptionAction, Rect)
captionButtons = captionButtonsConfigured defaultCaptionConfig

-- | 'captionButtons' at a size of your own.
captionButtonsConfigured :: Ui :> es => CaptionConfig -> Bool -> Eff es (Maybe CaptionAction, Rect)
captionButtonsConfigured cfg maximized = do
  mini <- captionButton cfg GlyphMinimize
  mid <- captionButton cfg (if maximized then GlyphRestore else GlyphMaximize)
  close <- captionButton cfg GlyphClose
  let action
        | respClicked mini = Just CaptionMinimize
        | respClicked mid = Just CaptionToggleMaximize
        | respClicked close = Just CaptionClose
        | otherwise = Nothing
  pure (action, rectUnion (respRect mini) (respRect close))

--------------------------------------------------------------------------------
-- The window's border
--------------------------------------------------------------------------------

-- | The border a window with no frame of its own draws around itself.
data WindowFrame = WindowFrame
  { frameWidth :: !Float
  -- ^ The line's thickness, and the view's inset. Zero draws no border.
  , frameRadius :: !Float
  -- ^ Corner rounding, which should match the desktop's for the window.
  , frameColor :: !Color
  }

-- | A one-pixel border rounded by eight.
defaultWindowFrame :: Color -> WindowFrame
defaultWindowFrame col = WindowFrame {frameWidth = 1, frameRadius = 8, frameColor = col}

-- | Draw a border around the whole window, with the view inside it. A width
-- of zero (for a maximized window) draws none but keeps the container, so
-- the widgets inside keep their ids. The border style stays on the frame's
-- own panel; the view inside uses the surrounding theme.
windowFrame :: Ui :> es => WindowFrame -> Eff es a -> Eff es a
windowFrame frame body = do
  outer <- withContext $ \ctx -> scopeRawTheme ctx =<< getArenaScope (ctxNodeArena ctx)
  styled
    ( panelStyle
        ( background (themeWindow outer)
            . borderColor (frameColor frame)
            . borderWidth (frameWidth frame)
            . cornerRadius (frameRadius frame)
        )
    )
    $ panelWith (grow . gap 0 . padAll (frameWidth frame) . tight) (themed outer body)

--------------------------------------------------------------------------------
-- Geometry
--------------------------------------------------------------------------------

-- | What is left of a title bar row to drag the window by, minus the
-- rectangles in it that take clicks of their own (cut horizontally only).
-- Hand it to the backend as the drag region (@setWindowChrome@ in
-- @nano-ui-sdl@).
dragSpans :: Rect -> [Rect] -> [Rect]
dragSpans (Rect rx ry rw rh) taken =
  filter (\r -> rectW r > 0) (go rx (sortOn rectX (filter overlaps taken)))
  where
    right = rx + rw
    overlaps (Rect tx ty tw th) =
      tw > 0 && th > 0 && tx < right && tx + tw > rx && ty < ry + rh && ty + th > ry
    go x [] = [Rect x ry (right - x) rh]
    go x (Rect tx _ tw _ : rest) =
      Rect x ry (min right tx - x) rh : go (max x (tx + tw)) rest

--------------------------------------------------------------------------------
-- The glyphs
--------------------------------------------------------------------------------

-- | The square a glyph is drawn in, centred on whole pixels.
glyphBox :: Float -> Rect -> Rect
glyphBox s (Rect x y w h) = Rect (whole (x + (w - s) / 2)) (whole (y + (h - s) / 2)) s s

whole :: Float -> Float
whole v = fromIntegral (round v :: Int)

drawGlyph :: CaptionGlyph -> Rect -> Color -> CanvasM ()
drawGlyph glyph box@(Rect x y w h) col = case glyph of
  -- A line across the middle.
  GlyphMinimize -> drawRect (Rect x (y + whole (h / 2)) w 1) col
  -- A window: one square.
  GlyphMaximize -> strokeBox box col
  -- Two windows, the top and right edges of the one behind showing.
  GlyphRestore -> do
    drawRect (Rect (x + 2) y (w - 2) 1) col
    drawRect (Rect (x + w - 1) y 1 (h - 2)) col
    strokeBox (Rect x (y + 2) (w - 2) (h - 2)) col
  -- A cross, the one glyph drawn with smoothed lines.
  GlyphClose -> do
    drawStrokeAA (V2 (x + 0.5) (y + 0.5)) (V2 (x + w - 0.5) (y + h - 0.5)) 1.2 col
    drawStrokeAA (V2 (x + w - 0.5) (y + 0.5)) (V2 (x + 0.5) (y + h - 0.5)) 1.2 col

-- | A square one-pixel outline, as four fills.
strokeBox :: Rect -> Color -> CanvasM ()
strokeBox (Rect x y w h) col = do
  drawRect (Rect x y w 1) col
  drawRect (Rect x (y + h - 1) w 1) col
  drawRect (Rect x (y + 1) 1 (h - 2)) col
  drawRect (Rect (x + w - 1) (y + 1) 1 (h - 2)) col
