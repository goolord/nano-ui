{-# LANGUAGE OverloadedStrings #-}

-- | The chrome a window with no frame of its own has to draw for itself: the
-- three caption buttons, the border around the whole window, and the
-- geometry that says what the rest of the title bar is for.
--
-- Doing what any of it says is the backend's, since only it holds the window
-- ('NanoUI.Sdl.Chrome.windowCaption' in @nano-ui-sdl@ draws the buttons and
-- acts on them in one call).
--
-- The glyphs are drawn rather than written: a UI font has no characters for
-- them, and axis-aligned fills at whole pixels stay crisp at any size.
module NanoUI.Widgets.Caption
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
import NanoUI.Context (Context (..), CustomDrawContext (..), scopeRawTheme)
import NanoUI.Layout.Arena (getArenaScope)
import NanoUI.Monad (Ui, styled, themed, withContext)
import NanoUI.Style
  ( Theme (..)
  , background
  , borderColor
  , borderWidth
  , cornerRadius
  , defaultLayout
  , fixedWH
  , gap
  , grow
  , padAll
  , panelStyle
  , styleFg
  , tight
  )
import NanoUI.Types (Color, Rect (..), V2 (..), lerpColor)
import NanoUI.Widgets.Custom
  ( CanvasM
  , CustomWidgetSpec (..)
  , customWidget
  , defaultCustomWidgetSpec
  , drawRect
  , drawRoundedRect
  , drawStrokeAA
  , runCanvas
  )
import NanoUI.Widgets.Layout (panelWith)
import NanoUI.Widgets.Node (Response, respClicked, respRect)

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
  -- ^ What the close button lights up in, or the theme's red when unset. It
  -- is the one hue a window's chrome spends, so an application that has a
  -- red of its own for the one button that cannot be taken back says so
  -- here.
  , capCornerRadius :: !Float
  -- ^ How far the close button's top right is rounded. That is the one
  -- corner of the three buttons that reaches a corner of the window, so it
  -- takes the window's own rounding with it; zero leaves it square.
  }

-- | 44 by 'captionBarHeight', with a ten-pixel glyph and a corner rounded by
-- eight, which is what the desktop rounds a window by.
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

-- | One caption button. It lights up under the pointer: 'GlyphClose' in the
-- theme's red, since closing is the one of the three that cannot be undone,
-- and the others a step up from the window's own colour.
--
-- The close button's lit background is rounded at the top right by
-- 'capCornerRadius', since that corner of it is a corner of the window.
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

-- | A fill whose top right is rounded and whose other three corners are
-- square: a rounded rectangle, and the two rectangles that square the rest of
-- it off again.
cornerRect :: Float -> Rect -> Color -> CanvasM ()
cornerRect radius r@(Rect x y w h) col
  | radius <= 0 || radius > w || radius > h = drawRect r col
  | otherwise = do
      drawRoundedRect r radius col
      drawRect (Rect x (y + radius) w (h - radius)) col
      drawRect (Rect x y (w - radius) radius) col

-- | The three buttons in a row, at their default size: what they were asked
-- to do, and the rectangle all three take up, which is a rectangle the
-- window cannot be dragged by. It runs from the first button's left edge to
-- the last one's right, so whatever the row puts between them is in it too.
-- Pass whether the window is maximized, which decides what the middle button
-- shows.
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
  pure (action, spanOf (respRect mini) (respRect close))
  where
    spanOf a b = Rect (rectX a) (rectY a) (rectX b + rectW b - rectX a) (max (rectH a) (rectH b))

--------------------------------------------------------------------------------
-- The window's border
--------------------------------------------------------------------------------

-- | The border a window with no frame of its own draws around itself.
data WindowFrame = WindowFrame
  { frameWidth :: !Float
  -- ^ How thick the line is, and the inset the view is drawn at, so that
  -- nothing inside paints over it. Zero draws no border at all.
  , frameRadius :: !Float
  -- ^ How far its corners are rounded. This wants to be what the desktop
  -- rounds the window by, or the line is cut off where the two part company.
  , frameColor :: !Color
  }

-- | A one-pixel border rounded by eight, which is what the desktop rounds a
-- window by.
defaultWindowFrame :: Color -> WindowFrame
defaultWindowFrame col = WindowFrame {frameWidth = 1, frameRadius = 8, frameColor = col}

-- | Draw a border around the whole window, with the view inside it.
--
-- A window with no frame of its own has no outline, and this is what tells
-- it from whatever is behind it.
--
-- A width of zero draws none, which is what a maximized window wants: its
-- edges are the screen's. The container is there either way, so the widgets
-- inside keep their ids, and their state, when the border comes and goes.
--
-- The frame's style is its own panel's and goes no further: the view inside
-- is drawn in the theme around the frame, not with the window's border on
-- every panel, card and menu in it.
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

-- | What is left of a title bar to drag the window by: the row, minus
-- everything in it that takes a click of its own (menu buttons, tabs, the
-- caption buttons). Rectangles outside the row are ignored, and only the
-- horizontal is cut, since a title bar is a row.
--
-- Hand the result to the backend as the window's drag region
-- (@setWindowChrome@ in @nano-ui-sdl@).
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

-- | The square a glyph is drawn in: centred in the button and put on whole
-- pixels, so that a one-pixel line covers one pixel.
glyphBox :: Float -> Rect -> Rect
glyphBox s (Rect x y w h) = Rect (whole (x + (w - s) / 2)) (whole (y + (h - s) / 2)) s s
  where
    whole v = fromIntegral (round v :: Int)

drawGlyph :: CaptionGlyph -> Rect -> Color -> CanvasM ()
drawGlyph glyph box@(Rect x y w h) col = case glyph of
  -- A line across the middle.
  GlyphMinimize -> drawRect (Rect x (y + whole (h / 2)) w 1) col
  -- A window: one square.
  GlyphMaximize -> strokeBox box col
  -- Two windows, one behind the other. Only the top and right edges of the
  -- one behind show past the one in front.
  GlyphRestore -> do
    drawRect (Rect (x + 2) y (w - 2) 1) col
    drawRect (Rect (x + w - 1) y 1 (h - 2)) col
    strokeBox (Rect x (y + 2) (w - 2) (h - 2)) col
  -- A cross: the one glyph that is not axis-aligned, so the one drawn with a
  -- smoothed line rather than filled pixels.
  GlyphClose -> do
    drawStrokeAA (V2 (x + 0.5) (y + 0.5)) (V2 (x + w - 0.5) (y + h - 0.5)) 1.2 col
    drawStrokeAA (V2 (x + w - 0.5) (y + 0.5)) (V2 (x + 0.5) (y + h - 0.5)) 1.2 col
  where
    whole v = fromIntegral (round v :: Int)

-- | A one-pixel outline, as four fills: the canvas's stroked rectangle is a
-- rounded one, and these are squares.
strokeBox :: Rect -> Color -> CanvasM ()
strokeBox (Rect x y w h) col = do
  drawRect (Rect x y w 1) col
  drawRect (Rect x (y + h - 1) w 1) col
  drawRect (Rect x (y + 1) 1 (h - 2)) col
  drawRect (Rect (x + w - 1) (y + 1) 1 (h - 2)) col
