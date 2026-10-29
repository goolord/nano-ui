-- | Code for the book's Custom Drawing chapter
-- (@book/src/widgets/custom-drawing.md@). The canvas examples with pictures
-- are in @nano-ui-svg/app/CanvasExamples.hs@.
module Book.CustomDrawing where

import Data.Foldable (for_)
import Data.Primitive.SmallArray (SmallArray)
import Data.Text qualified as T
import NanoUI
import NanoUI.Svg (rasterizeSvg)

-- ANCHOR: with_forms
controls :: Var Float -> Float -> [Float] -> NanoUI ()
controls volume progress samples = do
  knobWith alignMid 48 0 100 volume
  progressBarWith fillW 6 progress
  sparklineWith (fixedWH 120 32) 120 32 samples
-- ANCHOR_END: with_forms

-- ANCHOR: settings_row
settingsRow :: Var Bool -> Var Float -> NanoUI ()
settingsRow enabled volume = rowWith (gap 12 . alignMid) $ do
  toggleSwitch enabled
  labelWith (alignMid . fixedW 64) "Enabled"
  knob 0 100 volume
  v <- readVar volume
  labelWith (alignMid . fontMono . fixedW 48) (T.pack (show (round v :: Int)))
-- ANCHOR_END: settings_row

-- ANCHOR: custom_widget
dot :: Float -> Float -> NanoUI Response
dot angle volume = do
  (resp, ()) <- customWidget defaultCustomWidgetSpec
    { widgetLayout = fixedWH 48 48 defaultLayout
    , widgetContent = contentKey [angle, volume]
    , widgetDraw = \cdc (Rect x y w h) -> runCanvasFor cdc $ do
        drawCircle (V2 (x + w / 2) (y + h / 2)) 16 (themeAccent (cdcTheme cdc))
    , widgetCursor = Just (\_ _ _ -> UiCursorPointer)
    }
  pure resp
-- ANCHOR_END: custom_widget

-- ANCHOR: content_key
gauge :: Float -> Float -> Float -> NanoUI Response
gauge angle volume scrollY = do
  (resp, ()) <- customWidget defaultCustomWidgetSpec
    { widgetLayout = fixedWH 48 48 defaultLayout
    , widgetContent = contentKey [angle, volume, fromIntegral (round scrollY :: Int)]
    , widgetDraw = \cdc rect -> runCanvasFor cdc (drawGauge rect)
    }
  pure resp
-- ANCHOR_END: content_key

-- ANCHOR: versioned
-- ghostOps :: Rect -> SmallArray DrawOp
dragGhost :: Float -> Float -> NanoUI Response
dragGhost mx my = drawingVersioned (contentKey [mx, my]) (fixedWH 32 32) ghostOps
-- ANCHOR_END: versioned

-- ANCHOR: svg_icons
icons :: Svg -> Color -> NanoUI ()
icons star accent = do
  svgIcon 24 star
  svgIconWith (fixedWH 32 32 . fontColor accent) star
  whenM (iconButton star "Star") starItem
-- ANCHOR_END: svg_icons

-- ANCHOR: svg_canvas
starBadge :: ImageHandle Color -> Svg -> Color -> NanoUI Response
starBadge owner star tint = do
  iid <- useImageRgba owner tint 32 32 (rasterizeSvg 32 32 tint star)
  canvas (fixedWH 140 44) $ \(Rect x y _ _) ->
    for_ iid $ \i -> drawImage (Rect (x + 8) (y + 6) 32 32) i colorWhite
-- ANCHOR_END: svg_canvas

-- Stand-ins for application code the snippets refer to.

drawGauge :: Rect -> CanvasM ()
drawGauge _ = pure ()

ghostOps :: Rect -> SmallArray DrawOp
ghostOps _ = mempty

starItem :: NanoUI ()
starItem = pure ()
