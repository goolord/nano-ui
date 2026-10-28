-- | Canvas examples for the book's Custom Drawing chapter. Each anchored
-- block is included in @book/src/widgets/custom-drawing.md@, and the glossary
-- generator renders the same definitions to @book/src/images/canvas/@, so the
-- code on the page is the code that drew the picture beside it.
--
-- Everything outside the anchors is annotation: the captioned cells, and the
-- thin guides (path points, the bounds a stroke is measured against, a
-- transform's starting shape) drawn over the examples.
module CanvasExamples (canvasExamples) where

import Control.Monad (void)
import Data.Foldable (for_)
import Data.Text (Text)
import NanoUI
import NanoUI.Internal.Path (Path (..), Segment (..))
import NanoUI.Path qualified as P

-- ANCHOR: palette
ink, blue, teal, amber, rose :: Color
ink = colorRGB 226 232 240
blue = colorRGB 59 130 246
teal = colorRGB 20 184 166
amber = colorRGB 245 158 11
rose = colorRGB 244 63 94

centre :: Rect -> V2
centre (Rect x y w h) = V2 (x + w / 2) (y + h / 2)
-- ANCHOR_END: palette

-- ANCHOR: badge
badge :: NanoUI ()
badge = void $ canvas (fixedWH 120 40) $ \r@(Rect x y _ h) -> do
  drawRoundedRect r 8 blue
  drawStrokePath (P.circle (V2 (x + 20) (y + h / 2)) 8) 2 colorWhite
  drawText (V2 (x + 68) (y + h / 2)) AlignCenter AlignMiddle "Ready" colorWhite
-- ANCHOR_END: badge

-- ANCHOR: primitives
primitives :: [(Text, Rect -> CanvasM ())]
primitives =
  [ ("drawRect", \r -> drawRect r blue)
  , ("drawRoundedRect", \r -> drawRoundedRect r 14 teal)
  , ("drawCircle", \r -> drawCircle (centre r) 28 amber)
  , ("drawStrokeCircle", \r -> drawStrokeCircle (centre r) 28 4 rose)
  , ("drawStrokeAA", \(Rect x y w h) -> drawStrokeAA (V2 x (y + h)) (V2 (x + w) y) 4 ink)
  , ("drawLinearGradientH", \r -> drawLinearGradientH r blue teal)
  , ("drawLinearGradientV", \r -> drawLinearGradientV r amber rose)
  , ("drawQuadGradient", \r -> drawQuadGradient r blue teal amber rose)
  ]
-- ANCHOR_END: primitives

-- ANCHOR: segments
segments :: [(Text, Rect -> P.Path)]
segments =
  [ ("lineTo", \(Rect x y w h) ->
      P.moveTo (V2 x (y + h)) <> P.lineTo (V2 (x + w / 2) y) <> P.lineTo (V2 (x + w) (y + h)))
  , ("quadTo", \(Rect x y w h) ->
      P.moveTo (V2 x (y + h)) <> P.quadTo (V2 (x + w / 2) (y - 12)) (V2 (x + w) (y + h)))
  , ("cubicTo", \(Rect x y w h) ->
      P.moveTo (V2 x (y + h)) <> P.cubicTo (V2 x y) (V2 (x + w) (y + h)) (V2 (x + w) y))
  , ("arc", \r -> P.arc (centre r) 28 0 (3 * pi / 2))
  , ("moveTo, arc, close", \r -> P.moveTo (centre r) <> P.arc (centre r) 28 0 (3 * pi / 2) <> P.close)
  ]
-- ANCHOR_END: segments

-- ANCHOR: shapes
shapes :: [(Text, Rect -> CanvasM ())]
shapes =
  [ ("rect", \r -> shape (P.rect r))
  , ("roundedRect", \r -> shape (P.roundedRect r 12))
  , ("roundedRectCorners", \r -> shape (P.roundedRectCorners r 24 4 24 4))
  , ("circle", \r -> shape (P.circle (centre r) 28))
  , ("ellipse", \r -> shape (P.ellipse (centre r) (V2 48 22)))
  , ("polygon", \(Rect x y w h) -> shape (P.polygon [V2 (x + w / 2) y, V2 (x + w) (y + h), V2 x (y + h)]))
  , ("polyline", \(Rect x y w h) ->
      drawStrokePath (P.polyline [V2 x (y + h), V2 (x + w / 3) y, V2 (x + 2 * w / 3) (y + h), V2 (x + w) y]) 2 blue)
  ]
  where
    shape p = drawPath p (withAlpha blue 0.35) >> drawStrokePath p 2 blue
-- ANCHOR_END: shapes

-- ANCHOR: fill_rules
fillRules :: [(Text, Rect -> CanvasM ())]
fillRules =
  [ ("NonZero", \r -> drawPathWith P.NonZero (rings r) (P.Solid teal))
  , ("NonZero, middle reversed", \r -> drawPathWith P.NonZero (reversedMiddle r) (P.Solid teal))
  , ("EvenOdd", \r -> drawPathWith P.EvenOdd (rings r) (P.Solid teal))
  ]
  where
    rings r = P.circle (centre r) 30 <> P.circle (centre r) 20 <> P.circle (centre r) 9
    -- A negative sweep runs the middle circle anticlockwise.
    reversedMiddle r@(Rect x y w h) =
      P.circle (centre r) 30
        <> P.moveTo (V2 (x + w / 2 + 20) (y + h / 2))
        <> P.arc (centre r) 20 0 (-2 * pi)
        <> P.close
        <> P.circle (centre r) 9
-- ANCHOR_END: fill_rules

-- ANCHOR: paints
paints :: [(Text, Rect -> CanvasM ())]
paints =
  [ ("Solid", \r -> drawPathWith P.NonZero (P.roundedRect r 10) (P.Solid blue))
  , ("Linear, two stops", \r@(Rect x y w _) ->
      drawPathWith P.NonZero (P.roundedRect r 10) (P.Linear (V2 x y) (V2 (x + w) y) [(0, blue), (1, teal)]))
  , ("Linear, short span", \r@(Rect x y w _) ->
      drawPathWith P.NonZero (P.roundedRect r 10) (P.Linear (V2 (x + w / 3) y) (V2 (x + 2 * w / 3) y) [(0, blue), (1, teal)]))
  , ("Linear, three stops", \r@(Rect x y w h) ->
      drawPathWith P.NonZero (P.circle (centre r) 30) (P.Linear (V2 x y) (V2 (x + w) (y + h)) [(0, amber), (0.5, rose), (1, blue)]))
  ]
-- ANCHOR_END: paints

-- ANCHOR: caps
capLine :: Rect -> P.Path
capLine (Rect x y w h) = P.moveTo (V2 (x + 16) (y + h / 2)) <> P.lineTo (V2 (x + w - 16) (y + h / 2))

caps :: [(Text, Rect -> CanvasM ())]
caps =
  [ (name, \r -> drawStrokePathWith (P.stroke 18) {P.strokeCap = cap} (capLine r) (P.Solid blue))
  | (name, cap) <- [("ButtCap", P.ButtCap), ("RoundCap", P.RoundCap), ("SquareCap", P.SquareCap)]
  ]
-- ANCHOR_END: caps

-- ANCHOR: joins
zigzag :: Rect -> P.Path
zigzag (Rect x y w h) = P.polyline [V2 (x + w / 4) (y + h - 4), V2 (x + w / 2) (y + 10), V2 (x + 3 * w / 4) (y + h - 4)]

joins :: [(Text, Rect -> CanvasM ())]
joins =
  [ ("MiterJoin", withJoin (P.stroke 14) {P.strokeJoin = P.MiterJoin})
  , ("RoundJoin", withJoin (P.stroke 14) {P.strokeJoin = P.RoundJoin})
  , ("BevelJoin", withJoin (P.stroke 14) {P.strokeJoin = P.BevelJoin})
  , ("strokeMiterLimit 1.5", withJoin (P.stroke 14) {P.strokeJoin = P.MiterJoin, P.strokeMiterLimit = 1.5})
  ]
  where
    withJoin st r = drawStrokePathWith st (zigzag r) (P.Solid teal)
-- ANCHOR_END: joins

-- ANCHOR: dashes
dashes :: [(Text, Rect -> CanvasM ())]
dashes =
  [ ("[12, 6]", dashed (P.stroke 4) {P.strokeDash = [12, 6]})
  , ("[12, 6], offset 9", dashed (P.stroke 4) {P.strokeDash = [12, 6], P.strokeDashOffset = 9})
  , ("[18, 5, 3, 5]", dashed (P.stroke 4) {P.strokeDash = [18, 5, 3, 5]})
  , ("[0, 10], RoundCap", dashed (P.stroke 6) {P.strokeDash = [0, 10], P.strokeCap = P.RoundCap})
  ]
  where
    dashed st r = drawStrokePathWith st (P.roundedRect r 12) (P.Solid amber)
-- ANCHOR_END: dashes

-- ANCHOR: transforms
arrowPath :: Rect -> P.Path
arrowPath r =
  let V2 cx cy = centre r
   in P.polygon [V2 (cx - 24) (cy - 6), V2 cx (cy - 6), V2 cx (cy - 16), V2 (cx + 24) cy, V2 cx (cy + 16), V2 cx (cy + 6), V2 (cx - 24) (cy + 6)]

arrow :: Rect -> CanvasM ()
arrow r = drawPath (arrowPath r) teal >> drawStrokePath (arrowPath r) 2 ink

-- | Scale about a point: move it to the origin, scale, move it back.
scaleAround :: V2 -> Float -> Float -> P.Transform
scaleAround (V2 cx cy) sx sy = P.translate cx cy <> P.scale sx sy <> P.translate (-cx) (-cy)

transforms :: [(Text, Rect -> CanvasM ())]
transforms =
  [ ("translate 20 10", \r -> withTransform (P.translate 20 10) (arrow r))
  , ("rotateAround c (pi / 4)", \r -> withTransform (P.rotateAround (centre r) (pi / 4)) (arrow r))
  , ("scaleAround c 1.5 1.5", \r -> withTransform (scaleAround (centre r) 1.5 1.5) (arrow r))
  , ("scaleAround c (-1) 1", \r -> withTransform (scaleAround (centre r) (-1) 1) (arrow r))
  ]
-- ANCHOR_END: transforms

-- ANCHOR: clip
clipped :: Rect -> CanvasM ()
clipped r@(Rect x y _ h) = do
  drawCircle (centre r) 34 rose
  drawText (V2 (x - 6) (y + h / 2)) AlignStart AlignMiddle "text past the edge" ink

clipBox :: Rect -> Rect
clipBox (Rect x y w h) = Rect (x + 10) (y + 8) (w - 20) (h - 16)

clips :: [(Text, Rect -> CanvasM ())]
clips =
  [ ("no clip", clipped)
  , ("withClip box", \r -> withClip (clipBox r) (clipped r))
  ]
-- ANCHOR_END: clip

-- ANCHOR: text
textAnchors :: [(Text, Rect -> CanvasM ())]
textAnchors =
  [ (name, \r -> drawText (centre r) ax AlignMiddle "Anchor" ink)
  | (name, ax) <- [("AlignStart", AlignStart), ("AlignCenter", AlignCenter), ("AlignEnd", AlignEnd)]
  ]
    <> [ (name, \r -> drawText (centre r) AlignCenter ay "Anchor" ink)
       | (name, ay) <- [("AlignTop", AlignTop), ("AlignMiddle", AlignMiddle), ("AlignBaseline", AlignBaseline), ("AlignBottom", AlignBottom)]
       ]
    <> [ ("drawTextWith", \r ->
          drawTextWith defaultTextFont {textFontSize = 24, textFontWeight = WeightBold} (centre r) AlignCenter AlignMiddle "42" amber)
       ]
-- ANCHOR_END: text

-- ANCHOR: borders
borders :: [(Text, Rect -> CanvasM ())]
borders =
  [ ("drawStrokeRoundedRect", \r -> drawStrokeRoundedRect r 12 8 (withAlpha blue 0.8))
  , ("drawStrokePath", \r -> drawStrokePath (P.roundedRect r 12) 8 (withAlpha blue 0.8))
  ]
-- ANCHOR_END: borders

-- ---------------------------------------------------------------------------
-- Annotation
-- ---------------------------------------------------------------------------

guideInk :: Color
guideInk = colorRGBA 148 163 184 200

guideStroke :: P.Path -> CanvasM ()
guideStroke p = drawStrokePath p 1 guideInk

dashedGuide :: P.Path -> CanvasM ()
dashedGuide p = drawStrokePathWith (P.stroke 1) {P.strokeDash = [3, 3]} p (P.Solid guideInk)

dot :: V2 -> CanvasM ()
dot p = drawCircle p 2.5 ink

hollowDot :: V2 -> CanvasM ()
hollowDot p = drawStrokeCircle p 3.5 1.5 guideInk

-- | The path's points: filled dots on the curve, hollow dots and thin arms
-- for control points, and the centre of an arc.
pathPoints :: P.Path -> CanvasM ()
pathPoints (Path segs) = go (V2 0 0) segs
  where
    go _ [] = pure ()
    go cur (s : rest) = case s of
      SegMove x y -> dot (V2 x y) >> go (V2 x y) rest
      SegLine x y -> dot (V2 x y) >> go (V2 x y) rest
      SegQuad qx qy x y -> do
        guideStroke (P.polyline [cur, V2 qx qy, V2 x y])
        hollowDot (V2 qx qy)
        dot (V2 x y)
        go (V2 x y) rest
      SegCubic ax ay bx by x y -> do
        guideStroke (P.polyline [cur, V2 ax ay])
        guideStroke (P.polyline [V2 bx by, V2 x y])
        hollowDot (V2 ax ay)
        hollowDot (V2 bx by)
        dot (V2 x y)
        go (V2 x y) rest
      SegArc cx cy rx _ _ a0 sweep -> do
        let start = V2 (cx + rx * cos a0) (cy + rx * sin a0)
            end = V2 (cx + rx * cos (a0 + sweep)) (cy + rx * sin (a0 + sweep))
        dashedGuide (P.polyline [start, V2 cx cy, end])
        hollowDot (V2 cx cy)
        dot start
        dot end
        go end rest
      _ -> go cur rest

-- | One canvas holding a grid of captioned cells. Each cell's drawing area
-- is passed to its example.
cells :: Int -> Float -> Float -> [(Text, Rect -> CanvasM ())] -> NanoUI ()
cells cols cw ch examples =
  columnWith (padAll 12) $
    void $ canvas (fixedWH (fromIntegral cols * cw) (fromIntegral rows * ch)) $ \(Rect x0 y0 _ _) ->
      for_ (zip [0 :: Int ..] examples) $ \(i, (caption, draw)) -> do
        let (rowIx, colIx) = i `divMod` cols
            cx = x0 + fromIntegral colIx * cw
            cy = y0 + fromIntegral rowIx * ch
            area = Rect (cx + 26) (cy + 18) (cw - 52) (ch - 56)
        drawRoundedRect (Rect (cx + 4) (cy + 4) (cw - 8) (ch - 8)) 8 (colorRGBA 255 255 255 10)
        draw area
        drawTextWith
          defaultTextFont {textFontSize = 12, textFontVariant = FontMono}
          (V2 (cx + cw / 2) (cy + ch - 18))
          AlignCenter
          AlignMiddle
          caption
          guideInk
  where
    rows = (length examples + cols - 1) `div` cols

-- | Add an annotation over every example in a list.
annotate :: (Rect -> CanvasM ()) -> [(Text, Rect -> CanvasM ())] -> [(Text, Rect -> CanvasM ())]
annotate guide = map (\(caption, draw) -> (caption, \r -> draw r >> guide r))

crosshair :: V2 -> CanvasM ()
crosshair (V2 x y) = do
  guideStroke (P.moveTo (V2 (x - 40) y) <> P.lineTo (V2 (x + 40) y))
  guideStroke (P.moveTo (V2 x (y - 16)) <> P.lineTo (V2 x (y + 16)))
  drawCircle (V2 x y) 2.5 rose

-- | A grid example as (file name, canvas width, view): 'cells' and its
-- padding.
cellGrid :: Text -> Int -> Float -> Float -> [(Text, Rect -> CanvasM ())] -> (Text, Float, NanoUI ())
cellGrid name cols cw ch examples = (name, fromIntegral cols * cw + 24, cells cols cw ch examples)

-- | Every example as (file name, canvas width, view). The width covers the
-- padded canvas; the generator crops the height to what was drawn.
canvasExamples :: [(Text, Float, NanoUI ())]
canvasExamples =
  [ ("badge", 144, columnWith (padAll 12) badge)
  , cellGrid "primitives" 4 160 120 primitives
  , cellGrid "segments" 5 150 130 [(c, \r -> drawStrokePath (mk r) 3 blue >> pathPoints (mk r)) | (c, mk) <- segments]
  , cellGrid "shapes" 4 160 120 shapes
  , cellGrid "fill-rules" 3 200 130 fillRules
  , cellGrid "paints" 4 160 120 paints
  , cellGrid "caps" 3 180 100 (annotate (\r -> guideStroke (capLine r) >> pathPoints (capLine r)) caps)
  , cellGrid "joins" 4 170 120 (annotate (\r -> guideStroke (zigzag r) >> pathPoints (zigzag r)) joins)
  , cellGrid "dashes" 4 160 120 dashes
  , cellGrid "transforms" 4 170 130 (annotate (dashedGuide . arrowPath) transforms)
  , cellGrid "clip" 2 200 130 (annotate (\r -> dashedGuide (P.rect (clipBox r))) clips)
  , cellGrid "text" 4 150 100 (annotate (\r -> crosshair (centre r)) textAnchors)
  , cellGrid "borders" 2 200 130 (annotate (guideStroke . P.rect) borders)
  ]
