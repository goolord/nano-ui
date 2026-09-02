{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | diagrams-lib backend that emits nano-ui 'DrawOp's.
--
-- Solid fills, polylines, and text. No gradients, clips, or dashes.
-- Chart chrome ('labeledChart', legends, ticks, frame) uses 'PlotStyle'
-- from the host 'Theme'.
module NanoUI.Diagrams
  ( NanoUIBackend (..)
  , B
  , diagram
  , diagramOps
  , fitLayout
  , linePlot
  , scatterPlot
  , barPlot
  , PlotStyle (..)
  , themePlotStyle
  , defaultPlotStyle
  , uiPlotStyle
  , colourOf
  , plotLbl
  , legendLine
  , legendFill
  , legendMarks
  , labeledChart
  , dataUnit
  , inkLine
  , fillBars
  , inkScatter
  )
where

import Control.Lens (Lens', (^.), (^?))
import Data.Colour (AlphaColour, Colour, alphaChannel, black)
import Data.Colour qualified as Colour
import Data.Colour.SRGB (RGB (..), sRGB24, toSRGB)
import Data.Hashable (Hashable, hash, hashWithSalt)
import Data.List (tails)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Tree (Tree (Node))
import Data.Proxy (Proxy (..))
import Data.Typeable (TyCon, Typeable, typeOf, typeRep, typeRepTyCon)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Diagrams.Attributes (_lineWidthU)
import Diagrams.Core
  ( Backend (..)
  , N
  , QDiagram
  , Renderable (..)
  , V
  , renderDia
  )
import Diagrams.Core qualified as DiaCore
import Diagrams.Core.Types (Annotation, Prim (..), RNode (..), RTree)
import Diagrams.Located (Located, unLoc)
import Diagrams.Path (Path, pathTrails)
import Diagrams.Prelude
  ( Any
  , Diagram
  , P2
  , SizeSpec
  , Trail
  , V2 (..)
  , alignBL
  , alignedText
  , circle
  , fc
  , fontSizeL
  , fromVertices
  , lc
  , lw
  , lwO
  , moveTo
  , none
  , origin
  , p2
  , papply
  , phantom
  , rect
  , reflectY
  , reflectionY
  , size
  , translate
  , unp2
  , ( # )
  , (^&)
  )
import Diagrams.Segment (FixedSegment (..))
import Diagrams.Trail (fixTrail, isLoop)
import Diagrams.TwoD.Adjust (adjustDia2D)
import Diagrams.TwoD.Attributes (_AC, _fillTexture, _lineTexture)
import Diagrams.TwoD.Size (mkHeight)
import Diagrams.TwoD.Text (Text (..), TextAlignment (..))
import Effectful (Eff, type (:>))
import NanoUI
  ( Color
  , DrawOp (..)
  , FontMetrics (..)
  , Layout (..)
  , Rect (..)
  , Response
  , Sizing (..)
  , Style (..)
  , Theme (..)
  , Ui
  , colorA
  , colorB
  , colorG
  , colorR
  , colorRGBA
  , colorToWord32
  , defaultTheme
  , drawTextBox
  , drawingCached
  , lerpColor
  , shiftDrawOp
  , uiFontMetrics
  , uiTheme
  )

-- | Token identifying this backend.
data NanoUIBackend = NanoUIBackend
  deriving (Eq, Show)

type B = NanoUIBackend

type instance V NanoUIBackend = V2
type instance N NanoUIBackend = Double

instance Semigroup (Render NanoUIBackend V2 n) where
  NRender f <> NRender g = NRender (\sty -> f sty <> g sty)

instance Monoid (Render NanoUIBackend V2 n) where
  mempty = NRender (const [])

instance Eq n => Eq (Options NanoUIBackend V2 n) where
  NanoUIOptions a t1 == NanoUIOptions b t2 = a == b && t1 == t2

instance Hashable n => Hashable (Options NanoUIBackend V2 n) where
  hashWithSalt s (NanoUIOptions sz t) = hashWithSalt s (sz, t)

nanoSize :: Lens' (Options NanoUIBackend V2 n) (SizeSpec V2 n)
nanoSize f (NanoUIOptions sz t) = fmap (\sz' -> NanoUIOptions sz' t) (f sz)

instance (Typeable n, RealFloat n) => Backend NanoUIBackend V2 n where
  newtype Render NanoUIBackend V2 n = NRender (DiaCore.Style V2 n -> [DrawOp])
  type Result NanoUIBackend V2 n = Vector DrawOp
  data Options NanoUIBackend V2 n = NanoUIOptions (SizeSpec V2 n) !Bool
  renderRTree _ (NanoUIOptions _ textOnly) rt = V.fromList (walkStyle textOnly mempty rt)
  adjustDia c opts d = (sz, t <> reflectionY, d')
    where
      (sz, t, d') = adjustDia2D nanoSize c opts (d # reflectY)

walkStyle :: (Typeable n, RealFloat n) => Bool -> DiaCore.Style V2 n -> RTree NanoUIBackend V2 n Annotation -> [DrawOp]
walkStyle textOnly sty (Node n cs) =
  case n of
    RPrim prim
      | textOnly && isPathPrim prim -> []
      | otherwise ->
          let NRender f = render NanoUIBackend prim
           in f sty
    RStyle s -> concatMap (walkStyle textOnly (sty <> s)) cs
    _ -> concatMap (walkStyle textOnly sty) cs

pathTyCon :: TyCon
pathTyCon = typeRepTyCon (typeRep (Proxy :: Proxy (Path V2 Double)))

isPathPrim :: Prim NanoUIBackend V2 n -> Bool
isPathPrim (Prim p) = typeRepTyCon (typeOf p) == pathTyCon

instance (Typeable n, RealFloat n) => Renderable (Path V2 n) NanoUIBackend where
  render _ path = NRender $ \sty -> concatMap (trailOps sty) (pathTrails path)

instance (Typeable n, RealFloat n) => Renderable (Text n) NanoUIBackend where
  render _ (Text tr align str)
    | null str = NRender (const [])
    | otherwise = NRender $ \sty ->
        let p = papply tr origin
            (x, y) = unp2 p
            (ax, ay) =
              case align of
                BaselineText -> (0, -1)
                BoxAlignedText bx by -> (toF bx, toF by)
            col =
              case solidColour (sty ^? (_fillTexture . _AC)) of
                Just c -> c
                Nothing ->
                  fromMaybe
                    (themeMuted defaultTheme)
                    (solidColour (sty ^? (_lineTexture . _AC)))
         in [DrawText (toF x) (toF y) ax ay (T.pack str) col]

trailOps :: (Typeable n, RealFloat n) => DiaCore.Style V2 n -> Located (Trail V2 n) -> [DrawOp]
trailOps sty lt =
  let pts = trailSamples lt
      lineW = toF (fromMaybe 1 (sty ^. _lineWidthU))
      fillC = solidColour (sty ^? (_fillTexture . _AC))
      lineC = solidColour (sty ^? (_lineTexture . _AC))
      closed = isLoop (unLoc lt)
      fills =
        case fillC of
          Just c
            | colorA c > 0 && closed -> fanFill c pts
          _ -> []
      strokes =
        case lineC of
          Just c
            | colorA c > 0 && lineW > 0 -> strokePoly c lineW pts
          _ -> []
   in fills ++ strokes

trailSamples :: RealFloat n => Located (Trail V2 n) -> [P2 n]
trailSamples lt =
  case map sampleSeg (fixTrail lt) of
    [] -> []
    (firstSeg : rest) ->
      let pts = firstSeg ++ concatMap (drop 1) rest
       in if isLoop (unLoc lt) && not (null pts)
            then pts ++ take 1 pts
            else pts

sampleSeg :: RealFloat n => FixedSegment V2 n -> [P2 n]
sampleSeg (FLinear p0 p1) = [p0, p1]
sampleSeg (FCubic p0 c1 c2 p1) =
  [evalCubic p0 c1 c2 p1 (fromIntegral i / 8) | i <- [0 .. 8 :: Int]]

evalCubic :: RealFloat n => P2 n -> P2 n -> P2 n -> P2 n -> n -> P2 n
evalCubic p0 c1 c2 p1 t =
  let u = 1 - t
      (x0, y0) = unp2 p0
      (x1, y1) = unp2 c1
      (x2, y2) = unp2 c2
      (x3, y3) = unp2 p1
      x = u * u * u * x0 + 3 * u * u * t * x1 + 3 * u * t * t * x2 + t * t * t * x3
      y = u * u * u * y0 + 3 * u * u * t * y1 + 3 * u * t * t * y2 + t * t * t * y3
   in p2 (x, y)

fanFill :: RealFloat n => Color -> [P2 n] -> [DrawOp]
fanFill col pts0 =
  case map unp2 (stripClosed pts0) of
    ((ax, ay) : rest@(_ : _ : _)) ->
      [ FillTriangle (toF ax) (toF ay) (toF bx) (toF by) (toF cx) (toF cy) col
      | ((bx, by), (cx, cy)) <- zip rest (drop 1 rest)
      ]
    _ -> []

strokePoly :: RealFloat n => Color -> Float -> [P2 n] -> [DrawOp]
strokePoly col lineW pts =
  [ Stroke (toF x0) (toF y0) (toF x1) (toF y1) lineW col
  | ((x0, y0), (x1, y1)) <- zip (map unp2 pts) (map unp2 (drop 1 pts))
  ]

stripClosed :: RealFloat n => [P2 n] -> [P2 n]
stripClosed pts@(p : _ : _) =
  case reverse pts of
    (q : _)
      | unp2 p == unp2 q -> zipWith const pts (drop 1 pts)
    _ -> pts
stripClosed pts = pts

solidColour :: Maybe (AlphaColour Double) -> Maybe Color
solidColour mc = do
  ac <- mc
  let a = alphaChannel ac
  if a <= 0
    then Nothing
    else
      let RGB r g b = toSRGB (ac `Colour.over` black)
          q x = round (clamp01 x * 255)
       in Just (colorRGBA (q r) (q g) (q b) (q a))

clamp01 :: Double -> Double
clamp01 x = max 0 (min 1 x)

toF :: Real n => n -> Float
toF = realToFrac

-- | Lower a diagram to draw ops. Scale is uniform (envelope aspect). The
-- result is centered in `(w, h)` so a wide `fillW` slot is not stretched.
diagramOps :: Double -> Double -> QDiagram NanoUIBackend V2 Double Any -> Vector DrawOp
diagramOps = renderDiagramOps False

-- Text primitives only. Label fit skips path tessellation.
diagramTextOps :: Double -> Double -> QDiagram NanoUIBackend V2 Double Any -> Vector DrawOp
diagramTextOps = renderDiagramOps True

renderDiagramOps ::
  Bool -> Double -> Double -> QDiagram NanoUIBackend V2 Double Any -> Vector DrawOp
renderDiagramOps textOnly w h d
  | w <= 0 || h <= 0 = V.empty
  | otherwise =
      let outH = uniformHeight w h d
          V2 dw dh = size d
          outW =
            if dh <= 1e-9
              then w
              else outH * dw / dh
          ops = renderDia NanoUIBackend (NanoUIOptions (mkHeight outH) textOnly) d
          dx = realToFrac ((w - outW) / 2)
          dy = realToFrac ((h - outH) / 2)
       in V.map (shiftDrawOp dx dy) ops

-- Envelope height that fits in (w, h) without stretching X independently of Y.
uniformHeight :: Double -> Double -> QDiagram NanoUIBackend V2 Double Any -> Double
uniformHeight w h d =
  let V2 dw dh = size d
   in if dw <= 1e-9 || dh <= 1e-9
        then h
        else min h (w * dh / dw)

-- Host-font boxes stay fixed while positions scale. Grow until adjacent
-- labels no longer overlap (plus 2px).
labelFitScale :: FontMetrics -> Vector DrawOp -> Double
labelFitScale fm ops =
  let ts = [(x, y, ax, ay, t) | DrawText x y ax ay t _ <- V.toList ops]
      k = maximum (1 : [pairK a b | (a : rest) <- tails ts, b <- rest]) :: Float
   in min 8 (realToFrac k)
  where
    pairK (x1, y1, ax1, ay1, t1) (x2, y2, ax2, ay2, t2) =
      let Rect px1 py1 tw1 th1 = drawTextBox fm x1 y1 ax1 ay1 t1
          Rect px2 py2 tw2 th2 = drawTextBox fm x2 y2 ax2 ay2 t2
          overlapX = px1 < px2 + tw2 && px2 < px1 + tw1
          overlapY = py1 < py2 + th2 && py2 < py1 + th1
       in if overlapX && overlapY
            then
              max
                (axisK x1 x2 (px1 - x1) tw1 (px2 - x2) tw2)
                (axisK y1 y2 (py1 - y1) th1 (py2 - y2) th2)
            else 1
    axisK a1 a2 o1 size1 o2 size2 =
      let (loA, loO, loS, hiA, hiO) =
            if a1 <= a2
              then (a1, o1, size1, a2, o2)
              else (a2, o2, size2, a1, o1)
          den = hiA - loA
          need = loO + loS + 2 - hiO
       in if den <= 1e-6 then 1 else max 1 (need / den)

diagramFrame :: PlotStyle -> Float -> Rect -> Vector DrawOp
diagramFrame ps bw (Rect x y w h) =
  V.fromList
    [ FillRect (Rect x y w h) (plotFrameBg ps)
    , Stroke x y (x + w) y bw (plotFrameBorder ps)
    , Stroke (x + w) y (x + w) (y + h) bw (plotFrameBorder ps)
    , Stroke (x + w) (y + h) x (y + h) bw (plotFrameBorder ps)
    , Stroke x (y + h) x y bw (plotFrameBorder ps)
    ]

-- Pixel size that matches the envelope, fitted to the layout height (or both
-- Fixed bounds), then grown so host-font tick labels do not overlap.
fitLayout :: FontMetrics -> Layout -> Diagram B -> Layout
fitLayout fm layout d =
  let V2 dw dh = size d
      ar = if dh <= 1e-9 then 1 else dw / dh
      growW =
        case layoutWidth layout of
          Grow _ -> True
          _ -> False
      (baseW, baseH) =
        case (layoutWidth layout, layoutHeight layout) of
          (Fixed bw, Fixed bh)
            | dw > 1e-9 && dh > 1e-9 ->
                let s = min (realToFrac bw / dw) (realToFrac bh / dh)
                 in (dw * s, dh * s)
          (_, Fixed bh) ->
            let h = realToFrac bh :: Double
             in (h * ar, h)
          (Fixed bw, _) ->
            let w = realToFrac bw :: Double
             in (w, w / ar)
          _ ->
            let h =
                  if layoutMinH layout > 0
                    then realToFrac (layoutMinH layout)
                    else 160
             in (h * ar, h)
      clampSize x = realToFrac (max 8 x) :: Float
      arF = clampSize (max 0.2 ar)
   in if growW
        then
          -- Grow inside a Fit parent measures as minW (else 32). Probe labels
          -- so the slot is at least as wide as host-font ticks, then let extra
          -- parent width raise height via layoutAspect.
          let probeH0 =
                realToFrac
                  ( if layoutMinH layout > 0
                      then layoutMinH layout
                      else 200
                  ) ::
                  Double
              probeH = min probeH0 (realToFrac (layoutMaxH layout))
              probeW = probeH * ar
              k = labelFitScale fm (diagramTextOps probeW probeH d)
              needW = clampSize (probeW * k)
              needH = clampSize (probeH * k)
              minH = max (max (layoutMinH layout) 200) needH
              maxH = max (min (layoutMaxH layout) 320) needH
           in layout
                { layoutHeight = Fit
                , layoutAspect = arF
                , layoutMinW = max (layoutMinW layout) needW
                , layoutMinH = minH
                , layoutMaxH = maxH
                }
        else
          let k = labelFitScale fm (diagramTextOps baseW baseH d)
              wF = clampSize (baseW * k)
              hF = clampSize (baseH * k)
           in layout
                { layoutWidth = Fixed wF
                , layoutHeight = Fixed hF
                , layoutMinW = wF
                , layoutMaxW = wF
                , layoutMinH = hF
                , layoutMaxH = hF
                }

-- | Fit a diagrams value into a laid-out widget. Layout fit and draw ops are
-- cached while envelope, font, theme, and size stay the same.
diagram :: Ui :> es => Layout -> QDiagram NanoUIBackend V2 Double Any -> Eff es Response
diagram layout d = do
  fm <- uiFontMetrics
  theme <- uiTheme
  let V2 dw dh = size d
      content = themePlotKey theme
      ps = themePlotStyle theme
  drawingCached dw dh (fmLineHeight fm) content layout (pure (fitLayout fm layout d)) $ \rectBox ->
    let borderW = 1
        inset = borderW
        inner =
          Rect
            (rectX rectBox + inset)
            (rectY rectBox + inset)
            (max 0 (rectW rectBox - 2 * inset))
            (max 0 (rectH rectBox - 2 * inset))
        w = realToFrac (rectW inner) :: Double
        h = realToFrac (rectH inner)
        plot =
          if w <= 0 || h <= 0
            then V.empty
            else V.map (shiftDrawOp (rectX inner) (rectY inner)) (diagramOps w h d)
     in diagramFrame ps borderW rectBox <> plot

-- | Polyline in the unit square. Data min/max map to [0,1].
linePlot :: [(Double, Double)] -> Diagram B
linePlot pts =
  case normalizeXY pts of
    [] -> mempty
    ps -> fromVertices ps

-- | Markers in the unit square. Both axes scale from the data min/max.
scatterPlot :: [(Double, Double)] -> Diagram B
scatterPlot pts =
  case normalizeXY pts of
    [] -> mempty
    ps -> mconcat [circle 0.045 # moveTo p | p <- ps]

-- | Bars in the unit square. Y is scaled by the data max. X is uniform bins.
barPlot :: [(Double, Double)] -> Diagram B
barPlot bins =
  case bins of
    [] -> mempty
    _ ->
      let ys = map snd bins
          maxY = maximum (1e-9 : map abs ys)
          n = fromIntegral (length bins) :: Double
          w = 0.72 / n
          bar i y =
            let bh = abs y / maxY
                cx = (fromIntegral i + 0.5) / n
             in rect w bh # translate ((cx ^& (signum y * bh / 2)))
       in mconcat (zipWith bar [0 :: Int ..] ys)

normalizeXY :: [(Double, Double)] -> [P2 Double]
normalizeXY [] = []
normalizeXY pts =
  let xs = map fst pts
      ys = map snd pts
      minX = minimum xs
      maxX = maximum xs
      minY = minimum ys
      maxY = maximum ys
      dx = max 1e-9 (maxX - minX)
      dy = max 1e-9 (maxY - minY)
   in [p2 ((x - minX) / dx, (y - minY) / dy) | (x, y) <- pts]

-- | Series and chrome colours derived from a nano-ui 'Theme'.
data PlotStyle = PlotStyle
  { plotInk :: Colour Double
  , plotFill :: Colour Double
  , plotGrid :: Colour Double
  , plotMuted :: Colour Double
  , plotFrameBg :: Color
  , plotFrameBorder :: Color
  }
  deriving (Eq, Show)

-- | Host 'Color' as a diagrams 'Colour'. Alpha is dropped.
colourOf :: Color -> Colour Double
colourOf c = sRGB24 (colorR c) (colorG c) (colorB c)

themePlotStyle :: Theme -> PlotStyle
themePlotStyle t =
  let muted = themeMuted t
      panel = themePanel t
   in PlotStyle
        { plotInk = colourOf (themeRed t)
        , plotFill = colourOf (lerpColor (themeAccent t) muted 0.22)
        , plotGrid = colourOf (themeSeparator t)
        , plotMuted = colourOf muted
        , plotFrameBg = styleBg (themeInput t)
        , plotFrameBorder = styleBorder panel
        }

defaultPlotStyle :: PlotStyle
defaultPlotStyle = themePlotStyle defaultTheme

themePlotKey :: Theme -> Int
themePlotKey t =
  hash
    [ colorToWord32 (themeAccent t)
    , colorToWord32 (themeMuted t)
    , colorToWord32 (themeRed t)
    , colorToWord32 (themeOrange t)
    , colorToWord32 (themeYellow t)
    , colorToWord32 (themeGreen t)
    , colorToWord32 (themePurple t)
    , colorToWord32 (themeSeparator t)
    , colorToWord32 (themeWindow t)
    , colorToWord32 (styleBg (themePanel t))
    , colorToWord32 (styleBorder (themePanel t))
    , colorToWord32 (styleBg (themeInput t))
    ]

uiPlotStyle :: Ui :> es => Eff es PlotStyle
uiPlotStyle = fmap themePlotStyle uiTheme

-- diagrams fontSizeL does not size host glyphs. Positions scale with the
-- plot; glyph size stays the host font.
plotLbl :: PlotStyle -> Double -> Double -> String -> Diagram B
plotLbl ps ax ay s =
  alignedText ax ay s # fontSizeL 0.085 # fc (plotMuted ps) # lc (plotMuted ps) # lw none

legendLine :: PlotStyle -> String -> Diagram B
legendLine ps name =
  (fromVertices [p2 (0, 0), p2 (0.18, 0)] # lc (plotInk ps) # lwO 2)
    <> (plotLbl ps 0 0.5 name # moveTo (p2 (0.24, 0)))

legendFill :: PlotStyle -> String -> Diagram B
legendFill ps name =
  (rect 0.14 0.07 # fc (plotFill ps) # lw none # moveTo (p2 (0.07, 0)))
    <> (plotLbl ps 0 0.5 name # moveTo (p2 (0.24, 0)))

legendMarks :: PlotStyle -> String -> Diagram B
legendMarks ps name =
  (fromVertices [p2 (0, 0), p2 (0.18, 0)] # lc (plotInk ps) # lwO 2)
    <> (circle 0.035 # fc (plotInk ps) # lw none # moveTo (p2 (0.09, 0)))
    <> (plotLbl ps 0 0.5 name # moveTo (p2 (0.24, 0)))

-- Axis labels and legend sit in a phantom margin. diagrams text has no envelope.
labeledChart ::
  PlotStyle ->
  [(Double, String)] ->
  [(Double, String)] ->
  String ->
  String ->
  Diagram B ->
  Diagram B ->
  Diagram B
labeledChart ps xTicks yTicks xTitle yTitle legend series =
  let axes =
        ( fromVertices [p2 (0, 0), p2 (1, 0)]
            <> fromVertices [p2 (0, 0), p2 (0, 1)]
            <> mconcat [fromVertices [p2 (x, 0), p2 (x, 0.035)] | (x, _) <- xTicks]
            <> mconcat [fromVertices [p2 (0, y), p2 (0.03, y)] | (y, _) <- yTicks]
        )
          # lc (plotGrid ps)
          # lwO 1
      xLabs =
        mconcat
          [ plotLbl ps 0.5 1 lab # moveTo (p2 (x, -0.055))
          | (x, lab) <- xTicks
          ]
      yLabs =
        mconcat
          [ plotLbl ps 1 0.5 lab # moveTo (p2 (-0.05, y))
          | (y, lab) <- yTicks
          ]
      xt = plotLbl ps 0.5 1 xTitle # fontSizeL 0.1 # moveTo (p2 (0.5, -0.22))
      yt = plotLbl ps 0 0.5 yTitle # fontSizeL 0.1 # moveTo (p2 (-leftM + 0.04, 1.12))
      lg = legend # moveTo (p2 (1.18, 0.92))
      leftM = 0.42 :: Double
      rightM = 0.55 :: Double
      botM = 0.34 :: Double
      topM = 0.16 :: Double
      marginBox :: Diagram B
      marginBox =
        rect (1 + leftM + rightM) (1 + botM + topM)
          # alignBL
          # moveTo (p2 (-leftM, -botM))
   in (axes <> series <> xLabs <> yLabs <> xt <> yt <> lg) <> phantom marginBox

-- | Map a data value onto [0,1] given the axis min and max.
dataUnit :: Double -> Double -> Double -> Double
dataUnit lo hi v = (v - lo) / max 1e-9 (hi - lo)

-- | 'linePlot' in 'plotInk', 2px stroke.
inkLine :: PlotStyle -> [(Double, Double)] -> Diagram B
inkLine ps pts = linePlot pts # lc (plotInk ps) # lwO 2

-- | 'barPlot' filled with 'plotFill', no stroke.
fillBars :: PlotStyle -> [(Double, Double)] -> Diagram B
fillBars ps bins = barPlot bins # fc (plotFill ps) # lw none

-- | 'scatterPlot' filled with 'plotInk'.
inkScatter :: PlotStyle -> [(Double, Double)] -> Diagram B
inkScatter ps pts = scatterPlot pts # fc (plotInk ps) # lw none

