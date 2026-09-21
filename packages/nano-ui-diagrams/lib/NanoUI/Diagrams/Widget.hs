-- | Widgets that place a diagram in a nano-ui layout, and the plot style
-- derived from the current theme.
module NanoUI.Diagrams.Widget
  ( diagram
  , diagramWithEnvelope
  , diagramWithKeyAndEnvelope
  , fitLayout
  , labelFitScale
  , diagramFrame
  , frameInner
  , PlotStyle (..)
  , themePlotStyle
  , defaultPlotStyle
  , uiPlotStyle
  , colourOf
  , themePlotKey
  ) where


import Data.Colour (Colour)
import Data.Colour.SRGB (sRGB24)
import Data.Hashable (hash, hashWithSalt)
import Data.Primitive.PrimArray (indexPrimArray, newPrimArray, runPrimArray, writePrimArray)
import Data.Primitive.SmallArray (SmallArray, emptySmallArray, indexSmallArray, mapSmallArray', sizeofSmallArray, smallArrayFromList)
import Diagrams.Core (QDiagram)
import Diagrams.Prelude (Any, Diagram, V2 (..), size)
import Effectful (Eff, type (:>))
import NanoUI
  ( Color
  , DrawOp (..)
  , Layout (..)
  , Rect (..)
  , Response
  , Sizing (..)
  , Style (..)
  , Theme (..)
  , Ui
  , colorB
  , colorG
  , colorR
  , colorToWord32
  , defaultLayout
  , defaultTheme
  , drawingCached
  , lerpColor
  , shiftDrawOp
  , styleBg
  , styleBorder
  , themeAccent
  , themeGreen
  , themeMuted
  , themeOrange
  , themePurple
  , themeRed
  , themeSeparator
  , themeWindow
  , themeYellow
  , uiTheme
  )
import NanoUI.Backend (FontMetrics (..), drawTextBox, prepareFontMetricsMany, uiFontMetrics)
import NanoUI.Internal.Context (lookupDrawFitEnvelope)
import NanoUI.Internal.Monad (askContext)
import NanoUI.Monad (currentId, uiIO)
import NanoUI.Diagrams.Backend
  ( B
  , NanoUIBackend
  , diagramOps
  , diagramTextOps
  )

-- | Colours for diagram content, grid, muted labels, and the enclosing frame.
data PlotStyle = PlotStyle
  { plotInk :: Colour Double
  , plotFill :: Colour Double
  , plotGrid :: Colour Double
  , plotMuted :: Colour Double
  , plotFrameBg :: Color
  , plotFrameBorder :: Color
  }
  deriving (Eq, Show)

-- | Convert RGB channels to a diagrams colour, discarding alpha.
colourOf :: Color -> Colour Double
colourOf c = sRGB24 (colorR c) (colorG c) (colorB c)

-- | Derive plot colours from a nano-ui theme's accents and surfaces.
themePlotStyle :: Theme -> PlotStyle
themePlotStyle t =
  let muted = themeMuted t
      panel = themePanel t
   in PlotStyle
        { plotInk = colourOf (themeRed t)
        , plotFill = colourOf (lerpColor (themeAccent t) muted 0.22)
        , plotGrid = colourOf (lerpColor (themeSeparator t) muted 0.30)
        , plotMuted = colourOf muted
        , plotFrameBg = styleBg (themeInput t)
        , plotFrameBorder = styleBorder panel
        }

-- | Plot colours derived from 'defaultTheme'.
defaultPlotStyle :: PlotStyle
defaultPlotStyle = themePlotStyle defaultTheme

-- | Cache key for theme colours used by chart content and frames.
themePlotKey :: Theme -> Int
themePlotKey t =
  hash (colorToWord32 (themeAccent t))
    `hashWithSalt` colorToWord32 (themeMuted t)
    `hashWithSalt` colorToWord32 (themeRed t)
    `hashWithSalt` colorToWord32 (themeOrange t)
    `hashWithSalt` colorToWord32 (themeYellow t)
    `hashWithSalt` colorToWord32 (themeGreen t)
    `hashWithSalt` colorToWord32 (themePurple t)
    `hashWithSalt` colorToWord32 (themeSeparator t)
    `hashWithSalt` colorToWord32 (themeWindow t)
    `hashWithSalt` colorToWord32 (styleBg (themePanel t))
    `hashWithSalt` colorToWord32 (styleBorder (themePanel t))
    `hashWithSalt` colorToWord32 (styleBg (themeInput t))

-- | Plot colours from the current view's scoped theme.
uiPlotStyle :: Ui :> es => Eff es PlotStyle
uiPlotStyle = fmap themePlotStyle uiTheme

-- | Estimate a scale from 1 to 2 that separates overlapping plain-text labels.
-- The cap means this is a sizing heuristic, not a guarantee against overlap.
labelFitScale :: FontMetrics -> SmallArray DrawOp -> Double
labelFitScale fm ops =
  let -- Six numbers a label: its anchor, and its box's origin and size.
      !n = foldl' (\c op -> case op of DrawText {} -> c + 1; _ -> c) 0 ops
      !ts = runPrimArray $ do
        out <- newPrimArray (n * 6)
        let fill !i !b
              | i >= sizeofSmallArray ops = pure out
              | otherwise = case indexSmallArray ops i of
                  DrawText x y ax ay t _ -> do
                    let Rect px py tw th = drawTextBox fm x y ax ay t
                    writePrimArray out b x
                    writePrimArray out (b + 1) y
                    writePrimArray out (b + 2) px
                    writePrimArray out (b + 3) py
                    writePrimArray out (b + 4) tw
                    writePrimArray out (b + 5) th
                    fill (i + 1) (b + 6)
                  _ -> fill (i + 1) b
        fill 0 0
      at i field = indexPrimArray ts (i * 6 + field)
      !k = outerLoop 0 (1.0 :: Float)
        where
          outerLoop !i !acc
            | i >= n - 1 = acc
            | otherwise  =
                let innerLoop !j !m
                      | j >= n    = m
                      | otherwise =
                          let !pairVal = pairK (at i 0) (at i 1) (at i 2) (at i 3) (at i 4) (at i 5) (at j 0) (at j 1) (at j 2) (at j 3) (at j 4) (at j 5)
                          in innerLoop (j + 1) (max m pairVal)
                in outerLoop (i + 1) (innerLoop (i + 1) acc)
   in min 2 (realToFrac k)
  where

    pairK !x1 !y1 !px1 !py1 !tw1 !th1 !x2 !y2 !px2 !py2 !tw2 !th2 =
      let !overlapX = px1 < px2 + tw2 && px2 < px1 + tw1
          !overlapY = py1 < py2 + th2 && py2 < py1 + th1
       in if overlapX && overlapY
            then
              max
                (axisK x1 x2 (px1 - x1) tw1 (px2 - x2) tw2)
                (axisK y1 y2 (py1 - y1) th1 (py2 - y2) th2)
            else 1.0

    axisK !a1 !a2 !o1 !size1 !o2 !size2 =
      let (!loA, !loO, !loS, !hiA, !hiO) =
            if a1 <= a2
              then (a1, o1, size1, a2, o2)
              else (a2, o2, size2, a1, o1)
          !den  = hiA - loA
          !need = loO + loS + 2 - hiO
       in if den <= 1e-6 then 1.0 else max 1.0 (need / den)

-- | Background and four border strokes for a logical-pixel rectangle.
-- The second argument is border width in logical pixels.
diagramFrame :: PlotStyle -> Float -> Rect -> SmallArray DrawOp
diagramFrame ps bw (Rect x y w h) =
  smallArrayFromList
    [ FillRect (Rect x y w h) (plotFrameBg ps)
    , Stroke x y (x + w) y bw (plotFrameBorder ps)
    , Stroke (x + w) y (x + w) (y + h) bw (plotFrameBorder ps)
    , Stroke (x + w) (y + h) x (y + h) bw (plotFrameBorder ps)
    , Stroke x (y + h) x y bw (plotFrameBorder ps)
    ]

-- Grow plots cap here unless the caller set a tighter layoutMaxH.
growPlotCapH :: Float
growPlotCapH = 260

-- | Choose diagram dimensions from its aspect ratio and measured labels.
-- Growing widths retain flexibility; other widths become fixed. Prepare font
-- metrics for all labels before calling this pure sizing operation.
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
   in if growW
        then
          let capH = min growPlotCapH (layoutMaxH layout)
              floorH = if layoutMinH layout > 0 then layoutMinH layout else 180
              probeH0 =
                realToFrac
                  ( if layoutMinH layout > 0
                      then layoutMinH layout
                      else 200
                  ) ::
                  Double
              probeH = min probeH0 (realToFrac capH)
              probeW = probeH * ar
              k = labelFitScale fm (diagramTextOps probeW probeH d)
              needW = clampSize (probeW * k)
              needH = min capH (max floorH (clampSize (probeH * k)))
           in layout
                { layoutHeight = Fit
                , layoutMinW = max (layoutMinW layout) needW
                , layoutMinH = needH
                , layoutMaxH = max needH capH
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

-- | 'diagramWithEnvelope' whose cached draw ops are also keyed by @userKey@.
-- Change the key when the diagram's content changes.
diagramWithKeyAndEnvelope ::
  Ui :> es =>
  Int ->
  Double ->
  Double ->
  (Layout -> Layout) ->
  QDiagram NanoUIBackend V2 Double Any ->
  Eff es Response
diagramWithKeyAndEnvelope userKey dw dh f =
  framedDiagram (\t -> hash (userKey, themePlotKey t)) dw dh (f defaultLayout)

-- | 'diagram' with an explicit envelope width and height.
diagramWithEnvelope ::
  Ui :> es =>
  Double ->
  Double ->
  (Layout -> Layout) ->
  QDiagram NanoUIBackend V2 Double Any ->
  Eff es Response
diagramWithEnvelope dw dh f = framedDiagram themePlotKey dw dh (f defaultLayout)

-- | Draw a diagram inside the plot frame. Its draw ops are cached under the
-- content key the caller derives from the current theme.
framedDiagram ::
  Ui :> es =>
  (Theme -> Int) ->
  Double ->
  Double ->
  Layout ->
  QDiagram NanoUIBackend V2 Double Any ->
  Eff es Response
framedDiagram contentKey dw dh layout d = do
  fm <- uiFontMetrics
  theme <- uiTheme
  let ps = themePlotStyle theme
  drawingCached dw dh (fmLineHeight fm) (contentKey theme) (const layout) (fitLayoutIO fm layout d) $ \rectBox ->
    let inner = frameInner rectBox
        w = realToFrac (rectW inner) :: Double
        h = realToFrac (rectH inner)
        plot =
          if w <= 0 || h <= 0
            then emptySmallArray
            else mapSmallArray' (shiftDrawOp (rectX inner) (rectY inner)) (diagramOps w h d)
     in diagramFrame ps frameBorder rectBox <> plot

frameBorder :: Float
frameBorder = 1

-- | The box a framed diagram draws into, inside its border.
frameInner :: Rect -> Rect
frameInner (Rect x y w h) =
  Rect (x + frameBorder) (y + frameBorder) (max 0 (w - 2 * frameBorder)) (max 0 (h - 2 * frameBorder))

fitLayoutIO :: FontMetrics -> Layout -> Diagram B -> IO Layout
fitLayoutIO fm layout d = do
  let texts = foldr (\op rest -> case op of
        DrawText _ _ _ _ t _ -> t : rest
        _ -> rest) [] (diagramTextOps 100 100 d)
  prepared <- prepareFontMetricsMany fm texts
  pure (fitLayout prepared layout d)

-- | Draw a diagram inside a framed box sized by the layout modifier. Text in
-- the diagram is measured with the current font so labels fit.
diagram :: Ui :> es => (Layout -> Layout) -> QDiagram NanoUIBackend V2 Double Any -> Eff es Response
diagram f d = do
  ctx <- askContext
  wid <- currentId
  fm <- uiFontMetrics
  theme <- uiTheme
  let content = themePlotKey theme
  mEnv <- uiIO (lookupDrawFitEnvelope ctx wid (fmLineHeight fm) content (f defaultLayout))
  case mEnv of
    Just (dw, dh) -> diagramWithEnvelope dw dh f d
    Nothing ->
      let V2 dw dh = size d
       in diagramWithEnvelope dw dh f d
