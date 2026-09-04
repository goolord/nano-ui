{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Diagrams.Widget
  ( diagram
  , fitLayout
  , labelFitScale
  , diagramFrame
  , PlotStyle (..)
  , themePlotStyle
  , defaultPlotStyle
  , uiPlotStyle
  , colourOf
  , themePlotKey
  ) where


import Data.Colour (Colour)
import Data.Colour.SRGB (sRGB24)
import Data.Hashable (hash)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Diagrams.Core (QDiagram)
import Diagrams.Prelude (Any, Diagram, V2 (..), size)
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
  , colorB
  , colorG
  , colorR
  , colorToWord32
  , defaultTheme
  , drawTextBox
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
  , uiFontMetrics
  , uiTheme
  )
import NanoUI.Diagrams.Backend
  ( B
  , NanoUIBackend
  , diagramOps
  , diagramTextOps
  )

data PlotStyle = PlotStyle
  { plotInk :: Colour Double
  , plotFill :: Colour Double
  , plotGrid :: Colour Double
  , plotMuted :: Colour Double
  , plotFrameBg :: Color
  , plotFrameBorder :: Color
  }
  deriving (Eq, Show)

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

labelFitScale :: FontMetrics -> V.Vector DrawOp -> Double
labelFitScale fm ops =
  let !ts = V.mapMaybe extractBox ops
      !n  = V.length ts
      !k  = outerLoop 0 (1.0 :: Float)
        where
          outerLoop !i !acc
            | i >= n - 1 = acc
            | otherwise  =
                let !(x1, y1, px1, py1, tw1, th1) = V.unsafeIndex ts i
                    innerLoop !j !m
                      | j >= n    = m
                      | otherwise =
                          let !(x2, y2, px2, py2, tw2, th2) = V.unsafeIndex ts j
                              !pairVal = pairK x1 y1 px1 py1 tw1 th1 x2 y2 px2 py2 tw2 th2
                          in innerLoop (j + 1) (max m pairVal)
                in outerLoop (i + 1) (innerLoop (i + 1) acc)
   in min 2 (realToFrac k)
  where
    extractBox (DrawText x y ax ay t _) =
      let !(Rect px py tw th) = drawTextBox fm x y ax ay t
       in Just (x, y, px, py, tw, th)
    extractBox _ = Nothing

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

diagramFrame :: PlotStyle -> Float -> Rect -> Vector DrawOp
diagramFrame ps bw (Rect x y w h) =
  V.fromList
    [ FillRect (Rect x y w h) (plotFrameBg ps)
    , Stroke x y (x + w) y bw (plotFrameBorder ps)
    , Stroke (x + w) y (x + w) (y + h) bw (plotFrameBorder ps)
    , Stroke (x + w) (y + h) x (y + h) bw (plotFrameBorder ps)
    , Stroke x (y + h) x y bw (plotFrameBorder ps)
    ]

-- Grow plots cap here unless the caller set a tighter layoutMaxH.
growPlotCapH :: Float
growPlotCapH = 260

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
                , layoutAspect = arF
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
