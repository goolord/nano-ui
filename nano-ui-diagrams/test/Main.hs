{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless)
import Data.Colour.Names (coral, steelblue)
import Data.List (tails)
import Data.Vector qualified as V
import Diagrams.Prelude
  ( Diagram
  , alignedText
  , alignBL
  , circle
  , fc
  , fromVertices
  , lw
  , lwO
  , moveTo
  , none
  , p2
  , phantom
  , rect
  , ( # )
  )
import NanoUI
import NanoUI.Diagrams
  ( B
  , diagram
  , diagramOps
  , defaultPlotStyle
  , PlotStyle (..)
  , fitLayout
  , labeledChart
  , legendFill
  , linePlot
  , scatterPlot
  , barPlot
  , colourOf
  )
import NanoUI.Testing (DrawData (..), drawCmdNull, newPixelContext, runFrame)

main :: IO ()
main = do
  ctx <- newPixelContext
  let inp = emptyInput {inputWindowSize = Size 240 120}
  (_, _, filled, _) <-
    runFrame ctx inp $
      diagram (fixedWH 200 80 defaultLayout) (circle 1 # fc coral # lw none)
  (_, _, stroked, _) <-
    runFrame ctx inp $
      diagram (fixedWH 200 80 defaultLayout) (linePlot [(x, sin x) | x <- [0, 0.2 .. 6.2]] # lwO 2)
  (_, _, bars, _) <-
    runFrame ctx inp $
      diagram (fixedWH 200 80 defaultLayout) (barPlot [(1, 2), (2, 5), (3, 3)] # fc steelblue # lw none)
  (_, _, scatter, _) <-
    runFrame ctx inp $
      diagram (fixedWH 200 80 defaultLayout) (scatterPlot [(1, 4), (2, 1), (5, 3)] # fc coral # lw none)
  let ok d = drawIndexCount d > 0 && not (drawCmdNull d)
  unless (ok filled && ok stroked && ok bars && ok scatter) $
    fail "diagrams produced no draw commands"
  (_, _, filledAgain, _) <-
    runFrame ctx inp $
      diagram (fixedWH 200 80 defaultLayout) (circle 1 # fc coral # lw none)
  unless (ok filledAgain) $
    fail "cached diagram produced no draw commands"
  let leftM = 0.38
      rightM = 0.82
      botM = 0.36
      topM = 0.22
      marginBox :: Diagram B
      marginBox =
        rect (1 + leftM + rightM) (1 + botM + topM)
          # alignBL
          # moveTo (p2 (-leftM, -botM))
      dump =
        ( mconcat
            [ alignedText 0.5 1 lab # moveTo (p2 (x, -0.055))
            | (x, lab) <- [(0.1, "Mon" :: String), (0.3, "Tue"), (0.5, "Wed"), (0.7, "Thu"), (0.9, "Fri")]
            ]
            <> fromVertices [p2 (0, 0), p2 (1, 0)]
        )
          <> phantom marginBox
      fm = monospaceMetrics 16
      fitted = fitLayout fm (fixedH 180 defaultLayout) dump
      ops =
        case (layoutWidth fitted, layoutHeight fitted) of
          (Fixed bw, Fixed bh) -> diagramOps (realToFrac bw) (realToFrac bh) dump
          _ -> V.empty
      texts = [(x, y, ax, ay, t) | DrawText x y ax ay t _ <- V.toList ops]
      xs = [x | (x, _, _, _, _) <- texts]
      boxes = [drawTextBox fm x y ax ay t | (x, y, ax, ay, t) <- texts]
      overlap a b =
        let Rect x1 y1 w1 h1 = a
            Rect x2 y2 w2 h2 = b
         in x1 < x2 + w2 && x2 < x1 + w1 && y1 < y2 + h2 && y2 < y1 + h1
  unless (length xs == 5 && maximum xs - minimum xs > 40) $
    fail "axis labels did not spread along x"
  unless (not (or [overlap a b | (a : rest) <- tails boxes, b <- rest])) $
    fail "axis label boxes overlap"
  let fittedGrow = fitLayout fm (fillW defaultLayout) dump
      opsGrow =
        diagramOps
          (realToFrac (layoutMinW fittedGrow))
          (realToFrac (layoutMinH fittedGrow))
          dump
      boxesGrow =
        [ drawTextBox fm x y ax ay t
        | DrawText x y ax ay t _ <- V.toList opsGrow
        ]
  unless (layoutMinW fittedGrow > 200) $
    fail "fillW fitLayout did not request label width"
  unless (not (or [overlap a b | (a : rest) <- tails boxesGrow, b <- rest])) $
    fail "fillW axis label boxes overlap"
  let unitLoop = fromVertices [p2 (0, 0), p2 (1, 0), p2 (1, 1), p2 (0, 1)] # lwO 1
      opsBox = diagramOps 300 80 unitLoop
      xsBox = concat [[x0, x1] | Stroke x0 _ x1 _ _ _ <- V.toList opsBox]
  unless (not (null xsBox)) $
    fail "diagramOps produced no strokes"
  let spanX = maximum xsBox - minimum xsBox
  unless (spanX > 50 && spanX < 120) $
    fail "diagramOps did not keep envelope aspect"
  unless (minimum xsBox > 80) $
    fail "diagramOps did not center in a wide slot"
  let chart =
        labeledChart
          defaultPlotStyle
          [(0.1, "Mon"), (0.3, "Tue"), (0.5, "Wed"), (0.7, "Thu"), (0.9, "Fri")]
          [(0, "0")]
          "day"
          "count"
          (legendFill defaultPlotStyle "count")
          (barPlot [(1, 2), (2, 5), (3, 4), (4, 7), (5, 3)])
      chartOps = diagramOps 400 280 chart
      chartTexts = [t | DrawText _ _ _ _ t _ <- V.toList chartOps]
  unless (length chartTexts >= 8) $
    fail "labeledChart dropped tick or title text"
  unless (plotInk defaultPlotStyle == colourOf (themeRed defaultTheme)) $
    fail "defaultPlotStyle ink is not themeRed"
  unless (length (themeSeries defaultTheme) == 6) $
    fail "themeSeries dropped a series colour"
  putStrLn "nano-ui-diagrams: ok"
