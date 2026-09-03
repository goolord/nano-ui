-- | Terminal test helpers: adaptive contexts, palette probes, and raster checks.
module NanoUI.Testing.Term
  ( newAdaptiveTerminalContext
  , newTerminalContext
  , queryTerminalColors
  , terminalDefaultFg
  , terminalDefaultBg
  , terminalThemeFromColors
  , frameBytes
  , cellChar
  , cellRows
  , cellsH
  , narrowChar
  , rasterize
  , rasterizeLayered
  , MouseAction (..)
  , MouseBtn (..)
  , TermEvent (..)
  , noMods
  , decode
  , flushPending
  , terminalGridW
  , terminalBracketSpans
  , terminalPairsOk
  , terminalBracketsOk
  , terminalBracketHasTrail
  ) where

import Data.Text qualified as T
import NanoUI (Rect (..), fontAwesomeIcon, rectW, rectX)
import NanoUI.Testing (wideTrailChar)
import NanoUI.Term.Ansi (frameBytes)
import NanoUI.Term.Cells (Cells, cellChar, cellRows, cellsH, narrowChar, rasterize, rasterizeLayered)
import NanoUI.Term.Event (MouseAction (..), MouseBtn (..), TermEvent (..), noMods)
import NanoUI.Term.Palette
  ( newAdaptiveTerminalContext
  , newTerminalContext
  , queryTerminalColors
  , terminalDefaultBg
  , terminalDefaultFg
  , terminalThemeFromColors
  )
import NanoUI.Term.Vt (decode, flushPending)

terminalGridW :: Cells -> Int
terminalGridW cells =
  case cellRows cells of
    (r : _) -> length r
    [] -> 0

terminalBracketSpans :: [(Rect, T.Text, a, b, c)] -> [Rect]
terminalBracketSpans spans =
  [r | (r, txt, _, _, _) <- spans, T.isPrefixOf "[ " txt]

terminalPairsOk :: Cells -> [(Rect, T.Text, a, b, c)] -> Bool
terminalPairsOk cells spans =
  let
    skip = oneColFaOrigins spans
    gw = terminalGridW cells
   in
    all
      ( \(x, y) ->
          let
            c = cellChar cells x y
           in
            not (fontAwesomeIcon c)
              || (x, y) `elem` skip
              || ( x + 1 < gw
                     && cellChar cells (x + 1) y == wideTrailChar
                 )
      )
      [ (x, y)
      | y <- [0 .. cellsH cells - 1]
      , x <- [0 .. gw - 1]
      ]

terminalBracketsOk :: Cells -> [(Rect, T.Text, a, b, c)] -> Bool
terminalBracketsOk cells spans =
  all
    ( \(Rect x y w h) ->
        let
          x0 = max 0 (round x)
          y0 = max 0 (round y)
          x1 = min (terminalGridW cells - 1) (round (x + w - 1))
          y1 = min (cellsH cells - 1) (round (y + h - 1))
         in
          all
            ( \cy ->
                all
                  (\cx -> cellChar cells cx cy /= wideTrailChar)
                  [x0 .. x1]
            )
            [y0 .. y1]
    )
    (terminalBracketSpans spans)

terminalBracketHasTrail :: Cells -> Rect -> Bool
terminalBracketHasTrail cells (Rect x y w h) =
  let
    x0 = max 0 (round x)
    y0 = max 0 (round y)
    x1 = min (terminalGridW cells - 1) (round (x + w - 1))
    y1 = min (cellsH cells - 1) (round (y + h - 1))
   in
    any
      ( \cy ->
          any (\cx -> cellChar cells cx cy == wideTrailChar) [x0 .. x1]
      )
      [y0 .. y1]

oneColFaOrigins :: [(Rect, T.Text, a, b, c)] -> [(Int, Int)]
oneColFaOrigins spans =
  [ (round (rectX r), round (rectY r))
  | (r, txt, _, _, _) <- spans
  , rectW r < 2
  , loneFontAwesome (T.strip txt)
  ]

loneFontAwesome :: T.Text -> Bool
loneFontAwesome txt =
  T.length txt == 1 && fontAwesomeIcon (T.head txt)
