module NanoUI.Render.ASCII
  ( renderASCII
  , renderASCIIFromRects
  ) where

import NanoUI.Draw (DrawCmd (..), DrawData (..), Layer (..))
import NanoUI.Types (Rect (..))

renderASCII :: Int -> Int -> DrawData -> [String]
renderASCII width height drawData =
  let grid = replicate height (replicate width ' ')
   in foldl (applyCmd width height) grid (drawCommands drawData)

renderASCIIFromRects :: Int -> Int -> [(Rect, Char)] -> [String]
renderASCIIFromRects width height rects =
  foldl (flip stampRect) (replicate height (replicate width ' ')) rects

stampRect :: (Rect, Char) -> [String] -> [String]
stampRect (rect, ch) grid =
  let Rect x y w h = rect
      ix = round x
      iy = round y
      iw = max 1 (round w)
      ih = max 1 (round h)
   in stamp grid ix iy iw ih ch

stamp :: [String] -> Int -> Int -> Int -> Int -> Char -> [String]
stamp grid x y w h ch =
  [ if rowIdx >= y && rowIdx < y + h
      then stampRow row x w ch
      else row
    | (rowIdx, row) <- zip [0 ..] grid
  ]

stampRow :: String -> Int -> Int -> Char -> String
stampRow row x w ch =
  [ if colIdx >= x && colIdx < x + w then ch else c
    | (colIdx, c) <- zip [0 ..] row
  ]

applyCmd :: Int -> Int -> [String] -> DrawCmd -> [String]
applyCmd _ _ grid cmd =
  let ch =
        case cmdLayer cmd of
          LayerBackground -> '.'
          LayerContent -> '#'
          LayerOverlay -> '*'
          LayerChrome -> '|'
   in stamp grid (round (cmdClipX cmd)) (round (cmdClipY cmd)) (max 1 (round (cmdClipW cmd))) (max 1 (round (cmdClipH cmd))) ch
