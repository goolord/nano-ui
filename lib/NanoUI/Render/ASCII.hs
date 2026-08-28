module NanoUI.Render.ASCII
  ( renderASCII
  ) where

import NanoUI.Draw (DrawCmd (..), DrawData (..), Layer (..))

renderASCII :: Int -> Int -> DrawData -> [String]
renderASCII width height drawData =
  let grid = replicate height (replicate width ' ')
   in foldl (applyCmd width height) grid (drawCommands drawData)

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
