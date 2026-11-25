-- | Checks the style-index layout in "NanoUI.Internal.StyleBits": within each
-- node type's fields no two share a bit, every field fits in 64 bits, and a
-- button's label font has room for a whole text font packing.
module Main (main) where

import Control.Monad (unless)
import Data.Bits ((.&.))
import NanoUI.Internal.StyleBits
import System.Exit (exitFailure)

main :: IO ()
main = do
  let problems = concatMap layoutProblems styleLayouts <> labelFontProblems <> flagProblems
  mapM_ putStrLn problems
  unless (null problems) exitFailure
  putStrLn ("style-bits-spec: " <> show (length styleLayouts) <> " layouts checked")

layoutProblems :: (String, [(String, BitField)]) -> [String]
layoutProblems (node, fields) =
  [ node <> ": " <> name <> " " <> show f <> " does not fit in 64 bits"
  | (name, f) <- fields
  , bfShift f < 0 || bfWidth f < 1 || fieldEnd f > 64
  ]
    <> [ node <> ": " <> a <> " and " <> b <> " share bits"
       | ((a, fa), i) <- zip fields [0 :: Int ..]
       , ((b, fb), j) <- zip fields [0 ..]
       , i < j
       , fieldMask fa .&. fieldMask fb /= 0
       ]

labelFontProblems :: [String]
labelFontProblems =
  [ "labelFont is " <> show (bfWidth labelFont) <> " bits wide, too narrow for " <> name
  | (name, f) <- textFontFields
  , fieldEnd f > bfWidth labelFont
  ]

flagProblems :: [String]
flagProblems =
  [ name <> " is a flag but " <> show (bfWidth f) <> " bits wide"
  | (name, f) <- [("inputSearch", inputSearch), ("inputSelectable", inputSelectable), ("inputPassword", inputPassword), ("inputNumeric", inputNumeric)]
  , bfWidth f /= 1
  ]
