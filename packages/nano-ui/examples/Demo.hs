module Main (main) where

import Control.Monad (when)
import Data.Text (Text)
import Data.Text qualified as T
import NanoUI
import NanoUI.Testing (newContext, renderASCII, runFrame)

data Person = Person
  { personName :: !Text
  , personAge :: !Int
  }
  deriving (Eq, Show)

colPeople :: Colonnade Headed Person Text
colPeople =
  mconcat
    [ headed "Name" personName
    , headed "Age" (T.pack . show . personAge)
    ]

people :: [Person]
people =
  [ Person "David" 63
  , Person "Ava" 34
  , Person "Sonia" 12
  ]

main :: IO ()
main = do
  ctx <- newContext
  let inp =
        emptyInput
          { inputWindowSize = Size 60 24
          , inputMousePos = V2 30 10
          , inputMousePressed = True
          , inputMouseDown = True
          }
  (_, msgs, drawData, _) <-
    runFrame
      ctx
      inp
      ( column
          (defaultLayout {layoutWidth = Grow 1, layoutHeight = Grow 1})
          ( do
              (sort, setSort) <- useTableSort (SortCol 0 SortAsc)
              tableResp <- table "people" colPeople people sort
              when (tableRespChanged tableResp) (setSort (tableSort tableResp))
              label "nano-ui demo"
          )
      )
  putStrLn "=== nano-ui ASCII demo ==="
  mapM_ putStrLn (renderASCII 60 24 drawData)
  putStrLn "--- messages ---"
  print (length msgs)
