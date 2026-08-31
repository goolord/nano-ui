{-# LANGUAGE OverloadedStrings #-}
import Control.Monad (replicateM_, void)
import Data.IORef (newIORef)
import NanoUI
import NanoUI.Context (getPrevRect, getStore, newContext, storeFloatList)
import qualified Data.IntMap.Strict as IM

main :: IO ()
main = do
  _failed <- newIORef 0
  ctx <- newContext
  let inp0 = emptyInput {inputWindowSize = Size 640 400}
      ui = do
        (win, _) <- window True "Resize" (label "Body")
        pure win
  replicateM_ 2 $ void (runFrame ctx inp0 ui)
  (win, _, _, _) <- runFrame ctx inp0 ui
  m0 <- getPrevRect ctx (respId win)
  print ("prev0" :: String, m0)
  case m0 of
    Nothing -> putStrLn "no rect"
    Just (Rect x y w h) -> do
      let grab = V2 (x + w - 4) (y + h - 4)
          press =
            inp0
              { inputMousePos = grab
              , inputMouseDown = True
              , inputMousePressed = True
              }
      (_, _, _, _) <- runFrame ctx press ui
      store1 <- getStore ctx
      print ("store after press" :: String, IM.toList (storeFloatList store1))
      mPress <- getPrevRect ctx (respId win)
      print ("prev press" :: String, mPress)
      let moved =
            press
              { inputMousePos = V2 (x + w + 40) (y + h + 30)
              , inputMousePressed = False
              }
      (_, _, _, _) <- runFrame ctx moved ui
      store2 <- getStore ctx
      print ("store after move" :: String, IM.toList (storeFloatList store2))
      m1 <- getPrevRect ctx (respId win)
      print ("prev1" :: String, m1)
