module Cases.NoThunks (runNoThunksTest) where

import Data.IORef (IORef, readIORef)
import qualified Data.IntMap.Strict as IM
import NanoUI.Context (Context (..))
import NanoUI.Store (WidgetStore (..))
import NoThunks.Class (NoThunks, noThunks)

import NanoUI
import NanoUI.Testing
import NanoUI.Testing.Assert (assert, withInput)

-- | Long-lived widget state must not retain thunks in stored values. Run a
-- frame whose widgets populate several store maps, then check every stored
-- value directly with nothunks. (Container spines/lists are not checked:
-- on GHC 9.14 nothunks flags WHNF list and IntMap internals as thunks, a
-- false positive. Individual values -- Text, Int, Float, Float pairs --
-- are checked precisely, which catches the strict-container WHNF trap
-- where a stored tuple's components remain unevaluated.)
runNoThunksTest :: Context -> IORef Int -> IO ()
runNoThunksTest ctx failed = do
  let inp = withInput 300 200
      ui = column $ do
        _ <- textInput "Name" "hello"
        _ <- slider "Vol" 0 100 42
        pure ()
  _ <- runFrame ctx inp ui
  _ <- runFrame ctx inp ui
  store <- readIORef (ctxStore ctx)
  checkAll failed "storeText" (storeText store)
  checkAll failed "storeInt" (storeInt store)
  checkAll failed "storeFloat" (storeFloat store)
  checkAll failed "storeDouble" (storeDouble store)
  checkAll failed "storePoint" (storePoint store)
  where
    checkAll ::
      NoThunks v => IORef Int -> String -> IM.IntMap v -> IO ()
    checkAll failed' what m =
      IM.foldlWithKey'
        (\acc !k !v -> acc >> checkOne failed' what k v)
        (pure ())
        m

    checkOne :: NoThunks v => IORef Int -> String -> Int -> v -> IO ()
    checkOne failed' what k v = do
      result <- noThunks [] v
      case result of
        Nothing -> pure ()
        Just info -> do
          putStrLn
            ( "thunks retained in "
                ++ what
                ++ " at key "
                ++ show k
                ++ ": "
                ++ show info
            )
          assert failed' False
