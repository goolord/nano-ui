module Cases.NoThunks (tests) where

import Spec
import qualified Data.IntMap.Strict as IM
import NanoUI.Internal.Context (Context (..))
import NanoUI.Internal.Store (WidgetStore (..))
import NoThunks.Class (NoThunks, noThunks)

tests :: [Spec]
tests = [spec "no-thunks" runNoThunksTest]

-- | Long-lived widget state keeps no thunks in its stored values, such as a
-- stored tuple's components (the strict-container WHNF trap). Values only:
-- on GHC 9.14 nothunks flags WHNF list and IntMap spines as thunks.
runNoThunksTest :: Context -> IORef Int -> IO ()
runNoThunksTest ctx failed = do
  _ <- warmup2 ctx (withInput 300 200) (column (withValue "hello" textInput >> withValue 42 (slider 0 100)))
  store <- readIORef (ctxStore ctx)
  checkAll "storeText" (storeText store)
  checkAll "storeInt" (storeInt store)
  checkAll "storeFloat" (storeFloat store)
  checkAll "storeDouble" (storeDouble store)
  checkAll "storePoint" (storePoint store)
  checkAll "storeQuiet" (storeQuiet store)
  where
    checkAll :: NoThunks v => String -> IM.IntMap v -> IO ()
    checkAll what m =
      forM_ (IM.toList m) $ \(k, !v) ->
        noThunks [] v >>= mapM_ (\info -> do
          putStrLn ("thunks retained in " ++ what ++ " at key " ++ show k ++ ": " ++ show info)
          assert failed False)
