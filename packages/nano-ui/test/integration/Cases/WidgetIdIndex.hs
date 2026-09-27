module Cases.WidgetIdIndex (tests) where

import Spec
import Data.Text qualified as T
import NanoUI.Internal.Context (Context (..), DamageState (..), PrevFrame (..), getsDamage)
import NanoUI.Internal.Layout.Arena (NodeArena (..), getWidgetId, lookupNodeByWidgetId)
import NanoUI.Internal.Store (ptrEq)

tests :: [Spec]
tests =
  [ spec "shared-key-vanish-damage" (runSharedKeyVanishDamageTest 1)
  , spec "unshared-key-vanish-damage" (runSharedKeyVanishDamageTest 0)
  , spec "shared-key-steady-prev-rects" runSharedKeySteadyPrevRectsTest
  , spec "widget-id-index-lookups" runWidgetIdIndexLookupsTest
  , spec "widget-id-index-epoch-wrap" runWidgetIdIndexEpochWrapTest
  ]

inp :: Input
inp = withInputOff 320 300

-- | A label removed after a table repaints its old area and drops out of the
-- previous rects, with or without a frozen column (two panes under one id).
runSharedKeyVanishDamageTest :: Int -> Context -> IORef Int -> IO ()
runSharedKeyVanishDamageTest freeze ctx failed = do
  mResp <- warmup ctx inp (tableAndLabel freeze True) >> warmup2 ctx inp (tableAndLabel freeze True)
  _ <- takeDamage ctx
  assertJust failed mResp $ \resp -> do
    let old = respRect resp
    assert failed (rectW old > 0 && rectH old > 0)
    _ <- runFrame ctx inp (tableAndLabel freeze False)
    assert failed . (`damageCovers` old) =<< takeDamage ctx
    assertEq failed Nothing =<< getPrevRect ctx (respId resp)

-- | With such a table shown, an unchanged frame reuses last frame's rect map.
runSharedKeySteadyPrevRectsTest :: Context -> IORef Int -> IO ()
runSharedKeySteadyPrevRectsTest ctx failed = do
  let frameRects = runFrame ctx inp (tableAndLabel 1 True) >> getsDamage ctx (pfRects . dsPrev)
  !before <- replicateM_ 2 frameRects >> frameRects
  !after <- frameRects
  assert failed (not (null before) && ptrEq before after)

-- | Every widget resolves to its own node, beyond the index's initial size,
-- and a widget left out next frame resolves to nothing.
runWidgetIdIndexLookupsTest :: Context -> IORef Int -> IO ()
runWidgetIdIndexLookupsTest ctx failed = do
  let na = ctxNodeArena ctx
      ui n = column (forM [1 .. n :: Int] (\i -> withKey i (button' "b")))
      resolvesToItself r = maybe (pure False) (fmap (== respId r) . getWidgetId na) =<< lookupNodeByWidgetId na (respId r)
  resps <- evalUi ctx inp (ui 3000)
  assertEq failed 3000 (length resps)
  assert failed . and =<< mapM resolvesToItself resps
  _ <- runFrame ctx inp (ui 10)
  assert failed . and =<< mapM resolvesToItself (take 10 resps)
  assert failed . all null =<< mapM (lookupNodeByWidgetId na . respId) (drop 10 resps)

-- | A wrapping frame epoch does not revive entries from the previous lap.
runWidgetIdIndexEpochWrapTest :: Context -> IORef Int -> IO ()
runWidgetIdIndexEpochWrapTest ctx failed = do
  let na = ctxNodeArena ctx
  writeIORef (naEpoch na) maxBound
  gone <- evalUi ctx inp (withKey (1 :: Int) (button' "gone"))
  assert failed . not . null =<< lookupNodeByWidgetId na (respId gone)
  writeIORef (naEpoch na) maxBound
  _ <- runFrame ctx inp (withKey (2 :: Int) (label "other"))
  assertEq failed 1 =<< readIORef (naEpoch na)
  assertEq failed Nothing =<< lookupNodeByWidgetId na (respId gone)

-- | A table with @freeze@ frozen columns, and a label after it while @showLabel@.
tableAndLabel :: Int -> Bool -> NanoUI (Maybe Response)
tableAndLabel freeze showLabel = column $ do
  (sortState, _) <- useTableSort (SortCol 0 SortAsc)
  let rows = [(n, n) | n <- map (T.pack . show) [1 .. 20 :: Int]]
  _ <- tableConfigured defaultTableConfig {tableFreezeCols = freeze} (fixedH 150) "people" (headed "Name" fst <> headed "Value" snd) rows sortState
  if showLabel then Just <$> label' "bye" else pure Nothing
