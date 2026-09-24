module Cases.WidgetIdIndex (tests) where

import Spec
import Data.Text qualified as T
import NanoUI.Internal.Context (Context (..), DamageState (..), getsDamage)
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

-- | A table with a frozen column has two scroll panes under one widget id.
-- A label after it that goes away repaints where it was and leaves the
-- previous frame's rects, as it does next to a table without frozen columns:
-- counting the panes' id twice must not stand in for the label's.
runSharedKeyVanishDamageTest :: Int -> Context -> IORef Int -> IO ()
runSharedKeyVanishDamageTest freeze ctx failed = do
  let inp = withInputOff 320 300
  _ <- runFrame ctx inp (tableAndLabel freeze True)
  _ <- runFrame ctx inp (tableAndLabel freeze True)
  (mResp, _, _, _) <- runFrame ctx inp (tableAndLabel freeze True)
  _ <- takeDamage ctx
  assertJust failed mResp $ \resp -> do
    let old = respRect resp
    assert failed (rectW old > 0 && rectH old > 0)
    _ <- runFrame ctx inp (tableAndLabel freeze False)
    dmg <- takeDamage ctx
    assert failed (damageCovers dmg old)
    assertEq failed Nothing =<< getPrevRect ctx (respId resp)

-- | While such a table is on screen, a frame where nothing moved or went away
-- keeps last frame's rect map rather than rebuilding it from nothing.
runSharedKeySteadyPrevRectsTest :: Context -> IORef Int -> IO ()
runSharedKeySteadyPrevRectsTest ctx failed = do
  let inp = withInputOff 320 300
  replicateM_ 3 (runFrame ctx inp (tableAndLabel 1 True))
  !before <- getsDamage ctx dsPrevRects
  _ <- runFrame ctx inp (tableAndLabel 1 True)
  !after <- getsDamage ctx dsPrevRects
  assert failed (not (null before))
  assert failed (ptrEq before after)

-- | Every widget of a frame resolves to its own node, past the index's first
-- size, and a widget the next frame leaves out resolves to nothing.
runWidgetIdIndexLookupsTest :: Context -> IORef Int -> IO ()
runWidgetIdIndexLookupsTest ctx failed = do
  let inp = withInputOff 800 600
      na = ctxNodeArena ctx
      ui n = column (forM [1 .. n :: Int] (\i -> withKey i (button' "b")))
      resolvesToItself r = do
        mIdx <- lookupNodeByWidgetId na (respId r)
        maybe (pure False) (fmap (== respId r) . getWidgetId na) mIdx
  (resps, _, _, _) <- runFrame ctx inp (ui 3000)
  assertEq failed 3000 (length resps)
  assert failed . and =<< mapM resolvesToItself resps
  _ <- runFrame ctx inp (ui 10)
  assert failed . and =<< mapM resolvesToItself (take 10 resps)
  assert failed . all null =<< mapM (lookupNodeByWidgetId na . respId) (drop 10 resps)

-- | When the frame epoch wraps, entries written the last time the epoch had
-- its new value do not come back.
runWidgetIdIndexEpochWrapTest :: Context -> IORef Int -> IO ()
runWidgetIdIndexEpochWrapTest ctx failed = do
  let inp = withInputOff 800 600
      na = ctxNodeArena ctx
  writeIORef (naEpoch na) maxBound
  gone <- fst4 <$> runFrame ctx inp (withKey (1 :: Int) (button' "gone"))
  assert failed . not . null =<< lookupNodeByWidgetId na (respId gone)
  writeIORef (naEpoch na) maxBound
  _ <- runFrame ctx inp (withKey (2 :: Int) (label "other"))
  assertEq failed 1 =<< readIORef (naEpoch na)
  assertEq failed Nothing =<< lookupNodeByWidgetId na (respId gone)
  where
    fst4 (a, _, _, _) = a

tableAndLabel :: Int -> Bool -> NanoUI (Maybe Response)
tableAndLabel freeze showLabel = column $ do
  (sortState, _) <- useTableSort (SortCol 0 SortAsc)
  _ <- tableConfigured defaultTableConfig {tableFreezeCols = freeze} (fixedH 150) "people" cols rows sortState
  if showLabel then Just <$> label' "bye" else pure Nothing
  where
    cols :: Colonnade Headed (T.Text, T.Text) T.Text
    cols = headed "Name" fst <> headed "Value" snd
    rows = [("row-" <> n, "val-" <> n) | i <- [1 .. 20 :: Int], let n = T.pack (show i)]

damageCovers :: Damage -> Rect -> Bool
damageCovers DamageFull _ = True
damageCovers (DamageClip clip) r = covers clip r
