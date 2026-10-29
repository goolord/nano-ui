{-# OPTIONS_GHC -O0 #-}
module Cases.WidgetIdIndex (tests) where

import Spec
import Data.Text qualified as T
import NanoUI.Internal.Context (Context (..), DamageState (..), PrevFrame (..), getsDamage)
import NanoUI.Internal.Layout.Arena (NodeArena (..), getWidgetId, lookupNodeByWidgetId)
import NanoUI.Internal.Store (ptrEq)

tests :: [Spec]
tests =
  [ spec "shared-key-prev-rects" runSharedKeyPrevRectsTest
  , spec "widget-id-index-lookups" runWidgetIdIndexLookupsTest
  , spec "widget-id-index-epoch-wrap" runWidgetIdIndexEpochWrapTest
  , spec "width-memo-frame-tag-wrap" runWidthMemoFrameTagWrapTest
  ]

inp :: Input
inp = withInputOff 320 300

-- | With a table shown, with or without a frozen column (two panes under one
-- id), an unchanged frame reuses last frame's rect map, and a label removed
-- after the table repaints its old area and drops out of the previous rects.
runSharedKeyPrevRectsTest :: Context -> IORef Int -> IO ()
runSharedKeyPrevRectsTest _ failed = forM_ [0, 1] $ \freeze -> do
  ctx <- newContext
  Just resp <- last <$> replicateM 3 (evalUi ctx inp (tableAndLabel freeze True))
  let old = respRect resp
      frameRects = runFrame ctx inp (tableAndLabel freeze True) >> getsDamage ctx (pfRects . dsPrev)
  assert failed (rectW old > 0 && rectH old > 0)
  !before <- frameRects
  !after <- frameRects
  assert failed (not (null before) && ptrEq before after)
  _ <- runFrame ctx inp (tableAndLabel freeze False)
  assert failed . (`damageCovers` old) =<< takeDamage ctx
  assertEq failed Nothing =<< getPrevRect ctx (respId resp)

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

-- | A wrapping frame tag does not revive width memo entries from the
-- previous lap: text at the same node and width measures afresh.
runWidthMemoFrameTagWrapTest :: Context -> IORef Int -> IO ()
runWidthMemoFrameTagWrapTest ctx failed = do
  let ui = columnWith tight . labelWith (\l -> l {layoutWidth = Fixed 100})
      lapFrame txt = writeIORef (naFrameTag (ctxNodeArena ctx)) maxBound >> runFrame ctx inp (ui txt) >> arenaRects ctx
  _ <- lapFrame "a long label that wraps onto several lines"
  lapped <- lapFrame "a\nb"
  fresh <- newContext
  expected <- runFrame fresh inp (ui "a\nb") >> arenaRects fresh
  assertEq failed expected lapped

-- | A table with @freeze@ frozen columns, and a label after it while @showLabel@.
tableAndLabel :: Int -> Bool -> NanoUI (Maybe Response)
tableAndLabel freeze showLabel = column $ do
  let rows = [(n, n) | n <- map (T.pack . show) [1 .. 20 :: Int]]
  _ <- tableConfigured' defaultTableConfig {tableFreezeCols = freeze} (fixedH 150) "people" (headed "Name" fst <> headed "Value" snd) rows (constVar (SortCol 0 SortAsc))
  if showLabel then Just <$> label' "bye" else pure Nothing
