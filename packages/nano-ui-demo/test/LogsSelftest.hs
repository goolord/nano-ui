-- | The log viewer on a hidden window: pinning while entries arrive,
-- scrolling back, Jump to Bottom, filters, horizontal scroll, Select All,
-- copying, and clearing the selection.
module LogsSelftest
  ( selftest
  ) where

import Data.Text qualified as T
import DemoApp (withHiddenWindow)
import Drive
import Logs (appendEntries, logsApp, newAppState)
import NanoUI
import NanoUI.Internal.Context (Context (..), getPrevRect)
import NanoUI.Shortcut

selftest :: IO ()
selftest = withHiddenWindow 1000 700 (V2 500 350) id $ \ctx env base -> do
  state <- newVar =<< newAppState 60 False
  scroller <- newVar Nothing
  let append n = liftIO (readVarIO state >>= (`appendEntries` n) >>= writeVarIO state) >> frame >> frame
      scrollerId = liftIO (readVarIO scroller) >>= maybe (fail "no scroller WidgetId") pure
      -- A row is one selectable span: id, timestamp, level, service and message.
      isRow txt = "#00" `T.isPrefixOf` txt && " [" `T.isInfixOf` txt
      rows = (\ss -> [(r, t) | (r, t, _, _, _) <- ss, isRow t]) <$> spans
  runDrive ctx env base (logsApp state scroller) $ do
    frame
    frame
    expect "Log Viewer"
    -- "● PINNED", as "⏸ UNPINNED" holds "PINNED" too.
    expect "● PINNED"
    rows >>= check "no full log line among the selectable spans" . not . null
    append 40
    expect "● PINNED"
    -- Pinned is the scroller's own end, so the last row clears the
    -- horizontal bar.
    metrics <- liftIO . getScrollMetrics ctx =<< scrollerId
    check ("a PINNED view should sit at the scroller's end: " <> show metrics) $
      fmap (\m -> v2Y (scrollOffset m) == v2Y (scrollRange m)) metrics == Just True
    wheelAt (V2 500 350) (V2 0 (-15))
    expect "UNPINNED"
    -- Entries arriving while scrolled back leave the view where it is.
    append 50
    expect "UNPINNED"
    click "Jump to Bottom"
    expect "● PINNED"
    -- A filter applies in the frame that clicks it, and the view clamps so
    -- the fewer rows stay visible; the header counts the rows it hides.
    click "WARN"
    expect " filtered]"
    warned <- map snd <$> rows
    check "the WARN filter left no rows, or rows of other levels" $
      not (null warned) && all ("[WARN ]" `T.isInfixOf`) warned
    click "ALL"
    wheelAt (V2 500 350) (V2 20 0)
    offset <- liftIO . getScrollOffset2D ctx =<< scrollerId
    check "horizontal scroll did not move the horizontal offset" (v2X offset > 0)
    click "Select All"
    selected <- (||) <$> found "ALL LOGS SELECTED" <*> found "Deselect All"
    check "Select All did not select the logs" selected
    chord (cmdOrCtrl <> key 'c')
    clip <- liftIO (ctxClipboardGet ctx)
    check "Ctrl+C after Select All did not copy every selected line" $
      maybe False (\t -> "#00" `T.isPrefixOf` t && "\n" `T.isInfixOf` t) clip
    pressKey KeyEscape
    expect "Select All"
    -- A click on a visible row clears Select All.
    click "Select All"
    port <- liftIO . getPrevRect ctx =<< scrollerId
    visible <- (\rs -> [v | (r, _) <- rs, Just v <- [port >>= (`rectIntersect` r)], rectW v > 20]) <$> rows
    case visible of
      v : _ -> clickAt (V2 (rectX v + 4) (rectY v + rectH v / 2))
      [] -> check "no visible log row to click" False
    expect "Select All"
  putStrLn "logs selftest: ok"
  where
    found needle = (any (\(_, t, _, _, _) -> needle `T.isInfixOf` t) <$> spans)
