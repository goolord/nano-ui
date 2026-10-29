-- | Code for the book's Async Work & Animation chapter
-- (@book/src/advanced/async.md@).
module Book.Async where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (displayException)
import Control.Monad (forM_, void)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import NanoUI

-- ANCHOR: handles
data App = App
  { appSearch :: Task Text [Text]
  , appFile :: Task Text Text
  , appProgress :: Stream () Int
  }

newApp :: IO App
newApp = App <$> newTask <*> newTask <*> newStream
-- ANCHOR_END: handles

-- ANCHOR: task_status
results :: App -> Text -> NanoUI ()
results app queryKey = do
  status <- useTaskStatus (appSearch app) queryKey (fetchSearchResults queryKey)

  case status of
    TaskRunning previous -> showPreviousOrSpinner previous
    TaskDone found       -> renderResults found
    TaskFailed err _     -> danger (T.pack (displayException err))
-- ANCHOR_END: task_status

-- ANCHOR: use_task
fileView :: App -> Text -> NanoUI ()
fileView app path = do
  contents <- useTask (appFile app) path (T.readFile (T.unpack path))
  label (fromMaybe "Loading..." contents)
-- ANCHOR_END: use_task

-- ANCHOR: stream
progress :: App -> NanoUI ()
progress app = do
  pct <- useStream (appProgress app) () (0 :: Int) $ \update ->
    forM_ [1 .. 100] $ \i -> do
      threadDelay 50000
      update (const i)
  progressBar (fromIntegral pct / 100)
-- ANCHOR_END: stream

-- ANCHOR: wake
refreshInBackground :: Var [Text] -> NanoUI ()
refreshInBackground queryResult = do
  wake <- askWake -- in the view, once

  void . liftIO . forkIO $ do
    result <- expensiveQuery
    writeVarIO queryResult result
    wake -- schedule one frame
-- ANCHOR_END: wake

-- ANCHOR: animate_to
sidebar :: Float -> NanoUI ()
sidebar targetWidth = do
  currentWidth <- animateTo (Tween EaseOutQuad 0.25 0) targetWidth
  panelWith (fixedW currentWidth . fillH) (label "Sidebar")
-- ANCHOR_END: animate_to

-- ANCHOR: spring
follow :: Float -> Float -> NanoUI V2
follow targetX targetY = animateToA (Spring presetSmooth) (V2 targetX targetY)
-- ANCHOR_END: spring

-- ANCHOR: pulse
fading :: NanoUI Float
fading = pulse 1.2
-- ANCHOR_END: pulse

-- ANCHOR: keep_animating
breathingDot :: NanoUI ()
breathingDot = do
  t <- pulse 1.2
  (resp, ()) <- customWidget defaultCustomWidgetSpec
    { widgetLayout = fixedWH 24 24 defaultLayout
    , widgetContent = contentKey [t * 100]
    , widgetDraw = \cdc (Rect x y w h) -> runCanvasFor cdc $
        drawCircle (V2 (x + w / 2) (y + h / 2)) (6 + 3 * t) (themeAccent (cdcTheme cdc))
    }
  keepAnimating resp
-- ANCHOR_END: keep_animating

-- ANCHOR: wake_after
clock :: NanoUI ()
clock = do
  label =<< currentTimeText
  wakeAfter 1
-- ANCHOR_END: wake_after

-- Stand-ins for application code the snippets refer to.

fetchSearchResults :: Text -> IO [Text]
fetchSearchResults q = pure [q]

showPreviousOrSpinner :: Maybe [Text] -> NanoUI ()
showPreviousOrSpinner = maybe (label "Searching...") renderResults

renderResults :: [Text] -> NanoUI ()
renderResults = mapM_ label

expensiveQuery :: IO [Text]
expensiveQuery = pure []

currentTimeText :: NanoUI Text
currentTimeText = pure "12:00"
