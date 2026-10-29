-- | Code for the book's Layout chapter (@book/src/layout/index.md@) that has
-- no picture. The pictured examples are in
-- @nano-ui-svg/app/LayoutExamples.hs@.
module Book.Layout where

import Control.Monad (when)
import Data.Foldable (for_)
import Data.Text (Text)
import NanoUI
import NanoUI.Monad (askContext)

-- ANCHOR: modifiers
settingsPanel :: NanoUI ()
settingsPanel =
  columnWith (gap 8 . padAll 12 . fillW) $ do
    label "Settings"
    whenM (buttonWith (fixedW 120) "Apply") apply
-- ANCHOR_END: modifiers

-- ANCHOR: scroll
itemList :: [Text] -> NanoUI ()
itemList items =
  scrollWith (fixedH 300 . fillW) $
    column $ for_ items label
-- ANCHOR_END: scroll

-- ANCHOR: virtualized
virtualizedList :: [Item] -> NanoUI ()
virtualizedList items = do
  sid <- currentId
  scrollWith (fixedH 500 . fillW) $ do
    metrics <- getScrollMetricsUi sid
    let rowH = 28
        total = length items
        (offsetY, viewH) = case metrics of
          Nothing -> (0, 0)
          Just m  -> (v2Y (scrollOffset m), rectH (scrollViewport m))
        firstVisible = max 0 (floor (offsetY / rowH))
        lastVisible  = min total (firstVisible + ceiling (viewH / rowH) + 2)
        topH    = fromIntegral firstVisible * rowH
        bottomH = fromIntegral (total - lastVisible) * rowH
        visibleRows = take (lastVisible - firstVisible) (drop firstVisible items)
    columnWith (tight . fillW) $ do
      when (topH > 0) (spacer Fit (Fixed topH))
      for_ visibleRows $ \item ->
        withKey (itemId item) $
          labelWith (fixedH rowH . fillW) (itemTitle item)
      when (bottomH > 0) (spacer Fit (Fixed bottomH))
-- ANCHOR_END: virtualized

-- ANCHOR: reveal
reveal :: WidgetId -> Float -> Int -> Bool -> NanoUI ()
reveal sid rowH index shouldReveal =
  when shouldReveal $
    scrollRectIntoViewUi sid (Rect 0 (fromIntegral index * rowH) 1 rowH) ScrollNearest ScrollInstant
-- ANCHOR_END: reveal

-- ANCHOR: wheel_step
oneRowPerNotch :: Float -> NanoUI ()
oneRowPerNotch rowHeight = do
  ctx <- askContext
  liftIO $ setScrollTuning ctx defaultScrollTuning
    {scrollWheelStep = rowHeight, scrollSmoothTime = 0}
-- ANCHOR_END: wheel_step

-- Stand-ins for application code the snippets refer to.

data Item = Item {itemId :: Int, itemTitle :: Text}

apply :: NanoUI ()
apply = pure ()
