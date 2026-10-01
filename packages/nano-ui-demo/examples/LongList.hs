-- | LongList: how do I show 100,000 rows? Build only the ones on screen.
--
-- The simple way to show a list is to build every row inside 'scroll':
--
-- > scroll (forM_ items (\item -> withKey (itemId item) (itemRow item)))
--
-- That is the right choice for a few hundred rows. But a view runs every
-- frame it is woken, and every row it declares is a layout node to lay out,
-- so 100,000 rows would cost 100,000 nodes on every hover and keypress.
--
-- A virtualized list builds only the rows the viewport shows. With rows of
-- a fixed height, row @i@ sits at @i * rowH@ in the content, so the scroller's
-- offset and viewport height say which rows are visible. 'getScrollMetricsUi'
-- reports them, keyed by the scroller's id, which 'currentId' gives before
-- the scroller is declared. Two spacers stand in for the rows above and
-- below, so the content keeps its full height and the scrollbar its size.
--
-- Rows that do not exist cannot be scrolled to with 'scrollIntoView', so the
-- keyboard selection and "Jump to row" use 'scrollRectIntoViewUi' with the
-- row's rectangle in content coordinates instead.
--
-- Run it with @cabal run nano-ui-example-long-list@.
module Main (main) where

import Data.Foldable (for_)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import NanoUI
import NanoUI.Backend.Sdl (SdlOptions (..), defaultSdlOptions, runSdlApp)

rowCount :: Int
rowCount = 100000

-- | Every row has this height, which is what makes the arithmetic work.
rowH :: Float
rowH = 26

-- | Row @i@'s place in the content. The scroller and its column are 'tight',
-- so the content starts at the first row.
rowRect :: Int -> Rect
rowRect i = Rect 0 (fromIntegral i * rowH) 1 rowH

main :: IO ()
main =
  runSdlApp
    defaultSdlOptions
      { sdlAppShouldQuit = pressedOnceIn KeyEscape
      , sdlWindowSettings = defaultWindowSettings {wsTitle = "Long list", wsSize = Size 560 640}
      }
    view

view :: NanoUI ()
view = columnWith (padAll 12 . gap 8 . grow) $ do
  (selected, setSelected) <- useInt 0
  (target, setTarget) <- useInt 1

  jump <- rowWith (tight . gap 8 . alignMid . fillW) $ do
    label "Jump to row"
    let cfg = defaultNumericInputConfig {nicMin = 1, nicMax = fromIntegral rowCount}
    (field, value) <- numericInputConfigured' cfg (fromIntegral target)
    setTarget (round value)
    go <- button "Go"
    muted ("Selected: row " <> T.pack (show (selected + 1)))
    pure (go || respSubmitted field)

  -- Keys no widget took: while the number field is focused, Up and Down
  -- step its value instead and these stay False.
  up <- keyPressed KeyUp
  down <- keyPressed KeyDown
  pageUp <- keyPressed KeyPageUp
  pageDown <- keyPressed KeyPageDown

  -- Nothing may be declared between currentId and scrollArea, or the
  -- scroller would take a different id than the one read here.
  sid <- currentId
  before <- getScrollMetricsUi sid
  let page = maybe 10 (\m -> max 1 (floor (rectH (scrollViewport m) / rowH) - 1)) before
      step
        | up = Just (-1)
        | down = Just 1
        | pageUp = Just (-page)
        | pageDown = Just page
        | otherwise = Nothing
      -- A row to select and how to bring it into view, if anything asked.
      command
        | jump = Just (clampRow (target - 1), ScrollCenter)
        | otherwise = (\d -> (clampRow (selected + d), ScrollNearest)) <$> step
      current = maybe selected fst command
  -- An instant command moves the offset now, before the rows are picked.
  for_ command $ \(goTo, align) -> do
    setSelected goTo
    scrollRectIntoViewUi sid (rowRect goTo) align ScrollInstant

  -- Read the metrics again, after the command, so the rows built are the
  -- ones the new offset shows. (Wheel and scrollbar drags are applied before
  -- the view runs, so they need nothing extra.) Before the first layout
  -- there are no metrics; build a screenful from the top.
  metrics <- getScrollMetricsUi sid
  let (firstRow, lastRow) = case metrics of
        Nothing -> (0, 40)
        Just m ->
          let top = v2Y (scrollOffset m)
              height = rectH (scrollViewport m)
           in (max 0 (floor (top / rowH)), min (rowCount - 1) (ceiling ((top + height) / rowH)))
  highlight <- themeSelection <$> uiTheme
  (_, clicked) <- scrollArea (tight . grow) $
    columnWith (tight . gap 0 . fillW) $ do
      spacer Fit (Fixed (fromIntegral firstRow * rowH))
      -- Keyed by row number, so a row keeps its widget ids as it scrolls.
      picks <- traverse (\i -> withKey i (listRow highlight (i == current) i)) [firstRow .. lastRow]
      spacer Fit (Fixed (fromIntegral (rowCount - 1 - lastRow) * rowH))
      pure (listToMaybe [i | (i, True) <- zip [firstRow ..] picks])
  for_ clicked setSelected
  where
    clampRow = max 0 . min (rowCount - 1)

-- | One row: a 'mouseArea' for the click, around a panel whose background
-- shows the selection. The panel is always there, transparent when not
-- selected, so the row's widgets are the same on every frame.
listRow :: Color -> Bool -> Int -> NanoUI Bool
listRow highlight isSelected i = do
  let bg = if isSelected then highlight else colorTransparent
  (_, area) <- mouseArea (fillW . fixedH rowH) $
    styled (panelStyle (background bg . borderWidth 0 . cornerRadius 4)) $
      panelWith (asRow . tight . grow . padXY 8 0 . gap 12 . alignMid) $ do
        labelWith (fontMono . fontMuted . fixedW 64) (T.pack (show (i + 1)))
        label ("Item " <> T.pack (show (i + 1)))
        labelWith (fontMuted . alignEnd . fillW) (T.pack (show ((i * 7919) `mod` 10007)))
  pure (respClicked area)
  where
    -- A panel stacks its children in a column unless told otherwise.
    asRow l = l {layoutDirection = Row}
