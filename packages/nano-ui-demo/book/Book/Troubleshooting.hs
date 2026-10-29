-- | Code for the book's Troubleshooting chapter
-- (@book/src/troubleshooting/recurring-bugs.md@).
module Book.Troubleshooting where

import Control.Monad (unless)
import Data.Primitive.SmallArray (SmallArray)
import Data.Text (Text)
import Data.Text qualified as T
import NanoUI
import NanoUI.Backend (FontMetrics (..))
import NanoUI.Shortcut (key)

-- ANCHOR: fixed_width
count :: Int -> NanoUI ()
count n = labelWith (fixedW 48 . alignEnd) (T.pack (show n))
-- ANCHOR_END: fixed_width

-- ANCHOR: name_line
-- In a row, a label too narrow for its text cuts it off rather than wrapping.
nameLine :: (Layout -> Layout) -> Text -> NanoUI ()
nameLine f = rowWith (fillW . tight) . labelWith (fillW . f)
-- ANCHOR_END: name_line

-- ANCHOR: measured_select
diskSelect :: [Text] -> Var Int -> NanoUI ()
diskSelect options selectedVar = do
  fm <- uiFontMetrics
  let w = maximum (0 : map (lineWidth fm) options) + 56
  selectWith (fixedW w) options selectedVar
-- ANCHOR_END: measured_select

-- ANCHOR: strip_chrome
flatList :: NanoUI () -> NanoUI ()
flatList = styled (inputStyle (background colorTransparent . borderWidth 0))
-- ANCHOR_END: strip_chrome

-- ANCHOR: run_width
runWidth :: FontMetrics -> Text -> Float
runWidth fm run = lineWidth fm run
-- ANCHOR_END: run_width

-- ANCHOR: snap
snap :: FontMetrics -> Float -> Float
snap f v = let k = max 1 (fmSnapScale f) in fromIntegral (round (v * k) :: Int) / k
-- ANCHOR_END: snap

-- ANCHOR: sort_arrow
sortArrow :: Int -> Bool -> Float -> NanoUI Response
sortArrow col ascending scrollY =
  drawingVersioned (contentKey [fromIntegral col, if ascending then 1 else 0, scrollY]) (fixedWH 12 12) arrowOps
-- ANCHOR_END: sort_arrow

-- ANCHOR: focus_at_end
appView :: Var Text -> NanoUI ()
appView query = do
  searchResp <- searchInput' "Search" query
  -- ... all widgets ...
  searching <- isFocused (respId searchResp)
  unless searching $ whenM (shortcut (key KeyDelete)) deleteSelected
-- ANCHOR_END: focus_at_end

-- Stand-ins for application code the snippets refer to.

arrowOps :: Rect -> SmallArray DrawOp
arrowOps _ = mempty

deleteSelected :: NanoUI ()
deleteSelected = pure ()
