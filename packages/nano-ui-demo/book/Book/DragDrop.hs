-- | Code for the book's Drag & Drop chapter
-- (@book/src/widgets/drag-and-drop.md@).
module Book.DragDrop where

import Control.Monad (forM, void, when)
import Data.Foldable (for_)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import NanoUI

-- ANCHOR: reorder
chips :: Var [Int] -> NanoUI ()
chips orderVar = rowWith (gap 6) $ do
  order <- readVar orderVar -- orderVar <- newVar [0 .. 4] in setup
  drawn <- forM order $ \i -> (i,) <$> chip i
  r <- useReorder order drawn
  writeVar orderVar (reorderOrder r)
-- ANCHOR_END: reorder

-- ANCHOR: tab_drag
tabStrip :: DragHandle Text -> Var Text -> [Tab Text ()] -> NanoUI ()
tabStrip dragHandle active documents = do
  headers <- tabBar' active documents
  gesture <- useDrag dragHandle (tabHeaders headers)
  for_ gesture $ \d -> when (dragPhase d == DragReleased) $ do
    let rest = filter ((/= dragPayload d) . fst) (tabHeaders headers)
    for_ (insertionIndex DragAxisX (tabStripRect headers) (map (respRect . snd) rest) (dragAt d)) $
      moveTab (dragPayload d)
-- ANCHOR_END: tab_drag

-- ANCHOR: drop_zone
fileDrop :: NanoUI ()
fileDrop = do
  (_, _, target) <- dropZone (fillW . padAll 12) (label "Drop files here")
  when (dropReceived target) (mapM_ openFile (dropFiles target))
-- ANCHOR_END: drop_zone

-- ANCHOR: pane_grid
dockPanes :: PaneGridConfig -> Maybe (Drag Word64) -> NanoUI ()
dockPanes cfg gesture = do
  panes <- paneGrid cfg
  case (gesture, pgrDropTarget panes) of
    (Just d, Just target) | dragPhase d == DragReleased -> void (commitPaneDrop target)
    _ -> pure ()
-- ANCHOR_END: pane_grid

-- Stand-ins for application code the snippets refer to.

chip :: Int -> NanoUI Response
chip i = button' (T.pack (show i))

moveTab :: Text -> Int -> NanoUI ()
moveTab _ _ = pure ()

openFile :: Text -> NanoUI ()
openFile _ = pure ()
