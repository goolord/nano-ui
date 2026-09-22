-- | Expandable tree rows with a controlled pre-order selection index.
module NanoUI.Internal.Widgets.Tree (TreeItem (..), tree, tree') where

import Control.Monad (zipWithM)
import Data.IORef (writeIORef)
import Data.Foldable (asum, find, fold, toList)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Primitive.SmallArray (SmallArray, indexSmallArray, sizeofSmallArray, smallArrayFromList)
import Effectful (Eff, type (:>))
import qualified Data.IntSet as IS
import NanoUI.Internal.Context (Context (..), adoptSlot, getPrevRect, getStore, intKey, registerFocusable, writeSlots)
import NanoUI.Internal.Font (treeChevronRect)
import NanoUI.Internal.Id (WidgetId (..), hashWidgetId)
import NanoUI.Internal.Input (inputMousePos)
import NanoUI.Internal.Layout.Arena (NodeType (..))
import NanoUI.Internal.Store (fieldInt, fieldIntSet, lookupSlot, slotWrite)
import NanoUI.Internal.Monad (Ui, askContext, askInput, focusedWidget, nextId, uiIO, withKey)
import NanoUI.Internal.Style (defaultLayout, fillW, gap, tight)
import NanoUI.Internal.Types (Rect (..), clamp, rectContains)
import NanoUI.Internal.WidgetText (treeEncodeStyle)
import NanoUI.Internal.Widgets.Behavior (KeyNav (..), useKeyNav)
import NanoUI.Internal.Widgets.Combinators (finishInput)
import NanoUI.Internal.Widgets.Layout (columnWith)
import NanoUI.Internal.Widgets.Node (Response (..), addWidgetStyled, tagContainer)

-- | Label and child items for a tree row. An empty child list makes a leaf.
data TreeItem = TreeItem {treeItemLabel :: !Text, treeItemChildren :: ![TreeItem]}
  deriving (Eq, Show)

-- | A visible row: pre-order node index, depth, whether it has children, label.
type TreeRow = (Int, Int, Bool, Text)

-- | Nodes in a subtree, its root included.
subtreeSize :: TreeItem -> Int
subtreeSize item = 1 + forestSize (treeItemChildren item)

forestSize :: [TreeItem] -> Int
forestSize = foldl' (\acc x -> acc + subtreeSize x) 0

-- | Visible rows in pre-order, skipping the children of nodes @expanded@
-- rejects. One pass: rows come out in order, and a subtree hands the next
-- pre-order index to the continuation that lists its later siblings.
visibleRows :: (Int -> Bool) -> [TreeItem] -> SmallArray TreeRow
visibleRows expanded items = smallArrayFromList (go 0 0 items (const []))
  where
    go !idx !_ [] k = k idx
    go !idx !depth (item@(TreeItem lbl kids) : rest) k =
      let hasKids = not (null kids)
       in (idx, depth, hasKids, lbl)
            : if hasKids && expanded idx
              then go (idx + 1) (depth + 1) kids (\next -> go next depth rest k)
              else go (idx + subtreeSize item) depth rest k

treeKeyNav ::
  KeyNav ->
  SmallArray TreeRow ->
  SmallArray Response ->
  WidgetId ->
  Int ->
  IS.IntSet ->
  (Int, IS.IntSet, Maybe WidgetId)
treeKeyNav nav rows resps focus selected expanded
  | hashWidgetId focus == 0 || not moving = stay
  | otherwise = maybe stay (\pos -> step pos (rowAt pos)) (find ((== focus) . widAt) [0 .. n - 1])
 where
  moving = knUp nav || knDown nav || knLeft nav || knRight nav || knEnter nav || knSpace nav
  n = sizeofSmallArray rows
  rowAt = indexSmallArray rows
  widAt i = rawRespId (indexSmallArray resps i)
  stay = (selected, expanded, Nothing)
  moveTo p = let (idx, _, _, _) = rowAt p in (idx, expanded, Just (widAt p))
  expandTo s = (selected, s, Nothing)
  step pos (nodeIdx, depth, hasKids, _)
    | knDown nav, pos + 1 < n = moveTo (pos + 1)
    | knUp nav, pos > 0 = moveTo (pos - 1)
    | knEnter nav || knSpace nav, hasKids = expandTo (toggle nodeIdx expanded)
    | knRight nav, hasKids, not open = expandTo (IS.insert nodeIdx expanded)
    | knLeft nav, hasKids, open = expandTo (IS.delete nodeIdx expanded)
    | knLeft nav, depth > 0 =
        maybe (nodeIdx, expanded, Nothing) moveTo $
          find (\i -> let (_, d, _, _) = rowAt i in d < depth) [pos - 1, pos - 2 .. 0]
    | otherwise = stay
   where
    open = IS.member nodeIdx expanded

toggle :: Int -> IS.IntSet -> IS.IntSet
toggle idx s = if IS.member idx s then IS.delete idx s else IS.insert idx s

-- | One visible row, and the selection and expansion its click asks for: a
-- click on a parent's chevron toggles it and keeps the selection.
treeRow ::
  (Ui :> es) => Int -> TreeRow -> Int -> IS.IntSet -> Eff es (Response, Maybe (Int, IS.IntSet))
treeRow rowIdx (nodeIdx, depth, hasKids, lbl) selected expanded = do
  ctx <- askContext
  inp <- askInput
  wid <- nextId
  let style = treeEncodeStyle nodeIdx depth hasKids (IS.member nodeIdx expanded) (odd rowIdx)
      value = if selected == nodeIdx then 1 else 0
  resp <- addWidgetStyled wid NodeTree lbl value (tight . fillW $ defaultLayout) style
  uiIO $ registerFocusable ctx wid
  if not (rawRespClicked resp)
    then pure (resp, Nothing)
    else uiIO $ do
      mrect <- getPrevRect ctx wid
      let mouse = inputMousePos inp
          onChevron = case mrect of
            Just rect@(Rect x y _ h) ->
              rectContains (treeChevronRect (ctxFontMetrics ctx) x y h depth) mouse
                && rectContains rect mouse
            _ -> False
      pure . (resp,) . Just $
        if hasKids && onChevron then (selected, toggle nodeIdx expanded) else (nodeIdx, expanded)

-- | Collapsible tree. Rows are numbered in pre-order; pass the selected row
-- and the result is the selection after this frame's click or arrow keys.
-- Expansion is kept by the widget. @key@ distinguishes trees in one scope.
{-# INLINE tree #-}
tree :: (Foldable f, Ui :> es) => Text -> f TreeItem -> Int -> Eff es Int
tree key items index = snd <$> tree' key items index

-- | 'tree' returning its response and selected pre-order item index.
tree' :: (Foldable f, Ui :> es) => Text -> f TreeItem -> Int -> Eff es (Response, Int)
tree' key inputItems index =
  withKey ("tree:" <> key) $ do
    groupId <- nextId
    ctx <- askContext
    let items = toList inputItems
        groupKey = intKey groupId
        total = forestSize items
        clamped = if total <= 0 then 0 else clamp 0 (total - 1) index
        -- Every parent starts expanded.
        allParents = IS.fromList [i | (i, _, True, _) <- toList (visibleRows (const True) items)]
    selected <- uiIO $ adoptSlot fieldInt ctx groupId groupKey clamped
    expandedSet <- fromMaybe allParents . lookupSlot fieldIntSet groupKey <$> uiIO (getStore ctx)
    let rows = visibleRows (`IS.member` expandedSet) items
    columnWith (tight . gap 0 . fillW) $ do
      tagContainer groupId
      results <-
        zipWithM
          (\rowIdx row@(i, _, _, _) -> withKey i (treeRow rowIdx row selected expandedSet))
          [0 ..]
          (toList rows)
      let resps = smallArrayFromList (map fst results)
          (clickSel, clickExp) = fromMaybe (selected, expandedSet) (asum (map snd results))
      focus <- focusedWidget
      nav <- useKeyNav focus
      let (keySel, keyExp, mFocus) = treeKeyNav nav rows resps focus clickSel clickExp
      result <- finishInput fieldInt ctx groupId groupKey selected (fold resps) keySel
      uiIO (writeSlots ctx (slotWrite fieldIntSet groupKey keyExp))
      mapM_ (uiIO . writeIORef (ctxFocusId ctx)) mFocus
      pure result
