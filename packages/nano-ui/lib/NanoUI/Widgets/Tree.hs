{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module NanoUI.Widgets.Tree (TreeItem (..), tree, tree') where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.IORef (writeIORef)
import Data.Foldable (fold, toList)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Primitive.SmallArray (SmallArray, indexSmallArray, mapSmallArray', sizeofSmallArray, smallArrayFromList)
import Effectful (Eff, type (:>))
import qualified Data.IntSet as IS
import NanoUI.Context (Context (..), adoptStoreInt, getFocusId, intKey, recordStoreInt, registerFocusable)
import NanoUI.Font (treeChevronRect)
import NanoUI.Frame.Hit (scrollHitRect)
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Input (inputMousePos)
import NanoUI.Layout.Arena (NodeType (..))
import NanoUI.Monad (Ui, askContext, askInput, nextId, uiIO, withKey)
import NanoUI.Style (defaultLayout, fillW, gap, tight)
import NanoUI.Types (Rect (..), clamp, rectContains)
import NanoUI.WidgetText (treeEncodeStyle)
import NanoUI.Widgets.Behavior (KeyNav (..), ensureInt, ensureIntSet, putInt, putIntSet, useKeyNav)
import NanoUI.Widgets.Combinators (selectableItem)
import NanoUI.Widgets.Layout (columnWith)
import NanoUI.Widgets.Node (Response (..), setChanged, tagContainer)

data TreeItem = TreeItem {treeItemLabel :: !Text, treeItemChildren :: ![TreeItem]}
  deriving (Eq, Show)

-- | A visible row: pre-order node index, depth, whether it has children, label.
type TreeRow = (Int, Int, Bool, Text)

-- | Nodes in a subtree, its root included.
subtreeSize :: TreeItem -> Int
subtreeSize item = 1 + forestSize (treeItemChildren item)

forestSize :: [TreeItem] -> Int
forestSize = foldl' (\acc x -> acc + subtreeSize x) 0

-- | Visible rows in pre-order, skipping the children of collapsed nodes. One
-- pass: rows come out in order, and a subtree hands the next pre-order index
-- to the continuation that lists its later siblings.
visibleRows :: IS.IntSet -> [TreeItem] -> SmallArray TreeRow
visibleRows expanded items = smallArrayFromList (go 0 0 items (const []))
  where
    go !idx !_ [] k = k idx
    go !idx !depth (item@(TreeItem lbl kids) : rest) k =
      let hasKids = not (null kids)
       in (idx, depth, hasKids, lbl)
            : if hasKids && IS.member idx expanded
              then go (idx + 1) (depth + 1) kids (\next -> go next depth rest k)
              else go (idx + subtreeSize item) depth rest k

-- | Pre-order indices of every node that has children (the default expansion).
parentIndices :: [TreeItem] -> IS.IntSet
parentIndices items = snd (go 0 items IS.empty)
  where
    go !idx [] acc = (idx, acc)
    go !idx (TreeItem _ kids : rest) acc
      | null kids = go (idx + 1) rest acc
      | otherwise = case go (idx + 1) kids (IS.insert idx acc) of
          (next, acc') -> go next rest acc'

treeKeyNav ::
  KeyNav ->
  SmallArray TreeRow ->
  SmallArray Response ->
  WidgetId ->
  Int ->
  IS.IntSet ->
  (Int, IS.IntSet, Maybe WidgetId)
treeKeyNav nav rows resps focus selected expanded
  | hashWidgetId focus == 0 || not moving = (selected, expanded, Nothing)
  | otherwise = case [pos | pos <- [0 .. n - 1], widAt pos == focus] of
      pos : _ -> step pos (indexSmallArray rows pos)
      [] -> (selected, expanded, Nothing)
 where
  moving = knUp nav || knDown nav || knLeft nav || knRight nav || knEnter nav || knSpace nav
  n = sizeofSmallArray rows
  widAt i = rawRespId (indexSmallArray resps i)
  idxAt i = let (idx, _, _, _) = indexSmallArray rows i in idx
  wantToggle = knEnter nav || knSpace nav
  parentPosition pos depth = go (pos - 1)
    where
      go i
        | i < 0 = Nothing
        | otherwise =
            let (_, d, _, _) = indexSmallArray rows i
             in if d < depth then Just i else go (i - 1)
  step pos (nodeIdx, depth, hasKids, _)
    | knDown nav, pos + 1 < n = let p = pos + 1 in (idxAt p, expanded, Just (widAt p))
    | knUp nav, pos > 0 = let p = pos - 1 in (idxAt p, expanded, Just (widAt p))
    | wantToggle, hasKids = (selected, toggle nodeIdx expanded, Nothing)
    | knRight nav, hasKids, not (IS.member nodeIdx expanded) = (selected, IS.insert nodeIdx expanded, Nothing)
    | knLeft nav, hasKids, IS.member nodeIdx expanded = (selected, IS.delete nodeIdx expanded, Nothing)
    | knLeft nav, depth > 0 =
        case parentPosition pos depth of
          Just p -> (idxAt p, expanded, Just (widAt p))
          Nothing -> (nodeIdx, expanded, Nothing)
    | otherwise = (selected, expanded, Nothing)

toggle :: Int -> IS.IntSet -> IS.IntSet
toggle idx s = if IS.member idx s then IS.delete idx s else IS.insert idx s

treeRow :: (Ui :> es) => Int -> TreeRow -> Int -> IS.IntSet -> Eff es (Response, Maybe Int, Maybe IS.IntSet)
treeRow rowIdx (nodeIdx, depth, hasKids, lbl) selectedIdx expandedSet = do
  ctx <- askContext
  inp <- askInput
  let expanded = IS.member nodeIdx expandedSet
      selected = selectedIdx == nodeIdx
      isOdd = odd rowIdx
  resp <- selectableItem NodeTree lbl selected (tight . fillW $ defaultLayout) (treeEncodeStyle nodeIdx depth hasKids expanded isOdd)
  uiIO $ registerFocusable ctx (rawRespId resp)
  if not (rawRespClicked resp)
    then pure (resp, Nothing, Nothing)
    else uiIO $ do
      mrect <- scrollHitRect ctx (rawRespId resp)
      let mouse = inputMousePos inp
          onChevron = case mrect of
            Just rect@(Rect x y w h) ->
              rectContains (treeChevronRect (ctxFontMetrics ctx) x y w h depth) mouse
                && rectContains rect mouse
            _ -> False
      if hasKids && onChevron
        then pure (setChanged False resp, Nothing, Just (toggle nodeIdx expandedSet))
        else pure (setChanged (not selected) resp, Just nodeIdx, Nothing)

-- | Collapsible tree. Rows are numbered in pre-order; pass the selected row
-- and the result is the selection after this frame's click or arrow keys.
-- Expansion is kept by the widget. @key@ distinguishes trees in one scope.
{-# INLINE tree #-}
tree :: (Foldable f, Ui :> es) => Text -> f TreeItem -> Int -> Eff es Int
tree key items index = snd <$> tree' key items index

tree' :: (Foldable f, Ui :> es) => Text -> f TreeItem -> Int -> Eff es (Response, Int)
tree' key inputItems index =
  withKey ("tree:" <> key) $ do
    groupId <- nextId
    ctx <- askContext
    let items = toList inputItems
        groupKey = intKey groupId
        total = forestSize items
        clamped = if total <= 0 then 0 else clamp 0 (total - 1) index
    uiIO $ adoptStoreInt ctx groupId groupKey clamped
    selected <- ensureInt groupKey clamped
    expandedSet <- ensureIntSet groupKey (parentIndices items)
    let rows = visibleRows expandedSet items
    columnWith (tight . gap 0 . fillW) $ do
      tagContainer groupId
      results <-
        smallArrayFromList
          <$> sequence [withKey i (treeRow rowIdx row selected expandedSet) | rowIdx <- [0 .. sizeofSmallArray rows - 1], let row@(i, _, _, _) = indexSmallArray rows rowIdx]
      let resps = mapSmallArray' (\(r, _, _) -> r) results
          afterClickSel = fromMaybe selected (foldr (\(_, idx, _) rest -> idx <|> rest) Nothing results)
          afterClickExp = fromMaybe expandedSet (foldr (\(_, _, s) rest -> s <|> rest) Nothing results)
      focus <- uiIO (getFocusId ctx)
      nav <- useKeyNav focus
      let (keySel, keyExp, mFocus) = treeKeyNav nav rows resps focus afterClickSel afterClickExp
      when (keySel /= selected) $ putInt groupKey keySel
      uiIO $ recordStoreInt ctx groupKey keySel
      when (keyExp /= expandedSet) $ putIntSet groupKey keyExp
      maybe (pure ()) (\wid -> uiIO $ writeIORef (ctxFocusId ctx) wid) mFocus
      pure (setChanged (keySel /= selected) (fold resps), keySel)
