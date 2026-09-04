{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module NanoUI.Widgets.Tree (TreeItem (..), tree) where

import Control.Monad (when)
import Data.IORef (writeIORef)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Effectful (Eff, type (:>))
import qualified Data.IntSet as IS
import NanoUI.Context (Context (..), getFocusId, intKey, registerFocusable)
import NanoUI.Font (treeChevronRect)
import NanoUI.Frame.Hit (scrollHitRect)
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Input (inputMousePos)
import NanoUI.Layout.Arena (NodeType (..))
import NanoUI.Monad (Ui, askContext, askInput, nextId, uiIO, withKey)
import NanoUI.Style (defaultLayout, fillW, gap, tight)
import NanoUI.Types (Rect (..), rectContains)
import NanoUI.WidgetText (treeEncodeStyle)
import NanoUI.Widgets.Behavior (KeyNav (..), ensureInt, ensureIntSet, putInt, putIntSet, useKeyNav)
import NanoUI.Widgets.Combinators (selectableItem)
import NanoUI.Widgets.Layout (columnWith)
import NanoUI.Widgets.Node (Response (..), setChanged, tagContainer)

data TreeItem = TreeItem {treeItemLabel :: !Text, treeItemChildren :: ![TreeItem]}
  deriving (Eq, Show)

countSubtree :: (n -> [n]) -> n -> Int
countSubtree kids node = 1 + foldl' (\acc k -> acc + countSubtree kids k) 0 (kids node)

countForest :: (n -> [n]) -> [n] -> Int
countForest kids = foldl' (\acc x -> acc + countSubtree kids x) 0

visibleForest :: (n -> [n]) -> IS.IntSet -> [n] -> [(Int, Int, Bool, n)]
visibleForest kids expanded items = snd (go 0 0 items)
  where
    go !idx _ [] = (idx, [])
    go !idx !depth (x : xs) =
      let k = kids x
          hasKids = not (null k)
          isExp = hasKids && IS.member idx expanded
          (afterKidsIdx, kVis) =
            if isExp
              then go (idx + 1) (depth + 1) k
              else if hasKids
                then (idx + countSubtree kids x, [])
                else (idx + 1, [])
          (finalIdx, sVis) = go afterKidsIdx depth xs
       in (finalIdx, (idx, depth, hasKids, x) : (kVis ++ sVis))

forestParents :: (n -> [n]) -> [n] -> [Int]
forestParents kids items = snd (go 0 items)
  where
    go !idx [] = (idx, [])
    go !idx (x : xs) =
      let k = kids x
          hasKids = not (null k)
          (afterKids, kParents) = if hasKids then go (idx + 1) k else (idx + 1, [])
          (finalIdx, sParents) = go afterKids xs
          thisParent = if hasKids then [idx] else []
       in (finalIdx, thisParent ++ kParents ++ sParents)

treeKeyNav ::
  KeyNav ->
  [(Int, Int, Bool, a)] ->
  [Response] ->
  WidgetId ->
  Int ->
  IS.IntSet ->
  (Int, IS.IntSet, Maybe WidgetId)
treeKeyNav nav rows resps focus selected expanded
  | hashWidgetId focus == 0 || not moving = (selected, expanded, Nothing)
  | otherwise = case [(i, trow) | (i, (trow, r)) <- zip [0 ..] (zip rows resps), rawRespId r == focus] of
      ((pos, trow) : _) -> step pos trow
      [] -> (selected, expanded, Nothing)
 where
  moving = knUp nav || knDown nav || knLeft nav || knRight nav || knEnter nav || knSpace nav
  n = length rows
  widAt i = rawRespId (resps !! i)
  idxAt i = let (idx, _, _, _) = rows !! i in idx
  wantToggle = knEnter nav || knSpace nav
  parentOf idx =
    case break (\(i, _, _, _) -> i == idx) rows of
      (before, (_, destDepth, _, _) : _) ->
        case [p | (p, d, _, _) <- reverse before, d < destDepth] of
          (p : _) -> p
          [] -> idx
      _ -> idx
  step pos (nodeIdx, depth, hasKids, _)
    | knDown nav, pos + 1 < n = let p = pos + 1 in (idxAt p, expanded, Just (widAt p))
    | knUp nav, pos > 0 = let p = pos - 1 in (idxAt p, expanded, Just (widAt p))
    | wantToggle, hasKids = (selected, toggle nodeIdx expanded, Nothing)
    | knRight nav, hasKids, not (IS.member nodeIdx expanded) = (selected, IS.insert nodeIdx expanded, Nothing)
    | knLeft nav, hasKids, IS.member nodeIdx expanded = (selected, IS.delete nodeIdx expanded, Nothing)
    | knLeft nav, depth > 0 =
        let pidx = parentOf nodeIdx
         in case [i | (i, (p, _, _, _)) <- zip [0 ..] rows, p == pidx] of
              (p : _) -> (pidx, expanded, Just (widAt p))
              [] -> (pidx, expanded, Nothing)
    | otherwise = (selected, expanded, Nothing)

toggle :: Int -> IS.IntSet -> IS.IntSet
toggle idx s = if IS.member idx s then IS.delete idx s else IS.insert idx s

treeRow :: (Ui :> es) => Int -> (Int, Int, Bool, Text) -> Int -> IS.IntSet -> Eff es (Response, Maybe Int, Maybe IS.IntSet)
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
              rectContains (treeChevronRect (ctxHostProfile ctx) (ctxFontMetrics ctx) x y w h depth) mouse
                && rectContains rect mouse
            _ -> False
      if hasKids && onChevron
        then pure (setChanged False resp, Nothing, Just (toggle nodeIdx expandedSet))
        else pure (setChanged (not selected) resp, Just nodeIdx, Nothing)

tree :: (Ui :> es) => Text -> [TreeItem] -> Int -> Eff es (Response, Int)
tree key items initial =
  withKey ("tree:" <> key) $ do
    groupId <- nextId
    let groupKey = intKey groupId
        total = countForest treeItemChildren items
        clamped = if total <= 0 then 0 else max 0 (min (total - 1) initial)
        defaultExpanded = IS.fromList (forestParents treeItemChildren items)
    selected <- ensureInt groupKey clamped
    expandedSet <- ensureIntSet groupKey defaultExpanded
    let rows = [(i, d, has, treeItemLabel item) | (i, d, has, item) <- visibleForest treeItemChildren expandedSet items]
    ctx <- askContext
    columnWith (tight . gap 0 . fillW) $ do
      tagContainer groupId
      results <- mapM (\(rowIdx, row@(i, _, _, _)) -> withKey i (treeRow rowIdx row selected expandedSet)) (zip [0 :: Int ..] rows)
      let resps = [r | (r, _, _) <- results]
          afterClickSel = fromMaybe selected (listToMaybe [idx | (_, Just idx, _) <- results])
          afterClickExp = fromMaybe expandedSet (listToMaybe [s | (_, _, Just s) <- results])
      focus <- uiIO (getFocusId ctx)
      nav <- useKeyNav focus
      let (keySel, keyExp, mFocus) = treeKeyNav nav rows resps focus afterClickSel afterClickExp
      when (keySel /= selected) $ putInt groupKey keySel
      when (keyExp /= expandedSet) $ putIntSet groupKey keyExp
      maybe (pure ()) (\wid -> uiIO $ writeIORef (ctxFocusId ctx) wid) mFocus
      pure (setChanged (keySel /= selected) (mconcat resps), keySel)
