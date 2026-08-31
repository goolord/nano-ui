{-# LANGUAGE OverloadedStrings #-}

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
import NanoUI.Input (inputMousePos)
import NanoUI.Layout.Arena (NodeType (..))
import NanoUI.Monad (Ui, askContext, askInput, nextId, uiIO, withKey)
import NanoUI.Style (defaultLayout, fillW, gap, tight)
import NanoUI.Types (Rect (..), rectContains)
import NanoUI.WidgetText (treeEncodeStyle)
import NanoUI.Widgets.Behavior (ensureInt, ensureIntSet, putInt, putIntSet, useKeyNav)
import NanoUI.Widgets.Combinators (countForest, forestParents, selectableItem, treeKeyNav, visibleForest)
import NanoUI.Widgets.Layout (column)
import NanoUI.Widgets.Node (Response (..), setChanged, tagContainer)

data TreeItem = TreeItem {treeItemLabel :: !Text, treeItemChildren :: ![TreeItem]}
  deriving (Eq, Show)

toggle :: Int -> IS.IntSet -> IS.IntSet
toggle idx s = if IS.member idx s then IS.delete idx s else IS.insert idx s

treeRow :: (Ui :> es) => (Int, Int, Bool, Text) -> Int -> IS.IntSet -> Eff es (Response, Maybe Int, Maybe IS.IntSet)
treeRow (nodeIdx, depth, hasKids, lbl) selectedIdx expandedSet = do
  ctx <- askContext
  inp <- askInput
  let expanded = IS.member nodeIdx expandedSet
      selected = selectedIdx == nodeIdx
  resp <- selectableItem NodeTree lbl selected (tight . fillW $ defaultLayout) (treeEncodeStyle nodeIdx depth hasKids expanded)
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
    column (tight . gap 0 . fillW $ defaultLayout) $ do
      tagContainer groupId
      results <- mapM (\(row@(i, _, _, _)) -> withKey i (treeRow row selected expandedSet)) rows
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
