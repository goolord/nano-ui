{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Widgets.Tree
  ( TreeItem (..)
  , tree
  ) where

import Control.Monad (when)
import Data.IORef (writeIORef)
import Data.List (unfoldr)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Effectful (Eff, type (:>))
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import NanoUI.Context
  ( Context (..)
  , getFocusId
  , getStore
  , intKey
  , registerFocusable
  , setStore
  )
import NanoUI.Font (treeChevronRect)
import NanoUI.Frame.Hit (scrollHitRect)
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Input (inputMousePos)
import NanoUI.Layout.Arena (NodeType (..))
import NanoUI.Monad (Ui, askContext, askInput, nextId, uiIO, withKey)
import NanoUI.Store (WidgetStore (..))
import NanoUI.Style (defaultLayout, fillW, gap, tight)
import NanoUI.Types (Rect (..), rectContains)
import NanoUI.WidgetText (treeEncodeStyle)
import NanoUI.Widgets.Behavior (KeyNav (..), useKeyNav)
import NanoUI.Widgets.Layout (column)
import NanoUI.Widgets.Node
  ( Response (..)
  , addWidgetStyled
  , setChanged
  , tagContainer
  )

data TreeItem = TreeItem
  { treeItemLabel :: !Text
  , treeItemChildren :: ![TreeItem]
  }
  deriving (Eq, Show)

data FlatRow = FlatRow
  { flatIdx :: !Int
  , flatDepth :: !Int
  , flatHasChildren :: !Bool
  , flatLabel :: !Text
  }

indexTree :: [TreeItem] -> [(Int, Int, TreeItem)]
indexTree items = snd (go 0 0 items)
  where
    go next _ [] = (next, [])
    go next depth (item : rest) =
      let (nextKids, kidRows) = go (next + 1) (depth + 1) (treeItemChildren item)
          (nextRest, restRows) = go nextKids depth rest
       in (nextRest, (next, depth, item) : kidRows ++ restRows)

countNodes :: [TreeItem] -> Int
countNodes = foldl' (\n i -> n + 1 + countNodes (treeItemChildren i)) 0

visibleRows :: [TreeItem] -> IS.IntSet -> [FlatRow]
visibleRows items expanded =
  unfoldr step (indexTree items)
  where
    step [] = Nothing
    step ((idx, depth, item) : rest) =
      let hasKids = not (null (treeItemChildren item))
          pending =
            if hasKids && not (IS.member idx expanded)
              then dropWhile (\(_, d, _) -> d > depth) rest
              else rest
       in Just (FlatRow idx depth hasKids (treeItemLabel item), pending)

allParentIndices :: [TreeItem] -> [Int]
allParentIndices items =
  [idx | (idx, _, item) <- indexTree items, not (null (treeItemChildren item))]

parentVisibleIdx :: [FlatRow] -> Int -> Int
parentVisibleIdx rows idx =
  case break ((== idx) . flatIdx) rows of
    (before, _ : _) ->
      case [r | r <- reverse before, flatDepth r < destDepth] of
        (p : _) -> flatIdx p
        [] -> idx
      where
        destDepth = maybe 0 flatDepth (listToMaybe [r | r <- rows, flatIdx r == idx])
    _ -> idx

toggle :: Int -> IS.IntSet -> IS.IntSet
toggle idx s
  | IS.member idx s = IS.delete idx s
  | otherwise = IS.insert idx s

applyTreeKeys ::
  KeyNav ->
  [FlatRow] ->
  [Response] ->
  WidgetId ->
  Int ->
  IS.IntSet ->
  (Int, IS.IntSet, Maybe WidgetId)
applyTreeKeys nav rows resps focus selected expanded
  | hashWidgetId focus == 0 || not moving =
      (selected, expanded, Nothing)
  | otherwise =
      case [(i, row) | (i, (row, r)) <- zip [0 ..] (zip rows resps), rawRespId r == focus] of
        ((pos, row) : _) -> step pos row
        [] -> (selected, expanded, Nothing)
  where
    moving = knUp nav || knDown nav || knLeft nav || knRight nav || knEnter nav || knSpace nav
    n = length rows
    widAt i = rawRespId (resps !! i)
    idxAt i = flatIdx (rows !! i)
    wantToggle = knEnter nav || knSpace nav
    step pos row
      | knDown nav, pos + 1 < n =
          let p = pos + 1 in (idxAt p, expanded, Just (widAt p))
      | knUp nav, pos > 0 =
          let p = pos - 1 in (idxAt p, expanded, Just (widAt p))
      | wantToggle, flatHasChildren row =
          (selected, toggle (flatIdx row) expanded, Nothing)
      | knRight nav, flatHasChildren row, not (IS.member (flatIdx row) expanded) =
          (selected, IS.insert (flatIdx row) expanded, Nothing)
      | knLeft nav, flatHasChildren row, IS.member (flatIdx row) expanded =
          (selected, IS.delete (flatIdx row) expanded, Nothing)
      | knLeft nav, flatDepth row > 0 =
          let pidx = parentVisibleIdx rows (flatIdx row)
              pPos = [i | (i, r) <- zip [0 ..] rows, flatIdx r == pidx]
           in case pPos of
                (p : _) -> (pidx, expanded, Just (widAt p))
                [] -> (pidx, expanded, Nothing)
      | otherwise = (selected, expanded, Nothing)

treeRow ::
  (Ui :> es) =>
  FlatRow ->
  Int ->
  IS.IntSet ->
  Eff es (Response, Maybe Int, Maybe IS.IntSet)
treeRow row selectedIdx expandedSet = do
  wid <- nextId
  ctx <- askContext
  inp <- askInput
  uiIO $ registerFocusable ctx wid
  let nodeIdx = flatIdx row
      depth = flatDepth row
      hasKids = flatHasChildren row
      expanded = IS.member nodeIdx expandedSet
      selected = selectedIdx == nodeIdx
  resp <-
    addWidgetStyled
      wid
      NodeTree
      (flatLabel row)
      (if selected then 1 else 0)
      (tight . fillW $ defaultLayout)
      (treeEncodeStyle nodeIdx depth hasKids expanded)
      Nothing
  if not (rawRespClicked resp)
    then pure (resp, Nothing, Nothing)
    else uiIO $ do
      mrect <- scrollHitRect ctx wid
      let mouse = inputMousePos inp
          onChevron =
            case mrect of
              Nothing -> False
              Just rect@(Rect x y w h) ->
                rectContains (treeChevronRect (ctxHostProfile ctx) (ctxFontMetrics ctx) x y w h depth) mouse
                  && rectContains rect mouse
      if hasKids && onChevron
        then pure (setChanged False resp, Nothing, Just (toggle nodeIdx expandedSet))
        else pure (setChanged (not selected) resp, Just nodeIdx, Nothing)

tree :: (Ui :> es) => Text -> [TreeItem] -> Int -> Eff es (Response, Int)
tree key items initial =
  withKey ("tree:" <> key) $ do
    groupId <- nextId
    let groupKey = intKey groupId
        total = countNodes items
        clamped = if total <= 0 then 0 else max 0 (min (total - 1) initial)
        defaultExpanded = IS.fromList (allParentIndices items)
    ctx <- askContext
    store0 <- uiIO (getStore ctx)
    when (not (IM.member groupKey (storeInt store0))) $
      uiIO $
        setStore
          ctx
          ( store0
              { storeInt = IM.insert groupKey clamped (storeInt store0)
              , storeIntSet = IM.insert groupKey defaultExpanded (storeIntSet store0)
              }
          )
    store1 <- uiIO (getStore ctx)
    let selected = IM.findWithDefault clamped groupKey (storeInt store1)
        expandedSet = IM.findWithDefault defaultExpanded groupKey (storeIntSet store1)
        rows = visibleRows items expandedSet
    column (tight . gap 0 . fillW $ defaultLayout) $ do
      tagContainer groupId
      results <- mapM (\row -> withKey (flatIdx row) (treeRow row selected expandedSet)) rows
      let resps = [r | (r, _, _) <- results]
          afterClickSel = fromMaybe selected (listToMaybe [idx | (_, Just idx, _) <- results])
          afterClickExp = fromMaybe expandedSet (listToMaybe [s | (_, _, Just s) <- results])
          resp = mconcat resps
      focus <- uiIO (getFocusId ctx)
      nav <- useKeyNav focus
      let (keySel, keyExp, mFocus) = applyTreeKeys nav rows resps focus afterClickSel afterClickExp
      store2 <- uiIO (getStore ctx)
      when (keySel /= selected) $
        uiIO $ setStore ctx (store2 {storeInt = IM.insert groupKey keySel (storeInt store2)})
      store3 <- uiIO (getStore ctx)
      when (keyExp /= expandedSet) $
        uiIO $
          setStore
            ctx
            (store3 {storeIntSet = IM.insert groupKey keyExp (storeIntSet store3)})
      case mFocus of
        Just wid -> uiIO $ writeIORef (ctxFocusId ctx) wid
        Nothing -> pure ()
      pure (setChanged (keySel /= selected) resp, keySel)
