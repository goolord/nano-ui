{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Widgets.Tree
  ( TreeItem (..)
  , tree
  ) where

import Control.Monad (when)
import Data.IORef (writeIORef)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Effectful (Eff, type (:>))
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Text as T
import GHC.Stack (HasCallStack)
import NanoUI.Context
  ( Context (..)
  , getFocusId
  , getPrevRect
  , getStore
  , intKey
  , registerFocusable
  , setStore
  )
import NanoUI.Font (treeChevronRect)
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Input (Input (..), Key (..), inputChars, inputKeys, inputKeysElem, inputMousePos)
import NanoUI.Layout.Arena (NodeType (..))
import NanoUI.Monad (Ui, askContext, askInput, currentId, uiIO, withKey)
import NanoUI.Store (WidgetStore (..))
import NanoUI.Style (defaultLayout, fillW, gap, tight)
import NanoUI.Types (Rect (..), rectContains, rectUnion)
import NanoUI.WidgetText (treePackRow)
import NanoUI.Widgets.Layout (column)
import NanoUI.Widgets.Node
  ( Response (..)
  , addWidgetResp
  , mkResponse
  , setChanged
  )

-- | One node in a hierarchical tree.
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

-- | Depth-first indices for every node, including collapsed ones.
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

-- | Visible rows given which parent indices are expanded.
visibleRows :: [TreeItem] -> IS.IntSet -> [FlatRow]
visibleRows items expanded = go (indexTree items)
  where
    go [] = []
    go ((idx, depth, item) : rest) =
      let hasKids = not (null (treeItemChildren item))
          row =
            FlatRow
              { flatIdx = idx
              , flatDepth = depth
              , flatHasChildren = hasKids
              , flatLabel = treeItemLabel item
              }
          skipDesc =
            if hasKids && not (IS.member idx expanded)
              then dropWhile (\(_, d, _) -> d > depth) rest
              else rest
       in row : go skipDesc

allParentIndices :: [TreeItem] -> [Int]
allParentIndices items =
  [ idx
  | (idx, _, item) <- indexTree items
  , not (null (treeItemChildren item))
  ]

mergeResponses :: [Response] -> Response
mergeResponses [] = mkResponse (WidgetId 0) (Rect 0 0 0 0) False False False False
mergeResponses (r : rs) =
  foldl'
    ( \acc x ->
        Response
          { rawRespId = rawRespId x
          , rawRespRect = rectUnion (rawRespRect acc) (rawRespRect x)
          , rawRespHovered = rawRespHovered acc || rawRespHovered x
          , rawRespPressed = rawRespPressed acc || rawRespPressed x
          , rawRespClicked = rawRespClicked acc || rawRespClicked x
          , rawRespChanged = rawRespChanged acc || rawRespChanged x
          }
    )
    r
    rs

parentVisibleIdx :: [FlatRow] -> Int -> Int
parentVisibleIdx rows idx =
  case break ((== idx) . flatIdx) rows of
    (before, _ : _) ->
      case [r | r <- reverse before, flatDepth r < destDepth] of
        (p : _) -> flatIdx p
        [] -> idx
      where
        destDepth =
          case [flatDepth r | r <- rows, flatIdx r == idx] of
            (d : _) -> d
            [] -> 0
    _ -> idx

applyTreeKeys ::
  Input ->
  [FlatRow] ->
  [Response] ->
  WidgetId ->
  Int ->
  IS.IntSet ->
  (Int, IS.IntSet, Maybe WidgetId)
applyTreeKeys inp rows resps focus selected expanded
  | hashWidgetId focus == 0 || not (wantUp || wantDown || wantLeft || wantRight || wantToggle) =
      (selected, expanded, Nothing)
  | otherwise =
      case [ (i, row)
           | (i, (row, r)) <- zip [0 ..] (zip rows resps)
           , rawRespId r == focus
           ] of
        ((pos, row) : _) -> step pos row
        [] -> (selected, expanded, Nothing)
  where
    keys = inputKeys inp
    wantUp = inputKeysElem KeyUp keys
    wantDown = inputKeysElem KeyDown keys
    wantLeft = inputKeysElem KeyLeft keys
    wantRight = inputKeysElem KeyRight keys
    wantToggle =
      inputKeysElem KeyEnter keys
        || T.any (== ' ') (inputChars inp)
    n = length rows
    widAt i = rawRespId (resps !! i)
    idxAt i = flatIdx (rows !! i)
    step pos row
      | wantDown, pos + 1 < n =
          let p = pos + 1
           in (idxAt p, expanded, Just (widAt p))
      | wantUp, pos > 0 =
          let p = pos - 1
           in (idxAt p, expanded, Just (widAt p))
      | wantToggle, flatHasChildren row =
          (selected, toggle (flatIdx row) expanded, Nothing)
      | wantRight, flatHasChildren row, not (IS.member (flatIdx row) expanded) =
          (selected, IS.insert (flatIdx row) expanded, Nothing)
      | wantLeft, flatHasChildren row, IS.member (flatIdx row) expanded =
          (selected, IS.delete (flatIdx row) expanded, Nothing)
      | wantLeft, flatDepth row > 0 =
          let pidx = parentVisibleIdx rows (flatIdx row)
              pPos = [i | (i, r) <- zip [0 ..] rows, flatIdx r == pidx]
           in case pPos of
                (p : _) -> (pidx, expanded, Just (widAt p))
                [] -> (pidx, expanded, Nothing)
      | otherwise = (selected, expanded, Nothing)

toggle :: Int -> IS.IntSet -> IS.IntSet
toggle idx s
  | IS.member idx s = IS.delete idx s
  | otherwise = IS.insert idx s

treeRow ::
  (HasCallStack, Ui :> es) =>
  Int ->
  FlatRow ->
  Int ->
  IS.IntSet ->
  Eff es (Response, Maybe Int, Maybe IS.IntSet)
treeRow groupKey row selectedIdx expandedSet = do
  wid <- currentId
  ctx <- askContext
  inp <- askInput
  uiIO $ registerFocusable ctx wid
  let nodeIdx = flatIdx row
      depth = flatDepth row
      hasKids = flatHasChildren row
      expanded = IS.member nodeIdx expandedSet
      selected = selectedIdx == nodeIdx
      host = ctxHostProfile ctx
      fm = ctxFontMetrics ctx
      nodeText = treePackRow groupKey nodeIdx depth hasKids expanded (flatLabel row)
  resp <-
    addWidgetResp
      wid
      NodeTree
      nodeText
      (if selected then 1 else 0)
      (tight . fillW $ defaultLayout)
      Nothing
  if not (rawRespClicked resp)
    then pure (resp, Nothing, Nothing)
    else
      uiIO $ do
        mrect <- getPrevRect ctx wid
        let mouse = inputMousePos inp
            onChevron =
              case mrect of
                Nothing -> False
                Just rect@(Rect x y w h) ->
                  rectContains (treeChevronRect host fm x y w h depth) mouse
                    && rectContains rect mouse
        if hasKids && onChevron
          then
            pure
              ( setChanged False resp
              , Nothing
              , Just (toggle nodeIdx expandedSet)
              )
          else
            pure
              ( setChanged (not selected) resp
              , Just nodeIdx
              , Nothing
              )

-- | Expandable tree. Returns the selected node index in depth-first order
-- over the full item list (collapsed nodes keep their indices).
-- The key distinguishes two trees at the same call site.
tree ::
  (HasCallStack, Ui :> es) =>
  Text ->
  [TreeItem] ->
  Int ->
  Eff es (Response, Int)
tree key items initial =
  withKey ("tree:" <> key) $ do
    groupId <- currentId
    let groupKey = intKey groupId
        total = countNodes items
        clamped =
          if total <= 0
            then 0
            else max 0 (min (total - 1) initial)
        defaultExpanded = IS.fromList (allParentIndices items)
    ctx <- askContext
    store0 <- uiIO (getStore ctx)
    when (not (IM.member groupKey (storeTreeSelected store0))) $
      uiIO $
        setStore
          ctx
          ( store0
              { storeTreeSelected = IM.insert groupKey clamped (storeTreeSelected store0)
              , storeTreeExpanded =
                  IM.insert groupKey defaultExpanded (storeTreeExpanded store0)
              }
          )
    store1 <- uiIO (getStore ctx)
    let selected = IM.findWithDefault clamped groupKey (storeTreeSelected store1)
        expandedSet =
          IM.findWithDefault defaultExpanded groupKey (storeTreeExpanded store1)
        rows = visibleRows items expandedSet
    column (tight . gap 0 . fillW $ defaultLayout) $ do
      results <-
        mapM
          (\row -> withKey (flatIdx row) (treeRow groupKey row selected expandedSet))
          rows
      let (resps, selMaybes, expMaybes) = unzip3 results
          resp = mergeResponses resps
          clickedSel = listToMaybe [idx | Just idx <- selMaybes]
          clickedExp = listToMaybe [s | Just s <- expMaybes]
          afterClickSel = fromMaybe selected clickedSel
          afterClickExp = fromMaybe expandedSet clickedExp
      focus <- uiIO (getFocusId ctx)
      inp <- askInput
      let (keySel, keyExp, mFocus) =
            applyTreeKeys inp rows resps focus afterClickSel afterClickExp
      store2 <- uiIO (getStore ctx)
      when (keySel /= selected) $
        uiIO $
          setStore
            ctx
            (store2 {storeTreeSelected = IM.insert groupKey keySel (storeTreeSelected store2)})
      store3 <- uiIO (getStore ctx)
      when (keyExp /= expandedSet) $
        uiIO $
          setStore
            ctx
            ( store3
                { storeTreeExpanded =
                    IM.insert groupKey keyExp (storeTreeExpanded store3)
                }
            )
      case mFocus of
        Just wid -> uiIO $ writeIORef (ctxFocusId ctx) wid
        Nothing -> pure ()
      pure (setChanged (keySel /= selected) resp, keySel)
