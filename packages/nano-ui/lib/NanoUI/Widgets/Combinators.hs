{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Layout and visual helpers shared by Table, Tabs, Tree, and Radio.
module NanoUI.Widgets.Combinators
  ( gridColumns
  , syncScroll
  , headerRow
  , indentedRow
  , stripedRow
  , buttonStyled
  , selectableItem
  , keyedRow
  , listAt
  , fitList
  , listClipper
  , virtualIndices
  , setAt
  , normalizeOrder
  , visibleCols
  , rebuildOrder
  , minColW
  , headerEdgeHit
  , headerAtPoint
  , keyedRowLay
  )
where

import Control.Monad (void, when)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Effectful (Eff, type (:>))
import NanoUI.Context (getScrollOffset, setScrollOffset)
import NanoUI.Id (WidgetId (..))
import NanoUI.Layout.Arena (NodeType (..))
import NanoUI.Monad (Ui, askContext, nextId, uiIO, withKey)
import NanoUI.Style
  ( Layout (..)
  , Sizing (..)
  , defaultLayout
  , fillW
  , tight
  )
import NanoUI.Types (Rect (..), V2 (..), rectContains, v2X, v2Y)
import NanoUI.Widgets.Layout
  ( column
  , row
  , separator
  , spacer
  )
import NanoUI.Widgets.Node
  ( Response (..)
  , addWidgetStyled
  , rawRespRect
  )

-- | One row of cells keyed by caller ids (column index, not visible position).
gridColumns :: (Ui :> es) => [Int] -> [Layout] -> [Eff es ()] -> Eff es ()
gridColumns keys layouts cells =
  void $
    row (tight $ defaultLayout {layoutGap = 0}) $
      mapM_
        ( \(n, (k, lay, cell)) -> do
            when (n > 0) $ void separator
            withKey k (column lay cell)
        )
        (zip [0 :: Int ..] (zip3 keys layouts cells))

-- | Copy vertical scroll offset from master to slave.
syncScroll :: (Ui :> es) => WidgetId -> WidgetId -> Eff es ()
syncScroll master slave = do
  ctx <- askContext
  uiIO $ do
    off <- getScrollOffset ctx master
    setScrollOffset ctx slave off

headerRow :: (Ui :> es) => Layout -> Eff es a -> Eff es a
headerRow = row

indentedRow :: (Ui :> es) => Int -> Layout -> Eff es a -> Eff es a
indentedRow depth layout child =
  row layout $ do
    when (depth > 0) $
      void (spacer (Fixed (fromIntegral depth * 12)) Fit)
    child

stripedRow :: (Ui :> es) => Int -> Layout -> Text -> Eff es Response
stripedRow rowIdx layout txt = do
  wid <- nextId
  let stripe = if even rowIdx then 1 else 2
  addWidgetStyled wid NodeText txt 0 layout stripe Nothing

-- | Button with styleIdx for active, sort, badge, or close chrome.
buttonStyled :: (Ui :> es) => Text -> Float -> Layout -> Int -> Eff es Response
buttonStyled txt value layout styleIdx = do
  wid <- nextId
  addWidgetStyled wid NodeButton txt value layout styleIdx Nothing

selectableItem :: (Ui :> es) => NodeType -> Text -> Bool -> Layout -> Int -> Eff es Response
selectableItem nt txt selected layout styleIdx = do
  wid <- nextId
  addWidgetStyled
    wid
    nt
    txt
    (if selected then 1 else 0)
    layout
    styleIdx
    Nothing

keyedRow :: (Ui :> es) => [Int] -> (Int -> Eff es a) -> Eff es [a]
keyedRow = keyedRowLay (tight . fillW $ defaultLayout {layoutGap = 0})

keyedRowLay :: (Ui :> es) => Layout -> [Int] -> (Int -> Eff es a) -> Eff es [a]
keyedRowLay lay keys act =
  row lay $
    mapM
      ( \(n, k) -> do
          when (n > 0) $ void separator
          withKey k (act k)
      )
      (zip [0 :: Int ..] keys)

listAt :: [a] -> Int -> a -> a
listAt xs i d = case drop i xs of
  (x : _) -> x
  _ -> d

fitList :: Int -> a -> [a] -> [a]
fitList n d xs = take n (xs ++ repeat d)

{-# INLINE listClipper #-}
listClipper :: Int -> Float -> Float -> Float -> (Int, Int)
listClipper itemCount scrollOff viewH itemH
  | itemCount <= 0 || itemH <= 0 || viewH <= 0 = (0, -1)
  | otherwise =
      let firstVis = max 0 (floor (scrollOff / itemH))
          lastVis = min (itemCount - 1) (floor ((scrollOff + viewH - 1) / itemH))
       in if lastVis < firstVis then (0, -1) else (firstVis, lastVis)

{-# INLINE virtualIndices #-}
virtualIndices :: Int -> Float -> Float -> Float -> [Int]
virtualIndices n scrollOff viewH itemH =
  let (lo, hi) = listClipper n scrollOff viewH itemH
   in if hi < lo then [] else [lo .. hi]

setAt :: Int -> a -> [a] -> [a]
setAt i x xs
  | i < 0 || i >= length xs = xs
  | otherwise = take i xs ++ x : drop (i + 1) xs

normalizeOrder :: Int -> [Int] -> [Int]
normalizeOrder n stored =
  let valid = filter (\i -> i >= 0 && i < n) stored
      seen = IS.fromList valid
   in valid ++ [i | i <- [0 .. n - 1], not (IS.member i seen)]

visibleCols :: [Int] -> IntSet -> [Int]
visibleCols order hidden = filter (`IS.notMember` hidden) order

rebuildOrder :: IntSet -> [Int] -> [Int] -> [Int]
rebuildOrder hidden newVis old =
  let go [] vs = vs
      go (i : is) vs
        | IS.member i hidden = i : go is vs
        | otherwise = case vs of
            (v : vs') -> v : go is vs'
            [] -> i : is
   in go old newVis

minColW :: Float
minColW = 40

headerEdgeHit :: Float -> [(Int, Response)] -> V2 -> Maybe Int
headerEdgeHit pad cols mouse =
  listToMaybe
    [ i
    | (i, r) <- cols
    , let Rect x y w h = rawRespRect r
    , w > 0 && h > 0
    , let mx = v2X mouse
          my = v2Y mouse
    , my >= y && my <= y + h
    , abs (mx - (x + w)) <= pad
    ]

headerAtPoint :: [(Int, Response)] -> V2 -> Maybe Int
headerAtPoint cols mouse = listToMaybe [i | (i, r) <- cols, rectContains (rawRespRect r) mouse]

