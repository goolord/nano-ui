{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Layout and visual helpers shared by Table, Tabs, Tree, and Radio.
module NanoUI.Widgets.Combinators
  ( gridColumnsLay
  , stripedRow
  , buttonStyled
  , buttonStyledEx
  , selectableItem
  , withBoundedIndex
  , fitList
  , listClipper
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
import NanoUI.Context (isDisabled, registerFocusable)
import NanoUI.Layout.Arena (NodeType (..))
import NanoUI.Monad (Ui, askContext, nextId, uiIO, withKey)
import NanoUI.Style (Layout (..))
import NanoUI.Types (Rect (..), V2 (..), rectContains, v2X, v2Y)
import NanoUI.Widgets.Behavior (keyActivated)
import NanoUI.Widgets.Layout
  ( column'
  , row'
  , separator
  )
import NanoUI.Widgets.Node
  ( Response (..)
  , addWidgetStyled
  , rawRespRect
  , setClicked
  )

-- | One row of cells with custom row layout.
gridColumnsLay :: (Ui :> es) => Layout -> [Int] -> [Layout] -> [Eff es ()] -> Eff es ()
gridColumnsLay lay keys layouts cells =
  void (row' lay (go True keys layouts cells))
 where
  -- Walk in lockstep without allocating zip tuples and indices per cell.
  go first (key : moreKeys) (layout : moreLayouts) (cell : moreCells) = do
    when (not first) $ void separator
    void (withKey key (column' layout cell))
    go False moreKeys moreLayouts moreCells
  go _ _ _ _ = pure ()

stripedRow :: (Ui :> es) => Int -> Layout -> Text -> Eff es Response
stripedRow rowIdx layout txt = do
  wid <- nextId
  let stripe = if even rowIdx then 1 else 2
  addWidgetStyled wid NodeText txt 0 layout stripe

-- | Button with styleIdx for active, sort, badge, or close chrome. Focusable
-- and activatable with Enter or Space while focused.
buttonStyled :: (Ui :> es) => Text -> Float -> Layout -> Int -> Eff es Response
buttonStyled = buttonStyledEx True

-- | Shared activation path for ordinary buttons, menu items, and header chrome.
-- Disabled controls keep their identity and geometry but cannot take focus or
-- activate, including through a click queued before they became disabled.
{-# INLINE buttonStyledEx #-}
buttonStyledEx :: (Ui :> es) => Bool -> Text -> Float -> Layout -> Int -> Eff es Response
buttonStyledEx enabled txt value layout styleIdx = do
  wid <- nextId
  ctx <- askContext
  disabled <- uiIO (isDisabled ctx wid)
  let active = enabled && not disabled
  when active $ uiIO (registerFocusable ctx wid)
  resp <- addWidgetStyled wid NodeButton txt value layout styleIdx
  if active
    then do
      keyClick <- keyActivated wid
      pure (if keyClick then setClicked True resp else resp)
    else pure resp
      { rawRespHovered = False
      , rawRespPressed = False
      , rawRespClicked = False
      , rawRespRightPressed = False
      , rawRespRightClicked = False
      }

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

-- | Run an index-based picker over every value of a bounded enum. Indices
-- are offset by @fromEnum minBound@, so enums that do not start at 0 map
-- correctly. Meant for small enums: every value becomes an option.
withBoundedIndex ::
  forall a r f.
  (Bounded a, Enum a, Functor f) =>
  (a -> Text) -> a -> ([Text] -> Int -> f (r, Int)) -> f (r, a)
withBoundedIndex encode initial pick =
  fmap (toEnum . (+ lower))
    <$> pick (map encode [minBound .. maxBound]) (fromEnum initial - lower)
  where
    lower = fromEnum (minBound :: a)

keyedRowLay :: (Ui :> es) => Layout -> [Int] -> (Int -> Eff es a) -> Eff es [a]
keyedRowLay lay keys act =
  row' lay $
    mapM
      ( \(n, k) -> do
          when (n > 0) $ void separator
          withKey k (act k)
      )
      (zip [0 :: Int ..] keys)

fitList :: Int -> a -> [a] -> [a]
fitList n fallback xs = take n (xs ++ repeat fallback)

-- | First and last visible item index for a uniform-height list, or
-- @(0, -1)@ when nothing is visible.
{-# INLINE listClipper #-}
listClipper :: Int -> Float -> Float -> Float -> (Int, Int)
listClipper itemCount scrollOff viewH itemH
  | itemCount <= 0 || itemH <= 0 || viewH <= 0 = (0, -1)
  | otherwise =
      let firstVis = max 0 (floor (scrollOff / itemH))
          lastVis = min (itemCount - 1) (floor ((scrollOff + viewH - 1) / itemH))
       in if lastVis < firstVis then (0, -1) else (firstVis, lastVis)

setAt :: Int -> a -> [a] -> [a]
setAt i x xs
  | i < 0 = xs
  | otherwise = case splitAt i xs of
      (before, _ : after) -> before ++ x : after
      (_, []) -> xs

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

-- | Hit-test a column resize edge. The grab zone spans the whole column
-- height (header top to body bottom), so a column can be resized by its
-- boundary line anywhere down the table, not just on the header cell.
headerEdgeHit :: Float -> Float -> Float -> [(Int, Response)] -> V2 -> Maybe Int
headerEdgeHit pad yTop yBot cols mouse =
  listToMaybe
    [ i
    | (i, r) <- cols
    , let Rect x y w h = rawRespRect r
    , w > 0 && h > 0
    , let mx = v2X mouse
          my = v2Y mouse
    , my >= min y yTop && my <= max (y + h) yBot
    , abs (mx - (x + w)) <= pad
    ]

headerAtPoint :: [(Int, Response)] -> V2 -> Maybe Int
headerAtPoint cols mouse = listToMaybe [i | (i, r) <- cols, rectContains (rawRespRect r) mouse]
