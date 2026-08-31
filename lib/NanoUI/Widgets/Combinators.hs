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
  )
where

import Control.Monad (void, when)
import Data.Text (Text)
import Effectful (Eff, type (:>))
import NanoUI.Context (getScrollOffset, setScrollOffset)
import NanoUI.Id (WidgetId)
import NanoUI.Layout.Arena (NodeType (..))
import NanoUI.Monad (Ui, askContext, nextId, uiIO, withKey)
import NanoUI.Style
  ( Layout (..)
  , Sizing (..)
  , defaultLayout
  , fillW
  , tight
  )
import NanoUI.Widgets.Layout (column, row, separator, spacer)
import NanoUI.Widgets.Node (Response, addWidgetStyled)

-- | One row of cells, each with its own layout. Call once per grid row.
gridColumns :: (Ui :> es) => [Layout] -> [Eff es ()] -> Eff es ()
gridColumns layouts cells =
  void $
    row (tight . fillW $ defaultLayout {layoutGap = 0}) $
      mapM_
        ( \(i, (lay, cell)) ->
            withKey (i :: Int) $ do
              when (i > 0) $ void separator
              column lay cell
        )
        (zip [0 ..] (zip layouts cells))

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

selectableItem :: (Ui :> es) => NodeType -> Text -> Bool -> Layout -> Eff es Response
selectableItem nt txt selected layout = do
  wid <- nextId
  addWidgetStyled
    wid
    nt
    txt
    (if selected then 1 else 0)
    layout
    0
    Nothing
