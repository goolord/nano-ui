{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Widgets.Table
  ( SortDir (..)
  , SortCol (..)
  , ColSize (..)
  , TableCfg (..)
  , TableResponse (..)
  , defaultTableCfg
  , table
  , tableEx
  , tableCfg
  , useTableSort
  , tableRespChanged
  , tableRespClicked
  , tableHiddenIndices
  , sortRows
  , Colonnade
  , Headed (..)
  , headed
  , headless
  )
where

import Colonnade (Colonnade, Headed (..), headed, headless)
import Control.Monad (void, when)
import Data.IntSet qualified as IS
import Data.Text (Text)
import Data.Text qualified as T
import Effectful (Eff, type (:>))
import qualified Data.IntMap.Strict as IM
import NanoUI.Context (Context (..), getStore, intKey)
import NanoUI.Font (monoFontMarker, scrollBarGutter)
import NanoUI.Host (isCellHost)
import NanoUI.Input (inputMouseDown, inputMousePos)
import NanoUI.Layout.Arena (NodeType (..))
import NanoUI.Monad (Ui, askContext, askInput, nextId, uiIO, withKey)
import NanoUI.Store (WidgetStore (..), slotDrag, slotDragW, slotKey)
import NanoUI.Style (AlignX (..), AlignY (..), Layout (..), Padding (..), Sizing (..), defaultLayout, fillW, tight)
import NanoUI.Types (v2X)
import NanoUI.WidgetText (tableHeaderLabel)
import NanoUI.Widgets.Combinators
  ( ColSize (..), SortCol (..), SortDir (..), TableCfg (..), TableResponse (..)
  , buttonStyled, clampSortCol, colBoxLayout, colSizing, columnCells, columnCount
  , columnHeaders, columnWidths, defaultTableCfg, dragCol, finishTable, fitList
  , isResizeDrag, listAt, minColW, normalizeOrder, numericColumns, resolvedWidth
  , setAt, sortMarkStyle, sortRows, stripedRow, tableHiddenIndices, tableRespChanged
  , tableRespClicked, tableSplitPanes, useTableSort, visibleCols, writeColW
  )
import NanoUI.Widgets.Layout (column)
import NanoUI.Widgets.Node (addWidgetStyled)

table :: (Ui :> es) => Text -> Colonnade Headed row Text -> [row] -> SortCol -> Eff es (TableResponse, SortCol)
table = tableEx (tight . fillW $ defaultLayout {layoutGap = 0})

tableEx :: (Ui :> es) => Layout -> Text -> Colonnade Headed row Text -> [row] -> SortCol -> Eff es (TableResponse, SortCol)
tableEx = tableCfg defaultTableCfg

tableCfg ::
  (Ui :> es) =>
  TableCfg ->
  Layout ->
  Text ->
  Colonnade Headed row Text ->
  [row] ->
  SortCol ->
  Eff es (TableResponse, SortCol)
tableCfg cfg outerLayout key cols rows curSort =
  withKey ("table:" <> key) $ do
    stateWid <- nextId
    vWid <- nextId
    hWid <- nextId
    let n = columnCount cols
        sort0 = clampSortCol n curSort
        stateKey = intKey stateWid
    ctx <- askContext
    inp <- askInput
    st0 <- uiIO (getStore ctx)
    let host = ctxHostProfile ctx
        terminal = isCellHost host
        contentWs = columnWidths ctx cols rows
        sizes = tableColSizes cfg
        order0 = normalizeOrder n (IM.findWithDefault [0 .. n - 1] stateKey (storeIntList st0))
        hidden0 = IM.findWithDefault (tableHidden cfg) stateKey (storeIntSet st0)
        widths0 = fitList n 0 (IM.findWithDefault [] stateKey (storeFloatList st0))
        drag0 = IM.findWithDefault 0 (slotKey slotDrag stateKey) (storeInt st0)
        dragX0 = IM.findWithDefault 0 stateKey (storeFloat st0)
        dragW0 = IM.findWithDefault 0 (slotKey slotDragW stateKey) (storeFloat st0)
        mx = v2X (inputMousePos inp)
        resizing = isResizeDrag drag0 && inputMouseDown inp
        widths1 = if resizing then setAt (dragCol drag0) (max minColW (dragW0 + mx - dragX0)) widths0 else widths0
    when (widths1 /= widths0) $ uiIO $ writeColW ctx stateKey widths1
    let vis = visibleCols order0 hidden0
        freezeN = min (max 0 (tableFreezeCols cfg)) (length vis)
        freezeR = min (max 0 (tableFreezeRows cfg)) (length rows)
        frozenIdx = take freezeN vis
        unfrozenIdx = drop freezeN vis
        sorted = sortRows cols sort0 rows
        pinned = take freezeR sorted
        scrollRows = drop freezeR sorted
        hdrs = columnHeaders cols
        numeric = numericColumns cols rows
        cellPad = if terminal then Padding 0 0 0 0 else Padding 10 8 10 8
        rowMinH = if terminal then 1 else 28
        resolvedW i = resolvedWidth sizes contentWs widths1 i
        colSizingFor i = colSizing sizes widths1 (resolvedW i) i
        itemLayout i =
          (colBoxLayout (colSizingFor i) (resolvedW i))
            { layoutAlignX = if listAt numeric i False then AlignEnd else AlignStart
            , layoutAlignY = AlignMiddle
            , layoutPadding = cellPad
            , layoutMinH = rowMinH
            }
        cellLayout i =
          (itemLayout i)
            { layoutWidth = Grow 1
            , layoutMinW = 0
            }
        shown i txt = if listAt numeric i False then monoFontMarker <> txt else txt
        hChromeH = if terminal then 1 else scrollBarGutter host (ctxFontMetrics ctx)
        renderHeader i =
          buttonStyled (tableHeaderLabel terminal (listAt hdrs i T.empty)) (if sortColIndex sort0 == i then 1 else 0) (itemLayout i) (sortMarkStyle sort0 i)
        renderCell ri r i = void (stripedRow ri (cellLayout i) (shown i (columnCells cols r !! i)))
        colBox i = colBoxLayout (colSizingFor i) (resolvedW i)
    column outerLayout $ do
      showAllResp <-
        if IS.null hidden0
          then pure Nothing
          else fmap Just $ do
            wid <- nextId
            addWidgetStyled wid NodeButton "Show all columns" 0 (tight . fillW $ defaultLayout) 0 Nothing
      headerPairs <-
        tableSplitPanes vWid hWid rowMinH hChromeH frozenIdx unfrozenIdx pinned scrollRows itemLayout colBox renderHeader renderCell
      finishTable n stateKey terminal vis order0 hidden0 drag0 dragX0 dragW0 widths0 widths1 sort0 headerPairs showAllResp resolvedW
