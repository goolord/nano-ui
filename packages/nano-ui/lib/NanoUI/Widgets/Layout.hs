{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Widgets.Layout
  ( panel
  , panelResponse
  , row
  , rowResponse
  , column
  , columnResponse
  , label
  , labelEx
  , separator
  , spacer
  , scroll
  , scroll2D
  , scrollArea
  , scrollArea2D
  , scrollConfigured
  , scrollAreaId
  , scrollAreaIdConfigured
  , row_
  , rowWith
  , row'
  , column_
  , columnWith
  , column'
  , panel_
  , panelWith
  , panel'
  , scroll_
  , scrollWith
  , center
  , flexRow
  , flexCol
  , hGroup
  , vGroup
  , flex
  , sep
  )
where

import Control.Monad (void)
import Data.IORef (readIORef, writeIORef)
import Data.Text (Text)
import Effectful (Eff, type (:>))
import NanoUI.Context (Context (..), setScrollConfig)
import NanoUI.Frame.Scroll.Geometry
  ( ScrollConfig (..)
  , defaultScrollConfig
  , encodeScrollConfig
  , scrollDefault1D
  )
import NanoUI.Id (WidgetId)
import NanoUI.Layout.Arena
  ( DirTag (..)
  , NodeType (..)
  , addNodeFromLayout
  , getDirection
  , setStyleIdx
  , setWidgetId
  )
import NanoUI.Monad (Ui, askContext, askInput, nextId, uiIO)
import NanoUI.Style
  ( AlignX (..)
  , Direction (..)
  , Layout (..)
  , Sizing (..)
  , alignMid
  , defaultLayout
  , fillH
  , fillW
  , gap
  , grow
  )
import NanoUI.Widgets.Node
  ( Response
  , addSizingLeafNode
  , addWidget
  , container
  , containerResponse
  , parentIdx
  )

{-# INLINE panel #-}
panel :: Ui :> es => Layout -> Eff es a -> Eff es a
panel = container NodePanel

{-# INLINE panel_ #-}
panel_ :: Ui :> es => Eff es a -> Eff es a
panel_ = panel defaultLayout

{-# INLINE panelWith #-}
panelWith :: Ui :> es => (Layout -> Layout) -> Eff es a -> Eff es a
panelWith f = panel (f defaultLayout)

{-# INLINE panel' #-}
panel' :: Ui :> es => [Layout -> Layout] -> Eff es a -> Eff es a
panel' mods = panel (foldr (.) id mods defaultLayout)

{-# INLINE panelResponse #-}
panelResponse :: Ui :> es => Layout -> Eff es a -> Eff es (a, Response)
panelResponse = containerResponse NodePanel

{-# INLINE row #-}
row :: Ui :> es => Layout -> Eff es a -> Eff es a
row layout child = container NodeContainer (layout {layoutDirection = Row}) child

{-# INLINE row_ #-}
row_ :: Ui :> es => Eff es a -> Eff es a
row_ = row defaultLayout

{-# INLINE rowWith #-}
rowWith :: Ui :> es => (Layout -> Layout) -> Eff es a -> Eff es a
rowWith f = row (f defaultLayout)

{-# INLINE row' #-}
row' :: Ui :> es => [Layout -> Layout] -> Eff es a -> Eff es a
row' mods = row (foldr (.) id mods defaultLayout)

{-# INLINE rowResponse #-}
rowResponse :: Ui :> es => Layout -> Eff es a -> Eff es (a, Response)
rowResponse layout child = containerResponse NodeContainer (layout {layoutDirection = Row}) child

{-# INLINE column #-}
column :: Ui :> es => Layout -> Eff es a -> Eff es a
column layout child = container NodeContainer (layout {layoutDirection = Column}) child

{-# INLINE column_ #-}
column_ :: Ui :> es => Eff es a -> Eff es a
column_ = column defaultLayout

{-# INLINE columnWith #-}
columnWith :: Ui :> es => (Layout -> Layout) -> Eff es a -> Eff es a
columnWith f = column (f defaultLayout)

{-# INLINE column' #-}
column' :: Ui :> es => [Layout -> Layout] -> Eff es a -> Eff es a
column' mods = column (foldr (.) id mods defaultLayout)

{-# INLINE columnResponse #-}
columnResponse :: Ui :> es => Layout -> Eff es a -> Eff es (a, Response)
columnResponse layout child = containerResponse NodeContainer (layout {layoutDirection = Column}) child

{-# INLINE label #-}
label :: Ui :> es => Text -> Eff es Response
label = labelEx defaultLayout

{-# INLINE labelEx #-}
labelEx :: Ui :> es => Layout -> Text -> Eff es Response
labelEx layout txt = do
  wid <- nextId
  addWidget wid NodeText txt 0 layout

{-# INLINE sep #-}
sep :: Ui :> es => Eff es ()
sep = void separator

{-# INLINE flex #-}
flex :: Ui :> es => Eff es ()
flex = void (spacer (Grow 1) Fit)

{-# INLINE separator #-}
separator :: Ui :> es => Eff es Response
separator = do
  wid <- nextId
  ctx <- askContext
  inp <- askInput
  uiIO $ do
    stack <- readIORef (ctxContainerStack ctx)
    let
      parent = parentIdx stack
    parentDir <-
      if parent < 0
        then pure DirColumn
        else getDirection (ctxNodeArena ctx) parent
    let
      (dir, wSiz, hSiz) =
        case parentDir of
          DirColumn -> (Column, Grow 1, Fixed 1)
          DirRow -> (Row, Fixed 1, Grow 1)
    addSizingLeafNode ctx inp wid NodeSeparator dir wSiz hSiz

{-# INLINE spacer #-}
spacer :: Ui :> es => Sizing -> Sizing -> Eff es Response
spacer w h = do
  wid <- nextId
  ctx <- askContext
  inp <- askInput
  uiIO $ addSizingLeafNode ctx inp wid NodeSpacer Row w h

{-# INLINE scroll #-}
scroll :: Ui :> es => Layout -> Eff es a -> Eff es a
scroll layout child = do
  (_, r) <- scrollArea layout child
  pure r

{-# INLINE scroll_ #-}
scroll_ :: Ui :> es => Eff es a -> Eff es a
scroll_ = scroll defaultLayout

{-# INLINE scrollWith #-}
scrollWith :: Ui :> es => (Layout -> Layout) -> Eff es a -> Eff es a
scrollWith f = scroll (f defaultLayout)

{-# INLINE center #-}
center :: Ui :> es => Eff es a -> Eff es a
center = column (grow . alignMid $ defaultLayout { layoutAlignX = AlignCenter })

{-# INLINE flexRow #-}
flexRow :: Ui :> es => Eff es a -> Eff es a
flexRow = row (fillW defaultLayout)

{-# INLINE flexCol #-}
flexCol :: Ui :> es => Eff es a -> Eff es a
flexCol = column (fillH defaultLayout)

{-# INLINE hGroup #-}
hGroup :: Ui :> es => Float -> Eff es a -> Eff es a
hGroup g = row (gap g defaultLayout)

{-# INLINE vGroup #-}
vGroup :: Ui :> es => Float -> Eff es a -> Eff es a
vGroup g = column (gap g defaultLayout)

{-# INLINE scrollArea #-}
scrollArea :: Ui :> es => Layout -> Eff es a -> Eff es (WidgetId, a)
scrollArea layout child = do
  ctx <- askContext
  wid <- nextId
  stack <- uiIO $ do
    stack0 <- readIORef (ctxContainerStack ctx)
    let
      parent = parentIdx stack0
    idx <- addNodeFromLayout (ctxNodeArena ctx) NodeScrollContainer parent layout
    setWidgetId (ctxNodeArena ctx) idx wid
    let cfg = scrollDefault1D (layoutDirection layout)
    setStyleIdx (ctxNodeArena ctx) idx (encodeScrollConfig cfg)
    setScrollConfig ctx wid cfg
    writeIORef (ctxContainerStack ctx) (idx : stack0)
    pure stack0
  childR <- child
  uiIO (writeIORef (ctxContainerStack ctx) stack)
  pure (wid, childR)

-- | Scroll container with a chosen widget id. Same id on two panes shares the offset.
scrollAreaId :: Ui :> es => WidgetId -> Layout -> Int -> Eff es a -> Eff es a
scrollAreaId wid layout styleIdx child = do
  ctx <- askContext
  stack <- uiIO $ do
    stack0 <- readIORef (ctxContainerStack ctx)
    let
      parent = parentIdx stack0
    idx <- addNodeFromLayout (ctxNodeArena ctx) NodeScrollContainer parent layout
    setWidgetId (ctxNodeArena ctx) idx wid
    setStyleIdx (ctxNodeArena ctx) idx styleIdx
    writeIORef (ctxContainerStack ctx) (idx : stack0)
    pure stack0
  r <- child
  uiIO (writeIORef (ctxContainerStack ctx) stack)
  pure r

{-# INLINE scrollAreaIdConfigured #-}
scrollAreaIdConfigured :: Ui :> es => WidgetId -> Layout -> ScrollConfig -> Eff es a -> Eff es a
scrollAreaIdConfigured wid layout cfg child = do
  ctx <- askContext
  stack <- uiIO $ do
    stack0 <- readIORef (ctxContainerStack ctx)
    let parent = parentIdx stack0
    idx <- addNodeFromLayout (ctxNodeArena ctx) NodeScrollContainer parent layout
    setWidgetId (ctxNodeArena ctx) idx wid
    setStyleIdx (ctxNodeArena ctx) idx (encodeScrollConfig cfg)
    setScrollConfig ctx wid cfg
    writeIORef (ctxContainerStack ctx) (idx : stack0)
    pure stack0
  r <- child
  uiIO (writeIORef (ctxContainerStack ctx) stack)
  pure r

{-# INLINE scroll2D #-}
scroll2D :: Ui :> es => Layout -> Eff es a -> Eff es a
scroll2D layout child = fmap snd (scrollArea2D layout child)

{-# INLINE scrollArea2D #-}
scrollArea2D :: Ui :> es => Layout -> Eff es a -> Eff es (WidgetId, a)
scrollArea2D layout child = scrollConfigured defaultScrollConfig layout child

{-# INLINE scrollConfigured #-}
scrollConfigured :: Ui :> es => ScrollConfig -> Layout -> Eff es a -> Eff es (WidgetId, a)
scrollConfigured cfg layout child = do
  ctx <- askContext
  wid <- nextId
  stack <- uiIO $ do
    stack0 <- readIORef (ctxContainerStack ctx)
    let parent = parentIdx stack0
    idx <- addNodeFromLayout (ctxNodeArena ctx) NodeScrollContainer parent layout
    setWidgetId (ctxNodeArena ctx) idx wid
    setStyleIdx (ctxNodeArena ctx) idx (encodeScrollConfig cfg)
    setScrollConfig ctx wid cfg
    writeIORef (ctxContainerStack ctx) (idx : stack0)
    pure stack0
  childR <- child
  uiIO (writeIORef (ctxContainerStack ctx) stack)
  pure (wid, childR)
