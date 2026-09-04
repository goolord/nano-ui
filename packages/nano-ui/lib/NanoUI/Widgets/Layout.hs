{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Widgets.Layout
  ( panel
  , panel_
  , panelWith
  , panel'
  , panelResponse
  , panelResponseWith
  , panelResponse'
  , row
  , row_
  , rowWith
  , row'
  , rowResponse
  , rowResponseWith
  , rowResponse'
  , column
  , column_
  , columnWith
  , column'
  , columnResponse
  , columnResponseWith
  , columnResponse'
  , label
  , labelWith
  , labelEx
  , label'
  , separator
  , spacer
  , scroll
  , scroll_
  , scrollWith
  , scroll'
  , scroll2D
  , scroll2D_
  , scroll2DWith
  , scroll2D'
  , scrollArea
  , scrollArea2D
  , scrollConfigured
  , scrollAreaId
  , scrollAreaIdConfigured
  , grid
  , grid_
  , gridWith
  , grid'
  , gridResponse
  , gridResponseWith
  , gridResponse'
  , gridPanel
  , gridPanel_
  , gridPanelWith
  , gridPanel'
  , gridPanelResponse
  , gridPanelResponseWith
  , gridPanelResponse'
  , gridAutoFit
  , gridAutoFit_
  , gridAutoFitWith
  , gridAutoFit'
  , gridAutoFitResponse
  , gridAutoFitResponseWith
  , gridAutoFitResponse'
  , responsive
  , responsiveRowCol
  , windowAspect
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
import NanoUI.Input (Input (inputWindowSize))
import NanoUI.Monad (Ui, askContext, askDefaultLayout, askInput, nextId, uiIO)
import NanoUI.Style
  ( AlignX (..)
  , Direction (..)
  , Layout (..)
  , Sizing (..)
  , alignMid
  , fillH
  , fillW
  , gap
  , grow
  )
import NanoUI.Types (Size (..))
import NanoUI.Widgets.Node
  ( Response
  , addSizingLeafNode
  , addWidget
  , container
  , containerResponse
  , parentIdx
  )

-- =============================================================================
-- Panel
-- =============================================================================

{-# INLINE panel #-}
panel :: Ui :> es => Eff es a -> Eff es a
panel child = do
  base <- askDefaultLayout
  panel' base child

{-# INLINE panel_ #-}
panel_ :: Ui :> es => Eff es a -> Eff es a
panel_ = panel

{-# INLINE panelWith #-}
panelWith :: Ui :> es => (Layout -> Layout) -> Eff es a -> Eff es a
panelWith f child = do
  base <- askDefaultLayout
  panel' (f base) child

{-# INLINE panel' #-}
panel' :: Ui :> es => Layout -> Eff es a -> Eff es a
panel' = container NodePanel

{-# INLINE panelResponse #-}
panelResponse :: Ui :> es => Eff es a -> Eff es (a, Response)
panelResponse child = do
  base <- askDefaultLayout
  panelResponse' base child

{-# INLINE panelResponseWith #-}
panelResponseWith :: Ui :> es => (Layout -> Layout) -> Eff es a -> Eff es (a, Response)
panelResponseWith f child = do
  base <- askDefaultLayout
  panelResponse' (f base) child

{-# INLINE panelResponse' #-}
panelResponse' :: Ui :> es => Layout -> Eff es a -> Eff es (a, Response)
panelResponse' = containerResponse NodePanel

-- =============================================================================
-- Row
-- =============================================================================

{-# INLINE row #-}
row :: Ui :> es => Eff es a -> Eff es a
row child = do
  base <- askDefaultLayout
  row' base child

{-# INLINE row_ #-}
row_ :: Ui :> es => Eff es a -> Eff es a
row_ = row

{-# INLINE rowWith #-}
rowWith :: Ui :> es => (Layout -> Layout) -> Eff es a -> Eff es a
rowWith f child = do
  base <- askDefaultLayout
  row' (f base) child

{-# INLINE row' #-}
row' :: Ui :> es => Layout -> Eff es a -> Eff es a
row' layout child = container NodeContainer (layout {layoutDirection = Row}) child

{-# INLINE rowResponse #-}
rowResponse :: Ui :> es => Eff es a -> Eff es (a, Response)
rowResponse child = do
  base <- askDefaultLayout
  rowResponse' base child

{-# INLINE rowResponseWith #-}
rowResponseWith :: Ui :> es => (Layout -> Layout) -> Eff es a -> Eff es (a, Response)
rowResponseWith f child = do
  base <- askDefaultLayout
  rowResponse' (f base) child

{-# INLINE rowResponse' #-}
rowResponse' :: Ui :> es => Layout -> Eff es a -> Eff es (a, Response)
rowResponse' layout child = containerResponse NodeContainer (layout {layoutDirection = Row}) child

-- =============================================================================
-- Column
-- =============================================================================

{-# INLINE column #-}
column :: Ui :> es => Eff es a -> Eff es a
column child = do
  base <- askDefaultLayout
  column' base child

{-# INLINE column_ #-}
column_ :: Ui :> es => Eff es a -> Eff es a
column_ = column

{-# INLINE columnWith #-}
columnWith :: Ui :> es => (Layout -> Layout) -> Eff es a -> Eff es a
columnWith f child = do
  base <- askDefaultLayout
  column' (f base) child

{-# INLINE column' #-}
column' :: Ui :> es => Layout -> Eff es a -> Eff es a
column' layout child = container NodeContainer (layout {layoutDirection = Column}) child

{-# INLINE columnResponse #-}
columnResponse :: Ui :> es => Eff es a -> Eff es (a, Response)
columnResponse child = do
  base <- askDefaultLayout
  columnResponse' base child

{-# INLINE columnResponseWith #-}
columnResponseWith :: Ui :> es => (Layout -> Layout) -> Eff es a -> Eff es (a, Response)
columnResponseWith f child = do
  base <- askDefaultLayout
  columnResponse' (f base) child

{-# INLINE columnResponse' #-}
columnResponse' :: Ui :> es => Layout -> Eff es a -> Eff es (a, Response)
columnResponse' layout child = containerResponse NodeContainer (layout {layoutDirection = Column}) child

-- =============================================================================
-- Grid
-- =============================================================================

{-# INLINE grid #-}
grid :: Ui :> es => Int -> Eff es a -> Eff es a
grid n child = do
  base <- askDefaultLayout
  grid' n base child

{-# INLINE grid_ #-}
grid_ :: Ui :> es => Int -> Eff es a -> Eff es a
grid_ = grid

{-# INLINE gridWith #-}
gridWith :: Ui :> es => Int -> (Layout -> Layout) -> Eff es a -> Eff es a
gridWith n f child = do
  base <- askDefaultLayout
  grid' n (f base) child

{-# INLINE grid' #-}
grid' :: Ui :> es => Int -> Layout -> Eff es a -> Eff es a
grid' n layout child = container NodeContainer (layout {layoutGridCols = max 1 n}) child

{-# INLINE gridResponse #-}
gridResponse :: Ui :> es => Int -> Eff es a -> Eff es (a, Response)
gridResponse n child = do
  base <- askDefaultLayout
  gridResponse' n base child

{-# INLINE gridResponseWith #-}
gridResponseWith :: Ui :> es => Int -> (Layout -> Layout) -> Eff es a -> Eff es (a, Response)
gridResponseWith n f child = do
  base <- askDefaultLayout
  gridResponse' n (f base) child

{-# INLINE gridResponse' #-}
gridResponse' :: Ui :> es => Int -> Layout -> Eff es a -> Eff es (a, Response)
gridResponse' n layout child = containerResponse NodeContainer (layout {layoutGridCols = max 1 n}) child

-- =============================================================================
-- Grid Panel
-- =============================================================================

{-# INLINE gridPanel #-}
gridPanel :: Ui :> es => Int -> Eff es a -> Eff es a
gridPanel n child = do
  base <- askDefaultLayout
  gridPanel' n base child

{-# INLINE gridPanel_ #-}
gridPanel_ :: Ui :> es => Int -> Eff es a -> Eff es a
gridPanel_ = gridPanel

{-# INLINE gridPanelWith #-}
gridPanelWith :: Ui :> es => Int -> (Layout -> Layout) -> Eff es a -> Eff es a
gridPanelWith n f child = do
  base <- askDefaultLayout
  gridPanel' n (f base) child

{-# INLINE gridPanel' #-}
gridPanel' :: Ui :> es => Int -> Layout -> Eff es a -> Eff es a
gridPanel' n layout child = container NodePanel (layout {layoutGridCols = max 1 n}) child

{-# INLINE gridPanelResponse #-}
gridPanelResponse :: Ui :> es => Int -> Eff es a -> Eff es (a, Response)
gridPanelResponse n child = do
  base <- askDefaultLayout
  gridPanelResponse' n base child

{-# INLINE gridPanelResponseWith #-}
gridPanelResponseWith :: Ui :> es => Int -> (Layout -> Layout) -> Eff es a -> Eff es (a, Response)
gridPanelResponseWith n f child = do
  base <- askDefaultLayout
  gridPanelResponse' n (f base) child

{-# INLINE gridPanelResponse' #-}
gridPanelResponse' :: Ui :> es => Int -> Layout -> Eff es a -> Eff es (a, Response)
gridPanelResponse' n layout child = containerResponse NodePanel (layout {layoutGridCols = max 1 n}) child

-- =============================================================================
-- Grid AutoFit
-- =============================================================================

{-# INLINE gridAutoFit #-}
gridAutoFit :: Ui :> es => Float -> Eff es a -> Eff es a
gridAutoFit minW child = do
  base <- askDefaultLayout
  gridAutoFit' minW base child

{-# INLINE gridAutoFit_ #-}
gridAutoFit_ :: Ui :> es => Float -> Eff es a -> Eff es a
gridAutoFit_ = gridAutoFit

{-# INLINE gridAutoFitWith #-}
gridAutoFitWith :: Ui :> es => Float -> (Layout -> Layout) -> Eff es a -> Eff es a
gridAutoFitWith minW f child = do
  base <- askDefaultLayout
  gridAutoFit' minW (f base) child

{-# INLINE gridAutoFit' #-}
gridAutoFit' :: Ui :> es => Float -> Layout -> Eff es a -> Eff es a
gridAutoFit' minW layout child = container NodeContainer (layout {layoutGridMinColW = max 1 minW}) child

{-# INLINE gridAutoFitResponse #-}
gridAutoFitResponse :: Ui :> es => Float -> Eff es a -> Eff es (a, Response)
gridAutoFitResponse minW child = do
  base <- askDefaultLayout
  gridAutoFitResponse' minW base child

{-# INLINE gridAutoFitResponseWith #-}
gridAutoFitResponseWith :: Ui :> es => Float -> (Layout -> Layout) -> Eff es a -> Eff es (a, Response)
gridAutoFitResponseWith minW f child = do
  base <- askDefaultLayout
  gridAutoFitResponse' minW (f base) child

{-# INLINE gridAutoFitResponse' #-}
gridAutoFitResponse' :: Ui :> es => Float -> Layout -> Eff es a -> Eff es (a, Response)
gridAutoFitResponse' minW layout child = containerResponse NodeContainer (layout {layoutGridMinColW = max 1 minW}) child

-- | Choose between two container builders based on window width.
{-# INLINE responsive #-}
responsive :: Ui :> es => Float -> (Eff es a -> Eff es a) -> (Eff es a -> Eff es a) -> Eff es a -> Eff es a
responsive breakpoint wideContainer narrowContainer child = do
  inp <- askInput
  let w = sizeW (inputWindowSize inp)
  if w >= breakpoint then wideContainer child else narrowContainer child

-- | Row when window width >= breakpoint; Column when narrower.
-- Direct replacement for flex-wrapping responsive layouts.
{-# INLINE responsiveRowCol #-}
responsiveRowCol :: Ui :> es => Float -> Layout -> Eff es a -> Eff es a
responsiveRowCol breakpoint layout child = do
  inp <- askInput
  let w = sizeW (inputWindowSize inp)
      dir = if w >= breakpoint then Row else Column
  container NodeContainer (layout {layoutDirection = dir}) child

-- | Width as a fraction of window width, and height locked to aspect ratio (width / height).
{-# INLINE windowAspect #-}
windowAspect :: Ui :> es => Float -> Float -> Layout -> Eff es Layout
windowAspect frac ratio layout = do
  inp <- askInput
  let w = frac * sizeW (inputWindowSize inp)
  pure $ layout { layoutWidth = Fixed w, layoutHeight = Fixed (w / ratio), layoutMinW = w, layoutMaxW = w }

{-# INLINE label #-}
label :: Ui :> es => Text -> Eff es Response
label txt = do
  base <- askDefaultLayout
  labelEx base txt

{-# INLINE labelWith #-}
labelWith :: Ui :> es => (Layout -> Layout) -> Text -> Eff es Response
labelWith f txt = do
  base <- askDefaultLayout
  labelEx (f base) txt

{-# INLINE labelEx #-}
labelEx :: Ui :> es => Layout -> Text -> Eff es Response
labelEx layout txt = do
  wid <- nextId
  addWidget wid NodeText txt 0 layout

{-# INLINE label' #-}
label' :: Ui :> es => Layout -> Text -> Eff es Response
label' = labelEx

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
scroll :: Ui :> es => Eff es a -> Eff es a
scroll child = do
  base <- askDefaultLayout
  scroll' base child

{-# INLINE scroll_ #-}
scroll_ :: Ui :> es => Eff es a -> Eff es a
scroll_ = scroll

{-# INLINE scrollWith #-}
scrollWith :: Ui :> es => (Layout -> Layout) -> Eff es a -> Eff es a
scrollWith f child = do
  base <- askDefaultLayout
  scroll' (f base) child

{-# INLINE scroll' #-}
scroll' :: Ui :> es => Layout -> Eff es a -> Eff es a
scroll' layout child = do
  (_, r) <- scrollArea layout child
  pure r

{-# INLINE center #-}
center :: Ui :> es => Eff es a -> Eff es a
center = columnWith (grow . alignMid . (\l -> l { layoutAlignX = AlignCenter }))

{-# INLINE flexRow #-}
flexRow :: Ui :> es => Eff es a -> Eff es a
flexRow = rowWith fillW

{-# INLINE flexCol #-}
flexCol :: Ui :> es => Eff es a -> Eff es a
flexCol = columnWith fillH

{-# INLINE hGroup #-}
hGroup :: Ui :> es => Float -> Eff es a -> Eff es a
hGroup g = rowWith (gap g)

{-# INLINE vGroup #-}
vGroup :: Ui :> es => Float -> Eff es a -> Eff es a
vGroup g = columnWith (gap g)

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
scroll2D :: Ui :> es => Eff es a -> Eff es a
scroll2D child = do
  base <- askDefaultLayout
  scroll2D' base child

{-# INLINE scroll2D_ #-}
scroll2D_ :: Ui :> es => Eff es a -> Eff es a
scroll2D_ = scroll2D

{-# INLINE scroll2DWith #-}
scroll2DWith :: Ui :> es => (Layout -> Layout) -> Eff es a -> Eff es a
scroll2DWith f child = do
  base <- askDefaultLayout
  scroll2D' (f base) child

{-# INLINE scroll2D' #-}
scroll2D' :: Ui :> es => Layout -> Eff es a -> Eff es a
scroll2D' layout child = fmap snd (scrollArea2D layout child)

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
