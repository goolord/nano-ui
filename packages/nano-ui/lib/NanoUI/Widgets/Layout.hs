{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Widgets.Layout
  ( panel
  , panel_
  , panelWith
  , panel'
  , panelResponse
  , panelResponseWith
  , panelResponse'
  , panelBg
  , panelBgWith
  , panelBg'
  , panelStyled
  , panelStyledWith
  , panelStyled'
  , boxWith
  , callout
  , calloutWith
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
  , packPanelStyle
  , padXY
  )
import NanoUI.Types (Color (..), Size (..), colorRGBA, lerpColor)
import NanoUI.Widgets.Node
  ( Response
  , addSizingLeafNode
  , addWidget
  , container
  , containerResponse
  , containerStyled
  , parentIdx
  )

-- =============================================================================
-- Internal Ambient Helpers
-- =============================================================================

{-# INLINE withDefault #-}
withDefault :: Ui :> es => (Layout -> Eff es a -> Eff es r) -> Eff es a -> Eff es r
withDefault c child = do
  base <- askDefaultLayout
  c base child

{-# INLINE withDefaultWith #-}
withDefaultWith :: Ui :> es => (Layout -> Layout) -> (Layout -> Eff es a -> Eff es r) -> Eff es a -> Eff es r
withDefaultWith f c child = do
  base <- askDefaultLayout
  c (f base) child

-- =============================================================================
-- Panel
-- =============================================================================

{-# INLINE panel #-}
panel :: Ui :> es => Eff es a -> Eff es a
panel = withDefault panel'

{-# INLINE panel_ #-}
panel_ :: Ui :> es => Eff es a -> Eff es a
panel_ = panel

{-# INLINE panelWith #-}
panelWith :: Ui :> es => (Layout -> Layout) -> Eff es a -> Eff es a
panelWith = (`withDefaultWith` panel')

{-# INLINE panel' #-}
panel' :: Ui :> es => Layout -> Eff es a -> Eff es a
panel' = container NodePanel

{-# INLINE panelResponse #-}
panelResponse :: Ui :> es => Eff es a -> Eff es (a, Response)
panelResponse = withDefault panelResponse'

{-# INLINE panelResponseWith #-}
panelResponseWith :: Ui :> es => (Layout -> Layout) -> Eff es a -> Eff es (a, Response)
panelResponseWith = (`withDefaultWith` panelResponse')

{-# INLINE panelResponse' #-}
panelResponse' :: Ui :> es => Layout -> Eff es a -> Eff es (a, Response)
panelResponse' = containerResponse NodePanel

{-# INLINE panelBg #-}
panelBg :: Ui :> es => Color -> Eff es a -> Eff es a
panelBg col = withDefault (panelBg' col)

{-# INLINE panelBgWith #-}
panelBgWith :: Ui :> es => Color -> (Layout -> Layout) -> Eff es a -> Eff es a
panelBgWith col f = withDefaultWith f (panelBg' col)

{-# INLINE panelBg' #-}
panelBg' :: Ui :> es => Color -> Layout -> Eff es a -> Eff es a
panelBg' col layout child =
  containerStyled NodePanel layout (packPanelStyle col (Color 0)) child

{-# INLINE panelStyled #-}
panelStyled :: Ui :> es => Color -> Color -> Eff es a -> Eff es a
panelStyled bgCol borderCol = withDefault (panelStyled' bgCol borderCol)

{-# INLINE panelStyledWith #-}
panelStyledWith :: Ui :> es => Color -> Color -> (Layout -> Layout) -> Eff es a -> Eff es a
panelStyledWith bgCol borderCol f = withDefaultWith f (panelStyled' bgCol borderCol)

{-# INLINE panelStyled' #-}
panelStyled' :: Ui :> es => Color -> Color -> Layout -> Eff es a -> Eff es a
panelStyled' bgCol borderCol layout child =
  containerStyled NodePanel layout (packPanelStyle bgCol borderCol) child

{-# INLINE boxWith #-}
boxWith :: Ui :> es => Color -> (Layout -> Layout) -> Eff es a -> Eff es a
boxWith = panelBgWith

{-# INLINE callout #-}
callout :: Ui :> es => Color -> Eff es a -> Eff es a
callout borderCol = calloutWith borderCol id

{-# INLINE calloutWith #-}
calloutWith :: Ui :> es => Color -> (Layout -> Layout) -> Eff es a -> Eff es a
calloutWith borderCol f =
  let bgCol = lerpColor borderCol (colorRGBA 30 30 35 255) 0.88
   in panelStyledWith bgCol borderCol (f . padXY 10 6 . gap 4 . fillW)

-- =============================================================================
-- Row
-- =============================================================================

{-# INLINE row #-}
row :: Ui :> es => Eff es a -> Eff es a
row = withDefault row'

{-# INLINE row_ #-}
row_ :: Ui :> es => Eff es a -> Eff es a
row_ = row

{-# INLINE rowWith #-}
rowWith :: Ui :> es => (Layout -> Layout) -> Eff es a -> Eff es a
rowWith = (`withDefaultWith` row')

{-# INLINE row' #-}
row' :: Ui :> es => Layout -> Eff es a -> Eff es a
row' layout = container NodeContainer (layout {layoutDirection = Row})

{-# INLINE rowResponse #-}
rowResponse :: Ui :> es => Eff es a -> Eff es (a, Response)
rowResponse = withDefault rowResponse'

{-# INLINE rowResponseWith #-}
rowResponseWith :: Ui :> es => (Layout -> Layout) -> Eff es a -> Eff es (a, Response)
rowResponseWith = (`withDefaultWith` rowResponse')

{-# INLINE rowResponse' #-}
rowResponse' :: Ui :> es => Layout -> Eff es a -> Eff es (a, Response)
rowResponse' layout = containerResponse NodeContainer (layout {layoutDirection = Row})

-- =============================================================================
-- Column
-- =============================================================================

{-# INLINE column #-}
column :: Ui :> es => Eff es a -> Eff es a
column = withDefault column'

{-# INLINE column_ #-}
column_ :: Ui :> es => Eff es a -> Eff es a
column_ = column

{-# INLINE columnWith #-}
columnWith :: Ui :> es => (Layout -> Layout) -> Eff es a -> Eff es a
columnWith = (`withDefaultWith` column')

{-# INLINE column' #-}
column' :: Ui :> es => Layout -> Eff es a -> Eff es a
column' layout = container NodeContainer (layout {layoutDirection = Column})

{-# INLINE columnResponse #-}
columnResponse :: Ui :> es => Eff es a -> Eff es (a, Response)
columnResponse = withDefault columnResponse'

{-# INLINE columnResponseWith #-}
columnResponseWith :: Ui :> es => (Layout -> Layout) -> Eff es a -> Eff es (a, Response)
columnResponseWith = (`withDefaultWith` columnResponse')

{-# INLINE columnResponse' #-}
columnResponse' :: Ui :> es => Layout -> Eff es a -> Eff es (a, Response)
columnResponse' layout = containerResponse NodeContainer (layout {layoutDirection = Column})

-- =============================================================================
-- Grid
-- =============================================================================

{-# INLINE grid #-}
grid :: Ui :> es => Int -> Eff es a -> Eff es a
grid n = withDefault (grid' n)

{-# INLINE grid_ #-}
grid_ :: Ui :> es => Int -> Eff es a -> Eff es a
grid_ = grid

{-# INLINE gridWith #-}
gridWith :: Ui :> es => Int -> (Layout -> Layout) -> Eff es a -> Eff es a
gridWith n f = withDefaultWith f (grid' n)

{-# INLINE grid' #-}
grid' :: Ui :> es => Int -> Layout -> Eff es a -> Eff es a
grid' n layout = container NodeContainer (layout {layoutGridCols = max 1 n})

{-# INLINE gridResponse #-}
gridResponse :: Ui :> es => Int -> Eff es a -> Eff es (a, Response)
gridResponse n = withDefault (gridResponse' n)

{-# INLINE gridResponseWith #-}
gridResponseWith :: Ui :> es => Int -> (Layout -> Layout) -> Eff es a -> Eff es (a, Response)
gridResponseWith n f = withDefaultWith f (gridResponse' n)

{-# INLINE gridResponse' #-}
gridResponse' :: Ui :> es => Int -> Layout -> Eff es a -> Eff es (a, Response)
gridResponse' n layout = containerResponse NodeContainer (layout {layoutGridCols = max 1 n})

-- =============================================================================
-- Grid Panel
-- =============================================================================

{-# INLINE gridPanel #-}
gridPanel :: Ui :> es => Int -> Eff es a -> Eff es a
gridPanel n = withDefault (gridPanel' n)

{-# INLINE gridPanel_ #-}
gridPanel_ :: Ui :> es => Int -> Eff es a -> Eff es a
gridPanel_ = gridPanel

{-# INLINE gridPanelWith #-}
gridPanelWith :: Ui :> es => Int -> (Layout -> Layout) -> Eff es a -> Eff es a
gridPanelWith n f = withDefaultWith f (gridPanel' n)

{-# INLINE gridPanel' #-}
gridPanel' :: Ui :> es => Int -> Layout -> Eff es a -> Eff es a
gridPanel' n layout = container NodePanel (layout {layoutGridCols = max 1 n})

{-# INLINE gridPanelResponse #-}
gridPanelResponse :: Ui :> es => Int -> Eff es a -> Eff es (a, Response)
gridPanelResponse n = withDefault (gridPanelResponse' n)

{-# INLINE gridPanelResponseWith #-}
gridPanelResponseWith :: Ui :> es => Int -> (Layout -> Layout) -> Eff es a -> Eff es (a, Response)
gridPanelResponseWith n f = withDefaultWith f (gridPanelResponse' n)

{-# INLINE gridPanelResponse' #-}
gridPanelResponse' :: Ui :> es => Int -> Layout -> Eff es a -> Eff es (a, Response)
gridPanelResponse' n layout = containerResponse NodePanel (layout {layoutGridCols = max 1 n})

-- =============================================================================
-- Grid AutoFit
-- =============================================================================

{-# INLINE gridAutoFit #-}
gridAutoFit :: Ui :> es => Float -> Eff es a -> Eff es a
gridAutoFit minW = withDefault (gridAutoFit' minW)

{-# INLINE gridAutoFit_ #-}
gridAutoFit_ :: Ui :> es => Float -> Eff es a -> Eff es a
gridAutoFit_ = gridAutoFit

{-# INLINE gridAutoFitWith #-}
gridAutoFitWith :: Ui :> es => Float -> (Layout -> Layout) -> Eff es a -> Eff es a
gridAutoFitWith minW f = withDefaultWith f (gridAutoFit' minW)

{-# INLINE gridAutoFit' #-}
gridAutoFit' :: Ui :> es => Float -> Layout -> Eff es a -> Eff es a
gridAutoFit' minW layout = container NodeContainer (layout {layoutGridMinColW = max 1 minW})

{-# INLINE gridAutoFitResponse #-}
gridAutoFitResponse :: Ui :> es => Float -> Eff es a -> Eff es (a, Response)
gridAutoFitResponse minW = withDefault (gridAutoFitResponse' minW)

{-# INLINE gridAutoFitResponseWith #-}
gridAutoFitResponseWith :: Ui :> es => Float -> (Layout -> Layout) -> Eff es a -> Eff es (a, Response)
gridAutoFitResponseWith minW f = withDefaultWith f (gridAutoFitResponse' minW)

{-# INLINE gridAutoFitResponse' #-}
gridAutoFitResponse' :: Ui :> es => Float -> Layout -> Eff es a -> Eff es (a, Response)
gridAutoFitResponse' minW layout = containerResponse NodeContainer (layout {layoutGridMinColW = max 1 minW})

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
scroll = withDefault scroll'

{-# INLINE scroll_ #-}
scroll_ :: Ui :> es => Eff es a -> Eff es a
scroll_ = scroll

{-# INLINE scrollWith #-}
scrollWith :: Ui :> es => (Layout -> Layout) -> Eff es a -> Eff es a
scrollWith = (`withDefaultWith` scroll')

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
scroll2D = withDefault scroll2D'

{-# INLINE scroll2D_ #-}
scroll2D_ :: Ui :> es => Eff es a -> Eff es a
scroll2D_ = scroll2D

{-# INLINE scroll2DWith #-}
scroll2DWith :: Ui :> es => (Layout -> Layout) -> Eff es a -> Eff es a
scroll2DWith = (`withDefaultWith` scroll2D')

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
