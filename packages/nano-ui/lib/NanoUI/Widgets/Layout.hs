{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Widgets.Layout
  ( panel
  , panelWith
  , panel'
  , callout
  , calloutWith
  , row
  , rowWith
  , row'
  , column
  , columnWith
  , column'
  , hstack
  , vstack
  , label
  , label'
  , labelWith
  , labelWith'
  , labelEx
  , separator
  , spacer
  , flex
  , scroll
  , scrollWith
  , scroll'
  , scroll2D
  , scroll2DWith
  , scroll2D'
  , scrollArea
  , scrollArea2D
  , scrollConfigured
  , scrollAreaIdConfigured
  , grid
  , gridWith
  , grid'
  , responsive
  , responsiveRowCol
  , center
  )
where

import Control.Monad (void)
import Data.IORef (readIORef)
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
  , NodeIdx
  , NodeType (..)
  , addNodeFromLayout
  , getDirection
  , setStyleIdx
  , setWidgetId
  )
import NanoUI.Input (Input (inputWindowSize))
import NanoUI.Monad (Ui, askContext, askDefaultLayout, askInput, nextId, styled, uiIO)
import NanoUI.Style
  ( AlignX (..)
  , Direction (..)
  , Layout (..)
  , Sizing (..)
  , alignMid
  , fillW
  , gap
  , grow
  , padXY
  , panelStyle
  )
import NanoUI.Style qualified as Style
import NanoUI.Types (Color (..), Size (..), lerpColor)
import NanoUI.Widgets.Node
  ( Response
  , addSizingLeafNode
  , addWidget
  , container

  , parentIdx
  , withContainerNode
  )

-- =============================================================================
-- Internal Ambient Helpers
-- =============================================================================

{-# INLINE withDefault #-}
withDefault :: Ui :> es => (Layout -> Eff es a -> Eff es r) -> Eff es a -> Eff es r
withDefault = withDefaultWith id

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

{-# INLINE panelWith #-}
panelWith :: Ui :> es => (Layout -> Layout) -> Eff es a -> Eff es a
panelWith = (`withDefaultWith` panel')

{-# INLINE panel' #-}
panel' :: Ui :> es => Layout -> Eff es a -> Eff es a
panel' = container NodePanel

{-# INLINE callout #-}
callout :: Ui :> es => Color -> Eff es a -> Eff es a
callout borderCol = calloutWith borderCol id

-- | A panel tinted with @col@: a border in it and a faint wash of it over the
-- panel colour. The tint applies to the callout's own panel and to panels
-- nested in it.
{-# INLINE calloutWith #-}
calloutWith :: Ui :> es => Color -> (Layout -> Layout) -> Eff es a -> Eff es a
calloutWith col f =
  styled
    (\t -> panelStyle (Style.background (lerpColor col (Style.styleBg (Style.themePanel t)) 0.88) . Style.borderColor col) t)
    . panelWith (f . padXY 10 6 . gap 8 . fillW)

-- =============================================================================
-- Row
-- =============================================================================

{-# INLINE row #-}
row :: Ui :> es => Eff es a -> Eff es a
row = withDefault row'

{-# INLINE rowWith #-}
rowWith :: Ui :> es => (Layout -> Layout) -> Eff es a -> Eff es a
rowWith = (`withDefaultWith` row')

{-# INLINE row' #-}
row' :: Ui :> es => Layout -> Eff es a -> Eff es a
row' layout = container NodeContainer (layout {layoutDirection = Row})

-- =============================================================================
-- Column
-- =============================================================================

{-# INLINE column #-}
column :: Ui :> es => Eff es a -> Eff es a
column = withDefault column'

{-# INLINE columnWith #-}
columnWith :: Ui :> es => (Layout -> Layout) -> Eff es a -> Eff es a
columnWith = (`withDefaultWith` column')

{-# INLINE column' #-}
column' :: Ui :> es => Layout -> Eff es a -> Eff es a
column' layout = container NodeContainer (layout {layoutDirection = Column})

-- =============================================================================
-- Collection stacks
-- =============================================================================

-- | Run a collection of widgets side by side, as in @hstack (map label names)@.
{-# INLINE hstack #-}
hstack :: (Foldable f, Ui :> es) => f (Eff es ()) -> Eff es ()
hstack = row . sequence_

-- | Run a collection of widgets top to bottom.
{-# INLINE vstack #-}
vstack :: (Foldable f, Ui :> es) => f (Eff es ()) -> Eff es ()
vstack = column . sequence_

-- =============================================================================
-- Grid
-- =============================================================================

{-# INLINE grid #-}
grid :: Ui :> es => Int -> Eff es a -> Eff es a
grid n = withDefault (grid' n)

{-# INLINE gridWith #-}
gridWith :: Ui :> es => Int -> (Layout -> Layout) -> Eff es a -> Eff es a
gridWith n f = withDefaultWith f (grid' n)

{-# INLINE grid' #-}
grid' :: Ui :> es => Int -> Layout -> Eff es a -> Eff es a
grid' n layout = container NodeContainer (layout {layoutGridCols = max 1 n})

-- =============================================================================
-- Responsive
-- =============================================================================

-- | Choose between two container builders based on window width.
{-# INLINE responsive #-}
responsive :: Ui :> es => Float -> (Eff es a -> Eff es a) -> (Eff es a -> Eff es a) -> Eff es a -> Eff es a
responsive breakpoint wideContainer narrowContainer child = do
  inp <- askInput
  let w = sizeW (inputWindowSize inp)
  if w >= breakpoint then wideContainer child else narrowContainer child

-- | A row while the window is at least @breakpoint@ wide, a column below it.
{-# INLINE responsiveRowCol #-}
responsiveRowCol :: Ui :> es => Float -> (Layout -> Layout) -> Eff es a -> Eff es a
responsiveRowCol breakpoint f child = do
  inp <- askInput
  base <- askDefaultLayout
  let w = sizeW (inputWindowSize inp)
      dir = if w >= breakpoint then Row else Column
  container NodeContainer ((f base) {layoutDirection = dir}) child

-- | A line of text. Newlines start new lines.
{-# INLINE label #-}
label :: Ui :> es => Text -> Eff es ()
label txt = void (label' txt)

-- | 'label' returning its 'Response', for a tooltip or an anchored popup.
{-# INLINE label' #-}
label' :: Ui :> es => Text -> Eff es Response
label' txt = do
  base <- askDefaultLayout
  labelEx base txt

-- | 'label' with a layout modifier, for example @labelWith fontMono@.
{-# INLINE labelWith #-}
labelWith :: Ui :> es => (Layout -> Layout) -> Text -> Eff es ()
labelWith f txt = void (labelWith' f txt)

{-# INLINE labelWith' #-}
labelWith' :: Ui :> es => (Layout -> Layout) -> Text -> Eff es Response
labelWith' f txt = do
  base <- askDefaultLayout
  labelEx (f base) txt

{-# INLINE labelEx #-}
labelEx :: Ui :> es => Layout -> Text -> Eff es Response
labelEx layout txt = do
  wid <- nextId
  addWidget wid NodeText txt 0 layout

-- | Takes up the remaining space along the parent's direction.
{-# INLINE flex #-}
flex :: Ui :> es => Eff es ()
flex = spacer (Grow 1) Fit

-- | A one-pixel rule: horizontal in a column, vertical in a row.
separator :: Ui :> es => Eff es ()
separator = void $ do
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

-- | Empty space with the given sizing on each axis.
{-# INLINE spacer #-}
spacer :: Ui :> es => Sizing -> Sizing -> Eff es ()
spacer w h = do
  wid <- nextId
  ctx <- askContext
  inp <- askInput
  void (uiIO $ addSizingLeafNode ctx inp wid NodeSpacer Row w h)

{-# INLINE scroll #-}
scroll :: Ui :> es => Eff es a -> Eff es a
scroll = withDefault scroll'

{-# INLINE scrollWith #-}
scrollWith :: Ui :> es => (Layout -> Layout) -> Eff es a -> Eff es a
scrollWith = (`withDefaultWith` scroll')

{-# INLINE scroll' #-}
scroll' :: Ui :> es => Layout -> Eff es a -> Eff es a
scroll' layout child =
  snd <$> scrollConfigured (scrollDefault1D (layoutDirection layout)) layout child

{-# INLINE center #-}
center :: Ui :> es => Eff es a -> Eff es a
center = columnWith (grow . alignMid . (\l -> l { layoutAlignX = AlignCenter }))

-- | Push a scroll container node, run the child inside it, then pop.
{-# INLINE scrollContainerWith #-}
scrollContainerWith :: Ui :> es => WidgetId -> (NodeIdx -> IO ()) -> Layout -> Eff es a -> Eff es a
scrollContainerWith wid setup layout child = do
  ctx <- askContext
  idx <- uiIO $ do
    stack <- readIORef (ctxContainerStack ctx)
    idx <- addNodeFromLayout (ctxNodeArena ctx) NodeScrollContainer (parentIdx stack) layout
    setWidgetId (ctxNodeArena ctx) idx wid
    setup idx
    pure idx
  -- Unscoped: a scroll container's children keep their parent's id scope.
  withContainerNode False idx child

-- | Style index + context scroll config for a container with a chosen config.
{-# INLINE configureScrollContainer #-}
configureScrollContainer :: Context -> WidgetId -> ScrollConfig -> NodeIdx -> IO ()
configureScrollContainer ctx wid cfg idx = do
  setStyleIdx (ctxNodeArena ctx) idx (encodeScrollConfig cfg)
  setScrollConfig ctx wid cfg

-- | 'scrollWith' that also returns the container's widget id, which keys its
-- scroll offset.
{-# INLINE scrollArea #-}
scrollArea :: Ui :> es => (Layout -> Layout) -> Eff es a -> Eff es (WidgetId, a)
scrollArea f child = do
  layout <- f <$> askDefaultLayout
  scrollConfigured (scrollDefault1D (layoutDirection layout)) layout child

{-# INLINE scrollAreaIdConfigured #-}
scrollAreaIdConfigured :: Ui :> es => WidgetId -> Layout -> ScrollConfig -> Eff es a -> Eff es a
scrollAreaIdConfigured wid layout cfg child = do
  ctx <- askContext
  scrollContainerWith wid (configureScrollContainer ctx wid cfg) layout child

-- | Scroll container on both axes.
{-# INLINE scroll2D #-}
scroll2D :: Ui :> es => Eff es a -> Eff es a
scroll2D = withDefault scroll2D'

{-# INLINE scroll2DWith #-}
scroll2DWith :: Ui :> es => (Layout -> Layout) -> Eff es a -> Eff es a
scroll2DWith = (`withDefaultWith` scroll2D')

{-# INLINE scroll2D' #-}
scroll2D' :: Ui :> es => Layout -> Eff es a -> Eff es a
scroll2D' layout child = fmap snd (scrollConfigured defaultScrollConfig layout child)

-- | 'scroll2DWith' that also returns the container's widget id.
{-# INLINE scrollArea2D #-}
scrollArea2D :: Ui :> es => (Layout -> Layout) -> Eff es a -> Eff es (WidgetId, a)
scrollArea2D f child = do
  layout <- f <$> askDefaultLayout
  scrollConfigured defaultScrollConfig layout child

{-# INLINE scrollConfigured #-}
scrollConfigured :: Ui :> es => ScrollConfig -> Layout -> Eff es a -> Eff es (WidgetId, a)
scrollConfigured cfg layout child = do
  ctx <- askContext
  wid <- nextId
  r <- scrollContainerWith wid (configureScrollContainer ctx wid cfg) layout child
  pure (wid, r)
