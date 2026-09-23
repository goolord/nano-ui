-- | Rows, columns, grids, panels, labels, and scrollers built during the view pass.
module NanoUI.Internal.Widgets.Layout
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
  , scroll2D
  , scroll2DWith
  , scrollArea
  , scrollArea2D
  , scrollAreaIdConfigured
  , grid
  , gridWith
  , responsive
  , responsiveRowCol
  , center
  )
where

import Control.Monad (void)
import Data.Text (Text)
import Effectful (Eff, type (:>))
import NanoUI.Internal.Context (Context (..))
import NanoUI.Internal.Frame.Scroll.Geometry
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Layout.Arena
import NanoUI.Internal.Monad (Ui, askContext, askDefaultLayout, nextId, styled, uiIO, windowWidth, withContext)
import NanoUI.Internal.Style
import NanoUI.Internal.Style qualified as Style
import NanoUI.Internal.Types (Color (..), lerpColor)
import NanoUI.Internal.Widgets.Node

{-# INLINE withDefaultWith #-}
withDefaultWith :: Ui :> es => (Layout -> Layout) -> (Layout -> Eff es a -> Eff es r) -> Eff es a -> Eff es r
withDefaultWith f c child = do
  base <- askDefaultLayout
  c (f base) child

-- | Container with the theme's panel background and border. Returns its body's result.
{-# INLINE panel #-}
panel :: Ui :> es => Eff es a -> Eff es a
panel = panelWith id

-- | 'panel' with modified layout defaults.
{-# INLINE panelWith #-}
panelWith :: Ui :> es => (Layout -> Layout) -> Eff es a -> Eff es a
panelWith = (`withDefaultWith` panel')

{-# INLINE panel' #-}
panel' :: Ui :> es => Layout -> Eff es a -> Eff es a
panel' = container NodePanel

-- | Full-width panel tinted by a colour, with a matching border. See 'calloutWith'.
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

-- | Lay out children left to right, using current defaults and a child id scope.
{-# INLINE row #-}
row :: Ui :> es => Eff es a -> Eff es a
row = rowWith id

-- | 'row' with modified layout defaults; direction remains horizontal.
{-# INLINE rowWith #-}
rowWith :: Ui :> es => (Layout -> Layout) -> Eff es a -> Eff es a
rowWith = (`withDefaultWith` row')

{-# INLINE row' #-}
row' :: Ui :> es => Layout -> Eff es a -> Eff es a
row' layout = container NodeContainer (layout {layoutDirection = Row})

-- | Lay out children top to bottom, using current defaults and a child id scope.
{-# INLINE column #-}
column :: Ui :> es => Eff es a -> Eff es a
column = columnWith id

-- | 'column' with modified layout defaults; direction remains vertical.
{-# INLINE columnWith #-}
columnWith :: Ui :> es => (Layout -> Layout) -> Eff es a -> Eff es a
columnWith = (`withDefaultWith` column')

{-# INLINE column' #-}
column' :: Ui :> es => Layout -> Eff es a -> Eff es a
column' layout = container NodeContainer (layout {layoutDirection = Column})

-- | Run a collection of widgets side by side, as in @hstack (map label names)@.
{-# INLINE hstack #-}
hstack :: (Foldable f, Ui :> es) => f (Eff es ()) -> Eff es ()
hstack = row . sequence_

-- | Run a collection of widgets top to bottom.
{-# INLINE vstack #-}
vstack :: (Foldable f, Ui :> es) => f (Eff es ()) -> Eff es ()
vstack = column . sequence_

-- | Place children in a grid with at least one column. The count is clamped to 1.
{-# INLINE grid #-}
grid :: Ui :> es => Int -> Eff es a -> Eff es a
grid n = gridWith n id

-- | 'grid' with a layout modifier for spacing, sizing, and alignment.
{-# INLINE gridWith #-}
gridWith :: Ui :> es => Int -> (Layout -> Layout) -> Eff es a -> Eff es a
gridWith n f =
  withDefaultWith f $ \layout -> container NodeContainer layout {layoutGridCols = max 1 n}

-- | Choose between two container builders based on window width.
{-# INLINE responsive #-}
responsive :: Ui :> es => Float -> (Eff es a -> Eff es a) -> (Eff es a -> Eff es a) -> Eff es a -> Eff es a
responsive breakpoint wideContainer narrowContainer child = do
  w <- windowWidth
  if w >= breakpoint then wideContainer child else narrowContainer child

-- | A row while the window is at least @breakpoint@ wide, a column below it.
{-# INLINE responsiveRowCol #-}
responsiveRowCol :: Ui :> es => Float -> (Layout -> Layout) -> Eff es a -> Eff es a
responsiveRowCol breakpoint f = responsive breakpoint (rowWith f) (columnWith f)

-- | A line of text. Newlines start new lines.
{-# INLINE label #-}
label :: Ui :> es => Text -> Eff es ()
label txt = void (label' txt)

-- | 'label' returning its 'Response', for a tooltip or an anchored popup.
{-# INLINE label' #-}
label' :: Ui :> es => Text -> Eff es Response
label' = labelWith' id

-- | 'label' with a layout modifier, for example @labelWith fontMono@.
{-# INLINE labelWith #-}
labelWith :: Ui :> es => (Layout -> Layout) -> Text -> Eff es ()
labelWith f txt = void (labelWith' f txt)

-- | 'labelWith' returning geometry and interaction information in a 'Response'.
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
separator = do
  wid <- nextId
  withContext $ \ctx -> do
    parent <- currentParent ctx
    parentDir <- if parent < 0 then pure DirColumn else getDirection (ctxNodeArena ctx) parent
    case parentDir of
      DirColumn -> addSizedLeaf ctx wid NodeSeparator Column (Grow 1) (Fixed 1)
      DirRow -> addSizedLeaf ctx wid NodeSeparator Row (Fixed 1) (Grow 1)

-- | Empty space with the given sizing on each axis.
{-# INLINE spacer #-}
spacer :: Ui :> es => Sizing -> Sizing -> Eff es ()
spacer w h = do
  wid <- nextId
  withContext (\ctx -> addSizedLeaf ctx wid NodeSpacer Row w h)

-- | A leaf with no content under the current parent, sized on each axis. It
-- takes no input, so it resolves no 'Response'.
addSizedLeaf :: Context -> WidgetId -> NodeType -> Direction -> Sizing -> Sizing -> IO ()
addSizedLeaf ctx wid nt dir w h = do
  parent <- currentParent ctx
  idx <-
    addNode (ctxNodeArena ctx) nt parent . gap 0 . tight $
      defaultLayout {layoutDirection = dir, layoutWidth = w, layoutHeight = h}
  setWidgetId (ctxNodeArena ctx) idx wid

-- | Fill the available space and centre the body's children on both axes.
{-# INLINE center #-}
center :: Ui :> es => Eff es a -> Eff es a
center = columnWith (grow . alignMid . alignCenter)

-- | Scroll along the current layout direction, vertical by default. Constrain
-- the viewport size so content can overflow it; the body still runs each frame.
{-# INLINE scroll #-}
scroll :: Ui :> es => Eff es a -> Eff es a
scroll = scrollWith id

-- | 'scroll' with modified viewport layout. Use 'scrollArea' when scroll commands
-- need the container's id.
{-# INLINE scrollWith #-}
scrollWith :: Ui :> es => (Layout -> Layout) -> Eff es a -> Eff es a
scrollWith f child = snd <$> scrollArea f child

-- | 'scrollWith' that also returns the container's widget id, which keys its
-- scroll offset.
{-# INLINE scrollArea #-}
scrollArea :: Ui :> es => (Layout -> Layout) -> Eff es a -> Eff es (WidgetId, a)
scrollArea = scrollAreaFrom (scrollDefault1D . layoutDirection)

{-# INLINE scrollAreaIdConfigured #-}
scrollAreaIdConfigured :: Ui :> es => WidgetId -> Layout -> ScrollConfig -> Eff es a -> Eff es a
scrollAreaIdConfigured wid layout cfg child = do
  ctx <- askContext
  idx <- uiIO $ do
    parent <- currentParent ctx
    idx <- addNodeFromLayout (ctxNodeArena ctx) NodeScrollContainer parent layout
    setWidgetId (ctxNodeArena ctx) idx wid
    setStyleIdx (ctxNodeArena ctx) idx (encodeScrollConfig cfg)
    pure idx
  -- Unscoped: a scroll container's children keep their parent's id scope.
  withContainerNode False idx child

-- | Scroll container on both axes.
{-# INLINE scroll2D #-}
scroll2D :: Ui :> es => Eff es a -> Eff es a
scroll2D = scroll2DWith id

-- | 'scroll2D' with modified viewport layout.
{-# INLINE scroll2DWith #-}
scroll2DWith :: Ui :> es => (Layout -> Layout) -> Eff es a -> Eff es a
scroll2DWith f child = snd <$> scrollArea2D f child

-- | 'scroll2DWith' that also returns the container's widget id.
{-# INLINE scrollArea2D #-}
scrollArea2D :: Ui :> es => (Layout -> Layout) -> Eff es a -> Eff es (WidgetId, a)
scrollArea2D = scrollAreaFrom (const defaultScrollConfig)

-- | A scroll container with a fresh id over the modified defaults, configured
-- from its layout.
{-# INLINE scrollAreaFrom #-}
scrollAreaFrom ::
  Ui :> es => (Layout -> ScrollConfig) -> (Layout -> Layout) -> Eff es a -> Eff es (WidgetId, a)
scrollAreaFrom config f child = do
  layout <- f <$> askDefaultLayout
  wid <- nextId
  (wid,) <$> scrollAreaIdConfigured wid layout (config layout) child
