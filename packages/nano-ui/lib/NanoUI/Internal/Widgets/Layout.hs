-- | Rows, columns, grids, panels, labels, and scrollers built during the view pass.
module NanoUI.Internal.Widgets.Layout
  ( withDefaultWith
  , panel
  , panelWith
  , panelWith'
  , panel'
  , callout
  , calloutWith
  , calloutWith'
  , calloutTone
  , calloutToneWith
  , calloutToneWith'
  , row
  , rowWith
  , rowWith'
  , row'
  , column
  , columnWith
  , columnWith'
  , column'
  , layers
  , layersWith
  , layersWith'
  , clipBox
  , label
  , label'
  , labelWith
  , labelWith'
  , labelEx
  , separator
  , spacer
  , spacerW
  , spacerH
  , flex
  , ScrollPolicy (..)
  , ScrollConfig (..)
  , defaultScrollConfig
  , scroll2DConfig
  , scroll
  , scrollWith
  , scrollWith'
  , scroll2D
  , scroll2DWith
  , scroll2DWith'
  , scrollConfigured
  , scrollConfigured'
  , scrollAreaIdConfigured
  , grid
  , gridWith
  , gridWith'
  , responsive
  , responsiveRowCol
  , withContainerQuery
  , withContainerSize
  , withContainerWidth
  , responsiveIn
  , responsiveRowColIn
  , center
  )
where

import Control.Monad (void, when)
import Data.Text (Text)
import NanoUI.Internal.Context (Context (..), getPrevRect, intKey, recordLayoutRead, registerFocusable)
import NanoUI.Internal.Layout.ScrollGeometry
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Layout.Arena
import NanoUI.Internal.Monad (NanoUI, askContext, askDefaultLayout, askInput, currentId, liftIO, nextId, styled, uiTheme, windowSize, windowWidth, withContext)
import NanoUI.Internal.Style
import NanoUI.Internal.Style qualified as Style
import NanoUI.Internal.Types (Color (..), Rect (..), Size (..), lerpColor)
import NanoUI.Internal.Widgets.Node

{-# INLINE withDefaultWith #-}
withDefaultWith :: (Layout -> Layout) -> (Layout -> NanoUI a -> NanoUI r) -> NanoUI a -> NanoUI r
withDefaultWith f c child = do
  base <- askDefaultLayout
  c (f base) child

-- | Container with the theme's panel background and border. Returns its body's result.
{-# INLINE panel #-}
panel :: NanoUI a -> NanoUI a
panel = panelWith id

-- | 'panel' with modified layout defaults.
{-# INLINE panelWith #-}
panelWith :: (Layout -> Layout) -> NanoUI a -> NanoUI a
panelWith = (`withDefaultWith` panel')

-- | 'panel' with an explicit 'Layout' record.
{-# INLINE panel' #-}
panel' :: Layout -> NanoUI a -> NanoUI a
panel' = container NodePanel

-- | 'panelWith' returning its 'Response', as 'rowWith'' does. It takes an
-- id, like a widget.
panelWith' :: (Layout -> Layout) -> NanoUI a -> NanoUI (a, Response)
panelWith' f = withDefaultWith f (containerResponse NodePanel)

-- | Full-width panel tinted by a color, with a matching border. See 'calloutWith'.
{-# INLINE callout #-}
callout :: Color -> NanoUI a -> NanoUI a
callout borderCol = calloutWith borderCol id

calloutTheme :: Color -> Theme -> Theme
calloutTheme col t =
  panelStyle (Style.background (lerpColor col (Style.styleBg (Style.themePanel t)) 0.88) . Style.borderColor col) t

-- | A panel tinted with @col@: a border in it and a faint wash of it over the
-- panel color. The tint applies to the callout's own panel and to panels
-- nested in it.
{-# INLINE calloutWith #-}
calloutWith :: Color -> (Layout -> Layout) -> NanoUI a -> NanoUI a
calloutWith col f = styled (calloutTheme col) . panelWith (f . padXY 10 6 . gap 8 . fillW)

-- | 'calloutWith' returning its 'Response', as 'rowWith'' does.
calloutWith' :: Color -> (Layout -> Layout) -> NanoUI a -> NanoUI (a, Response)
calloutWith' col f = styled (calloutTheme col) . panelWith' (f . padXY 10 6 . gap 8 . fillW)

-- | Full-width callout panel tinted by a semantic theme 'Tone' (for example,
-- @calloutTone Warning@ or @calloutTone Danger@).
{-# INLINE calloutTone #-}
calloutTone :: Tone -> NanoUI a -> NanoUI a
calloutTone t = calloutToneWith t id

-- | 'calloutTone' with a layout modifier.
{-# INLINE calloutToneWith #-}
calloutToneWith :: Tone -> (Layout -> Layout) -> NanoUI a -> NanoUI a
calloutToneWith t f body = do
  col <- flip toneColor t <$> uiTheme
  calloutWith col f body

-- | 'calloutToneWith' returning its 'Response'.
{-# INLINE calloutToneWith' #-}
calloutToneWith' :: Tone -> (Layout -> Layout) -> NanoUI a -> NanoUI (a, Response)
calloutToneWith' t f body = do
  col <- flip toneColor t <$> uiTheme
  calloutWith' col f body

-- | Lay out children left to right, using current defaults and a child id scope.
{-# INLINE row #-}
row :: NanoUI a -> NanoUI a
row = rowWith id

-- | 'row' with modified layout defaults; direction remains horizontal.
{-# INLINE rowWith #-}
rowWith :: (Layout -> Layout) -> NanoUI a -> NanoUI a
rowWith = (`withDefaultWith` row')

-- | 'rowWith' returning its 'Response', whose 'respRect' is where the row
-- was laid out last frame. A row that runs out of room can drop controls by
-- comparing it with the widths they need:
--
-- > (_, bar) <- rowWith' fillW $ do
-- >   label title
-- >   flex
-- >   when (rectW (respRect bar) >= 300) controls
--
-- Unlike 'rowWith', it takes an id, like a widget.
rowWith' :: (Layout -> Layout) -> NanoUI a -> NanoUI (a, Response)
rowWith' f = withDefaultWith (\l -> (f l) {layoutDirection = Row}) (containerResponse NodeContainer)

-- | 'columnWith' returning its 'Response', as 'rowWith''.
columnWith' :: (Layout -> Layout) -> NanoUI a -> NanoUI (a, Response)
columnWith' f = withDefaultWith (\l -> (f l) {layoutDirection = Column}) (containerResponse NodeContainer)

-- | 'row' with an explicit 'Layout' record; direction remains horizontal.
{-# INLINE row' #-}
row' :: Layout -> NanoUI a -> NanoUI a
row' layout = container NodeContainer (layout {layoutDirection = Row})

-- | Lay out children top to bottom, using current defaults and a child id scope.
{-# INLINE column #-}
column :: NanoUI a -> NanoUI a
column = columnWith id

-- | 'column' with modified layout defaults; direction remains vertical.
{-# INLINE columnWith #-}
columnWith :: (Layout -> Layout) -> NanoUI a -> NanoUI a
columnWith = (`withDefaultWith` column')

-- | 'column' with an explicit 'Layout' record; direction remains vertical.
{-# INLINE column' #-}
column' :: Layout -> NanoUI a -> NanoUI a
column' layout = container NodeContainer (layout {layoutDirection = Column})

-- | Stack children in the same box, later ones on top, such as a badge on
-- an icon or a caption over an image. The box is as large as its largest
-- child, and each child is placed by its alignment: @alignEnd . alignTop@
-- for a corner badge, @alignCenter . alignMiddle@ to center it. A growing
-- child ('fillW', 'grow') fills the box on that axis. The pointer goes to
-- the child on top: where one widget covers another, hover, presses and
-- focus go to the top one, and the one underneath shows no hover or tooltip
-- there.
--
-- This is a container with the 'layered' flow, which panels and cards can
-- also use. To cover a container without affecting its size, pin the
-- overlay instead ('pinAt').
{-# INLINE layers #-}
layers :: NanoUI a -> NanoUI a
layers = layersWith id

-- | 'layers' with modified layout defaults; the flow remains layered.
{-# INLINE layersWith #-}
layersWith :: (Layout -> Layout) -> NanoUI a -> NanoUI a
layersWith = (`withDefaultWith` (container NodeContainer . layered))

-- | 'layersWith' returning its 'Response', as 'rowWith'' does.
layersWith' :: (Layout -> Layout) -> NanoUI a -> NanoUI (a, Response)
layersWith' f = withDefaultWith (layered . f) (containerResponse NodeContainer)

-- | A plain container that clips its children to its bounds without painting
-- a panel background or border.
{-# INLINE clipBox #-}
clipBox :: LayoutModifier -> NanoUI a -> NanoUI a
clipBox f = withDefaultWith (f . tight) clipContainer

-- | Place children in a grid with at least one column. The count is clamped to 1.
{-# INLINE grid #-}
grid :: Int -> NanoUI a -> NanoUI a
grid n = gridWith n id

-- | 'grid' with a layout modifier for spacing, sizing, and alignment.
{-# INLINE gridWith #-}
gridWith :: Int -> (Layout -> Layout) -> NanoUI a -> NanoUI a
gridWith n f =
  withDefaultWith f $ \layout -> container NodeContainer layout {layoutGridCols = max 1 n}

-- | 'gridWith' returning its 'Response', as 'rowWith'' does.
gridWith' :: Int -> (Layout -> Layout) -> NanoUI a -> NanoUI (a, Response)
gridWith' n f =
  withDefaultWith f $ \layout -> containerResponse NodeContainer layout {layoutGridCols = max 1 n}

-- | Choose between two container builders based on window width.
{-# INLINE responsive #-}
responsive :: Float -> (NanoUI a -> NanoUI a) -> (NanoUI a -> NanoUI a) -> NanoUI a -> NanoUI a
responsive breakpoint wideContainer narrowContainer child = do
  w <- windowWidth
  if w >= breakpoint then wideContainer child else narrowContainer child

-- | A row while the window is at least @breakpoint@ wide, a column below it.
{-# INLINE responsiveRowCol #-}
responsiveRowCol :: Float -> (Layout -> Layout) -> NanoUI a -> NanoUI a
responsiveRowCol breakpoint f = responsive breakpoint (rowWith f) (columnWith f)

-- | Shared container-size measurement scaffold. Runs @body@ with the derived
-- value @q@ computed from the container's inner content size (or window size
-- minus padding on its first frame), requesting a follow-up frame only when
-- @changed@ reports a meaningful transition.
containerQuery ::
  LayoutModifier ->
  (Size -> q) ->
  (q -> q -> Bool) ->
  (q -> Layout -> Layout) ->
  (q -> NanoUI a) ->
  NanoUI a
containerQuery f project changed adjustLayout body = do
  wid <- currentId
  ctx <- askContext
  winSz <- windowSize
  base <- askDefaultLayout
  let layout0 = f (fillW (tight base))
      Padding pl pr pt pb = layoutPadding layout0
      innerOf (Rect _ _ rw rh) = Size (max 0 (rw - pl - pr)) (max 0 (rh - pt - pb))
      fallback = Size (max 0 (sizeW winSz - pl - pr)) (max 0 (sizeH winSz - pt - pb))
      valOf = project . maybe fallback innerOf
  q <- liftIO $ do
    cur <- valOf <$> getPrevRect ctx wid
    recordLayoutRead ctx (intKey wid) $ do
      next <- valOf <$> getPrevRect ctx wid
      pure (changed cur next)
    pure cur
  container NodeContainer (adjustLayout q layout0) (tagContainer wid >> body q)

-- | Run @body@ inside an unpadded container that fills the available width
-- (unless @f@ overrides sizing), projecting the container's inner content
-- 'Size' to a discrete value @a@. A follow-up frame is scheduled only when
-- the projected value changes (not on every pixel of divider drag), making
-- this ideal for multi-tier breakpoints or responsive grid column counts.
withContainerQuery :: Eq a => LayoutModifier -> (Size -> a) -> (a -> NanoUI r) -> NanoUI r
withContainerQuery f project = containerQuery f project (/=) (\_ l -> l)

-- | Run @body@ inside an unpadded container that fills the available width
-- (unless @f@ overrides sizing), passing the container's inner content 'Size'
-- (outer laid-out bounds less its padding, or 'windowSize' before its first
-- layout). When layout changes that size by more than half a pixel, a
-- follow-up frame is scheduled; pure scrolling or movement does not.
withContainerSize :: LayoutModifier -> (Size -> NanoUI a) -> NanoUI a
withContainerSize f =
  containerQuery f id (\(Size w h) (Size w' h') -> abs (w' - w) > 0.5 || abs (h' - h) > 0.5) (\_ l -> l)

-- | Like 'withContainerSize', passing only the container's inner content width
-- and scheduling a follow-up frame only when that width changes.
withContainerWidth :: LayoutModifier -> (Float -> NanoUI a) -> NanoUI a
withContainerWidth f =
  containerQuery f sizeW (\w w' -> abs (w' - w) > 0.5) (\_ l -> l)

-- | Container-scoped counterpart to 'responsive': chooses between
-- @wideContainer@ and @narrowContainer@ based on the enclosing container's
-- laid-out inner width (rather than 'windowWidth'), settling automatically
-- when a split pane or parent resizes across @breakpoint@.
responsiveIn :: Float -> (NanoUI a -> NanoUI a) -> (NanoUI a -> NanoUI a) -> NanoUI a -> NanoUI a
responsiveIn breakpoint wideContainer narrowContainer child =
  withContainerQuery (direction Column) ((>= breakpoint) . sizeW) $ \isWide ->
    if isWide then wideContainer child else narrowContainer child

-- | A row while its own laid-out inner width is at least @breakpoint@, a
-- column below it. Defaults to 'fillW' and 'tight' unless @f@ overrides them.
responsiveRowColIn :: Float -> LayoutModifier -> NanoUI a -> NanoUI a
responsiveRowColIn breakpoint f child =
  containerQuery
    f
    ((>= breakpoint) . sizeW)
    (/=)
    (\isWide l -> l {layoutDirection = if isWide then Row else Column})
    (const child)

-- | A line of text. Newlines start new lines.
{-# INLINE label #-}
label :: Text -> NanoUI ()
label txt = void (label' txt)

-- | 'label' returning its 'Response', for a tooltip or an anchored popup.
{-# INLINE label' #-}
label' :: Text -> NanoUI Response
label' = labelWith' id

-- | 'label' with a layout modifier, for example @labelWith fontMono@.
--
-- A label wider than its room wraps in a column. In a row, a single-line
-- label with 'fillW' or 'maxW' instead ends in @...@ where it is cut, so
-- @labelWith fillW name@ beside a row's buttons uses the remaining width.
-- 'NanoUI.truncateTextUi' cuts text the same way for a drawing.
{-# INLINE labelWith #-}
labelWith :: (Layout -> Layout) -> Text -> NanoUI ()
labelWith f txt = void (labelWith' f txt)

-- | 'labelWith' returning geometry and interaction information in a 'Response'.
{-# INLINE labelWith' #-}
labelWith' :: (Layout -> Layout) -> Text -> NanoUI Response
labelWith' f txt = do
  base <- askDefaultLayout
  labelEx (f base) txt

{-# INLINE labelEx #-}
labelEx :: Layout -> Text -> NanoUI Response
labelEx layout txt = do
  wid <- nextId
  addWidget wid NodeText txt 0 layout

-- | Takes up the remaining space along the parent's direction: across a row,
-- down a column. Flexes in one container share that space equally.
flex :: NanoUI ()
flex = axisLeaf NodeSpacer Fit (Grow 1)

-- | A one-pixel rule: horizontal in a column, vertical in a row.
separator :: NanoUI ()
separator = axisLeaf NodeSeparator (Grow 1) (Fixed 1)

-- | A leaf sized @across@ and @along@ the current parent's direction (a
-- column at the root).
axisLeaf :: NodeType -> Sizing -> Sizing -> NanoUI ()
axisLeaf nt across along = do
  wid <- nextId
  withContext $ \ctx -> do
    parent <- currentParent ctx
    dir <- if parent < 0 then pure DirColumn else getDirection (ctxNodeArena ctx) parent
    case dir of
      DirColumn -> addSizedLeaf ctx wid nt Column across along
      DirRow -> addSizedLeaf ctx wid nt Row along across

-- | Empty space with the given sizing on each axis.
{-# INLINE spacer #-}
spacer :: Sizing -> Sizing -> NanoUI ()
spacer w h = do
  wid <- nextId
  withContext (\ctx -> addSizedLeaf ctx wid NodeSpacer Row w h)

-- | Fixed horizontal space in logical pixels.
{-# INLINE spacerW #-}
spacerW :: Float -> NanoUI ()
spacerW w = spacer (Fixed w) Fit

-- | Fixed vertical space in logical pixels.
{-# INLINE spacerH #-}
spacerH :: Float -> NanoUI ()
spacerH h = spacer Fit (Fixed h)

-- | A leaf with no content under the current parent, sized on each axis. It
-- takes no input, so it resolves no 'Response'.
addSizedLeaf :: Context -> WidgetId -> NodeType -> Direction -> Sizing -> Sizing -> IO ()
addSizedLeaf ctx wid nt dir w h = do
  parent <- currentParent ctx
  idx <-
    addNode (ctxNodeArena ctx) nt parent . gap 0 . tight $
      defaultLayout {layoutDirection = dir, layoutWidth = w, layoutHeight = h}
  setWidgetId (ctxNodeArena ctx) idx wid

-- | Fill the available space and put the body in its middle on both axes.
-- The body is one block whose children stack top to bottom as in a
-- 'column', each placed across the block by its own alignment: give a child
-- narrower than its siblings 'alignCenter' to center it under them too.
{-# INLINE center #-}
center :: NanoUI a -> NanoUI a
center = layersWith grow . columnWith (tight . alignCenter . alignMiddle)

-- | Scroll along the current layout direction, vertical by default. Constrain
-- the viewport size so content can overflow it; the body still runs each frame.
{-# INLINE scroll #-}
scroll :: NanoUI a -> NanoUI a
scroll = scrollWith id

-- | 'scroll' with modified viewport layout. Use 'scrollWith'' when scroll
-- commands or overlays need the container's 'Response' ('respId', 'respRect').
{-# INLINE scrollWith #-}
scrollWith :: LayoutModifier -> NanoUI a -> NanoUI a
scrollWith f child = fst <$> scrollWith' f child

-- | 'scrollWith' returning the body's result and the container's 'Response',
-- whose 'respId' keys its scroll offset.
{-# INLINE scrollWith' #-}
scrollWith' :: LayoutModifier -> NanoUI a -> NanoUI (a, Response)
scrollWith' = scrollFrom' (scrollDefault1D . layoutDirection)

{-# INLINE scrollAreaIdConfigured #-}
scrollAreaIdConfigured :: WidgetId -> Layout -> ScrollConfig -> NanoUI a -> NanoUI a
scrollAreaIdConfigured wid layout cfg child = do
  idx <- withContext $ \ctx -> do
    parent <- currentParent ctx
    idx <- addNodeFromLayout (ctxNodeArena ctx) NodeScrollContainer parent layout
    setWidgetId (ctxNodeArena ctx) idx wid
    setStyleIdx (ctxNodeArena ctx) idx (encodeScrollConfig cfg)
    when (scrollFocusable cfg) $
      registerFocusable ctx wid
    pure idx
  -- Unscoped: a scroll container's children keep their parent's id scope.
  withContainerNode False idx child

-- | Scroll container on both axes.
{-# INLINE scroll2D #-}
scroll2D :: NanoUI a -> NanoUI a
scroll2D = scroll2DWith id

-- | 'scroll2D' with modified viewport layout.
{-# INLINE scroll2DWith #-}
scroll2DWith :: LayoutModifier -> NanoUI a -> NanoUI a
scroll2DWith f child = fst <$> scroll2DWith' f child

-- | 'scroll2DWith' returning the body's result and the container's 'Response'.
{-# INLINE scroll2DWith' #-}
scroll2DWith' :: LayoutModifier -> NanoUI a -> NanoUI (a, Response)
scroll2DWith' = scrollFrom' (const scroll2DConfig)

-- | Scroll container with an explicit 'ScrollConfig' and viewport layout.
{-# INLINE scrollConfigured #-}
scrollConfigured :: ScrollConfig -> LayoutModifier -> NanoUI a -> NanoUI a
scrollConfigured cfg f child = fst <$> scrollConfigured' cfg f child

-- | 'scrollConfigured' returning the body's result and the container's 'Response'.
{-# INLINE scrollConfigured' #-}
scrollConfigured' :: ScrollConfig -> LayoutModifier -> NanoUI a -> NanoUI (a, Response)
scrollConfigured' cfg = scrollFrom' (const cfg)

{-# INLINE scrollFrom' #-}
scrollFrom' ::
  (Layout -> ScrollConfig) -> LayoutModifier -> NanoUI a -> NanoUI (a, Response)
scrollFrom' config f child = do
  layout <- f <$> askDefaultLayout
  wid <- nextId
  inp <- askInput
  r <- scrollAreaIdConfigured wid layout (config layout) child
  resp <- withContext (\ctx -> resolveInteraction ctx inp (-1) wid)
  pure (r, resp)
