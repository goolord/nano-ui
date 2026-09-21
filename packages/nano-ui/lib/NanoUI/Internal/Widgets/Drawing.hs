{-# LANGUAGE OverloadedStrings #-}

-- | Layout nodes painted by cached vector operations. Use content versions
-- whenever the builder's output can change without a size change.
module NanoUI.Internal.Widgets.Drawing
  ( DrawOp (..)
  , DrawingBuild
  , drawing
  , drawingVersioned
  , drawingCached
  )
where

import Data.Text qualified as T
import Data.Primitive.SmallArray (SmallArray)
import Effectful (Eff, type (:>))
import NanoUI.Internal.Context (cachedWidgetLayout, registerDrawing)
import NanoUI.Internal.Draw (DrawOp (..), DrawingBuild)
import NanoUI.Internal.Layout.Arena (NodeType (NodeDrawing))
import NanoUI.Internal.Monad (Ui, askContext, nextId, uiIO)
import NanoUI.Internal.Style (Layout, defaultLayout)
import NanoUI.Internal.Types (Rect)
import NanoUI.Internal.Widgets.Node (Response, addWidget)

-- | Vector ops for a laid-out widget. Paint caches ops while width and height
-- stay the same, then translates when the widget moves. Unversioned: the cache
-- drops while the widget animates because the builder has no content key, and
-- a builder that draws something else at the same size neither rebuilds nor
-- repaints. Use 'drawingVersioned' for output that changes, or
-- 'NanoUI.Internal.Widgets.Custom.customWidget' without a key to have every frame
-- rebuild and compare.
{-# INLINE drawing #-}
drawing :: Ui :> es => (Layout -> Layout) -> (Rect -> SmallArray DrawOp) -> Eff es Response
drawing = drawingVersioned 0

-- | Like 'drawing', but the tessellated op cache is keyed by an explicit
-- content version. Change the version whenever the builder output changes
-- (a model pointer, dirty counter, or content hash): that rebuilds the ops and
-- repaints the widget. Frames with the same version replay cached ops without
-- rebuilding, even while the widget animates. Version 0 means unversioned, as
-- in 'drawing'.
drawingVersioned :: Ui :> es => Int -> (Layout -> Layout) -> (Rect -> SmallArray DrawOp) -> Eff es Response
drawingVersioned version f build = do
  wid <- nextId
  ctx <- askContext
  uiIO (registerDrawing ctx wid version build)
  addWidget wid NodeDrawing T.empty 0 (f defaultLayout)

-- | Like 'drawingVersioned', but the layout itself comes from @compute@, which
-- only reruns when the envelope, line height, content key, or modifier result
-- change.
drawingCached ::
  Ui :> es =>
  Double ->
  Double ->
  Float ->
  Int ->
  (Layout -> Layout) ->
  IO Layout ->
  DrawingBuild ->
  Eff es Response
drawingCached dw dh lh content f compute build = do
  wid <- nextId
  ctx <- askContext
  layout <- uiIO (cachedWidgetLayout ctx wid dw dh lh content (f defaultLayout) compute)
  uiIO (registerDrawing ctx wid content build)
  addWidget wid NodeDrawing T.empty 0 layout
