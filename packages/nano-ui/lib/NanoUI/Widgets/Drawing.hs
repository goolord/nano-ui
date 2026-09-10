{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Widgets.Drawing
  ( DrawOp (..)
  , DrawingBuild
  , drawing
  , drawingVersioned
  , drawingCached
  )
where

import Data.Text qualified as T
import Data.Vector (Vector)
import Effectful (Eff, type (:>))
import NanoUI.Context (cachedWidgetLayout, registerDrawing)
import NanoUI.Draw (DrawOp (..), DrawingBuild)
import NanoUI.Layout.Arena (NodeType (NodeDrawing))
import NanoUI.Monad (Ui, askContext, nextId, uiIO)
import NanoUI.Style (Layout)
import NanoUI.Types (Rect)
import NanoUI.Widgets.Node (Response, addWidget)

-- | Vector ops for a laid-out widget. Paint caches ops while width and height
-- stay the same, then translates when the widget moves. Unversioned: the cache
-- drops while the widget animates because the builder has no content key.
drawing :: Ui :> es => Layout -> (Rect -> Vector DrawOp) -> Eff es Response
drawing layout build = do
  wid <- nextId
  ctx <- askContext
  uiIO (registerDrawing ctx wid 0 build)
  addWidget wid NodeDrawing T.empty 0 layout

-- | Like 'drawing', but the tessellated op cache is keyed by an explicit
-- content version. Bump the version whenever the builder output changes (a
-- model pointer, dirty counter, or content hash); unchanged frames — including
-- frames where the widget is animating — replay cached ops without rebuilding.
-- Version 0 is treated as unversioned and falls back to 'drawing' semantics.
drawingVersioned :: Ui :> es => Int -> Layout -> (Rect -> Vector DrawOp) -> Eff es Response
drawingVersioned version layout build = do
  wid <- nextId
  ctx <- askContext
  uiIO (registerDrawing ctx wid (if version == 0 then 1 else version) build)
  addWidget wid NodeDrawing T.empty 0 layout

-- | Same as 'drawing', but skip a layout rebuild while envelope, font, content
-- key, and the caller layout match last frame.
drawingCached ::
  Ui :> es =>
  Double ->
  Double ->
  Float ->
  Int ->
  Layout ->
  IO Layout ->
  DrawingBuild ->
  Eff es Response
drawingCached dw dh lh content incoming compute build = do
  wid <- nextId
  ctx <- askContext
  layout <- uiIO (cachedWidgetLayout ctx wid dw dh lh content incoming compute)
  uiIO (registerDrawing ctx wid content build)
  addWidget wid NodeDrawing T.empty 0 layout
