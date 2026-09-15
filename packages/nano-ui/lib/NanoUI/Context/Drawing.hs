-- | Per-widget drawing registrations and the caches derived from them.
module NanoUI.Context.Drawing
  ( registerPopupConfig
  , lookupPopupConfig
  , registerDrawing
  , lookupDrawing
  , cachedDrawingOps
  , cachedWidgetLayout
  , lookupDrawFitEnvelope
  , pruneDrawOpCache
  , registerCustomDrawing
  , lookupCustomDrawing
  , cachedCustomDrawingOps
  , registerCustomMeasure
  , lookupCustomMeasure
  , registerCustomCursor
  , lookupCustomCursor
  , registerCustomDamageSlop
  , lookupCustomDamageSlop
  , resetDrawingScopeCache
  , hasCustomLayoutInputs
  ) where

import Data.IORef (modifyIORef', readIORef)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Vector (Vector)
import Data.Vector qualified as V

import NanoUI.Context.Animation (isAnimatingKey)
import NanoUI.Context.Types
  ( Context (..)
  , CustomDrawBuild
  , CustomDrawContext (..)
  , CustomDrawOpCacheEntry (..)
  , CustomMeasureFn
  , DrawFitCache (..)
  , DrawOpCacheEntry (..)
  , DrawingCacheState (..)
  , DrawingEntry (..)
  , PopupConfig (..)
  , intKey
  )
import NanoUI.Draw (DrawOp, DrawingBuild, shiftDrawOp)
import NanoUI.Id (WidgetId)
import NanoUI.Input (UiCursorKind)
import NanoUI.Style (Layout)
import NanoUI.Types (PopupAnchor, PopupPlacement, Rect (..), rectH, rectW)

{-# INLINE lookupIn #-}
lookupIn :: (DrawingCacheState -> IntMap a) -> Context -> WidgetId -> IO (Maybe a)
lookupIn field ctx wid = IM.lookup (intKey wid) . field <$> readIORef (ctxDrawingCache ctx)

{-# INLINE registerIn #-}
registerIn ::
  (DrawingCacheState -> IntMap a) ->
  (IntMap a -> DrawingCacheState -> DrawingCacheState) ->
  Context ->
  WidgetId ->
  a ->
  IO ()
registerIn field setField ctx wid v =
  modifyIORef' (ctxDrawingCache ctx) $ \dc ->
    setField (IM.insert (intKey wid) v (field dc)) dc

{-# INLINE registerPopupConfig #-}
registerPopupConfig :: Context -> WidgetId -> PopupAnchor -> PopupPlacement -> Float -> IO ()
registerPopupConfig ctx wid anchor placement offset =
  registerIn dcsPopupConfigs (\m dc -> dc {dcsPopupConfigs = m}) ctx wid (PopupConfig anchor placement offset)

{-# INLINE lookupPopupConfig #-}
lookupPopupConfig :: Context -> WidgetId -> IO (Maybe (PopupAnchor, PopupPlacement, Float))
lookupPopupConfig ctx wid =
  fmap (\(PopupConfig anchor placement offset) -> (anchor, placement, offset))
    <$> lookupIn dcsPopupConfigs ctx wid

{-# INLINE registerDrawing #-}
registerDrawing :: Context -> WidgetId -> Int -> DrawingBuild -> IO ()
registerDrawing ctx wid content build =
  registerIn dcsDrawings (\m dc -> dc {dcsDrawings = m}) ctx wid (DrawingEntry content build)

{-# INLINE lookupDrawing #-}
lookupDrawing :: Context -> WidgetId -> IO (Maybe DrawingEntry)
lookupDrawing = lookupIn dcsDrawings

-- | One draw-op cache step. @hit@ holds the bounds and ops of an entry whose
-- key still matches. A same-size hit is reused, translated if the widget
-- moved; a miss or a resize uses @rebuilt@. Ops that differ from the entry are
-- written back with @store@.
serveOps ::
  Maybe (Rect, Vector DrawOp) ->
  Rect ->
  Vector DrawOp ->
  (Vector DrawOp -> IO ()) ->
  IO (Vector DrawOp)
serveOps hit rect rebuilt store =
  case hit of
    Just (r, ops)
      | rectW r == rectW rect && rectH r == rectH rect ->
          if rectX r == rectX rect && rectY r == rectY rect
            then pure ops
            else keep (V.map (shiftDrawOp (rectX rect - rectX r) (rectY rect - rectY r)) ops)
    _ -> keep rebuilt
  where
    keep ops = store ops >> pure ops

-- | Rebuild draw ops when the content version or width/height change. A move
-- only translates. An unversioned drawing (content 0) additionally drops its
-- cache while the widget is animating, since it has no other invalidation
-- signal; versioned drawings are invalidated by their content key alone.
cachedDrawingOps :: Context -> WidgetId -> Int -> Rect -> DrawingBuild -> IO (Vector DrawOp)
cachedDrawingOps ctx wid content rect build = do
  let k = intKey wid
  animated <-
    if content == 0
      then isAnimatingKey ctx k
      else pure False
  cached <- IM.lookup k . dcsDrawOpCache <$> readIORef (ctxDrawingCache ctx)
  let hit = case cached of
        Just DrawOpCacheEntry {doeContent = c, doeBounds = r, doeOps = ops}
          | c == content && not animated -> Just (r, ops)
        _ -> Nothing
  serveOps hit rect (build rect) $ \ops ->
    modifyIORef' (ctxDrawingCache ctx) $ \s ->
      s {dcsDrawOpCache = IM.insert k (DrawOpCacheEntry content rect ops) (dcsDrawOpCache s)}

-- | Reuse a derived layout while envelope, font, content key, and caller layout match.
cachedWidgetLayout ::
  Context ->
  WidgetId ->
  Double ->
  Double ->
  Float ->
  Int ->
  Layout ->
  IO Layout ->
  IO Layout
cachedWidgetLayout ctx wid dw dh lh content incoming compute = do
  let k = intKey wid
  dc <- readIORef (ctxDrawingCache ctx)
  case IM.lookup k (dcsDrawFitCache dc) of
    Just e
      | dfcDw e == dw
          && dfcDh e == dh
          && dfcLh e == lh
          && dfcContent e == content
          && dfcIn e == incoming ->
          pure (dfcOut e)
    _ -> do
      out <- compute
      modifyIORef' (ctxDrawingCache ctx) $ \s ->
        s { dcsDrawFitCache = IM.insert k (DrawFitCache dw dh lh content incoming out) (dcsDrawFitCache s)
          , dcsDrawOpCache = IM.delete k (dcsDrawOpCache s)
          }
      pure out

lookupDrawFitEnvelope ::
  Context ->
  WidgetId ->
  Float ->
  Int ->
  Layout ->
  IO (Maybe (Double, Double))
lookupDrawFitEnvelope ctx wid lh content incoming = do
  cached <- lookupIn dcsDrawFitCache ctx wid
  pure $ case cached of
    Just e
      | dfcLh e == lh
          && dfcContent e == content
          && dfcIn e == incoming ->
          Just (dfcDw e, dfcDh e)
    _ -> Nothing

-- | Drop cached ops for drawings that did not rebuild this frame.
pruneDrawOpCache :: Context -> IO ()
pruneDrawOpCache ctx =
  modifyIORef' (ctxDrawingCache ctx) $ \dc ->
    let live = dcsDrawings dc
        customLive = dcsCustomDrawings dc
     in dc
          { dcsDrawOpCache = dcsDrawOpCache dc `IM.intersection` live
          , dcsCustomDrawOpCache = dcsCustomDrawOpCache dc `IM.intersection` customLive
          , dcsDrawFitCache = dcsDrawFitCache dc `IM.intersection` live
          }

{-# INLINE registerCustomDrawing #-}
registerCustomDrawing :: Context -> WidgetId -> CustomDrawBuild -> IO ()
registerCustomDrawing = registerIn dcsCustomDrawings (\m dc -> dc {dcsCustomDrawings = m})

{-# INLINE lookupCustomDrawing #-}
lookupCustomDrawing :: Context -> WidgetId -> IO (Maybe CustomDrawBuild)
lookupCustomDrawing = lookupIn dcsCustomDrawings

-- | Cached draw ops for custom widgets with interaction state awareness.
cachedCustomDrawingOps ::
  Context ->
  WidgetId ->
  Rect ->
  CustomDrawContext ->
  CustomDrawBuild ->
  IO (Vector DrawOp)
cachedCustomDrawingOps ctx wid rect cdc build = do
  let k = intKey wid
      hov = cdcHovered cdc
      prs = cdcPressed cdc
      foc = cdcFocused cdc
  animated <- isAnimatingKey ctx k
  cached <- IM.lookup k . dcsCustomDrawOpCache <$> readIORef (ctxDrawingCache ctx)
  let hit = case cached of
        Just CustomDrawOpCacheEntry {cdeBounds = r, cdeHovered = h, cdePressed = p, cdeFocused = f, cdeOps = ops}
          | not animated && h == hov && p == prs && f == foc -> Just (r, ops)
        _ -> Nothing
  serveOps hit rect (build cdc rect) $ \ops ->
    modifyIORef' (ctxDrawingCache ctx) $ \s ->
      s {dcsCustomDrawOpCache = IM.insert k (CustomDrawOpCacheEntry rect hov prs foc ops) (dcsCustomDrawOpCache s)}

{-# INLINE registerCustomMeasure #-}
registerCustomMeasure :: Context -> WidgetId -> CustomMeasureFn -> IO ()
registerCustomMeasure = registerIn dcsCustomMeasures (\m dc -> dc {dcsCustomMeasures = m})

{-# INLINE lookupCustomMeasure #-}
lookupCustomMeasure :: Context -> WidgetId -> IO (Maybe CustomMeasureFn)
lookupCustomMeasure = lookupIn dcsCustomMeasures

{-# INLINE registerCustomCursor #-}
registerCustomCursor :: Context -> WidgetId -> (CustomDrawContext -> UiCursorKind) -> IO ()
registerCustomCursor = registerIn dcsCustomCursors (\m dc -> dc {dcsCustomCursors = m})

{-# INLINE lookupCustomCursor #-}
lookupCustomCursor :: Context -> WidgetId -> IO (Maybe (CustomDrawContext -> UiCursorKind))
lookupCustomCursor = lookupIn dcsCustomCursors

{-# INLINE registerCustomDamageSlop #-}
registerCustomDamageSlop :: Context -> WidgetId -> Float -> IO ()
registerCustomDamageSlop = registerIn dcsCustomDamageSlop (\m dc -> dc {dcsCustomDamageSlop = m})

{-# INLINE lookupCustomDamageSlop #-}
lookupCustomDamageSlop :: Context -> WidgetId -> IO (Maybe Float)
lookupCustomDamageSlop = lookupIn dcsCustomDamageSlop

resetDrawingScopeCache :: Context -> IO ()
resetDrawingScopeCache ctx =
  modifyIORef' (ctxDrawingCache ctx) $ \dc ->
    dc
      { dcsDrawings = IM.empty
      , dcsPopupConfigs = IM.empty
      , dcsCustomMeasures = IM.empty
      , dcsCustomCursors = IM.empty
      , dcsCustomDrawings = IM.empty
      , dcsCustomDamageSlop = IM.empty
      }

-- | True when any node has a custom measure function, whose output is not
-- captured by the arena descriptor comparison, so whole-layout reuse must be
-- disabled for the frame.
hasCustomLayoutInputs :: Context -> IO Bool
hasCustomLayoutInputs ctx =
  not . IM.null . dcsCustomMeasures <$> readIORef (ctxDrawingCache ctx)
