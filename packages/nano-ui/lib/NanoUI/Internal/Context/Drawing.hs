-- | Per-widget drawing registrations and the caches derived from them.
module NanoUI.Internal.Context.Drawing
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
  , refreshCustomDrawingOps
  , drawingOpsStale
  , registerCustomMeasure
  , lookupCustomMeasure
  , registerCustomCursor
  , lookupCustomCursor
  , registerCustomDamageSlop
  , lookupCustomDamageSlop
  , registerPointerTracked
  , isPointerTracked
  , resetDrawingScopeCache
  , hasCustomLayoutInputs
  ) where

import Data.IORef (modifyIORef', readIORef)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Primitive.SmallArray (SmallArray, mapSmallArray')

import NanoUI.Internal.Context.Animation (isAnimatingKey)
import NanoUI.Internal.Context.Types
  ( Context (..)
  , CustomDrawBuild
  , CustomDrawContext (..)
  , CustomDrawOpCacheEntry (..)
  , CustomDrawingEntry (..)
  , CustomMeasureFn
  , DrawFitCache (..)
  , DrawOpCacheEntry (..)
  , DrawingCacheState (..)
  , DrawingEntry (..)
  , PopupConfig (..)
  , intKey
  )
import NanoUI.Internal.Draw (DrawOp, DrawingBuild, shiftDrawOp)
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Input (UiCursorKind)
import NanoUI.Internal.Style (Layout)
import NanoUI.Internal.Types (PopupAnchor, PopupPlacement, Rect (..), rectH, rectW)

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

-- | Register a popup anchor, preferred placement, and logical-pixel offset
-- for the current view pass.
{-# INLINE registerPopupConfig #-}
registerPopupConfig :: Context -> WidgetId -> PopupAnchor -> PopupPlacement -> Float -> IO ()
registerPopupConfig ctx wid anchor placement offset =
  registerIn dcsPopupConfigs (\m dc -> dc {dcsPopupConfigs = m}) ctx wid (PopupConfig anchor placement offset)

-- | Current popup placement registration, or 'Nothing' for an unregistered id.
{-# INLINE lookupPopupConfig #-}
lookupPopupConfig :: Context -> WidgetId -> IO (Maybe (PopupAnchor, PopupPlacement, Float))
lookupPopupConfig ctx wid =
  fmap (\(PopupConfig anchor placement offset) -> (anchor, placement, offset))
    <$> lookupIn dcsPopupConfigs ctx wid

-- | Register a draw builder and content version. Change the version when
-- captured content changes without a size change.
{-# INLINE registerDrawing #-}
registerDrawing :: Context -> WidgetId -> Int -> DrawingBuild -> IO ()
registerDrawing ctx wid content build =
  registerIn dcsDrawings (\m dc -> dc {dcsDrawings = m}) ctx wid (DrawingEntry content build)

-- | Current drawing registration, or 'Nothing'.
{-# INLINE lookupDrawing #-}
lookupDrawing :: Context -> WidgetId -> IO (Maybe DrawingEntry)
lookupDrawing = lookupIn dcsDrawings

-- | One draw-op cache step. @hit@ holds the bounds and ops of an entry whose
-- key still matches. A same-size hit is reused, translated if the widget
-- moved; a miss or a resize uses @rebuilt@. Ops that differ from the entry are
-- written back with @store@.
serveOps ::
  Maybe (Rect, SmallArray DrawOp) ->
  Rect ->
  SmallArray DrawOp ->
  (SmallArray DrawOp -> IO ()) ->
  IO (SmallArray DrawOp)
serveOps hit rect rebuilt store =
  case hit of
    Just (r, ops)
      | rectW r == rectW rect && rectH r == rectH rect ->
          if rectX r == rectX rect && rectY r == rectY rect
            then pure ops
            else keep (mapSmallArray' (shiftDrawOp (rectX rect - rectX r) (rectY rect - rectY r)) ops)
    _ -> keep rebuilt
  where
    keep ops = store ops >> pure ops

-- | Rebuild draw ops when the content version or width/height change. A move
-- only translates. An unversioned drawing (content 0) additionally drops its
-- cache while the widget is animating, since it has no other invalidation
-- signal; versioned drawings are invalidated by their content key alone.
cachedDrawingOps :: Context -> WidgetId -> Int -> Rect -> DrawingBuild -> IO (SmallArray DrawOp)
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

-- | Cached drawing-envelope width/height if line height, content key, and
-- input layout still match. 'Nothing' requires measuring the envelope again.
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

-- | Drop cached ops for drawings not registered in the current view pass.
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

-- | Register an interaction-aware painter. A nonzero content key must cover
-- its external inputs; zero requests rebuilding and comparison each frame.
{-# INLINE registerCustomDrawing #-}
registerCustomDrawing :: Context -> WidgetId -> Int -> CustomDrawBuild -> IO ()
registerCustomDrawing ctx wid content build =
  registerIn dcsCustomDrawings (\m dc -> dc {dcsCustomDrawings = m}) ctx wid (CustomDrawingEntry content build)

-- | Current custom painter and content key, or 'Nothing'.
{-# INLINE lookupCustomDrawing #-}
lookupCustomDrawing :: Context -> WidgetId -> IO (Maybe CustomDrawingEntry)
lookupCustomDrawing = lookupIn dcsCustomDrawings

-- | Whether a cache entry was built from these inputs, leaving aside where the
-- widget sits: ops built at another origin translate rather than rebuild.
{-# INLINE customEntryMatches #-}
customEntryMatches :: CustomDrawOpCacheEntry -> Int -> Rect -> CustomDrawContext -> Int -> Bool
customEntryMatches e content rect cdc gen =
  cdeContent e == content
    && rectW (cdeBounds e) == rectW rect
    && rectH (cdeBounds e) == rectH rect
    && cdeHovered e == cdcHovered cdc
    && cdePressed e == cdcPressed cdc
    && cdeFocused e == cdcFocused cdc
    && cdeDisabled e == cdcDisabled cdc
    && cdeGen e == gen

-- | Draw ops for a custom widget's paint: the ops 'refreshCustomDrawingOps'
-- settled on this frame while every input still matches, translated if the
-- widget only moved, else a fresh build.
cachedCustomDrawingOps ::
  Context ->
  WidgetId ->
  Int ->
  Rect ->
  CustomDrawContext ->
  CustomDrawBuild ->
  IO (SmallArray DrawOp)
cachedCustomDrawingOps ctx wid content rect cdc build = do
  let k = intKey wid
  gen <- readIORef (ctxMetricGen ctx)
  cached <- IM.lookup k . dcsCustomDrawOpCache <$> readIORef (ctxDrawingCache ctx)
  let hit = case cached of
        Just e | customEntryMatches e content rect cdc gen -> Just (cdeBounds e, cdeOps e)
        _ -> Nothing
  serveOps hit rect (build cdc rect) (storeCustomDrawingOps ctx k content rect cdc gen)

-- | Settle a custom widget's ops for this frame and cache them for paint,
-- returning whether what it draws changed at an unchanged rect.
--
-- A widget that declares a content key is taken at its word, as a versioned
-- drawing is: an unchanged key with unchanged size, interaction state and
-- metrics neither rebuilds the ops nor repaints them, animating or not, so a
-- drawing that reads an animated value has to fold it into its key. One that
-- only moved keeps its ops too; paint translates them. Without a key (0) the
-- build can read anything (a sort flag, a fraction), and nothing but building
-- it shows that its output changed, so it is rebuilt and compared.
--
-- Whatever forced a rebuild, the ops it produced decide the damage, so a key
-- bumped without a visible change repaints nothing and a rebuild the key never
-- mentioned still repaints. A new, moved or resized widget reports no change:
-- rect damage covers it.
refreshCustomDrawingOps ::
  Context ->
  WidgetId ->
  Int ->
  Rect ->
  CustomDrawContext ->
  CustomDrawBuild ->
  IO Bool
refreshCustomDrawingOps ctx wid content rect cdc build = do
  let k = intKey wid
  gen <- readIORef (ctxMetricGen ctx)
  cached <- IM.lookup k . dcsCustomDrawOpCache <$> readIORef (ctxDrawingCache ctx)
  let keyed = content /= 0
  case cached of
    -- A keyed widget that only moved keeps its ops: paint translates them, and
    -- the move is damaged by the rect delta.
    Just e | keyed && customEntryMatches e content rect cdc gen -> pure False
    _ -> do
      let ops = build cdc rect
          -- Whatever made this frame rebuild - the key, the interaction state,
          -- a theme or font change - the ops are built now, so ask them
          -- directly rather than trusting the key for damage as well.
          changed = case cached of
            Just e | cdeBounds e == rect -> cdeOps e /= ops
            _ -> False
      storeCustomDrawingOps ctx k content rect cdc gen ops
      pure changed

storeCustomDrawingOps :: Context -> Int -> Int -> Rect -> CustomDrawContext -> Int -> SmallArray DrawOp -> IO ()
storeCustomDrawingOps ctx k content rect cdc gen ops =
  modifyIORef' (ctxDrawingCache ctx) $ \s ->
    let entry =
          CustomDrawOpCacheEntry
            content
            rect
            (cdcHovered cdc)
            (cdcPressed cdc)
            (cdcFocused cdc)
            (cdcDisabled cdc)
            gen
            ops
     in s {dcsCustomDrawOpCache = IM.insert k entry (dcsCustomDrawOpCache s)}

-- | Whether a versioned drawing's cached ops are for another version at the
-- same rect. Paint rebuilds them; the pixels they covered must repaint too,
-- and checking the version costs nothing next to building the ops here.
drawingOpsStale :: Context -> WidgetId -> Int -> Rect -> IO Bool
drawingOpsStale ctx wid content rect = do
  cached <- IM.lookup (intKey wid) . dcsDrawOpCache <$> readIORef (ctxDrawingCache ctx)
  pure $ case cached of
    Just DrawOpCacheEntry {doeContent = c, doeBounds = r} -> c /= content && r == rect
    Nothing -> False

-- | Register a widget measurement callback for the current view pass.
{-# INLINE registerCustomMeasure #-}
registerCustomMeasure :: Context -> WidgetId -> CustomMeasureFn -> IO ()
registerCustomMeasure = registerIn dcsCustomMeasures (\m dc -> dc {dcsCustomMeasures = m})

-- | Registered measurement callback, or 'Nothing' for default layout sizing.
{-# INLINE lookupCustomMeasure #-}
lookupCustomMeasure :: Context -> WidgetId -> IO (Maybe CustomMeasureFn)
lookupCustomMeasure = lookupIn dcsCustomMeasures

-- | Register cursor selection from a custom widget's interaction state.
{-# INLINE registerCustomCursor #-}
registerCustomCursor :: Context -> WidgetId -> (CustomDrawContext -> UiCursorKind) -> IO ()
registerCustomCursor = registerIn dcsCustomCursors (\m dc -> dc {dcsCustomCursors = m})

-- | Registered cursor selector, or 'Nothing'.
{-# INLINE lookupCustomCursor #-}
lookupCustomCursor :: Context -> WidgetId -> IO (Maybe (CustomDrawContext -> UiCursorKind))
lookupCustomCursor = lookupIn dcsCustomCursors

-- | Register extra logical-pixel repaint margin for a custom widget's overdraw.
{-# INLINE registerCustomDamageSlop #-}
registerCustomDamageSlop :: Context -> WidgetId -> Float -> IO ()
registerCustomDamageSlop = registerIn dcsCustomDamageSlop (\m dc -> dc {dcsCustomDamageSlop = m})

-- | Registered repaint margin, or 'Nothing' when no override exists.
{-# INLINE lookupCustomDamageSlop #-}
lookupCustomDamageSlop :: Context -> WidgetId -> IO (Maybe Float)
lookupCustomDamageSlop = lookupIn dcsCustomDamageSlop

-- | Ask for a frame whenever the pointer moves over a widget, not only when
-- it crosses onto another one: for a widget that draws what is under the
-- pointer inside itself, such as the row of a self-drawn list.
{-# INLINE registerPointerTracked #-}
registerPointerTracked :: Context -> WidgetId -> IO ()
registerPointerTracked ctx wid = registerIn dcsPointerTracked (\m dc -> dc {dcsPointerTracked = m}) ctx wid ()

-- | Whether a widget asked for a frame on every pointer move over it.
{-# INLINE isPointerTracked #-}
isPointerTracked :: Context -> WidgetId -> IO Bool
isPointerTracked ctx wid = (== Just ()) <$> lookupIn dcsPointerTracked ctx wid

-- | Clear per-pass registrations while retaining compiled ops and fitted
-- layouts. Call before rebuilding the view, then prune caches against new registrations.
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
      , dcsPointerTracked = IM.empty
      }

-- | True when any node has a custom measure function, whose output is not
-- captured by the arena descriptor comparison, so whole-layout reuse must be
-- disabled for the frame.
hasCustomLayoutInputs :: Context -> IO Bool
hasCustomLayoutInputs ctx =
  not . IM.null . dcsCustomMeasures <$> readIORef (ctxDrawingCache ctx)
