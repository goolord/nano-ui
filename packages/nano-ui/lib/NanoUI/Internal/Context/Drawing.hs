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
  , registerCustomEntry
  , lookupCustomDrawing
  , cachedCustomDrawingOps
  , refreshCustomDrawingOps
  , drawingOpsStale
  , registerCustomMeasure
  , lookupCustomMeasure
  , customMeasureHooks
  , lookupCustomDamageSlop
  , resetDrawingScopeCache
  ) where

import Control.Monad (mfilter, when)
import Data.IORef (modifyIORef', readIORef, writeIORef)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IntSet (IntSet)
import Data.Primitive.SmallArray (SmallArray, mapSmallArray')

import NanoUI.Internal.Context.Animation (getLiveAnimations)
import NanoUI.Internal.Context.Types
import NanoUI.Internal.Draw (DrawOp, DrawingBuild, shiftDrawOp)
import NanoUI.Internal.Id (WidgetId)
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

{-# INLINE sameSize #-}
sameSize :: Rect -> Rect -> Bool
sameSize a b = rectW a == rectW b && rectH a == rectH b

-- | Ops built at @r@ for a same-size @rect@: as they are, or translated and
-- written back with @store@ when the widget moved.
{-# INLINE placeOps #-}
placeOps :: Rect -> Rect -> SmallArray DrawOp -> (SmallArray DrawOp -> IO ()) -> IO (SmallArray DrawOp)
placeOps r rect ops store
  | rectX r == rectX rect && rectY r == rectY rect = pure ops
  | otherwise = do
      let moved = mapSmallArray' (shiftDrawOp (rectX rect - rectX r) (rectY rect - rectY r)) ops
      store moved
      pure moved

-- | Rebuild draw ops when the content version or width/height change. A move
-- only translates. An unversioned drawing (content 0) additionally drops its
-- cache while the widget is animating, since it has no other invalidation
-- signal; versioned drawings are invalidated by their content key alone.
cachedDrawingOps :: Context -> WidgetId -> Int -> Rect -> DrawingBuild -> IO (SmallArray DrawOp)
cachedDrawingOps ctx wid content rect build = do
  animated <-
    if content == 0
      then IM.member (intKey wid) <$> getLiveAnimations ctx
      else pure False
  cached <- lookupIn dcsDrawOpCache ctx wid
  let store =
        registerIn dcsDrawOpCache (\m dc -> dc {dcsDrawOpCache = m}) ctx wid . DrawOpCacheEntry content rect
  case cached of
    Just (DrawOpCacheEntry c r ops)
      | c == content && not animated && sameSize r rect -> placeOps r rect ops store
    _ -> let ops = build rect in ops <$ store ops

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
  cached <- lookupIn dcsDrawFitCache ctx wid
  let k = intKey wid
  case cached of
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
pruneDrawOpCache ctx = do
  dc <- readIORef (ctxDrawingCache ctx)
  let live = dcsDrawings dc
      customLive = dcsCustomDrawings dc
      -- The intersection copies a map even when it keeps every key, so
      -- test first: most frames have nothing to drop.
      stale cache keep = not (IM.isSubmapOfBy (\_ _ -> True) cache keep)
  when (stale (dcsDrawOpCache dc) live || stale (dcsCustomDrawOpCache dc) customLive || stale (dcsDrawFitCache dc) live) $
    writeIORef (ctxDrawingCache ctx) $!
      dc
        { dcsDrawOpCache = dcsDrawOpCache dc `IM.intersection` live
        , dcsCustomDrawOpCache = dcsCustomDrawOpCache dc `IM.intersection` customLive
        , dcsDrawFitCache = dcsDrawFitCache dc `IM.intersection` live
        }

-- | Register an interaction-aware painter with the default cursor and
-- repaint margin. A nonzero content key must cover its external inputs; zero
-- requests rebuilding and comparison each frame.
{-# INLINE registerCustomDrawing #-}
registerCustomDrawing :: Context -> WidgetId -> Int -> CustomDrawBuild -> IO ()
registerCustomDrawing ctx wid content build =
  registerCustomEntry ctx wid (CustomDrawingEntry content build Nothing 0 False)

-- | Register a painter together with the cursor, repaint margin and pointer
-- tracking it asks for.
{-# INLINE registerCustomEntry #-}
registerCustomEntry :: Context -> WidgetId -> CustomDrawingEntry -> IO ()
registerCustomEntry = registerIn dcsCustomDrawings (\m dc -> dc {dcsCustomDrawings = m})

-- | Current custom painter and content key, or 'Nothing'.
{-# INLINE lookupCustomDrawing #-}
lookupCustomDrawing :: Context -> WidgetId -> IO (Maybe CustomDrawingEntry)
lookupCustomDrawing = lookupIn dcsCustomDrawings

-- | Whether a cache entry was built from these inputs, leaving aside where the
-- widget sits: ops built at another origin translate rather than rebuild.
{-# INLINE customEntryMatches #-}
customEntryMatches :: CustomDrawOpCacheEntry -> Int -> Rect -> CustomDrawContext -> Int -> Bool
customEntryMatches e content rect cdc gen =
  customEntrySized e content rect gen
    && cdeHovered e == cdcHovered cdc
    && cdePressed e == cdcPressed cdc
    && cdeFocused e == cdcFocused cdc
    && cdeDisabled e == cdcDisabled cdc

-- | 'customEntryMatches' without the interaction state: the content key, the
-- size and the metrics generation.
{-# INLINE customEntrySized #-}
customEntrySized :: CustomDrawOpCacheEntry -> Int -> Rect -> Int -> Bool
customEntrySized e content rect gen =
  cdeContent e == content
    && sameSize (cdeBounds e) rect
    && cdeGen e == gen

-- | Draw ops for a custom widget's paint: the ops 'refreshCustomDrawingOps'
-- settled on this frame, translated if the widget only moved, else a fresh
-- build with the draw context from @newCdc@.
--
-- The refresh ran against this frame's interaction state, which nothing
-- changes before paint, so an entry with this content, size and metrics is
-- this frame's and paint need not build a context to check it. Two nodes
-- sharing an id at different sizes miss here, and the one painted second
-- rebuilds.
cachedCustomDrawingOps ::
  Context ->
  WidgetId ->
  Int ->
  Rect ->
  IO CustomDrawContext ->
  CustomDrawBuild ->
  IO (SmallArray DrawOp)
cachedCustomDrawingOps ctx wid content rect newCdc build = do
  gen <- readIORef (ctxMetricGen ctx)
  cached <- lookupIn dcsCustomDrawOpCache ctx wid
  case cached of
    Just e
      | customEntrySized e content rect gen ->
          placeOps (cdeBounds e) rect (cdeOps e) $ \ops ->
            registerIn dcsCustomDrawOpCache (\m dc -> dc {dcsCustomDrawOpCache = m}) ctx wid e {cdeBounds = rect, cdeOps = ops}
    _ -> do
      cdc <- newCdc
      let ops = build cdc rect
      storeCustomDrawingOps ctx wid content rect cdc gen ops
      pure ops

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
  gen <- readIORef (ctxMetricGen ctx)
  cached <- lookupIn dcsCustomDrawOpCache ctx wid
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
      storeCustomDrawingOps ctx wid content rect cdc gen ops
      pure changed

storeCustomDrawingOps :: Context -> WidgetId -> Int -> Rect -> CustomDrawContext -> Int -> SmallArray DrawOp -> IO ()
storeCustomDrawingOps ctx wid content rect cdc gen ops =
  registerIn dcsCustomDrawOpCache (\m dc -> dc {dcsCustomDrawOpCache = m}) ctx wid $
    CustomDrawOpCacheEntry content rect (cdcHovered cdc) (cdcPressed cdc) (cdcFocused cdc) (cdcDisabled cdc) gen ops

-- | Whether a versioned drawing's cached ops are for another version at the
-- same rect. Paint rebuilds them; the pixels they covered must repaint too,
-- and checking the version costs nothing next to building the ops here.
drawingOpsStale :: Context -> WidgetId -> Int -> Rect -> IO Bool
drawingOpsStale ctx wid content rect = do
  cached <- lookupIn dcsDrawOpCache ctx wid
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

-- | The widgets with a measurement callback registered this view pass.
customMeasureHooks :: Context -> IO IntSet
customMeasureHooks ctx = IM.keysSet . dcsCustomMeasures <$> readIORef (ctxDrawingCache ctx)

-- | Registered repaint margin, or 'Nothing' when no override exists.
{-# INLINE lookupCustomDamageSlop #-}
lookupCustomDamageSlop :: Context -> WidgetId -> IO (Maybe Float)
lookupCustomDamageSlop ctx wid =
  mfilter (> 0) . fmap cdrDamageSlop <$> lookupIn dcsCustomDrawings ctx wid

-- | Clear per-pass registrations while retaining compiled ops and fitted
-- layouts. Call before rebuilding the view, then prune caches against new registrations.
resetDrawingScopeCache :: Context -> IO ()
resetDrawingScopeCache ctx =
  modifyIORef' (ctxDrawingCache ctx) $ \dc ->
    dc
      { dcsDrawings = IM.empty
      , dcsPopupConfigs = IM.empty
      , dcsCustomMeasures = IM.empty
      , dcsCustomDrawings = IM.empty
      }
