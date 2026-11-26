-- | Per-widget drawing registrations and the caches derived from them.
module NanoUI.Internal.Context.Drawing
  ( registerPopupConfig
  , lookupPopupConfig
  , registerDropdownPopup
  , dropdownPopups
  , registerHoverZone
  , hoverZoneCrossed
  , registerDrawing
  , cachedDrawingOps
  , cachedWidgetLayout
  , lookupDrawFitEnvelope
  , pruneDrawOpCache
  , registerCustomDrawing
  , registerCustomEntry
  , lookupCustomDrawing
  , lookupCustomDrawingAt
  , lookupDrawingAt
  , lookupCustomMeasureAt
  , customDrawingOffNode
  , cachedCustomDrawingOps
  , refreshCustomDrawingOps
  , drawingOpsStale
  , registerCustomMeasure
  , lookupCustomMeasure
  , customMeasureHooks
  , lookupCustomDamageSlop
  , resetDrawingScopeCache
  , registerNodeShaderAt
  , lookupNodeShaderAt
  , registerShaderEffectAt
  , lookupShaderEffectAt
  , activeEffectNodes
  , getShaderCapabilities
  ) where

import Control.Monad (mfilter, unless, when)
import Data.Bits (complement)
import Data.IORef (modifyIORef', readIORef, writeIORef)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Maybe (isJust)
import Data.Primitive.Array (readArray, sizeofMutableArray, writeArray)
import Data.Primitive.PrimArray (readPrimArray, setPrimArray, writePrimArray)
import Data.Primitive.PrimVar (readPrimVar)
import Data.Primitive.SmallArray (SmallArray, mapSmallArray')
import Data.Word (Word64)

import NanoUI.Internal.Context.Animation (isAnimatingKey)
import NanoUI.Internal.Context.Types
import NanoUI.Internal.Draw (DrawOp, DrawingBuild, shiftDrawOp)
import NanoUI.Internal.Id (WidgetId, mix64)
import NanoUI.Internal.Layout.Arena (NodeArena (..), NodeIdx, NodeType (..), getNodeType, lookupNodeByKey, lookupNodeByWidgetId)
import NanoUI.Internal.Layout.Arena.Types (growBoxedStoreCopy)
import NanoUI.Internal.Shader (ShaderCapabilities, ShaderEffectEntry, ShaderNodeEntry)
import NanoUI.Internal.Store (ptrEq)
import NanoUI.Internal.Style (Layout)
import NanoUI.Internal.Types (PopupAnchor, PopupPlacement, Rect (..), V2, rectContains, rectH, rectW)

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

-- | Register popup @wid@ as a dropdown for the current view pass:
-- placement puts it where its field's rect this frame puts its dropdown, not
-- by an anchor.
registerDropdownPopup :: Context -> WidgetId -> DropdownPopup -> IO ()
registerDropdownPopup = registerIn dcsDropdowns (\m dc -> dc {dcsDropdowns = m})

-- | The dropdown popups this pass registered, by popup key
-- ('registerDropdownPopup').
{-# INLINE dropdownPopups #-}
dropdownPopups :: Context -> IO (IntMap DropdownPopup)
dropdownPopups ctx = dcsDropdowns <$> readIORef (ctxDrawingCache ctx)

-- | Request a frame when the pointer enters or leaves @rect@ this pass. Used
-- for tooltip targets, which may be non-interactive labels or containers
-- skipped by normal hit-testing. A @tracked@ rect also requests a frame on
-- every pointer move inside it, for an open tooltip that follows the pointer.
registerHoverZone :: Context -> Bool -> Rect -> IO ()
registerHoverZone ctx tracked rect =
  modifyIORef' (ctxDrawingCache ctx) $ \dc -> dc {dcsHoverZones = HoverZone rect tracked : dcsHoverZones dc}

-- | Whether a pointer move from @from@ to @to@ enters or leaves a rect
-- registered this pass, or moves over a tracked one ('registerHoverZone').
hoverZoneCrossed :: Context -> V2 -> V2 -> IO Bool
hoverZoneCrossed ctx from to =
  any crossed . dcsHoverZones <$> readIORef (ctxDrawingCache ctx)
  where
    crossed (HoverZone r tracked) = let on = rectContains r from in on /= rectContains r to || (on && tracked)

-- | Node @idx@'s registrations this view pass ('ctxNodeCustom').
{-# INLINE nodeCustomAt #-}
nodeCustomAt :: Context -> NodeIdx -> IO NodeCustom
nodeCustomAt ctx idx = do
  arr <- readIORef (ctxNodeCustom ctx)
  if idx < 0 || idx >= sizeofMutableArray arr
    then pure noNodeCustom
    else do
      nc <- readArray arr idx
      ep <- readPrimVar (naEpoch (ctxNodeArena ctx))
      pure (if ncEpoch nc == ep then nc else noNodeCustom)

-- | The registrations of the node widget @wid@ was declared as.
{-# INLINE nodeCustomOf #-}
nodeCustomOf :: Context -> WidgetId -> IO NodeCustom
nodeCustomOf ctx wid = maybe (pure noNodeCustom) (nodeCustomAt ctx) =<< lookupNodeByWidgetId (ctxNodeArena ctx) wid

-- | Change the registrations of node @idx@ directly.
{-# INLINE registerAtIdx #-}
registerAtIdx :: Context -> NodeIdx -> (NodeCustom -> IO NodeCustom) -> IO ()
registerAtIdx ctx idx f = do
  arr0 <- readIORef (ctxNodeCustom ctx)
  let size = sizeofMutableArray arr0
  arr <-
    if idx < size
      then pure arr0
      else do
        grown <- growBoxedStoreCopy noNodeCustom arr0 size (max (idx + 1) (2 * size))
        grown <$ writeIORef (ctxNodeCustom ctx) grown
  ep <- readPrimVar (naEpoch (ctxNodeArena ctx))
  old <- readArray arr idx
  new <- f (if ncEpoch old == ep then old else noNodeCustom {ncEpoch = ep})
  writeArray arr idx $! new

-- | Change the registrations of the node widget @wid@ was declared as; a
-- widget with no node this pass registers nothing.
{-# INLINE registerAt #-}
registerAt :: Context -> WidgetId -> (NodeIdx -> NodeCustom -> IO NodeCustom) -> IO ()
registerAt ctx wid f =
  lookupNodeByWidgetId (ctxNodeArena ctx) wid >>= mapM_ (\idx -> registerAtIdx ctx idx (f idx))

-- | Attach a leaf 'ShaderNodeEntry' to node @idx@ for the current view pass.
{-# INLINE registerNodeShaderAt #-}
registerNodeShaderAt :: Context -> NodeIdx -> ShaderNodeEntry -> IO ()
registerNodeShaderAt ctx idx entry =
  registerAtIdx ctx idx (\nc -> pure nc {ncShader = Just entry})

-- | Look up the leaf 'ShaderNodeEntry' on node @idx@, if any.
{-# INLINE lookupNodeShaderAt #-}
lookupNodeShaderAt :: Context -> NodeIdx -> IO (Maybe ShaderNodeEntry)
lookupNodeShaderAt ctx idx = ncShader <$> nodeCustomAt ctx idx

-- | Attach a Tier 2 'ShaderEffectEntry' to container node @idx@ for the
-- current view pass.
{-# INLINE registerShaderEffectAt #-}
registerShaderEffectAt :: Context -> NodeIdx -> ShaderEffectEntry -> IO ()
registerShaderEffectAt ctx idx entry = do
  registerAtIdx ctx idx (\nc -> pure nc {ncEffect = Just entry})
  ep <- readPrimVar (naEpoch (ctxNodeArena ctx))
  (curEp, ns) <- readIORef (ctxEffectNodes ctx)
  writeIORef (ctxEffectNodes ctx) $! if curEp == ep then (ep, idx : ns) else (ep, [idx])

-- | Look up the Tier 2 'ShaderEffectEntry' on container node @idx@, if any.
{-# INLINE lookupShaderEffectAt #-}
lookupShaderEffectAt :: Context -> NodeIdx -> IO (Maybe ShaderEffectEntry)
lookupShaderEffectAt ctx idx = ncEffect <$> nodeCustomAt ctx idx

-- | Container node indices carrying Tier 2 shader effects in the current build
-- epoch, or empty when the frame declared none.
{-# INLINE activeEffectNodes #-}
activeEffectNodes :: Context -> IO [NodeIdx]
activeEffectNodes ctx = do
  ep <- readPrimVar (naEpoch (ctxNodeArena ctx))
  (curEp, ns) <- readIORef (ctxEffectNodes ctx)
  pure (if curEp == ep then ns else [])

-- | Read the shader capabilities reported by the active backend.
{-# INLINE getShaderCapabilities #-}
getShaderCapabilities :: Context -> IO ShaderCapabilities
getShaderCapabilities ctx = readIORef (ctxShaderCaps ctx)

-- | Add @n@ to word @w@ of 'ctxCustomPass'.
{-# INLINE bumpPass #-}
bumpPass :: Context -> Int -> Word64 -> IO ()
bumpPass ctx w n = readPrimArray (ctxCustomPass ctx) w >>= writePrimArray (ctxCustomPass ctx) w . (+ n)

-- | Register a draw builder and content version. Change the version when
-- captured content changes without a size change.
{-# INLINE registerDrawing #-}
registerDrawing :: Context -> WidgetId -> Int -> DrawingBuild -> IO ()
registerDrawing ctx wid content build =
  registerAt ctx wid $ \_ nc -> do
    when (null (ncDrawing nc)) (bumpPass ctx 2 1)
    pure nc {ncDrawing = Just (DrawingEntry content build)}

-- | The 'DrawingEntry' registered on node @idx@, if any.
{-# INLINE lookupDrawingAt #-}
lookupDrawingAt :: Context -> NodeIdx -> IO (Maybe DrawingEntry)
lookupDrawingAt ctx idx = ncDrawing <$> nodeCustomAt ctx idx

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
-- cache while the widget is animating, and on the frame after, since it has
-- no other invalidation signal; versioned drawings are invalidated by their
-- content key alone.
cachedDrawingOps :: Context -> WidgetId -> Int -> Rect -> DrawingBuild -> IO (SmallArray DrawOp)
cachedDrawingOps ctx wid content rect build = do
  animated <-
    if content == 0
      then isAnimatingKey ctx wid
      else pure False
  cached <- lookupIn dcsDrawOpCache ctx wid
  -- Ops built while animating are stale on the next frame, even one whose
  -- animation ended before paint: its view drew the widget again.
  let store =
        registerIn dcsDrawOpCache (\m dc -> dc {dcsDrawOpCache = m}) ctx wid
          . DrawOpCacheEntry (if animated then complement content else content) rect
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
          , -- Mark the ops stale rather than deleting the entry so
            -- 'drawingOpsStale' detects the change during the damage pass.
            dcsDrawOpCache = IM.adjust (\e -> e {doeContent = complement content}) k (dcsDrawOpCache s)
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
-- A cache holds an entry only for a widget that registered, so one with no
-- more entries than this pass's registrations holds none to drop, and most
-- frames compare sizes only. One that holds more keeps the keys that
-- registered.
pruneDrawOpCache :: Context -> IO ()
pruneDrawOpCache ctx = do
  dc <- readIORef (ctxDrawingCache ctx)
  unless (IM.null (dcsDrawOpCache dc) && IM.null (dcsCustomDrawOpCache dc) && IM.null (dcsDrawFitCache dc)) $
    pruneDrawOpCaches ctx dc

-- | 'pruneDrawOpCache' once some cache holds an entry.
pruneDrawOpCaches :: Context -> DrawingCacheState -> IO ()
pruneDrawOpCaches ctx dc = do
  drawings <- fromIntegral <$> readPrimArray (ctxCustomPass ctx) 2
  customs <- fromIntegral <$> readPrimArray (ctxCustomPass ctx) 3
  let keep :: Int -> (NodeCustom -> Bool) -> IntMap a -> IO (IntMap a)
      keep live registered cache
        | IM.size cache <= live = pure cache
        | otherwise = flip IM.traverseMaybeWithKey cache $ \k v -> do
            nc <- maybe (pure noNodeCustom) (nodeCustomAt ctx) =<< lookupNodeByKey (ctxNodeArena ctx) k
            pure (if registered nc then Just v else Nothing)
  ops <- keep drawings (isJust . ncDrawing) (dcsDrawOpCache dc)
  customOps <- keep customs (isJust . ncCustom) (dcsCustomDrawOpCache dc)
  fits <- keep drawings (isJust . ncDrawing) (dcsDrawFitCache dc)
  when (not (ptrEq ops (dcsDrawOpCache dc) && ptrEq customOps (dcsCustomDrawOpCache dc) && ptrEq fits (dcsDrawFitCache dc))) $
    writeIORef (ctxDrawingCache ctx) $! dc {dcsDrawOpCache = ops, dcsCustomDrawOpCache = customOps, dcsDrawFitCache = fits}

-- | Register an interaction-aware painter with the default cursor and
-- repaint margin. A nonzero content key must cover its external inputs; zero
-- requests rebuilding and comparison each frame.
{-# INLINE registerCustomDrawing #-}
registerCustomDrawing :: Context -> WidgetId -> Int -> CustomDrawBuild -> IO ()
registerCustomDrawing ctx wid content build =
  registerCustomEntry ctx wid (CustomDrawingEntry content build Nothing 0 False KeysNone)

-- | Store the widget's 'CustomDrawingEntry', replacing any earlier one.
{-# INLINE registerCustomEntry #-}
registerCustomEntry :: Context -> WidgetId -> CustomDrawingEntry -> IO ()
registerCustomEntry ctx wid entry =
  registerAt ctx wid $ \idx nc -> do
    when (null (ncCustom nc)) $ do
      bumpPass ctx 3 1
      nt <- getNodeType (ctxNodeArena ctx) idx
      when (nt /= NodeDrawing) (writePrimArray (ctxCustomPass ctx) 0 1)
    pure nc {ncCustom = Just entry}

-- | The 'CustomDrawingEntry' registered for the widget, if any.
{-# INLINE lookupCustomDrawing #-}
lookupCustomDrawing :: Context -> WidgetId -> IO (Maybe CustomDrawingEntry)
lookupCustomDrawing ctx wid = ncCustom <$> nodeCustomOf ctx wid

-- | The 'CustomDrawingEntry' registered on node @idx@, if any.
{-# INLINE lookupCustomDrawingAt #-}
lookupCustomDrawingAt :: Context -> NodeIdx -> IO (Maybe CustomDrawingEntry)
lookupCustomDrawingAt ctx idx = ncCustom <$> nodeCustomAt ctx idx

-- | Whether a custom drawing was registered this pass on a node that is no
-- drawing (a pane grid's focus ring), which paint builds afresh.
customDrawingOffNode :: Context -> IO Bool
customDrawingOffNode ctx = (/= 0) <$> readPrimArray (ctxCustomPass ctx) 0

-- | Whether a cache entry matches the given content key, size, draw context,
-- and font generation (ignoring origin, which only translates cached ops).
{-# INLINE customEntryMatches #-}
customEntryMatches :: CustomDrawOpCacheEntry -> Int -> Rect -> CustomDrawContext -> Int -> Bool
customEntryMatches e content rect cdc gen =
  customEntrySized e content rect gen
    && cdeHovered e == cdcHovered cdc
    && cdePressed e == cdcPressed cdc
    && cdeFocused e == cdcFocused cdc
    && cdeActive e == cdcActive cdc
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

-- | Update the cached draw ops for a custom widget and return whether the
-- output changed in place (at the same bounds).
--
-- A non-zero content key skips rebuilding when the key, size, interaction
-- state, and font generation all match the cached entry (callers that depend
-- on an animation value must include it in the key). A key of 0 always
-- rebuilds and compares the resulting 'DrawOp' array.
--
-- When a rebuild runs, damage is determined by comparing the new ops against
-- the previous ops at the same rect; new, moved, or resized widgets return
-- 'False' because rect-diff damage already covers them.
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
          changed = case cached of
            Just e | cdeBounds e == rect -> cdeOps e /= ops
            _ -> False
      storeCustomDrawingOps ctx wid content rect cdc gen ops
      pure changed

storeCustomDrawingOps :: Context -> WidgetId -> Int -> Rect -> CustomDrawContext -> Int -> SmallArray DrawOp -> IO ()
storeCustomDrawingOps ctx wid content rect cdc gen ops =
  registerIn dcsCustomDrawOpCache (\m dc -> dc {dcsCustomDrawOpCache = m}) ctx wid $
    CustomDrawOpCacheEntry content rect (cdcHovered cdc) (cdcPressed cdc) (cdcFocused cdc) (cdcActive cdc) (cdcDisabled cdc) gen ops

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
registerCustomMeasure ctx wid fn =
  registerAt ctx wid $ \idx nc -> do
    when (null (ncMeasure nc)) (bumpPass ctx 1 (mix64 (fromIntegral idx) 0x4d454153))
    pure nc {ncMeasure = Just fn}

-- | Registered measurement callback, or 'Nothing' for default layout sizing.
{-# INLINE lookupCustomMeasure #-}
lookupCustomMeasure :: Context -> WidgetId -> IO (Maybe CustomMeasureFn)
lookupCustomMeasure ctx wid = ncMeasure <$> nodeCustomOf ctx wid

-- | The measurement callback registered on node @idx@, if any.
{-# INLINE lookupCustomMeasureAt #-}
lookupCustomMeasureAt :: Context -> NodeIdx -> IO (Maybe CustomMeasureFn)
lookupCustomMeasureAt ctx idx = ncMeasure <$> nodeCustomAt ctx idx

-- | A signature of the nodes with a measurement callback registered this
-- view pass: a sum of a hash of each one's index, so the layout cache can
-- tell when one appears or goes.
customMeasureHooks :: Context -> IO Word64
customMeasureHooks ctx = readPrimArray (ctxCustomPass ctx) 1

-- | Registered repaint margin, or 'Nothing' when no override exists.
{-# INLINE lookupCustomDamageSlop #-}
lookupCustomDamageSlop :: Context -> WidgetId -> IO (Maybe Float)
lookupCustomDamageSlop ctx wid =
  mfilter (> 0) . fmap cdrDamageSlop <$> lookupCustomDrawing ctx wid

-- | Clear per-pass registrations while retaining compiled ops and fitted
-- layouts. Call before rebuilding the view, then prune caches against new registrations.
resetDrawingScopeCache :: Context -> IO ()
resetDrawingScopeCache ctx = do
  setPrimArray (ctxCustomPass ctx) 0 customPassWords 0
  modifyIORef' (ctxDrawingCache ctx) $ \dc ->
    dc
      { dcsPopupConfigs = IM.empty
      , dcsDropdowns = IM.empty
      , dcsHoverZones = []
      }
