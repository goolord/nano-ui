-- | Compare completed frames and explicit invalidations to choose repaint bounds.
-- Damage is computed before painting so retained backends can redraw only a clip.
module NanoUI.Internal.Damage
  ( updatePrevRects
  , floatingPanelRects
  , FrameSnapshot (..)
  , captureFrameSnapshot
  , writeDamage
  , damagePieces
  ) where

import Control.Applicative ((<|>))
import Control.Exception (evaluate)
import Control.Monad (filterM, forM_, unless, when, (<$!>), (>=>))
import Data.Bits (xor)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.List (partition, tails)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Hashable (hashWithSalt)
import NanoUI.Internal.Context
import NanoUI.Internal.Id (WidgetId (..), hashWidgetId)
import NanoUI.Internal.Input
import NanoUI.Internal.Store (diffKeys, eqByPtr, mirrorStoresChanged, ptrEq, slotChangedKeys)
import NanoUI.Internal.Layout.Arena
import NanoUI.Internal.Frame.Scroll.Geometry (decodeScrollConfig, scrollBare)
import NanoUI.Internal.Widgets.Custom (mkCustomDrawContext)
import NanoUI.Internal.Types

layoutSettleMinArea :: Float
layoutSettleMinArea = 0.25

-- Partial retain clears with themeWindow. Expand interaction clips to the painted
-- panel/window backdrop so slop pixels get the correct fill, not window color.
backdropRectFromNode :: Context -> Int -> IO (Maybe Rect)
backdropRectFromNode Context {ctxNodeArena = na} idx = walkAncestors na idx $ \i -> do
  nt <- getNodeType na i
  paints <- if isFloatingNode nt then pure True else containerPaints na i nt
  if paints then getNonzeroRect na i else pure Nothing

-- | Whether a container paints a backdrop of its own: a panel, or a scroll
-- container with a well, which a bare one and one that grows both ways lack.
{-# INLINE containerPaints #-}
containerPaints :: NodeArena -> NodeIdx -> NodeType -> IO Bool
containerPaints na i nt = case nt of
  NodeScrollContainer -> do
    wTag <- axTag <$> getWidthSizing na i
    hTag <- axTag <$> getHeightSizing na i
    si <- getStyleIdx na i
    pure (not ((wTag == SizingGrow && hTag == SizingGrow) || scrollBare (decodeScrollConfig si)))
  _ -> pure (nt == NodePanel)

-- | The key a painting container without a widget id is walked under: its
-- nearest keyed ancestor's key mixed with how far past that ancestor it was
-- added. It holds while nothing is added or dropped ahead of it inside that
-- ancestor, and otherwise changes, which damage takes as one container going
-- and another arriving.
containerKey :: NodeArena -> NodeIdx -> IO Int
containerKey na idx = go idx
  where
    go i = do
      p <- getParent na i
      if p < 0
        then pure (mix 0 idx)
        else do
          w <- getWidgetId na p
          if hashWidgetId w /= 0 then pure (mix (intKey w) (idx - p)) else go p
    -- Key 0 names nothing.
    mix k off = let h = hashWithSalt k off `xor` 0x3C6EF372FE94F82A in if h == 0 then 1 else h

-- | The clip node @idx@ is painted in: the one its parent hands its content
-- ('NanoUI.Internal.Frame.Scroll.applyScrollOffsets'). For a scroll container
-- or a panel it is wider than the node's own clip, which is what it hands
-- its own content: its viewport, or its inside.
outerClip :: NodeArena -> Rect -> NodeIdx -> IO Rect
outerClip na window idx = do
  p <- getParent na idx
  if p < 0
    then pure window
    else do
      clip <- fromMaybe window <$> getClipBounds na p
      pt <- getNodeType na p
      -- A widget's content is clipped to the widget as well.
      if isWidgetNode pt
        then do
          r <- getNodeRect na p
          pure (fromMaybe (Rect (rectX r) (rectY r) 0 0) (rectIntersect clip r))
        else pure clip

{-# INLINE getNonzeroRect #-}
getNonzeroRect :: NodeArena -> Int -> IO (Maybe Rect)
getNonzeroRect arena i = do
  r <- getNodeRect arena i
  pure (if rectNonEmpty r then Just r else Nothing)

updatePrevRects :: Context -> Size -> IO ()
updatePrevRects ctx@Context {ctxNodeArena = na} size@(Size winW winH) = do
  PrevFrame oldRects oldClips oldOuters oldTexts oldLooks <- getsDamage ctx dsPrev
  count <- arenaCount na
  let setPrev p = modifyDamage ctx (\ds -> ds {dsPrev = p})
  if count <= 0
    then setPrev emptyPrevFrame
    else do
      -- Scroll containers and panels ('BackdropNodes'), and no other nodes,
      -- first: the clip each is painted in, and, for one that paints and has
      -- no widget id, such as a panel, its rect under a key of its own
      -- ('containerKey'), so that one that moves or changes size repaints
      -- where it was.
      (containerCount, containerAt) <- classNodes na BackdropNodes
      let window = Rect 0 0 winW winH
          containers !j !m !om !foundOld !foundOuter !dropped
            | j >= containerCount = pure (m, om, foundOld, foundOuter, dropped)
            | otherwise = do
                idx <- containerAt j
                wid <- getWidgetId na idx
                superseded <- if hashWidgetId wid == 0 then pure False else getIdSuperseded na idx
                nt <- getNodeType na idx
                keyless <- if hashWidgetId wid == 0 then containerPaints na idx nt else pure False
                if superseded || (hashWidgetId wid == 0 && not keyless)
                  then containers (j + 1) m om foundOld foundOuter dropped
                  else do
                    k <- if keyless then containerKey na idx else pure (intKey wid)
                    let isOld = keyless && IM.member k oldRects
                    mRect <- getNonzeroRect na idx
                    case mRect of
                      Nothing ->
                        containers (j + 1)
                          (if isOld then IM.delete k m else m)
                          (dropKey k om)
                          foundOld foundOuter (dropped || isOld)
                      Just r -> do
                        o <- outerClip na window idx
                        containers (j + 1)
                          (if keyless then putNew k r m else m)
                          (putNew k o om)
                          (foundOld + if isOld then 1 else 0)
                          (foundOuter + if IM.member k oldOuters then 1 else 0)
                          dropped
      (m0, om, foundContainers, foundOuter, droppedContainer) <- containers 0 oldRects oldOuters 0 0 False
      -- Then every node with a widget id. Walk the arena from base maps,
      -- touching only entries whose value changed. Seeded with last frame's
      -- maps, frames with stable rects (hover, text churn, animations)
      -- allocate nothing. The walk cannot delete keys that vanished from the
      -- arena, so when the key set changed it clears the maps and walks again
      -- from empty ones, where no key counts as old. Of nodes that share a
      -- key (a table's frozen and scrolling panes) only the last, the one
      -- 'lookupNodeByKey' finds, is walked ('getIdSuperseded'): counting each
      -- would stand in for a key that went away, and writing each would
      -- rewrite the maps every frame.
      let go !i !m !cm !tm !lm !foundOld !dropped
            | i >= count =
                if dropped || foundOld /= IM.size oldRects || foundOuter /= IM.size oldOuters
                  then setPrev emptyPrevFrame >> updatePrevRects ctx size
                  else
                    unless (ptrEq m oldRects && ptrEq cm oldClips && ptrEq om oldOuters && ptrEq tm oldTexts && ptrEq lm oldLooks) $
                      setPrev (PrevFrame m cm om tm lm)
            | otherwise = do
                wid <- getWidgetId na i
                superseded <- if hashWidgetId wid == 0 then pure True else getIdSuperseded na i
                if superseded
                  then go (i + 1) m cm tm lm foundOld dropped
                  else do
                    let !k = intKey wid
                        isOld = IM.member k oldRects
                    mRect <- getNonzeroRect na i
                    case mRect of
                      Nothing ->
                        go (i + 1) (if isOld then IM.delete k m else m) (dropKey k cm) (dropKey k tm) (dropKey k lm) foundOld (dropped || isOld)
                      Just r -> do
                        mClip <- getClipBounds na i
                        nt <- getNodeType na i
                        -- Text nodes, and images, whose text is their image
                        -- id: switching an image repaints it like new text,
                        -- and so does changing how it is drawn.
                        tm' <-
                          if nt == NodeText || nt == NodeImage
                            then (\txt -> putNew k txt tm) <$!> getText na i
                            else pure $! dropKey k tm
                        lm' <-
                          if nt == NodeImage
                            then maybe (dropKey k lm) (\n -> putNew k (inLook n) lm) <$!> getImageNode na i
                            else pure $! dropKey k lm
                        go (i + 1) (putNew k r m) (maybe (dropKey k cm) (\c -> putNew k c cm) mClip) tm' lm' (foundOld + if isOld then 1 else 0) dropped
      go 0 m0 oldClips oldTexts oldLooks foundContainers droppedContainer

-- | @m@ with @k@ mapped to @v@: @m@ itself when it already is.
{-# INLINE putNew #-}
putNew :: Eq a => Int -> a -> IM.IntMap a -> IM.IntMap a
putNew k v m = if IM.lookup k m == Just v then m else IM.insert k v m

-- | @m@ without @k@: @m@ itself when it has no @k@.
{-# INLINE dropKey #-}
dropKey :: Int -> IM.IntMap a -> IM.IntMap a
dropKey k m = if IM.member k m then IM.delete k m else m

-- | Floating panels in the order the frame paints them, bottom first: every
-- window, then every modal, then every popup, each kind in arena order. The
-- pointer's layer is the last of these that holds it.
floatingPanelsInOrder :: Context -> IO [(Int, Rect)]
floatingPanelsInOrder Context {ctxNodeArena = na} = do
  tagged <- foldClassNodeRevM na FloatingNodes step []
  pure [p | r <- [0, 1, 2 :: Int], (r', p) <- tagged, r' == r]
  where
    rank nt = case nt of
      NodeWindow -> 0
      NodeModal -> 1
      _ -> 2
    step acc idx = do
      wid <- getWidgetId na idx
      if hashWidgetId wid == 0
        then pure acc
        else do
          nt <- getNodeType na idx
          rect <- getNodeRect na idx
          pure ((rank nt, (intKey wid, rect)) : acc)

-- | Current floating-panel bounds keyed by widget id, in logical window coordinates.
floatingPanelRects :: Context -> IO (IM.IntMap Rect)
floatingPanelRects ctx = IM.fromList <$> floatingPanelsInOrder ctx

-- | State 'NanoUI.Internal.Frame' captures before the UI pass; 'writeDamage' compares
-- it against the finished frame.
data FrameSnapshot = FrameSnapshot
  { fsOpaqueFollow :: !Bool
  -- ^ The previous frame asked for this one through 'markDirty' (a model
  -- change), not only 'markDirtyCovered' (store writes, which damaged their
  -- effects per key). Such a frame repaints all, since the change it follows
  -- may have altered any pixel.
  , fsSize :: !Size
  , fsStore :: !WidgetStore
  , fsHot :: !WidgetId
  , fsActive :: !WidgetId
  , fsFocus :: !WidgetId
  , fsFloatingRects :: !(IM.IntMap Rect)
  , fsPrev :: !PrevFrame
  , fsAnimKeys :: !IS.IntSet
  }

-- | Read the 'FrameSnapshot' before the UI pass, ahead of 'clearDirty'
-- (which resets 'dsDirtyOpaque').
captureFrameSnapshot :: Context -> IO FrameSnapshot
captureFrameSnapshot ctx = do
  ds <- readIORef (ctxDamageState ctx)
  evaluate
    =<< FrameSnapshot (dsDirtyOpaque ds) (dsLastWindowSize ds)
      <$> getStore ctx
      <*> readIORef (ctxLastHotId ctx)
      <*> readIORef (ctxActiveId ctx)
      <*> readIORef (ctxFocusId ctx)
      <*> getsOverlay ctx osPrevFloatingRects
      <*> pure (dsPrev ds)
      <*> (IM.keysSet <$> getLiveAnimations ctx)

-- | What the finished frame looks like and what changed since the snapshot.
-- Derived fields stay lazy: a frame that is already 'DamageFull' for a cheap
-- reason never pays for them.
data FrameDelta = FrameDelta
  { fdWinSize :: !Size
  , fdStore :: !WidgetStore
  , fdPrev :: !PrevFrame
  -- ^ This frame's 'dsPrev'.
  , fdFloatingRects :: !(IM.IntMap Rect)
  , fdModalFlip :: !Bool
  , fdLiveAnims :: !(IM.IntMap Animation)
  , fdWindowLive :: !Bool
  , fdRequests :: ![DamageRequest]
  , fdAnimLive :: Bool
  , fdFloatingChanged :: Bool
  , fdScrollChanged :: Bool
  , fdPointsChanged :: Bool
  , fdScrollOnly :: Bool
  -- ^ Only 'storeFloat' (and the 'storeQuiet' bookkeeping) changed in the
  -- store, e.g. a floating pane scrolled.
  , fdSettledMoved :: !RectGroup
  -- ^ Changed key rects, clipped to their scroll viewports, that cover some
  -- area.
  , fdChurn :: !RectGroup
  -- ^ Rects of keys that left or joined the arena.
  , fdRedrawn :: ![Int]
  -- ^ Keys of drawings whose ops changed at an unchanged rect.
  }

writeDamage :: Context -> Input -> FrameSnapshot -> IO ()
writeDamage ctx inp snap = do
  newStore <- getStore ctx
  panels <- floatingPanelsInOrder ctx
  new <- getsDamage ctx dsPrev
  modalFlip <- modalDamageFlip ctx
  liveAnims <- getLiveAnimations ctx
  settled <- takeAnimSettled ctx
  windowLive <- getsInteraction ctx (\s -> isJust (isWindowDrag s) || isJust (isWindowResize s))
  requests <- getsDamage ctx dsRequests
  redrawn <- refreshCustomDrawings ctx
  let oldStore = fsStore snap
      newFloatingRects = IM.fromList panels
  (settledMoved, churn) <- rectDeltas (map snd panels) (fsPrev snap) new
  let scrollChanged = not (eqByPtr (storeFloat oldStore) (storeFloat newStore))
      pointsChanged = not (eqByPtr (storePoint oldStore) (storePoint newStore))
  let delta =
        FrameDelta
          { fdWinSize = inputWindowSize inp
          , fdStore = newStore
          , fdPrev = new
          , fdFloatingRects = newFloatingRects
          , fdModalFlip = modalFlip
          , fdLiveAnims = liveAnims
          , fdWindowLive = windowLive
          , fdRequests = requests
          , fdAnimLive = not (IM.null liveAnims) || settled
          , fdFloatingChanged = fsFloatingRects snap /= newFloatingRects
          , fdScrollChanged = scrollChanged
          , fdPointsChanged = pointsChanged
          , fdScrollOnly =
              scrollChanged
                && not pointsChanged
                && storeMirrorGen oldStore == storeMirrorGen newStore
                && storeOpenSelect oldStore == storeOpenSelect newStore
                && null (slotChangedKeys oldStore newStore)
          , fdSettledMoved = settledMoved
          , fdChurn = churn
          , fdRedrawn = redrawn
          }
  -- The store diff runs only once the cheap checks would clip: a frame
  -- already repainting whole needs no per-key damage.
  (dmg, pieces) <-
    if needsFullDamage snap delta
      then pure (DamageFull, [])
      else do
        (misses, owners) <- storeKeyChanges ctx oldStore newStore
        -- A changed key that resolves to no node (a local hook's key, or a
        -- widget-internal key nothing in the arena carries) may have changed
        -- paint state no diff describes, so nothing narrower than the window
        -- is known to cover it. Key 0 names no widget and is damaged by
        -- nothing, so its writes cannot escalate a frame.
        if any (\k -> k /= 0 && IM.notMember k owners) misses
          then pure (DamageFull, [])
          else clipDamage ctx snap delta owners
  modifyDamage ctx $ \ds ->
    ds
      { dsDamage = dmg
      , dsDamagePieces = pieces
      , dsLastWindowSize = inputWindowSize inp
      , dsRequests = []
      }
  -- Most frames have no panel now or before, and then there is nothing to write.
  prevEmpty <- getsOverlay ctx (IM.null . osPrevFloatingRects)
  unless (null panels && prevEmpty) $
    modifyOverlay ctx $ \os ->
      os {osPrevFloatingRects = newFloatingRects, osPrevFloatingOrder = map fst panels}
  -- The follow-up frames these request only settle placement: the flip and
  -- floating change each made this frame repaint whole already, so their
  -- pixels are not waiting on the next frame.
  when modalFlip (markDirtyCovered ctx)
  when (fdFloatingChanged delta && not (IM.null (fsFloatingRects snap) && not (IM.null newFloatingRects))) $
    markDirtyCovered ctx

-- | Settle every drawing's ops for this frame and return the keys of those
-- that now draw something else at an unchanged rect. A drawing follows state
-- the arena does not hold, so nothing else damages it, and paint must not
-- replay the previous frame's ops for it. What this costs per widget is the
-- widget's own choice: see 'refreshCustomDrawingOps'.
refreshCustomDrawings :: Context -> IO [Int]
refreshCustomDrawings ctx@Context {ctxNodeArena = na} = do
  dc <- readIORef (ctxDrawingCache ctx)
  -- A drawing with neither entry settles nothing, so a view without drawings
  -- skips the walk, and the walk visits only the drawing nodes.
  if IM.null (dcsDrawings dc) && IM.null (dcsCustomDrawings dc)
    then pure []
    else foldClassNodesM na DrawingNodes step []
  where
    step acc i = do
      wid <- getWidgetId na i
      rect <- getNodeRect na i
      mCustom <- lookupCustomDrawing ctx wid
      changed <- case mCustom of
        Just (CustomDrawingEntry content build _ _ _) -> do
          cdc <- mkCustomDrawContext ctx (ctxFontMetrics ctx) wid
          refreshCustomDrawingOps ctx wid content rect cdc build
        Nothing -> do
          -- A versioned drawing rebuilds in paint once its version
          -- changes, but the pixels it covered still need damage. An
          -- unversioned one is cached by contract, so it stays put.
          mDrawing <- lookupDrawing ctx wid
          case mDrawing of
            Just (DrawingEntry content _) | content /= 0 -> drawingOpsStale ctx wid content rect
            _ -> pure False
      pure (if changed then intKey wid : acc else acc)

-- | Whether the frame repaints the whole window rather than a clip.
--
-- Store writes are repainted per key instead ('storeKeyChanges',
-- 'storeKeyDamage'); 'writeDamage' escalates a frame whose changed keys do not
-- all resolve to a node after this check. A follow-up frame requested by a
-- model change ('fsOpaqueFollow') repaints whole: the change may alter paint
-- state no diff describes (a box's colour, a node value), so a non-empty rect
-- or text diff does not show it is covered.
needsFullDamage :: FrameSnapshot -> FrameDelta -> Bool
needsFullDamage snap d =
  ReqFull `elem` fdRequests d
    || neverPainted
    -- Even when the frame's only store change is in 'storeFloat', which also
    -- holds slider and knob values, not just scroll offsets.
    || fsOpaqueFollow snap
    || not (fdScrollOnly d)
      && ( -- A local hook wrote state ('bumpMirror'). Its key names no
           -- widget, and a float hook's key is not even in the store diff,
           -- so nothing narrower than the window is known to cover it.
           mirrorStoresChanged (fsStore snap) (fdStore d)
             || sizeChanged
             || fdModalFlip d
             || fdFloatingChanged d
             || fdWindowLive d
             || (orphanAnim && fdAnimLive d)
             || keysChanged
             || layoutSettle
         )
    || (missingAnim && fdAnimLive d)
  where
    oldRects = pfRects (fsPrev snap)
    newRects = pfRects (fdPrev d)
    oldSize = fsSize snap
    -- Before any frame there is nothing to diff against, and the window
    -- backdrop outside every widget rect was never painted.
    neverPainted = oldSize == Size 0 0
    sizeChanged = oldSize /= fdWinSize d
    -- A live animation repaints whole on its first frame without a rect: its
    -- widget just left, or it has none and started this frame. After that it
    -- is clipped like the rest, so a perpetual animation whose widget has
    -- left the arena (@keepAnimating@ behind a tab switch) stops repainting
    -- the window.
    firstRectless k =
      IM.notMember k newRects && (IM.member k oldRects || IS.notMember k (fsAnimKeys snap))
    orphanAnim = any firstRectless (IM.keys (fdLiveAnims d))
    -- One with no rect last frame either is not clipped even when only a
    -- scroller moved: the retain texture may never have shown it.
    missingAnim =
      any (\k -> k /= 0 && IM.notMember k oldRects && firstRectless k) (IM.keys (fdLiveAnims d))
    keysChanged =
      not (IM.null oldRects)
        && rgAny (fdChurn d)
        && not (rgInPanels (fdChurn d))
    layoutSettle =
      not (IM.null oldRects)
        && rgAny (fdSettledMoved d)
        && not (fdAnimLive d)
        && not (fdScrollChanged d)
        && not (rgInPanels (fdSettledMoved d))

-- | The clip covering everything that changed, with the pieces it splits
-- into ('damagePieces'), or 'DamageFull' once the pieces cover over half the
-- window.
clipDamage ::
  Context -> FrameSnapshot -> FrameDelta -> IM.IntMap NodeIdx -> IO (Damage, [Rect])
clipDamage ctx snap d owners = do
  let old = fsPrev snap
      new = fdPrev d
      Size winW winH = fdWinSize d
      rectIn p wid = IM.lookup (intKey wid) (pfRects p)
  acc <- newIORef []
  -- Key @k@'s old and new rects, each grown by @grow@ and clipped in its own
  -- frame ('addKeyDamage').
  let damageKeyBy k grow = do
        forM_ (IM.lookup k (pfRects old)) $ \r -> addKeyDamage acc old k r (grow r)
        forM_ (IM.lookup k (pfRects new)) $ \r -> addKeyDamage acc new k r (grow r)
      resolveKey k = damageKeyBy k . resolveDamageRect
      resolveSlop k = resolveKey k (DamageInflated defaultDamageSlop)
  forM_ (fdRequests d) $ \case
    ReqFull -> pure ()
    ReqRect r -> addRect acc r
    ReqWidget wid bounds -> resolveKey (intKey wid) bounds
    ReqKey k bounds -> resolveKey k bounds
    ReqPeers wids bounds -> forM_ wids $ \wid -> resolveKey (intKey wid) bounds
  -- Backdrop expansion covers interaction slop (hover/press halos) and
  -- explicit damage requests. Animation keys must not expand to their panel
  -- backdrop: an animated widget inside a large panel would damage the whole
  -- panel every frame, and once that union crosses half the window the frame
  -- degrades to DamageFull. The scissored replay redraws the backdrop fill
  -- inside the anim's own rect+slop, so no stale pixels remain.
  let addNodeBackdrop =
        maybe (pure Nothing) (backdropRectFromNode ctx)
          >=> mapM_ (addRect acc . clipRectToWindow winW winH)
      addBackdrop k = unless (k == 0) $ addNodeBackdrop =<< lookupNodeByKey (ctxNodeArena ctx) k
      -- A widget that held a role last frame repaints its old rect too.
      hadRole wid = wid == fsHot snap || wid == fsActive snap || wid == fsFocus snap
      addInteraction wid = unless (k == 0) $ do
        slop <- fromMaybe defaultDamageSlop <$> lookupCustomDamageSlop ctx wid
        let side p mr = forM_ mr $ \r -> addKeyDamage acc p k r (rectInflate slop r)
        side old (if hadRole wid then rectIn old wid else Nothing)
        side new (rectIn new wid)
        addNodeBackdrop =<< lookupNodeByKey (ctxNodeArena ctx) k
        where
          k = intKey wid
      -- A parked pointer must not re-damage its hot widget every frame: only
      -- an id change (hover in/out, press, focus move) or a rect move
      -- repaints. Unchanged interaction rects kept the steady state at
      -- DamageFull whenever the hot widget sat inside a panel whose backdrop
      -- covered over half the window.
      role oldW newW = when (rectIn old oldW /= rectIn new newW) $ addInteraction oldW >> addInteraction newW
  role (fsHot snap) =<< getHotId ctx
  role (fsActive snap) =<< readIORef (ctxActiveId ctx)
  role (fsFocus snap) =<< readIORef (ctxFocusId ctx)
  forM_ (fdRequests d) $ \case
    ReqKey k _ -> addBackdrop k
    _ -> pure ()
  when (fdScrollChanged d || fdPointsChanged d) $
    scrollOffsetDamage ctx acc (fsStore snap) (fdStore d)
  -- The changed store keys that are not widget keys, through the widgets
  -- owning them ('storeKeyOwners'): each owner once, as a 'ReqWidget' with the
  -- standard slop would.
  forM_ (IS.toList (IS.fromList (IM.elems owners))) $ \idx ->
    resolveSlop . intKey =<< getWidgetId (ctxNodeArena ctx) idx
  let addAnim k = unless (k == 0) $ resolveSlop k
  IS.foldr (\k rest -> addAnim k >> rest) (pure ()) (fsAnimKeys snap)
  IM.foldrWithKey
    (\k _ rest -> unless (IS.member k (fsAnimKeys snap)) (addAnim k) >> rest)
    (pure ())
    (fdLiveAnims d)
  -- Same-key text changes that keep the rect (monospace counters, refreshed
  -- readouts) still repaint: rect-delta damage alone would leave them stale.
  -- New text keys inside floating panels also land here; outside panels the
  -- keysChanged predicate already forces full damage. updatePrevRects keeps
  -- last frame's map when no text changed.
  let addText k = mapM_ (addRect acc) (IM.lookup k (pfRects new))
  unless (ptrEq (pfTexts new) (pfTexts old)) $
    IM.foldrWithKey (\k _ rest -> addText k >> rest) (pure ()) $
      IM.differenceWith
        (\n o -> if n /= o then Just n else Nothing)
        (pfTexts new)
        (pfTexts old)
  -- An image drawn another way at the same rect, or no longer drawn its
  -- own way, repaints as a text change does.
  unless (ptrEq (pfLooks new) (pfLooks old)) $
    forM_ (IM.keys (IM.union (pfLooks new) (pfLooks old))) $ \k ->
      unless (IM.lookup k (pfLooks new) == IM.lookup k (pfLooks old)) (addText k)
  -- Drawings redrawn in place repaint their own rects, like a text change
  -- that keeps its rect.
  forM_ (fdRedrawn d) $ \k ->
    forM_ (IM.lookup k (pfRects new)) $ \r -> addKeyDamage acc new k r r
  unless (fdScrollOnly d) $ addGroup acc (fdSettledMoved d)
  -- Keys that left repaint as the current backdrop over their old rects. Keys
  -- that arrived must repaint inside their new rects too: the retain texture
  -- has never shown that content, and nothing else covers it (mirror writes
  -- escalate these frames to DamageFull, but layout-driven churn inside
  -- floating panels does not).
  addGroup acc (fdChurn d)
  -- Floating panels that moved, opened or closed repaint where they were and
  -- where they are.
  let addFloating other k r rest = unless (IM.lookup k other == Just r) (addRect acc r) >> rest
  IM.foldrWithKey (addFloating (fdFloatingRects d)) (pure ()) (fsFloatingRects snap)
  IM.foldrWithKey (addFloating (fsFloatingRects snap)) (pure ()) (fdFloatingRects d)
  added <- readIORef acc
  let clip = clipRectToWindow winW winH (rectBounds added)
      winArea = winW * winH
      -- A frame with more rects than this repaints their bounds.
      pieces
        | null (drop 64 added) = damagePieces (map (clipRectToWindow winW winH) added)
        | otherwise = []
      area = if null pieces then rectArea clip else sum (map rectArea pieces)
  -- A live animation with an empty clip is not DamageFull: its
  -- key was either scroll-clipped out of view (nothing visible
  -- changes; scrolling back in damages via the scroll delta) or
  -- rect-less, which missingAnim already promoted to full.
  pure $
    if winArea > 0 && area > winArea * 0.5
      then (DamageFull, [])
      else (DamageClip clip, pieces)

-- | Most rects a frame's damage splits into.
maxDamagePieces :: Int
maxDamagePieces = 4

-- | Rects closer than this, in logical pixels, merge into one piece. It is
-- more than the backdrop's inflation on both sides, so pieces stay disjoint
-- once each is painted a pixel past its edge.
pieceMergeGap :: Float
pieceMergeGap = 16

-- | Split damage into at most 'maxDamagePieces' disjoint rects: rects that
-- overlap or lie within 'pieceMergeGap' of each other merge, and then the
-- pair whose union grows least merges until few enough remain. Returns no
-- pieces when one or none remain, or when they cover most of their bounding
-- box, since painting the box costs about the same.
damagePieces :: [Rect] -> [Rect]
damagePieces rects =
  case shrink (settle (filter rectNonEmpty rects)) of
    ps@(_ : _ : _)
      | sum (map rectArea ps) < 0.7 * rectArea (foldr1 rectUnion ps) -> ps
    _ -> []
  where
    near a b = isJust (rectIntersect (rectInflate pieceMergeGap a) b)
    -- Merge the first near pair until no pair is near.
    settle ps = maybe ps settle (mergeNear ps)
    mergeNear [] = Nothing
    mergeNear (p : ps) = case break (near p) ps of
      (before, q : after) -> Just (rectUnion p q : before ++ after)
      (_, []) -> (p :) <$> mergeNear ps
    shrink ps
      | length ps <= maxDamagePieces = ps
      | otherwise =
          let indexed = zip [0 :: Int ..] ps
              cost a b = rectArea (rectUnion a b) - rectArea a - rectArea b
              (_, i, j) = minimum [(cost a b, i', j') | ((i', a) : later) <- tails indexed, (j', b) <- later]
              (pair, rest) = partition (\(k, _) -> k == i || k == j) indexed
           in shrink (settle (foldr1 rectUnion (map snd pair) : map snd rest))

-- | Damage @d@ around key @k@'s rect @r@ in frame @p@, clipped to the clip
-- the rect is painted in there ('ownClip'), and kept inside @r@ when @k@ is a
-- scroll container or panel (it has an entry in 'pfOuterClips'): the slop
-- around a widget's rect is for its halo, which those have none of, and the
-- clip around them would let it spill.
addKeyDamage :: RectUnion -> PrevFrame -> Int -> Rect -> Rect -> IO ()
addKeyDamage acc p k r d =
  when (rectNonEmpty clipped) $ addRect acc clipped
  where
    clipped = clipToViewport (ownClip p k) (if IM.member k (pfOuterClips p) then clipToViewport (Just r) d else d)

-- | The rects damage gathers, newest first.
type RectUnion = IORef [Rect]

{-# INLINE addRect #-}
addRect :: RectUnion -> Rect -> IO ()
addRect acc r = modifyIORef' acc (r :)

-- | Edges of a running bounds: @x0, y0, x1, y1@.
data Edges = Edges !Float !Float !Float !Float

-- | The bounds of @rects@, empty ones included, or the zero rect for none.
rectBounds :: [Rect] -> Rect
rectBounds rects
  | x0 > x1 = Rect 0 0 0 0
  | otherwise = Rect x0 y0 (x1 - x0) (y1 - y0)
  where
    grow (Edges a b c d) (Rect x y w h) = Edges (min a x) (min b y) (max c (x + w)) (max d (y + h))
    Edges x0 y0 x1 y1 = foldl' grow (Edges (1 / 0) (1 / 0) (-1 / 0) (-1 / 0)) rects

-- | A set of rects reduced to what damage needs from it.
data RectGroup = RectGroup
  { rgAny :: !Bool
  , rgInPanels :: !Bool
  -- ^ Some floating panel fully contains each rect; False for an empty group.
  , rgBounds :: !Rect
  }

addGroup :: RectUnion -> RectGroup -> IO ()
addGroup acc g = when (rgAny g) $ addRect acc (rgBounds g)

-- | One pass over the keys whose rect changed, reduced to the settled moves
-- (each side clipped to the scroll viewport it had in its own frame, above
-- 'layoutSettleMinArea') and the keys that left or joined the arena.
rectDeltas :: [Rect] -> PrevFrame -> PrevFrame -> IO (RectGroup, RectGroup)
rectDeltas panelRects oldP newP
  | ptrEq old new = pure (emptyGroup, emptyGroup)
  | otherwise = do
      settled <- newIORef []
      churn <- newIORef []
      IM.foldrWithKey
        ( \k r rest -> do
            when (rectNonEmpty r) $ do
              when (IM.notMember k new || IM.notMember k old) $ addRect churn r
              let oldClip = ownClip oldP k
                  newClip = ownClip newP k
                  side clip = maybe (Rect 0 0 0 0) (clipToViewport clip)
                  -- Most moves keep their viewport (a list scrolling), and
                  -- clipping the union once covers both sides.
                  clipped
                    | oldClip == newClip = clipToViewport newClip r
                    | otherwise = unionNonEmpty (side oldClip (IM.lookup k old)) (side newClip (IM.lookup k new))
              when (rectArea clipped >= layoutSettleMinArea) $ addRect settled clipped
            rest
        )
        (pure ())
        (IM.mergeWithKey (\_ a b -> if a /= b then Just (rectUnion a b) else Nothing) id id old new)
      (,) <$> (group <$> readIORef settled) <*> (group <$> readIORef churn)
  where
    old = pfRects oldP
    new = pfRects newP
    emptyGroup = RectGroup False False (Rect 0 0 0 0)
    unionNonEmpty a b
      | not (rectNonEmpty a) = b
      | not (rectNonEmpty b) = a
      | otherwise = rectUnion a b
    inPanels r = any (rectFullyInside r) panelRects
    group rs = RectGroup (not (null rs)) (not (null rs || null panelRects) && all inPanels rs) (rectBounds rs)

-- | The clip key @k@'s own rect is painted in, in frame @p@: for a scroll
-- container or a panel the one around it, and otherwise its viewport clip.
-- Each rect is clipped in its own frame: the viewport a key has now can be
-- smaller than the one it had (a widget or scroller around it shrank with
-- it), and would cut the pixels it vacated out of the damage.
{-# INLINE ownClip #-}
ownClip :: PrevFrame -> Int -> Maybe Rect
ownClip p k = IM.lookup k (pfOuterClips p) <|> IM.lookup k (pfClips p)

clipToViewport :: Maybe Rect -> Rect -> Rect
clipToViewport clip r = maybe r (fromMaybe (Rect 0 0 0 0) . rectIntersect r) clip

clipRectToWindow :: Float -> Float -> Rect -> Rect
clipRectToWindow winW winH = clipToViewport (Just (Rect 0 0 winW winH))

scrollOffsetDamage :: Context -> RectUnion -> WidgetStore -> WidgetStore -> IO ()
scrollOffsetDamage Context {ctxNodeArena = na} acc oldStore newStore =
  unless (null changedKeys) $ do
    -- Every store key that holds a scroll node's offset, mapped to the first
    -- such node, and every scroll range, mapped to each node with that id.
    -- Built once, only on frames where an offset or range changed.
    owners <- foldClassNodeRevM na PointerNodes addOwner IM.empty
    forM_ changedKeys $ \k ->
      forM_ (IM.findWithDefault [] k owners) $ \idx -> do
        -- The scroll node's rect covers the content viewport AND the
        -- scrollbar lane: offset changes move the thumb, which paints
        -- outside the content clip.
        getNonzeroRect na idx >>= mapM_ (addRect acc)
        walkFloatingAncestors na idx (\i _ -> getNonzeroRect na i) >>= mapM_ (addRect acc)
  where
    -- Floating-pane offsets live in storeFloat; wheel/keyboard offsets
    -- live under the SlotTextAreaScroll slot in storePoint. Both move the
    -- scroller's content and its chrome. New or removed float offsets only
    -- count when nonzero. The range the scroll pass publishes to storePoint
    -- changes with the content's size, which resizes the thumb and can show
    -- or hide a bar; content that changes but still fits keeps a zero range
    -- and repaints nothing here. Two scrollers can share an id (a table's
    -- frozen pane and body), and only the first publishes, so a range change
    -- repaints every scroller with the id.
    (oldF, newF) = (storeFloat oldStore, storeFloat newStore)
    changedKeys =
      filter (\k -> IM.findWithDefault 0 k oldF /= IM.findWithDefault 0 k newF) (diffKeys oldF newF)
        ++ diffKeys (storePoint oldStore) (storePoint newStore)
    addOwner m idx = do
      nt <- getNodeType na idx
      if not (isScrollNode nt)
        then pure m
        else do
          wid <- getWidgetId na idx
          let widKey = intKey wid
              one = [idx]
          pure $
            IM.insert widKey one $
              IM.insert (slotKey SlotScrollCross widKey) one $
                IM.insert (slotKey SlotTextAreaScroll widKey) one $
                  IM.insertWith (++) (slotKey SlotScrollRange widKey) one m

-- | The store keys (outside the scroll offsets) whose value changed and that
-- are not themselves widget keys, with the node each owns through a sub-slot
-- spelling. Changed widget keys were already damaged by their 'ReqKey'
-- requests. The owners fold runs only when some key needs it.
storeKeyChanges :: Context -> WidgetStore -> WidgetStore -> IO ([Int], IM.IntMap NodeIdx)
storeKeyChanges ctx oldStore newStore = do
  misses <-
    filterM (\k -> isNothing <$> lookupNodeByKey (ctxNodeArena ctx) k) (slotChangedKeys oldStore newStore)
  owners <-
    if null misses then pure IM.empty else storeKeyOwners (ctxNodeArena ctx) (IS.fromList misses)
  pure (misses, owners)

-- | Each of @wanted@ that maps to a node this frame, keyed to the node that
-- owns it: each widget's base key plus the sub-slot spellings its reads and
-- writes use. Built once per frame, only when a changed key is not itself a
-- widget key.
storeKeyOwners :: NodeArena -> IS.IntSet -> IO (IM.IntMap NodeIdx)
storeKeyOwners na wanted = do
  n <- arenaCount na
  -- Stops once every wanted key has an owner.
  let target = IS.size wanted
      go !i !found !m
        | i < 0 || found == target = pure m
        | otherwise = do
            wid <- getWidgetId na i
            if hashWidgetId wid == 0
              then go (i - 1) found m
              else do
                let !k = intKey wid
                    add (!c, !m') sk
                      | IS.member sk wanted && IM.notMember sk m' = (c + 1, IM.insert sk i m')
                      | otherwise = (c, m')
                    -- The widget's own key first, then each sub-slot spelling.
                    (found', m'') =
                      foldl' (\acc s -> add acc (slotKey s k)) (add (found, m) k) ownerSlots
                go (i - 1) found' m''
  go (n - 1) (0 :: Int) IM.empty

-- | The sub-slots whose writes repaint from the store at paint time: carets and
-- anchors in text fields, a text area's buffer (its document and caret) and
-- history, drag targets, a colour picker's opening colour, and the seen/mode
-- records a controlled edit co-writes. Shared, so resolving an owner does not
-- build a key list per node per frame.
ownerSlots :: [Slot]
ownerSlots =
  [ SlotSeen
  , SlotCursor
  , SlotAnchor
  , SlotTextMode
  , SlotTextHistory
  , SlotSearchCommitted
  , SlotSearchAge
  , SlotTextAreaAnchorRow
  , SlotTextAreaAnchorCol
  , SlotTextAreaChanged
  , SlotTextAreaText
  , SlotTextAreaBuffer
  , SlotDrop
  , SlotNumericHeld
  , SlotNumericRepeat
  , SlotMenuOpen
  , SlotColorBase
  ]
