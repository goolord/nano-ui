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

import Control.Exception (evaluate)
import Control.Monad (filterM, forM_, unless, when, (>=>))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.List (partition, tails)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
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
  paints <- case nt of
    NodeScrollContainer -> do
      wTag <- axTag <$> getWidthSizing na i
      hTag <- axTag <$> getHeightSizing na i
      si <- getStyleIdx na i
      pure (not ((wTag == SizingGrow && hTag == SizingGrow) || scrollBare (decodeScrollConfig si)))
    _ -> pure (nt == NodePanel || isFloatingNode nt)
  if paints then getNonzeroRect na i else pure Nothing

{-# INLINE getNonzeroRect #-}
getNonzeroRect :: NodeArena -> Int -> IO (Maybe Rect)
getNonzeroRect arena i = do
  r <- getNodeRect arena i
  pure (if rectNonEmpty r then Just r else Nothing)

updatePrevRects :: Context -> IO ()
updatePrevRects ctx@Context {ctxNodeArena = na} = do
  oldRects <- getsDamage ctx dsPrevRects
  oldClips <- getsDamage ctx dsPrevClips
  oldTexts <- getsDamage ctx dsPrevNodeTexts
  count <- arenaCount na
  if count <= 0
    then modifyDamage ctx (\ds -> ds {dsPrevRects = IM.empty, dsPrevClips = IM.empty, dsPrevNodeTexts = IM.empty})
    else do
      -- Walk the arena from base maps, touching only entries whose value
      -- changed. Seeded with last frame's maps, frames with stable rects
      -- (hover, text churn, animations) allocate nothing. The walk cannot
      -- delete keys that vanished from the arena, so when the key set changed
      -- it reruns from empty maps, where no key counts as old.
      let go olds !i !m !cm !tm !foundOld !dropped
            | i >= count =
                if dropped || foundOld /= IM.size olds
                  then go IM.empty 0 IM.empty IM.empty IM.empty 0 False
                  else modifyDamage ctx (\ds -> ds {dsPrevRects = m, dsPrevClips = cm, dsPrevNodeTexts = tm})
            | otherwise = do
                wid <- getWidgetId na i
                if hashWidgetId wid == 0
                  then go olds (i + 1) m cm tm foundOld dropped
                  else do
                    let !k = intKey wid
                        isOld = IM.member k olds
                    mRect <- getNonzeroRect na i
                    case mRect of
                      Nothing ->
                        let dropped' = dropped || isOld
                            m' = if isOld then IM.delete k m else m
                            cm' = if IM.member k cm then IM.delete k cm else cm
                            tm' = if IM.member k tm then IM.delete k tm else tm
                         in go olds (i + 1) m' cm' tm' foundOld dropped'
                      Just r -> do
                        mClip <- getClipRect na i
                        nt <- getNodeType na i
                        let !m' = if IM.lookup k m == Just r then m else IM.insert k r m
                            !cm' = case mClip of
                              Just c -> if IM.lookup k cm == Just c then cm else IM.insert k c cm
                              Nothing -> if IM.member k cm then IM.delete k cm else cm
                        -- Text nodes, and images, whose text is their image
                        -- id: switching an image repaints it like new text.
                        tm' <-
                          if nt == NodeText || nt == NodeImage
                            then do
                              txt <- getText na i
                              pure $! if IM.lookup k tm == Just txt then tm else IM.insert k txt tm
                            else pure $! if IM.member k tm then IM.delete k tm else tm
                        go olds (i + 1) m' cm' tm' (foundOld + if isOld then 1 else 0) dropped
      go oldRects 0 oldRects oldClips oldTexts 0 False

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
  , fsHotRect :: !(Maybe Rect)
  , fsActiveRect :: !(Maybe Rect)
  , fsFocusRect :: !(Maybe Rect)
  , fsFloatingRects :: !(IM.IntMap Rect)
  , fsRects :: !(IM.IntMap Rect)
  , fsClips :: !(IM.IntMap Rect)
  -- ^ Last frame's viewport clips ('getClipRect') of the keys in 'fsRects'.
  -- A rect from last frame is clipped by the viewport it was drawn in: the
  -- viewport a key has now can be smaller (the root shrank with it) and would
  -- cut the pixels it vacated out of the damage.
  , fsTexts :: !(IM.IntMap Text)
  , fsAnimKeys :: !IS.IntSet
  }

-- | Read the 'FrameSnapshot' before the UI pass, ahead of 'clearDirty'
-- (which resets 'dsDirtyOpaque').
captureFrameSnapshot :: Context -> IO FrameSnapshot
captureFrameSnapshot ctx = do
  hot <- readIORef (ctxLastHotId ctx)
  active <- readIORef (ctxActiveId ctx)
  focus <- readIORef (ctxFocusId ctx)
  evaluate
    =<< FrameSnapshot
      <$> getsDamage ctx dsDirtyOpaque
      <*> getsDamage ctx dsLastWindowSize
      <*> getStore ctx
      <*> pure hot
      <*> pure active
      <*> pure focus
      <*> getPrevRect ctx hot
      <*> getPrevRect ctx active
      <*> getPrevRect ctx focus
      <*> getsOverlay ctx osPrevFloatingRects
      <*> getsDamage ctx dsPrevRects
      <*> getsDamage ctx dsPrevClips
      <*> getsDamage ctx dsPrevNodeTexts
      <*> (IM.keysSet <$> getLiveAnimations ctx)

-- | What the finished frame looks like and what changed since the snapshot.
-- Derived fields stay lazy: a frame that is already 'DamageFull' for a cheap
-- reason never pays for them.
data FrameDelta = FrameDelta
  { fdWinSize :: !Size
  , fdStore :: !WidgetStore
  , fdRects :: !(IM.IntMap Rect)
  , fdTexts :: !(IM.IntMap Text)
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
  newRects <- getsDamage ctx dsPrevRects
  newTexts <- getsDamage ctx dsPrevNodeTexts
  modalFlip <- modalDamageFlip ctx
  liveAnims <- getLiveAnimations ctx
  settled <- takeAnimSettled ctx
  winDragActive <- isJust <$> getsInteraction ctx isWindowDrag
  winResizeActive <- isJust <$> getsInteraction ctx isWindowResize
  requests <- getsDamage ctx dsRequests
  redrawn <- refreshCustomDrawings ctx
  let oldRects = fsRects snap
      oldStore = fsStore snap
      newFloatingRects = IM.fromList panels
  (settledMoved, churn) <- rectDeltas ctx (map snd panels) (fsClips snap) oldRects newRects
  let scrollChanged = not (eqByPtr (storeFloat oldStore) (storeFloat newStore))
  let delta =
        FrameDelta
          { fdWinSize = inputWindowSize inp
          , fdStore = newStore
          , fdRects = newRects
          , fdTexts = newTexts
          , fdFloatingRects = newFloatingRects
          , fdModalFlip = modalFlip
          , fdLiveAnims = liveAnims
          , fdWindowLive = winDragActive || winResizeActive
          , fdRequests = requests
          , fdAnimLive = not (IM.null liveAnims) || settled
          , fdFloatingChanged = fsFloatingRects snap /= newFloatingRects
          , fdScrollChanged = scrollChanged
          , fdPointsChanged = not (eqByPtr (storePoint oldStore) (storePoint newStore))
          , fdScrollOnly =
              scrollChanged
                && eqByPtr (storePoint oldStore) (storePoint newStore)
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
    oldRects = fsRects snap
    newRects = fdRects d
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
  let oldRects = fsRects snap
      newRects = fdRects d
      Size winW winH = fdWinSize d
      oldOf wid
        | wid == fsHot snap = fsHotRect snap
        | wid == fsActive snap = fsActiveRect snap
        | wid == fsFocus snap = fsFocusRect snap
        | otherwise = Nothing
  acc <- newIORef []
  let resolveKey = resolveKeyDamage ctx acc (fsClips snap) oldRects newRects
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
      addInteraction wid = unless (k == 0) $ do
        node <- lookupNodeByKey (ctxNodeArena ctx) k
        newR <- getPrevRect ctx wid
        slop <- fromMaybe defaultDamageSlop <$> lookupCustomDamageSlop ctx wid
        clip <- maybe (pure Nothing) (getClipRect (ctxNodeArena ctx)) node
        let addSide c = mapM_ (mapM_ (addRect acc) . clipKeyRect k c . rectInflate slop)
        addSide (IM.lookup k (fsClips snap)) (oldOf wid)
        addSide clip newR
        addNodeBackdrop node
        where
          k = intKey wid
      -- A parked pointer must not re-damage its hot widget every frame: only
      -- an id change (hover in/out, press, focus move) or a rect move
      -- repaints. Unchanged interaction rects kept the steady state at
      -- DamageFull whenever the hot widget sat inside a panel whose backdrop
      -- covered over half the window.
      role oldW oldR newW = do
        newR <- getPrevRect ctx newW
        when (oldR /= newR) $ addInteraction oldW >> addInteraction newW
  role (fsHot snap) (fsHotRect snap) =<< getHotId ctx
  role (fsActive snap) (fsActiveRect snap) =<< readIORef (ctxActiveId ctx)
  role (fsFocus snap) (fsFocusRect snap) =<< readIORef (ctxFocusId ctx)
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
  let addText k = mapM_ (addRect acc) (IM.lookup k newRects)
  unless (ptrEq (fdTexts d) (fsTexts snap)) $
    IM.foldrWithKey (\k _ rest -> addText k >> rest) (pure ()) $
      IM.differenceWith
        (\new old -> if new /= old then Just new else Nothing)
        (fdTexts d)
        (fsTexts snap)
  -- Drawings redrawn in place repaint their own rects, like a text change
  -- that keeps its rect.
  forM_ (fdRedrawn d) $ \k ->
    forM_ (IM.lookup k newRects) $ \r -> do
      clip <- keyViewportClip ctx k
      mapM_ (addRect acc) (clipKeyRect k clip r)
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

-- | Damage key @k@'s old and new rects, resolved through @bounds@ and each
-- clipped to the key's viewport in its own frame: @oldClips@ for the old
-- rect, 'keyViewportClip' for the new one.
resolveKeyDamage ::
  Context -> RectUnion -> IM.IntMap Rect -> IM.IntMap Rect -> IM.IntMap Rect -> Int -> DamageBounds -> IO ()
resolveKeyDamage ctx acc oldClips oldRects newRects k bounds = do
  clip <- keyViewportClip ctx k
  forM_ [(IM.lookup k oldClips, IM.lookup k oldRects), (clip, IM.lookup k newRects)] $ \(c, mr) ->
    forM_ mr $ \r -> do
      let clipped = clipToViewport c (resolveDamageRect bounds r)
      when (rectNonEmpty clipped) $ addRect acc clipped

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
rectDeltas ::
  Context -> [Rect] -> IM.IntMap Rect -> IM.IntMap Rect -> IM.IntMap Rect -> IO (RectGroup, RectGroup)
rectDeltas ctx panelRects oldClips old new
  | ptrEq old new = pure (emptyGroup, emptyGroup)
  | otherwise = do
      settled <- newIORef []
      churn <- newIORef []
      IM.foldrWithKey
        ( \k r rest -> do
            when (rectNonEmpty r) $ do
              when (IM.notMember k new || IM.notMember k old) $ addRect churn r
              newClip <- keyViewportClip ctx k
              let side clip = maybe (Rect 0 0 0 0) (clipToViewport clip)
                  clipped =
                    unionNonEmpty
                      (side (IM.lookup k oldClips) (IM.lookup k old))
                      (side newClip (IM.lookup k new))
              when (rectArea clipped >= layoutSettleMinArea) $ addRect settled clipped
            rest
        )
        (pure ())
        (IM.mergeWithKey (\_ a b -> if a /= b then Just (rectUnion a b) else Nothing) id id old new)
      (,) <$> (group <$> readIORef settled) <*> (group <$> readIORef churn)
  where
    emptyGroup = RectGroup False False (Rect 0 0 0 0)
    unionNonEmpty a b
      | not (rectNonEmpty a) = b
      | not (rectNonEmpty b) = a
      | otherwise = rectUnion a b
    inPanels r = any (rectFullyInside r) panelRects
    group rs = RectGroup (not (null rs)) (not (null rs || null panelRects) && all inPanels rs) (rectBounds rs)

-- | The scroll-viewport clip of a keyed node. Look it up once per key and
-- clip each of its rects with 'clipToViewport'.
keyViewportClip :: Context -> Int -> IO (Maybe Rect)
keyViewportClip ctx k = lookupNodeByKey na k >>= maybe (pure Nothing) (getClipRect na)
  where
    na = ctxNodeArena ctx

clipToViewport :: Maybe Rect -> Rect -> Rect
clipToViewport clip r = maybe r (fromMaybe (Rect 0 0 0 0) . rectIntersect r) clip

clipRectToWindow :: Float -> Float -> Rect -> Rect
clipRectToWindow winW winH = clipToViewport (Just (Rect 0 0 winW winH))

-- | A keyed rect clipped to its viewport ('keyViewportClip'), or 'Nothing'
-- when nothing of it shows. Key 0 is not clipped.
clipKeyRect :: Int -> Maybe Rect -> Rect -> Maybe Rect
clipKeyRect k clip r
  | k == 0 = Just r
  | otherwise =
      let clipped = clipToViewport clip r
       in if rectNonEmpty clipped then Just clipped else Nothing

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
