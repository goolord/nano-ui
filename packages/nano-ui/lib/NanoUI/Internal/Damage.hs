-- | Compare completed frames and explicit invalidations to choose repaint bounds.
-- Damage is computed before painting so retained backends can redraw only a clip.
module NanoUI.Internal.Damage
  ( updatePrevRects
  , floatingPanelRects
  , FrameSnapshot (..)
  , writeDamage
  , damagePieces
  ) where

import Control.Monad (filterM, forM_, unless, when)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.List (partition, tails)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Primitive.PrimArray (MutablePrimArray, newPrimArray, readPrimArray, writePrimArray)
import Data.Text (Text)
import GHC.Exts (RealWorld)
import NanoUI.Internal.Context
  ( Animation
  , Context (..)
  , DamageRequest (..)
  , DrawingCacheState (..)
  , WidgetStore (..)
  , getHotId
  , getLiveAnimations
  , getAnimRest
  , pruneAnimRest
  , getAnimRectless
  , getPrevRect
  , getStore
  , getWindowDrag
  , getWindowResize
  , intKey
  , markDirtyCovered
  , modalDamageFlip
  , setAnimRectless
  , takeAnimSettled
  , lookupCustomDamageSlop
  , lookupCustomDrawing
  , lookupDrawing
  , refreshCustomDrawingOps
  , drawingOpsStale
  , CustomDrawingEntry (..)
  , DrawingEntry (..)
  , DamageState (..)
  , OverlayState (..)
  , getsDamage
  , modifyDamage
  , getsOverlay
  , modifyOverlay
  )
import NanoUI.Internal.Id (WidgetId (..), hashWidgetId)
import NanoUI.Internal.Input
  ( Input (..)
  , inputWindowSize
  )
import NanoUI.Internal.Frame.Hit (findNodeByKey)
import NanoUI.Internal.Store (Slot (..), eqByPtr, mirrorStoresChanged, ptrEq, slotChangedKeys, slotKey)
import NanoUI.Internal.Layout.Arena
  ( NodeArena
  , NodeIdx
  , NodeType (..)
  , SizingTag (..)
  , arenaCount
  , NodeClass (..)
  , foldClassNodesM
  , foldClassNodeRevM
  , foldFloatingNodeRevM
  , getClipRect
  , getHeightSizing
  , getNodeRect
  , getNodeType
  , getStyleIdx
  , getText
  , getWidgetId
  , getWidthSizing
  , isFloatingNode
  , isScrollNode
  , walkAncestors
  , walkFloatingAncestors
  )
import NanoUI.Internal.Frame.Scroll.Geometry (decodeScrollConfig, scrollBare)
import NanoUI.Internal.Widgets.Custom (mkCustomDrawContext)
import NanoUI.Internal.Types
  ( Damage (..)
  , DamageBounds (..)
  , Rect (..)
  , Size (..)
  , defaultDamageSlop
  , rectArea
  , rectFullyInside
  , rectInflate
  , rectIntersect
  , rectNonEmpty
  , rectUnion
  , resolveDamageRect
  )

layoutSettleMinArea :: Float
layoutSettleMinArea = 0.25

-- | Bound on how many consecutive rect-less frames a live animation may force a
-- full-window repaint. An animation whose widget is about to be laid out for the
-- first time gets a couple of frames of DamageFull cover; a perpetual animation
-- whose widget has left the arena (e.g. `keepAnimating` behind a tab switch)
-- must stop repainting the whole window once it is clearly gone.
orphanEscalateFrames :: Int
orphanEscalateFrames = 2

-- Partial retain clears with themeWindow. Expand interaction clips to the painted
-- panel/window backdrop so slop pixels get the correct fill, not window color.
backdropRectFromNode :: Context -> Int -> IO (Maybe Rect)
backdropRectFromNode ctx idx = walkAncestors (ctxNodeArena ctx) idx step
  where
    step i = do
      let na = ctxNodeArena ctx
      nt <- getNodeType na i
      if nt == NodePanel || isFloatingNode nt
        then getNonzeroRect na i
        else case nt of
          NodeScrollContainer -> do
            (wTag, _) <- getWidthSizing na i
            (hTag, _) <- getHeightSizing na i
            si <- getStyleIdx na i
            if (wTag == SizingGrow && hTag == SizingGrow) || scrollBare (decodeScrollConfig si)
              then pure Nothing
              else getNonzeroRect na i
          _ -> pure Nothing

{-# INLINE getNonzeroRect #-}
getNonzeroRect :: NodeArena -> Int -> IO (Maybe Rect)
getNonzeroRect arena i = do
  r <- getNodeRect arena i
  pure (if rectNonEmpty r then Just r else Nothing)

updatePrevRects :: Context -> IO ()
updatePrevRects ctx = do
  live <- getLiveAnimations ctx
  prevRectless <- getAnimRectless ctx
  oldRects <- getsDamage ctx dsPrevRects
  oldClips <- getsDamage ctx dsPrevClips
  oldTexts <- getsDamage ctx dsPrevNodeTexts
  let na = ctxNodeArena ctx
      bump rects = do
        rest <- getAnimRest ctx
        -- Frames without a rect, for the live or resting keys that have none.
        -- A key with a rect counts 0, which every reader takes as absent, so
        -- a frame where every animated widget has a rect builds nothing.
        let rectlessOf :: IM.IntMap v -> IM.IntMap Int
            rectlessOf m = IM.mapWithKey (\k _ -> IM.findWithDefault 0 k prevRectless + 1) (m `IM.difference` rects)
            restRectless = rectlessOf rest
            rectless' = rectlessOf live `IM.union` restRectless
            deadRest = IM.filter (> 300) restRectless
        unless (IM.null deadRest) $
          pruneAnimRest ctx (\k -> IM.notMember k deadRest)
        -- Every key is live or resting, so this drops exactly the dead resting
        -- keys that are not live again.
        unless (IM.null rectless' && IM.null prevRectless) $
          setAnimRectless ctx (rectless' `IM.difference` (deadRest `IM.difference` live))
  count <- arenaCount na
  if count <= 0
    then do
      modifyDamage ctx (\ds -> ds {dsPrevRects = IM.empty, dsPrevClips = IM.empty, dsPrevNodeTexts = IM.empty})
      bump IM.empty
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
                  else do
                    modifyDamage ctx (\ds -> ds {dsPrevRects = m, dsPrevClips = cm, dsPrevNodeTexts = tm})
                    bump m
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

floatingPanelsInOrder :: Context -> IO [(Int, Rect)]
floatingPanelsInOrder ctx = do
  foldFloatingNodeRevM na step []
  where
    na = ctxNodeArena ctx
    step acc idx = do
      nt <- getNodeType na idx
      if not (isFloatingNode nt)
        then pure acc
        else do
          wid <- getWidgetId na idx
          if hashWidgetId wid == 0
            then pure acc
            else do
              rect <- getNodeRect na idx
              pure ((intKey wid, rect) : acc)

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
  , fsTexts :: !(IM.IntMap Text)
  , fsAnimKeys :: !IS.IntSet
  }

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
  , fdRectless :: !(IM.IntMap Int)
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
  rectless <- getAnimRectless ctx
  winDragActive <- isJust <$> getWindowDrag ctx
  winResizeActive <- isJust <$> getWindowResize ctx
  requests <- getsDamage ctx dsRequests
  redrawn <- refreshCustomDrawings ctx
  let oldRects = fsRects snap
      oldStore = fsStore snap
      newFloatingRects = IM.fromList panels
  (settledMoved, churn) <- rectDeltas ctx (map snd panels) oldRects newRects
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
          , fdRectless = rectless
          , fdWindowLive = winDragActive || winResizeActive
          , fdRequests = requests
          , fdAnimLive = not (IM.null liveAnims) || settled
          , fdFloatingChanged = fsFloatingRects snap /= newFloatingRects
          , fdScrollChanged = scrollChanged
          , fdPointsChanged = not (eqByPtr (storePoint oldStore) (storePoint newStore))
          , fdScrollOnly =
              scrollChanged
                && oldStore == newStore {storeFloat = storeFloat oldStore, storeQuiet = storeQuiet oldStore}
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
refreshCustomDrawings ctx = do
  dc <- readIORef (ctxDrawingCache ctx)
  -- A drawing with neither entry settles nothing, so a view without drawings
  -- skips the walk, and the walk visits only the drawing nodes.
  if IM.null (dcsDrawings dc) && IM.null (dcsCustomDrawings dc)
    then pure []
    else foldClassNodesM na DrawingNodes step []
  where
    na = ctxNodeArena ctx
    step acc i = do
      wid <- getWidgetId na i
      rect <- getNodeRect na i
      mCustom <- lookupCustomDrawing ctx wid
      changed <- case mCustom of
        Just (CustomDrawingEntry content build) -> do
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
    || not (fdScrollOnly d)
      && ( fsOpaqueFollow snap
             -- A local hook wrote state ('bumpMirror'). Its key names no
             -- widget, and a float hook's key is not even in the store diff,
             -- so nothing narrower than the window is known to cover it.
             || mirrorStoresChanged (fsStore snap) (fdStore d)
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
    recentlyRectless k = IM.findWithDefault 0 k (fdRectless d) < orphanEscalateFrames
    orphanAnim =
      any (\k -> IM.notMember k newRects && recentlyRectless k) (IM.keys (fdLiveAnims d))
    -- A live animation whose key has no rect this frame or last is not
    -- clipped: the retain texture may never have shown it.
    missingAnim =
      any
        (\k -> k /= 0 && IM.notMember k oldRects && IM.notMember k newRects && recentlyRectless k)
        (IS.toList (fsAnimKeys snap <> IM.keysSet (fdLiveAnims d)))
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
  acc <- newPieceUnion
  resolveDamageRequests ctx acc oldRects newRects (fdRequests d)
  -- Backdrop expansion covers interaction slop (hover/press halos) and
  -- explicit damage requests. Animation keys must not expand to their panel
  -- backdrop: an animated widget inside a large panel would damage the whole
  -- panel every frame, and once that union crosses half the window the frame
  -- degrades to DamageFull. The scissored replay redraws the backdrop fill
  -- inside the anim's own rect+slop, so no stale pixels remain.
  let addBackdrop k =
        unless (k == 0) $
          findNodeByKey ctx k
            >>= maybe (pure Nothing) (backdropRectFromNode ctx)
            >>= mapM_ (addRect acc . clipRectToWindow winW winH)
      addInteraction wid = do
        when (hashWidgetId wid /= 0) $ do
          newR <- getPrevRect ctx wid
          slop <- fromMaybe defaultDamageSlop <$> lookupCustomDamageSlop ctx wid
          clip <- keyViewportClip ctx (intKey wid)
          let addSide = mapM_ (mapM_ (addRect acc) . clipKeyRect (intKey wid) clip . rectInflate slop)
          addSide (oldOf wid)
          addSide newR
        addBackdrop (intKey wid)
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
  storeKeyDamage ctx acc oldRects newRects owners
  let addAnim k =
        unless (k == 0) $
          resolveKeyDamage ctx acc oldRects newRects k (DamageInflated defaultDamageSlop)
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
  base <- readRectUnion acc
  added <- readAddedRects acc
  let clip = clipRectToWindow winW winH base
      winArea = winW * winH
      pieces = maybe [] (damagePieces . map (clipRectToWindow winW winH)) added
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

resolveDamageRequests ::
  Context ->
  RectUnion ->
  IM.IntMap Rect ->
  IM.IntMap Rect ->
  [DamageRequest] ->
  IO ()
resolveDamageRequests ctx acc oldRects newRects reqs =
  forM_ reqs $ \case
    ReqFull -> pure ()
    ReqRect r -> addRect acc r
    ReqWidget wid bounds -> resolveKey (intKey wid) bounds
    ReqKey k bounds -> resolveKey k bounds
    ReqPeers wids bounds -> forM_ wids $ \wid -> resolveKey (intKey wid) bounds
  where
    resolveKey = resolveKeyDamage ctx acc oldRects newRects

-- | Damage key @k@'s old and new rects, resolved through @bounds@ and clipped
-- to the key's viewport ('keyViewportClip').
resolveKeyDamage :: Context -> RectUnion -> IM.IntMap Rect -> IM.IntMap Rect -> Int -> DamageBounds -> IO ()
resolveKeyDamage ctx acc oldRects newRects k bounds = do
  clip <- keyViewportClip ctx k
  forM_ [IM.lookup k oldRects, IM.lookup k newRects] $
    mapM_ $ \r -> do
      let clipped = clipToViewport clip (resolveDamageRect bounds r)
      when (rectNonEmpty clipped) $ addRect acc clipped

-- | A running union of rects, as @x0, y0, x1, y1@ followed by how many of
-- them lie outside every floating panel, and for a piece union the rects
-- themselves. The bounds start inverted, so the first rect sets them and an
-- empty union reads back as the zero rect.
data RectUnion = RectUnion !(MutablePrimArray RealWorld Float) !(Maybe (IORef KeptRects))

-- | The rects a piece union holds, up to 'pieceRectLimit'.
data KeptRects = Kept !Int [Rect] | TooMany

-- | A union that keeps only the bounds of its rects.
newRectUnion :: IO RectUnion
newRectUnion = newUnion Nothing

-- | A union that also keeps the first 'pieceRectLimit' rects, for
-- 'damagePieces'. A frame that adds more repaints their bounds.
newPieceUnion :: IO RectUnion
newPieceUnion = newUnion . Just =<< newIORef (Kept 0 [])

pieceRectLimit :: Int
pieceRectLimit = 64

newUnion :: Maybe (IORef KeptRects) -> IO RectUnion
newUnion kept = do
  a <- newPrimArray 5
  writePrimArray a 0 infinity
  writePrimArray a 1 infinity
  writePrimArray a 2 (-infinity)
  writePrimArray a 3 (-infinity)
  writePrimArray a 4 0
  pure (RectUnion a kept)
  where
    infinity = 1 / 0

{-# INLINE addRect #-}
addRect :: RectUnion -> Rect -> IO ()
addRect (RectUnion a kept) r@(Rect x y w h) = do
  x0 <- readPrimArray a 0
  y0 <- readPrimArray a 1
  x1 <- readPrimArray a 2
  y1 <- readPrimArray a 3
  writePrimArray a 0 (min x0 x)
  writePrimArray a 1 (min y0 y)
  writePrimArray a 2 (max x1 (x + w))
  writePrimArray a 3 (max y1 (y + h))
  forM_ kept $ \ref -> modifyIORef' ref $ \case
    Kept n rs | n < pieceRectLimit -> Kept (n + 1) (r : rs)
    _ -> TooMany

-- | The rects a piece union kept, or 'Nothing' if it kept none or more were
-- added than it keeps.
readAddedRects :: RectUnion -> IO (Maybe [Rect])
readAddedRects (RectUnion _ kept) = case kept of
  Nothing -> pure Nothing
  Just ref ->
    readIORef ref >>= \case
      Kept _ rs -> pure (Just rs)
      TooMany -> pure Nothing

readRectUnion :: RectUnion -> IO Rect
readRectUnion (RectUnion a _) = do
  x0 <- readPrimArray a 0
  y0 <- readPrimArray a 1
  x1 <- readPrimArray a 2
  y1 <- readPrimArray a 3
  pure $! if x0 > x1 then Rect 0 0 0 0 else Rect x0 y0 (x1 - x0) (y1 - y0)

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
-- (clipped to scroll viewports, above 'layoutSettleMinArea') and the keys
-- that left or joined the arena.
rectDeltas :: Context -> [Rect] -> IM.IntMap Rect -> IM.IntMap Rect -> IO (RectGroup, RectGroup)
rectDeltas ctx panelRects old new
  | ptrEq old new = pure (emptyGroup, emptyGroup)
  | otherwise = do
      settled <- newRectUnion
      churn <- newRectUnion
      let note acc@(RectUnion a _) r = do
            addRect acc r
            unless (any (rectFullyInside r) panelRects) $
              readPrimArray a 4 >>= writePrimArray a 4 . (+ 1)
      IM.foldrWithKey
        ( \k r rest -> do
            when (rectNonEmpty r) $ do
              when (IM.notMember k new || IM.notMember k old) $ note churn r
              clipped <- clipDeltaToScrollViewport ctx k r
              when (rectArea clipped >= layoutSettleMinArea) $ note settled clipped
            rest
        )
        (pure ())
        (IM.mergeWithKey (\_ a b -> if a /= b then Just (rectUnion a b) else Nothing) id id old new)
      (,) <$> freeze settled <*> freeze churn
  where
    emptyGroup = RectGroup False False (Rect 0 0 0 0)
    freeze acc@(RectUnion a _) = do
      bounds <- readRectUnion acc
      x0 <- readPrimArray a 0
      x1 <- readPrimArray a 2
      outside <- readPrimArray a 4
      let !present = x0 <= x1
      pure (RectGroup present (present && not (null panelRects) && outside == 0) bounds)

-- | The scroll-viewport clip of a keyed node. Look it up once per key and
-- clip each of its rects with 'clipToViewport'.
keyViewportClip :: Context -> Int -> IO (Maybe Rect)
keyViewportClip ctx k = findNodeByKey ctx k >>= maybe (pure Nothing) (getClipRect (ctxNodeArena ctx))

clipToViewport :: Maybe Rect -> Rect -> Rect
clipToViewport clip r = maybe r (fromMaybe (Rect 0 0 0 0) . rectIntersect r) clip

clipDeltaToScrollViewport :: Context -> Int -> Rect -> IO Rect
clipDeltaToScrollViewport ctx k r = (`clipToViewport` r) <$> keyViewportClip ctx k

clipRectToWindow :: Float -> Float -> Rect -> Rect
clipRectToWindow winW winH r =
  fromMaybe (Rect 0 0 0 0) (rectIntersect r (Rect 0 0 winW winH))

-- | A keyed rect clipped to its viewport ('keyViewportClip'), or 'Nothing'
-- when nothing of it shows. Key 0 is not clipped.
clipKeyRect :: Int -> Maybe Rect -> Rect -> Maybe Rect
clipKeyRect k clip r
  | k == 0 = Just r
  | otherwise =
      let clipped = clipToViewport clip r
       in if rectNonEmpty clipped then Just clipped else Nothing

scrollOffsetDamage :: Context -> RectUnion -> WidgetStore -> WidgetStore -> IO ()
scrollOffsetDamage ctx acc oldStore newStore =
  unless (IM.null changedKeys) $ do
    -- Every store key that holds a scroll node's offset, mapped to the first
    -- such node, and every scroll range, mapped to each node with that id.
    -- Built once, only on frames where an offset or range changed.
    owners <- foldClassNodeRevM na PointerNodes addOwner IM.empty
    IM.foldrWithKey
      ( \k _ rest -> do
          forM_ (IM.findWithDefault [] k owners) $ \idx -> do
            -- The scroll node's rect covers the content viewport AND the
            -- scrollbar lane: offset changes move the thumb, which paints
            -- outside the content clip.
            getNonzeroRect na idx >>= mapM_ (addRect acc)
            floatingAncestorRect ctx idx >>= mapM_ (addRect acc)
          rest
      )
      (pure ())
      changedKeys
  where
    na = ctxNodeArena ctx
    -- Floating-pane offsets live in storeFloat; wheel/keyboard offsets
    -- live under the SlotTextAreaScroll slot in storePoint. Both move the
    -- scroller's content and its chrome. New or removed float offsets only
    -- count when nonzero. The range the scroll pass publishes to storePoint
    -- changes with the content's size, which resizes the thumb and can show
    -- or hide a bar; content that changes but still fits keeps a zero range
    -- and repaints nothing here. Two scrollers can share an id (a table's
    -- frozen pane and body), and only the first publishes, so a range change
    -- repaints every scroller with the id.
    changedKeys =
      changedKeysWith (fmap (const ()) . IM.filter (/= 0)) (storeFloat oldStore) (storeFloat newStore)
        `IM.union` changedKeysWith (fmap (const ())) (storePoint oldStore) (storePoint newStore)
    changedKeysWith :: Eq a => (IM.IntMap a -> IM.IntMap ()) -> IM.IntMap a -> IM.IntMap a -> IM.IntMap ()
    changedKeysWith oneSided old new =
      IM.mergeWithKey (\_ a b -> if a /= b then Just () else Nothing) oneSided oneSided old new
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

floatingAncestorRect :: Context -> Int -> IO (Maybe Rect)
floatingAncestorRect ctx idx =
  walkFloatingAncestors (ctxNodeArena ctx) idx (\i _ -> getNonzeroRect (ctxNodeArena ctx) i)

-- | The store keys (outside the scroll offsets) whose value changed and that
-- are not themselves widget keys, with the node each owns through a sub-slot
-- spelling. Changed widget keys were already damaged by their 'ReqKey'
-- requests. The owners fold runs only when some key needs it.
storeKeyChanges :: Context -> WidgetStore -> WidgetStore -> IO ([Int], IM.IntMap NodeIdx)
storeKeyChanges ctx oldStore newStore = do
  misses <-
    filterM (\k -> isNothing <$> findNodeByKey ctx k) (slotChangedKeys oldStore newStore)
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
                let add (!c, !m') sk
                      | IS.member sk wanted && IM.notMember sk m' = (c + 1, IM.insert sk i m')
                      | otherwise = (c, m')
                    (found', m'') = foldl' add (found, m) (ownerKeys (intKey wid))
                go (i - 1) found' m''
  go (n - 1) (0 :: Int) IM.empty
  where
    -- The slots whose writes repaint from the store at paint time: carets and
    -- anchors in text fields, a text area's document, buffer, history and
    -- content extents, drag targets, a colour picker's opening colour, and
    -- the seen/mode records a controlled edit co-writes.
    ownerKeys k =
      [ k
      , slotKey SlotSeen k
      , slotKey SlotCursor k
      , slotKey SlotAnchor k
      , slotKey SlotTextMode k
      , slotKey SlotTextHistory k
      , slotKey SlotSearchCommitted k
      , slotKey SlotSearchAge k
      , slotKey SlotTextAreaRow k
      , slotKey SlotTextAreaCol k
      , slotKey SlotTextAreaPrefCol k
      , slotKey SlotTextAreaAnchorRow k
      , slotKey SlotTextAreaAnchorCol k
      , slotKey SlotTextAreaChanged k
      , slotKey SlotTextAreaText k
      , slotKey SlotTextAreaDocument k
      , slotKey SlotTextAreaBuffer k
      , slotKey SlotTextAreaContentW k
      , slotKey SlotTextAreaContentH k
      , slotKey SlotDrop k
      , slotKey SlotNumericHeld k
      , slotKey SlotNumericRepeat k
      , slotKey SlotMenuOpen k
      , slotKey SlotColorBase k
      ]

-- | Damage for the changed store keys that are not widget keys, through the
-- widgets owning them ('storeKeyOwners'): each owner once, as a 'ReqWidget'
-- with the standard slop would.
storeKeyDamage :: Context -> RectUnion -> IM.IntMap Rect -> IM.IntMap Rect -> IM.IntMap NodeIdx -> IO ()
storeKeyDamage ctx acc oldRects newRects owners =
  forM_ (IS.toList (IS.fromList (IM.elems owners))) $ \idx -> do
    wid <- getWidgetId (ctxNodeArena ctx) idx
    resolveKeyDamage ctx acc oldRects newRects (intKey wid) (DamageInflated defaultDamageSlop)
