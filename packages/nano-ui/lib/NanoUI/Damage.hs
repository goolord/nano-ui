module NanoUI.Damage
  ( updatePrevRects
  , floatingPanelRects
  , FrameSnapshot (..)
  , writeDamage
  ) where

import Control.Monad (filterM, forM, unless, when)
import Data.IORef (readIORef)
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.Text (Text)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import NanoUI.Context
  ( Animation
  , Context (..)
  , DamageRequest (..)
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
  , markDirty
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
  , modifyOverlay
  )
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Input
  ( Input (..)
  , inputWindowSize
  )
import NanoUI.Frame.Hit (findNodeByKey)
import NanoUI.Store (eqByPtr, mirrorStoresChanged, ptrEq, slotKey, Slot (..))
import NanoUI.Layout.Arena
  ( NodeArena
  , NodeType (..)
  , SizingTag (..)
  , arenaCount
  , foldNodeRevM
  , getClipRect
  , getHeightSizing
  , getNodeType
  , getParent
  , getRect
  , getStyleIdx
  , getText
  , getWidgetId
  , getWidthSizing
  , isFloatingNode
  , isScrollNode
  )
import NanoUI.Frame.Scroll.Geometry (decodeScrollConfig, scrollBare)
import NanoUI.Widgets.Custom (mkCustomDrawContext)
import NanoUI.Types
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
backdropRectFromNode ctx idx = walkAncestors step (ctxNodeArena ctx) idx
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

{-# INLINE walkAncestors #-}
walkAncestors :: (Int -> IO (Maybe a)) -> NodeArena -> Int -> IO (Maybe a)
walkAncestors step arena idx = loop idx
  where
    loop i
      | i < 0 = pure Nothing
      | otherwise = do
          mr <- step i
          case mr of
            Just x -> pure (Just x)
            Nothing -> getParent arena i >>= loop

{-# INLINE getNonzeroRect #-}
getNonzeroRect :: NodeArena -> Int -> IO (Maybe Rect)
getNonzeroRect arena i = do
  (x, y, w, h) <- getRect arena i
  let r = Rect x y w h
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
        let rectless' =
              IM.fromSet
                (\k -> if IM.member k rects then 0 else IM.findWithDefault 0 k prevRectless + 1)
                (IM.keysSet live <> IM.keysSet rest)
            deadRest = IM.filterWithKey (\k _ -> IM.findWithDefault 0 k rectless' > 300) rest
        unless (IM.null deadRest) $
          pruneAnimRest ctx (\k -> IM.notMember k deadRest)
        -- Every key is live or resting, so this drops exactly the dead resting
        -- keys that are not live again.
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
floatingPanelsInOrder ctx = foldNodeRevM na step []
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
              (x, y, w, h) <- getRect na idx
              pure ((intKey wid, Rect x y w h) : acc)

floatingPanelRects :: Context -> IO (IM.IntMap Rect)
floatingPanelRects ctx = IM.fromList <$> floatingPanelsInOrder ctx

-- | State 'NanoUI.Frame' captures before the UI pass; 'writeDamage' compares
-- it against the finished frame.
data FrameSnapshot = FrameSnapshot
  { fsWasDirty :: !Bool
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
-- reason never pays for the rect diffs.
data FrameDelta = FrameDelta
  { fdWinSize :: !Size
  , fdOverlayOpen :: !Bool
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
  -- ^ Only 'storeFloat' changed in the store, e.g. a floating pane scrolled.
  , fdSettledMoved :: [Rect]
  , fdVanished :: [Rect]
  -- ^ Rects of keys that left the arena.
  , fdArrived :: [Rect]
  -- ^ Rects of keys that joined the arena.
  , fdRedrawn :: ![Int]
  -- ^ Keys of drawings whose ops changed at an unchanged rect.
  }

writeDamage :: Context -> Input -> Bool -> FrameSnapshot -> IO ()
writeDamage ctx inp overlayOpen snap = do
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
      keyedMoved = keyedRectDeltas oldRects newRects
  moved <- mapM (clipDeltaToScrollViewport ctx) keyedMoved
  let scrollChanged = not (eqByPtr (storeFloat oldStore) (storeFloat newStore))
      (diffOld, diffNew) = partitionDiffs oldRects newRects keyedMoved
      delta =
        FrameDelta
          { fdWinSize = inputWindowSize inp
          , fdOverlayOpen = overlayOpen
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
              scrollChanged && oldStore == newStore {storeFloat = storeFloat oldStore}
          , fdSettledMoved = filter (\r -> rectArea r >= layoutSettleMinArea) moved
          , fdVanished = diffOld
          , fdArrived = diffNew
          , fdRedrawn = redrawn
          }
  dmg <-
    if needsFullDamage snap delta
      then pure DamageFull
      else clipDamage ctx snap delta
  modifyDamage ctx (\ds -> ds {dsDamage = dmg, dsLastWindowSize = inputWindowSize inp, dsRequests = []})
  modifyOverlay ctx (\os -> os {osPrevFloatingRects = newFloatingRects, osPrevFloatingOrder = map fst panels})
  when modalFlip (markDirty ctx)
  when (fdFloatingChanged delta && not (IM.null (fsFloatingRects snap) && not (IM.null newFloatingRects))) $
    markDirty ctx

-- | Settle every drawing's ops for this frame and return the keys of those
-- that now draw something else at an unchanged rect. A drawing follows state
-- the arena does not hold, so nothing else damages it, and paint must not
-- replay the previous frame's ops for it. What this costs per widget is the
-- widget's own choice: see 'refreshCustomDrawingOps'.
refreshCustomDrawings :: Context -> IO [Int]
refreshCustomDrawings ctx = arenaCount na >>= \count -> go count 0 []
  where
    na = ctxNodeArena ctx
    go count !i acc
      | i >= count = pure acc
      | otherwise = do
          nt <- getNodeType na i
          if nt /= NodeDrawing
            then go count (i + 1) acc
            else do
              wid <- getWidgetId na i
              (x, y, w, h) <- getRect na i
              let rect = Rect x y w h
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
              go count (i + 1) (if changed then intKey wid : acc else acc)

-- | Whether the frame repaints the whole window rather than a clip.
needsFullDamage :: FrameSnapshot -> FrameDelta -> Bool
needsFullDamage snap d =
  ReqFull `elem` fdRequests d
    || not (fdScrollOnly d)
      && ( fsWasDirty snap
             || mirrorStoresChanged (fsStore snap) (fdStore d)
             || sizeChanged
             || fdOverlayOpen d
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
    sizeChanged = oldSize /= Size 0 0 && oldSize /= fdWinSize d
    recentlyRectless k = IM.findWithDefault 0 k (fdRectless d) < orphanEscalateFrames
    orphanAnim =
      any (\k -> IM.notMember k newRects && recentlyRectless k) (IM.keys (fdLiveAnims d))
    -- A live animation whose key has no rect this frame or last is not
    -- clipped: the retain texture may never have shown it.
    missingAnim =
      any
        (\k -> k /= 0 && IM.notMember k oldRects && IM.notMember k newRects && recentlyRectless k)
        (IS.toList (fsAnimKeys snap <> IM.keysSet (fdLiveAnims d)))
    panelRects = IM.elems (fdFloatingRects d)
    allInPanels rs =
      not (null panelRects)
        && not (null rs)
        && all (\r -> any (rectFullyInside r) panelRects) rs
    keysChanged =
      not (IM.null oldRects)
        && (not (null (fdArrived d)) || not (null (fdVanished d)))
        && not (allInPanels (fdArrived d ++ fdVanished d))
    layoutSettle =
      not (IM.null oldRects)
        && not (null (fdSettledMoved d))
        && not (fdAnimLive d)
        && not (fdScrollChanged d)
        && not (allInPanels (fdSettledMoved d))

-- | The clip covering everything that changed, or 'DamageFull' once that clip
-- exceeds half the window.
clipDamage :: Context -> FrameSnapshot -> FrameDelta -> IO Damage
clipDamage ctx snap d = do
  newHot <- getHotId ctx
  newActive <- readIORef (ctxActiveId ctx)
  newFocus <- readIORef (ctxFocusId ctx)
  let oldRects = fsRects snap
      newRects = fdRects d
      requests = fdRequests d
      Size winW winH = fdWinSize d
      -- A parked pointer must not re-damage its hot widget every frame:
      -- only an id change (hover in/out, press, focus move) or a rect
      -- move repaints. Unchanged interaction rects kept the steady state
      -- at DamageFull whenever the hot widget sat inside a panel whose
      -- backdrop covered over half the window.
      roles =
        [ (fsHot snap, fsHotRect snap, newHot)
        , (fsActive snap, fsActiveRect snap, newActive)
        , (fsFocus snap, fsFocusRect snap, newFocus)
        ]
      oldOf wid
        | wid == fsHot snap = fsHotRect snap
        | wid == fsActive snap = fsActiveRect snap
        | wid == fsFocus snap = fsFocusRect snap
        | otherwise = Nothing
  reqRs <- resolveDamageRequests ctx oldRects newRects requests
  changedRoles <-
    filterM
      ( \(_, oldR, newW) -> do
          newR <- getPrevRect ctx newW
          pure (oldR /= newR)
      )
      roles
  let changedWids = concat [[oldW, newW] | (oldW, _, newW) <- changedRoles]
  interactiveRs <-
    fmap concat $
      forM (filter (\w -> hashWidgetId w /= 0) changedWids) $ \wid -> do
        newR <- getPrevRect ctx wid
        mSlop <- lookupCustomDamageSlop ctx wid
        let slop = fromMaybe defaultDamageSlop mSlop
        catMaybes <$> forM (catMaybes [oldOf wid, newR])
          (clipKeyRect ctx (intKey wid) . rectInflate slop)
  scrollRs <-
    if fdScrollChanged d || fdPointsChanged d
      then scrollOffsetDamage ctx (fsStore snap) (fdStore d)
      else pure []
  animRs <-
    fmap concat $
      forM (IS.toList (IS.delete 0 (fsAnimKeys snap <> IM.keysSet (fdLiveAnims d)))) $ \k ->
        catMaybes <$> forM (catMaybes [IM.lookup k oldRects, IM.lookup k newRects])
          (clipKeyRect ctx k . rectInflate defaultDamageSlop)
  -- Backdrop expansion covers interaction slop (hover/press
  -- halos) and explicit damage requests. Animation keys must not
  -- expand to their panel backdrop: an animated widget inside a
  -- large panel would damage the whole panel every frame, and
  -- once that union crosses half the window the frame degrades
  -- to DamageFull. The scissored replay redraws the backdrop
  -- fill inside the anim's own rect+slop, so no stale pixels
  -- remain.
  backdropRs <-
    fmap (map (clipRectToWindow winW winH) . catMaybes) $
      forM (filter (/= 0) (map intKey changedWids ++ [k | ReqKey k _ <- requests])) $ \k ->
        findNodeByKey ctx k >>= maybe (pure Nothing) (backdropRectFromNode ctx)
  let -- Same-key text changes that keep the rect (monospace
      -- counters, refreshed readouts) still repaint: rect-delta
      -- damage alone would leave them stale. New text keys inside
      -- floating panels also land here; outside panels the
      -- keysChanged predicate already forces full damage.
      textChangedKeys
        -- updatePrevRects keeps last frame's map when no text changed.
        | ptrEq (fdTexts d) (fsTexts snap) = []
        | otherwise =
            IM.keys $
              IM.mergeWithKey
                (\_ new old -> if new /= old then Just () else Nothing)
                (IM.map (const ()))
                (const IM.empty)
                (fdTexts d)
                (fsTexts snap)
  textRs <-
    fmap concat $
      forM textChangedKeys $ \k ->
        case IM.lookup k newRects of
          Nothing -> pure []
          Just r -> do
            -- An image that switched to another image keeps its size, so
            -- only its own rect repaints. A text change can reflow the
            -- enclosing scroller's content and reactivate/resize its
            -- chrome (thumb, caps) outside the text rect; damage the
            -- scroll node's full rect so the lane repaints.
            mIdx <- findNodeByKey ctx k
            isImage <- maybe (pure False) (fmap (== NodeImage) . getNodeType (ctxNodeArena ctx)) mIdx
            if isImage
              then pure [r]
              else do
                mScroll <- scrollAncestorRect ctx k
                pure (r : maybe [] pure mScroll)
  -- Drawings redrawn in place repaint their own rects, like a text change
  -- that keeps its rect.
  redrawnRs <-
    catMaybes
      <$> forM (fdRedrawn d) (\k -> maybe (pure Nothing) (clipKeyRect ctx k) (IM.lookup k newRects))
  let layoutRs = if fdScrollOnly d then [] else fdSettledMoved d
      -- Keys that left repaint as the current backdrop over their
      -- old rects. Keys that arrived must repaint inside their new
      -- rects too: the retain texture has never shown that content,
      -- and nothing else covers it (mirror writes escalate these
      -- frames to DamageFull, but layout-driven churn inside
      -- floating panels does not).
      vanishedRs = fdVanished d ++ fdArrived d
      floatingRs = floatingRectDamage (fsFloatingRects snap) (fdFloatingRects d)
      base =
        unionRects
          ( reqRs
              ++ interactiveRs
              ++ scrollRs
              ++ animRs
              ++ backdropRs
              ++ layoutRs
              ++ vanishedRs
              ++ floatingRs
              ++ textRs
              ++ redrawnRs
          )
      clip = clipRectToWindow winW winH base
      winArea = winW * winH
  -- A live animation with an empty clip is not DamageFull: its
  -- key was either scroll-clipped out of view (nothing visible
  -- changes; scrolling back in damages via the scroll delta) or
  -- rect-less, which missingAnim already promoted to full.
  pure $
    if winArea > 0 && rectArea clip > winArea * 0.5
      then DamageFull
      else DamageClip clip

resolveDamageRequests ::
  Context ->
  IM.IntMap Rect ->
  IM.IntMap Rect ->
  [DamageRequest] ->
  IO [Rect]
resolveDamageRequests ctx oldRects newRects reqs =
  fmap concat $
    forM reqs $ \case
      ReqFull -> pure []
      ReqRect r -> pure [r]
      ReqWidget wid bounds -> resolveSingleKey ctx oldRects newRects (intKey wid) bounds
      ReqKey k bounds -> resolveSingleKey ctx oldRects newRects k bounds
      ReqPeers wids bounds ->
        fmap concat $ forM wids $ \wid ->
          resolveSingleKey ctx oldRects newRects (intKey wid) bounds

resolveSingleKey ::
  Context ->
  IM.IntMap Rect ->
  IM.IntMap Rect ->
  Int ->
  DamageBounds ->
  IO [Rect]
resolveSingleKey ctx oldRects newRects k bounds = do
  let oldR = IM.lookup k oldRects
      newR = IM.lookup k newRects
      resolved = catMaybes [fmap (resolveDamageRect bounds) oldR, fmap (resolveDamageRect bounds) newR]
  fmap (filter rectNonEmpty) $
    forM resolved $ \r ->
      clipDeltaToScrollViewport ctx (k, r)

floatingRectDamage :: IM.IntMap Rect -> IM.IntMap Rect -> [Rect]
floatingRectDamage old new =
  concat $ IM.elems $
    IM.mergeWithKey
      (\_ r1 r2 -> if r1 /= r2 then Just [r1, r2] else Nothing)
      (fmap (: []))
      (fmap (: []))
      old
      new

unionRects :: [Rect] -> Rect
unionRects [] = Rect 0 0 0 0
unionRects (r : rs) = foldl' rectUnion r rs

partitionDiffs :: IM.IntMap Rect -> IM.IntMap Rect -> [(Int, Rect)] -> ([Rect], [Rect])
partitionDiffs old new kMoved = go kMoved [] []
  where
    go [] dOld dNew = (dOld, dNew)
    go ((k, r) : rest) dOld dNew
      | IM.notMember k new = go rest (r : dOld) dNew
      | IM.notMember k old = go rest dOld (r : dNew)
      | otherwise = go rest dOld dNew

keyedRectDeltas :: IM.IntMap Rect -> IM.IntMap Rect -> [(Int, Rect)]
keyedRectDeltas old new
  | ptrEq old new = []
  | otherwise =
      filter (rectNonEmpty . snd) $ IM.toList $
        IM.mergeWithKey
          (\_ a b -> if a /= b then Just (rectUnion a b) else Nothing)
          id
          id
          old
          new

clipDeltaToScrollViewport :: Context -> (Int, Rect) -> IO Rect
clipDeltaToScrollViewport ctx (k, r) = do
  findNodeByKey ctx k >>= \case
    Nothing -> pure r
    Just idx -> do
      mClip <- getClipRect (ctxNodeArena ctx) idx
      pure $
        case mClip of
          Nothing -> r
          Just clip -> fromMaybe (Rect 0 0 0 0) (rectIntersect r clip)

clipRectToWindow :: Float -> Float -> Rect -> Rect
clipRectToWindow winW winH r =
  fromMaybe (Rect 0 0 0 0) (rectIntersect r (Rect 0 0 winW winH))

clipKeyRect :: Context -> Int -> Rect -> IO (Maybe Rect)
clipKeyRect ctx k r
  | k == 0 = pure (Just r)
  | otherwise = do
      clipped <- clipDeltaToScrollViewport ctx (k, r)
      pure (if rectNonEmpty clipped then Just clipped else Nothing)

-- | Rect of the nearest scroll-container ancestor of a keyed node, covering
-- the content viewport and the scrollbar lane its chrome paints in. The walk
-- stops at the first scroll node even when its rect is empty.
scrollAncestorRect :: Context -> Int -> IO (Maybe Rect)
scrollAncestorRect ctx k =
  findNodeByKey ctx k >>= maybe (pure Nothing) (fmap (fromMaybe Nothing) . walkAncestors step na)
  where
    na = ctxNodeArena ctx
    step i = do
      nt <- getNodeType na i
      if isScrollNode nt
        then Just <$> getNonzeroRect na i
        else pure Nothing

scrollOffsetDamage :: Context -> WidgetStore -> WidgetStore -> IO [Rect]
scrollOffsetDamage ctx oldStore newStore =
  case changedKeys of
    [] -> pure []
    _ -> do
      -- Every store key that holds a scroll node's offset, mapped to the first
      -- such node. Built once, only on frames where an offset changed.
      owners <- foldNodeRevM na addOwner IM.empty
      fmap concat $
        forM changedKeys $ \k ->
          case IM.lookup k owners of
            Nothing -> pure []
            Just idx -> do
              -- The scroll node's rect covers the content viewport AND the
              -- scrollbar lane: offset changes move the thumb, which paints
              -- outside the content clip.
              mNode <- getNonzeroRect na idx
              mFloat <- floatingAncestorRect ctx idx
              pure (catMaybes [mNode, mFloat])
  where
    na = ctxNodeArena ctx
    -- Floating-pane offsets live in storeFloat; wheel/keyboard offsets
    -- live under the SlotTextAreaScroll slot in storePoint. Both move the
    -- scroller's content and its chrome. New or removed float offsets only
    -- count when nonzero.
    changedKeys =
      changedKeysWith (fmap (const ()) . IM.filter (/= 0)) (storeFloat oldStore) (storeFloat newStore)
        ++ changedKeysWith (fmap (const ())) (storePoint oldStore) (storePoint newStore)
    changedKeysWith :: Eq a => (IM.IntMap a -> IM.IntMap ()) -> IM.IntMap a -> IM.IntMap a -> [Int]
    changedKeysWith oneSided old new =
      IM.keys (IM.mergeWithKey (\_ a b -> if a /= b then Just () else Nothing) oneSided oneSided old new)
    addOwner m idx = do
      nt <- getNodeType na idx
      if not (isScrollNode nt)
        then pure m
        else do
          wid <- getWidgetId na idx
          let widKey = intKey wid
          pure $
            IM.insert widKey idx $
              IM.insert (slotKey SlotScrollCross widKey) idx $
                IM.insert (slotKey SlotTextAreaScroll widKey) idx m

floatingAncestorRect :: Context -> Int -> IO (Maybe Rect)
floatingAncestorRect ctx idx =
  walkAncestors check (ctxNodeArena ctx) idx
  where
    check i = do
      nt <- getNodeType (ctxNodeArena ctx) i
      if isFloatingNode nt
        then getNonzeroRect (ctxNodeArena ctx) i
        else pure Nothing
