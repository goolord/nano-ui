-- | Compare completed frames and explicit invalidations to choose repaint bounds.
-- Damage is computed before painting so retained backends can redraw only a clip.
module NanoUI.Internal.Damage
  ( updatePrevRects
  , floatingPanelRects
  , FrameSnapshot (..)
  , writeDamage
  ) where

import Control.Monad (forM_, join, unless, when)
import Data.IORef (readIORef)
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.Maybe (fromMaybe, isJust)
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
  , getsOverlay
  , modifyOverlay
  )
import NanoUI.Internal.Id (WidgetId (..), hashWidgetId)
import NanoUI.Internal.Input
  ( Input (..)
  , inputWindowSize
  )
import NanoUI.Internal.Frame.Hit (findNodeByKey)
import NanoUI.Internal.Store (Slot (..), eqByPtr, mirrorStoresChanged, ptrEq, slotKey)
import NanoUI.Internal.Layout.Arena
  ( NodeArena
  , NodeIdx
  , NodeType (..)
  , SizingTag (..)
  , arenaCount
  , floatingNodeCount
  , foldNodesM
  , foldNodeRevM
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
  floating <- floatingNodeCount na
  if floating <= 0 then pure [] else foldNodeRevM na step []
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
  -- ^ Only 'storeFloat' changed in the store, e.g. a floating pane scrolled.
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
      delta =
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
              scrollChanged && oldStore == newStore {storeFloat = storeFloat oldStore}
          , fdSettledMoved = settledMoved
          , fdChurn = churn
          , fdRedrawn = redrawn
          }
  dmg <-
    if needsFullDamage snap delta
      then pure DamageFull
      else clipDamage ctx snap delta
  modifyDamage ctx (\ds -> ds {dsDamage = dmg, dsLastWindowSize = inputWindowSize inp, dsRequests = []})
  -- Most frames have no panel now or before, and then there is nothing to write.
  prevEmpty <- getsOverlay ctx (IM.null . osPrevFloatingRects)
  unless (null panels && prevEmpty) $
    modifyOverlay ctx $ \os ->
      os {osPrevFloatingRects = newFloatingRects, osPrevFloatingOrder = map fst panels}
  when modalFlip (markDirty ctx)
  when (fdFloatingChanged delta && not (IM.null (fsFloatingRects snap) && not (IM.null newFloatingRects))) $
    markDirty ctx

-- | Settle every drawing's ops for this frame and return the keys of those
-- that now draw something else at an unchanged rect. A drawing follows state
-- the arena does not hold, so nothing else damages it, and paint must not
-- replay the previous frame's ops for it. What this costs per widget is the
-- widget's own choice: see 'refreshCustomDrawingOps'.
refreshCustomDrawings :: Context -> IO [Int]
refreshCustomDrawings ctx = do
  dc <- readIORef (ctxDrawingCache ctx)
  -- A drawing with neither entry settles nothing, so a view without drawings
  -- skips the walk.
  if IM.null (dcsDrawings dc) && IM.null (dcsCustomDrawings dc)
    then pure []
    else foldNodesM na step []
  where
    na = ctxNodeArena ctx
    step acc i = do
      nt <- getNodeType na i
      if nt /= NodeDrawing
        then pure acc
        else do
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
needsFullDamage :: FrameSnapshot -> FrameDelta -> Bool
needsFullDamage snap d =
  ReqFull `elem` fdRequests d
    || not (fdScrollOnly d)
      && ( fsWasDirty snap
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

-- | The clip covering everything that changed, or 'DamageFull' once that clip
-- exceeds half the window.
clipDamage :: Context -> FrameSnapshot -> FrameDelta -> IO Damage
clipDamage ctx snap d = do
  let oldRects = fsRects snap
      newRects = fdRects d
      Size winW winH = fdWinSize d
      oldOf wid
        | wid == fsHot snap = fsHotRect snap
        | wid == fsActive snap = fsActiveRect snap
        | wid == fsFocus snap = fsFocusRect snap
        | otherwise = Nothing
  acc <- newRectUnion
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
  let addAnim k =
        unless (k == 0) $ do
          clip <- keyViewportClip ctx k
          forM_ [IM.lookup k oldRects, IM.lookup k newRects] $
            mapM_ (mapM_ (addRect acc) . clipKeyRect k clip . rectInflate defaultDamageSlop)
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
  let addText k =
        forM_ (IM.lookup k newRects) $ \r -> do
          -- An image that switched to another image keeps its size, so only
          -- its own rect repaints. A text change can reflow the enclosing
          -- scroller's content and reactivate/resize its chrome (thumb, caps)
          -- outside the text rect; damage the scroll node's full rect so the
          -- lane repaints.
          addRect acc r
          findNodeByKey ctx k >>= mapM_ (\idx -> do
            isImage <- (== NodeImage) <$> getNodeType (ctxNodeArena ctx) idx
            unless isImage $ scrollAncestorRect ctx idx >>= mapM_ (addRect acc))
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
  let clip = clipRectToWindow winW winH base
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
    resolveKey k bounds = do
      clip <- keyViewportClip ctx k
      forM_ [IM.lookup k oldRects, IM.lookup k newRects] $
        mapM_ $ \r -> do
          let clipped = clipToViewport clip (resolveDamageRect bounds r)
          when (rectNonEmpty clipped) $ addRect acc clipped

-- | A running union of rects, as @x0, y0, x1, y1@ followed by how many of
-- them lie outside every floating panel. The bounds start inverted, so the
-- first rect sets them and an empty union reads back as the zero rect.
newtype RectUnion = RectUnion (MutablePrimArray RealWorld Float)

newRectUnion :: IO RectUnion
newRectUnion = do
  a <- newPrimArray 5
  writePrimArray a 0 infinity
  writePrimArray a 1 infinity
  writePrimArray a 2 (-infinity)
  writePrimArray a 3 (-infinity)
  writePrimArray a 4 0
  pure (RectUnion a)
  where
    infinity = 1 / 0

{-# INLINE addRect #-}
addRect :: RectUnion -> Rect -> IO ()
addRect (RectUnion a) (Rect x y w h) = do
  x0 <- readPrimArray a 0
  y0 <- readPrimArray a 1
  x1 <- readPrimArray a 2
  y1 <- readPrimArray a 3
  writePrimArray a 0 (min x0 x)
  writePrimArray a 1 (min y0 y)
  writePrimArray a 2 (max x1 (x + w))
  writePrimArray a 3 (max y1 (y + h))

readRectUnion :: RectUnion -> IO Rect
readRectUnion (RectUnion a) = do
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
      let note acc@(RectUnion a) r = do
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
    freeze acc@(RectUnion a) = do
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

-- | Rect of the nearest scroll-container ancestor of a keyed node, covering
-- the content viewport and the scrollbar lane its chrome paints in. The walk
-- stops at the first scroll node even when its rect is empty.
scrollAncestorRect :: Context -> NodeIdx -> IO (Maybe Rect)
scrollAncestorRect ctx idx = join <$> walkAncestors na idx step
  where
    na = ctxNodeArena ctx
    step i = do
      nt <- getNodeType na i
      if isScrollNode nt
        then Just <$> getNonzeroRect na i
        else pure Nothing

scrollOffsetDamage :: Context -> RectUnion -> WidgetStore -> WidgetStore -> IO ()
scrollOffsetDamage ctx acc oldStore newStore =
  unless (IM.null changedKeys) $ do
    -- Every store key that holds a scroll node's offset, mapped to the first
    -- such node. Built once, only on frames where an offset changed.
    owners <- foldNodeRevM na addOwner IM.empty
    IM.foldrWithKey
      ( \k _ rest -> do
          forM_ (IM.lookup k owners) $ \idx -> do
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
    -- count when nonzero.
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
          pure $
            IM.insert widKey idx $
              IM.insert (slotKey SlotScrollCross widKey) idx $
                IM.insert (slotKey SlotTextAreaScroll widKey) idx m

floatingAncestorRect :: Context -> Int -> IO (Maybe Rect)
floatingAncestorRect ctx idx =
  walkFloatingAncestors (ctxNodeArena ctx) idx (\i _ -> getNonzeroRect (ctxNodeArena ctx) i)
