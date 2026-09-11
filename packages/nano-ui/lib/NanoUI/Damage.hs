{-# LANGUAGE BangPatterns #-}

module NanoUI.Damage
  ( updatePrevRects
  , floatingPanelRects
  , writeDamage
  ) where

import Control.Monad (filterM, forM, when)
import Data.IORef (readIORef)
import Data.IntMap.Strict qualified as IM
import Data.Text (Text)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import NanoUI.Context
  ( Context (..)
  , DamageRequest (..)
  , WidgetStore (..)
  , getDamageRequests
  , getHotId
  , getLiveAnimations
  , getAnimRectless
  , getPrevRect
  , getPrevClips
  , getPrevNodeTexts
  , getPrevRects
  , getStore
  , getWindowDrag
  , getWindowResize
  , intKey
  , markDirty
  , modalDamageFlip
  , setAnimRectless
  , setDamageAndWindowSize
  , setPrevFloatingPanels
  , setPrevNodeTexts
  , setPrevRectsAndClips
  , takeAnimSettled
  , lookupCustomDamageSlop
  )
import NanoUI.Store (mirrorStoresChanged)
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Input
  ( Input (..)
  , inputWindowSize
  )
import NanoUI.Frame.Hit (findNodeByKey)
import NanoUI.Store (slotKey, slotScrollCross, slotTextAreaScroll)
import NanoUI.Layout.Arena
  ( NodeArena
  , NodeType (..)
  , SizingTag (..)
  , arenaCount
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
  , rectUnion
  , rectW
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
backdropRectForKey :: Context -> Int -> IO (Maybe Rect)
backdropRectForKey ctx k
  | k == 0 = pure Nothing
  | otherwise = findNodeByKey ctx k >>= maybe (pure Nothing) (backdropRectFromNode ctx)

backdropRectsForInteraction :: Context -> [WidgetId] -> [Int] -> IO [Rect]
backdropRectsForInteraction ctx wids keys =
  catMaybes <$> mapM (backdropRectForKey ctx) (filter (/= 0) (map intKey wids ++ keys))

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
  pure (if nonzeroRect r then Just r else Nothing)

updatePrevRects :: Context -> IO ()
updatePrevRects ctx = do
  liveKeys <- IM.keys <$> getLiveAnimations ctx
  prevRectless <- getAnimRectless ctx
  oldRects <- getPrevRects ctx
  oldClips <- getPrevClips ctx
  oldTexts <- getPrevNodeTexts ctx
  let na = ctxNodeArena ctx
      bump rects = do
        let rectless' =
              IM.fromList
                [ (k, if IM.member k rects then 0 else IM.findWithDefault 0 k prevRectless + 1)
                | k <- liveKeys
                ]
        setAnimRectless ctx rectless'
  count <- arenaCount na
  if count <= 0
    then do
      setPrevRectsAndClips ctx IM.empty IM.empty
      setPrevNodeTexts ctx IM.empty
      bump IM.empty
    else do
      -- Rebuild every map from scratch. Used when the key set changes; the
      -- incremental path below cannot delete vanished keys by itself.
      let rebuild = do
            let rebuildGo !i !m !cm !tm
                  | i >= count = do
                      setPrevRectsAndClips ctx m cm
                      setPrevNodeTexts ctx tm
                      bump m
                  | otherwise = do
                      wid <- getWidgetId na i
                      if hashWidgetId wid == 0
                        then rebuildGo (i + 1) m cm tm
                        else do
                          mRect <- getNonzeroRect na i
                          case mRect of
                            Nothing -> rebuildGo (i + 1) m cm tm
                            Just r -> do
                              mClip <- getClipRect na i
                              let !k = intKey wid
                                  !m' = IM.insert k r m
                                  !cm' = maybe cm (\c -> IM.insert k c cm) mClip
                              nt <- getNodeType na i
                              tm' <-
                                if nt == NodeText
                                  then do
                                    txt <- getText na i
                                    let !tmNew = IM.insert k txt tm
                                    pure tmNew
                                  else pure tm
                              rebuildGo (i + 1) m' cm' tm'
            rebuildGo 0 IM.empty IM.empty IM.empty
          -- Incremental update: start from the previous maps and touch only
          -- entries whose value changed. On frames with stable rects (hover,
          -- text churn, animations) this allocates nothing.
          go !i !m !cm !tm !foundOld !dropped
            | i >= count =
                if dropped || foundOld /= IM.size oldRects
                  then rebuild
                  else do
                    setPrevRectsAndClips ctx m cm
                    setPrevNodeTexts ctx tm
                    bump m
            | otherwise = do
                wid <- getWidgetId na i
                if hashWidgetId wid == 0
                  then go (i + 1) m cm tm foundOld dropped
                  else do
                    let !k = intKey wid
                        isOld = IM.member k oldRects
                    mRect <- getNonzeroRect na i
                    case mRect of
                      Nothing ->
                        let dropped' = dropped || isOld
                            m' = if isOld then IM.delete k m else m
                            cm' = if IM.member k cm then IM.delete k cm else cm
                            tm' = if IM.member k tm then IM.delete k tm else tm
                         in go (i + 1) m' cm' tm' foundOld dropped'
                      Just r -> do
                        mClip <- getClipRect na i
                        nt <- getNodeType na i
                        let !m' = if IM.lookup k m == Just r then m else IM.insert k r m
                            !cm' = case mClip of
                              Just c -> if IM.lookup k cm == Just c then cm else IM.insert k c cm
                              Nothing -> if IM.member k cm then IM.delete k cm else cm
                        tm' <-
                          if nt == NodeText
                            then do
                              txt <- getText na i
                              pure (if IM.lookup k tm == Just txt then tm else IM.insert k txt tm)
                            else pure (if IM.member k tm then IM.delete k tm else tm)
                        go (i + 1) m' cm' tm' (foundOld + if isOld then 1 else 0) dropped
      go 0 oldRects oldClips oldTexts 0 False

floatingPanelsInOrder :: Context -> IO [(Int, Rect)]
floatingPanelsInOrder ctx = do
  let na = ctxNodeArena ctx
  n <- arenaCount na
  let go idx acc
        | idx >= n = pure (reverse acc)
        | otherwise = do
            nt <- getNodeType na idx
            if isFloatingNode nt
              then do
                wid <- getWidgetId na idx
                (x, y, w, h) <- getRect na idx
                if hashWidgetId wid == 0
                  then go (idx + 1) acc
                  else go (idx + 1) ((intKey wid, Rect x y w h) : acc)
              else go (idx + 1) acc
  go 0 []

floatingPanelRects :: Context -> IO (IM.IntMap Rect)
floatingPanelRects ctx = IM.fromList <$> floatingPanelsInOrder ctx

writeDamage ::
  Context ->
  Input ->
  Bool ->
  Bool ->
  Size ->
  WidgetStore ->
  WidgetId ->
  WidgetId ->
  WidgetId ->
  Maybe Rect ->
  Maybe Rect ->
  Maybe Rect ->
  IM.IntMap Rect ->
  IM.IntMap Rect ->
  IM.IntMap Text ->
  [Int] ->
  IO ()
writeDamage ctx inp wasDirty overlayOpen oldSize oldStore oldHot oldActive oldFocus oldHotR oldActiveR oldFocusR oldFloatingRects oldRects oldTexts animKeys = do
  let Size winW winH = inputWindowSize inp
      sizeChanged =
        oldSize /= Size 0 0 && oldSize /= Size winW winH
  newStore <- getStore ctx
  panels <- floatingPanelsInOrder ctx
  let newFloatingRects = IM.fromList panels
  newRects <- getPrevRects ctx
  newTexts <- getPrevNodeTexts ctx
  modalFlip <- modalDamageFlip ctx
  liveAnims <- getLiveAnimations ctx
  settled <- takeAnimSettled ctx
  rectless <- getAnimRectless ctx
  orphanAnim <-
    or <$> forM (IM.keys liveAnims) (\k ->
      pure (IM.notMember k newRects && IM.findWithDefault 0 k rectless < orphanEscalateFrames))
  winDragActive <- isJust <$> getWindowDrag ctx
  winResizeActive <- isJust <$> getWindowResize ctx
  let keyedMoved = keyedRectDeltas oldRects newRects
  moved <- mapM (clipDeltaToScrollViewport ctx newRects) keyedMoved
  let stripFloat s = s {storeFloat = IM.empty}
      scrollChanged = storeFloat oldStore /= storeFloat newStore
      scrollPointsChanged = storePoint oldStore /= storePoint newStore
      onlyScrollChanged =
        stripFloat oldStore == stripFloat newStore
          && storePoint oldStore == storePoint newStore
      onlyScrollFloatsChanged =
        scrollChanged && onlyScrollChanged
      settledMoved = filter significantLayoutRect moved
      panelRects = IM.elems newFloatingRects
      allInPanels rs =
        not (null panelRects)
          && not (null rs)
          && all (\r -> any (rectFullyInside r) panelRects) rs
      settledMovedInPanels = allInPanels settledMoved
      diffNew = IM.elems (IM.difference newRects oldRects)
      diffOld = IM.elems (IM.difference oldRects newRects)
      keysChangedInPanels = allInPanels (diffNew ++ diffOld)
      floatingChanged = oldFloatingRects /= newFloatingRects
      windowLive = winDragActive || winResizeActive
      animLive = not (IM.null liveAnims) || settled
      keysChanged =
        not onlyScrollFloatsChanged
          && not (IM.null oldRects)
          && (not (null diffNew) || not (null diffOld))
          && not keysChangedInPanels
      layoutSettle =
        not (IM.null oldRects)
          && not (null settledMoved)
          && not animLive
          && not scrollChanged
          && not settledMovedInPanels
      paintOrphan = orphanAnim && animLive

  requests <- getDamageRequests ctx
  let hasReqFull = ReqFull `elem` requests
      mirrorChanged = mirrorStoresChanged oldStore newStore
      full =
        hasReqFull
          || not onlyScrollFloatsChanged
            && ( wasDirty
                   || mirrorChanged
                   || sizeChanged
                   || overlayOpen
                   || modalFlip
                   || floatingChanged
                   || windowLive
                   || paintOrphan
                   || keysChanged
                   || layoutSettle
               )
  dmg <-
    if full
      then pure DamageFull
      else do
         newHot <- getHotId ctx
         newActive <- readIORef (ctxActiveId ctx)
         newFocus <- readIORef (ctxFocusId ctx)
         -- A parked pointer must not re-damage its hot widget every frame:
         -- only an id change (hover in/out, press, focus move) or a rect
         -- move repaints. Unchanged interaction rects kept the steady state
         -- at DamageFull whenever the hot widget sat inside a panel whose
         -- backdrop covered over half the window.
         let roles =
               [ (oldHot, oldHotR, newHot)
               , (oldActive, oldActiveR, newActive)
               , (oldFocus, oldFocusR, newFocus)
               ]
             oldOf wid
               | wid == oldHot = oldHotR
               | wid == oldActive = oldActiveR
               | wid == oldFocus = oldFocusR
               | otherwise = Nothing
             clipKeys = animKeys ++ IM.keys liveAnims
             missingAnim =
               any
                 ( \k ->
                     k /= 0
                       && isNothing (IM.lookup k oldRects)
                       && isNothing (IM.lookup k newRects)
                       && IM.findWithDefault 0 k rectless < orphanEscalateFrames
                 )
                 clipKeys
         if missingAnim && animLive
           then pure DamageFull
           else do
             reqRs <- resolveDamageRequests ctx oldRects newRects requests
             changedRoles <-
               filterM
                 ( \(_, oldR, newW) -> do
                     newR <- getPrevRect ctx newW
                     pure (oldR /= newR)
                 )
                 roles
             interactiveRs <-
               fmap concat $
                 forM (filter (\w -> hashWidgetId w /= 0) (concat [[oldW, newW] | (oldW, _, newW) <- changedRoles])) $ \wid -> do
                   newR <- getPrevRect ctx wid
                   mSlop <- lookupCustomDamageSlop ctx wid
                   let slop = fromMaybe defaultDamageSlop mSlop
                   catMaybes <$> forM (catMaybes [oldOf wid, newR])
                     (clipWidgetRect ctx newRects wid . rectInflate slop)
             scrollRs <-
               if scrollChanged || scrollPointsChanged
                 then scrollOffsetDamage ctx oldStore newStore
                 else pure []
             animRs <-
               fmap concat $
                 forM (filter (/= 0) clipKeys) $ \k ->
                   catMaybes <$> forM (catMaybes [IM.lookup k oldRects, IM.lookup k newRects])
                     (clipKeyRect ctx newRects k . rectInflate defaultDamageSlop)
             -- Backdrop expansion covers interaction slop (hover/press
             -- halos) and explicit damage requests. Animation keys must not
             -- expand to their panel backdrop: an animated widget inside a
             -- large panel would damage the whole panel every frame, and
             -- once that union crosses half the window the frame degrades
             -- to DamageFull. The scissored replay redraws the backdrop
             -- fill inside the anim's own rect+slop, so no stale pixels
             -- remain.
             backdropRs0 <- backdropRectsForInteraction ctx (concat [ [oldW, newW] | (oldW, _, newW) <- changedRoles]) [k | ReqKey k _ <- requests]
             let backdropRs = map (clipRectToWindow winW winH) backdropRs0
                 -- Same-key text changes that keep the rect (monospace
                 -- counters, refreshed readouts) still repaint: rect-delta
                 -- damage alone would leave them stale. New text keys inside
                 -- floating panels also land here; outside panels the
                 -- keysChanged predicate already forces full damage.
                 textChangedKeys =
                   [ k
                   | (k, t) <- IM.toList newTexts
                   , IM.lookup k oldTexts /= Just t
                   ]
             textRs <-
                fmap concat $
                 forM textChangedKeys $ \k ->
                   case IM.lookup k newRects of
                     Nothing -> pure []
                     Just r -> do
                       -- A text change can reflow the enclosing scroller's
                       -- content and reactivate/resize its chrome (thumb,
                       -- caps) outside the text rect; damage the scroll
                       -- node's full rect so the lane repaints.
                       mScroll <- scrollAncestorRect ctx k
                       pure (r : maybe [] pure mScroll)
             let layoutRs = if onlyScrollFloatsChanged then [] else settledMoved
                 -- Keys that left repaint as the current backdrop over their
                 -- old rects. Keys that arrived must repaint inside their new
                 -- rects too: the retain texture has never shown that content,
                 -- and nothing else covers it (mirror writes escalate these
                 -- frames to DamageFull, but layout-driven churn inside
                 -- floating panels does not).
                 vanishedRs = diffOld ++ diffNew
                 floatingRs = floatingRectDamage oldFloatingRects newFloatingRects
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
                     )
                 clip = clipRectToWindow winW winH base
                 winArea = winW * winH
             -- A live animation with an empty clip is not DamageFull: its
             -- key was either scroll-clipped out of view (nothing visible
             -- changes; scrolling back in damages via the scroll delta) or
             -- rect-less, which missingAnim already promoted above.
             if winArea > 0 && rectArea clip > winArea * 0.5
               then pure DamageFull
               else pure (DamageClip clip)
  setDamageAndWindowSize ctx dmg (Size winW winH)
  setPrevFloatingPanels ctx newFloatingRects (map fst panels)
  when modalFlip (markDirty ctx)
  when (floatingChanged && not (IM.null oldFloatingRects && not (IM.null newFloatingRects))) $
    markDirty ctx

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
  fmap (filter nonzeroRect) $
    forM resolved $ \r ->
      clipDeltaToScrollViewport ctx newRects (k, r)

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

keyedRectDeltas :: IM.IntMap Rect -> IM.IntMap Rect -> [(Int, Rect)]
keyedRectDeltas old new =
  filter (nonzeroRect . snd) $ IM.toList $
    IM.mergeWithKey
      (\_ a b -> if a /= b then Just (rectUnion a b) else Nothing)
      id
      id
      old
      new

clipDeltaToScrollViewport :: Context -> IM.IntMap Rect -> (Int, Rect) -> IO Rect
clipDeltaToScrollViewport ctx _newRects (k, r) = do
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

clipKeyRect :: Context -> IM.IntMap Rect -> Int -> Rect -> IO (Maybe Rect)
clipKeyRect ctx newRects k r
  | k == 0 = pure (Just r)
  | otherwise = do
      clipped <- clipDeltaToScrollViewport ctx newRects (k, r)
      pure (if nonzeroRect clipped then Just clipped else Nothing)

clipWidgetRect :: Context -> IM.IntMap Rect -> WidgetId -> Rect -> IO (Maybe Rect)
clipWidgetRect ctx newRects wid = clipKeyRect ctx newRects (intKey wid)

findScrollNodeByStoreKey :: Context -> Int -> IO (Maybe Int)
findScrollNodeByStoreKey ctx k = do
  count <- arenaCount (ctxNodeArena ctx)
  let go idx
        | idx >= count = pure Nothing
        | otherwise = do
            nt <- getNodeType (ctxNodeArena ctx) idx
            if not (isScrollNode nt)
              then go (idx + 1)
              else do
                wid <- getWidgetId (ctxNodeArena ctx) idx
                let widKey = intKey wid
                    crossKey = slotKey slotScrollCross widKey
                    scrollKey = slotKey slotTextAreaScroll widKey
                if k == widKey || k == crossKey || k == scrollKey
                  then pure (Just idx)
                  else go (idx + 1)
  go 0

-- | Rect of the nearest scroll-container ancestor of a keyed node, covering
-- the content viewport and the scrollbar lane its chrome paints in.
scrollAncestorRect :: Context -> Int -> IO (Maybe Rect)
scrollAncestorRect ctx k =
  findNodeByKey ctx k >>= maybe (pure Nothing) go
  where
    na = ctxNodeArena ctx
    go i = do
      nt <- getNodeType na i
      if isScrollNode nt
        then getNonzeroRect na i
        else do
          p <- getParent na i
          if p < 0 then pure Nothing else go p

scrollOffsetDamage :: Context -> WidgetStore -> WidgetStore -> IO [Rect]
scrollOffsetDamage ctx oldStore newStore = do
  let oldF = storeFloat oldStore
      newF = storeFloat newStore
      oldP = storePoint oldStore
      newP = storePoint newStore
      -- Floating-pane offsets live in storeFloat; wheel/keyboard offsets
      -- live under the slotTextAreaScroll slot in storePoint. Both move the
      -- scroller's content and its chrome.
      changedF = IM.keys $
        IM.mergeWithKey
          (\_ a b -> if a /= b then Just () else Nothing)
          (fmap (const ()) . IM.filter (/= 0))
          (fmap (const ()) . IM.filter (/= 0))
          oldF
          newF
      changedP =
        [ k
        | (k, ()) <-
            IM.toList $
              IM.mergeWithKey
                (\_ a b -> if a /= b then Just () else Nothing)
                (fmap (const ()))
                (fmap (const ()))
                oldP
                newP
        ]
  fmap concat $
    forM (changedF ++ changedP) $ \k ->
      findScrollNodeByStoreKey ctx k >>= \case
        Nothing -> pure []
        Just idx -> do
          nt <- getNodeType (ctxNodeArena ctx) idx
          if not (isScrollNode nt)
            then pure []
            else do
              -- The scroll node's rect covers the content viewport AND the
              -- scrollbar lane: offset changes move the thumb, which paints
              -- outside the content clip.
              mNode <- getNonzeroRect (ctxNodeArena ctx) idx
              mFloat <- floatingAncestorRect ctx idx
              pure (catMaybes [mNode, mFloat])

floatingAncestorRect :: Context -> Int -> IO (Maybe Rect)
floatingAncestorRect ctx idx =
  walkAncestors check (ctxNodeArena ctx) idx
  where
    check i = do
      nt <- getNodeType (ctxNodeArena ctx) i
      if isFloatingNode nt
        then getNonzeroRect (ctxNodeArena ctx) i
        else pure Nothing

nonzeroRect :: Rect -> Bool
nonzeroRect r = rectW r > 0 && rectH r > 0

significantLayoutRect :: Rect -> Bool
significantLayoutRect r = rectArea r >= layoutSettleMinArea
