{-# LANGUAGE BangPatterns #-}

module NanoUI.Damage
  ( updatePrevRects
  , floatingPanelRects
  , writeDamage
  ) where

import Control.Monad (forM, when)
import Data.IORef (readIORef)
import Data.IntMap.Strict qualified as IM
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import NanoUI.Context
  ( Context (..)
  , DamageRequest (..)
  , WidgetStore (..)
  , getDamageRequests
  , getHotId
  , getLiveAnimations
  , getPrevRect
  , getPrevRectByKey
  , getPrevRects
  , getStore
  , intKey
  , markDirty
  , modalDamageFlip
  , setDamageAndWindowSize
  , setPrevFloatingPanels
  , setPrevRectsAndClips
  , takeAnimSettled
  )
import NanoUI.Store (mirrorStoresChanged)
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Input
  ( Input (..)
  , inputWindowSize
  )
import NanoUI.Frame.Hit (findNodeByKey, findNodeByWidgetId)
import NanoUI.Store (slotKey, slotScrollCross)
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
  , getWidgetId
  , getWidthSizing
  , isFloatingNode
  , isScrollNode
  )
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

-- Partial retain clears with themeWindow. Expand interaction clips to the painted
-- panel/window backdrop so slop pixels get the correct fill, not window color.
backdropRectForWidget :: Context -> WidgetId -> IO (Maybe Rect)
backdropRectForWidget ctx wid
  | hashWidgetId wid == 0 = pure Nothing
  | otherwise = nodeBackdrop ctx (findNodeByWidgetId ctx wid)

backdropRectForKey :: Context -> Int -> IO (Maybe Rect)
backdropRectForKey ctx k
  | k == 0 = pure Nothing
  | otherwise = nodeBackdrop ctx (findNodeByKey ctx k)

nodeBackdrop :: Context -> IO (Maybe Int) -> IO (Maybe Rect)
nodeBackdrop ctx mIdxAct = do
  mIdx <- mIdxAct
  case mIdx of
    Nothing -> pure Nothing
    Just idx -> backdropRectFromNode ctx idx

backdropRectsForInteraction :: Context -> [WidgetId] -> [Int] -> IO [Rect]
backdropRectsForInteraction ctx wids keys = do
  ws <- catMaybes <$> forM wids (backdropRectForWidget ctx)
  ks <- catMaybes <$> forM keys (backdropRectForKey ctx)
  pure (ws ++ ks)

backdropRectFromNode :: Context -> Int -> IO (Maybe Rect)
backdropRectFromNode ctx idx = do
  let na = ctxNodeArena ctx
  nt <- getNodeType na idx
  case nt of
    NodePanel -> nodeRect na idx
    NodeWindow -> nodeRect na idx
    NodeModal -> nodeRect na idx
    NodePopup -> nodeRect na idx
    NodeScrollContainer -> do
      (wTag, _) <- getWidthSizing na idx
      (hTag, _) <- getHeightSizing na idx
      if wTag == SizingGrow && hTag == SizingGrow
        then walkParent na idx
        else nodeRect na idx
    _ -> walkParent na idx
  where
    nodeRect arena i = do
      (x, y, w, h) <- getRect arena i
      let r = Rect x y w h
      pure (if nonzeroRect r then Just r else Nothing)
    walkParent arena i = do
      p <- getParent arena i
      if p < 0
        then pure Nothing
        else backdropRectFromNode ctx p

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

updatePrevRects :: Context -> IO ()
updatePrevRects ctx = do
  count <- arenaCount (ctxNodeArena ctx)
  if count <= 0
    then setPrevRectsAndClips ctx IM.empty IM.empty
    else do
      let go !i !m !cm
            | i >= count = setPrevRectsAndClips ctx m cm
            | otherwise = do
                wid <- getWidgetId (ctxNodeArena ctx) i
                if hashWidgetId wid == 0
                  then go (i + 1) m cm
                  else do
                    (x, y, w, h) <- getRect (ctxNodeArena ctx) i
                    let r = Rect x y w h
                    if nonzeroRect r
                      then do
                        mClip <- getClipRect (ctxNodeArena ctx) i
                        let !k = intKey wid
                            !m' = IM.insert k r m
                            !cm' = maybe cm (\c -> IM.insert k c cm) mClip
                        go (i + 1) m' cm'
                      else go (i + 1) m cm
      go 0 IM.empty IM.empty

floatingPanelsInOrder :: Context -> IO [(Int, Rect)]
floatingPanelsInOrder ctx = do
  n <- arenaCount (ctxNodeArena ctx)
  let go idx acc
        | idx >= n = pure (reverse acc)
        | otherwise = do
            nt <- getNodeType (ctxNodeArena ctx) idx
            if nt == NodeWindow || nt == NodeModal || nt == NodePopup
              then do
                wid <- getWidgetId (ctxNodeArena ctx) idx
                (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
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
  [Int] ->
  IO ()
writeDamage ctx inp wasDirty overlayOpen oldSize oldStore oldHot oldActive oldFocus oldHotR oldActiveR oldFocusR oldFloatingRects oldRects animKeys = do
  let Size winW winH = inputWindowSize inp
      sizeChanged =
        oldSize /= Size 0 0 && oldSize /= Size winW winH
  newStore <- getStore ctx
  panels <- floatingPanelsInOrder ctx
  let newFloatingRects = IM.fromList panels
  newRects <- getPrevRects ctx
  modalFlip <- modalDamageFlip ctx
  liveAnims <- getLiveAnimations ctx
  settled <- takeAnimSettled ctx
  orphanAnim <-
    fmap or $
      forM (IM.keys liveAnims) $ \k ->
        isNothing <$> getPrevRectByKey ctx k
  winDragActive <- isJust <$> readIORef (ctxWindowDrag ctx)
  winResizeActive <- isJust <$> readIORef (ctxWindowResize ctx)
  let keyedMoved = keyedRectDeltas oldRects newRects
  moved <- mapM (clipDeltaToScrollViewport ctx newRects) keyedMoved
  let stripFloat s = s {storeFloat = IM.empty}
      scrollChanged = storeFloat oldStore /= storeFloat newStore
      onlyScrollFloatsChanged =
        scrollChanged && stripFloat oldStore == stripFloat newStore
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
        let ids = [oldHot, oldActive, oldFocus, newHot, newActive, newFocus]
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
                )
                clipKeys
        if missingAnim && animLive
          then pure DamageFull
          else do
            reqRs <- resolveDamageRequests ctx oldRects newRects requests
            interactiveRs <-
              fmap concat $
                forM ids $ \wid ->
                  if hashWidgetId wid == 0
                    then pure []
                    else do
                      newR <- getPrevRect ctx wid
                      let rects = catMaybes [oldOf wid, newR]
                      fmap catMaybes $
                        forM rects $ \r ->
                          clipWidgetRect ctx newRects wid (rectInflate defaultDamageSlop r)
            scrollRs <-
              if scrollChanged
                then scrollOffsetDamage ctx oldStore newStore
                else pure []
            animRs <-
              fmap concat $
                forM clipKeys $ \k ->
                  if k == 0
                    then pure []
                    else
                      fmap catMaybes $
                        forM (catMaybes [IM.lookup k oldRects, IM.lookup k newRects]) $ \r ->
                          clipDeltaToScrollViewport ctx newRects (k, rectInflate defaultDamageSlop r) >>= \c ->
                            if nonzeroRect c then pure (Just c) else pure Nothing
            backdropRs0 <- backdropRectsForInteraction ctx ids (clipKeys ++ [k | ReqKey k _ <- requests])
            let backdropRs = map (clipRectToWindow winW winH) backdropRs0
            let layoutRs = if onlyScrollFloatsChanged then [] else settledMoved
                vanishedRs = IM.elems (IM.difference oldRects newRects)
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
                    )
                rawClip = base
                clip = fromMaybe (Rect 0 0 0 0) (rectIntersect rawClip (Rect 0 0 winW winH))
                winArea = winW * winH
            if (animLive && not (nonzeroRect clip))
                 || (winArea > 0 && rectArea clip > winArea * 0.5)
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
    forM reqs $ \req ->
      case req of
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
  concat
    [ case (IM.lookup k old, IM.lookup k new) of
        (Nothing, Just r) -> [r]
        (Just r, Nothing) -> [r]
        (Just r1, Just r2)
          | r1 /= r2 -> [r1, r2]
        _ -> []
    | k <- IM.keys (IM.union old new)
    ]

unionRects :: [Rect] -> Rect
unionRects [] = Rect 0 0 0 0
unionRects (r : rs) = foldl' rectUnion r rs

keyedRectDeltas :: IM.IntMap Rect -> IM.IntMap Rect -> [(Int, Rect)]
keyedRectDeltas old new =
  filter (nonzeroRect . snd) $
    concat
      [ [ (k, r) | (k, r) <- IM.toList (IM.difference old new) ]
      , [ (k, r) | (k, r) <- IM.toList (IM.difference new old) ]
      , [ (k, rectUnion a b)
        | (k, a) <- IM.toList old
        , Just b <- [IM.lookup k new]
        , a /= b
        ]
      ]

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

clipWidgetRect :: Context -> IM.IntMap Rect -> WidgetId -> Rect -> IO (Maybe Rect)
clipWidgetRect ctx newRects wid r =
  if hashWidgetId wid == 0
    then pure (Just r)
    else do
      let k = intKey wid
      clipped <- clipDeltaToScrollViewport ctx newRects (k, r)
      if nonzeroRect clipped then pure (Just clipped) else pure Nothing

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
                if k == widKey || k == crossKey
                  then pure (Just idx)
                  else go (idx + 1)
  go 0

scrollOffsetDamage :: Context -> WidgetStore -> WidgetStore -> IO [Rect]
scrollOffsetDamage ctx oldStore newStore = do
  let oldF = storeFloat oldStore
      newF = storeFloat newStore
      changed =
        [ k
        | k <- IM.keys (IM.union oldF newF)
        , IM.findWithDefault 0 k oldF /= IM.findWithDefault 0 k newF
        ]
  fmap concat $
    forM changed $ \k ->
      findScrollNodeByStoreKey ctx k >>= \case
        Nothing -> pure []
        Just idx -> do
          nt <- getNodeType (ctxNodeArena ctx) idx
          if not (isScrollNode nt)
            then pure []
            else do
              mClip <- getClipRect (ctxNodeArena ctx) idx
              mFloat <- floatingAncestorRect ctx idx
              pure (catMaybes [mClip, mFloat])

floatingAncestorRect :: Context -> Int -> IO (Maybe Rect)
floatingAncestorRect ctx idx =
  walkAncestors check (ctxNodeArena ctx) idx
  where
    check i = do
      nt <- getNodeType (ctxNodeArena ctx) i
      if isFloatingNode nt
        then do
          (x, y, w, h) <- getRect (ctxNodeArena ctx) i
          let r = Rect x y w h
          pure (if nonzeroRect r then Just r else Nothing)
        else pure Nothing

nonzeroRect :: Rect -> Bool
nonzeroRect r = rectW r > 0 && rectH r > 0

significantLayoutRect :: Rect -> Bool
significantLayoutRect r = rectArea r >= layoutSettleMinArea
