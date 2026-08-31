module NanoUI.Damage
  ( updatePrevRects
  , updatePrevNodeTexts
  , floatingPanelRects
  , writeDamage
  ) where

import Control.Monad (foldM, forM, when)
import Data.IORef (readIORef, writeIORef)
import Data.IntMap.Strict qualified as IM
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import NanoUI.Context
  ( Context (..)
  , WidgetStore (..)
  , animInProgress
  , clearDirty
  , getPrevRect
  , getPrevRectByKey
  , getStore
  , intKey
  , isDirty
  , markDirty
  )
import NanoUI.Context.Modal (modalDamageFlip)
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Input
  ( Input (..)
  , inputChars
  , inputKeys
  , inputKeysNull
  , inputMousePressed
  , inputMouseReleased
  , inputMouseRightPressed
  , inputMouseRightReleased
  , inputPointerHeld
  , inputScroll
  , inputWindowSize
  )
import NanoUI.Frame.Chrome (displayText, textInputFocused, textInputValue)
import NanoUI.Frame.Hit (findNodeByWidgetId)
import NanoUI.Host (isCellHost)
import NanoUI.Layout.Arena
  ( NodeArena
  , NodeType (..)
  , SizingTag (..)
  , arenaCount
  , getHeightSizing
  , getNodeType
  , getNodeValue
  , getParent
  , getRect
  , getText
  , getWidgetId
  , getWidthSizing
  , isScrollNode
  )
import NanoUI.WidgetText
  ( radioParseOption
  , sliderValueText
  , textInputFieldText
  )
import NanoUI.Types
  ( Damage (..)
  , Rect (..)
  , Size (..)
  , V2 (..)
  , rectArea
  , rectH
  , rectInflate
  , rectIntersect
  , rectUnion
  , rectW
  )

textClipSlop :: Float
textClipSlop = 4

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

findNodeByKey :: Context -> Int -> IO (Maybe Int)
findNodeByKey ctx k = do
  count <- arenaCount (ctxNodeArena ctx)
  go (count - 1)
  where
    go idx
      | idx < 0 = pure Nothing
      | otherwise = do
          w <- getWidgetId (ctxNodeArena ctx) idx
          if intKey w == k
            then pure (Just idx)
            else go (idx - 1)

backdropRectFromNode :: Context -> Int -> IO (Maybe Rect)
backdropRectFromNode ctx idx = do
  let na = ctxNodeArena ctx
  nt <- getNodeType na idx
  case nt of
    NodePanel -> nodeRect na idx
    NodeWindow -> nodeRect na idx
    NodeModal -> nodeRect na idx
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

updatePrevRects :: Context -> IO ()
updatePrevRects ctx = do
  count <- arenaCount (ctxNodeArena ctx)
  if count <= 0
    then writeIORef (ctxPrevRects ctx) IM.empty
    else do
      acc <- foldM add IM.empty [0 .. count - 1]
      writeIORef (ctxPrevRects ctx) acc
  where
    add m idx = do
      wid <- getWidgetId (ctxNodeArena ctx) idx
      (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
      if hashWidgetId wid == 0
        then pure m
        else
          let r = Rect x y w h
           in if nonzeroRect r
                then pure (IM.insert (intKey wid) r m)
                else pure m

updatePrevNodeTexts :: Context -> IO ()
updatePrevNodeTexts ctx = do
  count <- arenaCount (ctxNodeArena ctx)
  if count <= 0
    then writeIORef (ctxPrevNodeTexts ctx) IM.empty
    else do
      pairs <- collectPrevTextPairs ctx (count - 1) []
      writeIORef (ctxPrevNodeTexts ctx) (IM.fromListWith preferTextSnapshot pairs)

collectPrevTextPairs :: Context -> Int -> [(Int, Text)] -> IO [(Int, Text)]
collectPrevTextPairs ctx !idx acc
  | idx < 0 = pure acc
  | otherwise = do
      wid <- getWidgetId (ctxNodeArena ctx) idx
      (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
      if hashWidgetId wid == 0
        then collectPrevTextPairs ctx (idx - 1) acc
        else
          let r = Rect x y w h
           in if nonzeroRect r
                then do
                  nt <- getNodeType (ctxNodeArena ctx) idx
                  snap <- paintTextSnapshot ctx nt idx
                  collectPrevTextPairs ctx (idx - 1) ((intKey wid, snap) : acc)
                else collectPrevTextPairs ctx (idx - 1) acc

-- Painted text fingerprint, not packed node text. Text inputs draw from store;
-- radio/checkbox dots follow node value in pixel mode.
paintTextSnapshot :: Context -> NodeType -> Int -> IO Text
paintTextSnapshot ctx nt idx = do
  let terminal = isCellHost (ctxHostProfile ctx)
  case nt of
    NodeTextInput -> do
      lbl <- getText (ctxNodeArena ctx) idx
      value <- textInputValue ctx idx
      focus <- textInputFocused ctx idx
      pure (lbl <> "\US" <> textInputFieldText lbl value focus)
    NodeSlider
      | not terminal -> do
          lbl <- displayText ctx nt idx
          wid <- getWidgetId (ctxNodeArena ctx) idx
          store <- getStore ctx
          let val = IM.findWithDefault 0 (intKey wid) (storeSlider store)
          pure (lbl <> "\US" <> sliderValueText val)
      | otherwise -> displayText ctx nt idx
    NodeRadio
      | not terminal -> paintedMarkSnapshot ctx nt idx
      | otherwise -> displayText ctx nt idx
    NodeCheckbox
      | not terminal -> paintedMarkSnapshot ctx nt idx
      | otherwise -> displayText ctx nt idx
    _ -> displayText ctx nt idx

paintedMarkSnapshot :: Context -> NodeType -> Int -> IO Text
paintedMarkSnapshot ctx nt idx = do
  lbl <- displayText ctx nt idx
  val <- getNodeValue (ctxNodeArena ctx) idx
  pure (lbl <> "\US" <> T.pack (show (round val :: Int)))

floatingPanelsInOrder :: Context -> IO [(Int, Rect)]
floatingPanelsInOrder ctx = do
  n <- arenaCount (ctxNodeArena ctx)
  let go idx acc
        | idx >= n = pure (reverse acc)
        | otherwise = do
            nt <- getNodeType (ctxNodeArena ctx) idx
            if nt == NodeWindow || nt == NodeModal
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
  IM.IntMap Text ->
  IM.IntMap Text ->
  [Int] ->
  IO ()
writeDamage ctx inp wasDirty overlayOpen oldSize oldStore oldHot oldActive oldFocus oldHotR oldActiveR oldFocusR oldFloatingRects oldRects oldTexts newTexts animKeys = do
  let Size winW winH = inputWindowSize inp
      sizeChanged =
        oldSize /= Size 0 0 && oldSize /= Size winW winH
      commanded =
        inputPointerHeld inp
          || inputMousePressed inp
          || inputMouseReleased inp
          || inputMouseRightPressed inp
          || inputMouseRightReleased inp
          || inputScroll inp /= V2 0 0
          || not (inputKeysNull (inputKeys inp))
          || not (T.null (inputChars inp))
  newStore <- getStore ctx
  panels <- floatingPanelsInOrder ctx
  let newFloatingRects = IM.fromList panels
  newRects <- readIORef (ctxPrevRects ctx)
  modalFlip <- modalDamageFlip ctx
  dirtyNow <- isDirty ctx
  liveAnims <- IM.filter animInProgress <$> readIORef (ctxAnimations ctx)
  settled <- readIORef (ctxAnimSettled ctx)
  writeIORef (ctxAnimSettled ctx) False
  orphanAnim <-
    fmap or $
      forM (IM.keys liveAnims) $ \k ->
        isNothing <$> getPrevRectByKey ctx k
  winDragActive <- isJust <$> readIORef (ctxWindowDrag ctx)
  winResizeActive <- isJust <$> readIORef (ctxWindowResize ctx)
  let keyedMoved = keyedRectDeltas oldRects newRects
  moved <- mapM (clipDeltaToScrollViewport ctx newRects) keyedMoved
  let settledMoved = filter significantLayoutRect moved
      storePaintChanged = paintStore oldStore /= paintStore newStore
      -- Expand changes which rows exist. Selection is node value, not text.
      -- Either one must Full: retain clips would leave ghost children / old highlight.
      treeStoreChanged =
        storeTreeSelected oldStore /= storeTreeSelected newStore
          || storeTreeExpanded oldStore /= storeTreeExpanded newStore
      colorStoreChanged =
        storeColor oldStore /= storeColor newStore
          || storeColorHue oldStore /= storeColorHue newStore
          || storeColorSv oldStore /= storeColorSv newStore
      dragStoreChanged = storeColorDrag oldStore /= storeColorDrag newStore
      -- Color/drag live in store but paint via widget rects. Do not Full for those alone.
      colorClipOnly =
        not storePaintChanged && (colorStoreChanged || dragStoreChanged)
      floatingChanged = oldFloatingRects /= newFloatingRects
      windowKeys =
        IM.keys (IM.union (storeWindow newStore) (storeWindowSize newStore))
      windowLive = winDragActive || winResizeActive
      windowClipRs =
        catMaybes [IM.lookup k newFloatingRects | k <- windowKeys]
      scrollChanged = storeScroll oldStore /= storeScroll newStore
      animLive = not (IM.null liveAnims) || settled
      keysChanged =
        not (IM.null oldRects)
          && ( not (IM.null (IM.difference newRects oldRects))
                 || not (IM.null (IM.difference oldRects newRects))
             )
      layoutSettle =
        not (IM.null oldRects)
          && not (null settledMoved)
          && not animLive
          && not scrollChanged
      paintOrphan = orphanAnim && animLive
      textChanged = not (IM.null (textChangeKeys oldTexts newTexts))
      full =
        ((wasDirty || dirtyNow) && not colorClipOnly)
          || sizeChanged
          || commanded
          || storePaintChanged
          || treeStoreChanged
          || overlayOpen
          || modalFlip
          || floatingChanged
          || windowLive
          || paintOrphan
          || keysChanged
          || textChanged
          || layoutSettle
  dmg <-
    if full
      then pure DamageFull
      else do
        newHot <- readIORef (ctxHotId ctx)
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
            rs <-
              fmap concat $
                forM ids $ \wid ->
                  if hashWidgetId wid == 0
                    then pure []
                    else do
                      newR <- getPrevRect ctx wid
                      let rects = catMaybes [oldOf wid, newR]
                      fmap catMaybes $
                        forM rects $ \r ->
                          clipWidgetRect ctx newRects wid r
            let vanishedRs = IM.elems (IM.difference oldRects newRects)
                textRs = textChangeRects oldTexts newTexts oldRects newRects
                colorRs =
                  storeKeyChangeRects (storeColor oldStore) (storeColor newStore) oldRects newRects
                    ++ storeKeyChangeRects (storeColorHue oldStore) (storeColorHue newStore) oldRects newRects
                    ++ storeKeyChangeRects (storeColorSv oldStore) (storeColorSv newStore) oldRects newRects
                    ++ storeKeyChangeRects (storeColorDrag oldStore) (storeColorDrag newStore) oldRects newRects
                textStoreRs =
                  storeKeyChangeRects (storeText oldStore) (storeText newStore) oldRects newRects
                    ++ storeKeyChangeRects (storeCursor oldStore) (storeCursor newStore) oldRects newRects
                    ++ storeKeyChangeRects (storeSelAnchor oldStore) (storeSelAnchor newStore) oldRects newRects
                    ++ storeKeyChangeRects (storeSlider oldStore) (storeSlider newStore) oldRects newRects
                    ++ storeKeyChangeRects (storeSelect oldStore) (storeSelect newStore) oldRects newRects
                    ++ storeKeyChangeRects (storeCheckbox oldStore) (storeCheckbox newStore) oldRects newRects
            radioStoreRs <- radioStoreChangeRects ctx oldStore newStore oldRects newRects
            animRs <-
              fmap concat $
                forM clipKeys $ \k ->
                  if k == 0
                    then pure []
                    else
                      fmap catMaybes $
                        forM (catMaybes [IM.lookup k oldRects, IM.lookup k newRects]) $ \r ->
                          clipDeltaToScrollViewport ctx newRects (k, r) >>= \c ->
                            if nonzeroRect c then pure (Just c) else pure Nothing
            backdropRs0 <- backdropRectsForInteraction ctx ids clipKeys
            let backdropRs = map (clipRectToWindow winW winH) backdropRs0
            let layoutRs = settledMoved
                base =
                  unionRects
                    ( rs
                        ++ animRs
                        ++ backdropRs
                        ++ layoutRs
                        ++ vanishedRs
                        ++ textRs
                        ++ textStoreRs
                        ++ radioStoreRs
                        ++ colorRs
                        ++ windowClipRs
                        ++ floatingRectDamage oldFloatingRects newFloatingRects
                    )
                rawClip =
                  if rectW base <= 0 || rectH base <= 0
                    then Rect 0 0 0 0
                    else rectInflate textClipSlop base
                -- Retain texture is the window. A clip taller than that
                -- stretches on blit (visible at scroll top, not bottom).
                clip = fromMaybe (Rect 0 0 0 0) (rectIntersect rawClip (Rect 0 0 winW winH))
                winArea = winW * winH
            if (animLive && not (nonzeroRect clip))
                 || (winArea > 0 && rectArea clip > winArea * 0.5)
              then pure DamageFull
              else pure (DamageClip clip)
  writeIORef (ctxDamage ctx) dmg
  writeIORef (ctxLastWindowSize ctx) (Size winW winH)
  writeIORef (ctxPrevFloatingRects ctx) newFloatingRects
  writeIORef (ctxPrevFloatingOrder ctx) (map fst panels)
  -- Color/drag already clipped or covered by pointer-held. Drop leftover dirty Full.
  when colorClipOnly (clearDirty ctx)
  when modalFlip (markDirty ctx)
  when (floatingChanged && not (IM.null oldFloatingRects && not (IM.null newFloatingRects))) $
    markDirty ctx

-- Windows live in their own path. Color/drag clip to widget rects, not Full.
paintStore :: WidgetStore -> WidgetStore
paintStore s =
  s
    { storeWindow = IM.empty
    , storeWindowSize = IM.empty
    , storeColor = IM.empty
    , storeColorHue = IM.empty
    , storeColorSv = IM.empty
    , storeColorDrag = IM.empty
    , storeTreeSelected = IM.empty
    , storeTreeExpanded = IM.empty
    }

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
clipDeltaToScrollViewport ctx newRects (k, r) = do
  mVp <- scrollViewportForKey ctx k newRects
  pure $
    case mVp of
      Nothing -> r
      -- Off-screen scroll children must drop out. Keeping `r` makes the
      -- clip taller than the retain texture and the blit stretches.
      Just vp -> fromMaybe (Rect 0 0 0 0) (rectIntersect r vp)

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

scrollViewportForKey :: Context -> Int -> IM.IntMap Rect -> IO (Maybe Rect)
scrollViewportForKey ctx k newRects =
  findNodeByKey ctx k >>= \case
    Nothing -> pure Nothing
    Just idx -> scrollViewportForNode ctx idx newRects

scrollViewportForNode :: Context -> Int -> IM.IntMap Rect -> IO (Maybe Rect)
scrollViewportForNode ctx idx newRects = do
  mScroll <- findScrollAncestor (ctxNodeArena ctx) idx
  case mScroll of
    Nothing -> pure Nothing
    Just scrollIdx -> do
      wid <- getWidgetId (ctxNodeArena ctx) scrollIdx
      pure (IM.lookup (intKey wid) newRects)

findScrollAncestor :: NodeArena -> Int -> IO (Maybe Int)
findScrollAncestor na idx = do
  nt <- getNodeType na idx
  if isScrollNode nt
    then pure (Just idx)
    else do
      p <- getParent na idx
      if p < 0
        then pure Nothing
        else findScrollAncestor na p

nonzeroRect :: Rect -> Bool
nonzeroRect r = rectW r > 0 && rectH r > 0

textChangeKeys :: IM.IntMap Text -> IM.IntMap Text -> IM.IntMap ()
textChangeKeys old new
  | IM.null old = IM.empty
  | otherwise =
      IM.fromList
        [ (k, ())
        | (k, tNew) <- IM.toList new
        , case IM.lookup k old of
            Nothing -> True
            Just tOld -> tOld /= tNew
        ]

preferTextSnapshot :: Text -> Text -> Text
preferTextSnapshot _ b = b

significantLayoutRect :: Rect -> Bool
significantLayoutRect r = rectArea r >= layoutSettleMinArea

radioStoreChangeRects ::
  Context ->
  WidgetStore ->
  WidgetStore ->
  IM.IntMap Rect ->
  IM.IntMap Rect ->
  IO [Rect]
radioStoreChangeRects ctx oldStore newStore oldRects newRects = do
  let changed = storeChangeKeys (storeRadio oldStore) (storeRadio newStore)
  if IM.null changed
    then pure []
    else do
      count <- arenaCount (ctxNodeArena ctx)
      rs <-
        forM [0 .. count - 1] $ \idx -> do
          nt <- getNodeType (ctxNodeArena ctx) idx
          if nt /= NodeRadio
            then pure []
            else do
              txt <- getText (ctxNodeArena ctx) idx
              wid <- getWidgetId (ctxNodeArena ctx) idx
              let (groupKey, _, _) = radioParseOption txt
              if IM.member groupKey changed
                then
                  let k = intKey wid
                   in pure (catMaybes [IM.lookup k oldRects, IM.lookup k newRects])
                else pure []
      pure (concat rs)

textChangeRects ::
  IM.IntMap Text ->
  IM.IntMap Text ->
  IM.IntMap Rect ->
  IM.IntMap Rect ->
  [Rect]
textChangeRects old new oldRects newRects =
  storeKeyChangeRects old new oldRects newRects

storeKeyChangeRects ::
  Eq a =>
  IM.IntMap a ->
  IM.IntMap a ->
  IM.IntMap Rect ->
  IM.IntMap Rect ->
  [Rect]
storeKeyChangeRects old new oldRects newRects =
  catMaybes
    [ case (IM.lookup k oldRects, IM.lookup k newRects) of
        (Just r1, Just r2) -> Just (rectUnion r1 r2)
        (Nothing, Just r) -> Just r
        (Just r, Nothing) -> Just r
        (Nothing, Nothing) -> Nothing
    | k <- IM.keys (storeChangeKeys old new)
    ]

storeChangeKeys :: Eq a => IM.IntMap a -> IM.IntMap a -> IM.IntMap ()
storeChangeKeys old new =
  IM.fromList
    [ (k, ())
    | k <- IM.keys (IM.union old new)
    , IM.lookup k old /= IM.lookup k new
    ]
