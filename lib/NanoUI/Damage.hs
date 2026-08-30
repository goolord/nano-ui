module NanoUI.Damage
  ( updatePrevRects
  , updatePrevNodeTexts
  , floatingPanelRects
  , writeDamage
  ) where

import Control.Monad (foldM, forM, when)
import Data.IORef (readIORef, writeIORef)
import Data.IntMap.Strict qualified as IM
import Data.Maybe (catMaybes, isNothing)
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
import NanoUI.Layout.Arena
  ( NodeType (..)
  , arenaCount
  , getNodeType
  , getRect
  , getText
  , getWidgetId
  )
import NanoUI.Types
  ( Damage (..)
  , Rect (..)
  , Size (..)
  , V2 (..)
  , rectArea
  , rectH
  , rectInflate
  , rectUnion
  , rectW
  )

textClipSlop :: Float
textClipSlop = 4

updatePrevRects :: Context -> IO ()
updatePrevRects ctx = do
  count <- arenaCount (ctxNodeArena ctx)
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
                then pure (IM.insertWith rectUnion (intKey wid) r m)
                else pure m

updatePrevNodeTexts :: Context -> IO ()
updatePrevNodeTexts ctx = do
  count <- arenaCount (ctxNodeArena ctx)
  acc <- foldM add IM.empty [0 .. count - 1]
  writeIORef (ctxPrevNodeTexts ctx) acc
  where
    add m idx = do
      wid <- getWidgetId (ctxNodeArena ctx) idx
      (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
      if hashWidgetId wid == 0
        then pure m
        else
          let r = Rect x y w h
           in if nonzeroRect r
                then do
                  txt <- getText (ctxNodeArena ctx) idx
                  pure (IM.insert (intKey wid) txt m)
                else pure m

floatingPanelRects :: Context -> IO (IM.IntMap Rect)
floatingPanelRects ctx = do
  n <- arenaCount (ctxNodeArena ctx)
  let go idx acc
        | idx >= n = pure acc
        | otherwise = do
            nt <- getNodeType (ctxNodeArena ctx) idx
            if nt == NodeWindow || nt == NodeModal
              then do
                wid <- getWidgetId (ctxNodeArena ctx) idx
                (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
                if hashWidgetId wid == 0
                  then go (idx + 1) acc
                  else go (idx + 1) (IM.insert (intKey wid) (Rect x y w h) acc)
              else go (idx + 1) acc
  go 0 IM.empty

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
  newFloatingRects <- floatingPanelRects ctx
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
  let storePaintChanged = paintStore oldStore /= paintStore newStore
      colorStoreChanged =
        storeColor oldStore /= storeColor newStore
          || storeColorHue oldStore /= storeColorHue newStore
          || storeColorSv oldStore /= storeColorSv newStore
      dragStoreChanged = storeColorDrag oldStore /= storeColorDrag newStore
      -- Color/drag live in store but paint via widget rects. Do not Full for those alone.
      colorClipOnly =
        not storePaintChanged && (colorStoreChanged || dragStoreChanged)
      floatingChanged = oldFloatingRects /= newFloatingRects
      windowLive =
        not (IM.null (storeWindow newStore))
          || not (IM.null (storeWindowSize newStore))
      moved = rectDeltas oldRects newRects
      animLive = not (IM.null liveAnims) || settled
      keysChanged =
        not (IM.null oldRects)
          && ( not (IM.null (IM.difference newRects oldRects))
                 || not (IM.null (IM.difference oldRects newRects))
             )
      layoutSettle =
        not (IM.null oldRects)
          && not (null moved)
          && not animLive
      paintOrphan = orphanAnim && animLive
      textChanged = not (IM.null (textChangeKeys oldTexts newTexts))
      full =
        ((wasDirty || dirtyNow) && not colorClipOnly)
          || sizeChanged
          || commanded
          || storePaintChanged
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
                      pure (catMaybes [oldOf wid, newR])
            let vanishedRs = IM.elems (IM.difference oldRects newRects)
                textRs = textChangeRects oldTexts newTexts oldRects newRects
                colorRs =
                  storeKeyChangeRects (storeColor oldStore) (storeColor newStore) oldRects newRects
                    ++ storeKeyChangeRects (storeColorHue oldStore) (storeColorHue newStore) oldRects newRects
                    ++ storeKeyChangeRects (storeColorSv oldStore) (storeColorSv newStore) oldRects newRects
                    ++ storeKeyChangeRects (storeColorDrag oldStore) (storeColorDrag newStore) oldRects newRects
            animRs <-
              fmap concat $
                forM clipKeys $ \k ->
                  if k == 0
                    then pure []
                    else pure (catMaybes [IM.lookup k oldRects, IM.lookup k newRects])
            let layoutRs = if animLive then moved else []
                base =
                  unionRects
                    ( rs
                        ++ animRs
                        ++ layoutRs
                        ++ vanishedRs
                        ++ textRs
                        ++ colorRs
                        ++ floatingRectDamage oldFloatingRects newFloatingRects
                    )
                clip =
                  if rectW base <= 0 || rectH base <= 0
                    then Rect 0 0 0 0
                    else rectInflate textClipSlop base
                winArea = winW * winH
            if (animLive && not (nonzeroRect clip))
                 || (winArea > 0 && rectArea clip > winArea * 0.5)
              then pure DamageFull
              else pure (DamageClip clip)
  writeIORef (ctxDamage ctx) dmg
  writeIORef (ctxLastWindowSize ctx) (Size winW winH)
  writeIORef (ctxPrevFloatingRects ctx) newFloatingRects
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

rectDeltas :: IM.IntMap Rect -> IM.IntMap Rect -> [Rect]
rectDeltas old new =
  filter nonzeroRect $
    IM.elems (IM.difference old new)
      ++ IM.elems (IM.difference new old)
      ++ IM.elems (IM.mapMaybe id (IM.intersectionWith delta old new))
  where
    delta a b
      | a == b = Nothing
      | otherwise = Just (rectUnion a b)

nonzeroRect :: Rect -> Bool
nonzeroRect r = rectW r > 0 && rectH r > 0

textChangeKeys :: IM.IntMap Text -> IM.IntMap Text -> IM.IntMap ()
textChangeKeys old new =
  IM.fromList
    [ (k, ())
    | (k, tNew) <- IM.toList new
    , Just tOld <- [IM.lookup k old]
    , tOld /= tNew
    ]

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
