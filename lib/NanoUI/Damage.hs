module NanoUI.Damage
  ( updatePrevRects
  , floatingPanelRects
  , writeDamage
  ) where

import Control.Monad (foldM, forM, when)
import Data.IORef (readIORef, writeIORef)
import Data.IntMap.Strict qualified as IM
import Data.Maybe (catMaybes, isNothing)
import Data.Text qualified as T
import NanoUI.Context
  ( Context (..)
  , WidgetStore (..)
  , animInProgress
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
  , inputPointerHeld
  )
import NanoUI.Layout.Arena
  ( NodeType (..)
  , arenaCount
  , getNodeType
  , getRect
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
  [Int] ->
  IO ()
writeDamage ctx inp wasDirty overlayOpen oldSize oldStore oldHot oldActive oldFocus oldHotR oldActiveR oldFocusR oldFloatingRects oldRects animKeys = do
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
      floatingChanged = oldFloatingRects /= newFloatingRects
      windowLive =
        not (IM.null (storeWindow newStore))
          || not (IM.null (storeWindowSize newStore))
      moved = rectDeltas oldRects newRects
      animLive = not (IM.null liveAnims) || settled
      keysAppeared =
        not (IM.null oldRects)
          && not (IM.null (IM.difference newRects oldRects))
      layoutSettle =
        not (IM.null oldRects)
          && not (null moved)
          && not animLive
      paintOrphan = orphanAnim && animLive
      full =
        wasDirty
          || dirtyNow
          || sizeChanged
          || commanded
          || storePaintChanged
          || overlayOpen
          || modalFlip
          || floatingChanged
          || windowLive
          || paintOrphan
          || keysAppeared
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
  when modalFlip (markDirty ctx)
  when (floatingChanged && not (IM.null oldFloatingRects && not (IM.null newFloatingRects))) $
    markDirty ctx

paintStore :: WidgetStore -> WidgetStore
paintStore s = s {storeWindow = IM.empty, storeWindowSize = IM.empty}

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
