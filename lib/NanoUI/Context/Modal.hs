module NanoUI.Context.Modal
  ( textInputEditActive
  , modalActive
  , overlayConsumesQuit
  , markEscapeConsumed
  , pointerBlockedByModal
  , pointerBlockedByOverlay
  , seedFloatingPanel
  , beginModal
  , endModal
  , beginFrameModal
  , modalDamageFlip
  ) where

import Data.IORef (readIORef, writeIORef)
import qualified Data.IntMap.Strict as IM
import Data.List (nub)
import NanoUI.Context.Internal (Context (..), intKey)
import NanoUI.Context.PrevRects (getPrevRectByKey)
import NanoUI.Context.Store (getStore)
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Input (Input (..), Key (KeyEscape), inputKeys, inputKeysElem)
import NanoUI.Store (WidgetStore (..))
import NanoUI.Types (Rect (..), V2 (..), rectContains, rectH, rectW)

textInputEditActive :: Context -> IO Bool
textInputEditActive ctx = do
  focus <- readIORef (ctxFocusId ctx)
  menu <- readIORef (ctxTextInputMenu ctx)
  pure (hashWidgetId focus /= 0 || menu /= Nothing)

modalActive :: Context -> IO Bool
modalActive ctx = do
  was <- readIORef (ctxModalWasActive ctx)
  now <- readIORef (ctxModalActive ctx)
  pure (was || now)

-- | True after this frame's UI consumed Escape (modal dismiss or text menu).
-- Call after 'runFrame'.
overlayConsumesQuit :: Context -> Input -> IO Bool
overlayConsumesQuit ctx inp = do
  consumed <- readIORef (ctxEscapeConsumed ctx)
  let esc = inputKeysElem KeyEscape (inputKeys inp)
  pure (esc && consumed)

markEscapeConsumed :: Context -> IO ()
markEscapeConsumed ctx = writeIORef (ctxEscapeConsumed ctx) True

pointerBlockedByModal :: Context -> IO Bool
pointerBlockedByModal ctx = do
  depth <- readIORef (ctxModalDepth ctx)
  if depth > 0
    then pure False
    else modalActive ctx

-- | Page widgets under a modal, or under the topmost floating rect at the mouse.
-- Only children of that topmost panel keep the pointer.
pointerBlockedByOverlay :: Context -> V2 -> IO Bool
pointerBlockedByOverlay ctx mouse = do
  modalBlocked <- pointerBlockedByModal ctx
  blocked <-
    if modalBlocked
      then pure True
      else do
        mTop <- cachedTopmost ctx mouse
        case mTop of
          Nothing -> pure False
          Just top -> do
            mCur <- readIORef (ctxCurrentFloatingId ctx)
            pure (mCur /= Just top)
  writeIORef (ctxLastPointerBlocked ctx) blocked
  pure blocked

cachedTopmost :: Context -> V2 -> IO (Maybe WidgetId)
cachedTopmost ctx mouse = do
  cache <- readIORef (ctxOverlayTopmostCache ctx)
  case cache of
    Just (p, t) | p == mouse -> pure t
    _ -> do
      t <- topmostFloatingAtMouse ctx mouse
      writeIORef (ctxOverlayTopmostCache ctx) (Just (mouse, t))
      pure t

topmostFloatingAtMouse :: Context -> V2 -> IO (Maybe WidgetId)
topmostFloatingAtMouse ctx mouse = do
  rects <- readIORef (ctxPrevFloatingRects ctx)
  order <- readIORef (ctxPrevFloatingOrder ctx)
  if IM.null rects && null order
    then pure Nothing
    else
      let hit k =
            case IM.lookup k rects of
              Just r | rectW r > 0 && rectH r > 0 && rectContains r mouse -> True
              _ -> False
          picked = foldl' (\acc k -> if hit k then Just k else acc) Nothing order
       in case picked of
            Just k -> pure (Just (WidgetId (fromIntegral k)))
            Nothing -> pure Nothing

-- | Record a panel box as topmost so later same-frame widgets see it.
seedFloatingPanel :: Context -> WidgetId -> Rect -> IO ()
seedFloatingPanel ctx wid rect
  | rectW rect <= 0 || rectH rect <= 0 = pure ()
  | otherwise = do
      let k = intKey wid
      rects <- readIORef (ctxPrevFloatingRects ctx)
      writeIORef (ctxPrevFloatingRects ctx) (IM.insert k rect rects)
      order <- readIORef (ctxPrevFloatingOrder ctx)
      writeIORef (ctxPrevFloatingOrder ctx) (filter (/= k) order ++ [k])
      writeIORef (ctxOverlayTopmostCache ctx) Nothing

seedFloatingFromStore :: Context -> IO ()
seedFloatingFromStore ctx = do
  store <- getStore ctx
  let keys = nub (IM.keys (storeWindow store) ++ IM.keys (storeWindowSize store))
  mapM_ (seedStoreKey ctx store) keys

seedStoreKey :: Context -> WidgetStore -> Int -> IO ()
seedStoreKey ctx store k = do
  rects <- readIORef (ctxPrevFloatingRects ctx)
  case IM.lookup k rects of
    Just r | rectW r > 0 && rectH r > 0 -> pure ()
    _ -> do
      mPrev <- getPrevRectByKey ctx k
      let fromStore =
            case (IM.lookup k (storeWindow store), IM.lookup k (storeWindowSize store)) of
              (Just (x, y), Just (w, h)) | w > 0 && h > 0 -> Just (Rect x y w h)
              _ -> Nothing
          picked =
            case mPrev of
              Just r | rectW r > 0 && rectH r > 0 -> Just r
              _ -> fromStore
      case picked of
        Just r -> seedFloatingPanel ctx (WidgetId (fromIntegral k)) r
        Nothing -> pure ()

beginModal :: Context -> IO ()
beginModal ctx = do
  writeIORef (ctxModalActive ctx) True
  depth <- readIORef (ctxModalDepth ctx)
  writeIORef (ctxModalDepth ctx) (depth + 1)

endModal :: Context -> IO ()
endModal ctx = do
  depth <- readIORef (ctxModalDepth ctx)
  writeIORef (ctxModalDepth ctx) (max 0 (depth - 1))

-- | Start-of-frame modal bookkeeping. Saves last frame's open flag, then clears
-- live refs before UI runs 'beginModal' again.
beginFrameModal :: Context -> IO ()
beginFrameModal ctx = do
  modalNow <- readIORef (ctxModalActive ctx)
  writeIORef (ctxModalWasActive ctx) modalNow
  writeIORef (ctxModalActive ctx) False
  writeIORef (ctxModalDepth ctx) 0
  writeIORef (ctxOverlayTopmostCache ctx) Nothing
  writeIORef (ctxCurrentFloatingId ctx) Nothing
  writeIORef (ctxLastPointerBlocked ctx) False
  seedFloatingFromStore ctx

-- | True when modal presence changed this frame (open or close).
modalDamageFlip :: Context -> IO Bool
modalDamageFlip ctx = do
  was <- readIORef (ctxModalWasActive ctx)
  now <- readIORef (ctxModalActive ctx)
  pure (was /= now)
