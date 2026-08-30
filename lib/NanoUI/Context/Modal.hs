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
import NanoUI.Context.Internal (Context (..), intKey)
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Input (Input (..), Key (KeyEscape), inputKeys, inputKeysElem)
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
-- Floating hit rects stay from last frame's writeDamage / live 'seedFloatingPanel'
-- calls. Do not re-seed closed windows from storeWindow (that left phantom hit
-- rects over the page after Debug/About closed).
beginFrameModal :: Context -> IO ()
beginFrameModal ctx = do
  modalNow <- readIORef (ctxModalActive ctx)
  writeIORef (ctxModalWasActive ctx) modalNow
  writeIORef (ctxModalActive ctx) False
  writeIORef (ctxModalDepth ctx) 0
  writeIORef (ctxOverlayTopmostCache ctx) Nothing
  writeIORef (ctxCurrentFloatingId ctx) Nothing
  writeIORef (ctxLastPointerBlocked ctx) False

-- | True when modal presence changed this frame (open or close).
modalDamageFlip :: Context -> IO Bool
modalDamageFlip ctx = do
  was <- readIORef (ctxModalWasActive ctx)
  now <- readIORef (ctxModalActive ctx)
  pure (was /= now)
