-- | Modal, floating-panel and menu pointer-capture state.
module NanoUI.Context.Overlay
  ( textInputEditActive
  , modalActive
  , overlayConsumesQuit
  , markEscapeConsumed
  , pointerBlockedByModal
  , pointerBlockedByOverlay
  , armMenuPointerCapture
  , seedFloatingPanel
  , beginModal
  , endModal
  , beginFrameModal
  , modalDamageFlip
  ) where

import Control.Monad (when)
import Data.IORef (readIORef)
import Data.IntMap.Strict qualified as IM

import NanoUI.Context.Core
  ( getMenuPointerGesture
  , getTextInputMenu
  , getsOverlay
  , modifyOverlay
  , setMenuPointerGesture
  , getsInteraction
  )
import NanoUI.Context.Types (Context (..), OverlayState (..), TextInputMenu (..), intKey, InteractionState (..))
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Input (Input, Key (KeyEscape), inputKeys, inputKeysElem, inputMousePos, inputMousePressed)
import NanoUI.Types (Rect, V2, rectContains, rectHit, rectNonEmpty)

textInputEditActive :: Context -> IO Bool
textInputEditActive ctx = do
  focus <- readIORef (ctxFocusId ctx)
  menu <- getTextInputMenu ctx
  pure (hashWidgetId focus /= 0 || menu /= Nothing)

modalActive :: Context -> IO Bool
modalActive ctx = getsOverlay ctx (\os -> osModalWasActive os || osModalActive os)

overlayConsumesQuit :: Context -> Input -> IO Bool
overlayConsumesQuit ctx inp = do
  consumed <- getsOverlay ctx osEscapeConsumed
  pure (inputKeysElem KeyEscape (inputKeys inp) && consumed)

markEscapeConsumed :: Context -> IO ()
markEscapeConsumed ctx = modifyOverlay ctx (\os -> os {osEscapeConsumed = True})

pointerBlockedByModal :: Context -> IO Bool
pointerBlockedByModal ctx =
  getsOverlay ctx (\os -> osModalDepth os <= 0 && (osModalWasActive os || osModalActive os))

pointerBlockedByOverlay :: Context -> V2 -> IO Bool
pointerBlockedByOverlay ctx mouse = do
  gesture <- getMenuPointerGesture ctx
  blocked <-
    if gesture
      then pure True
      else do
        menuBlocked <- overlayMenuBlocksPointer ctx mouse
        if menuBlocked
          then pure True
          else do
            modalBlocked <- pointerBlockedByModal ctx
            if modalBlocked
              then pure True
              else do
                mTop <- cachedTopmost ctx mouse
                case mTop of
                  Nothing -> pure False
                  Just top -> do
                    mCur <- getsOverlay ctx osCurrentFloatingId
                    pure (mCur /= Just top)
  modifyOverlay ctx (\os -> os {osLastPointerBlocked = blocked})
  pure blocked

armMenuPointerCapture :: Context -> Input -> IO ()
armMenuPointerCapture ctx inp =
  when (inputMousePressed inp) $ do
    blocked <- overlayMenuBlocksPointer ctx (inputMousePos inp)
    setMenuPointerGesture ctx blocked

overlayMenuBlocksPointer :: Context -> V2 -> IO Bool
overlayMenuBlocksPointer ctx mouse = do
  mMenu <- getTextInputMenu ctx
  let textMenu =
        case mMenu of
          Just m | rectContains (textInputMenuRect m) mouse -> True
          _ -> False
  if textMenu
    then pure True
    else do
      mDrop <- getsInteraction ctx isOpenSelectDrop
      pure
        ( case mDrop of
            Just (_, r) -> rectContains r mouse
            Nothing -> False
        )

cachedTopmost :: Context -> V2 -> IO (Maybe WidgetId)
cachedTopmost ctx mouse = do
  cache <- getsOverlay ctx osTopmostCache
  case cache of
    Just (p, t) | p == mouse -> pure t
    _ -> do
      t <- topmostFloatingAtMouse ctx mouse
      modifyOverlay ctx (\os -> os {osTopmostCache = Just (mouse, t)})
      pure t

topmostFloatingAtMouse :: Context -> V2 -> IO (Maybe WidgetId)
topmostFloatingAtMouse ctx mouse = do
  os <- readIORef (ctxOverlayState ctx)
  let rects = osPrevFloatingRects os
      order = osPrevFloatingOrder os
      hit k = maybe False (`rectHit` mouse) (IM.lookup k rects)
      picked = foldl' (\acc k -> if hit k then Just k else acc) Nothing order
  pure (WidgetId . fromIntegral <$> picked)

seedFloatingPanel :: Context -> WidgetId -> Rect -> IO ()
seedFloatingPanel ctx wid rect
  | not (rectNonEmpty rect) = pure ()
  | otherwise = do
      let k = intKey wid
      modifyOverlay ctx $ \os ->
        let rects = IM.insert k rect (osPrevFloatingRects os)
            order = filter (/= k) (osPrevFloatingOrder os) ++ [k]
         in os
              { osPrevFloatingRects = rects
              , osPrevFloatingOrder = order
              , osTopmostCache = Nothing
              }

beginModal :: Context -> IO ()
beginModal ctx =
  modifyOverlay ctx (\os -> os {osModalActive = True, osModalDepth = osModalDepth os + 1})

endModal :: Context -> IO ()
endModal ctx =
  modifyOverlay ctx (\os -> os {osModalDepth = max 0 (osModalDepth os - 1)})

beginFrameModal :: Context -> IO ()
beginFrameModal ctx =
  modifyOverlay ctx $ \os ->
    os
      { osModalWasActive = osModalActive os
      , osModalActive = False
      , osModalDepth = 0
      , osTopmostCache = Nothing
      , osCurrentFloatingId = Nothing
      , osLastPointerBlocked = False
      , osEscapeConsumed = False
      }

modalDamageFlip :: Context -> IO Bool
modalDamageFlip ctx = getsOverlay ctx (\os -> osModalWasActive os /= osModalActive os)
