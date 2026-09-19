-- | Modal and floating-panel state, and which layer the pointer reaches.
module NanoUI.Context.Overlay
  ( textInputEditActive
  , modalActive
  , overlayConsumesQuit
  , markEscapeConsumed
  , pointerBlockedByModal
  , routedInput
  , floatingLayerAt
  , seedFloatingPanel
  , beginModal
  , endModal
  , beginFrameModal
  , modalDamageFlip
  ) where

import Data.IORef (readIORef)
import Data.IntMap.Strict qualified as IM

import NanoUI.Context.Core
  ( getPointerRoute
  , getTextInputMenu
  , getsOverlay
  , modifyOverlay
  )
import NanoUI.Context.Types (Context (..), OverlayState (..), PointerRoute (..), intKey)
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Input (Input, Key (KeyEscape), inputKeys, inputKeysElem, withoutPointer)
import NanoUI.Types (Rect, V2, rectHit, rectNonEmpty)

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

-- | Whether a modal is up and the view being declared is outside it.
pointerBlockedByModal :: Context -> IO Bool
pointerBlockedByModal ctx =
  getsOverlay ctx (\os -> osModalDepth os <= 0 && (osModalWasActive os || osModalActive os))

-- | The frame's input as the widgets being declared in @layer@ see it (0 for
-- the page, a floating panel's key otherwise): as it is when the frame routed
-- the pointer there and no modal stands in front, and with no pointer in it
-- otherwise.
routedInput :: Context -> Int -> Input -> IO Input
routedInput ctx layer inp =
  getPointerRoute ctx >>= \case
    RouteLayer routed | routed == layer -> do
      blocked <- pointerBlockedByModal ctx
      pure (if blocked then withoutPointer inp else inp)
    _ -> pure (withoutPointer inp)

-- | The layer on top at @mouse@, going by where the floating panels were last
-- frame: the last panel declared that holds the point, or the page.
floatingLayerAt :: Context -> V2 -> IO Int
floatingLayerAt ctx mouse = do
  os <- readIORef (ctxOverlayState ctx)
  let rects = osPrevFloatingRects os
      hit k = maybe False (`rectHit` mouse) (IM.lookup k rects)
  pure (foldl' (\acc k -> if hit k then k else acc) 0 (osPrevFloatingOrder os))

seedFloatingPanel :: Context -> WidgetId -> Rect -> IO ()
seedFloatingPanel ctx wid rect
  | not (rectNonEmpty rect) = pure ()
  | otherwise = do
      let k = intKey wid
      modifyOverlay ctx $ \os ->
        os
          { osPrevFloatingRects = IM.insert k rect (osPrevFloatingRects os)
          , osPrevFloatingOrder = filter (/= k) (osPrevFloatingOrder os) ++ [k]
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
      , osEscapeConsumed = False
      }

modalDamageFlip :: Context -> IO Bool
modalDamageFlip ctx = getsOverlay ctx (\os -> osModalWasActive os /= osModalActive os)
