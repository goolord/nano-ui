-- | Modal and floating-panel state, and which layer the pointer reaches.
module NanoUI.Internal.Context.Overlay
  ( textInputEditActive
  , modalActive
  , overlayConsumesQuit
  , escapeReserved
  , markEscapeConsumed
  , markTabConsumed
  , tabConsumed
  , pointerBlockedByModal
  , routedInput
  , floatingLayerAt
  , seedFloatingPanel
  , beginModal
  , endModal
  , beginFrameModal
  , modalDamageFlip
  , takeKeyPress
  ) where

import Data.IORef (readIORef)
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.Maybe (isJust)

import NanoUI.Internal.Context.Core
import NanoUI.Internal.Context.Types (Context (..), InteractionState (..), OverlayState (..), PointerRoute (..), intKey)
import NanoUI.Internal.Id (WidgetId (..), hashWidgetId)
import NanoUI.Internal.Input (Input, Key (KeyEscape), Pressable (..), withoutPointer)
import NanoUI.Internal.Store (anySelectOpen)
import NanoUI.Internal.Types (Rect, V2, rectContains, rectNonEmpty)

-- | Whether any widget has keyboard focus or a text-edit menu is open.
-- This checks the focus id, not the focused node's type.
textInputEditActive :: Context -> IO Bool
textInputEditActive ctx = do
  focus <- readIORef (ctxFocusId ctx)
  menu <- getsInteraction ctx isTextInputMenu
  pure (hashWidgetId focus /= 0 || isJust menu)

-- | Whether a modal is declared this frame or was active on the previous frame.
modalActive :: Context -> IO Bool
modalActive ctx = getsOverlay ctx (\os -> osModalWasActive os || osModalActive os)

-- | Whether this input contains Escape already consumed by an overlay.
overlayConsumesQuit :: Context -> Input -> IO Bool
overlayConsumesQuit ctx inp = do
  consumed <- getsOverlay ctx osEscapeConsumed
  pure (pressedOnceIn KeyEscape inp && consumed)

-- | Whether this frame's Escape is spoken for: something took it already
-- ('overlayConsumesQuit'), or an open text-edit menu, an open dropdown or a
-- focused combo box's list ('isFocusCombo') closes on it. The view's Escape
-- consumers ask this, so none acts on the Escape such a menu takes, wherever
-- it is declared.
escapeReserved :: Context -> Input -> IO Bool
escapeReserved ctx inp = do
  taken <- overlayConsumesQuit ctx inp
  menuOrCombo <- getsInteraction ctx (\s -> isJust (isTextInputMenu s) || isFocusCombo s)
  dropdown <- anySelectOpen <$> getStore ctx
  pure (taken || menuOrCombo || dropdown)

-- | Mark Escape as handled so closing an overlay does not also quit the app.
markEscapeConsumed :: Context -> IO ()
markEscapeConsumed ctx = modifyOverlay ctx (\os -> os {osEscapeConsumed = True})

-- | Keep this frame's Tab from moving focus: the widget holding the keyboard
-- acts on it itself.
markTabConsumed :: Context -> IO ()
markTabConsumed ctx = modifyOverlay ctx (\os -> os {osTabConsumed = True})

-- | Whether something took this frame's Tab with 'markTabConsumed'.
tabConsumed :: Context -> IO Bool
tabConsumed ctx = getsOverlay ctx osTabConsumed

-- | Whether a modal is up and the view being declared is outside it.
pointerBlockedByModal :: Context -> IO Bool
pointerBlockedByModal ctx =
  getsOverlay ctx (\os -> osModalDepth os <= 0 && (osModalWasActive os || osModalActive os))

-- | Filter frame input for widgets in @layer@ (0 for the base page, or a
-- floating panel's key): preserves pointer state only when pointer input is
-- routed to @layer@ and not blocked by a modal.
routedInput :: Context -> Int -> Input -> IO Input
routedInput ctx layer inp =
  getsInteraction ctx isPointerRoute >>= \case
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
      hit k = maybe False (`rectContains` mouse) (IM.lookup k rects)
  pure (foldl' (\acc k -> if hit k then k else acc) 0 (osPrevFloatingOrder os))

-- | Record known panel bounds at the front of the floating hit order before
-- the next layout pass. Empty bounds are ignored.
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

-- | Increment modal nesting depth and mark a modal active for this frame.
beginModal :: Context -> IO ()
beginModal ctx =
  modifyOverlay ctx (\os -> os {osModalActive = True, osModalDepth = osModalDepth os + 1})

-- | Leave modal construction. Decreases depth without clearing the frame's active flag.
endModal :: Context -> IO ()
endModal ctx =
  modifyOverlay ctx (\os -> os {osModalDepth = max 0 (osModalDepth os - 1)})

-- | Save the previous modal flag, then clear current depth and Escape, Tab
-- and key consumption.
beginFrameModal :: Context -> IO ()
beginFrameModal ctx =
  modifyOverlay ctx $ \os ->
    os
      { osModalWasActive = osModalActive os
      , osModalActive = False
      , osModalDepth = 0
      , osEscapeConsumed = False
      , osTabConsumed = False
      , osKeysTaken = IS.empty
      }

-- | Claim the key press at index @i@ of this frame's keys for a shortcut.
-- Returns 'False' if another shortcut already claimed it.
takeKeyPress :: Context -> Int -> IO Bool
takeKeyPress ctx i = do
  taken <- getsOverlay ctx (IS.member i . osKeysTaken)
  if taken
    then pure False
    else True <$ modifyOverlay ctx (\os -> os {osKeysTaken = IS.insert i (osKeysTaken os)})

-- | Whether modal presence changed since the preceding frame.
modalDamageFlip :: Context -> IO Bool
modalDamageFlip ctx = getsOverlay ctx (\os -> osModalWasActive os /= osModalActive os)
