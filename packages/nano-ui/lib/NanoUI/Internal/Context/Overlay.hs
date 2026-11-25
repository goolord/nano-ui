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
import NanoUI.Internal.Context.Types (Context (..), InteractionState (..), OverlayState (..), PointerRoute (..))
import NanoUI.Internal.Id (hashWidgetId)
import NanoUI.Internal.Input (DropEvent (..), DropType (..), Input (..), Key (KeyEscape), Pressable (..), offWindow, withoutPointer)
import NanoUI.Internal.Store (anySelectOpen)
import NanoUI.Internal.Types (V2, rectContains)

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
-- ('markEscapeConsumed'), or took its press and it is still down or just
-- released, or an open text-edit menu, an open dropdown or a focused combo
-- box's list ('isFocusCombo') closes on it. The view's Escape consumers ask
-- this, so none acts on the Escape such a menu takes, wherever it is
-- declared.
escapeReserved :: Context -> IO Bool
escapeReserved ctx = do
  taken <- getsOverlay ctx osEscapeConsumed
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
-- routed to @layer@ and not blocked by a modal. An OS drag moves no pointer,
-- so its drops go by where they land: a layer that is not on top there, or
-- that a modal blocks, sees the drag off the window.
routedInput :: Context -> Int -> Input -> IO Input
routedInput ctx layer inp = do
  routed <-
    getsInteraction ctx isPointerRoute >>= \case
      RouteLayer routed | routed == layer -> unlessBlocked
      RouteDropdown routed | routed == layer -> unlessBlocked
      _ -> pure (withoutPointer inp)
  if null (inputDrops inp)
    then pure routed
    else do
      blocked <- pointerBlockedByModal ctx
      let own ev = case dropEventPos ev of
            Just p | dropEventType ev == DropPosition -> do
              top <- floatingLayerAt ctx p
              pure (if blocked || top /= layer then ev {dropEventPos = Just offWindow} else ev)
            _ -> pure ev
      drops <- traverse own (inputDrops inp)
      pure routed {inputDrops = drops}
  where
    unlessBlocked = do
      blocked <- pointerBlockedByModal ctx
      pure (if blocked then withoutPointer inp else inp)

-- | The layer on top at @mouse@, going by where the floating panels were last
-- frame: the last panel declared that holds the point, or the page.
floatingLayerAt :: Context -> V2 -> IO Int
floatingLayerAt ctx mouse = do
  os <- readIORef (ctxOverlayState ctx)
  let rects = osPrevFloatingRects os
      hit k = maybe False (`rectContains` mouse) (IM.lookup k rects)
  pure (foldl' (\acc k -> if hit k then k else acc) 0 (osPrevFloatingOrder os))

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
