module NanoUI.Context.Modal
  ( textInputEditActive
  , modalActive
  , overlayConsumesQuit
  , markEscapeConsumed
  , pointerBlockedByModal
  , beginModal
  , endModal
  , beginFrameModal
  , modalDamageFlip
  ) where

import Data.IORef (readIORef, writeIORef)
import NanoUI.Context.Internal (Context (..))
import NanoUI.Id (hashWidgetId)
import NanoUI.Input (Input (..), Key (KeyEscape))

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
  let esc = KeyEscape `elem` inputKeys inp
  pure (esc && consumed)

markEscapeConsumed :: Context -> IO ()
markEscapeConsumed ctx = writeIORef (ctxEscapeConsumed ctx) True

pointerBlockedByModal :: Context -> IO Bool
pointerBlockedByModal ctx = do
  depth <- readIORef (ctxModalDepth ctx)
  if depth > 0
    then pure False
    else modalActive ctx

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

-- | True when modal presence changed this frame (open or close).
modalDamageFlip :: Context -> IO Bool
modalDamageFlip ctx = do
  was <- readIORef (ctxModalWasActive ctx)
  now <- readIORef (ctxModalActive ctx)
  pure (was /= now)
