module NanoUI.Sdl.Cursor
  ( SdlCursors (..)
  , initCursors
  , destroyCursors
  , syncPointerCursor
  ) where

import Control.Monad (void, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word32)
import Foreign.C.Types (CUInt (..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peekByteOff)
import NanoUI (Context, Input (..), UiCursorKind (..), uiCursorKind)
import qualified SDL3.Sys.Bindgen.Mouse as Mouse
import SDL3.Sys.Mouse
  ( createSystemCursorSafe
  , destroyCursorSafe
  , getDefaultCursorSafe
  , setCursorSafe
  )

-- SDL 3.2+ SDL_SystemCursor indices (SDL_mouse.h); bindgen exports only 0–19.
sdlSystemCursorGrabIdx, sdlSystemCursorGrabbingIdx :: Word32
sdlSystemCursorGrabIdx = 27
sdlSystemCursorGrabbingIdx = 28

foreign import ccall unsafe "SDL_GetVersion"
  c_SDL_GetVersion :: Ptr Word32 -> IO ()

foreign import ccall unsafe "SDL_CreateSystemCursor"
  c_SDL_CreateSystemCursor :: CUInt -> IO (Ptr Mouse.SDL_Cursor)

sdlVersionAtLeast :: Word32 -> Word32 -> IO Bool
sdlVersionAtLeast wantMajor wantMinor =
  allocaBytes 12 $ \ptr -> do
    c_SDL_GetVersion ptr
    major <- peekByteOff ptr 0
    minor <- peekByteOff ptr 4
    pure $
      major > wantMajor
        || (major == wantMajor && minor >= wantMinor)

createExtendedSystemCursor :: Word32 -> IO (Ptr Mouse.SDL_Cursor)
createExtendedSystemCursor idx = c_SDL_CreateSystemCursor (fromIntegral idx)

extendedGrabCursorsSupported :: IO Bool
extendedGrabCursorsSupported = do
  okVer <- sdlVersionAtLeast 3 2
  if not okVer
    then pure False
    else do
      cur <- createExtendedSystemCursor sdlSystemCursorGrabIdx
      if cur == nullPtr
        then pure False
        else do
          destroyCursorSafe cur
          pure True

grabCursorOrFallback :: Ptr Mouse.SDL_Cursor -> Bool -> IO (Ptr Mouse.SDL_Cursor)
grabCursorOrFallback moveFallback supported =
  if supported
    then do
      cur <- createExtendedSystemCursor sdlSystemCursorGrabIdx
      if cur /= nullPtr then pure cur else pure moveFallback
    else pure moveFallback

grabbingCursorOrFallback :: Ptr Mouse.SDL_Cursor -> Bool -> IO (Ptr Mouse.SDL_Cursor)
grabbingCursorOrFallback moveFallback supported =
  if supported
    then do
      cur <- createExtendedSystemCursor sdlSystemCursorGrabbingIdx
      if cur /= nullPtr then pure cur else pure moveFallback
    else pure moveFallback

data SdlCursors = SdlCursors
  { scDefault :: Ptr Mouse.SDL_Cursor
  , scPointer :: Ptr Mouse.SDL_Cursor
  , scText :: Ptr Mouse.SDL_Cursor
  , scMoveFallback :: Ptr Mouse.SDL_Cursor
  , scGrab :: Ptr Mouse.SDL_Cursor
  , scGrabbing :: Ptr Mouse.SDL_Cursor
  , scCurrent :: IORef UiCursorKind
  }

initCursors :: IO SdlCursors
initCursors = do
  def <- getDefaultCursorSafe
  ptr <- createSystemCursorSafe Mouse.SDL_SYSTEM_CURSOR_POINTER
  text <- createSystemCursorSafe Mouse.SDL_SYSTEM_CURSOR_TEXT
  moveFallback <- createSystemCursorSafe Mouse.SDL_SYSTEM_CURSOR_MOVE
  supported <- extendedGrabCursorsSupported
  grab <- grabCursorOrFallback moveFallback supported
  grabbing <- grabbingCursorOrFallback moveFallback supported
  current <- newIORef UiCursorDefault
  when (def == nullPtr) $ fail "SDL_GetDefaultCursor failed"
  when (ptr == nullPtr) $ fail "SDL_CreateSystemCursor(SDL_SYSTEM_CURSOR_POINTER) failed"
  when (text == nullPtr) $ fail "SDL_CreateSystemCursor(SDL_SYSTEM_CURSOR_TEXT) failed"
  when (moveFallback == nullPtr) $ fail "SDL_CreateSystemCursor(SDL_SYSTEM_CURSOR_MOVE) failed"
  when (grab == nullPtr) $ fail "SDL_CreateSystemCursor(grab) failed"
  when (grabbing == nullPtr) $ fail "SDL_CreateSystemCursor(grabbing) failed"
  pure
    SdlCursors
      { scDefault = def
      , scPointer = ptr
      , scText = text
      , scMoveFallback = moveFallback
      , scGrab = grab
      , scGrabbing = grabbing
      , scCurrent = current
      }

destroyOwnedCursor :: Ptr Mouse.SDL_Cursor -> Ptr Mouse.SDL_Cursor -> IO ()
destroyOwnedCursor cur shared =
  when (cur /= nullPtr && cur /= shared) $
    destroyCursorSafe cur

destroyCursors :: SdlCursors -> IO ()
destroyCursors cursors = do
  let fb = scMoveFallback cursors
  destroyOwnedCursor (scPointer cursors) nullPtr
  destroyOwnedCursor (scText cursors) nullPtr
  destroyOwnedCursor (scGrab cursors) fb
  destroyOwnedCursor (scGrabbing cursors) fb
  destroyCursorSafe fb

cursorPtr :: SdlCursors -> UiCursorKind -> Ptr Mouse.SDL_Cursor
cursorPtr cursors = \case
  UiCursorDefault -> scDefault cursors
  UiCursorPointer -> scPointer cursors
  UiCursorText -> scText cursors
  UiCursorGrab -> scGrab cursors
  UiCursorGrabbing -> scGrabbing cursors

syncPointerCursor :: SdlCursors -> Context -> Input -> IO ()
syncPointerCursor cursors ctx inp = do
  want <- uiCursorKind ctx inp
  cur <- readIORef (scCurrent cursors)
  when (want /= cur) $ do
    void $ setCursorSafe (cursorPtr cursors want)
    writeIORef (scCurrent cursors) want
