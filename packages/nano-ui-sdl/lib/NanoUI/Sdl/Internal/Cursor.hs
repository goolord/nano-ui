-- | Own SDL system cursors and apply nano-ui cursor requests on the display thread.
module NanoUI.Sdl.Internal.Cursor
  ( SdlCursors (..)
  , initCursors
  , destroyCursors
  , syncPointerCursor
  , showCursorKind
  , sdlSystemCursor
  ) where

import Control.Monad (void, when)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Foreign.Ptr (Ptr, nullPtr)
import NanoUI (Input (..))
import NanoUI.Backend (cursorFallback)
import NanoUI.Testing (Context, UiCursorKind (..), uiCursorKind)
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import qualified SDL3.Sys.Bindgen.Mouse as Mouse
import SDL3.Sys.Mouse
  ( createSystemCursorSafe
  , destroyCursorSafe
  , getDefaultCursorSafe
  , setCursorSafe
  )

data SdlCursors = SdlCursors
  { scDefault :: Ptr Mouse.SDL_Cursor
  -- ^ The platform's default arrow, which SDL owns.
  , scSystem :: IORef [(Mouse.SDL_SystemCursor, Ptr Mouse.SDL_Cursor)]
  -- ^ The system cursors shown so far, each created the first time it is
  -- wanted. A NULL cursor is one the platform could not create (headless and
  -- dummy video drivers have none), so it is not asked for again.
  , scCurrent :: IORef UiCursorKind
  , scTrace :: Bool
  }

initCursors :: IO SdlCursors
initCursors =
  SdlCursors
    <$> getDefaultCursorSafe
    <*> newIORef []
    <*> newIORef UiCursorDefault
    -- Debug aid, read once here so cursor changes stay allocation-free:
    -- NANO_CURSOR_TRACE=1 logs every cursor change to stderr.
    <*> ((== Just "1") <$> lookupEnv "NANO_CURSOR_TRACE")

-- | Destroy the cursors this session created. SDL owns the default one.
destroyCursors :: SdlCursors -> IO ()
destroyCursors SdlCursors {scSystem} =
  mapM_ destroyCursorSafe . filter (/= nullPtr) . map snd =<< readIORef scSystem

-- | The SDL system cursor that shows @kind@, or its 'cursorFallback';
-- 'Nothing' for the platform's default arrow. SDL itself shows the one-way
-- resize arrows as two-way ones on platforms without them.
sdlSystemCursor :: UiCursorKind -> Maybe Mouse.SDL_SystemCursor
sdlSystemCursor kind = case cursorFallback kind of
  UiCursorPointer -> Just Mouse.SDL_SYSTEM_CURSOR_POINTER
  UiCursorText -> Just Mouse.SDL_SYSTEM_CURSOR_TEXT
  UiCursorNsResize -> Just Mouse.SDL_SYSTEM_CURSOR_NS_RESIZE
  UiCursorEwResize -> Just Mouse.SDL_SYSTEM_CURSOR_EW_RESIZE
  UiCursorNwseResize -> Just Mouse.SDL_SYSTEM_CURSOR_NWSE_RESIZE
  UiCursorNeswResize -> Just Mouse.SDL_SYSTEM_CURSOR_NESW_RESIZE
  UiCursorNotAllowed -> Just Mouse.SDL_SYSTEM_CURSOR_NOT_ALLOWED
  UiCursorWait -> Just Mouse.SDL_SYSTEM_CURSOR_WAIT
  UiCursorProgress -> Just Mouse.SDL_SYSTEM_CURSOR_PROGRESS
  UiCursorCrosshair -> Just Mouse.SDL_SYSTEM_CURSOR_CROSSHAIR
  UiCursorMove -> Just Mouse.SDL_SYSTEM_CURSOR_MOVE
  UiCursorNResize -> Just Mouse.SDL_SYSTEM_CURSOR_N_RESIZE
  UiCursorNeResize -> Just Mouse.SDL_SYSTEM_CURSOR_NE_RESIZE
  UiCursorEResize -> Just Mouse.SDL_SYSTEM_CURSOR_E_RESIZE
  UiCursorSeResize -> Just Mouse.SDL_SYSTEM_CURSOR_SE_RESIZE
  UiCursorSResize -> Just Mouse.SDL_SYSTEM_CURSOR_S_RESIZE
  UiCursorSwResize -> Just Mouse.SDL_SYSTEM_CURSOR_SW_RESIZE
  UiCursorWResize -> Just Mouse.SDL_SYSTEM_CURSOR_W_RESIZE
  UiCursorNwResize -> Just Mouse.SDL_SYSTEM_CURSOR_NW_RESIZE
  _ -> Nothing

-- | The cursor that shows @kind@: its 'sdlSystemCursor', created the first
-- time it is wanted, or the default arrow where the platform has none.
cursorFor :: SdlCursors -> UiCursorKind -> IO (Ptr Mouse.SDL_Cursor)
cursorFor SdlCursors {scDefault, scSystem} kind = case sdlSystemCursor kind of
  Nothing -> pure scDefault
  Just sys -> do
    c <-
      lookup sys <$> readIORef scSystem >>= \case
        Just c -> pure c
        Nothing -> do
          c <- createSystemCursorSafe sys
          modifyIORef' scSystem ((sys, c) :)
          pure c
    pure (if c == nullPtr then scDefault else c)

syncPointerCursor :: SdlCursors -> Context -> Input -> IO ()
syncPointerCursor cursors ctx inp = do
  want <- uiCursorKind ctx inp
  cur <- readIORef (scCurrent cursors)
  when (want /= cur) $ do
    when (scTrace cursors) $
      hPutStrLn stderr ("cursor: " ++ show want ++ " at " ++ show (inputMousePos inp))
    showCursorKind cursors want

-- | Show the cursor for @kind@. Where even the default arrow is NULL,
-- SDL_SetCursor(NULL) redraws the cursor already shown.
showCursorKind :: SdlCursors -> UiCursorKind -> IO ()
showCursorKind cursors kind = do
  void . setCursorSafe =<< cursorFor cursors kind
  writeIORef (scCurrent cursors) kind
