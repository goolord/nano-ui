-- | Own SDL system cursors and apply nano-ui cursor requests on the display thread.
module NanoUI.Sdl.Cursor
  ( SdlCursors (..)
  , initCursors
  , destroyCursors
  , syncPointerCursor
  ) where

import Control.Monad (void, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Foreign.Ptr (Ptr, nullPtr)
import NanoUI (Input (..))
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
  , scPointer :: Ptr Mouse.SDL_Cursor
  , scText :: Ptr Mouse.SDL_Cursor
  , scMoveFallback :: Ptr Mouse.SDL_Cursor
  , scGrab :: Ptr Mouse.SDL_Cursor
  , scGrabbing :: Ptr Mouse.SDL_Cursor
  , scNsResize :: Ptr Mouse.SDL_Cursor
  , scEwResize :: Ptr Mouse.SDL_Cursor
  , scNwseResize :: Ptr Mouse.SDL_Cursor
  , scNeswResize :: Ptr Mouse.SDL_Cursor
  , scCurrent :: IORef UiCursorKind
  , scTrace :: Bool
  }

initCursors :: IO SdlCursors
initCursors = do
  def <- getDefaultCursorSafe
  ptr <- createSystemCursorSafe Mouse.SDL_SYSTEM_CURSOR_POINTER
  text <- createSystemCursorSafe Mouse.SDL_SYSTEM_CURSOR_TEXT
  moveFallback <- createSystemCursorSafe Mouse.SDL_SYSTEM_CURSOR_MOVE
  ns <- createSystemCursorSafe Mouse.SDL_SYSTEM_CURSOR_NS_RESIZE
  ew <- createSystemCursorSafe Mouse.SDL_SYSTEM_CURSOR_EW_RESIZE
  nwse <- createSystemCursorSafe Mouse.SDL_SYSTEM_CURSOR_NWSE_RESIZE
  nesw <- createSystemCursorSafe Mouse.SDL_SYSTEM_CURSOR_NESW_RESIZE
  -- SDL_SYSTEM_CURSOR_GRAB (27) and GRABBING (28) have no bindgen patterns.
  -- Where SDL or the platform lacks them creation returns NULL, and the move
  -- cursor stands in.
  grab <- createSystemCursorSafe (Mouse.SDL_SystemCursor 27)
  grabbing <- createSystemCursorSafe (Mouse.SDL_SystemCursor 28)
  current <- newIORef UiCursorDefault
  -- Debug aid, read once here so cursor changes stay allocation-free:
  -- NANO_CURSOR_TRACE=1 logs every cursor change to stderr.
  trace <- (== Just "1") <$> lookupEnv "NANO_CURSOR_TRACE"
  -- NULL cursors are tolerated: SDL_SetCursor(NULL) selects the platform
  -- default arrow, which keeps us running on headless/dummy video drivers
  -- where system cursor shapes are unavailable.
  pure
    SdlCursors
      { scDefault = def
      , scPointer = ptr
      , scText = text
      , scMoveFallback = moveFallback
      , scGrab = if grab == nullPtr then moveFallback else grab
      , scGrabbing = if grabbing == nullPtr then moveFallback else grabbing
      , scNsResize = ns
      , scEwResize = ew
      , scNwseResize = nwse
      , scNeswResize = nesw
      , scCurrent = current
      , scTrace = trace
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
  destroyOwnedCursor (scNsResize cursors) nullPtr
  destroyOwnedCursor (scEwResize cursors) nullPtr
  destroyOwnedCursor (scNwseResize cursors) nullPtr
  destroyOwnedCursor (scNeswResize cursors) nullPtr
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
  UiCursorNsResize -> scNsResize cursors
  UiCursorEwResize -> scEwResize cursors
  UiCursorNwseResize -> scNwseResize cursors
  UiCursorNeswResize -> scNeswResize cursors

syncPointerCursor :: SdlCursors -> Context -> Input -> IO ()
syncPointerCursor cursors ctx inp = do
  want <- uiCursorKind ctx inp
  cur <- readIORef (scCurrent cursors)
  when (want /= cur) $ do
    when (scTrace cursors) $
      hPutStrLn stderr ("cursor: " ++ show want ++ " at " ++ show (inputMousePos inp))
    void $ setCursorSafe (cursorPtr cursors want)
    writeIORef (scCurrent cursors) want
