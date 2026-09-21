-- | Own SDL system cursors and apply nano-ui cursor requests on the display thread.
module NanoUI.Sdl.Internal.Cursor
  ( SdlCursors (..)
  , initCursors
  , destroyCursors
  , syncPointerCursor
  ) where

import Control.Monad (forM_, void, when)
import Data.Foldable (toList)
import Data.Primitive.SmallArray (SmallArray, indexSmallArray, smallArrayFromList)
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
  { scCursors :: !(SmallArray (Ptr Mouse.SDL_Cursor))
  -- ^ By 'fromEnum' of the 'UiCursorKind'. A NULL cursor selects the
  -- platform's default arrow.
  , scMoveFallback :: Ptr Mouse.SDL_Cursor
  , scCurrent :: IORef UiCursorKind
  , scTrace :: Bool
  }

initCursors :: IO SdlCursors
initCursors = do
  def <- getDefaultCursorSafe
  moveFallback <- createSystemCursorSafe Mouse.SDL_SYSTEM_CURSOR_MOVE
  let orMove c = if c == nullPtr then moveFallback else c
  -- In 'UiCursorKind' order. NULL cursors are tolerated: SDL_SetCursor(NULL)
  -- selects the platform default arrow, which keeps us running on
  -- headless/dummy video drivers where system cursor shapes are unavailable.
  -- SDL_SYSTEM_CURSOR_GRAB (27) and GRABBING (28) have no bindgen patterns.
  -- Where SDL or the platform lacks them creation returns NULL, and the move
  -- cursor stands in.
  owned <-
    sequence
      [ createSystemCursorSafe Mouse.SDL_SYSTEM_CURSOR_POINTER
      , createSystemCursorSafe Mouse.SDL_SYSTEM_CURSOR_TEXT
      , orMove <$> createSystemCursorSafe (Mouse.SDL_SystemCursor 27)
      , orMove <$> createSystemCursorSafe (Mouse.SDL_SystemCursor 28)
      , createSystemCursorSafe Mouse.SDL_SYSTEM_CURSOR_NS_RESIZE
      , createSystemCursorSafe Mouse.SDL_SYSTEM_CURSOR_EW_RESIZE
      , createSystemCursorSafe Mouse.SDL_SYSTEM_CURSOR_NWSE_RESIZE
      , createSystemCursorSafe Mouse.SDL_SYSTEM_CURSOR_NESW_RESIZE
      ]
  SdlCursors (smallArrayFromList (def : owned)) moveFallback
    <$> newIORef UiCursorDefault
    -- Debug aid, read once here so cursor changes stay allocation-free:
    -- NANO_CURSOR_TRACE=1 logs every cursor change to stderr.
    <*> ((== Just "1") <$> lookupEnv "NANO_CURSOR_TRACE")

-- | Destroy the cursors this session created. SDL owns the default one.
destroyCursors :: SdlCursors -> IO ()
destroyCursors SdlCursors {scCursors, scMoveFallback = fb} = do
  forM_ (drop 1 (toList scCursors)) $ \cur ->
    when (cur /= nullPtr && cur /= fb) (destroyCursorSafe cur)
  destroyCursorSafe fb

syncPointerCursor :: SdlCursors -> Context -> Input -> IO ()
syncPointerCursor cursors ctx inp = do
  want <- uiCursorKind ctx inp
  cur <- readIORef (scCurrent cursors)
  when (want /= cur) $ do
    when (scTrace cursors) $
      hPutStrLn stderr ("cursor: " ++ show want ++ " at " ++ show (inputMousePos inp))
    void $ setCursorSafe (indexSmallArray (scCursors cursors) (fromEnum want))
    writeIORef (scCurrent cursors) want
