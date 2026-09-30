-- | SDL3 native file dialogs.
--
-- These wrap SDL3's asynchronous dialog API ('SDL_ShowOpenFileDialog',
-- 'SDL_ShowSaveFileDialog', and 'SDL_ShowOpenFolderDialog') into a
-- non-blocking, poll-based interface. Launching a dialog returns a
-- 'FileDialogId' immediately and the app keeps running its normal event loop;
-- poll the handle on later frames to observe completion.
--
-- Threading: SDL3 may invoke the dialog callback on a background thread, so
-- the callback here does little: it decodes the result, frees the FFI
-- buffers the launch allocated, records the outcome, and wakes the event
-- loop. All UI-affecting work ('markDirty', reclaiming focus) is deferred to
-- the thread that polls the result.
module NanoUI.Sdl.Internal.Dialog
  ( FileFilter (..)
  , FileDialogOptions (..)
  , defaultFileDialogOptions
  , FileDialogId
  , FileDialogResult (..)
  , openFileDialog
  , saveFileDialog
  , openFolderDialog
  , pollFileDialog
  , cancelFileDialog
  , askOpenFileDialog
  , askSaveFileDialog
  , askOpenFolderDialog
  , pollFileDialogUi
  , peekFileDialogUi
  ) where

import Control.Monad (forM, unless, void, when, (>=>))
import Data.Int (Int32)
import Data.IORef (IORef, atomicWriteIORef, newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Foreign.C.String (newCString, peekCString)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (newArray, peekArray0)
import Foreign.Marshal.Utils (maybePeek)
import Foreign.Ptr (FunPtr, Ptr, castFunPtr, castPtr, nullPtr)
import Foreign.StablePtr (castPtrToStablePtr, castStablePtrToPtr, deRefStablePtr, freeStablePtr, newStablePtr)
import NanoUI.Sdl.Internal.Display (pushRefreshEvent)
import NanoUI.Sdl.Internal.DialogState
import NanoUI.Sdl.Internal.Window (SdlEnv (..), askSdlEnv)
import NanoUI.Internal.Monad (NanoUI, askContext)
import NanoUI.Testing (Context, markDirty, liftIO)
import SDL3.Sys.Bindgen.Dialog (SDL_DialogFileCallback (..), SDL_DialogFileFilter (..))
import SDL3.Sys.Bindgen.Runtime.PtrConst qualified as PtrConst
import SDL3.Sys.Dialog
  ( showOpenFileDialogSafe
  , showOpenFolderDialogSafe
  , showSaveFileDialogSafe
  )
import SDL3.Sys.Video (raiseWindowSafe, restoreWindowSafe)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)

-- | A file type filter shown in open/save dialogs.
data FileFilter = FileFilter
  { filterName :: !Text
  -- ^ Human-readable label, e.g. @"Haskell source"@.
  , filterPattern :: !Text
  -- ^ Semicolon-separated extension list, e.g. @"hs;lhs"@, or @"*"@.
  }
  deriving (Eq, Show)

-- | Common options for native file dialogs.
data FileDialogOptions = FileDialogOptions
  { dialogFilters :: ![FileFilter]
  -- ^ File filters (ignored by folder dialogs).
  , dialogDefaultLocation :: !(Maybe FilePath)
  -- ^ Starting folder or file.
  , dialogAllowMany :: !Bool
  -- ^ Allow selecting more than one entry (ignored by save dialogs).
  }
  deriving (Eq, Show)

-- | Sensible defaults: no filters, no default location, single selection.
defaultFileDialogOptions :: FileDialogOptions
defaultFileDialogOptions = FileDialogOptions [] Nothing False

-- | Opaque handle owned by the session that launched it. Passing it to a
-- different session does not consume its result or touch either window.
data FileDialogId = FileDialogId !(IORef Context) !(IORef FileDialogResult)
  deriving (Eq)

-- | Launch an open-file dialog. Returns a handle to poll for completion.
openFileDialog :: SdlEnv -> FileDialogOptions -> IO FileDialogId
openFileDialog env = launchDialog env OpenDialog

-- | Launch a save-file dialog. Returns a handle to poll for completion.
saveFileDialog :: SdlEnv -> FileDialogOptions -> IO FileDialogId
saveFileDialog env = launchDialog env SaveDialog

-- | Launch a folder-selection dialog. Returns a handle to poll for completion.
openFolderDialog :: SdlEnv -> FileDialogOptions -> IO FileDialogId
openFolderDialog env = launchDialog env FolderDialog

-- | Poll a previously launched dialog without blocking.
--
-- Each result is delivered exactly once: after the dialog completes, the
-- first poll that observes the finished state returns it and forgets the
-- handle, so later polls return 'FileDialogUnknown'.
pollFileDialog :: SdlEnv -> FileDialogId -> IO FileDialogResult
pollFileDialog env (FileDialogId owner ref)
  | owner /= sdlCachedCtx env = pure FileDialogUnknown
  | otherwise = do
      -- Only the poll that takes a finished result out delivers it.
      result <- takeResult ref
      unless (result == FileDialogPending || result == FileDialogUnknown) $ do
        -- The native dialog stole window focus; reclaim it so the app keeps
        -- receiving hover/motion/wheel events without an extra click.
        -- Restoration is a best-effort no-op when the window was never
        -- minimized; only a failed raise signals a possible focus failure.
        void (restoreWindowSafe (sdlWindow env))
        raised <- raiseWindowSafe (sdlWindow env)
        unless raised $
          hPutStrLn stderr "nano-ui: dialog completed but window raise failed; input may need a click"
        markDirty =<< readIORef (sdlCachedCtx env)
      pure result

-- | Stop tracking a dialog handle without waiting for the native dialog to
-- finish. The handle returns 'FileDialogUnknown' if polled afterwards.
-- The native dialog keeps running until the user dismisses it; its result is
-- discarded.
cancelFileDialog :: SdlEnv -> FileDialogId -> IO ()
cancelFileDialog env (FileDialogId owner ref) =
  when (owner == sdlCachedCtx env) (atomicWriteIORef ref FileDialogUnknown)

-- | Open an open-file dialog from a view, owned by the open SDL session.
-- Outside a session the handle reports 'FileDialogFailed'.
askOpenFileDialog :: FileDialogOptions -> NanoUI FileDialogId
askOpenFileDialog = askDialog OpenDialog

-- | 'askOpenFileDialog' for a save-file dialog.
askSaveFileDialog :: FileDialogOptions -> NanoUI FileDialogId
askSaveFileDialog = askDialog SaveDialog

-- | 'askOpenFileDialog' for a folder dialog.
askOpenFolderDialog :: FileDialogOptions -> NanoUI FileDialogId
askOpenFolderDialog = askDialog FolderDialog

askDialog :: DialogKind -> FileDialogOptions -> NanoUI FileDialogId
askDialog kind opts =
  askSdlEnv >>= \case
    Just env -> liftIO (launchDialog env kind opts)
    Nothing -> do
      owner <- liftIO . newIORef =<< askContext
      liftIO (FileDialogId owner <$> newIORef FileDialogFailed)

-- | Consuming poll from a view, with the same focus restoration as
-- 'pollFileDialog'. Outside a session it consumes the handle's result
-- without touching a window.
pollFileDialogUi :: FileDialogId -> NanoUI FileDialogResult
pollFileDialogUi did@(FileDialogId _ ref) =
  askSdlEnv >>= \case
    Just env -> liftIO (pollFileDialog env did)
    Nothing -> liftIO (takeResult ref)

-- | Observe without consuming or restoring focus. Repeated peeks report the
-- same result until 'pollFileDialogUi' consumes it. Use polling for actions
-- that must happen only once, such as opening the selected file.
peekFileDialogUi :: FileDialogId -> NanoUI FileDialogResult
peekFileDialogUi (FileDialogId _ ref) = liftIO (readIORef ref)

data DialogKind = OpenDialog | SaveDialog | FolderDialog
  deriving (Eq)

launchDialog :: SdlEnv -> DialogKind -> FileDialogOptions -> IO FileDialogId
launchDialog env kind opts = do
  -- SDL reads the filters and the location until it runs the callback, which
  -- frees them.
  names <-
    forM (if kind == FolderDialog then [] else dialogFilters opts) $ \(FileFilter name pattern_) ->
      (,) <$> newCString (T.unpack name) <*> newCString (T.unpack pattern_)
  filters <-
    if null names
      then pure nullPtr
      else newArray [SDL_DialogFileFilter (PtrConst.unsafeFromPtr n) (PtrConst.unsafeFromPtr p) | (n, p) <- names]
  location <- traverse newCString (dialogDefaultLocation opts)
  result <- newIORef FileDialogPending
  let release = do
        mapM_ (\(n, p) -> free n >> free p) names
        free filters
        mapM_ free location
  userdata <- castPtr . castStablePtrToPtr <$> newStablePtr (result, release)
  let win = sdlWindow env
      filtersConst = PtrConst.unsafeFromPtr filters
      nfilters = fromIntegral (length names)
      locationConst = PtrConst.unsafeFromPtr (fromMaybe nullPtr location)
  case kind of
    OpenDialog ->
      showOpenFileDialogSafe dialogCallback userdata win filtersConst nfilters locationConst (dialogAllowMany opts)
    SaveDialog ->
      showSaveFileDialogSafe dialogCallback userdata win filtersConst nfilters locationConst
    FolderDialog ->
      showOpenFolderDialogSafe dialogCallback userdata win locationConst (dialogAllowMany opts)
  pure (FileDialogId (sdlCachedCtx env) result)

-- | The callback every dialog shares, made once a process. A launch's
-- userdata carries its result cell and the release of its buffers, so there
-- is no callback to free while SDL might still call it.
{-# NOINLINE dialogCallback #-}
dialogCallback :: SDL_DialogFileCallback
dialogCallback = unsafePerformIO (SDL_DialogFileCallback . castFunPtr <$> mkDialogCallback onResult)

-- | SDL invoked the callback: decode the file list, release the FFI buffers
-- the launch owned, record the outcome, and only then wake the (possibly
-- idle) event loop. The status must be visible before the wake, or the woken
-- frame polls 'FileDialogPending', skips, and the result waits for an
-- unrelated event. A null list pointer means SDL hit an error; a null first
-- entry means the user canceled.
onResult :: Ptr () -> Ptr () -> Int32 -> IO ()
onResult userdata filelist _filterIdx = do
  let launch = castPtrToStablePtr userdata
  (result, release :: IO ()) <- deRefStablePtr launch
  freeStablePtr launch
  paths <- maybePeek (peekArray0 nullPtr >=> traverse peekCString) (castPtr filelist)
  release
  let outcome = case paths of
        Nothing -> FileDialogFailed
        Just [] -> FileDialogCancelled
        Just ps -> FileDialogSelected ps
  -- A cancelled dialog's handle stays unknown.
  completeResult result outcome
  pushRefreshEvent

-- SDL_DialogFileCallback is `void (*)(void *, const char * const *, int)`,
-- flattened to `void *` pointers at the FFI boundary.
foreign import ccall "wrapper"
  mkDialogCallback ::
    (Ptr () -> Ptr () -> Int32 -> IO ()) -> IO (FunPtr (Ptr () -> Ptr () -> Int32 -> IO ()))
