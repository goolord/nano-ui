-- | SDL3 native file dialogs.
--
-- These wrap SDL3's asynchronous dialog API ('SDL_ShowOpenFileDialog',
-- 'SDL_ShowSaveFileDialog', and 'SDL_ShowOpenFolderDialog') into a
-- non-blocking, poll-based interface. Launching a dialog returns a
-- 'FileDialogId' immediately and the app keeps running its normal event loop;
-- poll the handle on later frames to observe completion.
--
-- Threading: SDL3 may invoke the dialog callback on a background thread, so
-- the callback here is deliberately minimal — it decodes the result, frees the
-- FFI buffers it owned, wakes the event loop, and records the outcome. All
-- UI-affecting work ('markDirty', releasing the callback 'FunPtr') is deferred
-- to the thread that polls the result.
module NanoUI.Sdl.Dialog
  ( FileFilter (..)
  , FileDialogOptions (..)
  , defaultFileDialogOptions
  , FileDialogId (..)
  , FileDialogResult (..)
  , openFileDialog
  , saveFileDialog
  , openFolderDialog
  , pollFileDialog
  , cancelFileDialog
  , clearDialogState
  , askOpenFileDialog
  , askSaveFileDialog
  , askOpenFolderDialog
  , pollFileDialogUi
  ) where

import Control.Monad (forM, forM_)
import Data.Int (Int32)
import Data.IntMap.Strict qualified as IM
import Data.IORef (atomicModifyIORef', readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Effectful (Eff, type (:>))
import Foreign.C.String (CString, newCString, peekCString)
import Foreign.C.Types (CChar)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (mallocArray)
import Foreign.Ptr (FunPtr, Ptr, castFunPtr, castPtr, freeHaskellFunPtr, nullPtr)
import Foreign.Storable (peekElemOff, pokeElemOff)
import NanoUI.Sdl.Dialog.Types
  ( DialogCallback
  , DialogCallbackFunPtr
  , DialogState (..)
  , FileDialogId (..)
  , FileDialogResult (..)
  , PendingDialog (..)
  , clearDialogState
  )
import NanoUI.Sdl.Display (pushRefreshEvent)
import NanoUI.Sdl.Window (SdlEnv (..))
import NanoUI.Testing (Ui, askHost, markDirty, uiIO)
import SDL3.Sys.Bindgen.Dialog
  ( SDL_DialogFileCallback (..)
  , SDL_DialogFileCallback_Aux
  , SDL_DialogFileFilter (..)
  )
import SDL3.Sys.Bindgen.Runtime.PtrConst qualified as PtrConst
import SDL3.Sys.Dialog
  ( showOpenFileDialogSafe
  , showOpenFolderDialogSafe
  , showSaveFileDialogSafe
  )

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

-- | Launch an open-file dialog. Returns a handle to poll for completion.
openFileDialog :: SdlEnv -> FileDialogOptions -> IO FileDialogId
openFileDialog env opts =
  launchDialog env OpenDialog (dialogFilters opts) (dialogDefaultLocation opts) (dialogAllowMany opts)

-- | Launch a save-file dialog. Returns a handle to poll for completion.
saveFileDialog :: SdlEnv -> FileDialogOptions -> IO FileDialogId
saveFileDialog env opts =
  launchDialog env SaveDialog (dialogFilters opts) (dialogDefaultLocation opts) (dialogAllowMany opts)

-- | Launch a folder-selection dialog. Returns a handle to poll for completion.
openFolderDialog :: SdlEnv -> FileDialogOptions -> IO FileDialogId
openFolderDialog env opts =
  launchDialog env FolderDialog [] (dialogDefaultLocation opts) (dialogAllowMany opts)

-- | Poll a previously launched dialog without blocking.
--
-- Each result is delivered exactly once: after the dialog completes, the
-- first poll that observes the finished state returns it and forgets the
-- handle, so later polls return 'FileDialogUnknown'.
pollFileDialog :: SdlEnv -> FileDialogId -> IO FileDialogResult
pollFileDialog env (FileDialogId did) = do
  let st = sdlDialogState env
  (mcb, result) <-
    atomicModifyIORef' (dsPending st) $ \pending ->
      case IM.lookup did pending of
        Nothing -> (pending, (Nothing, FileDialogUnknown))
        Just (PendingDialog FileDialogPending _) -> (pending, (Nothing, FileDialogPending))
        Just (PendingDialog status cb) -> (IM.delete did pending, (Just cb, status))
  forM_ mcb freeHaskellFunPtr
  case result of
    FileDialogPending -> pure ()
    FileDialogUnknown -> pure ()
    _ -> do
      -- The dialog finished; request a redraw so the caller can reflect the
      -- result. Safe here: this runs on the polling (UI) thread.
      ctx <- readIORef (sdlCachedCtx env)
      markDirty ctx
  pure result

-- | Stop tracking a dialog handle without waiting for the native dialog to
-- finish. The handle returns 'FileDialogUnknown' if polled afterwards.
-- The native dialog keeps running until the user dismisses it; its result is
-- discarded. If a dialog is abandoned while still open, its small FFI
-- callback is left to be reclaimed at teardown or process exit.
cancelFileDialog :: SdlEnv -> FileDialogId -> IO ()
cancelFileDialog env (FileDialogId did) =
  atomicModifyIORef' (dsPending (sdlDialogState env)) $ \m -> (IM.delete did m, ())

-- | Open-file dialog, usable from within 'NanoUI' widget code. Returns
-- 'Nothing' when there is no SDL host to launch a dialog.
askOpenFileDialog :: Ui :> es => FileDialogOptions -> Eff es (Maybe FileDialogId)
askOpenFileDialog opts = do
  menv <- askHost
  case menv of
    Nothing -> pure Nothing
    Just env -> uiIO (Just <$> openFileDialog env opts)

-- | Save-file dialog, usable from within 'NanoUI' widget code. Returns
-- 'Nothing' when there is no SDL host to launch a dialog.
askSaveFileDialog :: Ui :> es => FileDialogOptions -> Eff es (Maybe FileDialogId)
askSaveFileDialog opts = do
  menv <- askHost
  case menv of
    Nothing -> pure Nothing
    Just env -> uiIO (Just <$> saveFileDialog env opts)

-- | Folder dialog, usable from within 'NanoUI' widget code. Returns
-- 'Nothing' when there is no SDL host to launch a dialog.
askOpenFolderDialog :: Ui :> es => FileDialogOptions -> Eff es (Maybe FileDialogId)
askOpenFolderDialog opts = do
  menv <- askHost
  case menv of
    Nothing -> pure Nothing
    Just env -> uiIO (Just <$> openFolderDialog env opts)

-- | Poll a dialog from within 'NanoUI' widget code.
pollFileDialogUi :: Ui :> es => FileDialogId -> Eff es FileDialogResult
pollFileDialogUi did = do
  menv <- askHost
  case menv of
    Nothing -> pure FileDialogUnknown
    Just env -> uiIO (pollFileDialog env did)

data DialogKind = OpenDialog | SaveDialog | FolderDialog

launchDialog ::
  SdlEnv ->
  DialogKind ->
  [FileFilter] ->
  Maybe FilePath ->
  Bool ->
  IO FileDialogId
launchDialog env kind filters mDefault allowMany = do
  (filtersPtr, filterStrs) <- allocFilters filters
  (defaultPtr, defaultStr) <- allocDefault mDefault
  let st = sdlDialogState env
  did <- nextDialogId st
  rawFp <- mkDialogCallback (onResult did st filterStrs filtersPtr defaultStr)
  -- Register the handle before showing: the callback may fire before this
  -- function returns, and it must find its entry.
  atomicModifyIORef' (dsPending st) $ \m ->
    (IM.insert did (PendingDialog FileDialogPending rawFp) m, ())
  let cb = SDL_DialogFileCallback (castFunPtr rawFp :: FunPtr SDL_DialogFileCallback_Aux)
      filtersConst = PtrConst.unsafeFromPtr filtersPtr
      nfilters = fromIntegral (length filters)
  case kind of
    OpenDialog ->
      showOpenFileDialogSafe cb nullPtr (sdlWindow env) filtersConst nfilters defaultPtr allowMany
    SaveDialog ->
      showSaveFileDialogSafe cb nullPtr (sdlWindow env) filtersConst nfilters defaultPtr
    FolderDialog ->
      showOpenFolderDialogSafe cb nullPtr (sdlWindow env) defaultPtr allowMany
  pure (FileDialogId did)

nextDialogId :: DialogState -> IO Int
nextDialogId st = atomicModifyIORef' (dsNextId st) $ \n -> (n + 1, n + 1)

-- | SDL invoked the callback: decode the file list, release the FFI buffers
-- this launch owned, wake the (possibly idle) event loop, and only then record
-- the outcome. Recording is the last action so that no thread ever observes a
-- finished dialog while the callback is still running.
onResult ::
  Int ->
  DialogState ->
  [CString] ->
  Ptr SDL_DialogFileFilter ->
  Maybe CString ->
  Ptr () ->
  Ptr () ->
  Int32 ->
  IO ()
onResult did st filterStrs filtersPtr defaultStr _userdata filelistRaw _filterIdx = do
  paths <- peekFileListRaw filelistRaw
  let outcome =
        case paths of
          Nothing -> FileDialogFailed
          Just [] -> FileDialogCancelled
          Just ps -> FileDialogSelected ps
  forM_ filterStrs free
  free filtersPtr
  forM_ defaultStr free
  pushRefreshEvent
  atomicModifyIORef' (dsPending st) $ \pending ->
    case IM.lookup did pending of
      Nothing -> (pending, ())
      Just pl -> (IM.insert did pl {pendingStatus = outcome} pending, ())

-- | Decode SDL's null-terminated file list into a plain list of paths.
--
-- A null list pointer means SDL hit an error; a null first entry means the
-- user canceled.
peekFileListRaw :: Ptr () -> IO (Maybe [FilePath])
peekFileListRaw filelistRaw = do
  let entries = castPtr filelistRaw :: Ptr (Ptr CChar)
  if entries == nullPtr
    then pure Nothing
    else go entries 0
  where
    go ptrs i = do
      p <- peekElemOff ptrs i
      if p == nullPtr
        then pure (Just [])
        else do
          path <- peekCString p
          rest <- go ptrs (i + 1)
          pure (fmap (path :) rest)

allocFilters :: [FileFilter] -> IO (Ptr SDL_DialogFileFilter, [CString])
allocFilters [] = pure (nullPtr, [])
allocFilters fs = do
  arr <- mallocArray (length fs)
  strs <-
    fmap concat $
      forM (zip [0 ..] fs) $ \(i, FileFilter name pattern_) -> do
        namePtr <- newCString (T.unpack name)
        patternPtr <- newCString (T.unpack pattern_)
        pokeElemOff
          arr
          i
          ( SDL_DialogFileFilter
              (PtrConst.unsafeFromPtr namePtr)
              (PtrConst.unsafeFromPtr patternPtr)
          )
        pure [namePtr, patternPtr]
  pure (arr, strs)

allocDefault :: Maybe FilePath -> IO (PtrConst.PtrConst CChar, Maybe CString)
allocDefault Nothing = pure (PtrConst.unsafeFromPtr nullPtr, Nothing)
allocDefault (Just path) = do
  cstr <- newCString path
  pure (PtrConst.unsafeFromPtr cstr, Just cstr)

-- SDL_DialogFileCallback is `void (*)(void *, const char * const *, int)`,
-- flattened to `void *` pointers at the FFI boundary.
foreign import ccall "wrapper"
  mkDialogCallback :: DialogCallback -> IO DialogCallbackFunPtr