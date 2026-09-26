-- | Idle probe: a window that should cost nothing while nobody touches it.
--
-- @cabal run nano-ui-sdl-idle -- <scene> [hidden]@ opens one of the scenes
-- below. @hidden@ keeps the window off screen, where no pointer can reach it:
-- the loop runs the same, so a check that must see no input can rely on it.
--
--   * @static@  labels, buttons and a list: nothing to animate or edit
--   * @focus@   the same, with the search field holding keyboard focus
--   * @wake@    the same, with a thread calling the loop's wake every second
--   * @spinner@ the same, with a spinner for the first three seconds only
--   * @clock@   the same, with a label of whole seconds kept by 'wakeAfter'
--   * @type@    @focus@, then a character typed into the field two seconds
--               in; prints how long the search took to report the change
--   * @startup@ the first ten frames each ask for the next, by marking the
--               context dirty or by waking the loop, and the tenth prints: a
--               frame asked for while the window opens must be drawn without
--               waiting for input
--
-- Watch the process in a profiler or Task Manager, or set @NANO_LOOP_TRACE@
-- to print how many passes and frames the session loop ran each second. An
-- idle scene should print nothing once it has settled. @type@ should print a
-- delay close to the search field's 300 ms debounce: the field sleeps through
-- it and is woken to commit, where it once polled at the display rate.
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forM_, forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Foreign.C.String (newCString)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Ptr (Ptr)
import Foreign.Storable (poke, sizeOf)
import GHC.Clock (getMonotonicTime)
import NanoUI
import NanoUI.Backend.Sdl (SdlOptions (..), defaultSdlOptions, runSdlApp)
import NanoUI.Internal.Context (Context (..))
import NanoUI.Testing (askContext, markDirty)
import SDL3.Sys.Bindgen.Events (SDL_Event, SDL_TextInputEvent (..))
import SDL3.Sys.Bindgen.Events qualified as Events
import SDL3.Sys.Bindgen.Runtime.PtrConst qualified as PtrConst
import SDL3.Sys.Bindgen.Stdinc (Uint32 (..), Uint64 (..))
import SDL3.Sys.Bindgen.Video (SDL_WindowID (..))
import SDL3.Sys.Events (pushEvent)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Text.Printf (printf)
import qualified Data.Text as T

main :: IO ()
main = do
  args <- getArgs
  let scene = case args of
        (s : _) -> s
        [] -> "static"
  frames <- newIORef (0 :: Int)
  started <- newIORef False
  typedAt <- newIORef (0 :: Double)
  runSdlApp
    defaultSdlOptions
      { sdlWindowSettings =
          defaultWindowSettings
            { wsTitle = "nano-ui idle: " <> T.pack scene
            , wsSize = Size 900 600
            , wsMode = if "hidden" `elem` drop 1 args then Hidden else Windowed
            }
      , sdlAppShouldQuit = \inp -> inputKeysElem KeyEscape (inputKeys inp)
      }
    (idleUi scene frames started typedAt)

idleUi :: String -> IORef Int -> IORef Bool -> IORef Double -> NanoUI ()
idleUi scene frames started typedAt = do
  n <- liftIO (readIORef frames)
  liftIO (writeIORef frames (n + 1))
  ctx <- askContext
  -- Background work starts once, from the first frame.
  first <- liftIO (not <$> readIORef started)
  when first $ liftIO $ do
    writeIORef started True
    when (scene == "wake") $ do
      mWake <- readIORef (ctxWakeLoop ctx)
      forM_ mWake $ \wake -> void $ forkIO $ forever $ threadDelay 1000000 >> wake
    when (scene == "type") $ void $ forkIO $ do
      threadDelay 2000000
      getMonotonicTime >>= writeIORef typedAt
      pushTextInput "a"
  -- Ask for the next frame both ways a view can: by marking the context
  -- dirty, and by waking the loop as a background thread would.
  when (scene == "startup" && n < 10) $ liftIO $ do
    if even n
      then markDirty ctx
      else readIORef (ctxWakeLoop ctx) >>= sequence_
    when (n == 9) $ putStrLn "startup: reached frame 10 without input" >> hFlush stdout
  columnWith (gap 8 . padAll 16 . fillW . fillH) $ do
    heading "Idle probe"
    label "This window should use no CPU or GPU while it is left alone."
    (query, setQuery) <- useText ""
    (resp, query') <- searchInput' "Search" query
    setQuery query'
    -- Hold keyboard focus without a click, as an app's search box would.
    when (scene `elem` ["focus", "type"] && n < 4) $
      requestFocus (respId resp)
    when (scene == "type" && respChanged resp) $ liftIO $ do
      now <- getMonotonicTime
      sent <- readIORef typedAt
      printf "search %s committed %.0f ms after the key\n" (show query') ((now - sent) * 1000)
      hFlush stdout
    t <- uiTime
    (t0, setT0) <- useState (0 :: Double)
    when (t0 == 0) (setT0 t)
    -- A loading indicator that goes away, as an app's would after startup.
    when (scene == "spinner" && t0 > 0 && t - t0 < 3) spinner
    when (scene == "clock") $ do
      let up = t - t0
          whole = floor up :: Int
      label ("Up " <> T.pack (show whole) <> " s")
      wakeAfter (fromIntegral (whole + 1) - up)
    (flag, setFlag) <- useFlag True
    setFlag =<< checkbox "A checkbox" flag
    row $ do
      void (button "One")
      void (button "Two")
      void (button "Three")
    forM_ [1 :: Int .. 12] $ \i ->
      label ("Row " <> T.pack (show i) <> (if T.null query' then "" else " matching " <> query'))

-- | Queue an SDL_EVENT_TEXT_INPUT, as the keyboard would. The event and its
-- string are leaked: SDL reads the text pointer when the loop takes the
-- event, and a probe types once.
pushTextInput :: String -> IO ()
pushTextInput str = do
  typed <- newCString str
  ev <- callocBytes (sizeOf (undefined :: SDL_Event)) :: IO (Ptr SDL_Event)
  poke ev.text
    SDL_TextInputEvent
      { type' = Events.SDL_EVENT_TEXT_INPUT
      , reserved = Uint32 0
      , timestamp = Uint64 0 -- SDL stamps an event pushed with none
      , windowID = SDL_WindowID (Uint32 0)
      , text = PtrConst.unsafeFromPtr typed
      }
  void (pushEvent ev)
