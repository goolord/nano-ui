{-# LANGUAGE DataKinds #-}

-- | Terminal backend: Win32 console on Windows, notcurses elsewhere.
--
-- notcurses OSC/DA probes echo as garbage in conhost/PowerShell
-- (notcurses #2914). The Win32 driver writes ANSI through the console API.
module NanoUI.Backend.Term
  ( TermDebugSnapshot (..)
  , TermOptions (..)
  , askTermDebug
  , defaultTermOptions
  , runTermApp
  , runTermAppReduce
  , emptyTermDebug
  , dbgPresentFps
  , dbgLoopFps
  , dbgFrameMs
  , dbgUiMs
  , dbgSkips
  , dbgVerts
  , dbgIndices
  , dbgCmds
  , dbgWinW
  , dbgWinH
  , dbgMouseX
  , dbgMouseY
  , dbgRtsOn
  , dbgGcs
  , dbgMajorGcs
  , dbgAllocMb
  , dbgLiveMb
  , dbgMaxMemMb
  , dbgCopiedMb
  , dbgGcPct
  , dbgLastGcGen
  , dbgLastGcMs
  , dbgCaps
  , dbgCpus
  ) where

import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Typeable (Typeable)
import Effectful (Eff, IOE, type (:>))
import NanoUI
  ( Input (..)
  , NanoUI
  , Theme
  , IconSet
  , inputMousePos
  , inputWindowSize
  )
import NanoUI.Testing
  ( Context
  , Ui
  , askContext
  , askHost
  , askInput
  , runEff
  , runFrameEff
  , runFrameReduceEff
  , uiIO
  , withIcons
  , withTheme
  )
import NanoUI.Term.Debug
  ( TermDebugHost (..)
  , TermDebugSnapshot (..)
  , emptyTermDebug
  , readTermDebug
  , dbgPresentFps
  , dbgLoopFps
  , dbgFrameMs
  , dbgUiMs
  , dbgSkips
  , dbgVerts
  , dbgIndices
  , dbgCmds
  , dbgWinW
  , dbgWinH
  , dbgMouseX
  , dbgMouseY
  , dbgRtsOn
  , dbgGcs
  , dbgMajorGcs
  , dbgAllocMb
  , dbgLiveMb
  , dbgMaxMemMb
  , dbgCopiedMb
  , dbgGcPct
  , dbgLastGcGen
  , dbgLastGcMs
  , dbgCaps
  , dbgCpus
  )
import NanoUI.Term.Palette (newTerminalContext)
import NanoUI.Term.Session (withTermSession)

data TermOptions = TermOptions
  { termAppTheme :: !(Maybe Theme)
  , termAppIcons :: !(Maybe IconSet)
  , termAppShouldQuit :: !(Input -> Bool)
  }

defaultTermOptions :: TermOptions
defaultTermOptions =
  TermOptions
    { termAppTheme = Nothing
    , termAppIcons = Nothing
    , termAppShouldQuit = const False
    }

runTermApp :: TermOptions -> NanoUI () -> IO ()
runTermApp options ui = do
  ctx <- termContext options
  runTermAppWithQuit ctx (termAppShouldQuit options) ui

runTermAppWithQuit :: Context -> (Input -> Bool) -> NanoUI () -> IO ()
runTermAppWithQuit = runTermAppWithQuitEff runEff

runTermAppReduce ::
  (Typeable msg, Eq model) =>
  TermOptions ->
  (msg -> model -> model) ->
  model ->
  (model -> NanoUI ()) ->
  IO ()
runTermAppReduce options update model view = do
  ctx <- termContext options
  runTermAppWithQuitReduce update ctx model (termAppShouldQuit options) view

termContext :: TermOptions -> IO Context
termContext options = do
  ctx0 <- newTerminalContext
  let themed = maybe ctx0 (withTheme ctx0) (termAppTheme options)
  pure $ maybe themed (withIcons themed) (termAppIcons options)

runTermAppWithQuitReduce ::
  (Typeable msg, Eq model) =>
  (msg -> model -> model) ->
  Context ->
  model ->
  (Input -> Bool) ->
  (model -> NanoUI ()) ->
  IO ()
runTermAppWithQuitReduce = runTermAppWithQuitReduceEff runEff

runTermAppWithQuitEff ::
  IOE :> es =>
  (forall x. Eff es x -> IO x) ->
  Context ->
  (Input -> Bool) ->
  Eff (Ui : es) () ->
  IO ()
runTermAppWithQuitEff unlift ctx shouldQuit ui =
  withTermSession ctx shouldQuit $ \c i -> do
    (_, _, d, _) <- runFrameEff unlift c i ui
    pure d

runTermAppWithQuitReduceEff ::
  (IOE :> es, Typeable msg, Eq model) =>
  (forall x. Eff es x -> IO x) ->
  (msg -> model -> model) ->
  Context ->
  model ->
  (Input -> Bool) ->
  (model -> Eff (Ui : es) ()) ->
  IO ()
runTermAppWithQuitReduceEff unlift update ctx model0 shouldQuit view = do
  modelRef <- newIORef model0
  withTermSession ctx shouldQuit $ \c i -> do
    m <- readIORef modelRef
    (_, m', _, d, _) <- runFrameReduceEff unlift update c i m view
    writeIORef modelRef m'
    pure d

askTermDebug :: Ui :> es => Eff es TermDebugSnapshot
askTermDebug = do
  ctx <- askContext
  inp <- askInput
  mhost <- askHost @TermDebugHost
  case mhost of
    Nothing -> pure emptyTermDebug
    Just (TermDebugHost ref) ->
      uiIO (readTermDebug ref (inputWindowSize inp) (inputMousePos inp) ctx)
