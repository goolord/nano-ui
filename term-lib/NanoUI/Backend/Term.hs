{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}

-- | Terminal backend: Win32 console on Windows, notcurses elsewhere.
--
-- notcurses OSC/DA probes echo as garbage in conhost/PowerShell
-- (notcurses #2914). The Win32 driver writes ANSI through the console API.
module NanoUI.Backend.Term
  ( TermOptions (..)
  , defaultTermOptions
  , runTermApp
  , runTermAppReduce
  ) where

#if defined(mingw32_HOST_OS)
import Control.Exception (finally)
#endif
import Control.Monad (when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Typeable (Typeable)
import Effectful (Eff, IOE, type (:>))
import GHC.Clock (getMonotonicTime)
import NanoUI
  ( Input (..)
  , Modifiers (..)
  , Size (..)
  , NanoUI
  , Theme
  , IconSet
  , asciiIcons
  , defaultTheme
  , emptyInput
  , V2 (..)
  )
import NanoUI.Testing
  ( Context
  , DrawData
  , Ui
  , ctxIcons
  , ctxTheme
  , withIcons
  , withTheme
  , withHostProfile
  , HostProfile (CellHost)
  , setHost
  , anyAnimating
  , collectRasterSpans
  , isDirty
  , debugPanelOpen
  , needsRedrawIdle
  , pointerDragActive
  , overlayConsumesQuit
  , runEff
  , runFrameEff
  , runFrameReduceEff
  , textInputEditActive
  , widgetNodeCount
  )
import NanoUI.Term.Debug
  ( TermDebugHost (..)
  , TermDrawStats (..)
  , newTermDebugSampler
  , noteLoop
  , notePresent
  , noteSkip
  , takeDebugLive
  )
import NanoUI.Term.Icons (detectIconSet)
import NanoUI.Term.Palette
  ( newTerminalContext
  , queryTerminalColors
  , terminalTheme
  , terminalThemeFromColors
  )
import NanoUI.Term.Cells
  ( Cells
  , rasterizeLayered
#if defined(mingw32_HOST_OS)
  , cellsSize
#endif
  )
import NanoUI.Term.Event (MouseAction (..), TermEvent (..))

#if defined(mingw32_HOST_OS)
import Data.ByteString.Builder (string7)
import NanoUI.Term.Ansi (frameBytes, setup, teardown)
import NanoUI.Term.Driver (Driver (..), withDriver)
#else
import NanoUI.Term.Notcurses (ncBlitCells, ncRead, ncSize, withNotcurses)
#endif

animateTimeout :: Int
animateTimeout = 16

-- Win32 INFINITE / notcurses: negative timeout blocks until input.
idleBlock :: Int
idleBlock = -1

-- | Application-owned terminal settings. 'Nothing' keeps runtime palette and
-- icon detection; explicit values are never overwritten by that detection.
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

-- Upgrade the theme and icon tier only while they are still the defaults, so an
-- app that set either one keeps its choice.
termContextIO :: Context -> IO Context
termContextIO ctx0 = do
  let ctxHost = withHostProfile ctx0 CellHost
  ctx <-
    if ctxTheme ctxHost == defaultTheme || ctxTheme ctxHost == terminalTheme
      then do
        (fg, bg) <- queryTerminalColors
        pure (withTheme ctxHost (terminalThemeFromColors fg bg))
      else pure ctxHost
  if ctxIcons ctx == asciiIcons
    then do
      icons <- detectIconSet
      pure (withIcons ctx icons)
    else pure ctx

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

#if defined(mingw32_HOST_OS)

withTermSession ::
  Context ->
  (Input -> Bool) ->
  (Context -> Input -> IO DrawData) ->
  IO ()
withTermSession ctx shouldQuit runOnce =
  termContextIO ctx >>= \ctx' ->
  withDriver $ \drv ->
    ( do
        drvWrite drv setup
        drvFlush drv
        drvRefreshViewport drv
        termMainLoop
          ctx'
          shouldQuit
          runOnce
          (drvSize drv)
          (drvRead drv)
          ( \before cur -> do
              when (fmap cellsSize before /= Just (cellsSize cur)) $
                drvWrite drv (string7 "\ESC[2J")
              drvWrite drv (frameBytes before cur)
              drvFlush drv
          )
    )
      `finally` ( do
                    drvWrite drv teardown
                    drvFlush drv
                )

#else

withTermSession ::
  Context ->
  (Input -> Bool) ->
  (Context -> Input -> IO DrawData) ->
  IO ()
withTermSession ctx shouldQuit runOnce =
  termContextIO ctx >>= \ctx' ->
  withNotcurses $ \nc ->
    termMainLoop
      ctx'
      shouldQuit
      runOnce
      (ncSize nc)
      (ncRead nc)
      (ncBlitCells nc)

#endif

termMainLoop ::
  Context ->
  (Input -> Bool) ->
  (Context -> Input -> IO DrawData) ->
  IO (Int, Int) ->
  (Int -> IO [TermEvent]) ->
  (Maybe Cells -> Cells -> IO ()) ->
  IO ()
termMainLoop ctx shouldQuit runOnce getSize readEvents present = do
  debugRef <- newTermDebugSampler
  setHost ctx (TermDebugHost debugRef)
  (w0, h0) <- getSize
  cellsRef <- newIORef Nothing
  startTime <- getMonotonicTime
  let inp0 =
        emptyInput
          { inputWindowSize = Size (fromIntegral w0) (fromIntegral h0)
          }
  prevInpRef <- newIORef inp0
  clickRef <- newIORef (0, V2 (-999) (-999), 0)
  let
    loop cellsRef' prevInpRef' clickRef' inp queued lastT = do
      pending <-
        if null queued
          then do
            dirty <- isDirty ctx
            dragging <- pointerDragActive ctx
            -- Open/close marks dirty for the next tree. Skip blocking read unless
            -- a drag is in progress (drag also marks dirty every frame).
            if dirty && not dragging
              then pure []
              else do
                animating <- anyAnimating ctx
                readEvents (if animating then animateTimeout else idleBlock)
          else pure []
      let (group, rest) = splitFrame (queued ++ pending)
      editActive <- textInputEditActive ctx
      if any isHardQuit group && not editActive
        then pure ()
        else do
          nowT <- getMonotonicTime
          let dt = realToFrac (nowT - lastT)
          noteLoop debugRef dt
          let inpRaw =
                foldl'
                  applyEvent
                  (clearEphemeral inp {inputDeltaTime = dt})
                  group
          inp' <- stampClicks clickRef' inpRaw
          editActive' <- textInputEditActive ctx
          if isHardQuitInput inp' && not editActive'
            then pure ()
            else do
              draw cellsRef' prevInpRef' inp'
              writeIORef prevInpRef' inp'
              overlayQuit <- overlayConsumesQuit ctx inp'
              if shouldQuit inp' && not overlayQuit
                then pure ()
                else loop cellsRef' prevInpRef' clickRef' inp' rest nowT

    draw prevCells prevInpCell inp = do
      prevI <- readIORef prevInpCell
      debugLive <- debugPanelOpen ctx
      wantDebug <- takeDebugLive debugRef debugLive
      need <- needsRedrawIdle ctx prevI inp
      if need || wantDebug
        then do
          t0 <- getMonotonicTime
          drawData <- runOnce ctx inp
          (baseSpans, overlaySpans) <- collectRasterSpans ctx inp
          nodes <- widgetNodeCount ctx
          let Size w h = inputWindowSize inp
              stats =
                TermDrawStats
                  { tdsNodes = nodes
                  , tdsBaseSpans = length baseSpans
                  , tdsOverlaySpans = length overlaySpans
                  }
          cells <- rasterizeLayered (round w) (round h) drawData baseSpans overlaySpans
          before <- readIORef prevCells
          let blitted = before /= Just cells
          when blitted $ do
            present before cells
            writeIORef prevCells (Just cells)
          t1 <- getMonotonicTime
          notePresent debugRef ((t1 - t0) * 1000) drawData stats blitted
        else noteSkip debugRef
  draw cellsRef prevInpRef inp0
  loop cellsRef prevInpRef clickRef inp0 [] startTime

splitFrame :: [TermEvent] -> ([TermEvent], [TermEvent])
splitFrame events =
  case break isButtonEdge events of
    (before, edge : rest) -> (before ++ [edge], rest)
    (before, []) -> (before, [])

isButtonEdge :: TermEvent -> Bool
isButtonEdge ev =
  case ev of
    EvMouse (MousePress _) _ _ _ -> True
    EvMouse (MouseRelease _) _ _ _ -> True
    _ -> False

isHardQuit :: TermEvent -> Bool
isHardQuit ev =
  case ev of
    EvChar c mods -> modCtrl mods && (c == 'c' || c == '\ETX')
    _ -> False

isHardQuitInput :: Input -> Bool
isHardQuitInput inp =
  any (\c -> modCtrl (inputModifiers inp) && (c == 'c' || c == '\ETX')) (inputChars inp)

clearEphemeral :: Input -> Input
clearEphemeral inp =
  inp
    { inputKeys = []
    , inputChars = []
    , inputMousePressed = False
    , inputMouseReleased = False
    , inputMouseRightPressed = False
    , inputMouseRightReleased = False
    , inputMouseClicks = 1
    , inputScroll = V2 0 0
    }

stampClicks :: IORef (Double, V2, Int) -> Input -> IO Input
stampClicks ref inp
  | not (inputMousePressed inp) = pure inp
  | otherwise = do
      now <- getMonotonicTime
      (t, pos, n) <- readIORef ref
      let V2 x y = inputMousePos inp
          V2 px py = pos
          close = abs (x - px) <= 1.5 && abs (y - py) <= 1.5
          quick = now - t <= 0.4
          n' = if close && quick then min 3 (n + 1) else 1
      writeIORef ref (now, inputMousePos inp, n')
      pure (inp {inputMouseClicks = n'})

applyEvent :: Input -> TermEvent -> Input
applyEvent inp ev =
  case ev of
    EvResize w h -> inp {inputWindowSize = Size (fromIntegral w) (fromIntegral h)}
    EvKey k mods -> inp {inputKeys = inputKeys inp ++ [k], inputModifiers = mods}
    EvChar c mods -> inp {inputChars = inputChars inp ++ [c], inputModifiers = mods}
    EvMouse action col row mods ->
      let positioned =
            inp
              { inputMousePos = V2 (fromIntegral col + 0.5) (fromIntegral row + 0.5)
              , inputModifiers = mods
              }
       in case action of
            MousePress _ ->
              positioned {inputMouseDown = True, inputMousePressed = True}
            MouseRelease _ ->
              positioned {inputMouseDown = False, inputMouseReleased = True}
            MouseDrag _ -> positioned {inputMouseDown = True}
            MouseMove -> positioned
            MouseScrollUp -> positioned {inputScroll = V2 0 (-1)}
            MouseScrollDown -> positioned {inputScroll = V2 0 1}
            MouseScrollLeft -> positioned {inputScroll = V2 (-1) 0}
            MouseScrollRight -> positioned {inputScroll = V2 1 0}
