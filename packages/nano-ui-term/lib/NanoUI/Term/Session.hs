{-# LANGUAGE CPP #-}

-- | Terminal session loop: event read, frame draw, cell present.
module NanoUI.Term.Session
  ( withTermSession
  , termContextIO
  ) where

#if defined(mingw32_HOST_OS)
import Control.Exception (finally)
#endif
import Control.Monad (when)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Text as T
import GHC.Clock (getMonotonicTime)
import NanoUI.Runner (SessionDriver (..), runSessionLoop)
import NanoUI
  ( Input (..)
  , Modifiers (..)
  , Size (..)
  , appendInputKey
  , asciiIcons
  , defaultTheme
  , emptyInput
  , V2 (..)
  )
import NanoUI.Testing
  ( Context
  , DrawData
  , ctxIcons
  , ctxTheme
  , withIcons
  , withTheme
  , withHostProfile
  , HostProfile (CellHost)
  , setHost
  , anyAnimating
  , collectRasterSpans
  , ctxSpanBase
  , ctxSpanOverlay
  , spanArenaCount
  , isDirty
  , debugPanelOpen
  , needsRedrawIdle
  , pointerDragActive
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
import NanoUI.Term.Palette (queryTerminalColors, terminalTheme, terminalThemeFromColors)
import NanoUI.Term.Cells
  ( Cells
  , rasterizeLayeredArena
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

idleBlock :: Int
idleBlock = -1

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
  let inp0 =
        emptyInput
          { inputWindowSize = Size (fromIntegral w0) (fromIntegral h0)
          }
      drv =
        SessionDriver
          { sdPollEvents    = pure []
          , sdWaitEvents    = readEvents
          , sdApplyEvent    = applyEvent
          , sdIsButtonEdge  = isButtonEdge
          , sdIsHardQuit    = isHardQuit
          , sdIsSessionQuit = const False
          , sdSyncDisplay   = \c i -> pure (c, i)
          , sdWaitTimeout   = \c wasAnim -> do
              anim <- anyAnimating c
              dirty <- isDirty c
              dragging <- pointerDragActive c
              if dirty && not dragging
                then pure 0
                else if anim || wasAnim
                  then pure animateTimeout
                  else pure idleBlock
          , sdShouldDraw    = \c prevI curI _ -> do
              debugLive <- debugPanelOpen c
              wantDebug <- takeDebugLive debugRef debugLive
              need <- needsRedrawIdle c prevI curI
              pure (need || wantDebug)
          , sdDraw          = \c curI _ -> do
              t0 <- getMonotonicTime
              drawData <- runOnce c curI
              _ <- collectRasterSpans c curI
              nodes <- widgetNodeCount c
              nBase <- spanArenaCount (ctxSpanBase c)
              nOver <- spanArenaCount (ctxSpanOverlay c)
              let Size w h = inputWindowSize curI
                  stats =
                    TermDrawStats
                      { tdsNodes = nodes
                      , tdsBaseSpans = nBase
                      , tdsOverlaySpans = nOver
                      }
              cells <-
                rasterizeLayeredArena
                  (round w)
                  (round h)
                  drawData
                  (ctxSpanBase c)
                  (ctxSpanOverlay c)
              before <- readIORef cellsRef
              let blitted = before /= Just cells
              when blitted $ do
                present before cells
                writeIORef cellsRef (Just cells)
              t1 <- getMonotonicTime
              notePresent debugRef ((t1 - t0) * 1000) drawData stats blitted
              pure (False, curI)
          , sdSkip          = \_ _ -> noteSkip debugRef
          , sdOnCursor      = \_ _ -> pure ()
          , sdNoteLoop      = noteLoop debugRef
          , sdShouldQuit    = shouldQuit
          , sdClickDistance = 1.5
          , sdClickTime     = 0.4
          }
  runSessionLoop drv ctx inp0

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

applyEvent :: Input -> TermEvent -> Input
applyEvent inp ev =
  case ev of
    EvResize w h -> inp {inputWindowSize = Size (fromIntegral w) (fromIntegral h)}
    EvKey k mods -> inp {inputKeys = appendInputKey k (inputKeys inp), inputModifiers = mods}
    EvChar c mods -> inp {inputChars = inputChars inp <> T.singleton c, inputModifiers = mods}
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
