{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Rgfw.Session
  ( RgfwOptions (..)
  , defaultRgfwOptions
  , runRgfwSession
  , runRgfwSessionReduce
  , runRgfwSessionReduceCustom
  , detectWindowResizeEdge
  , cursorIconForResizeEdge
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Control.Monad (void, when)
import Data.Bits ((.&.))
import Data.Char (chr, isPrint, ord, toLower)
import Data.IORef
  ( modifyIORef'
  , newIORef
  , readIORef
  , writeIORef
  )
import qualified Data.Text as T
import Data.Typeable (Typeable)
import qualified Data.Vector as V
import Data.Word (Word8, Word32)
import GHC.Clock (getMonotonicTime)
import NanoUI
  ( Input (..)
  , Key (..)
  , Modifiers (..)
  , NanoUI
  , Rect (..)
  , Size (..)
  , V2 (..)
  , emptyInput
  )
import NanoUI.Input (clearEphemeral)
import NanoUI.Context
  ( Context (..)
  , WindowResizeEdge (..)
  , setHost
  , withClipboard
  , withFontMetrics
  )
import NanoUI.Testing
  ( UiCursorKind (..)
  , newPixelContext
  , runEff
  , runFrameReduceEff
  , uiCursorKind
  )
import NanoUI.Runner
  ( checkHardQuit
  , checkSessionQuit
  , newClickTracker
  , stampClicks
  , stepDeltaTime
  )
import NanoUI.Layout.Arena (arenaCount)
import NanoUI.Rgfw.Debug
  ( RgfwDebugHost (..)
  , newRgfwDebugSampler
  , noteLoop
  , notePresent
  )
import NanoUI.Rgfw.Font.Cozette (cozetteMetrics, getCozetteFont)
import NanoUI.Rgfw.Render (renderArena, renderTextEditMenuOverlay)
import NanoUI.Rgfw.Surface
  ( RgfwSurface (..)
  , clearScreen
  , freeRgfwSurface
  , newRgfwSurface
  , packColor
  , resizeRgfwSurface
  )
import NanoUI.Rgfw.Theme (RgfwTheme (..), defaultDarkTheme)
import qualified RGFW as R

data RgfwOptions = RgfwOptions
  { optTitle  :: !String
  , optWidth  :: !Int
  , optHeight :: !Int
  , optTheme  :: !RgfwTheme
  , optCenter :: !Bool
  , optScale  :: !Float
  }

defaultRgfwOptions :: RgfwOptions
defaultRgfwOptions =
  RgfwOptions
    { optTitle  = "nano-ui (RGFW Single-Pass)"
    , optWidth  = 1680
    , optHeight = 1040
    , optTheme  = defaultDarkTheme
    , optCenter = True
    , optScale  = 0.0
    }

-- | Determines if a mouse coordinate falls on the resize handles/borders of a window.
detectWindowResizeEdge :: Rect -> V2 -> Maybe WindowResizeEdge
detectWindowResizeEdge (Rect wx wy ww wh) (V2 mx my) =
  if mx >= wx + ww - 26.0 && mx <= wx + ww + 4.0 && my >= wy && my <= wy + 24.0
    then Nothing
    else
      let !s = 8.0
          !b = 6.0
          !grip = 18.0
          !inOuterHalo = mx >= wx - s && mx <= wx + ww + s && my >= wy - s && my <= wy + wh + s
       in if not inOuterHalo
            then Nothing
            else
              if mx >= wx + ww - grip && mx <= wx + ww + s && my >= wy + wh - grip && my <= wy + wh + s
                then Just ResizeSE
              else if mx >= wx - s && mx <= wx + grip && my >= wy + wh - grip && my <= wy + wh + s
                then Just ResizeSW
              else if (mx >= wx - s && mx <= wx + grip && my >= wy - s && my < wy)
                      || (mx >= wx - s && mx < wx && my >= wy - s && my <= wy + grip)
                then Just ResizeNW
              else if mx >= wx + ww - grip && mx <= wx + ww + s && my >= wy - s && my < wy
                then Just ResizeNE
              else if mx >= wx && mx <= wx + ww && my >= wy + wh - b && my <= wy + wh + s
                then Just ResizeS
              else if mx >= wx + ww - b && mx <= wx + ww + s && my >= wy + 24.0 && my <= wy + wh
                then Just ResizeE
              else if mx >= wx - s && mx <= wx + b && my >= wy + b && my <= wy + wh
                then Just ResizeW
              else if mx >= wx && mx <= wx + ww && my >= wy - s && my < wy
                then Just ResizeN
              else if mx > wx + ww && mx <= wx + ww + s && my >= wy && my <= wy + wh
                then Just ResizeE
              else if mx >= wx - s && mx < wx && my >= wy && my <= wy + wh
                then Just ResizeW
              else if my > wy + wh && my <= wy + wh + s && mx >= wx && mx <= wx + ww
                then Just ResizeS
              else Nothing

cursorIconForResizeEdge :: WindowResizeEdge -> Word8
cursorIconForResizeEdge edge = case edge of
  ResizeE  -> R.rgfw_mouseResizeEW
  ResizeW  -> R.rgfw_mouseResizeEW
  ResizeN  -> R.rgfw_mouseResizeNS
  ResizeS  -> R.rgfw_mouseResizeNS
  ResizeNW -> R.rgfw_mouseResizeNWSE
  ResizeSE -> R.rgfw_mouseResizeNWSE
  ResizeNE -> R.rgfw_mouseResizeNESW
  ResizeSW -> R.rgfw_mouseResizeNESW

mapRgfwKey :: Word32 -> Maybe Key
mapRgfwKey k =
  case k of
    8   -> Just KeyBackspace
    127 -> Just KeyDelete
    10  -> Just KeyEnter
    13  -> Just KeyEnter
    27  -> Just KeyEscape
    9   -> Just KeyTab
    162 -> Just KeyUp
    163 -> Just KeyDown
    164 -> Just KeyLeft
    165 -> Just KeyRight
    168 -> Just KeyEnd
    169 -> Just KeyHome
    _   -> Nothing

mapRgfwCursor :: UiCursorKind -> Word8
mapRgfwCursor kind = case kind of
  UiCursorDefault    -> R.rgfw_mouseArrow
  UiCursorPointer    -> R.rgfw_mousePointingHand
  UiCursorText       -> R.rgfw_mouseIbeam
  UiCursorGrab       -> R.rgfw_mouseArrow
  UiCursorGrabbing   -> R.rgfw_mouseArrow
  UiCursorNsResize   -> R.rgfw_mouseResizeNS
  UiCursorEwResize   -> R.rgfw_mouseResizeEW
  UiCursorNwseResize -> R.rgfw_mouseResizeNWSE
  UiCursorNeswResize -> R.rgfw_mouseResizeNESW

runRgfwSession :: RgfwOptions -> NanoUI () -> IO ()
runRgfwSession opts app = runRgfwSessionReduce opts (\() m -> m) () (\_ -> app)

runRgfwSessionReduce ::
  (Typeable msg, Eq model) =>
  RgfwOptions ->
  (msg -> model -> model) ->
  model ->
  (model -> NanoUI ()) ->
  IO ()
runRgfwSessionReduce opts =
  runRgfwSessionReduceCustom opts (\_ -> (optTheme opts, optScale opts))

runRgfwSessionReduceCustom ::
  (Typeable msg, Eq model) =>
  RgfwOptions ->
  (model -> (RgfwTheme, Float)) ->
  (msg -> model -> model) ->
  model ->
  (model -> NanoUI ()) ->
  IO ()
runRgfwSessionReduceCustom opts getThemeAndScale updateModel initialModel view = do
  let flags = if optCenter opts then R.rgfw_windowCenter else 0
  mWin <- R.createWindow (optTitle opts) 0 0 (optWidth opts) (optHeight opts) flags
  case mWin of
    Nothing -> putStrLn "Failed to create RGFW window."
    Just win -> do
      monScaleInit <- R.windowScale win
      let !initMonScale = if monScaleInit > 0.0 then monScaleInit else 1.0
      monScaleRef <- newIORef initMonScale

      let resolveScale !userScale !monScale =
            if userScale > 0.0
              then userScale
              else if optScale opts > 0.0
                then optScale opts
                else if monScale > 0.0
                  then monScale
                  else 1.0

      let (_, initScaleChoice) = getThemeAndScale initialModel
          !initScale = resolveScale initScaleChoice initMonScale
          !initPhysW = optWidth opts
          !initPhysH = optHeight opts
          !initLogW = max 1 (round (fromIntegral initPhysW / initScale) :: Int)
          !initLogH = max 1 (round (fromIntegral initPhysH / initScale) :: Int)

      physSurf0 <- newRgfwSurface win initPhysW initPhysH
      physSurfRef <- newIORef physSurf0

      runningRef <- newIORef True
      modelRef <- newIORef initialModel
      scaleRef <- newIORef initScale
      winSizeRef <- newIORef (initPhysW, initPhysH)
      clickTracker <- newClickTracker

      clipRef <- newIORef ("" :: T.Text)
      let getClip = do
            t <- readIORef clipRef
            pure (if T.null t then Nothing else Just t)
          setClip t = do
            writeIORef clipRef t
            pure True

      ctx0 <- newPixelContext
      let ctx = withClipboard (withFontMetrics ctx0 cozetteMetrics) getClip setClip
      debugSampler <- newRgfwDebugSampler
      setHost ctx (RgfwDebugHost debugSampler)
      let font = getCozetteFont

      let initInp =
            emptyInput
              { inputWindowSize = Size (fromIntegral initLogW) (fromIntegral initLogH)
              }
      inpRef <- newIORef initInp
      lastTRef <- getMonotonicTime >>= newIORef

      R.withEventBuffer $ \evPtr -> do
        let loop = do
              running <- readIORef runningRef
              when running $ do
                curScale <- readIORef scaleRef
                lastT <- readIORef lastTRef
                (tNow, dt) <- stepDeltaTime lastT
                writeIORef lastTRef tNow
                noteLoop debugSampler

                -- 1. Poll and process RGFW events into Input
                let pollEvents = do
                      ev <- R.pollEvent win evPtr
                      case ev of
                        R.EventNone -> pure ()
                        R.EventWindowClose -> do
                          writeIORef runningRef False
                          pollEvents
                        R.EventWindowResize nw nh -> do
                          writeIORef winSizeRef (nw, nh)
                          pollEvents
                        R.EventScaleUpdate sx _sy -> do
                          let !validScale = if sx > 0.0 then sx else 1.0
                          writeIORef monScaleRef validScale
                          pollEvents
                        R.EventMouseMotion mx my -> do
                          let !lmx = fromIntegral mx / curScale
                              !lmy = fromIntegral my / curScale
                          modifyIORef' inpRef $ \inp ->
                            inp {inputMousePos = V2 lmx lmy}
                          pollEvents
                        R.EventMouseButton btn pressed -> do
                          let isDown = pressed
                          if btn == R.rgfw_mouseLeft
                            then do
                              modifyIORef' inpRef $ \inp ->
                                let wasDown = inputMouseDown inp
                                 in inp
                                      { inputMouseDown = isDown
                                      , inputMousePressed = isDown && not wasDown
                                      , inputMouseReleased = not isDown && wasDown
                                      }
                            else if btn == R.rgfw_mouseRight
                              then do
                                modifyIORef' inpRef $ \inp ->
                                  let wasDown = inputMouseRightDown inp
                                   in inp
                                        { inputMouseRightDown = isDown
                                        , inputMouseRightPressed = isDown && not wasDown
                                        , inputMouseRightReleased = not isDown && wasDown
                                        }
                              else pure ()
                          pollEvents
                        R.EventMouseScroll dx dy -> do
                          modifyIORef' inpRef $ \i -> i {inputScroll = V2 dx dy}
                          pollEvents
                        R.EventKeyChar ch -> do
                          let isCtrlChar = ch >= '\x01' && ch <= '\x1a'
                              effectiveCh =
                                if isCtrlChar
                                  then chr (ord ch + 96)
                                  else ch
                          when ((isPrint ch || isCtrlChar) && ch /= '\177' && ch /= '\b') $
                            modifyIORef' inpRef $ \inp ->
                              let cur = inputChars inp
                                  alreadyPresent = not (T.null cur) && T.last cur == effectiveCh
                                  curMods = inputModifiers inp
                                  mods = if isCtrlChar then curMods {modCtrl = True} else curMods
                               in if alreadyPresent
                                    then inp {inputModifiers = mods}
                                    else inp {inputChars = T.snoc cur effectiveCh, inputModifiers = mods}
                          pollEvents
                        R.EventKeyPress k m -> do
                          let shift = (m .&. 16) /= 0
                              ctrl  = (m .&. 4) /= 0 || (m .&. 32) /= 0
                              alt   = (m .&. 8) /= 0
                              mods  = Modifiers shift ctrl alt
                          modifyIORef' inpRef $ \inp ->
                            let curKeys = inputKeys inp
                                newKeys = case mapRgfwKey k of
                                  Just mk -> V.snoc curKeys mk
                                  Nothing -> curKeys
                                curChars = inputChars inp
                                newChars =
                                  if ctrl && ((k >= 65 && k <= 90) || (k >= 97 && k <= 122))
                                    then
                                      let !c = toLower (chr (fromIntegral k))
                                       in if not (T.null curChars) && T.last curChars == c
                                            then curChars
                                            else T.snoc curChars c
                                    else curChars
                             in inp {inputKeys = newKeys, inputChars = newChars, inputModifiers = mods}
                          pollEvents
                        R.EventKeyRelease _k m -> do
                          let shift = (m .&. 16) /= 0
                              ctrl  = (m .&. 4) /= 0 || (m .&. 32) /= 0
                              alt   = (m .&. 8) /= 0
                              mods  = Modifiers shift ctrl alt
                          modifyIORef' inpRef $ \inp -> inp {inputModifiers = mods}
                          pollEvents
                        _ ->
                          pollEvents
                pollEvents

                -- 2. Check resize / scale changes
                curModel <- readIORef modelRef
                curMonScale <- readIORef monScaleRef
                let (curTheme, userScale) = getThemeAndScale curModel
                    !newScale = resolveScale userScale curMonScale

                (curWinW, curWinH) <- R.windowSize win
                (pw0, ph0) <- readIORef winSizeRef
                let (pw, ph) = if curWinW > 0 && curWinH > 0 then (curWinW, curWinH) else (pw0, ph0)
                writeIORef winSizeRef (pw, ph)
                writeIORef scaleRef newScale
                let !lw = max 1 (round (fromIntegral pw / newScale) :: Int)
                    !lh = max 1 (round (fromIntegral ph / newScale) :: Int)

                physSurf <- readIORef physSurfRef
                physSurf' <- resizeRgfwSurface win physSurf pw ph
                writeIORef physSurfRef physSurf'

                rawInp <- readIORef inpRef
                stampedInp <- stampClicks clickTracker rawInp
                let curInp =
                      stampedInp
                        { inputWindowSize = Size (fromIntegral lw) (fromIntegral lh)
                        , inputDeltaTime = dt
                        }

                -- 3. Hard quit & quit check
                hardQuit <- checkHardQuit ctx curInp
                shouldTerm <- checkSessionQuit ctx (\_ -> False) curInp
                if hardQuit || shouldTerm
                  then writeIORef runningRef False
                  else do
                    -- 4. Execute UI Frame using core runFrameReduceEff
                    tUiStart <- getMonotonicTime
                    (_, newModel, _, _, _) <-
                      runFrameReduceEff runEff updateModel ctx curInp curModel view
                    writeIORef modelRef newModel
                    tUiEnd <- getMonotonicTime
                    let !uiMs = (tUiEnd - tUiStart) * 1000.0

                    -- 5. Render directly to physical software surface
                    tRenderStart <- getMonotonicTime
                    clearScreen physSurf' (packColor (thBackground curTheme))
                    hotId <- readIORef (ctxLastHotId ctx)
                    activeId <- readIORef (ctxActiveId ctx)
                    focusId <- readIORef (ctxFocusId ctx)
                    let na = ctxNodeArena ctx
                    count <- arenaCount na
                    renderArena physSurf' font newScale curTheme ctx na hotId activeId focusId

                    -- Topmost context menu overlay
                    renderTextEditMenuOverlay physSurf' font newScale curTheme ctx (inputMousePos curInp)

                    tRenderEnd <- getMonotonicTime
                    let !renderMs = (tRenderEnd - tRenderStart) * 1000.0

                    -- 6. Blit to screen
                    tBlitStart <- getMonotonicTime
                    R.blitSurface win (sRgfwSurface physSurf')
                    tBlitEnd <- getMonotonicTime
                    let !blitMs = (tBlitEnd - tBlitStart) * 1000.0
                        !frameMs = (tBlitEnd - tNow) * 1000.0

                    -- 7. Cursor update
                    curKind <- uiCursorKind ctx curInp
                    let cursorIcon = mapRgfwCursor curKind
                    if cursorIcon == R.rgfw_mouseArrow
                      then void (R.setMouseDefault win)
                      else void (R.setMouseStandard win cursorIcon)

                    -- 8. Note debug stats
                    notePresent
                      debugSampler
                      uiMs
                      renderMs
                      blitMs
                      frameMs
                      count
                      (fromIntegral lw)
                      (fromIntegral lh)
                      pw
                      ph
                      newScale
                      curMonScale

                    -- 9. Clear ephemeral input
                    writeIORef inpRef (clearEphemeral curInp)

                    -- 10. Frame pacing (~120 FPS adaptive target)
                    let !targetFrameUs = 8333 :: Int
                        !elapsedUs = round (frameMs * 1000.0)
                        !delayUs = max 0 (targetFrameUs - elapsedUs)
                    when (delayUs > 0) $ threadDelay delayUs

                loop
        let cleanup = do
              finalPhysSurf <- readIORef physSurfRef
              freeRgfwSurface finalPhysSurf
              R.closeWindow win
        loop `finally` cleanup
