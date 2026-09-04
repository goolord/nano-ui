{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Rgfw.Session
  ( RgfwOptions (..)
  , defaultRgfwOptions
  , runRgfwSession
  , runRgfwSessionReduce
  , runRgfwSessionReduceCustom
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Control.Monad (void, when)
import Data.Bits ((.&.))
import Data.Char (chr, isPrint, ord, toLower)
import Data.IORef
  ( IORef
  , newIORef
  , readIORef
  , writeIORef
  )
import qualified Data.Text as T
import Data.Typeable (Typeable)
import qualified Data.Vector as V
import Data.Word (Word8, Word32)
import Foreign.Ptr (Ptr)
import GHC.Clock (getMonotonicTime)
import NanoUI
  ( Input (..)
  , Key (..)
  , Modifiers (..)
  , NanoUI
  , Size (..)
  , V2 (..)
  , emptyInput
  )
import NanoUI.Context
  ( Context (..)
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
  ( SessionDriver (..)
  , runSessionLoop
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

      modelRef <- newIORef initialModel
      scaleRef <- newIORef initScale
      winSizeRef <- newIORef (initPhysW, initPhysH)

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

      R.withEventBuffer $ \evPtr -> do
        let drv =
              SessionDriver
                { sdPollEvents    = pollRgfwEvents win evPtr scaleRef monScaleRef winSizeRef
                , sdWaitEvents    = \_ -> pollRgfwEvents win evPtr scaleRef monScaleRef winSizeRef
                , sdApplyEvent    = applyRgfwEvent
                , sdIsButtonEdge  = isRgfwButtonEdge
                , sdIsHardQuit    = \_ -> False
                , sdIsSessionQuit = isRgfwSessionQuit
                , sdSyncDisplay   = \c inp -> do
                    (curWinW, curWinH) <- R.windowSize win
                    (pw0, ph0) <- readIORef winSizeRef
                    let (pw, ph) = if curWinW > 0 && curWinH > 0 then (curWinW, curWinH) else (pw0, ph0)
                    writeIORef winSizeRef (pw, ph)
                    curMonScale <- readIORef monScaleRef
                    curModel <- readIORef modelRef
                    let (_, userScale) = getThemeAndScale curModel
                        !newScale = resolveScale userScale curMonScale
                    writeIORef scaleRef newScale
                    let !lw = max 1 (round (fromIntegral pw / newScale) :: Int)
                        !lh = max 1 (round (fromIntegral ph / newScale) :: Int)
                    physSurf <- readIORef physSurfRef
                    physSurf' <- resizeRgfwSurface win physSurf pw ph
                    writeIORef physSurfRef physSurf'
                    pure (c, inp { inputWindowSize = Size (fromIntegral lw) (fromIntegral lh) })
                , sdWaitTimeout   = \_ _ -> pure 0
                , sdShouldDraw    = \_ _ _ _ -> pure True
                , sdDraw          = \c curInp _ -> do
                    tUiStart <- getMonotonicTime
                    curModel <- readIORef modelRef
                    (_, newModel, _, _, _) <-
                      runFrameReduceEff runEff updateModel c curInp curModel view
                    writeIORef modelRef newModel
                    tUiEnd <- getMonotonicTime
                    let !uiMs = (tUiEnd - tUiStart) * 1000.0

                    tRenderStart <- getMonotonicTime
                    curMonScale <- readIORef monScaleRef
                    curScale <- readIORef scaleRef
                    (pw, ph) <- readIORef winSizeRef
                    physSurf' <- readIORef physSurfRef
                    let (curTheme, _) = getThemeAndScale newModel
                    clearScreen physSurf' (packColor (thBackground curTheme))
                    hotId <- readIORef (ctxLastHotId c)
                    activeId <- readIORef (ctxActiveId c)
                    focusId <- readIORef (ctxFocusId c)
                    let na = ctxNodeArena c
                    count <- arenaCount na
                    renderArena physSurf' font curScale curTheme c na hotId activeId focusId
                    renderTextEditMenuOverlay physSurf' font curScale curTheme c (inputMousePos curInp)
                    tRenderEnd <- getMonotonicTime
                    let !renderMs = (tRenderEnd - tRenderStart) * 1000.0

                    tBlitStart <- getMonotonicTime
                    R.blitSurface win (sRgfwSurface physSurf')
                    tBlitEnd <- getMonotonicTime
                    let !blitMs = (tBlitEnd - tBlitStart) * 1000.0
                        !frameMs = (tBlitEnd - tUiStart) * 1000.0

                    let Size lw lh = inputWindowSize curInp
                    notePresent
                      debugSampler
                      uiMs
                      renderMs
                      blitMs
                      frameMs
                      count
                      lw
                      lh
                      pw
                      ph
                      curScale
                      curMonScale

                    let !targetFrameUs = 8333 :: Int
                        !elapsedUs = round (frameMs * 1000.0)
                        !delayUs = max 0 (targetFrameUs - elapsedUs)
                    when (delayUs > 0) $ threadDelay delayUs
                    pure (False, curInp)
                , sdSkip          = \_ _ -> pure ()
                , sdOnCursor      = \c curInp -> do
                    curKind <- uiCursorKind c curInp
                    let cursorIcon = mapRgfwCursor curKind
                    if cursorIcon == R.rgfw_mouseArrow
                      then void (R.setMouseDefault win)
                      else void (R.setMouseStandard win cursorIcon)
                , sdNoteLoop      = \_ -> noteLoop debugSampler
                , sdShouldQuit    = \_ -> False
                , sdClickDistance = 5.0
                , sdClickTime     = 0.4
                }
        let cleanup = do
              finalPhysSurf <- readIORef physSurfRef
              freeRgfwSurface finalPhysSurf
              R.closeWindow win
        runSessionLoop drv ctx initInp `finally` cleanup

data RgfwEvent
  = RgfwEvClose
  | RgfwEvResize !Int !Int
  | RgfwEvScale !Float
  | RgfwEvMotion !Float !Float
  | RgfwEvButton !Word8 !Bool
  | RgfwEvScroll !Float !Float
  | RgfwEvKeyChar !Char
  | RgfwEvKeyPress !Word32 !Word8
  | RgfwEvKeyRelease !Word32 !Word8

pollRgfwEvents :: R.Window -> Ptr R.RGFW_event -> IORef Float -> IORef Float -> IORef (Int, Int) -> IO [RgfwEvent]
pollRgfwEvents win evPtr scaleRef monScaleRef winSizeRef = do
  s <- readIORef scaleRef
  go s []
  where
    go s acc = do
      ev <- R.pollEvent win evPtr
      case ev of
        R.EventNone -> pure (reverse acc)
        R.EventWindowClose -> go s (RgfwEvClose : acc)
        R.EventWindowResize nw nh -> do
          writeIORef winSizeRef (nw, nh)
          go s (RgfwEvResize nw nh : acc)
        R.EventScaleUpdate sx _ -> do
          let !validScale = if sx > 0 then sx else 1
          writeIORef monScaleRef validScale
          go s (RgfwEvScale validScale : acc)
        R.EventMouseMotion mx my ->
          go s (RgfwEvMotion (fromIntegral mx / s) (fromIntegral my / s) : acc)
        R.EventMouseButton btn down -> go s (RgfwEvButton btn down : acc)
        R.EventMouseScroll dx dy -> go s (RgfwEvScroll dx dy : acc)
        R.EventKeyChar ch -> go s (RgfwEvKeyChar ch : acc)
        R.EventKeyPress k m -> go s (RgfwEvKeyPress k m : acc)
        R.EventKeyRelease k m -> go s (RgfwEvKeyRelease k m : acc)
        _ -> go s acc

applyRgfwEvent :: Input -> RgfwEvent -> Input
applyRgfwEvent inp ev = case ev of
  RgfwEvClose -> inp
  RgfwEvResize _ _ -> inp
  RgfwEvScale _ -> inp
  RgfwEvMotion x y -> inp {inputMousePos = V2 x y}
  RgfwEvButton btn isDown ->
    if btn == R.rgfw_mouseLeft
      then let wasDown = inputMouseDown inp
            in inp {inputMouseDown = isDown, inputMousePressed = isDown && not wasDown, inputMouseReleased = not isDown && wasDown}
      else if btn == R.rgfw_mouseRight
        then let wasDown = inputMouseRightDown inp
              in inp {inputMouseRightDown = isDown, inputMouseRightPressed = isDown && not wasDown, inputMouseRightReleased = not isDown && wasDown}
        else inp
  RgfwEvScroll dx dy -> inp {inputScroll = V2 dx dy}
  RgfwEvKeyChar ch ->
    let isCtrl = ch >= '\x01' && ch <= '\x1a'
        eff = if isCtrl then chr (ord ch + 96) else ch
     in if (isPrint ch || isCtrl) && ch /= '\177' && ch /= '\b'
          then let cur = inputChars inp
                   already = not (T.null cur) && T.last cur == eff
                   curMods = inputModifiers inp
                   mods = if isCtrl then curMods {modCtrl = True} else curMods
                in if already
                     then inp {inputModifiers = mods}
                     else inp {inputChars = T.snoc cur eff, inputModifiers = mods}
          else inp
  RgfwEvKeyPress k m ->
    let shift = (m .&. 16) /= 0
        ctrl  = (m .&. 4) /= 0 || (m .&. 32) /= 0
        alt   = (m .&. 8) /= 0
        mods  = Modifiers shift ctrl alt
        curKeys = inputKeys inp
        newKeys = case mapRgfwKey k of
          Just mk -> V.snoc curKeys mk
          Nothing -> curKeys
        curChars = inputChars inp
        newChars =
          if ctrl && ((k >= 65 && k <= 90) || (k >= 97 && k <= 122))
            then let !c = toLower (chr (fromIntegral k))
                  in if not (T.null curChars) && T.last curChars == c then curChars else T.snoc curChars c
            else curChars
     in inp {inputKeys = newKeys, inputChars = newChars, inputModifiers = mods}
  RgfwEvKeyRelease _k m ->
    let shift = (m .&. 16) /= 0
        ctrl  = (m .&. 4) /= 0 || (m .&. 32) /= 0
        alt   = (m .&. 8) /= 0
     in inp {inputModifiers = Modifiers shift ctrl alt}

isRgfwButtonEdge :: RgfwEvent -> Bool
isRgfwButtonEdge (RgfwEvButton {}) = True
isRgfwButtonEdge _ = False

isRgfwSessionQuit :: RgfwEvent -> Bool
isRgfwSessionQuit RgfwEvClose = True
isRgfwSessionQuit _ = False
