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
import Control.Monad (forM_, void, when)
import Data.Bits ((.&.))
import Data.Char (chr, isPrint, ord, toLower)
import qualified Data.IntMap.Strict as IM
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
import Effectful (runEff)
import GHC.Clock (getMonotonicTime)
import NanoUI
  ( Input (..)
  , Key (..)
  , Modifiers (..)
  , NanoUI
  , Rect (..)
  , Size (..)
  , V2 (..)
  , WidgetId (..)
  , emptyInput
  , rectContains
  )
import NanoUI.Input (clearEphemeral)
import NanoUI.Context
  ( Context (..)
  , TextInputDrag (..)
  , TextInputMenu (..)
  , WidgetStore (..)
  , WindowResizeDrag (..)
  , WindowResizeEdge (..)
  , clearPopupConfigs
  , ctxCurrentFloatingId
  , ctxTextInputDrag
  , ctxTextInputMenu
  , ctxWindowDrag
  , ctxWindowResize
  , decodeMessages
  , drainMessages
  , getStore
  , intKey
  , lookupPopupConfig
  , seedFloatingPanel
  , setHost
  , setPrevRect
  , setStore
  , withClipboard
  , withFontMetrics
  )
import NanoUI.Frame.Window
  ( resizeFromEdge
  )
import NanoUI.Rgfw.Debug
  ( RgfwDebugHost (..)
  , newRgfwDebugSampler
  , noteLoop
  , notePresent
  )
import NanoUI.Frame.TextEdit
  ( closeTextEditMenuOnEscape
  , closeTextEditMenuOnOutsideClick
  , finalizeTextEditMenuPick
  , normalizeTextFieldClicks
  , textEditMenuRectAt
  , textEditMenuWidth
  , textWordBounds
  )
import NanoUI.Store
  ( slotAnchor
  , slotCursor
  , slotKey
  , slotTextAreaAnchorCol
  , slotTextAreaAnchorRow
  , slotTextAreaCol
  , slotTextAreaPrefCol
  , slotTextAreaRow
  , slotWinSize
  )
import NanoUI.Id (initialIdContext)
import NanoUI.Layout.Arena
  ( DirTag (..)
  , NodeType (..)
  , arenaCount
  , getClipRect
  , getDirection
  , getFirstChild
  , getMinMax
  , getNextSibling
  , getNodeType
  , getParent
  , getRect
  , getWidgetId
  , isFloatingNode
  , resetNodeArena
  , setClipRect
  , setRect
  )
import NanoUI.Monad (runUi)
import NanoUI.Rgfw.Font.Cozette (cozetteMetrics, getCozetteFont)
import NanoUI.Rgfw.Layout (getContentHeight, getContentWidth, solveSinglePassLayoutWith)
import NanoUI.Rgfw.Render (renderArena, renderTextEditMenuOverlay)
import NanoUI.Rgfw.Surface
  ( RgfwSurface (..)
  , clearScreen
  , drawRectOutline
  , fillRect
  , freeRgfwSurface
  , newRgfwSurface
  , packColor
  , resizeRgfwSurface
  , toPhysRect
  )
import NanoUI.Rgfw.Theme (RgfwTheme (..), defaultDarkTheme)
import NanoUI.Testing (newPixelContext)
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
    , optScale  = 0.0 -- 0.0 means: use the DPI reported by the OS by default
    }

isInteractiveWidget :: NodeType -> Bool
isInteractiveWidget nt =
  case nt of
    NodeButton      -> True
    NodeCheckbox    -> True
    NodeRadio       -> True
    NodeSlider      -> True
    NodeTextInput   -> True
    NodeTextArea    -> True
    NodeSelect      -> True
    NodeColorPicker -> True
    NodeTree        -> True
    _               -> False

-- | Determines if a mouse coordinate falls on the resize handles/borders of a window.
detectWindowResizeEdge :: Rect -> V2 -> Maybe WindowResizeEdge
detectWindowResizeEdge (Rect wx wy ww wh) (V2 mx my) =
  -- Close button exclusion zone: top-right corner of title bar
  if mx >= wx + ww - 26.0 && mx <= wx + ww + 4.0 && my >= wy && my <= wy + 24.0
    then Nothing
    else
      let !s = 8.0     -- Outer halo thickness
          !b = 6.0     -- Inner border thickness
          !grip = 18.0 -- Corner grip zone
          !inOuterHalo = mx >= wx - s && mx <= wx + ww + s && my >= wy - s && my <= wy + wh + s
       in if not inOuterHalo
            then Nothing
            else
              -- 1. Check corners first (both inner grip & outer halo corner)
              -- Bottom-right corner (most common resize grip)
              if mx >= wx + ww - grip && mx <= wx + ww + s && my >= wy + wh - grip && my <= wy + wh + s
                then Just ResizeSE
              -- Bottom-left corner
              else if mx >= wx - s && mx <= wx + grip && my >= wy + wh - grip && my <= wy + wh + s
                then Just ResizeSW
              -- Top-left corner (strictly outside title bar: above top or left of left edge)
              else if (mx >= wx - s && mx <= wx + grip && my >= wy - s && my < wy)
                      || (mx >= wx - s && mx < wx && my >= wy - s && my <= wy + grip)
                then Just ResizeNW
              -- Top-right corner (above or outside, not inside close button)
              else if mx >= wx + ww - grip && mx <= wx + ww + s && my >= wy - s && my < wy
                then Just ResizeNE
              -- 2. Check edges (both inner border & outer halo)
              -- Bottom edge
              else if mx >= wx && mx <= wx + ww && my >= wy + wh - b && my <= wy + wh + s
                then Just ResizeS
              -- Right edge (below title bar)
              else if mx >= wx + ww - b && mx <= wx + ww + s && my >= wy + 24.0 && my <= wy + wh
                then Just ResizeE
              -- Left edge
              else if mx >= wx - s && mx <= wx + b && my >= wy + b && my <= wy + wh
                then Just ResizeW
              -- Top edge (outside window top)
              else if mx >= wx && mx <= wx + ww && my >= wy - s && my < wy
                then Just ResizeN
              -- Outer halo flanks
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
      scrollXRef <- newIORef (0.0 :: Float)
      maxScrollXRef <- newIORef (0.0 :: Float)
      scrollYRef <- newIORef (0.0 :: Float)
      maxScrollYRef <- newIORef (0.0 :: Float)
      contentWRef <- newIORef (0.0 :: Float)
      contentHRef <- newIORef (0.0 :: Float)
      scrollDragRef <- newIORef (0 :: Int, 0.0 :: Float)
      winScrollMapRef <- newIORef (IM.empty :: IM.IntMap (Float, Float))
      winMaxScrollMapRef <- newIORef (IM.empty :: IM.IntMap (Float, Float))
      winContentSizeMapRef <- newIORef (IM.empty :: IM.IntMap (Float, Float))
      winBodyRectMapRef <- newIORef (IM.empty :: IM.IntMap Rect)
      winRectMapRef <- newIORef (IM.empty :: IM.IntMap Rect)
      activeWinScrollDragRef <- newIORef (Nothing :: Maybe (WidgetId, Int, Float))
      clickTrackRef <- newIORef (0 :: Double, V2 0 0, 1 :: Int)
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

      hotRef <- newIORef (WidgetId 0)
      activeRef <- newIORef (WidgetId 0)
      focusRef <- newIORef (WidgetId 0)

      R.withEventBuffer $ \evPtr -> do
        let loop = do
              running <- readIORef runningRef
              when running $ do
                noteLoop debugSampler
                tFrameStart <- getMonotonicTime
                curScale <- readIORef scaleRef

                -- 1. Poll and process RGFW events
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
                              curInp <- readIORef inpRef
                              let wasDown = inputMouseDown curInp
                                  isPress = isDown && not wasDown
                                  isRelease = not isDown && wasDown
                              newClicks <-
                                if isPress
                                  then do
                                    now <- getMonotonicTime
                                    (lastT, lastPos, lastClicks) <- readIORef clickTrackRef
                                    let curPos = inputMousePos curInp
                                        V2 cx cy = curPos
                                        V2 px py = lastPos
                                        close = abs (cx - px) <= 4 && abs (cy - py) <= 4
                                        quick = (now - lastT) <= 0.4
                                        clicks = if close && quick then min 3 (lastClicks + 1) else 1
                                    writeIORef clickTrackRef (now, curPos, clicks)
                                    pure clicks
                                  else pure (inputMouseClicks curInp)
                              modifyIORef' inpRef $ \inp ->
                                inp
                                  { inputMouseDown = isDown
                                  , inputMousePressed = isPress
                                  , inputMouseReleased = isRelease
                                  , inputMouseClicks = newClicks
                                  }
                            else if btn == R.rgfw_mouseRight
                              then modifyIORef' inpRef $ \inp ->
                                let wasDown = inputMouseRightDown inp
                                    isPress = isDown && not wasDown
                                    isRelease = not isDown && wasDown
                                 in inp
                                      { inputMouseRightDown = isDown
                                      , inputMouseRightPressed = isPress
                                      , inputMouseRightReleased = isRelease
                                      }
                              else pure ()
                          pollEvents
                        R.EventMouseScroll dx dy -> do
                          inp <- readIORef inpRef
                          let mouse = inputMousePos inp
                          winRects <- readIORef winRectMapRef
                          winMaxScrolls <- readIORef winMaxScrollMapRef
                          winScrolls <- readIORef winScrollMapRef
                          let findHoveredWin [] = Nothing
                              findHoveredWin ((k, r) : rest) =
                                if rectContains r mouse
                                  then Just (k, r)
                                  else findHoveredWin rest
                              winList = reverse (IM.toList winRects)
                              mWinHit = findHoveredWin winList
                          let shift = modShift (inputModifiers inp)
                          case mWinHit of
                            Just (wKey, _r) -> do
                              let (mMaxX, mMaxY) = IM.findWithDefault (0, 0) wKey winMaxScrolls
                              if mMaxY > 0 || mMaxX > 0
                                then do
                                  let (curWinX, curWinY) = IM.findWithDefault (0, 0) wKey winScrolls
                                      (effDx, effDy) =
                                        if (shift || (mMaxY <= 0 && mMaxX > 0)) && dx == 0
                                          then (dy, 0)
                                          else (dx, dy)
                                  when (mMaxX > 0 && effDx /= 0) $ do
                                    let !newX = max 0 (min mMaxX (curWinX - effDx * 64))
                                    modifyIORef' winScrollMapRef (IM.insert wKey (newX, curWinY))
                                  when (mMaxY > 0 && effDy /= 0) $ do
                                    curScroll' <- readIORef winScrollMapRef
                                    let (curX', _) = IM.findWithDefault (0, 0) wKey curScroll'
                                        !newY = max 0 (min mMaxY (curWinY - effDy * 64))
                                    modifyIORef' winScrollMapRef (IM.insert wKey (curX', newY))
                                  modifyIORef' inpRef $ \i -> i {inputScroll = V2 effDx effDy}
                                else do
                                  curScrollX <- readIORef scrollXRef
                                  maxScrollX <- readIORef maxScrollXRef
                                  curScrollY <- readIORef scrollYRef
                                  maxScrollY <- readIORef maxScrollYRef
                                  let (effDx, effDy) =
                                        if (shift || (maxScrollY <= 0 && maxScrollX > 0)) && dx == 0
                                          then (dy, 0)
                                          else (dx, dy)
                                  when (maxScrollX > 0 && effDx /= 0) $ do
                                    let !newScrollX = max 0 (min maxScrollX (curScrollX - effDx * 64))
                                    writeIORef scrollXRef newScrollX
                                  when (maxScrollY > 0 && effDy /= 0) $ do
                                    let !newScrollY = max 0 (min maxScrollY (curScrollY - effDy * 64))
                                    writeIORef scrollYRef newScrollY
                                  modifyIORef' inpRef $ \i -> i {inputScroll = V2 effDx effDy}
                            Nothing -> do
                              curScrollX <- readIORef scrollXRef
                              maxScrollX <- readIORef maxScrollXRef
                              curScrollY <- readIORef scrollYRef
                              maxScrollY <- readIORef maxScrollYRef
                              let (effDx, effDy) =
                                    if (shift || (maxScrollY <= 0 && maxScrollX > 0)) && dx == 0
                                      then (dy, 0)
                                      else (dx, dy)
                              when (maxScrollX > 0 && effDx /= 0) $ do
                                let !newScrollX = max 0 (min maxScrollX (curScrollX - effDx * 64))
                                writeIORef scrollXRef newScrollX
                              when (maxScrollY > 0 && effDy /= 0) $ do
                                let !newScrollY = max 0 (min maxScrollY (curScrollY - effDy * 64))
                                writeIORef scrollYRef newScrollY
                              modifyIORef' inpRef $ \i -> i {inputScroll = V2 effDx effDy}
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
                          when (k == 170) $ do
                            -- PageUp
                            (pw, ph) <- readIORef winSizeRef
                            if shift
                              then do
                                let !lw = fromIntegral pw / curScale
                                curScrollX <- readIORef scrollXRef
                                writeIORef scrollXRef (max 0.0 (curScrollX - lw * 0.7))
                              else do
                                let !lh = fromIntegral ph / curScale
                                curScrollY <- readIORef scrollYRef
                                writeIORef scrollYRef (max 0.0 (curScrollY - lh * 0.7))
                          when (k == 171) $ do
                            -- PageDown
                            (pw, ph) <- readIORef winSizeRef
                            if shift
                              then do
                                let !lw = fromIntegral pw / curScale
                                curScrollX <- readIORef scrollXRef
                                maxScrollX <- readIORef maxScrollXRef
                                writeIORef scrollXRef (min maxScrollX (curScrollX + lw * 0.7))
                              else do
                                let !lh = fromIntegral ph / curScale
                                curScrollY <- readIORef scrollYRef
                                maxScrollY <- readIORef maxScrollYRef
                                writeIORef scrollYRef (min maxScrollY (curScrollY + lh * 0.7))
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

                -- 2. Execute UI Frame
                stillRunning <- readIORef runningRef
                when stillRunning $ do
                  curModel <- readIORef modelRef
                  curMonScale <- readIORef monScaleRef
                  let (curTheme, userScale) = getThemeAndScale curModel
                      !newScale = resolveScale userScale curMonScale

                  -- Check for resize or DPI scale change
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

                  modifyIORef' inpRef $ \inp ->
                    inp {inputWindowSize = Size (fromIntegral lw) (fromIntegral lh)}

                  curInp <- readIORef inpRef

                  -- Handle context menu escape key
                  closeTextEditMenuOnEscape ctx curInp

                  prevHot <- readIORef hotRef

                  -- Check if mouse press is inside text edit context menu
                  mMenuBeforePress <- readIORef (ctxTextInputMenu ctx)
                  let mouse = inputMousePos curInp
                      mx = v2X mouse
                      my = v2Y mouse
                      clickedInsideMenu = case mMenuBeforePress of
                        Just m  -> rectContains (textInputMenuRect m) mouse
                        Nothing -> False

                  if inputMousePressed curInp && clickedInsideMenu
                    then do
                      -- Pick action from context menu
                      finalizeTextEditMenuPick ctx curInp
                    else do
                      -- Close menu on outside click
                      when (inputMousePressed curInp) $
                        closeTextEditMenuOnOutsideClick ctx curInp

                      -- Handle normal mouse press
                      when (inputMousePressed curInp) $ do
                        let naPrev = ctxNodeArena ctx
                        prevCount <- arenaCount naPrev

                        when (prevHot /= WidgetId 0) $ do
                          writeIORef activeRef prevHot
                          writeIORef (ctxActiveId ctx) prevHot
                          writeIORef focusRef prevHot
                          writeIORef (ctxFocusId ctx) prevHot

                          -- Update caret cursor position and selection on click
                          let findPrevHot !i
                                | i < 0 = pure Nothing
                                | otherwise = do
                                    wid <- getWidgetId naPrev i
                                    if wid == prevHot then pure (Just i) else findPrevHot (i - 1)
                          mHotIdx <- findPrevHot (prevCount - 1)
                          case mHotIdx of
                            Just idx -> do
                              nt <- getNodeType naPrev idx
                              (rx, ry, _rw, _rh) <- getRect naPrev idx
                              let key = intKey prevHot
                                  rawClicks = max 1 (inputMouseClicks curInp)
                              store <- getStore ctx
                              case nt of
                                NodeTextInput -> do
                                  let txt = IM.findWithDefault "" key (storeText store)
                                      relX = mx - (rx + 6)
                                      !charIdx = max 0 (min (T.length txt) (round (max 0 relX / 6.0))) :: Int
                                  clicks <- normalizeTextFieldClicks ctx prevHot charIdx 0 0 False rawClicks
                                  let (anchorIdx, cursorIdx) = case clicks of
                                        2 -> textWordBounds txt charIdx
                                        c | c >= 3 -> (0, T.length txt)
                                        _ -> (charIdx, charIdx)
                                  setStore ctx $ store
                                    { storeInt =
                                        IM.insert (slotKey slotCursor key) cursorIdx $
                                          IM.insert (slotKey slotAnchor key) anchorIdx (storeInt store)
                                    }
                                  writeIORef (ctxTextInputDrag ctx) (Just (TextInputDrag prevHot charIdx 0 0 False clicks))
                                NodeTextArea -> do
                                  let txt = IM.findWithDefault "" key (storeText store)
                                      relX = mx - (rx + 6)
                                      relY = my - (ry + 6)
                                      !clickRow = max 0 (floor (max 0 relY / 14.0)) :: Int
                                      linesList = T.lines txt
                                      lineCount = length linesList
                                      !clampedRow = if lineCount > 0 then min (lineCount - 1) clickRow else 0
                                      targetLine = if clampedRow < lineCount then linesList !! clampedRow else ""
                                      !clickCol = max 0 (min (T.length targetLine) (round (max 0 relX / 6.0))) :: Int
                                  clicks <- normalizeTextFieldClicks ctx prevHot 0 clampedRow clickCol True rawClicks
                                  let (aRow, aCol, cRow, cCol) = case clicks of
                                        2 ->
                                          let (lo, hi) = textWordBounds targetLine clickCol
                                           in (clampedRow, lo, clampedRow, hi)
                                        c | c >= 3 ->
                                          let endRow = max 0 (lineCount - 1)
                                              endCol = if lineCount > 0 then T.length (last linesList) else 0
                                           in (0, 0, endRow, endCol)
                                        _ ->
                                          (clampedRow, clickCol, clampedRow, clickCol)
                                  setStore ctx $ store
                                    { storeInt =
                                        IM.insert (slotKey slotTextAreaRow key) cRow $
                                          IM.insert (slotKey slotTextAreaCol key) cCol $
                                            IM.insert (slotKey slotTextAreaPrefCol key) cCol $
                                              IM.insert (slotKey slotTextAreaAnchorRow key) aRow $
                                                IM.insert (slotKey slotTextAreaAnchorCol key) aCol (storeInt store)
                                    }
                                  writeIORef (ctxTextInputDrag ctx) (Just (TextInputDrag prevHot 0 clampedRow clickCol True clicks))
                                _ -> pure ()
                            Nothing -> pure ()

                        -- Scrollbar track click
                        maxScrollY <- readIORef maxScrollYRef
                        maxScrollX <- readIORef maxScrollXRef
                        contentW <- readIORef contentWRef
                        contentH <- readIORef contentHRef
                        curScrollY <- readIORef scrollYRef
                        curScrollX <- readIORef scrollXRef
                        let hasY = maxScrollY > 0
                            hasX = maxScrollX > 0
                            trackH = fromIntegral (if hasX then lh - 8 else lh)
                            trackW = fromIntegral (if hasY then lw - 8 else lw)
                            thumbH = max 24.0 (min (trackH - 4.0) (trackH * (trackH / max 1.0 contentH)))
                            thumbW = max 24.0 (min (trackW - 4.0) (trackW * (trackW / max 1.0 contentW)))
                            maxTravelY = max 1.0 (trackH - thumbH)
                            maxTravelX = max 1.0 (trackW - thumbW)
                            thumbY = if maxScrollY > 0 then (curScrollY / maxScrollY) * maxTravelY else 0
                            thumbX = if maxScrollX > 0 then (curScrollX / maxScrollX) * maxTravelX else 0
                        if hasY && mx >= fromIntegral (lw - 12) && my < trackH
                          then do
                            let grabOff = if my >= thumbY && my <= thumbY + thumbH
                                            then my - thumbY
                                            else thumbH / 2.0
                                !newScroll = max 0 (min maxScrollY (((my - grabOff) / maxTravelY) * maxScrollY))
                            writeIORef scrollDragRef (1, grabOff)
                            writeIORef scrollYRef newScroll
                          else if hasX && my >= fromIntegral (lh - 12) && mx < trackW
                            then do
                              let grabOff = if mx >= thumbX && mx <= thumbX + thumbW
                                              then mx - thumbX
                                              else thumbW / 2.0
                                  !newScroll = max 0 (min maxScrollX (((mx - grabOff) / maxTravelX) * maxScrollX))
                              writeIORef scrollDragRef (2, grabOff)
                              writeIORef scrollXRef newScroll
                            else pure ()

                        -- Check window scrollbar track click
                        winRectsPress <- readIORef winRectMapRef
                        winMaxScrollsPress <- readIORef winMaxScrollMapRef
                        winScrollsPress <- readIORef winScrollMapRef
                        winContentSizesPress <- readIORef winContentSizeMapRef
                        winBodyRectsPress <- readIORef winBodyRectMapRef

                        let checkWinScrollbarClick [] = pure False
                            checkWinScrollbarClick ((wKey, Rect wx wy ww wh) : rest) = do
                              let (maxSX, maxSY) = IM.findWithDefault (0, 0) wKey winMaxScrollsPress
                                  (curSX, curSY) = IM.findWithDefault (0, 0) wKey winScrollsPress
                                  (wContentW, wContentH) = IM.findWithDefault (ww, wh) wKey winContentSizesPress
                                  bodyR = IM.findWithDefault (Rect wx (wy + 24.0) ww (max 0 (wh - 24.0))) wKey winBodyRectsPress
                                  bodyTop = rectY bodyR
                                  bodyH = rectH bodyR
                                  wHasY = maxSY > 0
                                  wHasX = maxSX > 0
                                  sbW = 8.0 :: Float
                                  sbH = if wHasX then max 0 (bodyH - sbW) else max 0 (bodyH - 14.0)
                                  wThumbH = max 16.0 (min (sbH - 4.0) (sbH * (sbH / max 1.0 wContentH)))
                                  wMaxTravelY = max 1.0 (sbH - wThumbH)
                                  wThumbY = bodyTop + (if maxSY > 0 then (curSY / maxSY) * wMaxTravelY else 0)
                                  vTrackRect = Rect (wx + ww - sbW - 2.0) bodyTop (sbW + 4.0) sbH
                              if wHasY && rectContains vTrackRect mouse && my < wy + wh - 18.0
                                then do
                                  let grabOff = if my >= wThumbY && my <= wThumbY + wThumbH
                                                  then my - wThumbY
                                                  else wThumbH / 2.0
                                      !newSY = max 0 (min maxSY (((my - bodyTop - grabOff) / wMaxTravelY) * maxSY))
                                  writeIORef activeWinScrollDragRef (Just (WidgetId (fromIntegral wKey), 1, grabOff))
                                  modifyIORef' winScrollMapRef (IM.insert wKey (curSX, newSY))
                                  pure True
                                else do
                                  let sbW' = if wHasY then max 0 (ww - sbW - 18.0) else max 0 (ww - 18.0)
                                      wThumbW = max 16.0 (min (sbW' - 4.0) (sbW' * (sbW' / max 1.0 wContentW)))
                                      wMaxTravelX = max 1.0 (sbW' - wThumbW)
                                      wThumbX = wx + 1.0 + (if maxSX > 0 then (curSX / maxSX) * wMaxTravelX else 0)
                                      hTrackRect = Rect wx (wy + wh - sbW - 2.0) sbW' (sbW + 4.0)
                                  if wHasX && rectContains hTrackRect mouse && mx < wx + ww - 18.0
                                    then do
                                      let grabOff = if mx >= wThumbX && mx <= wThumbX + wThumbW
                                                      then mx - wThumbX
                                                      else wThumbW / 2.0
                                          !newSX = max 0 (min maxSX (((mx - (wx + 1.0) - grabOff) / wMaxTravelX) * maxSX))
                                      writeIORef activeWinScrollDragRef (Just (WidgetId (fromIntegral wKey), 2, grabOff))
                                      modifyIORef' winScrollMapRef (IM.insert wKey (newSX, curSY))
                                      pure True
                                    else checkWinScrollbarClick rest

                        clickedWinScrollbar <- checkWinScrollbarClick (reverse (IM.toList winRectsPress))

                        when (not clickedWinScrollbar) $ do
                          -- Check window resize first, then window header drag
                          let inSubtreePrev !rootIdx !idx
                                | idx == rootIdx = pure True
                                | otherwise = do
                                    p <- getParent naPrev idx
                                    if p < 0 then pure False else inSubtreePrev rootIdx p

                              findWinResize !i
                                | i < 0 = pure Nothing
                                | otherwise = do
                                    nt <- getNodeType naPrev i
                                    if nt == NodeWindow
                                      then do
                                        (wx, wy, ww, wh) <- getRect naPrev i
                                        case detectWindowResizeEdge (Rect wx wy ww wh) mouse of
                                          Just edge -> do
                                            wid <- getWidgetId naPrev i
                                            pure (Just (wid, edge, wx, wy, ww, wh, i))
                                          Nothing -> findWinResize (i - 1)
                                      else findWinResize (i - 1)

                          mWinResizeHit <- findWinResize (prevCount - 1)
                          case mWinResizeHit of
                            Just (winWid, edge, wx, wy, ww, wh, winIdx) -> do
                              let checkChildHit !j
                                    | j < 0 = pure False
                                    | otherwise = do
                                        inSub <- inSubtreePrev winIdx j
                                        if inSub
                                          then do
                                            wid <- getWidgetId naPrev j
                                            nt <- getNodeType naPrev j
                                            (rx, ry, rw, rh) <- getRect naPrev j
                                            let hit = wid /= WidgetId 0 && isInteractiveWidget nt && rectContains (Rect rx ry rw rh) mouse
                                            if hit then pure True else checkChildHit (j - 1)
                                          else checkChildHit (j - 1)
                              isChildHit <- checkChildHit (prevCount - 1)
                              let isOuterOrGrip =
                                    mx < wx || mx > wx + ww || my < wy || my > wy + wh
                                      || (mx >= wx + ww - 18.0 && my >= wy + wh - 18.0)
                              when (isOuterOrGrip || not isChildHit) $ do
                                writeIORef (ctxCurrentFloatingId ctx) (Just winWid)
                                seedFloatingPanel ctx winWid (Rect wx wy ww wh)
                                writeIORef activeRef (WidgetId 0)
                                writeIORef (ctxActiveId ctx) (WidgetId 0)
                                (minW, minH, maxW, maxH) <- getMinMax naPrev winIdx
                                writeIORef (ctxWindowResize ctx) $
                                  Just
                                    WindowResizeDrag
                                      { wrdWidget = winWid
                                      , wrdEdge = edge
                                      , wrdGrabX = mx
                                      , wrdGrabY = my
                                      , wrdStartX = wx
                                      , wrdStartY = wy
                                      , wrdStartW = ww
                                      , wrdStartH = wh
                                      , wrdMinW = if minW > 0 then minW else 160.0
                                      , wrdMinH = if minH > 0 then minH else 80.0
                                      , wrdMaxW = if maxW > 0 then maxW else fromIntegral lw
                                      , wrdMaxH = if maxH > 0 then maxH else fromIntegral lh
                                      }
                            Nothing -> do
                              -- Check window header drag
                              let findWinDrag !i
                                    | i < 0 = pure Nothing
                                    | otherwise = do
                                        nt <- getNodeType naPrev i
                                        if nt == NodeWindow
                                          then do
                                            (wx, wy, ww, _) <- getRect naPrev i
                                            let headerRect = Rect wx wy (max 0 (ww - 26.0)) 24.0
                                            if rectContains headerRect mouse
                                              then do
                                                wid <- getWidgetId naPrev i
                                                pure (Just (wid, wx, wy, ww, i))
                                              else findWinDrag (i - 1)
                                          else findWinDrag (i - 1)
                              mWinHeader <- findWinDrag (prevCount - 1)
                              case mWinHeader of
                                Just (winWid, wx, wy, ww, winIdx) -> do
                                  let checkChildHit !j
                                        | j < 0 = pure False
                                        | otherwise = do
                                            inSub <- inSubtreePrev winIdx j
                                            if inSub
                                              then do
                                                wid <- getWidgetId naPrev j
                                                nt <- getNodeType naPrev j
                                                (rx, ry, rw, rh) <- getRect naPrev j
                                                let hit = wid /= WidgetId 0 && isInteractiveWidget nt && rectContains (Rect rx ry rw rh) mouse
                                                if hit then pure True else checkChildHit (j - 1)
                                              else checkChildHit (j - 1)
                                  isChildHit <- checkChildHit (prevCount - 1)
                                  when (not isChildHit) $ do
                                    writeIORef (ctxCurrentFloatingId ctx) (Just winWid)
                                    seedFloatingPanel ctx winWid (Rect wx wy ww 24.0)
                                    writeIORef activeRef (WidgetId 0)
                                    writeIORef (ctxActiveId ctx) (WidgetId 0)
                                    writeIORef (ctxWindowDrag ctx) (Just (winWid, mx - wx, my - wy))
                                Nothing -> pure ()

                  -- Handle right click on text input / text area to open context menu
                  when (inputMouseRightPressed curInp) $ do
                    closeTextEditMenuOnOutsideClick ctx curInp
                    let naPrev = ctxNodeArena ctx
                    prevCount <- arenaCount naPrev
                    let findTextField !i
                          | i < 0 = pure Nothing
                          | otherwise = do
                              nt <- getNodeType naPrev i
                              if nt == NodeTextInput || nt == NodeTextArea
                                then do
                                  wid <- getWidgetId naPrev i
                                  (rx, ry, rw, rh) <- getRect naPrev i
                                  if wid /= WidgetId 0 && rectContains (Rect rx ry rw rh) mouse
                                    then pure (Just (wid, nt, rx, ry, rw, rh))
                                    else findTextField (i - 1)
                                else findTextField (i - 1)
                    mHitField <- findTextField (prevCount - 1)
                    case mHitField of
                      Just (wid, nt, rx, ry, _rw, _rh) -> do
                        writeIORef focusRef wid
                        writeIORef (ctxFocusId ctx) wid
                        store <- getStore ctx
                        let key = intKey wid
                        case nt of
                          NodeTextInput -> do
                            let txt = IM.findWithDefault "" key (storeText store)
                                relX = mx - (rx + 6)
                                !charIdx = max 0 (min (T.length txt) (round (max 0 relX / 6.0))) :: Int
                                curCursor = IM.findWithDefault (T.length txt) (slotKey slotCursor key) (storeInt store)
                                curAnchor = IM.findWithDefault curCursor (slotKey slotAnchor key) (storeInt store)
                                selLo = min curAnchor curCursor
                                selHi = max curAnchor curCursor
                                insideSel = selLo < selHi && charIdx >= selLo && charIdx <= selHi
                            when (not insideSel) $
                              setStore ctx $ store
                                { storeInt =
                                    IM.insert (slotKey slotCursor key) charIdx $
                                      IM.insert (slotKey slotAnchor key) charIdx (storeInt store)
                                }
                          NodeTextArea -> do
                            let txt = IM.findWithDefault "" key (storeText store)
                                relX = mx - (rx + 6)
                                relY = my - (ry + 6)
                                !clickRow = max 0 (floor (max 0 relY / 14.0)) :: Int
                                linesList = T.lines txt
                                lineCount = length linesList
                                !clampedRow = if lineCount > 0 then min (lineCount - 1) clickRow else 0
                                targetLine = if clampedRow < lineCount then linesList !! clampedRow else ""
                                !clickCol = max 0 (min (T.length targetLine) (round (max 0 relX / 6.0))) :: Int
                                curRow = IM.findWithDefault 0 (slotKey slotTextAreaRow key) (storeInt store)
                                curCol = IM.findWithDefault 0 (slotKey slotTextAreaCol key) (storeInt store)
                                ancRow = IM.findWithDefault curRow (slotKey slotTextAreaAnchorRow key) (storeInt store)
                                ancCol = IM.findWithDefault curCol (slotKey slotTextAreaAnchorCol key) (storeInt store)
                                ((r0, c0), (r1, c1)) =
                                  if (ancRow, ancCol) <= (curRow, curCol)
                                    then ((ancRow, ancCol), (curRow, curCol))
                                    else ((curRow, curCol), (ancRow, ancCol))
                                insideSel = (ancRow, ancCol) /= (curRow, curCol)
                                              && (clampedRow, clickCol) >= (r0, c0)
                                              && (clampedRow, clickCol) <= (r1, c1)
                            when (not insideSel) $
                              setStore ctx $ store
                                { storeInt =
                                    IM.insert (slotKey slotTextAreaRow key) clampedRow $
                                      IM.insert (slotKey slotTextAreaCol key) clickCol $
                                        IM.insert (slotKey slotTextAreaPrefCol key) clickCol $
                                          IM.insert (slotKey slotTextAreaAnchorRow key) clampedRow $
                                            IM.insert (slotKey slotTextAreaAnchorCol key) clickCol (storeInt store)
                                }
                          _ -> pure ()
                        let fm = ctxFontMetrics ctx
                        menuW <- textEditMenuWidth ctx
                        let menuRect = textEditMenuRectAt (ctxHostProfile ctx) fm mx my menuW (inputWindowSize curInp)
                        writeIORef (ctxTextInputMenu ctx) (Just (TextInputMenu wid menuRect))
                      Nothing -> pure ()

                  -- Mouse Drag: Window scrollbar drag, root scrollbar drag or text selection
                  when (inputMouseDown curInp && not (inputMousePressed curInp)) $ do
                    mWinDrag <- readIORef activeWinScrollDragRef
                    case mWinDrag of
                      Just (winWid, axis, grabOff) -> do
                        let wKey = intKey winWid
                        winMaxScrolls <- readIORef winMaxScrollMapRef
                        winScrolls <- readIORef winScrollMapRef
                        winContentSizes <- readIORef winContentSizeMapRef
                        winBodyRects <- readIORef winBodyRectMapRef
                        winRects <- readIORef winRectMapRef
                        case (IM.lookup wKey winRects, IM.lookup wKey winBodyRects, IM.lookup wKey winMaxScrolls) of
                          (Just (Rect wx _wy ww _wh), Just bodyR, Just (maxSX, maxSY)) -> do
                            let (curSX, curSY) = IM.findWithDefault (0, 0) wKey winScrolls
                                (contentW, contentH) = IM.findWithDefault (ww, rectH bodyR) wKey winContentSizes
                                bodyTop = rectY bodyR
                                bodyH = rectH bodyR
                                hasX = maxSX > 0
                                hasY = maxSY > 0
                                sbW = 8.0 :: Float
                            case axis of
                              1 | maxSY > 0 -> do
                                let sbH = if hasX then max 0 (bodyH - sbW) else max 0 (bodyH - 14.0)
                                    thumbH = max 16.0 (min (sbH - 4.0) (sbH * (sbH / max 1.0 contentH)))
                                    maxTravelY = max 1.0 (sbH - thumbH)
                                    !newSY = max 0 (min maxSY (((my - bodyTop - grabOff) / maxTravelY) * maxSY))
                                modifyIORef' winScrollMapRef (IM.insert wKey (curSX, newSY))
                              2 | maxSX > 0 -> do
                                let sbW' = if hasY then max 0 (ww - sbW - 18.0) else max 0 (ww - 18.0)
                                    thumbW = max 16.0 (min (sbW' - 4.0) (sbW' * (sbW' / max 1.0 contentW)))
                                    maxTravelX = max 1.0 (sbW' - thumbW)
                                    !newSX = max 0 (min maxSX (((mx - (wx + 1.0) - grabOff) / maxTravelX) * maxSX))
                                modifyIORef' winScrollMapRef (IM.insert wKey (newSX, curSY))
                              _ -> pure ()
                          _ -> pure ()
                      Nothing -> do
                        maxScrollY <- readIORef maxScrollYRef
                        maxScrollX <- readIORef maxScrollXRef
                        (sDrag, grabOff) <- readIORef scrollDragRef
                        contentW <- readIORef contentWRef
                        contentH <- readIORef contentHRef
                        let hasY = maxScrollY > 0
                            hasX = maxScrollX > 0
                            trackH = fromIntegral (if hasX then lh - 8 else lh)
                            trackW = fromIntegral (if hasY then lw - 8 else lw)
                            thumbH = max 24.0 (min (trackH - 4.0) (trackH * (trackH / max 1.0 contentH)))
                            thumbW = max 24.0 (min (trackW - 4.0) (trackW * (trackW / max 1.0 contentW)))
                            maxTravelY = max 1.0 (trackH - thumbH)
                            maxTravelX = max 1.0 (trackW - thumbW)
                        case sDrag of
                          1 | hasY -> do
                            let !newScroll = max 0 (min maxScrollY (((my - grabOff) / maxTravelY) * maxScrollY))
                            writeIORef scrollYRef newScroll
                          2 | hasX -> do
                            let !newScroll = max 0 (min maxScrollX (((mx - grabOff) / maxTravelX) * maxScrollX))
                            writeIORef scrollXRef newScroll
                          _ -> do
                            if hasY && mx >= fromIntegral (lw - 12) && my < trackH
                              then do
                                curScrollY <- readIORef scrollYRef
                                let thumbY = if maxScrollY > 0 then (curScrollY / maxScrollY) * maxTravelY else 0
                                    grabOff' = if my >= thumbY && my <= thumbY + thumbH
                                                 then my - thumbY
                                                 else thumbH / 2.0
                                    !newScroll = max 0 (min maxScrollY (((my - grabOff') / maxTravelY) * maxScrollY))
                                writeIORef scrollDragRef (1, grabOff')
                                writeIORef scrollYRef newScroll
                              else if hasX && my >= fromIntegral (lh - 12) && mx < trackW
                                then do
                                  curScrollX <- readIORef scrollXRef
                                  let thumbX = if maxScrollX > 0 then (curScrollX / maxScrollX) * maxTravelX else 0
                                      grabOff' = if mx >= thumbX && mx <= thumbX + thumbW
                                                   then mx - thumbX
                                                   else thumbW / 2.0
                                      !newScroll = max 0 (min maxScrollX (((mx - grabOff') / maxTravelX) * maxScrollX))
                                  writeIORef scrollDragRef (2, grabOff')
                                  writeIORef scrollXRef newScroll
                                else do
                                  curActive <- readIORef activeRef
                                  when (curActive /= WidgetId 0) $ do
                                    let naPrev = ctxNodeArena ctx
                                    prevCount <- arenaCount naPrev
                                    let findActive !i
                                          | i < 0 = pure Nothing
                                          | otherwise = do
                                              wid <- getWidgetId naPrev i
                                              if wid == curActive then pure (Just i) else findActive (i - 1)
                                    mActiveIdx <- findActive (prevCount - 1)
                                    case mActiveIdx of
                                      Just idx -> do
                                        nt <- getNodeType naPrev idx
                                        (rx, ry, _rw, _rh) <- getRect naPrev idx
                                        let key = intKey curActive
                                        store <- getStore ctx
                                        case nt of
                                          NodeTextInput -> do
                                            let txt = IM.findWithDefault "" key (storeText store)
                                                relX = mx - (rx + 6)
                                                !charIdx = max 0 (min (T.length txt) (round (max 0 relX / 6.0))) :: Int
                                            mDrag <- readIORef (ctxTextInputDrag ctx)
                                            let dragClicks = maybe 1 textInputDragClicks mDrag
                                                origAnchor = maybe charIdx textInputDragAnchor mDrag
                                            case dragClicks of
                                              c | c >= 3 -> pure ()
                                              2 -> do
                                                let (a0, a1) = textWordBounds txt origAnchor
                                                    (c0, c1) = textWordBounds txt charIdx
                                                    (newAnchor, newCursor) =
                                                      if charIdx >= origAnchor
                                                        then (a0, c1)
                                                        else (a1, c0)
                                                setStore ctx $ store
                                                  { storeInt =
                                                      IM.insert (slotKey slotAnchor key) newAnchor $
                                                        IM.insert (slotKey slotCursor key) newCursor (storeInt store)
                                                  }
                                              _ -> do
                                                setStore ctx $ store
                                                  { storeInt = IM.insert (slotKey slotCursor key) charIdx (storeInt store)
                                                  }
                                          NodeTextArea -> do
                                            let txt = IM.findWithDefault "" key (storeText store)
                                                relX = mx - (rx + 6)
                                                relY = my - (ry + 6)
                                                !dragRow = max 0 (floor (max 0 relY / 14.0)) :: Int
                                                linesList = T.lines txt
                                                lineCount = length linesList
                                                !clampedRow = if lineCount > 0 then min (lineCount - 1) dragRow else 0
                                                targetLine = if clampedRow < lineCount then linesList !! clampedRow else ""
                                                !dragCol = max 0 (min (T.length targetLine) (round (max 0 relX / 6.0))) :: Int
                                            mDrag <- readIORef (ctxTextInputDrag ctx)
                                            let dragClicks = maybe 1 textInputDragClicks mDrag
                                                origARow = maybe clampedRow textInputDragAnchorRow mDrag
                                                origACol = maybe dragCol textInputDragAnchorCol mDrag
                                            case dragClicks of
                                              c | c >= 3 -> pure ()
                                              2 -> do
                                                let anchorLine = if origARow < lineCount then linesList !! origARow else ""
                                                    (a0, a1) = textWordBounds anchorLine origACol
                                                    (c0, c1) = textWordBounds targetLine dragCol
                                                    (finalARow, finalACol, finalCRow, finalCCol) =
                                                      if (clampedRow, dragCol) >= (origARow, origACol)
                                                        then (origARow, a0, clampedRow, c1)
                                                        else (origARow, a1, clampedRow, c0)
                                                setStore ctx $ store
                                                  { storeInt =
                                                      IM.insert (slotKey slotTextAreaRow key) finalCRow $
                                                        IM.insert (slotKey slotTextAreaCol key) finalCCol $
                                                          IM.insert (slotKey slotTextAreaPrefCol key) finalCCol $
                                                            IM.insert (slotKey slotTextAreaAnchorRow key) finalARow $
                                                              IM.insert (slotKey slotTextAreaAnchorCol key) finalACol (storeInt store)
                                                  }
                                              _ -> do
                                                setStore ctx $ store
                                                  { storeInt =
                                                      IM.insert (slotKey slotTextAreaRow key) clampedRow $
                                                        IM.insert (slotKey slotTextAreaCol key) dragCol $
                                                          IM.insert (slotKey slotTextAreaPrefCol key) dragCol (storeInt store)
                                                  }
                                          _ -> pure ()
                                      Nothing -> pure ()

                    -- Window resize drag
                    mWinResize <- readIORef (ctxWindowResize ctx)
                    case mWinResize of
                      Just wrd -> do
                        let (nw, nh, nx, ny) = resizeFromEdge wrd mouse (fromIntegral lw) (fromIntegral lh)
                        store <- getStore ctx
                        let wid = wrdWidget wrd
                            !k = intKey wid
                        setStore
                          ctx
                          ( store
                              { storePoint =
                                  IM.insert (slotKey slotWinSize k) (nw, nh) $
                                    IM.insert k (nx, ny) (storePoint store)
                              }
                          )
                      Nothing -> do
                        -- Window position drag
                        mWinPosDrag <- readIORef (ctxWindowDrag ctx)
                        case mWinPosDrag of
                          Just (winWid, gx, gy) -> do
                            let !newX = mx - gx
                                !newY = my - gy
                            store <- getStore ctx
                            setStore ctx (store { storePoint = IM.insert (intKey winWid) (newX, newY) (storePoint store) })
                          Nothing -> pure ()

                  when (inputMouseReleased curInp) $ do
                    writeIORef (ctxWindowDrag ctx) Nothing
                    writeIORef (ctxWindowResize ctx) Nothing
                    writeIORef (ctxTextInputDrag ctx) Nothing
                    writeIORef activeWinScrollDragRef Nothing
                    writeIORef scrollDragRef (0, 0.0)
                    curActive <- readIORef activeRef
                    when (curActive /= WidgetId 0 && curActive == prevHot) $ do
                      writeIORef (ctxClickedId ctx) curActive
                    writeIORef activeRef (WidgetId 0)
                    writeIORef (ctxActiveId ctx) (WidgetId 0)

                  -- Reset node arena and ID scopes
                  resetNodeArena (ctxNodeArena ctx)
                  writeIORef (ctxContainerStack ctx) []
                  writeIORef (ctxIdContext ctx) initialIdContext
                  clearPopupConfigs ctx

                  -- Run UI monad
                  tUiStart <- getMonotonicTime
                  runEff (runUi ctx curInp (view curModel))

                  -- Clear one-shot click
                  writeIORef (ctxClickedId ctx) (WidgetId 0)

                  -- Run single-pass layout with popup and window positioning
                  let na = ctxNodeArena ctx
                      lookupWinPos wid = getStore ctx >>= \s -> pure (IM.lookup (intKey wid) (storePoint s))
                      lookupWinSz wid = getStore ctx >>= \s -> pure (IM.lookup (slotKey slotWinSize (intKey wid)) (storePoint s))
                  solveSinglePassLayoutWith na (fromIntegral lw) (fromIntegral lh) (lookupPopupConfig ctx) lookupWinPos lookupWinSz
                  contentH <- getContentHeight na
                  contentW <- getContentWidth na
                  writeIORef contentHRef contentH
                  writeIORef contentWRef contentW
                  let !maxScrollY = if contentH > fromIntegral lh then contentH - fromIntegral lh + 16 else 0
                      !maxScrollX = if contentW > fromIntegral lw then contentW - fromIntegral lw + 16 else 0
                  writeIORef maxScrollYRef maxScrollY
                  writeIORef maxScrollXRef maxScrollX
                  modifyIORef' scrollYRef (\s -> max 0 (min maxScrollY s))
                  modifyIORef' scrollXRef (\s -> max 0 (min maxScrollX s))
                  curScrollY <- readIORef scrollYRef
                  curScrollX <- readIORef scrollXRef

                  count <- arenaCount na

                  let isFloating !idx = do
                        nt <- getNodeType na idx
                        if isFloatingNode nt
                          then pure True
                          else do
                            p <- getParent na idx
                            if p < 0 then pure False else isFloating p

                  -- Apply scroll offset to non-floating nodes if content is scrolled
                  when (curScrollX > 0 || curScrollY > 0) $ do
                    let offsetNodes !i
                          | i >= count = pure ()
                          | otherwise = do
                              p <- getParent na i
                              floating <- isFloating i
                              when (p >= 0 && not floating) $ do
                                (rx, ry, rw, rh) <- getRect na i
                                setRect na i (rx - curScrollX) (ry - curScrollY) rw rh
                                mClip <- getClipRect na i
                                case mClip of
                                  Just (Rect cx cy cw ch) ->
                                    setClipRect na i (Rect (cx - curScrollX) (cy - curScrollY) cw ch)
                                  Nothing -> pure ()
                              offsetNodes (i + 1)
                    offsetNodes 0

                  -- Window content measurement, scrolling, and clipping
                  writeIORef winRectMapRef IM.empty
                  writeIORef winMaxScrollMapRef IM.empty
                  writeIORef winContentSizeMapRef IM.empty
                  writeIORef winBodyRectMapRef IM.empty

                  let processWindows !wIdx
                        | wIdx >= count = pure ()
                        | otherwise = do
                            wNt <- getNodeType na wIdx
                            if wNt == NodeWindow || wNt == NodeModal
                              then do
                                wid <- getWidgetId na wIdx
                                let !wKey = intKey wid
                                (wx, wy, ww, wh) <- getRect na wIdx
                                modifyIORef' winRectMapRef (IM.insert wKey (Rect wx wy ww wh))

                                let findTitleChild !ci
                                      | ci < 0 = pure (-1)
                                      | otherwise = do
                                          cnt <- getNodeType na ci
                                          cdir <- getDirection na ci
                                          if cnt == NodeContainer && cdir == DirRow
                                            then pure ci
                                            else getNextSibling na ci >>= findTitleChild
                                titleChild <- findTitleChild =<< getFirstChild na wIdx
                                let !hasTitleRow = titleChild >= 0

                                let !titleBarH = if hasTitleRow then 24.0 else 0.0
                                    !bodyTop = wy + titleBarH
                                    !bodyH = max 0.0 (wh - titleBarH)
                                    !bodyRect = Rect wx bodyTop ww bodyH
                                modifyIORef' winBodyRectMapRef (IM.insert wKey bodyRect)

                                -- Helper: check if node belongs to this window (and not a nested window)
                                let belongsToWin !curr
                                      | curr < 0 = pure False
                                      | curr == wIdx = pure True
                                      | otherwise = do
                                          cnt <- getNodeType na curr
                                          if (cnt == NodeWindow || cnt == NodeModal) && curr /= wIdx
                                            then pure False
                                            else do
                                              p <- getParent na curr
                                              belongsToWin p

                                    inTitleBar !curr
                                      | not hasTitleRow = pure False
                                      | curr < 0 = pure False
                                      | curr == titleChild = pure True
                                      | curr == wIdx = pure False
                                      | otherwise = do
                                          p <- getParent na curr
                                          inTitleBar p

                                -- Pass 1: Clamp title bar clips and measure body content bounds
                                let measureLoop !j !maxX !maxY
                                      | j >= count = pure (maxX, maxY)
                                      | otherwise = do
                                          belongs <- belongsToWin j
                                          if not belongs || j == wIdx
                                            then measureLoop (j + 1) maxX maxY
                                            else do
                                              inTitle <- inTitleBar j
                                              if inTitle
                                                then do
                                                  mClip <- getClipRect na j
                                                  let !tClip = case mClip of
                                                        Just (Rect cx cy cw ch) ->
                                                          let !cx0 = max wx cx
                                                              !cy0 = max wy cy
                                                              !cx1 = min (wx + ww) (cx + cw)
                                                              !cy1 = min (wy + 24.0) (cy + ch)
                                                           in Rect cx0 cy0 (max 0 (cx1 - cx0)) (max 0 (cy1 - cy0))
                                                        Nothing -> Rect wx wy ww 24.0
                                                  setClipRect na j tClip
                                                  measureLoop (j + 1) maxX maxY
                                                else do
                                                  (jx, jy, jw, jh) <- getRect na j
                                                  if jw > 0 && jh > 0
                                                    then measureLoop (j + 1) (max maxX (jx + jw)) (max maxY (jy + jh))
                                                    else measureLoop (j + 1) maxX maxY

                                (!bMaxX, !bMaxY) <- measureLoop 0 wx bodyTop

                                let !winContentW = max 0.0 (bMaxX - wx)
                                    !winContentH = max 0.0 (bMaxY - bodyTop)
                                    !maxSX = if winContentW > ww then winContentW - ww + 16.0 else 0.0
                                    !maxSY = if winContentH > bodyH then winContentH - bodyH + 16.0 else 0.0

                                modifyIORef' winMaxScrollMapRef (IM.insert wKey (maxSX, maxSY))
                                modifyIORef' winContentSizeMapRef (IM.insert wKey (winContentW, winContentH))

                                winScrolls <- readIORef winScrollMapRef
                                let (curSX, curSY) = IM.findWithDefault (0.0, 0.0) wKey winScrolls
                                    !clampedSX = max 0.0 (min maxSX curSX)
                                    !clampedSY = max 0.0 (min maxSY curSY)
                                modifyIORef' winScrollMapRef (IM.insert wKey (clampedSX, clampedSY))

                                -- Pass 2: Shift body nodes by scroll and clip strictly to bodyRect
                                let applyScrollClip !j
                                      | j >= count = pure ()
                                      | otherwise = do
                                          belongs <- belongsToWin j
                                          if not belongs || j == wIdx
                                            then applyScrollClip (j + 1)
                                            else do
                                              inTitle <- inTitleBar j
                                              if inTitle
                                                then applyScrollClip (j + 1)
                                                else do
                                                  (jx, jy, jw, jh) <- getRect na j
                                                  let !newX = jx - clampedSX
                                                      !newY = jy - clampedSY
                                                  setRect na j newX newY jw jh
                                                  let !cx0 = max (rectX bodyRect) newX
                                                      !cy0 = max (rectY bodyRect) newY
                                                      !cx1 = min (rectX bodyRect + rectW bodyRect) (newX + jw)
                                                      !cy1 = min (rectY bodyRect + rectH bodyRect) (newY + jh)
                                                      !finalClip = Rect cx0 cy0 (max 0 (cx1 - cx0)) (max 0 (cy1 - cy0))
                                                  setClipRect na j finalClip
                                                  applyScrollClip (j + 1)

                                applyScrollClip 0
                                processWindows (wIdx + 1)
                              else processWindows (wIdx + 1)

                  processWindows 0

                  -- Hit testing & hover/active/focus resolution (popups & floating windows prioritized, no click-through)
                  let inSubtree !arena !rootIdx !idx
                        | idx == rootIdx = pure True
                        | otherwise = do
                            p <- getParent arena idx
                            if p < 0 then pure False else inSubtree arena rootIdx p

                      findTopPopup !i
                        | i < 0 = pure Nothing
                        | otherwise = do
                            nt <- getNodeType na i
                            if nt == NodePopup
                              then do
                                (rx, ry, rw, rh) <- getRect na i
                                if rectContains (Rect rx ry rw rh) (inputMousePos curInp)
                                  then pure (Just i)
                                  else findTopPopup (i - 1)
                              else findTopPopup (i - 1)

                      findTopWindow !i
                        | i < 0 = pure Nothing
                        | otherwise = do
                            nt <- getNodeType na i
                            if nt == NodeWindow || nt == NodeModal
                              then do
                                (rx, ry, rw, rh) <- getRect na i
                                if rectContains (Rect rx ry rw rh) (inputMousePos curInp)
                                  then pure (Just i)
                                  else findTopWindow (i - 1)
                              else findTopWindow (i - 1)

                  mTopPopup <- findTopPopup (count - 1)
                  mTopWindow <- findTopWindow (count - 1)
                  let mTopActive = case mTopPopup of
                        Just p  -> Just p
                        Nothing -> mTopWindow

                  newHot <- case mTopActive of
                    Just topIdx -> do
                      let findInSub !i
                            | i < 0 = pure (WidgetId 0)
                            | otherwise = do
                                inSub <- inSubtree na topIdx i
                                if inSub
                                  then do
                                    nt <- getNodeType na i
                                    wid <- getWidgetId na i
                                    (rx, ry, rw, rh) <- getRect na i
                                    mClip <- getClipRect na i
                                    let isClipped = case mClip of
                                          Just cr -> not (rectContains cr (inputMousePos curInp))
                                          Nothing -> False
                                        isInteractive =
                                          wid /= WidgetId 0
                                            && isInteractiveWidget nt
                                            && not isClipped
                                            && rectContains (Rect rx ry rw rh) (inputMousePos curInp)
                                    if isInteractive then pure wid else findInSub (i - 1)
                                  else findInSub (i - 1)
                      findInSub (count - 1)
                    Nothing -> do
                      let findHitPage !i
                            | i < 0 = pure (WidgetId 0)
                            | otherwise = do
                                floating <- isFloating i
                                if not floating
                                  then do
                                    nt <- getNodeType na i
                                    wid <- getWidgetId na i
                                    (rx, ry, rw, rh) <- getRect na i
                                    mClip <- getClipRect na i
                                    let isClipped = case mClip of
                                          Just cr -> not (rectContains cr (inputMousePos curInp))
                                          Nothing -> False
                                        isInteractive =
                                          wid /= WidgetId 0
                                            && isInteractiveWidget nt
                                            && not isClipped
                                            && rectContains (Rect rx ry rw rh) (inputMousePos curInp)
                                    if isInteractive then pure wid else findHitPage (i - 1)
                                  else findHitPage (i - 1)
                      findHitPage (count - 1)

                  mMenuHot <- readIORef (ctxTextInputMenu ctx)
                  let mouseInTextMenu = case mMenuHot of
                        Just m  -> rectContains (textInputMenuRect m) (inputMousePos curInp)
                        Nothing -> False
                  let finalHot = if mouseInTextMenu then WidgetId 0 else newHot
                  writeIORef hotRef finalHot
                  writeIORef (ctxHotId ctx) finalHot

                  curActive <- readIORef activeRef
                  curFocus <- readIORef focusRef

                  -- Update system mouse cursor based on resize or text hover
                  mWinResizeActive <- readIORef (ctxWindowResize ctx)
                  case mWinResizeActive of
                    Just wrd -> void (R.setMouseStandard win (cursorIconForResizeEdge (wrdEdge wrd)))
                    Nothing -> do
                      let findHoverResize !i
                            | i < 0 = pure Nothing
                            | otherwise = do
                                nt <- getNodeType na i
                                if nt == NodeWindow
                                  then do
                                    (wx, wy, ww, wh) <- getRect na i
                                    case detectWindowResizeEdge (Rect wx wy ww wh) (inputMousePos curInp) of
                                      Just edge -> pure (Just edge)
                                      Nothing   -> findHoverResize (i - 1)
                                  else findHoverResize (i - 1)
                      mHoverEdge <- findHoverResize (count - 1)
                      case mHoverEdge of
                        Just edge -> void (R.setMouseStandard win (cursorIconForResizeEdge edge))
                        Nothing -> do
                          let findTextHover !i
                                | i < 0 = pure False
                                | otherwise = do
                                    nt <- getNodeType na i
                                    if nt == NodeTextInput || nt == NodeTextArea
                                      then do
                                        (rx, ry, rw, rh) <- getRect na i
                                        if rectContains (Rect rx ry rw rh) (inputMousePos curInp)
                                          then pure True
                                          else findTextHover (i - 1)
                                      else findTextHover (i - 1)
                          overText <- findTextHover (count - 1)
                          if overText
                            then void (R.setMouseStandard win R.rgfw_mouseIbeam)
                            else void (R.setMouseDefault win)

                  -- Update prevRects for next frame's widget interaction query
                  let updatePrev !i
                        | i >= count = pure ()
                        | otherwise = do
                            wid <- getWidgetId na i
                            when (wid /= WidgetId 0) $ do
                              (x, y, w, h) <- getRect na i
                              setPrevRect ctx wid (Rect x y w h)
                              nt <- getNodeType na i
                              when (isFloatingNode nt) $
                                seedFloatingPanel ctx wid (Rect x y w h)
                            updatePrev (i + 1)
                  updatePrev 0

                  tUiEnd <- getMonotonicTime
                  let !uiMs = (tUiEnd - tUiStart) * 1000.0

                  -- 3. Render directly to physical software surface
                  tRenderStart <- getMonotonicTime
                  clearScreen physSurf' (packColor (thBackground curTheme))
                  renderArena physSurf' font newScale curTheme ctx na finalHot curActive curFocus

                  -- Draw retro scrollbars for floating windows
                  winRectsRender <- readIORef winRectMapRef
                  winMaxScrollsRender <- readIORef winMaxScrollMapRef
                  winScrollsRender <- readIORef winScrollMapRef
                  winContentSizesRender <- readIORef winContentSizeMapRef
                  winBodyRectsRender <- readIORef winBodyRectMapRef
                  forM_ (IM.toList winRectsRender) $ \(wKey, Rect wx wy ww wh) -> do
                    let (maxSX, maxSY) = IM.findWithDefault (0, 0) wKey winMaxScrollsRender
                        (curSX, curSY) = IM.findWithDefault (0, 0) wKey winScrollsRender
                        (wContentW, wContentH) = IM.findWithDefault (ww, wh) wKey winContentSizesRender
                        bodyR = IM.findWithDefault (Rect wx (wy + 24.0) ww (max 0 (wh - 24.0))) wKey winBodyRectsRender
                        bodyTop = rectY bodyR
                        bodyH = rectH bodyR
                        hasY = maxSY > 0
                        hasX = maxSX > 0
                        sbW = 8.0 :: Float

                    when hasY $ do
                      let sbH = if hasX then max 0 (bodyH - sbW) else max 0 (bodyH - 14.0)
                          thumbH = max 16.0 (min (sbH - 4.0) (sbH * (sbH / max 1.0 wContentH)))
                          maxTravelY = max 1.0 (sbH - thumbH)
                          thumbY = bodyTop + (curSY / maxSY) * maxTravelY
                          sbX = wx + ww - sbW - 1.0
                          (!sx, !sy, !sw, !sh) = toPhysRect newScale sbX bodyTop sbW sbH
                          (!tx, !ty, !tw, !th) = toPhysRect newScale (sbX + 1.0) (thumbY + 1.0) (sbW - 2.0) (thumbH - 2.0)
                      fillRect physSurf' sx sy sw sh (packColor (thScrollTrack curTheme))
                      drawRectOutline physSurf' sx sy sw sh (packColor (thBorder curTheme))
                      fillRect physSurf' tx ty tw th (packColor (thThumb curTheme))
                      drawRectOutline physSurf' tx ty tw th (packColor (thBorder curTheme))

                    when hasX $ do
                      let sbW' = if hasY then max 0 (ww - sbW - 18.0) else max 0 (ww - 18.0)
                          thumbW = max 16.0 (min (sbW' - 4.0) (sbW' * (sbW' / max 1.0 wContentW)))
                          maxTravelX = max 1.0 (sbW' - thumbW)
                          thumbX = wx + 1.0 + (curSX / maxSX) * maxTravelX
                          sbY = wy + wh - sbW - 1.0
                          (!sx, !sy, !sw, !sh) = toPhysRect newScale (wx + 1.0) sbY sbW' sbW
                          (!tx, !ty, !tw, !th) = toPhysRect newScale (thumbX + 1.0) (sbY + 1.0) (thumbW - 2.0) (sbW - 2.0)
                      fillRect physSurf' sx sy sw sh (packColor (thScrollTrack curTheme))
                      drawRectOutline physSurf' sx sy sw sh (packColor (thBorder curTheme))
                      fillRect physSurf' tx ty tw th (packColor (thThumb curTheme))
                      drawRectOutline physSurf' tx ty tw th (packColor (thBorder curTheme))

                    when (hasX && hasY) $ do
                      let (!cx, !cy, !cw, !ch) = toPhysRect newScale (wx + ww - sbW - 1.0) (wy + wh - sbW - 1.0) sbW sbW
                      fillRect physSurf' cx cy cw ch (packColor (thScrollTrack curTheme))
                      drawRectOutline physSurf' cx cy cw ch (packColor (thBorder curTheme))

                  -- Draw retro scrollbars if window content exceeds viewport
                  when (maxScrollY > 0) $ do
                    let !sbLogW = 8.0 :: Float
                        !sbLogX = fromIntegral lw - sbLogW
                        !sbLogH = if maxScrollX > 0 then fromIntegral lh - 8.0 else fromIntegral lh
                        !thumbLogH = max 24.0 (min (sbLogH - 4.0) (sbLogH * (sbLogH / max 1.0 contentH)))
                        !thumbLogY = (curScrollY / maxScrollY) * (sbLogH - thumbLogH)
                        (!sx, !sy, !sw, !sh) = toPhysRect newScale sbLogX 0 sbLogW sbLogH
                        (!tx, !ty, !tw, !th) = toPhysRect newScale (sbLogX + 1.0) (thumbLogY + 1.0) (sbLogW - 2.0) (thumbLogH - 2.0)
                    fillRect physSurf' sx sy sw sh (packColor (thScrollTrack curTheme))
                    drawRectOutline physSurf' sx sy sw sh (packColor (thBorder curTheme))
                    fillRect physSurf' tx ty tw th (packColor (thThumb curTheme))
                    drawRectOutline physSurf' tx ty tw th (packColor (thBorder curTheme))

                  when (maxScrollX > 0) $ do
                    let !sbLogH = 8.0 :: Float
                        !sbLogY = fromIntegral lh - sbLogH
                        !sbLogW = if maxScrollY > 0 then fromIntegral lw - 8.0 else fromIntegral lw
                        !thumbLogW = max 24.0 (min (sbLogW - 4.0) (sbLogW * (sbLogW / max 1.0 contentW)))
                        !thumbLogX = (curScrollX / maxScrollX) * (sbLogW - thumbLogW)
                        (!sx, !sy, !sw, !sh) = toPhysRect newScale 0 sbLogY sbLogW sbLogH
                        (!tx, !ty, !tw, !th) = toPhysRect newScale (thumbLogX + 1.0) (sbLogY + 1.0) (thumbLogW - 2.0) (sbLogH - 2.0)
                    fillRect physSurf' sx sy sw sh (packColor (thScrollTrack curTheme))
                    drawRectOutline physSurf' sx sy sw sh (packColor (thBorder curTheme))
                    fillRect physSurf' tx ty tw th (packColor (thThumb curTheme))
                    drawRectOutline physSurf' tx ty tw th (packColor (thBorder curTheme))

                  when (maxScrollX > 0 && maxScrollY > 0) $ do
                    let (!cx, !cy, !cw, !ch) = toPhysRect newScale (fromIntegral lw - 8.0) (fromIntegral lh - 8.0) 8.0 8.0
                    fillRect physSurf' cx cy cw ch (packColor (thScrollTrack curTheme))
                    drawRectOutline physSurf' cx cy cw ch (packColor (thBorder curTheme))

                  -- Draw text edit context menu overlay (topmost overlay)
                  renderTextEditMenuOverlay physSurf' font newScale curTheme ctx (inputMousePos curInp)

                  tRenderEnd <- getMonotonicTime
                  let !renderMs = (tRenderEnd - tRenderStart) * 1000.0

                  tBlitStart <- getMonotonicTime
                  R.blitSurface win (sRgfwSurface physSurf')
                  tBlitEnd <- getMonotonicTime
                  let !blitMs = (tBlitEnd - tBlitStart) * 1000.0
                      !frameMs = (tBlitEnd - tFrameStart) * 1000.0

                  notePresent
                    debugSampler
                    uiMs
                    renderMs
                    blitMs
                    frameMs
                    count
                    contentW
                    contentH
                    pw
                    ph
                    newScale
                    curMonScale

                  -- 4. Clear ephemeral input fields
                  writeIORef inpRef (clearEphemeral curInp)

                  -- 5. Drain messages and update model
                  msgs <- drainMessages ctx
                  let typedMsgs = decodeMessages msgs
                      newModel = foldl' (flip updateModel) curModel typedMsgs
                  writeIORef modelRef newModel

                  -- 6. Frame pacing (~120 FPS target, adaptive)
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

