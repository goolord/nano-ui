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
import Control.Monad (when)
import Data.Bits ((.&.))
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
import Data.Word (Word32)
import Effectful (runEff)
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
  , WidgetStore (..)
  , clearPopupConfigs
  , decodeMessages
  , drainMessages
  , getStore
  , intKey
  , lookupPopupConfig
  , seedFloatingPanel
  , setPrevRect
  , setStore
  , withFontMetrics
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
  )
import NanoUI.Id (initialIdContext)
import NanoUI.Layout.Arena
  ( NodeType (..)
  , arenaCount
  , getClipRect
  , getNodeType
  , getParent
  , getRect
  , getWidgetId
  , isContainerNode
  , resetNodeArena
  , setClipRect
  , setRect
  )
import NanoUI.Monad (runUi)
import NanoUI.Rgfw.Font.Cozette (cozetteMetrics, getCozetteFont)
import NanoUI.Rgfw.Layout (getContentHeight, solveSinglePassLayoutWith)
import NanoUI.Rgfw.Render (renderArena)
import NanoUI.Rgfw.Surface
  ( RgfwSurface (..)
  , clearScreen
  , drawRectOutline
  , fillRect
  , freeRgfwSurface
  , newRgfwSurface
  , packColor
  , resizeRgfwSurface
  , upscaleSurface
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
  , optScale  :: !Int
  }

defaultRgfwOptions :: RgfwOptions
defaultRgfwOptions =
  RgfwOptions
    { optTitle  = "nano-ui (RGFW Single-Pass)"
    , optWidth  = 1680
    , optHeight = 1040
    , optTheme  = defaultDarkTheme
    , optCenter = True
    , optScale  = 1
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
  runRgfwSessionReduceCustom opts (\_ -> (optTheme opts, max 1 (optScale opts)))

runRgfwSessionReduceCustom ::
  (Typeable msg, Eq model) =>
  RgfwOptions ->
  (model -> (RgfwTheme, Int)) ->
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
      let (_, initScaleRaw) = getThemeAndScale initialModel
          initScale = max 1 initScaleRaw
      let !initPhysW = optWidth opts
          !initPhysH = optHeight opts
          !initLogW = max 1 (initPhysW `div` initScale)
          !initLogH = max 1 (initPhysH `div` initScale)

      physSurf0 <- newRgfwSurface win initPhysW initPhysH
      physSurfRef <- newIORef physSurf0

      logSurf0 <- newRgfwSurface win initLogW initLogH
      logSurfRef <- newIORef logSurf0

      runningRef <- newIORef True
      modelRef <- newIORef initialModel
      scaleRef <- newIORef initScale
      winSizeRef <- newIORef (initPhysW, initPhysH)
      scrollYRef <- newIORef (0.0 :: Float)
      maxScrollYRef <- newIORef (0.0 :: Float)

      ctx0 <- newPixelContext
      let ctx = withFontMetrics ctx0 cozetteMetrics
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
                        R.EventMouseMotion mx my -> do
                          let !lmx = mx `div` curScale
                              !lmy = my `div` curScale
                          modifyIORef' inpRef $ \inp ->
                            inp {inputMousePos = V2 (fromIntegral lmx) (fromIntegral lmy)}
                          pollEvents
                        R.EventMouseButton btn pressed -> do
                          let isDown = pressed
                          if btn == R.rgfw_mouseLeft
                            then modifyIORef' inpRef $ \inp ->
                              let wasDown = inputMouseDown inp
                                  isPress = isDown && not wasDown
                                  isRelease = not isDown && wasDown
                               in inp
                                    { inputMouseDown = isDown
                                    , inputMousePressed = isPress
                                    , inputMouseReleased = isRelease
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
                        R.EventMouseScroll _ dy -> do
                          curScroll <- readIORef scrollYRef
                          maxScroll <- readIORef maxScrollYRef
                          let !newScroll = max 0 (min maxScroll (curScroll - dy * 24))
                          writeIORef scrollYRef newScroll
                          modifyIORef' inpRef $ \inp ->
                            inp {inputScroll = V2 0 dy}
                          pollEvents
                        R.EventKeyChar ch -> do
                          modifyIORef' inpRef $ \inp ->
                            inp {inputChars = T.snoc (inputChars inp) ch}
                          pollEvents
                        R.EventKeyPress k m -> do
                          let shift = (m .&. 16) /= 0
                              ctrl  = (m .&. 4) /= 0
                              alt   = (m .&. 8) /= 0
                              mods  = Modifiers shift ctrl alt
                          when (k == 170) $ do
                            -- PageUp
                            (_, ph) <- readIORef winSizeRef
                            let !lh = ph `div` curScale
                            curScroll <- readIORef scrollYRef
                            writeIORef scrollYRef (max 0 (curScroll - fromIntegral lh * 0.7))
                          when (k == 171) $ do
                            -- PageDown
                            (_, ph) <- readIORef winSizeRef
                            let !lh = ph `div` curScale
                            curScroll <- readIORef scrollYRef
                            maxScroll <- readIORef maxScrollYRef
                            writeIORef scrollYRef (min maxScroll (curScroll + fromIntegral lh * 0.7))
                          modifyIORef' inpRef $ \inp ->
                            let curKeys = inputKeys inp
                                newKeys = case mapRgfwKey k of
                                  Just mk -> V.snoc curKeys mk
                                  Nothing -> curKeys
                             in inp {inputKeys = newKeys, inputModifiers = mods}
                          pollEvents
                        _ ->
                          pollEvents
                pollEvents

                -- 2. Execute UI Frame
                stillRunning <- readIORef runningRef
                when stillRunning $ do
                  curModel <- readIORef modelRef
                  let (curTheme, newScaleRaw) = getThemeAndScale curModel
                      !newScale = max 1 newScaleRaw

                  -- Check for resize or DPI scale change
                  (pw, ph) <- readIORef winSizeRef
                  writeIORef scaleRef newScale
                  let !lw = max 1 (pw `div` newScale)
                      !lh = max 1 (ph `div` newScale)

                  physSurf <- readIORef physSurfRef
                  physSurf' <- resizeRgfwSurface win physSurf pw ph
                  writeIORef physSurfRef physSurf'

                  logSurf <- readIORef logSurfRef
                  logSurf' <- resizeRgfwSurface win logSurf lw lh
                  writeIORef logSurfRef logSurf'

                  modifyIORef' inpRef $ \inp ->
                    inp {inputWindowSize = Size (fromIntegral lw) (fromIntegral lh)}

                  curInp <- readIORef inpRef

                  -- Handle mouse press and interaction transitions
                  prevHot <- readIORef hotRef
                  when (inputMousePressed curInp) $ do
                    when (prevHot /= WidgetId 0) $ do
                      writeIORef activeRef prevHot
                      writeIORef (ctxActiveId ctx) prevHot
                      writeIORef focusRef prevHot
                      writeIORef (ctxFocusId ctx) prevHot

                      -- Update caret cursor position on click
                      let naPrev = ctxNodeArena ctx
                      prevCount <- arenaCount naPrev
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
                          let mouse = inputMousePos curInp
                              mx = v2X mouse
                              my = v2Y mouse
                              key = intKey prevHot
                          store <- getStore ctx
                          case nt of
                            NodeTextInput -> do
                              let txt = IM.findWithDefault "" key (storeText store)
                                  relX = mx - (rx + 6)
                                  !charIdx = max 0 (min (T.length txt) (round (max 0 relX / 6.0))) :: Int
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
                              setStore ctx $ store
                                { storeInt =
                                    IM.insert (slotKey slotTextAreaRow key) clampedRow $
                                      IM.insert (slotKey slotTextAreaCol key) clickCol $
                                        IM.insert (slotKey slotTextAreaPrefCol key) clickCol $
                                          IM.insert (slotKey slotTextAreaAnchorRow key) clampedRow $
                                            IM.insert (slotKey slotTextAreaAnchorCol key) clickCol (storeInt store)
                                }
                            _ -> pure ()
                        Nothing -> pure ()

                    -- Scrollbar track click
                    maxScroll <- readIORef maxScrollYRef
                    when (maxScroll > 0) $ do
                      let mouse = inputMousePos curInp
                      when (v2X mouse >= fromIntegral (lw - 12)) $ do
                        let !newScroll = max 0 (min maxScroll ((v2Y mouse / fromIntegral lh) * maxScroll))
                        writeIORef scrollYRef newScroll

                  -- Mouse Drag: Scrollbar drag or text selection
                  when (inputMouseDown curInp && not (inputMousePressed curInp)) $ do
                    let mouse = inputMousePos curInp
                        mx = v2X mouse
                        my = v2Y mouse
                    maxScroll <- readIORef maxScrollYRef
                    if maxScroll > 0 && mx >= fromIntegral (lw - 12)
                      then do
                        let !newScroll = max 0 (min maxScroll ((my / fromIntegral lh) * maxScroll))
                        writeIORef scrollYRef newScroll
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
                                  setStore ctx $ store
                                    { storeInt =
                                        IM.insert (slotKey slotTextAreaRow key) clampedRow $
                                          IM.insert (slotKey slotTextAreaCol key) dragCol $
                                            IM.insert (slotKey slotTextAreaPrefCol key) dragCol (storeInt store)
                                    }
                                _ -> pure ()
                            Nothing -> pure ()

                  when (inputMouseReleased curInp) $ do
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
                  runEff (runUi ctx curInp (view curModel))

                  -- Clear one-shot click
                  writeIORef (ctxClickedId ctx) (WidgetId 0)

                  -- Run single-pass layout with popup positioning
                  let na = ctxNodeArena ctx
                  solveSinglePassLayoutWith na (fromIntegral lw) (fromIntegral lh) (lookupPopupConfig ctx)
                  contentH <- getContentHeight na
                  let !maxScroll = max 0 (contentH - fromIntegral lh + 16)
                  writeIORef maxScrollYRef maxScroll
                  modifyIORef' scrollYRef (\s -> max 0 (min maxScroll s))
                  curScroll <- readIORef scrollYRef

                  count <- arenaCount na

                  let isFloating !idx = do
                        nt <- getNodeType na idx
                        if nt == NodePopup
                          then pure True
                          else do
                            p <- getParent na idx
                            if p < 0 then pure False else isFloating p

                  -- Apply scroll offset to non-floating nodes if content is scrolled
                  when (curScroll > 0) $ do
                    let offsetNodes !i
                          | i >= count = pure ()
                          | otherwise = do
                              p <- getParent na i
                              floating <- isFloating i
                              when (p >= 0 && not floating) $ do
                                (rx, ry, rw, rh) <- getRect na i
                                setRect na i rx (ry - curScroll) rw rh
                                mClip <- getClipRect na i
                                case mClip of
                                  Just (Rect cx cy cw ch) ->
                                    setClipRect na i (Rect cx (cy - curScroll) cw ch)
                                  Nothing -> pure ()
                              offsetNodes (i + 1)
                    offsetNodes 0

                  -- Hit testing & hover/active/focus resolution (popups prioritized)
                  let findHitInPopups !i
                        | i < 0 = pure (WidgetId 0)
                        | otherwise = do
                            floating <- isFloating i
                            if floating
                              then do
                                nt <- getNodeType na i
                                wid <- getWidgetId na i
                                (rx, ry, rw, rh) <- getRect na i
                                let isInteractive =
                                      wid /= WidgetId 0
                                        && not (isContainerNode nt)
                                        && rectContains (Rect rx ry rw rh) (inputMousePos curInp)
                                if isInteractive then pure wid else findHitInPopups (i - 1)
                              else findHitInPopups (i - 1)

                      findHit !i
                        | i < 0 = pure (WidgetId 0)
                        | otherwise = do
                            nt <- getNodeType na i
                            wid <- getWidgetId na i
                            (rx, ry, rw, rh) <- getRect na i
                            let isInteractive =
                                  wid /= WidgetId 0
                                    && not (isContainerNode nt)
                                    && rectContains (Rect rx ry rw rh) (inputMousePos curInp)
                            if isInteractive
                              then pure wid
                              else findHit (i - 1)

                  hitPopup <- findHitInPopups (count - 1)
                  newHot <- if hitPopup /= WidgetId 0 then pure hitPopup else findHit (count - 1)
                  writeIORef hotRef newHot
                  writeIORef (ctxHotId ctx) newHot

                  curActive <- readIORef activeRef
                  curFocus <- readIORef focusRef

                  -- Update prevRects for next frame's widget interaction query
                  let updatePrev !i
                        | i >= count = pure ()
                        | otherwise = do
                            wid <- getWidgetId na i
                            when (wid /= WidgetId 0) $ do
                              (x, y, w, h) <- getRect na i
                              setPrevRect ctx wid (Rect x y w h)
                              nt <- getNodeType na i
                              when (nt == NodePopup) $
                                seedFloatingPanel ctx wid (Rect x y w h)
                            updatePrev (i + 1)
                  updatePrev 0

                  -- 3. Render directly to software surface
                  let targetSurf = if newScale <= 1 then physSurf' else logSurf'
                  clearScreen targetSurf (packColor (thBackground curTheme))
                  renderArena targetSurf font curTheme ctx na newHot curActive curFocus

                  -- Draw retro scrollbar if window content exceeds viewport
                  when (maxScroll > 0) $ do
                    let !sbW = 8
                        !sbX = lw - sbW
                        !sbH = lh
                        !thumbH = max 24 (min (lh - 4) (round (fromIntegral lh * (fromIntegral lh / max 1 contentH))))
                        !thumbY = round ((curScroll / maxScroll) * fromIntegral (lh - thumbH))
                    fillRect targetSurf sbX 0 sbW sbH (packColor (thScrollTrack curTheme))
                    drawRectOutline targetSurf sbX 0 sbW sbH (packColor (thBorder curTheme))
                    fillRect targetSurf (sbX + 1) (thumbY + 1) (sbW - 2) (thumbH - 2) (packColor (thThumb curTheme))
                    drawRectOutline targetSurf (sbX + 1) (thumbY + 1) (sbW - 2) (thumbH - 2) (packColor (thBorder curTheme))

                  if newScale <= 1
                    then R.blitSurface win (sRgfwSurface physSurf')
                    else do
                      upscaleSurface logSurf' physSurf' newScale
                      R.blitSurface win (sRgfwSurface physSurf')

                  -- 4. Clear ephemeral input fields
                  writeIORef inpRef (clearEphemeral curInp)

                  -- 5. Drain messages and update model
                  msgs <- drainMessages ctx
                  let typedMsgs = decodeMessages msgs
                      newModel = foldl' (flip updateModel) curModel typedMsgs
                  writeIORef modelRef newModel

                  -- 6. Frame pacing (~120 FPS max)
                  threadDelay 8000

                loop
        let cleanup = do
              finalPhysSurf <- readIORef physSurfRef
              freeRgfwSurface finalPhysSurf
              finalLogSurf <- readIORef logSurfRef
              freeRgfwSurface finalLogSurf
              R.closeWindow win
        loop `finally` cleanup

