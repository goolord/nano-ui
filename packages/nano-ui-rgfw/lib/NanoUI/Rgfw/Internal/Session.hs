-- | The RGFW window session: options, the runners, and translation of RGFW
-- events into 'NanoUI.Input.Input'.
--
-- RGFW reports no input-method composition: on X11 its input context leaves
-- the text being composed to the input method to draw, as Windows' does, and
-- the text an input method commits arrives as key-char events. So
-- 'NanoUI.Input.inputComposition' stays 'Nothing', and fields take the
-- committed text as typing.
module NanoUI.Rgfw.Internal.Session
  ( RgfwOptions (..)
  , defaultRgfwOptions
  , runRgfwApp
  , runRgfwAppReduce
  , runRgfwAppReduceCustom
  -- * Input translation
  , RgfwEvent
  , decodeRgfwEvents
  , applyRgfwEvent
  -- * Cursors
  , mapRgfwCursor
  ) where

import Control.Concurrent (myThreadId, rtsSupportsBoundThreads, runInBoundThread)
import Control.Exception (bracket)
import Control.Monad (unless, void, when)
import Data.Bits ((.&.), (.|.))
import Data.Char (chr, isDigit, isPrint, toLower)
import Data.IORef (IORef, atomicModifyIORef', atomicWriteIORef, newIORef, readIORef, writeIORef)
import Data.List (find)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Data.Word (Word8, Word32)
import Foreign.Ptr (Ptr)
import GHC.Clock (getMonotonicTime)
import NanoUI
  ( Appearance
  , NanoUI
  , Size (..)
  , Theme (..)
  , V2 (..)
  , WindowMode (..)
  , WindowPosition (..)
  , WindowSettings (..)
  , defaultWindowSettings
  , rgbaBytes
  , rgbaHeight
  , rgbaWidth
  , tomorrowNightMinDarkTheme
  , v2Add
  )
import NanoUI.Backend
  ( Damage (..)
  , Input (..)
  , Key (..)
  , Modifiers (..)
  , WindowHost (..)
  , WindowState (..)
  , answerScreenshots
  , applyKey
  , applyMouseButton
  , applyPointerLeave
  , cursorFallback
  , defaultWindowHost
  , defaultWindowState
  , emptyInput
  , installWindowHost
  , releaseAllKeys
  , keypadKey
  , modifiersFromBits
  , reportWindowState
  , mouseButtonNumber
  , setExplainLayout
  , setWakeLoop
  )
import NanoUI.Internal.Context
  ( Context (..)
  , setHost
  , withClipboard
  )
import NanoUI.Testing
  ( DrawData (..)
  , UiCursorKind (..)
  , collectRasterSpans
  , damageIsEmpty
  , drawCmdCount
  , runEff
  , runFrameReduceEff
  , takeDamage
  , takeDamagePieces
  , uiCursorKind
  )
import NanoUI.Internal.Debug (noteDebugPresent, noteDebugSkip)
import NanoUI.Runner
  ( SessionDriver (..)
  , runSessionLoop
  , shouldRedrawFrame
  )
import NanoUI.Internal.Layout.Arena (arenaCount)
import NanoUI.Rgfw.Internal.Context (applyRgfwTheme, newRgfwContext)
import NanoUI.Rgfw.Internal.Debug
  ( RgfwDebugHost (..)
  , RgfwDebugSampler (..)
  , RgfwFrameStats (..)
  , newRgfwDebugSampler
  )
import NanoUI.Rgfw.Internal.Font.Cozette (getCozetteFont)
import NanoUI.Rgfw.Internal.Gl (freeGlRenderer, newGlRenderer, renderArenaGl, retainedPixels, syncImagesGl)
import qualified RGFW as R

-- | Window and rendering options for the RGFW runners.
data RgfwOptions = RgfwOptions
  { optWindow :: !WindowSettings
  -- ^ The window (default: 'defaultWindowSettings'). Its sizes are in layout
  -- units, converted at the scale the window opens at. RGFW places a window
  -- itself, so 'WindowPositionDefault' centres it. RGFW windows are opaque
  -- and do not fade: 'wsTransparent' and 'wsOpacity' do nothing here.
  , optTheme  :: !Theme
  -- ^ Any core theme; the backend draws it square ('NanoUI.Rgfw.Internal.Context.applyRgfwTheme').
  , optThemeFor :: !(Maybe (Maybe Appearance -> Theme))
  -- ^ The theme for the desktop's light or dark setting, such as
  -- @'NanoUI.lightDark' light dark@, as SDL's @sdlAppThemeFor@ (default:
  -- 'Nothing'). Set, it replaces 'optTheme'. RGFW cannot read the setting,
  -- so it is given 'Nothing', for which 'NanoUI.lightDark' picks the dark
  -- theme.
  , optScale  :: !Float
  -- ^ UI scale. @0@ follows the monitor's scale.
  , optRefreshHz :: !Int
  -- ^ Frame pacing rate while animating. @0@ means 60.
  , optExplainLayout :: !Bool
  -- ^ Start with the layout overlay on, which outlines every layout node. A
  -- view turns it on and off with @explainLayout@.
  }

-- | A 'defaultWindowSettings' window with the dark theme, the monitor's
-- scale, and 60 Hz animation pacing.
defaultRgfwOptions :: RgfwOptions
defaultRgfwOptions =
  RgfwOptions
    { optWindow = defaultWindowSettings
    , optTheme  = tomorrowNightMinDarkTheme
    , optThemeFor = Nothing
    , optScale  = 0.0
    , optRefreshHz = 0
    , optExplainLayout = False
    }

-- | The theme the options ask for: 'optThemeFor' for an appearance RGFW
-- cannot read, else 'optTheme'.
optionsTheme :: RgfwOptions -> Theme
optionsTheme opts = maybe (optTheme opts) ($ Nothing) (optThemeFor opts)

-- | The key an RGFW key code names, given the modifier bits it came with.
mapRgfwKey :: Word32 -> Word8 -> Maybe Key
mapRgfwKey k m
  | Just named <- lookup k namedRgfwKeys = Just named
  | Just c <- lookup k keypadRgfwKeys = keypadKey (m .&. R.rgfw_modNumLock /= 0) c
  | k >= R.rgfw_keyF1 && k <= R.rgfw_keyF24 = Just (KeyF (fromIntegral (k - R.rgfw_keyF1) + 1))
  -- Below 128 the key codes are the ASCII characters the keys type unshifted
  -- in a US layout.
  | k > 32 && k < 127 = Just (KeyChar (toLower (chr (fromIntegral k))))
  | otherwise = Nothing

namedRgfwKeys :: [(Word32, Key)]
namedRgfwKeys =
  [ (R.rgfw_keyEscape, KeyEscape)
  , (R.rgfw_keyReturn, KeyEnter)
  , (R.rgfw_keyPadReturn, KeyEnter)
  , (R.rgfw_keyTab, KeyTab)
  , (R.rgfw_keyBackSpace, KeyBackspace)
  , (R.rgfw_keyDelete, KeyDelete)
  , (R.rgfw_keyLeft, KeyLeft)
  , (R.rgfw_keyRight, KeyRight)
  , (R.rgfw_keyUp, KeyUp)
  , (R.rgfw_keyDown, KeyDown)
  , (R.rgfw_keyHome, KeyHome)
  , (R.rgfw_keyEnd, KeyEnd)
  , (R.rgfw_keyPageUp, KeyPageUp)
  , (R.rgfw_keyPageDown, KeyPageDown)
  , (R.rgfw_keyInsert, KeyInsert)
  , (R.rgfw_keySpace, KeySpace)
  , (R.rgfw_keyPrintScreen, KeyPrintScreen)
  , (R.rgfw_keyPause, KeyPause)
  , (R.rgfw_keyCapsLock, KeyCapsLock)
  , (R.rgfw_keyNumLock, KeyNumLock)
  , (R.rgfw_keyScrollLock, KeyScrollLock)
  , (R.rgfw_keyMenu, KeyMenu)
  , (R.rgfw_keyPadSlash, KeyChar '/')
  , (R.rgfw_keyPadMultiply, KeyChar '*')
  , (R.rgfw_keyPadMinus, KeyChar '-')
  , (R.rgfw_keyPadPlus, KeyChar '+')
  , (R.rgfw_keyPadEqual, KeyChar '=')
  ]

-- | The keypad digits and point, by the character each types ('keypadKey').
keypadRgfwKeys :: [(Word32, Char)]
keypadRgfwKeys =
  zip
    [R.rgfw_keyPad0, R.rgfw_keyPad1, R.rgfw_keyPad2, R.rgfw_keyPad3, R.rgfw_keyPad4, R.rgfw_keyPad5, R.rgfw_keyPad6, R.rgfw_keyPad7, R.rgfw_keyPad8, R.rgfw_keyPad9, R.rgfw_keyPadPeriod]
    "0123456789."

modsFromRgfw :: Word8 -> Modifiers
modsFromRgfw m = modifiersFromBits m R.rgfw_modShift R.rgfw_modControl R.rgfw_modAlt R.rgfw_modSuper

-- | The RGFW standard cursor that shows a cursor kind, or its
-- 'cursorFallback'. 'R.rgfw_mouseArrow' selects the platform's default arrow.
mapRgfwCursor :: UiCursorKind -> Word8
mapRgfwCursor kind = case cursorFallback kind of
  UiCursorPointer    -> R.rgfw_mousePointingHand
  UiCursorText       -> R.rgfw_mouseIbeam
  UiCursorNsResize   -> R.rgfw_mouseResizeNS
  UiCursorEwResize   -> R.rgfw_mouseResizeEW
  UiCursorNwseResize -> R.rgfw_mouseResizeNWSE
  UiCursorNeswResize -> R.rgfw_mouseResizeNESW
  UiCursorNotAllowed -> R.rgfw_mouseNotAllowed
  UiCursorWait       -> R.rgfw_mouseWait
  UiCursorProgress   -> R.rgfw_mouseProgress
  UiCursorCrosshair  -> R.rgfw_mouseCrosshair
  UiCursorMove       -> R.rgfw_mouseResizeAll
  UiCursorNResize    -> R.rgfw_mouseResizeN
  UiCursorNeResize   -> R.rgfw_mouseResizeNE
  UiCursorEResize    -> R.rgfw_mouseResizeE
  UiCursorSeResize   -> R.rgfw_mouseResizeSE
  UiCursorSResize    -> R.rgfw_mouseResizeS
  UiCursorSwResize   -> R.rgfw_mouseResizeSW
  UiCursorWResize    -> R.rgfw_mouseResizeW
  UiCursorNwResize   -> R.rgfw_mouseResizeNW
  _                  -> R.rgfw_mouseArrow

-- | Run a view in an owned RGFW/OpenGL window until quit. Native resources are
-- released on exit. Window creation failure prints a message and returns.
runRgfwApp :: RgfwOptions -> NanoUI () -> IO ()
runRgfwApp opts app = runRgfwAppReduce opts (\() m -> m) () (\_ -> app)

-- | Model-driven runner. Fold emitted messages through the update function
-- in order, ignoring messages of other runtime types.
runRgfwAppReduce ::
  (Typeable msg, Eq model) =>
  RgfwOptions ->
  (msg -> model -> model) ->
  model ->
  (model -> NanoUI ()) ->
  IO ()
runRgfwAppReduce opts =
  runRgfwAppReduceCustom opts (\_ -> (optionsTheme opts, optScale opts))

-- | Reducer runner whose theme and UI scale are derived from the current
-- model. A non-positive scale follows the monitor. OpenGL calls remain on
-- the creating OS thread; native resources are released on exit.
runRgfwAppReduceCustom ::
  (Typeable msg, Eq model) =>
  RgfwOptions ->
  (model -> (Theme, Float)) ->
  (msg -> model -> model) ->
  model ->
  (model -> NanoUI ()) ->
  IO ()
runRgfwAppReduceCustom opts getThemeAndScale updateModel initialModel view = inBoundThread $ do
  -- The window opens hidden, at the scale asked for (the monitor's is known
  -- once it is on one), and is sized, placed and shown once it is.
  let flags =
        R.rgfw_windowHide
          .|. (if wsResizable settings then 0 else R.rgfw_windowNoResize)
          .|. (if wsMode settings == Fullscreen then R.rgfw_windowFullscreen else 0)
      (openW, openH) = pixelSize (resolveScale (snd (getThemeAndScale initialModel)) 1)
  bracket
    (R.createWindowGL (T.unpack (wsTitle settings)) 0 0 openW openH flags 3 2)
    (mapM_ R.closeWindow) $ \mWin -> case mWin of
      Nothing -> putStrLn "Failed to create RGFW window with an OpenGL 3.2 context."
      Just win -> runWindow win
  where
    settings = optWindow opts
    -- The GL context is current only on the OS thread that created it.
    inBoundThread act = if rtsSupportsBoundThreads then runInBoundThread act else act
    -- The model's scale, else the options', else the monitor's.
    resolveScale userScale monScale = fromMaybe 1 (find (> 0) [userScale, optScale opts, monScale])
    -- A window size in native pixels at a scale, and the settings' own.
    pixelsAt scale (Size w h) = (max 1 (round (w * scale)), max 1 (round (h * scale)))
    pixelSize scale = pixelsAt scale (wsSize settings)
    runWindow win = do
      let !refreshHz = if optRefreshHz opts > 0 then optRefreshHz opts else 60
          !refreshSec = 1.0 / fromIntegral refreshHz :: Double
      monScaleInit <- R.windowScale win
      let !initMonScale = if monScaleInit > 0.0 then monScaleInit else 1.0
      monScaleRef <- newIORef initMonScale

      let -- A size in native pixels in layout units at a scale.
          logicalSize (pw, ph) scale = Size (units pw) (units ph)
            where
              units v = fromIntegral (max 1 (round (fromIntegral v / scale) :: Int))
          (initTheme, initScaleChoice) = getThemeAndScale initialModel
          !initScale = resolveScale initScaleChoice initMonScale
          initPhys = pixelSize initScale
      when (initPhys /= pixelSize (resolveScale initScaleChoice 1)) $ uncurry (R.resizeWindow win) initPhys

      modelRef <- newIORef initialModel
      scaleRef <- newIORef initScale
      winSizeRef <- newIORef initPhys
      -- The theme last applied, before squaring: comparing it with the
      -- model's theme skips rebuilding the squared theme on every frame.
      themeRef <- newIORef initTheme
      cursorRef <- newIORef UiCursorDefault
      -- Physical size and scale of the last presented frame. The retained
      -- framebuffer holds that frame, so a frame at another size or scale
      -- has nothing to keep and paints in full.
      presentedRef <- newIORef (0, 0, 0)

      ctx0 <- newRgfwContext initTheme
      let ctx = withClipboard ctx0 R.readClipboardText R.writeClipboardText
      debugSampler <- newRgfwDebugSampler
      setHost ctx (RgfwDebugHost debugSampler)
      setExplainLayout ctx (optExplainLayout opts)
      -- Views give sizes in layout units, at the scale the window is at
      -- when they ask. An axis of zero is no limit.
      let limit set s = do
            scale <- readIORef scaleRef
            let Size w h = fromMaybe (Size 0 0) s
                axis v = if v <= 0 then 0 else round (v * scale)
            set win (axis w) (axis h)
          place = case wsPosition settings of
            WindowPositionDefault -> WindowPositionCentered
            p -> p
      installWindowHost ctx settings {wsPosition = place} $
        defaultWindowHost
          { hostSetTitle = R.setWindowName win
          , hostSetIcon = \p -> void (R.setWindowIcon win (rgbaWidth p) (rgbaHeight p) (rgbaBytes p))
          , hostSetMinSize = limit R.setWindowMinSize
          , hostSetMaxSize = limit R.setWindowMaxSize
            -- RGFW windows do not fade: 'NanoUI.setWindowOpacityUi' does
            -- nothing here.
          , hostSetMode = \case
              Windowed -> R.setWindowFullscreen win False >> R.showWindow win
              Fullscreen -> R.showWindow win >> R.setWindowFullscreen win True
              Hidden -> R.hideWindow win
          , hostMove = R.moveWindow win
          , hostCenter = R.centerWindow win
          , hostResize = \s -> readIORef scaleRef >>= \scale -> uncurry (R.resizeWindow win) (pixelsAt scale s)
          , hostMinimize = R.minimizeWindow win
          , hostMaximize = R.maximizeWindow win
          , hostRestore = R.restoreWindow win
          }
      reportWindowState ctx =<< rgfwWindowState win initScale
      unless (wsMode settings == Hidden) (R.showWindow win)
      -- Another thread (a background job) wakes the loop: it ends the event
      -- wait, and the pass it ends draws a frame. Until that frame starts,
      -- more wakes stop no more waits. The loop's own thread wakes it on
      -- every 'markDirty', which it sees for itself.
      loopThread <- myThreadId
      wakeRef <- newIORef False
      setWakeLoop ctx $ do
        me <- myThreadId
        unless (me == loopThread) $ do
          pending <- atomicModifyIORef' wakeRef (True,)
          unless pending R.stopWaitForEvent
      let font = getCozetteFont
          initInp = emptyInput {inputWindowSize = logicalSize initPhys initScale}
          -- Set the pointer shape only when the wanted kind changes, and
          -- hide the pointer for 'UiCursorHidden' until another is wanted.
          syncCursor c inp = do
            want <- uiCursorKind c inp
            cur <- readIORef cursorRef
            when (want /= cur) $ do
              writeIORef cursorRef want
              let hidden = want == UiCursorHidden
                  icon = mapRgfwCursor want
              when (hidden /= (cur == UiCursorHidden)) $ R.showMouse win (not hidden)
              unless hidden . void $
                if icon == R.rgfw_mouseArrow
                  then R.setMouseDefault win
                  else R.setMouseStandard win icon

      bracket newGlRenderer freeGlRenderer $ \renderer -> R.withEventBuffer $ \evPtr -> do
        let !animateTimeout = max 1 (floor (refreshSec * 1000) - 2) :: Int

        -- A frame whose damage is empty would swap in the picture already on
        -- screen, so it skips the render and the swap: an animation scrolled
        -- out of view costs its UI pass and nothing on the GPU. The opening
        -- frame and an animation's settle frame are forced. Other frames
        -- paint only their damage over the retained last frame.
        let drawOne force c curInp = do
              tUiStart <- getMonotonicTime
              curScale <- readIORef scaleRef
              (pw, ph) <- readIORef winSizeRef
              presented <- readIORef presentedRef
              let !paintFull = force || presented /= (pw, ph, curScale) || inputWindowRedraw curInp
              writeIORef (ctxPaintFull c) paintFull
              curModel <- readIORef modelRef
              let (frameTheme, _) = getThemeAndScale curModel
              appliedTheme <- readIORef themeRef
              when (frameTheme /= appliedTheme) $ do
                writeIORef themeRef frameTheme
                applyRgfwTheme c frameTheme
              (_, newModel, _, drawData, dirtyAfterUi) <-
                runFrameReduceEff runEff updateModel c curInp curModel view
              writeIORef modelRef newModel
              tUiEnd <- getMonotonicTime
              damage <- takeDamage c
              if damageIsEmpty damage && not paintFull
                then do
                  noteDebugSkip (rdsSampler debugSampler)
                  -- The retained frame, on screen, is this one.
                  answerScreenshots c (retainedPixels renderer pw ph)
                  pure dirtyAfterUi
                else do
                  tRenderStart <- getMonotonicTime
                  curMonScale <- readIORef monScaleRef
                  (baseSpans, overlaySpans) <- collectRasterSpans c curInp
                  pieces <- if paintFull then pure [] else takeDamagePieces c
                  syncImagesGl renderer c
                  renderArenaGl renderer font curScale pw ph (themeWindow frameTheme)
                      (if paintFull then DamageFull else damage) pieces drawData baseSpans overlaySpans
                  writeIORef presentedRef $! (pw, ph, curScale)
                  tSwapStart <- getMonotonicTime
                  R.swapBuffersGL win
                  tSwapEnd <- getMonotonicTime
                  answerScreenshots c (retainedPixels renderer pw ph)
                  nodes <- arenaCount (ctxNodeArena c)
                  let ms a b = (b - a) * 1000
                  noteDebugPresent (rdsSampler debugSampler) (ms tUiStart tUiEnd) (ms tRenderStart tSwapStart)
                    (ms tSwapStart tSwapEnd) (ms tUiStart tSwapEnd)
                    (drawVertexCount drawData) (drawIndexCount drawData) (drawCmdCount drawData)
                  writeIORef (rdsFrame debugSampler)
                    RgfwFrameStats
                      { fsNodes = nodes
                      , fsPhysW = pw
                      , fsPhysH = ph
                      , fsScale = curScale
                      , fsMonScale = curMonScale
                      }
                  -- A reduced message that switched themes changed the model, so
                  -- the core marks the frame dirty and the next one applies it.
                  pure dirtyAfterUi

        let drv =
              SessionDriver
                { sdPollEvents    = pollRgfwEvents win evPtr scaleRef monScaleRef winSizeRef
                , sdWaitEvents    = \t -> do
                    woke <- readIORef wakeRef
                    unless woke (R.waitForEvent t)
                    pollRgfwEvents win evPtr scaleRef monScaleRef winSizeRef
                , sdApplyEvent    = applyRgfwEvent
                , sdIsButtonEdge  = \case RgfwEvButton {} -> True; _ -> False
                , sdIsSessionQuit = \case RgfwEvClose -> True; _ -> False
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
                    reportWindowState c =<< rgfwWindowState win newScale
                    pure (c, inp {inputWindowSize = logicalSize (pw, ph) newScale})
                , sdDebug         = rdsSampler debugSampler
                , sdContinuous    = False
                  -- Presents are unthrottled (swap interval 0, no vsync), so a
                  -- live in-view animation is paced at the refresh period
                  -- instead of spinning.
                , sdPacingMs      = animateTimeout
                , sdPresentPaces  = pure False
                , sdShouldDraw    = \c prevInp inpSynced wasAnim refreshDue -> do
                    woke <- readIORef wakeRef
                    shouldRedrawFrame c prevInp inpSynced wasAnim False (refreshDue || woke)
                , sdDraw          = \c curInp forceFull -> do
                    -- The frame answers every wake before its view runs.
                    atomicWriteIORef wakeRef False
                    drawOne forceFull c curInp
                , sdOnCursor      = syncCursor
                , sdShouldQuit    = \_ -> False
                , sdAlignSec      = refreshSec
                }
        -- Present the opening frame before entering the loop so the window has
        -- content immediately; once idle the loop blocks with no redraws. The
        -- context stays as that frame left it: if it asked for another by
        -- marking the context dirty, the loop draws it at once instead of
        -- blocking until some input happens along.
        _ <- drawOne True ctx initInp
        runSessionLoop drv ctx initInp

-- | The window's state for views, at a scale: what RGFW keeps of its
-- position, focus and mode from its events, which costs no round trip to
-- the desktop.
rgfwWindowState :: R.Window -> Float -> IO WindowState
rgfwWindowState win scale = do
  (x, y) <- R.windowPosition win
  flags <- R.windowFlags win
  focused <- R.windowFocused win
  let has bit = flags .&. bit /= 0
  pure
    defaultWindowState
      { winScale = scale
      , winPosition = Just (x, y)
      , winFocused = focused
      , winMaximized = has R.rgfw_windowMaximize
      , winMinimized = has R.rgfw_windowMinimize
      , winFullscreen = has R.rgfw_windowFullscreen
      }

-- | An RGFW event translated for the input fold.
data RgfwEvent
  = RgfwEvClose
  | RgfwEvResize -- ^ window size or monitor scale changed (read back at sync)
  | RgfwEvMotion !Float !Float
  | RgfwEvButton !Word8 !Bool
  | RgfwEvLeave -- ^ the pointer left the window
  | RgfwEvScroll !Float !Float
  | RgfwEvChar !Char -- ^ typed character
  | RgfwEvKey !Word32 !Word8 !Bool -- ^ key and modifiers, down (an auto-repeat too) or up
  | RgfwEvFocusLost -- ^ the window lost the keyboard

-- | Drain the RGFW queue, recording size and scale changes for the next sync.
-- A key that types a character is reported by what it types in the current
-- layout ('layoutKey').
pollRgfwEvents :: R.Window -> Ptr R.RGFW_event -> IORef Float -> IORef Float -> IORef (Int, Int) -> IO [RgfwEvent]
pollRgfwEvents win evPtr scaleRef monScaleRef winSizeRef = do
  raw <- drain []
  scale <- readIORef scaleRef
  pure (decodeRgfwEvents scale raw)
  where
    drain acc = do
      ev <- R.pollEvent win evPtr
      case ev of
        R.EventNone -> pure (reverse acc)
        R.EventWindowResize nw nh -> do
          writeIORef winSizeRef (nw, nh)
          drain (ev : acc)
        R.EventScaleUpdate sx _ -> do
          writeIORef monScaleRef (if sx > 0 then sx else 1)
          drain (ev : acc)
        R.EventKeyPress k m -> relayout R.EventKeyPress k m
        R.EventKeyRepeat k m -> relayout R.EventKeyRepeat k m
        R.EventKeyRelease k m -> relayout R.EventKeyRelease k m
        _ -> drain (ev : acc)
      where
        relayout event k m = layoutKey k >>= \k' -> drain (event k' m : acc)
    -- RGFW names a physical key by what it types in a US layout. A letter or
    -- punctuation key takes what it types in the current one when that is
    -- ASCII; the digit row keeps its digits, as some layouts type symbols
    -- there unshifted.
    layoutKey k
      | k > 32 && k < 127 && not (isDigit (chr (fromIntegral k))) =
          maybe k (\mapped -> if mapped > 32 && mapped < 127 then mapped else k) <$> R.physicalToMappedKey k
      | otherwise = pure k

-- | Translate a batch of raw events in queue order. Pointer positions are
-- divided by the logical scale. A character event types its character
-- unless it is a control code: Ctrl+letter comes as one on some platforms,
-- and is a key chord, which the key event reports.
decodeRgfwEvents :: Float -> [R.Event] -> [RgfwEvent]
decodeRgfwEvents scale = mapMaybe $ \case
  R.EventWindowClose -> Just RgfwEvClose
  R.EventWindowResize _ _ -> Just RgfwEvResize
  R.EventScaleUpdate _ _ -> Just RgfwEvResize
  R.EventMouseMotion x y -> Just (RgfwEvMotion (fromIntegral x / scale) (fromIntegral y / scale))
  R.EventMouseButton btn down -> Just (RgfwEvButton btn down)
  R.EventMouseScroll dx dy -> Just (RgfwEvScroll dx dy)
  R.EventOther t
    | t == R.rgfw_mouseLeave -> Just RgfwEvLeave
    | t == R.rgfw_windowFocusOut -> Just RgfwEvFocusLost
  R.EventKeyPress k m -> Just (RgfwEvKey k m True)
  R.EventKeyRepeat k m -> Just (RgfwEvKey k m True)
  R.EventKeyRelease k m -> Just (RgfwEvKey k m False)
  R.EventKeyChar ch | isPrint ch -> Just (RgfwEvChar ch)
  _ -> Nothing

-- | Accumulate a decoded event into frame input. The caller handles close
-- and resize events separately; motion coordinates are already scaled by decoding.
applyRgfwEvent :: Input -> RgfwEvent -> Input
applyRgfwEvent inp ev = case ev of
  RgfwEvClose -> inp
  RgfwEvResize -> inp
  RgfwEvMotion x y -> inp {inputMousePos = V2 x y}
  -- RGFW numbers the buttons from 0 in the order 'mouseButtonNumber' counts
  -- them from 1: left, middle, right, then its misc buttons, the first two
  -- the back and forward side buttons.
  RgfwEvButton btn down -> applyMouseButton (mouseButtonNumber (fromIntegral btn + 1)) down inp
  RgfwEvLeave -> applyPointerLeave inp
  RgfwEvScroll dx dy -> inp {inputScroll = v2Add (inputScroll inp) (V2 dx dy)}
  RgfwEvChar c -> inp {inputChars = T.snoc (inputChars inp) c}
  -- 'applyKey' tells an auto-repeat by the key being held already.
  RgfwEvKey k m down ->
    (maybe inp (\key -> applyKey key down inp) (mapRgfwKey k m)) {inputModifiers = modsFromRgfw m}
  RgfwEvFocusLost -> releaseAllKeys inp
