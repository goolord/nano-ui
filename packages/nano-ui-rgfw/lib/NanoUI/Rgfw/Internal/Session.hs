-- | The RGFW window session: options, the runners, and translation of RGFW
-- events into 'NanoUI.Input.Input'.
--
-- RGFW reports no IME composition. On X11, as on Windows, its input context
-- leaves drawing the composition to the input method, and committed text
-- arrives as key-char events. So 'NanoUI.Input.inputComposition' stays
-- 'Nothing', and fields receive committed text as typing.
--
-- Files dropped on the window are not reported: 'NanoUI.Input.inputDrops'
-- stays empty, so drop zones see no drops.
module NanoUI.Rgfw.Internal.Session
  ( RgfwOptions (..)
  , defaultRgfwOptions
  , runRgfwApp
  , runRgfwAppWith
  , runRgfwAppReduce
  , runRgfwAppReduceCustom
  , runRgfwAppReduceCustomWith
  -- * Input translation
  , RgfwEvent
  , decodeRgfwEvents
  , applyRgfwEvent
  -- * Cursors
  , mapRgfwCursor
  ) where

import Control.Concurrent (rtsSupportsBoundThreads, runInBoundThread)
import Control.Exception (bracket, finally, onException)
import Control.Monad (forM_, unless, void, when)
import Data.Bits (complement, (.&.), (.|.))
import Data.Char (chr, isControl, isDigit, toLower)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (find)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as T
import Data.Word (Word8, Word32)
import Foreign.Ptr (Ptr, nullPtr)
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
  , WindowCapabilities (..)
  , WindowHost (..)
  , WindowState (..)
  , allWindowCapabilities
  , answerScreenshots
  , cancelTasks
  , closeWindowHost
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
  , sizeLimitAt
  , syncCursorKind
  , withBackendSession
  )
import NanoUI.Internal.Context
  ( Context (..)
  , setBackendSession
  , withClipboard
  )
import NanoUI.Testing
  ( DrawData (..)
  , UiCursorKind (..)
  , runFrame
  , collectRasterSpans
  , damageIsEmpty
  , drawCmdCount
  , takeDamage
  , takeDamagePieces
  , uiCursorKind
  )
import NanoUI.Internal.Debug (noteDebugPresent, noteDebugSkip)
import NanoUI.Emit (NanoUIE, newReducer, reduceFrame, reducerModel)
import NanoUI.Runner
  ( SessionDriver (..)
  , runSessionLoop
  , shouldRedrawFrame
  )
import NanoUI.Runner.Windows (WindowOps (..), afterMainPass, closeWindows, loopWindows, newWindows, openWindows)
import NanoUI.Internal.Layout.Arena (arenaCount)
import NanoUI.Rgfw.Internal.Context (applyRgfwTheme, newRgfwContext)
import NanoUI.Rgfw.Internal.Debug
  ( RgfwDebugSampler (..)
  , RgfwFrameStats (..)
  , newRgfwDebugSampler
  , rgfwSession
  )
import NanoUI.Rgfw.Internal.Font.Cozette (getCozetteFont)
import NanoUI.Rgfw.Internal.Gl (GlRenderer, freeGlRenderer, newGlRenderer, renderArenaGl, retainedPixels, syncImagesGl)
import qualified RGFW as R

-- | Window and rendering options for the RGFW runners.
data RgfwOptions = RgfwOptions
  { optWindow :: !WindowSettings
  -- ^ The window (default: 'defaultWindowSettings'). Sizes are in layout
  -- units, converted at the opening scale. RGFW places windows itself, so
  -- 'WindowPositionDefault' centres the window. 'wsTransparent' and
  -- 'wsOpacity' do nothing: RGFW windows are always opaque.
  , optTheme  :: !Theme
  -- ^ Any core theme; the backend draws it square ('NanoUI.Rgfw.Internal.Context.applyRgfwTheme').
  -- The @Custom@ runners take the theme from the model instead.
  , optThemeFor :: !(Maybe (Maybe Appearance -> Theme))
  -- ^ Pick the theme from the desktop's light or dark setting, such as
  -- @'NanoUI.lightDark' light dark@, like SDL's @sdlAppThemeFor@ (default:
  -- 'Nothing'). When set it replaces 'optTheme'. RGFW cannot read the
  -- setting, so the function gets 'Nothing' and 'NanoUI.lightDark' picks
  -- dark. The @Custom@ runners take the theme from the model instead.
  , optScale  :: !Float
  -- ^ UI scale. @0@ follows the monitor's scale.
  , optRefreshHz :: !Int
  -- ^ Frame pacing rate while animating. @0@ means 60.
  , optExplainLayout :: !Bool
  -- ^ Start with the layout overlay (an outline of every layout node) on.
  -- Views toggle it with @explainLayout@.
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

-- | 'optThemeFor' applied to 'Nothing', since RGFW cannot read the
-- appearance; else 'optTheme'.
optionsTheme :: RgfwOptions -> Theme
optionsTheme opts = maybe (optTheme opts) ($ Nothing) (optThemeFor opts)

-- | The key for an RGFW key code and its modifier bits.
mapRgfwKey :: Word32 -> Word8 -> Maybe Key
mapRgfwKey k m
  | Just named <- lookup k namedRgfwKeys = Just named
  | Just c <- lookup k keypadRgfwKeys = keypadKey (m .&. R.rgfw_modNumLock /= 0) c
  | k >= R.rgfw_keyF1 && k <= R.rgfw_keyF24 = Just (KeyF (fromIntegral (k - R.rgfw_keyF1) + 1))
  -- Codes below 128 are the ASCII characters the keys type unshifted on a US
  -- layout.
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

-- | Keypad digit and point codes, by the character each types ('keypadKey').
keypadRgfwKeys :: [(Word32, Char)]
keypadRgfwKeys =
  zip
    [R.rgfw_keyPad0, R.rgfw_keyPad1, R.rgfw_keyPad2, R.rgfw_keyPad3, R.rgfw_keyPad4, R.rgfw_keyPad5, R.rgfw_keyPad6, R.rgfw_keyPad7, R.rgfw_keyPad8, R.rgfw_keyPad9, R.rgfw_keyPadPeriod]
    "0123456789."

modsFromRgfw :: Word8 -> Modifiers
modsFromRgfw m = modifiersFromBits m R.rgfw_modShift R.rgfw_modControl R.rgfw_modAlt R.rgfw_modSuper

-- | The modifier bits of a key event, counting the key itself. On X11 and
-- Windows RGFW reads the bits before it records the key, so a modifier key's
-- press lacks its own bit and its release still has it. On macOS the bits
-- are already current.
ownModifier :: Word32 -> Bool -> Word8 -> Word8
ownModifier k down m = case lookup k modifierRgfwKeys of
  Just (bit, _) -> if down then m .|. bit else m .&. complement bit
  Nothing -> m

-- | Modifier keys, with their bit and the key on the other side.
modifierRgfwKeys :: [(Word32, (Word8, Word32))]
modifierRgfwKeys =
  [ (R.rgfw_keyShiftL, (R.rgfw_modShift, R.rgfw_keyShiftR))
  , (R.rgfw_keyShiftR, (R.rgfw_modShift, R.rgfw_keyShiftL))
  , (R.rgfw_keyControlL, (R.rgfw_modControl, R.rgfw_keyControlR))
  , (R.rgfw_keyControlR, (R.rgfw_modControl, R.rgfw_keyControlL))
  , (R.rgfw_keyAltL, (R.rgfw_modAlt, R.rgfw_keyAltR))
  , (R.rgfw_keyAltR, (R.rgfw_modAlt, R.rgfw_keyAltL))
  , (R.rgfw_keySuperL, (R.rgfw_modSuper, R.rgfw_keySuperR))
  , (R.rgfw_keySuperR, (R.rgfw_modSuper, R.rgfw_keySuperL))
  ]

-- | The RGFW standard cursor for a cursor kind, after 'cursorFallback'.
-- 'R.rgfw_mouseArrow' stands for the platform's default arrow.
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
-- released on exit. Throws an 'IOError' when the window or its OpenGL 3.2
-- context cannot be created.
runRgfwApp :: RgfwOptions -> NanoUI () -> IO ()
runRgfwApp opts app = runRgfwAppReduce opts (\() m -> m) () (\_ -> app)

-- | 'runRgfwApp' with a setup action run once the window and its OpenGL
-- context exist, before the first frame, that returns the view.
runRgfwAppWith :: RgfwOptions -> IO (NanoUI ()) -> IO ()
runRgfwAppWith opts setup =
  runRgfwAppReduceCustomWith opts (\_ -> (optionsTheme opts, optScale opts)) (\() m -> m) () setup (\app _ -> app)

-- | Model-driven runner. Fold emitted messages through the update function
-- in order, with the message type checked at compile time.
runRgfwAppReduce ::
  Eq model =>
  RgfwOptions ->
  (msg -> model -> model) ->
  model ->
  (model -> NanoUIE msg ()) ->
  IO ()
runRgfwAppReduce opts =
  runRgfwAppReduceCustom opts (\_ -> (optionsTheme opts, optScale opts))

-- | Reducer runner whose theme and UI scale are derived from the current
-- model. A non-positive scale falls back to 'optScale', then the monitor's.
-- 'optTheme' and 'optThemeFor' are not used; the model supplies the theme.
-- OpenGL calls remain on the creating OS thread; native resources are
-- released on exit.
runRgfwAppReduceCustom ::
  Eq model =>
  RgfwOptions ->
  (model -> (Theme, Float)) ->
  (msg -> model -> model) ->
  model ->
  (model -> NanoUIE msg ()) ->
  IO ()
runRgfwAppReduceCustom opts getThemeAndScale updateModel initialModel view =
  runRgfwAppReduceCustomWith opts getThemeAndScale updateModel initialModel (pure ()) (\_ -> view)

-- | 'runRgfwAppReduceCustom' with a setup action run once the window and its
-- OpenGL context exist, before the first frame, whose result the view takes.
runRgfwAppReduceCustomWith ::
  Eq model =>
  RgfwOptions ->
  (model -> (Theme, Float)) ->
  (msg -> model -> model) ->
  model ->
  IO env ->
  (env -> model -> NanoUIE msg ()) ->
  IO ()
runRgfwAppReduceCustomWith opts getThemeAndScale updateModel initialModel setup appView = inBoundThread $ do
  sh <- RgfwShared opts <$> newIORef (snd (getThemeAndScale initialModel)) <*> newIORef Nothing
  bracket (createRgfwWindow sh settings) R.closeWindow (runWindow sh)
  where
    settings = optWindow opts
    -- The GL context is current only on the OS thread that created it.
    inBoundThread act = if rtsSupportsBoundThreads then runInBoundThread act else act
    runWindow sh win = do
      rw <- setupRgfwWindow sh settings win
      reducer <- newReducer updateModel initialModel
      let initTheme = fst (getThemeAndScale initialModel)
      -- The theme last applied, before squaring: comparing it with the
      -- model's theme skips rebuilding the squared theme on every frame.
      themeRef <- newIORef initTheme
      ctx <- newRgfwWindowContext initTheme
      env <- setup
      setExplainLayout ctx (optExplainLayout opts)
      installRgfwHost settings rw ctx
      initInp <- rgfwOpeningInput rw

      let cleanup = closeRgfwContext ctx
          release r = cleanup `finally` (useGl sh win >> freeGlRenderer r)
      bracket (newGlRenderer `onException` cleanup) release $ \renderer -> R.withEventBuffer $ \evPtr -> do
        windows <- newWindows (childOps sh)
        let drawOne force c curInp =
              presentRgfw sh rw renderer force c curInp $ do
                curModel <- reducerModel reducer
                let (frameTheme, _) = getThemeAndScale curModel
                appliedTheme <- readIORef themeRef
                when (frameTheme /= appliedTheme) $ do
                  writeIORef themeRef frameTheme
                  applyRgfwTheme c frameTheme
                -- A reduced message that switched themes changed the model, so
                -- the core marks the frame dirty and the next one applies it.
                reduceFrame reducer c curInp (appView env)
            -- With further windows open, one drain takes every window's
            -- events and queues the others' on them; with none, this
            -- window's own events are taken as in a session of one.
            pollMain = do
              kids <- openWindows windows
              if null kids
                then pollRgfwEvents win evPtr (rwScale rw) (rwMonScale rw)
                else drainRgfwEvents evPtr rw [(rwWin (rcWindow c), (c, queue)) | (c, queue) <- kids]
            drv =
              SessionDriver
                { sdPollEvents    = pollMain
                , sdWaitEvents    = \t -> R.waitForEvent t >> pollMain
                , sdApplyEvent    = applyRgfwEvent
                , sdIsButtonEdge  = isRgfwButtonEdge
                , sdIsSessionQuit = isRgfwClose
                , sdSyncDisplay   = \c inp -> do
                    curModel <- reducerModel reducer
                    writeIORef (rsUserScale sh) $! snd (getThemeAndScale curModel)
                    syncRgfwDisplay sh rw c inp
                , sdDebug         = rdsSampler (rwDebug rw)
                , sdContinuous    = False
                  -- Presents are unthrottled (swap interval 0, no vsync), so a
                  -- live in-view animation is paced at the refresh period
                  -- instead of spinning.
                , sdPacingMs      = rgfwPacingMs opts
                , sdPresentPaces  = pure False
                , sdShouldDraw    = \c prevInp inpSynced wasAnim refreshDue ->
                    shouldRedrawFrame c prevInp inpSynced wasAnim False refreshDue
                , sdDraw          = \c curInp forceFull -> drawOne forceFull c curInp
                , sdOnCursor      = syncRgfwCursor rw
                , sdShouldQuit    = \_ -> False
                , sdAlignSec      = 1 / fromIntegral (rgfwRefreshHz opts)
                , sdWindows       = Just (loopWindows windows)
                }
        -- Present the opening frame before entering the loop so the window has
        -- content immediately; once idle the loop blocks with no redraws. If
        -- the opening frame marked the context dirty, the loop draws the
        -- follow-up frame immediately instead of blocking for input. The
        -- windows it declared open now. Views find the debug sampler on the
        -- context ('askRgfwDebug').
        withBackendSession ctx rgfwSession (rwDebug rw) $ do
          _ <- drawOne True ctx initInp
          afterMainPass windows ctx
          runSessionLoop drv ctx initInp `finally` closeWindows windows

-- | What the windows of one RGFW session share: the options, the UI scale
-- the model asks for, and the window whose OpenGL context is current.
data RgfwShared = RgfwShared
  { rsOpts :: !RgfwOptions
  , rsUserScale :: !(IORef Float)
  , rsCurrent :: !(IORef (Maybe R.Window))
  }

-- | One window and the state drawing into it needs.
data RgfwWindow = RgfwWindow
  { rwWin :: !R.Window
  , rwScale :: !(IORef Float)
  , rwMonScale :: !(IORef Float)
  , rwSize :: !(IORef (Int, Int))
  , rwPresented :: !(IORef (Int, Int, Float))
  -- ^ Physical size and scale of the last presented frame. The retained
  -- framebuffer holds that frame, so a frame at another size or scale has
  -- nothing to keep and paints in full.
  , rwCursor :: !(IORef UiCursorKind)
  , rwDebug :: !RgfwDebugSampler
  }

rgfwRefreshHz :: RgfwOptions -> Int
rgfwRefreshHz opts = if optRefreshHz opts > 0 then optRefreshHz opts else 60

rgfwPacingMs :: RgfwOptions -> Int
rgfwPacingMs opts = max 1 (floor (1000 / fromIntegral (rgfwRefreshHz opts) :: Double) - 2)

-- | The model's scale, else the options', else the monitor's.
resolveScale :: RgfwOptions -> Float -> Float -> Float
resolveScale opts userScale monScale = fromMaybe 1 (find (> 0) [userScale, optScale opts, monScale])

-- | Native pixel size of a layout size at a scale.
pixelsAt :: Float -> Size -> (Int, Int)
pixelsAt scale (Size w h) = (max 1 (round (w * scale)), max 1 (round (h * scale)))

-- | Native pixels to layout units at a scale.
logicalSize :: (Int, Int) -> Float -> Size
logicalSize (pw, ph) scale = Size (units pw) (units ph)
  where
    units v = fromIntegral (max 1 (round (fromIntegral v / scale) :: Int))

-- | Open a window for the settings, hidden, at the requested scale: the
-- monitor's scale is unknown until the window exists. 'setupRgfwWindow'
-- sizes it once that scale is known, and 'installRgfwHost' shows it. Its
-- OpenGL context is current afterwards.
createRgfwWindow :: RgfwShared -> WindowSettings -> IO R.Window
createRgfwWindow sh settings = do
  userScale <- readIORef (rsUserScale sh)
  let flags =
        R.rgfw_windowHide
          .|. (if wsResizable settings then 0 else R.rgfw_windowNoResize)
          .|. (if wsMode settings == Fullscreen then R.rgfw_windowFullscreen else 0)
      (openW, openH) = pixelsAt (resolveScale (rsOpts sh) userScale 1) (wsSize settings)
  R.createWindowGL (T.unpack (wsTitle settings)) 0 0 openW openH flags 3 2 >>= \case
    Nothing -> fail "nano-ui-rgfw: could not create a window with an OpenGL 3.2 core context"
    Just win -> win <$ writeIORef (rsCurrent sh) (Just win)

-- | Size a window just opened at its monitor's scale.
setupRgfwWindow :: RgfwShared -> WindowSettings -> R.Window -> IO RgfwWindow
setupRgfwWindow sh settings win = do
  userScale <- readIORef (rsUserScale sh)
  monScaleInit <- R.windowScale win
  let opts = rsOpts sh
      !monScale = if monScaleInit > 0.0 then monScaleInit else 1.0
      !scale = resolveScale opts userScale monScale
  -- A window opened fullscreen already covers its monitor, and resizing it
  -- would shrink it on Windows.
  phys <-
    if wsMode settings == Fullscreen
      then R.windowSize win
      else do
        let phys = pixelsAt scale (wsSize settings)
        when (phys /= pixelsAt (resolveScale opts userScale 1) (wsSize settings)) $ uncurry (R.resizeWindow win) phys
        pure phys
  RgfwWindow win
    <$> newIORef scale
    <*> newIORef monScale
    <*> newIORef phys
    <*> newIORef (0, 0, 0)
    <*> newIORef UiCursorDefault
    <*> newRgfwDebugSampler

-- | The opening input: the window's size, and the pointer off the window
-- until an entry or a motion says where it is, not on the widget in the
-- top-left corner.
rgfwOpeningInput :: RgfwWindow -> IO Input
rgfwOpeningInput rw = do
  phys <- readIORef (rwSize rw)
  scale <- readIORef (rwScale rw)
  pure (applyPointerLeave emptyInput {inputWindowSize = logicalSize phys scale})

-- | A context for a window's view, with the theme squared and the
-- clipboard, and a wake that ends the event wait (or the next one).
newRgfwWindowContext :: Theme -> IO Context
newRgfwWindowContext theme = do
  ctx <- (\c -> withClipboard c R.readClipboardText R.writeClipboardText) <$> newRgfwContext theme
  ctx <$ setWakeLoop ctx R.stopWaitForEvent

-- | Cancel a window's view's requests and background jobs, as it closes.
closeRgfwContext :: Context -> IO ()
closeRgfwContext ctx = closeWindowHost ctx `finally` cancelTasks ctx

isRgfwButtonEdge, isRgfwClose :: RgfwEvent -> Bool
isRgfwButtonEdge = \case RgfwEvButton {} -> True; _ -> False
isRgfwClose = \case RgfwEvClose -> True; _ -> False

-- | Install the host that carries out a view's window requests, report the
-- window's state, and show it unless the settings hide it.
installRgfwHost :: WindowSettings -> RgfwWindow -> Context -> IO ()
installRgfwHost settings rw ctx = do
  -- Size limits are in layout units, converted at the current scale.
  let win = rwWin rw
      limit set s = do
        scale <- readIORef (rwScale rw)
        uncurry (set win) (sizeLimitAt scale (0, 0) s)
      place = case wsPosition settings of
        WindowPositionDefault -> WindowPositionCentered
        p -> p
  installWindowHost ctx settings {wsPosition = place} $
    defaultWindowHost
      { hostCapabilities = allWindowCapabilities {wcOpacity = False}
      , hostSetTitle = R.setWindowName win
      , hostSetIcon = \p -> void (R.setWindowIcon win (rgbaWidth p) (rgbaHeight p) (rgbaBytes p))
      , hostSetMinSize = limit R.setWindowMinSize
      , hostSetMaxSize = limit R.setWindowMaxSize
        -- No 'hostSetOpacity': RGFW windows cannot fade.
      , hostSetMode = \case
          -- Leaving fullscreen restores the geometry saved on entering
          -- it, which on Windows a windowed window has none of.
          Windowed -> do
            full <- (\f -> f .&. R.rgfw_windowFullscreen /= 0) <$> R.windowFlags win
            when full (R.setWindowFullscreen win False)
            R.showWindow win
          Fullscreen -> R.showWindow win >> R.setWindowFullscreen win True
          Hidden -> R.hideWindow win
      , hostMove = R.moveWindow win
      , hostCenter = R.centerWindow win
      , hostResize = \s -> readIORef (rwScale rw) >>= \scale -> uncurry (R.resizeWindow win) (pixelsAt scale s)
      , hostMinimize = R.minimizeWindow win
      , hostMaximize = R.maximizeWindow win
      , hostRestore = R.restoreWindow win
      }
  reportWindowState ctx =<< rgfwWindowState win =<< readIORef (rwScale rw)
  unless (wsMode settings == Hidden) (R.showWindow win)

-- | Make the window's OpenGL context current, unless it is.
useGl :: RgfwShared -> R.Window -> IO ()
useGl sh win = do
  cur <- readIORef (rsCurrent sh)
  unless (cur == Just win) $ R.makeCurrentGL win >> writeIORef (rsCurrent sh) (Just win)

-- | Read the window's size and scale into the frame's input.
syncRgfwDisplay :: RgfwShared -> RgfwWindow -> Context -> Input -> IO (Context, Input)
syncRgfwDisplay sh rw c inp = do
  -- A minimized window reports 0x0 (on Windows): keep the last real size.
  (curWinW, curWinH) <- R.windowSize (rwWin rw)
  (pw0, ph0) <- readIORef (rwSize rw)
  let (pw, ph) = if curWinW > 0 && curWinH > 0 then (curWinW, curWinH) else (pw0, ph0)
  writeIORef (rwSize rw) (pw, ph)
  curMonScale <- readIORef (rwMonScale rw)
  userScale <- readIORef (rsUserScale sh)
  oldScale <- readIORef (rwScale rw)
  let !newScale = resolveScale (rsOpts sh) userScale curMonScale
      -- Motion was divided by the old scale when decoded.
      V2 mx my = inputMousePos inp
      !k = oldScale / newScale
      mouse = if k == 1 then inputMousePos inp else V2 (mx * k) (my * k)
  writeIORef (rwScale rw) newScale
  reportWindowState c =<< rgfwWindowState (rwWin rw) newScale
  pure (c, inp {inputWindowSize = logicalSize (pw, ph) newScale, inputMousePos = mouse})

-- | Touch the platform cursor only when the wanted kind changes.
-- 'UiCursorHidden' hides the pointer. Each window has its own.
syncRgfwCursor :: RgfwWindow -> Context -> Input -> IO ()
syncRgfwCursor rw c inp = uiCursorKind c inp >>= syncCursorKind (rwCursor rw) (R.showMouse win) (setIcon . mapRgfwCursor)
  where
    win = rwWin rw
    setIcon icon = void $ if icon == R.rgfw_mouseArrow then R.setMouseDefault win else R.setMouseStandard win icon

-- | Run a frame's UI pass and present it. A frame whose damage is empty
-- would swap in the picture already on screen, so it skips the render and
-- the swap: an animation scrolled out of view costs its UI pass and nothing
-- on the GPU. The opening frame and an animation's settle frame are forced.
-- Other frames paint only their damage over the retained last frame.
presentRgfw :: RgfwShared -> RgfwWindow -> GlRenderer -> Bool -> Context -> Input -> IO (a, DrawData, Bool) -> IO Bool
presentRgfw sh rw renderer force c curInp runUi = do
  useGl sh win
  tUiStart <- getMonotonicTime
  curScale <- readIORef (rwScale rw)
  (pw, ph) <- readIORef (rwSize rw)
  presented <- readIORef (rwPresented rw)
  let !paintFull = force || presented /= (pw, ph, curScale) || inputWindowRedraw curInp
  writeIORef (ctxPaintFull c) paintFull
  (_, drawData, dirtyAfterUi) <- runUi
  tUiEnd <- getMonotonicTime
  damage <- takeDamage c
  if damageIsEmpty damage && not paintFull
    then do
      noteDebugSkip (rdsSampler debugSampler)
      -- Nothing was drawn, so the retained framebuffer holds this frame.
      answerScreenshots c (retainedPixels renderer pw ph)
      pure dirtyAfterUi
    else do
      tRenderStart <- getMonotonicTime
      curMonScale <- readIORef (rwMonScale rw)
      (baseSpans, overlaySpans) <- collectRasterSpans c curInp
      pieces <- if paintFull then pure [] else takeDamagePieces c
      syncImagesGl renderer c
      -- A full frame clears to the draw data's colour, the page
      -- over the window colour of the theme it was painted in.
      renderArenaGl renderer getCozetteFont curScale pw ph
          (if paintFull then DamageFull else damage) pieces drawData baseSpans overlaySpans
      writeIORef (rwPresented rw) $! (pw, ph, curScale)
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
      pure dirtyAfterUi
  where
    win = rwWin rw
    debugSampler = rwDebug rw

-- | A further window ('NanoUI.nativeWindow'): the window, its renderer, and
-- the context its view runs on.
data RgfwChild = RgfwChild
  { rcWindow :: !RgfwWindow
  , rcRenderer :: !GlRenderer
  , rcContext :: !Context
  }

-- | How the session opens, draws and closes further windows. Each has its
-- own OpenGL context and renderer; the first window's drain queues their
-- events ('drainRgfwEvents').
childOps :: RgfwShared -> WindowOps RgfwChild RgfwEvent
childOps sh =
  WindowOps
    { woOpen = \parentCtx settings -> do
        win <- createRgfwWindow sh settings
        rw <- setupRgfwWindow sh settings win `onException` R.closeWindow win
        ctx <- newRgfwWindowContext =<< readIORef (ctxTheme parentCtx)
        renderer <- newGlRenderer `onException` R.closeWindow win
        installRgfwHost settings rw ctx
        setBackendSession ctx rgfwSession (rwDebug rw)
        pure (RgfwChild rw renderer ctx, ctx)
    , woClose = \c -> do
        let win = rwWin (rcWindow c)
        closeRgfwContext (rcContext c) `finally` do
          useGl sh win
          freeGlRenderer (rcRenderer c)
          R.closeWindow win
          writeIORef (rsCurrent sh) Nothing
    , woApplyEvent = applyRgfwEvent
    , woIsButtonEdge = isRgfwButtonEdge
    , woIsClose = isRgfwClose
    , woSync = syncRgfwDisplay sh . rcWindow
    , woDraw = \c ctx view inp force -> presentRgfw sh (rcWindow c) (rcRenderer c) force ctx inp (runFrame ctx inp view)
      -- Each window has its own cursor.
    , woCursor = syncRgfwCursor . rcWindow
    , woPacingMs = \_ -> rgfwPacingMs (rsOpts sh)
    }

-- | Take every window's events: the first window's are answered, and each
-- further window's queued on it. Events for a window just closed go nowhere.
drainRgfwEvents :: Ptr R.RGFW_event -> RgfwWindow -> [(R.Window, (RgfwChild, RgfwEvent -> IO ()))] -> IO [RgfwEvent]
drainRgfwEvents evPtr first kids = do
  raw <- go []
  forM_ kids $ \(win, (c, queue)) -> decodeFor (rcWindow c) [ev | (w, ev) <- raw, w == win] >>= mapM_ queue
  decodeFor first [ev | (w, ev) <- raw, w == rwWin first || w == R.Window nullPtr]
  where
    -- Prepared in queue order, for the window each is for: a scale change
    -- applies to that window's later events.
    go acc =
      R.pollAnyEvent evPtr >>= \case
        Nothing -> pure (reverse acc)
        Just (win, ev) -> do
          let target = maybe first (rcWindow . fst) (lookup win kids)
          ev' <- prepareRgfwEvent (rwMonScale target) ev
          go ((win, ev') : acc)
    decodeFor rw evs
      | null evs = pure []
      | otherwise = (`decodeRgfwEvents` evs) <$> readIORef (rwScale rw)

-- | The window's state at a scale, from what RGFW tracks through its events,
-- so reading it costs no round trip to the desktop.
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
  | RgfwEvKey !Word32 !Word8 !Bool -- ^ key, modifiers, down (including repeats) or up
  | RgfwEvFocusLost -- ^ the window lost keyboard focus
  | RgfwEvRedraw -- ^ the window was uncovered or restored: paint it all

-- | Drain the RGFW queue, recording scale changes for the next sync, which
-- reads the window size itself. Character keys are remapped to the current
-- layout ('layoutKey').
pollRgfwEvents :: R.Window -> Ptr R.RGFW_event -> IORef Float -> IORef Float -> IO [RgfwEvent]
pollRgfwEvents win evPtr scaleRef monScaleRef = do
  raw <- drain []
  scale <- readIORef scaleRef
  pure (decodeRgfwEvents scale raw)
  where
    drain acc =
      R.pollEvent win evPtr >>= \case
        R.EventNone -> pure (reverse acc)
        ev -> prepareRgfwEvent monScaleRef ev >>= \ev' -> drain (ev' : acc)

-- | Read one raw event before decoding: record a scale change for the next
-- sync, and remap a key to the current layout.
prepareRgfwEvent :: IORef Float -> R.Event -> IO R.Event
prepareRgfwEvent monScaleRef ev = case ev of
  R.EventScaleUpdate sx _ -> ev <$ writeIORef monScaleRef (if sx > 0 then sx else 1)
  R.EventKeyPress k m -> relayout R.EventKeyPress k m
  R.EventKeyRepeat k m -> relayout R.EventKeyRepeat k m
  -- A modifier key let go while the one on the other side is down
  -- leaves the modifier held, as that key's repeat would.
  R.EventKeyRelease k m
    | Just (_, twin) <- lookup k modifierRgfwKeys ->
        (\held -> if held then R.EventKeyRepeat twin m else ev) <$> R.isKeyDown twin
    | otherwise -> relayout R.EventKeyRelease k m
  _ -> pure ev
  where
    relayout event k m = (\k' -> event k' m) <$> layoutKey k
    -- RGFW key codes are what the key types on a US layout. Letter and
    -- punctuation keys are remapped to the current layout when the result is
    -- ASCII. Digits stay, since some layouts type symbols there unshifted.
    layoutKey k
      | k > 32 && k < 127 && not (isDigit (chr (fromIntegral k))) =
          maybe k (\mapped -> if mapped > 32 && mapped < 127 then mapped else k) <$> R.physicalToMappedKey k
      | otherwise = pure k

-- | Translate a batch of raw events in queue order. Pointer positions are
-- divided by the logical scale. Control characters are dropped: some
-- platforms send Ctrl+letter as one, and the key event already reports the
-- chord.
decodeRgfwEvents :: Float -> [R.Event] -> [RgfwEvent]
decodeRgfwEvents scale = mapMaybe $ \case
  R.EventWindowClose -> Just RgfwEvClose
  R.EventWindowResize _ _ -> Just RgfwEvResize
  R.EventScaleUpdate _ _ -> Just RgfwEvResize
  R.EventMouseMotion x y -> Just (RgfwEvMotion (fromIntegral x / scale) (fromIntegral y / scale))
  R.EventMouseButton btn down -> Just (RgfwEvButton btn down)
  -- RGFW's wheel is positive up and left; the input's is down and right.
  R.EventMouseScroll dx dy -> Just (RgfwEvScroll (negate dx) (negate dy))
  R.EventOther t
    | t == R.rgfw_mouseLeave -> Just RgfwEvLeave
    | t == R.rgfw_windowFocusOut -> Just RgfwEvFocusLost
    | t == R.rgfw_windowRefresh || t == R.rgfw_windowRestored -> Just RgfwEvRedraw
  R.EventKeyPress k m -> Just (RgfwEvKey k m True)
  R.EventKeyRepeat k m -> Just (RgfwEvKey k m True)
  R.EventKeyRelease k m -> Just (RgfwEvKey k m False)
  -- Every character but the controls types, joiners and bidi marks included.
  -- On macOS, RGFW passes on the private-use characters AppKit gives arrow
  -- and function keys (U+F700 to U+F747), which type nothing. The rest of
  -- the private-use area types, the Apple logo (U+F8FF) among it.
  R.EventKeyChar ch | not (isControl ch) && not (ch >= '\xF700' && ch <= '\xF747') -> Just (RgfwEvChar ch)
  _ -> Nothing

-- | Accumulate a decoded event into frame input. The caller handles close
-- and resize events separately; motion coordinates are already scaled by decoding.
applyRgfwEvent :: Input -> RgfwEvent -> Input
applyRgfwEvent inp ev = case ev of
  RgfwEvClose -> inp
  RgfwEvResize -> inp
  RgfwEvMotion x y -> inp {inputMousePos = V2 x y}
  -- RGFW numbers buttons from 0 in 'mouseButtonNumber' order: left, middle,
  -- right, back, forward, then the rest.
  RgfwEvButton btn down -> applyMouseButton (mouseButtonNumber (fromIntegral btn + 1)) down inp
  -- A drag keeps its pointer ('applyPointerLeave').
  RgfwEvLeave -> applyPointerLeave inp
  RgfwEvScroll dx dy -> inp {inputScroll = v2Add (inputScroll inp) (V2 dx dy)}
  RgfwEvChar c -> inp {inputChars = T.snoc (inputChars inp) c}
  -- 'applyKey' detects auto-repeat because the key is already held.
  RgfwEvKey k m down ->
    (maybe inp (\key -> applyKey key down inp) (mapRgfwKey k m)) {inputModifiers = modsFromRgfw (ownModifier k down m)}
  RgfwEvFocusLost -> releaseAllKeys inp
  RgfwEvRedraw -> inp {inputWindowRedraw = True}
