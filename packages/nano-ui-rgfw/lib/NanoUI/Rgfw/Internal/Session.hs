-- | The RGFW window session: options, the runners, and translation of RGFW
-- events into 'NanoUI.Input.Input'.
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
  ) where

import Control.Concurrent (rtsSupportsBoundThreads, runInBoundThread)
import Control.Exception (bracket)
import Control.Monad (void, when)
import Data.Bits ((.&.))
import Data.Char (chr, isPrint, ord)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Data.Word (Word8, Word32)
import Foreign.Ptr (Ptr)
import GHC.Clock (getMonotonicTime)
import NanoUI
  ( NanoUI
  , Size (..)
  , Theme (..)
  , V2 (..)
  , tomorrowNightMinDarkTheme
  , v2Add
  )
import NanoUI.Backend
  ( Damage (..)
  , Input (..)
  , Key (..)
  , Modifiers (..)
  , MouseButton (..)
  , appendInputKey
  , applyMouseButton
  , emptyInput
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
import NanoUI.Rgfw.Internal.Gl (freeGlRenderer, newGlRenderer, renderArenaGl)
import qualified RGFW as R

-- | Window and rendering options for the RGFW runners.
data RgfwOptions = RgfwOptions
  { optTitle  :: !String
  , optWidth  :: !Int
  , optHeight :: !Int
  , optTheme  :: !Theme
  -- ^ Any core theme; the backend draws it square ('NanoUI.Rgfw.Internal.Context.applyRgfwTheme').
  , optCenter :: !Bool
  -- ^ Center the window on the screen.
  , optScale  :: !Float
  -- ^ UI scale. @0@ follows the monitor's scale.
  , optRefreshHz :: !Int
  -- ^ Frame pacing rate while animating. @0@ means 60.
  }

-- | Centred 1680x1040 window with the dark theme, monitor scale, and 60 Hz
-- animation pacing. Width and height are native window pixels.
defaultRgfwOptions :: RgfwOptions
defaultRgfwOptions =
  RgfwOptions
    { optTitle  = "nano-ui (RGFW Single-Pass)"
    , optWidth  = 1680
    , optHeight = 1040
    , optTheme  = tomorrowNightMinDarkTheme
    , optCenter = True
    , optScale  = 0.0
    , optRefreshHz = 0
    }

mapRgfwKey :: Word32 -> Maybe Key
mapRgfwKey k
  | k == R.rgfw_keyBackSpace = Just KeyBackspace
  | k == R.rgfw_keyDelete = Just KeyDelete
  | k == R.rgfw_keyReturn = Just KeyEnter
  | k == R.rgfw_keyEscape = Just KeyEscape
  | k == R.rgfw_keyTab = Just KeyTab
  | k == R.rgfw_keyUp = Just KeyUp
  | k == R.rgfw_keyDown = Just KeyDown
  | k == R.rgfw_keyLeft = Just KeyLeft
  | k == R.rgfw_keyRight = Just KeyRight
  | k == R.rgfw_keyEnd = Just KeyEnd
  | k == R.rgfw_keyHome = Just KeyHome
  | otherwise = Nothing

-- | Decode RGFW key modifier bits. Super (Cmd) counts as Ctrl.
modsFromRgfw :: Word8 -> Modifiers
modsFromRgfw m =
  Modifiers
    { modShift = has R.rgfw_modShift
    , modCtrl = has R.rgfw_modControl || has R.rgfw_modSuper
    , modAlt = has R.rgfw_modAlt
    }
  where
    has bit = m .&. bit /= 0

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
  runRgfwAppReduceCustom opts (\_ -> (optTheme opts, optScale opts))

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
  let flags = if optCenter opts then R.rgfw_windowCenter else 0
  bracket
    (R.createWindowGL (optTitle opts) 0 0 (optWidth opts) (optHeight opts) flags 3 2)
    (mapM_ R.closeWindow) $ \mWin -> case mWin of
      Nothing -> putStrLn "Failed to create RGFW window with an OpenGL 3.2 context."
      Just win -> runWindow win
  where
    -- The GL context is current only on the OS thread that created it.
    inBoundThread act = if rtsSupportsBoundThreads then runInBoundThread act else act
    runWindow win = do
      let !refreshHz = if optRefreshHz opts > 0 then optRefreshHz opts else 60
          !refreshSec = 1.0 / fromIntegral refreshHz :: Double
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

      let (initTheme, initScaleChoice) = getThemeAndScale initialModel
          !initScale = resolveScale initScaleChoice initMonScale
          !initPhysW = optWidth opts
          !initPhysH = optHeight opts
          !initLogW = max 1 (round (fromIntegral initPhysW / initScale) :: Int)
          !initLogH = max 1 (round (fromIntegral initPhysH / initScale) :: Int)

      modelRef <- newIORef initialModel
      scaleRef <- newIORef initScale
      winSizeRef <- newIORef (initPhysW, initPhysH)
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
      let font = getCozetteFont
          initInp =
            emptyInput
              { inputWindowSize = Size (fromIntegral initLogW) (fromIntegral initLogH)
              }
          -- Set the pointer shape only when the wanted kind changes.
          syncCursor c inp = do
            want <- uiCursorKind c inp
            cur <- readIORef cursorRef
            when (want /= cur) $ do
              writeIORef cursorRef want
              let icon = mapRgfwCursor want
              void $
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
              let !uiMs = (tUiEnd - tUiStart) * 1000.0
              damage <- takeDamage c
              if damageIsEmpty damage && not paintFull
                then do
                  noteDebugSkip (rdsSampler debugSampler)
                  pure (dirtyAfterUi, curInp)
                else do
                  tRenderStart <- getMonotonicTime
                  curMonScale <- readIORef monScaleRef
                  (baseSpans, overlaySpans) <- collectRasterSpans c curInp
                  kept <-
                    renderArenaGl renderer font curScale pw ph (themeWindow frameTheme)
                      (if paintFull then DamageFull else damage) drawData baseSpans overlaySpans
                  -- A framebuffer the renderer had to replace held nothing to
                  -- keep: forget the size, and the frame asked for next is full.
                  writeIORef presentedRef (if kept then (pw, ph, curScale) else (0, 0, 0))
                  tRenderEnd <- getMonotonicTime
                  let !renderMs = (tRenderEnd - tRenderStart) * 1000.0

                  tSwapStart <- getMonotonicTime
                  R.swapBuffersGL win
                  tSwapEnd <- getMonotonicTime
                  let !swapMs = (tSwapEnd - tSwapStart) * 1000.0
                      !frameMs = (tSwapEnd - tUiStart) * 1000.0

                  nodes <- arenaCount (ctxNodeArena c)
                  noteDebugPresent (rdsSampler debugSampler) uiMs renderMs swapMs frameMs
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
                  pure (dirtyAfterUi || not kept, curInp)

        let drv =
              SessionDriver
                { sdPollEvents    = pollRgfwEvents win evPtr scaleRef monScaleRef winSizeRef
                , sdWaitEvents    = \t -> do
                    R.waitForEvent t
                    pollRgfwEvents win evPtr scaleRef monScaleRef winSizeRef
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
                    pure (c, inp { inputWindowSize = Size (fromIntegral lw) (fromIntegral lh) })
                , sdDebug         = rdsSampler debugSampler
                , sdContinuous    = False
                  -- Presents are unthrottled (swap interval 0, no vsync), so a
                  -- live in-view animation is paced at the refresh period
                  -- instead of spinning.
                , sdPacingMs      = animateTimeout
                , sdPresentPaces  = pure False
                , sdShouldDraw    = \c prevInp inpSynced wasAnim refreshDue ->
                    shouldRedrawFrame c prevInp inpSynced wasAnim False refreshDue
                , sdDraw          = \c curInp forceFull -> drawOne forceFull c curInp
                , sdOnCursor      = syncCursor
                , sdShouldQuit    = \_ -> False
                , sdAlignSec      = refreshSec
                }
        -- Present the opening frame before entering the loop so the window has
        -- content immediately; once idle the loop blocks with no redraws. The
        -- context stays as that frame left it: if it asked for another by
        -- marking the context dirty, the loop draws it at once instead of
        -- blocking until some input happens along.
        (_, inpStart) <- drawOne True ctx initInp
        runSessionLoop drv ctx inpStart

-- | An RGFW event translated for the input fold.
data RgfwEvent
  = RgfwEvClose
  | RgfwEvResize -- ^ window size or monitor scale changed (read back at sync)
  | RgfwEvMotion !Float !Float
  | RgfwEvButton !Word8 !Bool
  | RgfwEvScroll !Float !Float
  | RgfwEvChar !Char !Bool -- ^ typed character; True when a Ctrl chord typed it
  | RgfwEvKeyPress !Word32 !Word8
  | RgfwEvKeyRelease !Word8

-- | Drain the RGFW queue, recording size and scale changes for the next sync.
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
        _ -> drain (ev : acc)

-- | What the previous event typed. One keystroke can queue both a key-char
-- and a Ctrl+letter key-press event, in either order (X11 queues the char
-- first), and must type its letter once.
data Typed = TypedNothing | TypedByChar !Char | TypedByChord !Char
  deriving (Eq)

-- | Translate a batch of raw events in queue order. Pointer positions are
-- divided by the logical scale. A Ctrl+letter press types its letter unless
-- the adjacent key-char event of the same keystroke already did, and a
-- key-char event right after such a press is dropped.
decodeRgfwEvents :: Float -> [R.Event] -> [RgfwEvent]
decodeRgfwEvents scale = go TypedNothing
  where
    go _ [] = []
    go typed (ev : rest) = case ev of
      R.EventKeyPress k m
        | modCtrl (modsFromRgfw m) && k >= R.rgfw_keyA && k <= R.rgfw_keyZ ->
            let c = chr (fromIntegral k)
             in RgfwEvKeyPress k m
                  : if typed == TypedByChar c
                      then go TypedNothing rest
                      else RgfwEvChar c True : go (TypedByChord c) rest
      -- Control codes \x01..\x1a are Ctrl+letter chords; backspace and delete
      -- are keys, not text.
      R.EventKeyChar ch
        | ch /= '\b' && ch /= '\DEL' && (isPrint ch || chord) ->
            let c = if chord then chr (ord ch + 96) else ch
             in if typed == TypedByChord c
                  then go TypedNothing rest
                  else RgfwEvChar c chord : go (TypedByChar c) rest
        where
          chord = ch >= '\x01' && ch <= '\x1a'
      _ -> maybe id (:) (untyped ev) (go TypedNothing rest)
    untyped ev = case ev of
      R.EventWindowClose -> Just RgfwEvClose
      R.EventWindowResize _ _ -> Just RgfwEvResize
      R.EventScaleUpdate _ _ -> Just RgfwEvResize
      R.EventMouseMotion x y -> Just (RgfwEvMotion (fromIntegral x / scale) (fromIntegral y / scale))
      R.EventMouseButton btn down -> Just (RgfwEvButton btn down)
      R.EventMouseScroll dx dy -> Just (RgfwEvScroll dx dy)
      R.EventKeyPress k m -> Just (RgfwEvKeyPress k m)
      R.EventKeyRelease _ m -> Just (RgfwEvKeyRelease m)
      _ -> Nothing

-- | Accumulate a decoded event into frame input. The caller handles close
-- and resize events separately; motion coordinates are already scaled by decoding.
applyRgfwEvent :: Input -> RgfwEvent -> Input
applyRgfwEvent inp ev = case ev of
  RgfwEvClose -> inp
  RgfwEvResize -> inp
  RgfwEvMotion x y -> inp {inputMousePos = V2 x y}
  RgfwEvButton btn down
    | btn == R.rgfw_mouseLeft -> applyMouseButton MouseLeft down inp
    | btn == R.rgfw_mouseRight -> applyMouseButton MouseRight down inp
    | otherwise -> inp
  RgfwEvScroll dx dy -> inp {inputScroll = v2Add (inputScroll inp) (V2 dx dy)}
  RgfwEvChar c chord ->
    inp
      { inputChars = T.snoc (inputChars inp) c
      , inputModifiers = if chord then (inputModifiers inp) {modCtrl = True} else inputModifiers inp
      }
  RgfwEvKeyPress k m ->
    inp
      { inputKeys = maybe id appendInputKey (mapRgfwKey k) (inputKeys inp)
      , inputModifiers = modsFromRgfw m
      }
  RgfwEvKeyRelease m -> inp {inputModifiers = modsFromRgfw m}

isRgfwButtonEdge :: RgfwEvent -> Bool
isRgfwButtonEdge (RgfwEvButton {}) = True
isRgfwButtonEdge _ = False

isRgfwSessionQuit :: RgfwEvent -> Bool
isRgfwSessionQuit RgfwEvClose = True
isRgfwSessionQuit _ = False
