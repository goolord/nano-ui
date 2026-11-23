{-# LANGUAGE MultiWayIf #-}

-- | The browser session: options, the event loop over a 'WebHost', display
-- sync, and frame presentation. The loop is "NanoUI.Runner"'s, as on the
-- desktop backends: its wait for events is a wait on a JavaScript promise,
-- which lets the browser run until an event or a wake arrives.
module NanoUI.SdlWeb.Internal.Session
  ( SdlWebOptions (..)
  , defaultSdlWebOptions
  , WebFont (..)
  , runWebSession
  , WebFrame
  ) where

import Control.Exception (bracket, finally)
import Control.Monad (forM_, unless, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import GHC.Clock (getMonotonicTime)
import NanoUI
  ( Appearance (..)
  , Rect (..)
  , Size (..)
  , Theme
  , WindowSettings (..)
  , defaultWindowSettings
  , followSystemTheme
  )
import NanoUI.Backend
  ( Damage (..)
  , DrawData (..)
  , Input (..)
  , InputPurpose (..)
  , TextInputArea (..)
  , UiCursorKind (..)
  , WindowHost (..)
  , WindowState (..)
  , closeWindowHost
  , cancelTasks
  , damageIsEmpty
  , defaultWindowHost
  , defaultWindowState
  , emptyInput
  , installWindowHost
  , markDirty
  , noWindowCapabilities
  , reportWindowState
  , setExplainLayout
  , setSystemAppearance
  , setWakeLoop
  , syncCursorKind
  , takeDamage
  , textInputArea
  , uiCursorKind
  , WindowCapabilities (..)
  , applyPointerLeave
  )
import NanoUI.Internal.Context (Context, retainedFrameStale, setDrawSnapScale, withFramePaint)
import NanoUI.Internal.Debug (noteDebugFrame, noteDebugSkip)
import NanoUI.Runner (DebugSamplerRef, SessionDriver (..), newDebugSampler, pacingMsFor, runSessionLoop, shouldRedrawFrame)
import NanoUI.Backend (clearMeasureCache, damageFull, newPixelContext, withClipboard, withTheme)
import NanoUI.SdlWeb.Internal.Font
import NanoUI.SdlWeb.Internal.Host
import NanoUI.SdlWeb.Internal.Input
import NanoUI.SdlWeb.Internal.Render
import NanoUI.Ttf.Internal.Font (beginTtfFrame, ttfFrameDropped)

-- | A font for the web runners. The browser has no font directories, so a
-- font other than the bundled Inter comes as the bytes of a TrueType or
-- OpenType file, fetched or embedded by the program.
data WebFont
  = WebFontInter
  -- ^ The bundled Inter subset.
  | WebFontBytes !ByteString !String
  -- ^ A font file's bytes and a label for it.
  deriving (Eq, Show)

-- | Options for the web runners. The canvas's size is the page's, so the
-- window settings give only the title ('wsTitle'), which becomes the
-- document's.
data SdlWebOptions = SdlWebOptions
  { webWindowSettings :: !WindowSettings
  , webTheme :: !(Maybe Theme)
  -- ^ Initial UI theme override (default: 'Nothing').
  , webThemeFor :: !(Maybe (Maybe Appearance -> Theme))
  -- ^ Pick the theme from the colour scheme the page prefers
  -- (@prefers-color-scheme@), and follow it when it changes (default:
  -- 'Nothing').
  , webFont :: !WebFont
  -- ^ UI font (default: 'WebFontInter').
  , webMonoFont :: !WebFont
  -- ^ Monospace font (default: 'WebFontInter', set on a grid of whole
  -- pixels).
  , webFontSize :: !Float
  -- ^ Base font size in points (default: 16).
  , webUiScale :: !Float
  -- ^ Zoom of the whole UI on top of the device pixel ratio, which already
  -- follows the browser's own zoom (default: 1).
  , webRefreshHz :: !Int
  -- ^ Frame pacing rate while animating (default: 60).
  , webExplainLayout :: !Bool
  -- ^ Start with the layout overlay on (default: 'False').
  , webShouldQuit :: !(Input -> Bool)
  -- ^ End the session when this holds for a frame's input (default: never).
  }

defaultSdlWebOptions :: SdlWebOptions
defaultSdlWebOptions =
  SdlWebOptions
    { webWindowSettings = defaultWindowSettings
    , webTheme = Nothing
    , webThemeFor = Nothing
    , webFont = WebFontInter
    , webMonoFont = WebFontInter
    , webFontSize = 16
    , webUiScale = 1
    , webRefreshHz = 60
    , webExplainLayout = False
    , webShouldQuit = const False
    }

-- | A frame's UI pass: its draw data, and whether the context needs another
-- frame.
type WebFrame = Context -> Input -> IO (DrawData, Bool)

-- | The session's state beyond the context.
data WebEnv = WebEnv
  { weHost :: !WebHost
  , weFonts :: !SdlFontCache
  , weRenderer :: !Renderer
  , weScale :: !(IORef Float)
  -- ^ Device pixels per layout unit: the pixel ratio times the UI scale.
  , weZoom :: !Float
  , weBacking :: !(IORef (Int, Int))
  -- ^ The drawing buffer's size in device pixels.
  , wePresented :: !(IORef (Int, Int, Float))
  -- ^ Buffer size and scale of the last presented frame. The buffer keeps
  -- that frame, so a frame at another size or scale has nothing to keep and
  -- paints in full.
  , weCursor :: !(IORef UiCursorKind)
  , weTextInput :: !(IORef (Maybe (InputPurpose, Rect)))
  , weDebug :: !DebugSamplerRef
  }

-- | Run a session on a host until the view quits or 'webShouldQuit' holds.
runWebSession :: WebHost -> SdlWebOptions -> WebFrame -> IO ()
runWebSession host opts frame = do
  base <- newPixelContext
  ctx0 <- maybe (pure base) (withTheme base) (webTheme opts)
  forM_ (webThemeFor opts) (followSystemTheme ctx0)
  setExplainLayout ctx0 (webExplainLayout opts)
  canvas <- queryCanvas host
  let zoom = if webUiScale opts > 0 then webUiScale opts else 1
  scaleRef <- newIORef (ciPixelRatio canvas * zoom)
  withTtf $
    bracket newGlyphAtlas destroyGlyphAtlas $ \glyphs ->
      bracket (newSdlFontCache (fontSource (webFont opts)) (fontSource (webMonoFont opts)) glyphs (webFontSize opts) scaleRef) destroySdlFontCache $ \fonts ->
        bracket newRenderer freeRenderer $ \renderer -> do
          env <-
            WebEnv host fonts renderer scaleRef zoom
              <$> newIORef (0, 0)
              <*> newIORef (0, 0, 0)
              <*> newIORef UiCursorDefault
              <*> newIORef Nothing
              <*> newDebugSampler
          ctx1 <- withWebFonts env ctx0
          setDrawSnapScale ctx1 =<< readIORef scaleRef
          setWakeLoop ctx1 (whWake host)
          installWindowHost ctx1 (webWindowSettings opts) $
            defaultWindowHost
              { hostCapabilities = noWindowCapabilities {wcTitle = True}
              , hostSetTitle = withUtf8 (whSetTitle host)
              }
          withUtf8 (whSetTitle host) (wsTitle (webWindowSettings opts))
          (ctx2, inp0) <- syncWeb env ctx1 (applyPointerLeave emptyInput)
          let draw c inp force = drawWeb env c inp force (frame c inp)
              drv =
                SessionDriver
                  { sdPollEvents = pollWeb env
                  , sdWaitEvents = \t -> whWaitEvents host t >> pollWeb env
                  , sdPacedWait = Nothing
                  , sdApplyEvent = applyWebEvent
                  , sdIsButtonEdge = isButtonEdge
                  , sdIsSessionQuit = const False
                  , sdSyncDisplay = syncWeb env
                  , sdDebug = weDebug env
                  , sdContinuous = False
                  , sdVsyncOff = False
                  , sdPacingMs = pacingMsFor (1 / fromIntegral refreshHz)
                  , sdPresentPaces = pure False
                  , sdShouldDraw = \c prevInp inp wasAnim refreshDue -> shouldRedrawFrame c prevInp inp wasAnim False refreshDue
                  , sdDraw = draw
                  , sdOnCursor = syncCursor env
                  , sdShouldQuit = webShouldQuit opts
                  , sdAlignSec = 1 / fromIntegral refreshHz
                  , sdWindows = Nothing
                  }
          -- The opening frame paints before the loop waits, so the canvas
          -- has content at once; if it left the context dirty, the loop
          -- draws the next frame without waiting.
          (do _ <- draw ctx2 inp0 True
              runSessionLoop drv ctx2 inp0)
            `finally` (closeWindowHost ctx2 `finally` cancelTasks ctx2)
  where
    refreshHz = if webRefreshHz opts > 0 then webRefreshHz opts else 60
    fontSource = \case
      WebFontInter -> embeddedFontSource
      WebFontBytes bytes label -> FontFromMemory bytes label

-- | The context with the font cache's metrics and resolver, and the page's
-- clipboard.
withWebFonts :: WebEnv -> Context -> IO Context
withWebFonts env ctx = do
  c <- withSdlFontCache (weFonts env) ctx
  pure (withClipboard c (readClipboard (weHost env)) (writeClipboard (weHost env)))

-- | Take the queued events, decoded at the zoom. The queue is read in
-- batches, and a composition whose characters run on into the next batch
-- is carried over to it whole.
pollWeb :: WebEnv -> IO [WebEvent]
pollWeb env = allocaBytes (batch * eventRecordSize) $ \buf -> go buf Nothing []
  where
    batch = 256
    go buf carried acc = do
      n <- whTakeEvents (weHost env) buf batch
      (evs, partial) <- decodeEventBatch (weZoom env) carried buf n
      let acc' = acc ++ evs
      if n < batch
        then pure (acc' ++ maybe [] (pure . finishComposition) partial)
        else go buf partial acc'

-- | Read the canvas's size, pixel ratio, focus and colour scheme into the
-- frame's input and the window state. A new pixel ratio reopens the fonts
-- at it, which hands back a new context and repaints everything.
syncWeb :: WebEnv -> Context -> Input -> IO (Context, Input)
syncWeb env ctx inp = do
  canvas <- queryCanvas (weHost env)
  let zoom = weZoom env
      scale = ciPixelRatio canvas * zoom
      pw = max 1 (round (ciWidth canvas * ciPixelRatio canvas))
      ph = max 1 (round (ciHeight canvas * ciPixelRatio canvas))
  oldScale <- readIORef (weScale env)
  ctx' <-
    if abs (scale - oldScale) > 1e-4
      then do
        writeIORef (weScale env) scale
        setDrawSnapScale ctx scale
        reloadSdlFontCache (weFonts env) =<< sdlFontCacheSource (weFonts env)
        c <- withWebFonts env ctx
        clearMeasureCache ctx
        -- Glyphs change under rects and texts that may not: repaint all.
        damageFull ctx
        markDirty ctx
        pure c
      else pure ctx
  backing <- readIORef (weBacking env)
  unless (backing == (pw, ph)) $ do
    whResizeBacking (weHost env) pw ph
    writeIORef (weBacking env) (pw, ph)
  setSystemAppearance ctx' $ case ciScheme canvas of
    1 -> Just AppearanceLight
    2 -> Just AppearanceDark
    _ -> Nothing
  reportWindowState ctx' defaultWindowState {winScale = scale, winFocused = ciFocused canvas}
  let size = Size (max 1 (ciWidth canvas / zoom)) (max 1 (ciHeight canvas / zoom))
  pure (ctx', inp {inputWindowSize = size})

-- | Run a frame's UI pass and paint its damage, as nano-ui-sdl's draw path
-- does: the glyph atlas is maintained before the pass, and a frame that
-- could not place its glyphs is dropped for one after the atlas resets.
drawWeb :: WebEnv -> Context -> Input -> Bool -> IO (DrawData, Bool) -> IO Bool
drawWeb env ctx inp force evaluateUi = do
  let fonts = weFonts env
      glyphs = sfcGlyphAtlas fonts
  freshAtlas <- beginTtfFrame fonts ctx
  scale <- readIORef (weScale env)
  (pw, ph) <- readIORef (weBacking env)
  presented <- readIORef (wePresented env)
  let paintFull = retainedFrameStale force inp presented (pw, ph, scale)
  when (inputWindowRedraw inp) $ do
    -- A restored GL context lost every texture.
    markGlyphPagesDirty glyphs
    forgetTextures (weRenderer env)
  t0 <- getMonotonicTime
  -- A full frame draws whole whatever its damage, so its pass works none
  -- out.
  (drawData, dirty) <- withFramePaint ctx paintFull paintFull evaluateUi
  t1 <- getMonotonicTime
  syncTextInput env ctx
  damage <- takeDamage ctx
  atlasReset <- ttfFrameDropped fonts ctx freshAtlas
  if
    | atlasReset -> True <$ noteDebugSkip (weDebug env)
    | damageIsEmpty damage && not paintFull -> dirty <$ noteDebugSkip (weDebug env)
    | otherwise -> do
        syncTextures (weHost env) (weRenderer env) glyphs ctx
        renderDrawData (weHost env) (weRenderer env) scale (pw, ph) (if paintFull then DamageFull else snapDamage scale damage) drawData
        writeIORef (wePresented env) (pw, ph, scale)
        t2 <- getMonotonicTime
        noteDebugFrame (weDebug env) t0 t1 t2 t2 drawData
        pure dirty

-- | Start, move or stop the page's text input to match the focused text
-- widget, so its input method and on-screen keyboard follow the caret.
syncTextInput :: WebEnv -> Context -> IO ()
syncTextInput env ctx = do
  area <- textInputArea ctx
  let zoom = weZoom env
      next = (\a -> (textInputAreaPurpose a, textInputAreaRect a)) <$> area
  current <- readIORef (weTextInput env)
  unless (next == current) $ do
    writeIORef (weTextInput env) next
    case next of
      Nothing -> when (isJust current) $ whTextInput (weHost env) (-1) 0 0 0 0
      Just (purpose, Rect x y w h) ->
        let code = case purpose of InputNormal -> 0; InputSecure -> 1; InputNumeric -> 2
            css v = realToFrac (v * zoom)
         in whTextInput (weHost env) code (css x) (css y) (css (max 1 w)) (css (max 1 h))

-- | Set the canvas's cursor when the wanted kind changes.
syncCursor :: WebEnv -> Context -> Input -> IO ()
syncCursor env ctx inp =
  uiCursorKind ctx inp >>= syncCursorKind (weCursor env) shown (setCss . cssCursor)
  where
    setCss = withUtf8 (whSetCursor (weHost env))
    -- Hiding is a cursor of its own in CSS; showing again sets the kind next.
    shown visible = unless visible (setCss (cssCursor UiCursorHidden))

-- | The clipboard text the page last saw: its length first, then the bytes.
readClipboard :: WebHost -> IO (Maybe Text)
readClipboard host = do
  len <- whClipboardRead host nullPtr 0
  if len < 0
    then pure Nothing
    else allocaBytes (max 1 len) $ \buf -> do
      n <- whClipboardRead host buf len
      if n < 0 then pure Nothing else Just . TE.decodeUtf8Lenient <$> BS.packCStringLen (castPtr buf, min n len)

writeClipboard :: WebHost -> Text -> IO Bool
writeClipboard host txt = True <$ withUtf8 (whClipboardWrite host) txt

-- | Pass text to the page as UTF-8 bytes and their length.
withUtf8 :: (Ptr Word8 -> Int -> IO ()) -> Text -> IO ()
withUtf8 f txt = unsafeUseAsCStringLen (TE.encodeUtf8 txt) $ \(p, n) -> f (castPtr p) n
