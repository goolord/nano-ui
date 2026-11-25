{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}
-- The 'BackendSession' instance for the browser session is an orphan by
-- construction: the core owns the backend kind, and this package owns its
-- session.
{-# OPTIONS_GHC -Wno-orphans #-}

-- | The browser session: options, the event loop over a 'WebHost', display
-- sync, and frame presentation. The loop is "NanoUI.Runner"'s, as on the
-- desktop backends: its wait for events is a wait on a JavaScript promise,
-- which lets the browser run until an event or a wake arrives.
module NanoUI.SdlWeb.Internal.Session
  ( SdlWebOptions (..)
  , defaultSdlWebOptions
  , WebFont (..)
  , WebFontService (..)
  , runWebSession
  , WebFrame
    -- * From the view
  , setSdlWebUiFont
  , webFontFamilies
  , askSdlWebDebug
  , listWebFontFamilies
  ) where

import Control.Exception (bracket, finally)
import Control.Monad (forM_, unless, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import GHC.TypeLits (symbolSing)
import System.IO (hPutStrLn, stderr)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import GHC.Clock (getMonotonicTime)
import NanoUI
  ( Appearance (..)
  , NanoUI
  , Rect (..)
  , ShaderCapabilities (..)
  , Size (..)
  , Theme
  , WindowSettings (..)
  , defaultWindowSettings
  )
import NanoUI.Backend
  ( Backend (..)
  , BackendSession
  , Damage (..)
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
  , followSystemThemeIO
  , installWindowHost
  , markDirty
  , noWindowCapabilities
  , reportWindowState
  , setExplainLayout
  , setShaderCapabilities
  , setSystemAppearance
  , setWakeLoop
  , syncCursorKind
  , takeDamage
  , textInputArea
  , uiCursorKind
  , WindowCapabilities (..)
  , SBackend (..)
  , applyPointerLeave
  , askBackendSession
  , setBackendSession
  )
import NanoUI.Internal.Context (Context, markDirtyCovered, retainedFrameStale, setDrawSnapScale, withFramePaint)
import NanoUI.Internal.Debug (CoreDebugSnapshot (..), noteDebugFrame, noteDebugSkip, refreshDebugSnapshot)
import NanoUI.Monad (askContext, liftIO)
import NanoUI.Runner (DebugSamplerRef, SessionDriver (..), newDebugSampler, pacingMsFor, runSessionLoop, shouldRedrawFrame)
import NanoUI.Backend (clearMeasureCache, damageFull, newPixelContext, withClipboard, withTheme)
import NanoUI.SdlWeb.Internal.Debug (SdlWebDebugSnapshot (..))
import NanoUI.SdlWeb.Internal.Font
import NanoUI.SdlWeb.Internal.Fonts (FontOrigin (..), findFontFamilies, knownFontFamilies, loadFontBytes, rendererName, searchCandidates)
import NanoUI.SdlWeb.Internal.Host
import NanoUI.SdlWeb.Internal.Input
import NanoUI.SdlWeb.Internal.Render
import NanoUI.Ttf.Internal.Font (beginTtfFrame, ttfFrameDropped)

-- | A font for the web runners. The browser has no font directories: a
-- font other than the bundled Inter is an installed family the browser hands
-- over (see 'listWebFontFamilies'), a file fetched by URL, or bytes the
-- program embeds. A font that cannot be loaded falls back to Inter, as a
-- missing font does on the desktop.
data WebFont
  = WebFontInter
  -- ^ The bundled Inter subset.
  | WebFontBytes !ByteString !String
  -- ^ A font file's bytes and a label for it.
  | WebFontInstalled !String
  -- ^ An installed family by name, through the Local Font Access API
  -- (Chromium-based browsers, after the user allows it). Its regular face,
  -- else the first the browser lists.
  | WebFontUrl !String
  -- ^ A TrueType or OpenType file at a URL. Another origin must allow the
  -- page to read it (CORS).
  | WebFontSearch ![String]
  -- ^ The first of these families that can be had, as
  -- @NanoUI.Backend.Sdl.FontSearch@ searches the desktop's: installed (once
  -- the user has let the browser list them), else from the font service
  -- ('webFontService'). Names compare ignoring case, spaces, @-@ and @_@;
  -- @sans-serif@, @serif@ and @monospace@ stand for common families. With
  -- none found, the bundled Inter.
  deriving (Eq, Show)

-- | Where families come from when they are not installed, or not allowed.
data WebFontService
  = Fontsource
  -- ^ Fontsource (fontsource.org): about 2,100 open-licence families,
  -- Google Fonts' among them, as TrueType from its CDN (cdn.jsdelivr.net).
  -- The page fetches the catalogue when 'webFontFamilies' is first asked
  -- for, and each family's regular face when it is used.
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
  , webFontService :: !(Maybe WebFontService)
  -- ^ A service to list families from and load them by name, in every
  -- browser (default: 'Nothing': the page contacts no third party). Firefox
  -- and Safari give a page no installed fonts, so there a service is the
  -- only source of families beyond the page's own.
  , webFallbackFonts :: ![WebFont]
  -- ^ Fonts for the characters the UI and monospace fonts lack, tried in
  -- order, as the desktop finds them among the system's fonts. Loaded when
  -- the session starts; one that will not load is left out, with a console
  -- warning. The default is the wider Inter that @scripts/build-web.sh@
  -- serves beside the page, @fonts/inter-extended.ttf@: Latin beyond ASCII,
  -- Greek, Cyrillic, punctuation and common symbols, which the bundled
  -- ASCII Inter lacks.
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
    , webFontService = Nothing
    , webFallbackFonts = [WebFontUrl "fonts/inter-extended.ttf"]
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
  , weDebugPublished :: !(IORef (Maybe SdlWebDebugSnapshot))
  , weFontRequest :: !(IORef WebFont)
  -- ^ The UI font the view asked for ('setSdlWebUiFont').
  , weFontApplied :: !(IORef WebFont)
  -- ^ The UI font the font cache has open.
  , weRendererName :: !Text
  , weRefreshHz :: !Int
  , weFontService :: !(Maybe WebFontService)
  , weFamilies :: !(IORef (Maybe (Int, [String])))
  -- ^ The families 'webFontFamilies' last read, at the page's version of
  -- them; 'Nothing' until a view first asks.
  }

-- | A browser session keeps its environment on the context, so the view's
-- calls ('setSdlWebUiFont', 'askSdlWebDebug') find it without a handle.
type instance BackendSession ('Custom "sdl-web") = WebEnv

webSession :: SBackend ('Custom "sdl-web")
webSession = SCustom symbolSing

-- | Run a session on a host until the view quits or 'webShouldQuit' holds.
runWebSession :: WebHost -> SdlWebOptions -> WebFrame -> IO ()
runWebSession host opts frame = do
  base <- newPixelContext
  ctx0 <- maybe (pure base) (withTheme base) (webTheme opts)
  forM_ (webThemeFor opts) (followSystemThemeIO ctx0)
  setExplainLayout ctx0 (webExplainLayout opts)
  canvas <- queryCanvas host
  let zoom = if webUiScale opts > 0 then webUiScale opts else 1
  scaleRef <- newIORef (ciPixelRatio canvas * zoom)
  -- A search by name needs the service named before it loads anything.
  when (any isSearch [webFont opts, webMonoFont opts]) $
    whStartFontDiscovery host (serviceCode (webFontService opts))
  sans <- resolveWebFont host (webFont opts)
  mono <- resolveWebFont host (webMonoFont opts)
  fallbacks <- newWebFallbacks . concat =<< mapM (loadFallback host) (webFallbackFonts opts)
  let hooks = webFontHooks fallbacks
  rendererLabel <- rendererName host
  withTtf hooks $
    bracket newGlyphAtlas destroyGlyphAtlas $ \glyphs ->
      bracket (newSdlFontCache hooks sans mono glyphs (webFontSize opts) scaleRef) destroySdlFontCache $ \fonts ->
        bracket newRenderer freeRenderer $ \renderer -> do
          env <-
            WebEnv host fonts renderer scaleRef zoom
              <$> newIORef (0, 0)
              <*> newIORef (0, 0, 0)
              <*> newIORef UiCursorDefault
              <*> newIORef Nothing
              <*> newDebugSampler
              <*> newIORef Nothing
              <*> newIORef (webFont opts)
              <*> newIORef (webFont opts)
              <*> pure rendererLabel
              <*> pure refreshHz
              <*> pure (webFontService opts)
              <*> newIORef Nothing
          ctx1 <- withWebFonts env ctx0
          setBackendSession ctx1 webSession env
          setShaderCapabilities ctx1 (ShaderCapabilities True True)
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
    isSearch = \case WebFontSearch _ -> True; _ -> False

-- | The page's code for a font service.
serviceCode :: Maybe WebFontService -> Int
serviceCode = \case Nothing -> 0; Just Fontsource -> 1

-- | The font source for a 'WebFont', loading an installed family or a URL
-- through the page. One that cannot be loaded is the bundled Inter, with a
-- warning on the console.
resolveWebFont :: WebHost -> WebFont -> IO FontSource
resolveWebFont host = \case
  WebFontInter -> pure embeddedFontSource
  WebFontBytes bytes label -> pure (FontFromMemory bytes label)
  WebFontInstalled family -> load InstalledFamily family
  WebFontUrl url -> load FontUrl url
  WebFontSearch names -> search (concatMap searchCandidates names)
  where
    load origin name =
      loadFontBytes host origin name >>= \case
        Just bytes -> pure (FontFromMemory bytes name)
        Nothing -> do
          hPutStrLn stderr ("nano-ui-sdl-web: could not load the font " <> show name <> "; using Inter")
          pure embeddedFontSource
    search [] = pure embeddedFontSource
    search (name : rest) =
      loadFontBytes host FamilyByName name >>= \case
        Just bytes -> pure (FontFromMemory bytes name)
        Nothing -> search rest

-- | A fallback font's bytes: none for the bundled Inter, which is the UI
-- font's own fallback already, or for a font that will not load.
loadFallback :: WebHost -> WebFont -> IO [ByteString]
loadFallback host = \case
  WebFontInter -> pure []
  WebFontBytes bytes _ -> pure [bytes]
  WebFontInstalled family -> load InstalledFamily family
  WebFontUrl url -> load FontUrl url
  WebFontSearch names -> firstOf (concatMap searchCandidates names)
  where
    firstOf [] = pure []
    firstOf (name : rest) =
      loadFontBytes host FamilyByName name >>= maybe (firstOf rest) (pure . pure)
    load origin name =
      loadFontBytes host origin name >>= \case
        Just bytes -> pure [bytes]
        Nothing -> [] <$ hPutStrLn stderr ("nano-ui-sdl-web: could not load the fallback font " <> show name)

-- | Ask for a UI font. The session loads and applies it before the next
-- frame, which this asks for: the glyph atlas, metrics and text all change
-- together, as 'NanoUI.Backend.Sdl.setSdlUiFont' does on the desktop.
-- Loading an installed family or a URL waits for the page, so that frame
-- comes when the font is in. Asking for the font already asked for does
-- nothing.
setSdlWebUiFont :: WebFont -> NanoUI ()
setSdlWebUiFont font = do
  ctx <- askContext
  askBackendSession webSession >>= mapM_ (\env -> liftIO $ do
    cur <- readIORef (weFontRequest env)
    unless (cur == font) $ writeIORef (weFontRequest env) font >> markDirtyCovered ctx)

-- | The session's frame statistics, canvas, font and renderer, refreshed at
-- most four times a second; 'Nothing' outside a browser session. Asking
-- keeps the sampler running and asks for frames at its rate, so ask only
-- while the numbers are on screen.
askSdlWebDebug :: NanoUI (Maybe SdlWebDebugSnapshot)
askSdlWebDebug =
  askBackendSession webSession >>= traverse (\env -> liftIO $
    refreshDebugSnapshot (weDebug env) (weDebugPublished env) $ \core -> do
      canvas <- queryCanvas (weHost env)
      scale <- readIORef (weScale env)
      source <- sdlFontCacheSource (weFonts env)
      let zoom = weZoom env
      pure
        SdlWebDebugSnapshot
          { dbgCore = core {dbgWinW = ciWidth canvas / zoom, dbgWinH = ciHeight canvas / zoom}
          , dbgScale = scale
          , dbgPixelRatio = ciPixelRatio canvas
          , dbgFont = fontSourceLabel source
          , dbgRenderer = weRendererName env
          , dbgRefreshHz = weRefreshHz env
          })

-- | The font families there are to choose from, for 'WebFontSearch', as
-- @NanoUI.Backend.Sdl.listFontFamilies@ lists the desktop's: the installed
-- ones (Chromium-based browsers, once the user allows it), then the font
-- service's ('webFontService'). The first call starts finding them and
-- answers what is known, possibly nothing; the view runs again as more
-- arrive. While the browser has not asked the user about installed fonts,
-- it asks at their next click or key press on the page, the only moment a
-- browser shows that question; the answer is kept for the site, so later
-- visits list them at once. Firefox and Safari have no installed fonts to
-- give, so there the list is the service's.
webFontFamilies :: NanoUI [String]
webFontFamilies =
  askBackendSession webSession >>= \case
    Nothing -> pure []
    Just env -> liftIO $ do
      known <- readIORef (weFamilies env)
      when (known == Nothing) $ whStartFontDiscovery (weHost env) (serviceCode (weFontService env))
      version <- whFontsVersion (weHost env)
      case known of
        Just (seen, families) | seen == version -> pure families
        _ -> do
          families <- knownFontFamilies (weHost env)
          families <$ writeIORef (weFamilies env) (Just (version, families))

-- | The installed font families alone, asking the user now; or why they
-- cannot be listed: the browser has no Local Font Access API (only
-- Chromium-based browsers have it), the user declined, or this is not a
-- browser. 'webFontFamilies' is the usual way, which asks at the user's next
-- press by itself. A browser shows the question only right after a click or
-- key press, so call this from a task the press starts ('NanoUI.useTask'). It
-- waits for the user's answer.
listWebFontFamilies :: IO (Either String [String])
listWebFontFamilies
  | browserAvailable = findFontFamilies =<< browserHost
  | otherwise = pure (Left "installed fonts are listed in a browser only")

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
-- frame's input and the window state. A new pixel ratio, or a UI font the
-- view asked for, reopens the fonts, which hands back a new context and
-- repaints everything.
syncWeb :: WebEnv -> Context -> Input -> IO (Context, Input)
syncWeb env ctx inp = do
  canvas <- queryCanvas (weHost env)
  let zoom = weZoom env
      scale = ciPixelRatio canvas * zoom
      pw = max 1 (round (ciWidth canvas * ciPixelRatio canvas))
      ph = max 1 (round (ciHeight canvas * ciPixelRatio canvas))
  oldScale <- readIORef (weScale env)
  -- A view that lists the families runs again when more arrive.
  readIORef (weFamilies env) >>= mapM_ (\(seen, _) -> do
    version <- whFontsVersion (weHost env)
    unless (version == seen) (markDirty ctx))
  requested <- readIORef (weFontRequest env)
  applied <- readIORef (weFontApplied env)
  let scaleChanged = abs (scale - oldScale) > 1e-4
      fontChanged = requested /= applied
  ctx' <-
    if scaleChanged || fontChanged
      then do
        writeIORef (weScale env) scale
        setDrawSnapScale ctx scale
        source <-
          if fontChanged
            then resolveWebFont (weHost env) requested
            else sdlFontCacheSource (weFonts env)
        writeIORef (weFontApplied env) requested
        reloadSdlFontCache (weFonts env) source
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
