{-# LANGUAGE DataKinds #-}

-- | SDL3 draw path: frame execution and retain-texture present.
module NanoUI.Sdl.Runner
  ( newSdlContext
  , runSdlSession
  , sdlDrawFrame
  , drawEff
  , drawReduceEff
  , askSdlEnv
  , askSdlDebug
  , setSdlUiFont
  , readSdlDebugEnv
  ) where

import Control.Exception (finally)
import Control.Monad (unless, void, when)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Primitive.SmallArray (SmallArray, smallArrayFromListN)
import Data.Typeable (Typeable)
import GHC.Clock (getMonotonicTime)
import NanoUI
  ( Input (..)
  , NanoUI
  , Size (..)
  , V2 (..)
  , themeWindow
  )
import Effectful (Eff, IOE, type (:>))
import NanoUI.Testing
  ( Context
  , Damage (..)
  , DrawData
  , Layer (..)
  , Ui
  , ctxPaintFull
  , ctxTheme
  , damageFull
  , damageIsEmpty
  , markDirty
  , runEff
  , runFrameEff
  , runFrameReduceEff
  , takeDamage
  , askHost
  , uiIO
  )
import NanoUI.Testing (newPixelContext)
import NanoUI.Sdl.Session (runSdlSession)
import NanoUI.Sdl.Debug
  ( SdlDebugSnapshot (..)
  , emptySdlDebug
  , notePresent
  , noteSkip
  , readSdlDebug
  )
import NanoUI.Sdl.Cursor (syncPointerCursor)
import NanoUI.Sdl.Display
  ( queryMouseWindowPos
  , queryRendererName
  , queryWindowLogicalSize
  , retainBegin
  , retainBlit
  , retainCreate
  , retainDestroy
  , windowToLogicalCoords
  )
import NanoUI.Sdl.Render (flushRenderBatch)
import NanoUI.Sdl.Font
  ( fontSourceLabel
  , glyphAtlasTexture
  , prepareGlyphAtlasForFrame
  , takeGlyphAtlasResetFlag
  )
import NanoUI.Sdl.NanoUIFont (NanoUIFont (..))
import NanoUI.Sdl.Window (SdlEnv (..))
import Foreign.Ptr (Ptr, nullPtr)
import qualified NanoUI.Sdl.Image as SdlImage
import NanoUI.Sdl.Render (renderDrawDataPass, snapDamage)
import SDL3.Sys.Render (renderPresentSafe)

newSdlContext :: IO Context
newSdlContext = newPixelContext

allLayersArr :: SmallArray Layer
allLayersArr = smallArrayFromListN 4 [LayerBackground, LayerContent, LayerOverlay, LayerChrome]

sdlDrawFrame :: Context -> NanoUI () -> SdlEnv -> Input -> Bool -> IO (Bool, Input)
sdlDrawFrame ctx ui env inp forceFull = drawEff runEff ctx ui env inp forceFull

drawEff ::
  IOE :> es =>
  (forall x. Eff es x -> IO x) ->
  Context ->
  Eff (Ui : es) () ->
  SdlEnv ->
  Input ->
  Bool ->
  IO (Bool, Input)
drawEff unlift ctx ui env inp forceFull = do
  SdlImage.syncImageAtlas (sdlRenderer env) (sdlImages env) ctx
  (tex, retainNew, presentFull) <- prepareRetain ctx env inp forceFull
  t0 <- getMonotonicTime
  (_, _, drawData, dirtyAfterUi) <- runFrameEff unlift ctx inp ui
  t1 <- getMonotonicTime
  finishDraw ctx env inp tex retainNew presentFull t0 t1 drawData dirtyAfterUi

-- | Decide whether this present repaints everything, then make sure the
-- retain texture exists. Must run before the frame so the paint pass can cull
-- to the damage clip when the present will be partial.
prepareRetain :: Context -> SdlEnv -> Input -> Bool -> IO (Ptr (), Bool, Bool)
prepareRetain ctx env inp forceFull = do
  -- Glyph-atlas maintenance before any quad is recorded: if the atlas ran
  -- out of space during the previous frame, reset it now (re-warming the
  -- base fonts) so a reset can never wipe the texture underneath
  -- already-recorded text mid-frame.
  prepareGlyphAtlasForFrame (sdlGlyphAtlas env)
  scale <- readIORef (sdlScaleRef env)
  let Size lw lh = inputWindowSize inp
      pw = max 1 (round (lw * scale))
      ph = max 1 (round (lh * scale))
  (tex, retainNew) <- ensureRetain env pw ph scale
  let presentFull = forceFull || retainNew || sdlContinuous env || inputWindowRedraw inp
  writeIORef (ctxPaintFull ctx) presentFull
  pure (tex, retainNew, presentFull)

drawReduceEff ::
  (IOE :> es, Typeable msg, Eq model) =>
  (forall x. Eff es x -> IO x) ->
  (msg -> model -> model) ->
  IORef model ->
  (model -> Eff (Ui : es) ()) ->
  Context ->
  SdlEnv ->
  Input ->
  Bool ->
  IO (Bool, Input)
drawReduceEff unlift update modelRef view ctx env inp forceFull = do
  SdlImage.syncImageAtlas (sdlRenderer env) (sdlImages env) ctx
  (tex, retainNew, presentFull) <- prepareRetain ctx env inp forceFull
  t0 <- getMonotonicTime
  m <- readIORef modelRef
  (_, m', _, drawData, dirtyAfterUi) <- runFrameReduceEff unlift update ctx inp m view
  writeIORef modelRef m'
  t1 <- getMonotonicTime
  finishDraw ctx env inp tex retainNew presentFull t0 t1 drawData dirtyAfterUi

finishDraw :: Context -> SdlEnv -> Input -> Ptr () -> Bool -> Bool -> Double -> Double -> DrawData -> Bool -> IO (Bool, Input)
finishDraw ctx env inp tex retainNew presentFull t0 t1 drawData dirtyAfterUi = do
  let uiMs = (t1 - t0) * 1000
  scale <- readIORef (sdlScaleRef env)
  syncPointerCursor (sdlCursors env) ctx inp
  dmg0 <- takeDamage ctx
  let Size lw lh = inputWindowSize inp
  -- Frame damage from writeDamage is authoritative: a live animation whose
  -- key is out of view or scroll-clipped produces empty damage, and forcing
  -- DamageFull here would turn every skip frame into a full present. A
  -- window redraw event (expose/restore) is the exception: the backbuffer
  -- is gone, so the next present must be full.
  let damage0 =
        if presentFull
          then DamageFull
          else snapDamage scale dmg0
      damage = damage0
  writeIORef (sdlLastPresented env) False
  -- A glyph-atlas reset or exhaustion during the UI pass means quads
  -- recorded before that point hold stale (or unplaceable) UVs. Drop the
  -- frame instead of presenting it: the screen keeps the previous valid
  -- frame, 'damageFull' forces a full repaint, and
  -- 'prepareGlyphAtlasForFrame' resets the atlas before the next frame
  -- records any quads, so text never flickers or vanishes for a frame.
  atlasReset <- takeGlyphAtlasResetFlag (sdlGlyphAtlas env)
  if atlasReset || damageIsEmpty damage || lw <= 0 || lh <= 0
    then do
      when atlasReset $ do
        damageFull ctx
        markDirty ctx
      noteSkip (sdlDebug env)
      pure (atlasReset || dirtyAfterUi, inp)
    else do
      okBegin <- retainBegin (sdlRenderer env) tex scale
      unless okBegin $ fail "SDL_SetRenderTarget(retain) failed"
      theme <- readIORef (ctxTheme ctx)
      glyphTex <- glyphAtlasTexture (sdlGlyphAtlas env)
      -- Persistent batch created once per session (sdlBatch): no C
      -- calloc/free pair per presented frame. Flush unconditionally so an
      -- aborted pass cannot leak pending geometry into the next frame.
      --
      -- A full repaint must start from a clean retain texture: the frame only
      -- records draw commands for what the app paints, and a bare window
      -- background (no full-window panel node) leaves the rest of the texture
      -- untouched. Without the clear, text that moved or shrank on a bare
      -- backdrop ghosts against the previous frame's pixels. Partial clips
      -- keep the undamaged region and never clear. The clear targets the
      -- retain texture, which is only blitted to the window afterwards, so it
      -- cannot flash on screen mid-frame.
      let batch = sdlBatch env
      renderDrawDataPass
        batch
        (sdlRenderer env)
        scale
        (if retainNew || damage == DamageFull then Just (themeWindow theme) else Nothing)
        drawData
        allLayersArr
        (sdlImages env)
        glyphTex
        damage
        `finally` flushRenderBatch batch
      t2 <- getMonotonicTime
      -- Damage limits updates to the retained texture, not the final copy:
      -- SDL leaves the window backbuffer undefined after each present.
      okBlit <- retainBlit (sdlRenderer env) tex
      unless okBlit $ fail "SDL_RenderTexture(retain) failed"
      void $ renderPresentSafe (sdlRenderer env)
      t3 <- getMonotonicTime
      let renderMs = (t2 - t1) * 1000
          presentMs = (t3 - t2) * 1000
          frameMs = (t3 - t0) * 1000
      notePresent (sdlDebug env) uiMs renderMs presentMs frameMs drawData
      writeIORef (sdlLastPresented env) True
      pure (dirtyAfterUi, inp)

ensureRetain :: SdlEnv -> Int -> Int -> Float -> IO (Ptr (), Bool)
ensureRetain env w h scale = do
  (tex, ow, oh, oldScale) <- readIORef (sdlRetain env)
  let scaleChanged = abs (oldScale - scale) > 0.001
  if tex /= nullPtr && ow == w && oh == h
    then do
      when scaleChanged $ writeIORef (sdlRetain env) (tex, w, h, scale)
      -- Same pixel size after a DPI change still holds the old present.
      pure (tex, scaleChanged)
    else do
      retainDestroy tex
      tex' <- retainCreate (sdlRenderer env) w h
      when (tex' == nullPtr) $ fail "SDL_CreateTexture(retain) failed"
      writeIORef (sdlRetain env) (tex', w, h, scale)
      pure (tex', True)

askSdlEnv :: Ui :> es => Eff es (Maybe SdlEnv)
askSdlEnv = askHost

askSdlDebug :: Ui :> es => Eff es SdlDebugSnapshot
askSdlDebug = do
  menv <- askSdlEnv
  case menv of
    Nothing -> pure emptySdlDebug
    Just env -> uiIO (readSdlDebugEnv env)

-- | Request a UI font family. The SDL display thread resolves and applies it
-- before the next frame (see 'NanoUI.Sdl.Window.syncDisplay'), rebuilding the
-- glyph atlas and text resolver. A no-op on non-SDL hosts.
setSdlUiFont :: Ui :> es => NanoUIFont -> Eff es ()
setSdlUiFont font = do
  menv <- askSdlEnv
  case menv of
    Nothing -> pure ()
    Just env -> uiIO $ do
      cur <- readIORef (sdlFontRequestRef env)
      when (cur /= font) $ writeIORef (sdlFontRequestRef env) font

readSdlDebugEnv :: SdlEnv -> IO SdlDebugSnapshot
readSdlDebugEnv env = do
  scale <- readIORef (sdlScaleRef env)
  fontSource <- readIORef (sdlFontSourceRef env)
  name <- queryRendererName (sdlRenderer env)
  size <- queryWindowLogicalSize (sdlWindow env) scale
  mouse <- queryMouseWindowPos
  let pos = maybe (V2 0 0) (windowToLogicalCoords scale) mouse
      refreshHz = round (1 / sdlRefreshPeriod env)
  readSdlDebug (sdlDebug env) size pos (fontSourceLabel fontSource) scale name (sdlVsync env) refreshHz
