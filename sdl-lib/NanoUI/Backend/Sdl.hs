{-# LANGUAGE DataKinds #-}

-- | SDL3 backend: consumes geometry ('DrawData') from the core frame loop and
-- maps SDL events into 'Input'. Idle frames skip 'runFrame' until a command,
-- hover target change, scroll drag, focused text field, 'markDirty', or an active animation demands a redraw.
-- Cross-thread 'markDirty' pushes a registered SDL user event to wake 'SDL_WaitEvent'.
-- Hover and animation frames scissor into the retain texture; partial damage blits only the dirty rect to the window.
module NanoUI.Backend.Sdl
  ( SdlEnv (..)
  , newSdlContext
  , sdlTheme
  , runSdlApp
  , runSdlAppEff
  , runSdlAppWithQuit
  , runSdlAppWithQuitEff
  , runSdlAppWith
  , runSdlAppWithEff
  , runSdlAppReduce
  , runSdlAppReduceEff
  , runSdlAppWithQuitReduce
  , runSdlAppWithQuitReduceEff
  , registerRgbaImage
  , sdlDrawFrame
  , sdlDrawFrameEff
  , withSdlBench
  , acquireSdlBench
  , releaseSdlBench
  , syncDisplay
  , SdlDebugSnapshot (..)
  , emptySdlDebug
  , readSdlDebugEnv
  , askSdlEnv
  , askSdlDebug
  ) where

import Control.Exception (bracket, finally)
import Control.Monad (unless, void, when)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Typeable (Typeable)
import GHC.Clock (getMonotonicTime)
import NanoUI
  ( Context
  , ImageId
  , Input (..)
  , Layer (..)
  , NanoUI
  , Theme
  , Ui
  , Eff
  , IOE
  , askHost
  , runFrameEff
  , runFrameReduceEff
  , type (:>)
  , uiIO
  , anyAnimating
  , collectRasterSpans
  , ctxTheme
  , Damage (..)
  , DrawData
  , damageIsEmpty
  , defaultTheme
  , emptyInput
  , enableMeasureCache
  , isDirty
  , monospaceMetrics
  , needsRedraw
  , newContext
  , textFieldActive
  , debugPanelOpen
  , overlayConsumesQuit
  , registerImage
  , Rect (..)
  , rectIntersect
  , runEff
  , Size (..)
  , takeDamage
  , textInputEditActive
  , themeWindow
  , withExternalText
  , withFontMetrics
  , withTheme
  , V2 (..)
  )
import NanoUI.Sdl.Debug
  ( SdlDebugSnapshot (..)
  , emptySdlDebug
  , noteLoop
  , notePresent
  , noteSkip
  , readSdlDebug
  , takeDebugLive
  )
import NanoUI.Sdl.Cursor (syncPointerCursor)
import NanoUI.Sdl.Display
  ( installResizeWatch
  , queryMouseWindowPos
  , queryRendererName
  , queryWindowLogicalSize
  , retainBegin
  , retainBlit
  , retainBlitRect
  , retainCreate
  , retainDestroy
  , windowToLogicalCoords
  )
import NanoUI.Sdl.Event (SdlEvent (..))
import NanoUI.Sdl.Batch (withRenderBatch)
import NanoUI.Sdl.Font (renderTextSpans)
import NanoUI.Sdl.Input
  ( applyEvent
  , clearEphemeral
  , isHardQuit
  , isHardQuitInput
  , pollEvents
  , splitFrame
  , waitEvent
  , waitEventTimeout
  )
import Data.ByteString (ByteString)
import Data.Maybe (isJust)
import Foreign.Ptr (Ptr, nullPtr)
import SDL3.Sys.Bindgen.Render (SDL_Renderer)
import qualified NanoUI.Sdl.Image as SdlImage
import NanoUI.Sdl.Render (renderDrawDataPass, snapDamage, clipPixelRect)
import NanoUI.Sdl.Window (SdlEnv (..), acquireSdlBench, defaultWindowSize, releaseSdlBench, syncDisplay, withSdl, withSdlBench)
import SDL3.Sys.Bindgen.Blendmode (sDL_BLENDMODE_BLEND)
import SDL3.Sys.Render (renderPresentSafe, setRenderDrawBlendModeSafe)

sdlTheme :: Theme
sdlTheme = defaultTheme

newSdlContext :: IO Context
newSdlContext = do
  ctx0 <- newContext
  ctx <- enableMeasureCache ctx0
  pure
    ( withExternalText
        (withTheme (withFontMetrics ctx (monospaceMetrics 16)) defaultTheme)
        True
    )

animateTimeout :: Int
animateTimeout = 16

runSdlApp :: Context -> NanoUI () -> IO ()
runSdlApp = runSdlAppEff runEff

runSdlAppEff ::
  IOE :> es =>
  (forall x. Eff es x -> IO x) ->
  Context ->
  Eff (Ui : es) () ->
  IO ()
runSdlAppEff unlift ctx ui = runSdlAppWithQuitEff unlift ctx (const False) ui

runSdlAppWithQuit :: Context -> (Input -> Bool) -> NanoUI () -> IO ()
runSdlAppWithQuit = runSdlAppWithQuitEff runEff

runSdlAppWithQuitEff ::
  IOE :> es =>
  (forall x. Eff es x -> IO x) ->
  Context ->
  (Input -> Bool) ->
  Eff (Ui : es) () ->
  IO ()
runSdlAppWithQuitEff unlift ctx shouldQuit ui =
  runSdlAppWithEff unlift ctx (const (pure ())) shouldQuit ui

runSdlAppReduce ::
  (Typeable msg, Eq model) =>
  (msg -> model -> model) ->
  Context ->
  model ->
  (model -> NanoUI ()) ->
  IO ()
runSdlAppReduce = runSdlAppReduceEff runEff

runSdlAppReduceEff ::
  (IOE :> es, Typeable msg, Eq model) =>
  (forall x. Eff es x -> IO x) ->
  (msg -> model -> model) ->
  Context ->
  model ->
  (model -> Eff (Ui : es) ()) ->
  IO ()
runSdlAppReduceEff unlift update ctx model view =
  runSdlAppWithQuitReduceEff unlift update ctx model (const False) view

runSdlAppWithQuitReduce ::
  (Typeable msg, Eq model) =>
  (msg -> model -> model) ->
  Context ->
  model ->
  (Input -> Bool) ->
  (model -> NanoUI ()) ->
  IO ()
runSdlAppWithQuitReduce = runSdlAppWithQuitReduceEff runEff

runSdlAppWithQuitReduceEff ::
  (IOE :> es, Typeable msg, Eq model) =>
  (forall x. Eff es x -> IO x) ->
  (msg -> model -> model) ->
  Context ->
  model ->
  (Input -> Bool) ->
  (model -> Eff (Ui : es) ()) ->
  IO ()
runSdlAppWithQuitReduceEff unlift update ctx model0 shouldQuit view = do
  modelRef <- newIORef model0
  runSdlSession ctx (const (pure ())) shouldQuit $ \c env i force ->
    drawReduceEff unlift update modelRef view c env i force

registerRgbaImage :: Context -> ImageId -> Int -> Int -> ByteString -> IO Bool
registerRgbaImage = registerImage

runSdlAppWith :: Context -> (SdlEnv -> IO ()) -> (Input -> Bool) -> NanoUI () -> IO ()
runSdlAppWith = runSdlAppWithEff runEff

runSdlAppWithEff ::
  IOE :> es =>
  (forall x. Eff es x -> IO x) ->
  Context ->
  (SdlEnv -> IO ()) ->
  (Input -> Bool) ->
  Eff (Ui : es) () ->
  IO ()
runSdlAppWithEff unlift ctx setup shouldQuit ui =
  runSdlSession ctx setup shouldQuit $ \c env i force ->
    drawEff unlift c ui env i force

runSdlSession ::
  Context ->
  (SdlEnv -> IO ()) ->
  (Input -> Bool) ->
  (Context -> SdlEnv -> Input -> Bool -> IO (Bool, Input)) ->
  IO ()
runSdlSession ctx setup shouldQuit drawFn =
  withSdl ctx "nano-ui" defaultWindowSize $ \ctx0 env -> do
    setup env
    void $ setRenderDrawBlendModeSafe (sdlRenderer env) (fromIntegral sDL_BLENDMODE_BLEND)
    now <- getMonotonicTime
    (ctx1, inp0) <- syncDisplay ctx0 env emptyInput
    prev <- newIORef inp0
    pendingRedraw <- newIORef False
    wasAnimating <- newIORef False
    (_, synced0) <- drawFn ctx1 env inp0 True
    writeIORef pendingRedraw False
    writeIORef prev synced0
    ctxRef <- newIORef ctx1
    drawing <- newIORef False
    let onResize = do
          void $
            tryWithDrawingLock drawing $ do
              liveCtx <- readIORef ctxRef
              inp <- readIORef prev
              (ctx', inpSynced) <- syncDisplay liveCtx env (clearEphemeral inp)
              (_, s) <- drawFn ctx' env inpSynced True
              writeIORef ctxRef ctx'
              writeIORef prev s
    bracket (installResizeWatch onResize) id $ \_ ->
      loop ctxRef drawFn env prev pendingRedraw wasAnimating drawing shouldQuit synced0 [] now

loop ::
  IORef Context ->
  (Context -> SdlEnv -> Input -> Bool -> IO (Bool, Input)) ->
  SdlEnv ->
  IORef Input ->
  IORef Bool ->
  IORef Bool ->
  IORef Bool ->
  (Input -> Bool) ->
  Input ->
  [SdlEvent] ->
  Double ->
  IO ()
loop ctxRef drawFn env prev pendingRedraw wasAnimating drawing shouldQuit inp queued lastT = do
  ctx <- readIORef ctxRef
  debugOpen <- debugPanelOpen ctx
  wantDebug <- takeDebugLive (sdlDebug env) debugOpen
  pending <-
    if null queued
      then do
        polled <- pollEvents
        if not (null polled)
          then pure polled
          else do
            animating <- anyAnimating ctx
            editing <- textFieldActive ctx
            if animating || wantDebug || editing || debugOpen
              then waitEventTimeout animateTimeout
              else waitEvent
      else pure queued
  let (group, rest) = splitFrame pending
  editActive <- textInputEditActive ctx
  if any (== EvQuit) group || (any isHardQuit group && not editActive)
    then pure ()
    else do
      now <- getMonotonicTime
      let dt = realToFrac (now - lastT)
      noteLoop (sdlDebug env) dt
      let inp' =
            foldl'
              applyEvent
              (clearEphemeral inp {inputDeltaTime = dt})
              group
      (ctx', inpSynced) <- syncDisplay ctx env inp'
      writeIORef ctxRef ctx'
      editActive' <- textInputEditActive ctx'
      if isHardQuitInput inpSynced && not editActive'
        then pure ()
        else do
          prevInp <- readIORef prev
          pendingDirty <- readIORef pendingRedraw
          wasAnim <- readIORef wasAnimating
          need <- needsRedraw ctx' prevInp inpSynced
          dirtyNow <- isDirty ctx'
          anim <- anyAnimating ctx'
          editing <- textFieldActive ctx'
          let forceFinal = wasAnim && not anim
              shouldDraw =
                need
                  || anim
                  || forceFinal
                  || pendingDirty
                  || dirtyNow
                  || debugOpen
                  || wantDebug
                  || editing
          writeIORef wasAnimating anim
          synced <-
            if shouldDraw
              then do
                ms <-
                  tryWithDrawingLock drawing $ do
                    (_, s) <- drawFn ctx' env inpSynced (debugOpen || wantDebug)
                    writeIORef pendingRedraw False
                    writeIORef prev s
                    pure s
                maybe (pure inpSynced) pure ms
              else do
                noteSkip (sdlDebug env)
                writeIORef prev inpSynced
                pure inpSynced
          overlayQuit <- overlayConsumesQuit ctx' inpSynced
          if shouldQuit inpSynced && not overlayQuit
            then pure ()
            else
              loop
                ctxRef
                drawFn
                env
                prev
                pendingRedraw
                wasAnimating
                drawing
                shouldQuit
                synced
                (if null rest then [] else rest)
                now

tryWithDrawingLock :: IORef Bool -> IO a -> IO (Maybe a)
tryWithDrawingLock ref act = do
  ok <- atomicModifyIORef' ref $ \busy -> if busy then (True, False) else (True, True)
  if ok
    then Just <$> (act `finally` writeIORef ref False)
    else pure Nothing

sdlDrawFrame :: Context -> NanoUI () -> SdlEnv -> Input -> Bool -> IO (Bool, Input)
sdlDrawFrame = sdlDrawFrameEff runEff

sdlDrawFrameEff ::
  IOE :> es =>
  (forall x. Eff es x -> IO x) ->
  Context ->
  Eff (Ui : es) () ->
  SdlEnv ->
  Input ->
  Bool ->
  IO (Bool, Input)
sdlDrawFrameEff = drawEff

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
  t0 <- getMonotonicTime
  (_, _, drawData, dirtyAfterUi) <- runFrameEff unlift ctx inp ui
  finishDraw ctx env inp forceFull t0 drawData dirtyAfterUi

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
  t0 <- getMonotonicTime
  m <- readIORef modelRef
  (_, m', _, drawData, dirtyAfterUi) <- runFrameReduceEff unlift update ctx inp m view
  writeIORef modelRef m'
  finishDraw ctx env inp forceFull t0 drawData dirtyAfterUi

finishDraw :: Context -> SdlEnv -> Input -> Bool -> Double -> DrawData -> Bool -> IO (Bool, Input)
finishDraw ctx env inp forceFull t0 drawData dirtyAfterUi = do
  scale <- readIORef (sdlScaleRef env)
  t1 <- getMonotonicTime
  syncPointerCursor (sdlCursors env) ctx inp
  dmg0 <- takeDamage ctx
  let damage = if forceFull then DamageFull else snapDamage scale dmg0
  if damageIsEmpty damage
    then do
      notePresent (sdlDebug env) ((t1 - t0) * 1000) drawData
      pure (dirtyAfterUi, inp)
    else do
      let Size lw lh = inputWindowSize inp
          pw = max 1 (round (lw * scale))
          ph = max 1 (round (lh * scale))
      tex <- ensureRetain env pw ph
      okBegin <- retainBegin (sdlRenderer env) tex
      unless okBegin $ fail "SDL_SetRenderTarget(retain) failed"
      (baseSpans, overlaySpans) <- collectRasterSpans ctx inp
      font <- readIORef (sdlFontRef env)
      monoFont <- readIORef (sdlMonoFontRef env)
      let clear = themeWindow (ctxTheme ctx)
          spansIn = filterSpans damage
      withRenderBatch (sdlRenderer env) $ \batch -> do
        renderDrawDataPass batch (sdlRenderer env) scale (Just clear) drawData [LayerBackground] (sdlImages env) damage
        renderTextSpans batch (sdlRenderer env) scale font monoFont (sdlTextCache env) (spansIn baseSpans)
        renderDrawDataPass batch (sdlRenderer env) scale Nothing drawData [LayerContent] (sdlImages env) damage
        renderDrawDataPass batch (sdlRenderer env) scale Nothing drawData [LayerOverlay] (sdlImages env) damage
        renderTextSpans batch (sdlRenderer env) scale font monoFont (sdlTextCache env) (spansIn overlaySpans)
        renderDrawDataPass batch (sdlRenderer env) scale Nothing drawData [LayerChrome] (sdlImages env) damage
      okBlit <- blitRetain (sdlRenderer env) scale tex damage
      unless okBlit $ fail "SDL_RenderTexture(retain) failed"
      void $ renderPresentSafe (sdlRenderer env)
      notePresent (sdlDebug env) ((t1 - t0) * 1000) drawData
      pure (dirtyAfterUi, inp)

ensureRetain :: SdlEnv -> Int -> Int -> IO (Ptr ())
ensureRetain env w h = do
  (tex, ow, oh) <- readIORef (sdlRetain env)
  if tex /= nullPtr && ow == w && oh == h
    then pure tex
    else do
      retainDestroy tex
      tex' <- retainCreate (sdlRenderer env) w h
      when (tex' == nullPtr) $ fail "SDL_CreateTexture(retain) failed"
      writeIORef (sdlRetain env) (tex', w, h)
      pure tex'

filterSpans :: Damage -> [(Rect, a, b, c, Rect)] -> [(Rect, a, b, c, Rect)]
filterSpans DamageFull spans = spans
filterSpans (DamageClip clip) spans =
  filter (\(box, _, _, _, _) -> isJust (rectIntersect clip box)) spans

blitRetain :: Ptr SDL_Renderer -> Float -> Ptr () -> Damage -> IO Bool
blitRetain ren scale tex damage =
  case damage of
    DamageFull -> retainBlit ren tex
    DamageClip r ->
      let (px, py, pw, ph) = clipPixelRect scale r
       in retainBlitRect ren tex px py pw ph px py

askSdlEnv :: Ui :> es => Eff es (Maybe SdlEnv)
askSdlEnv = askHost

askSdlDebug :: Ui :> es => Eff es SdlDebugSnapshot
askSdlDebug = do
  menv <- askSdlEnv
  case menv of
    Nothing -> pure emptySdlDebug
    Just env -> uiIO (readSdlDebugEnv env)

readSdlDebugEnv :: SdlEnv -> IO SdlDebugSnapshot
readSdlDebugEnv env = do
  scale <- readIORef (sdlScaleRef env)
  name <- queryRendererName (sdlRenderer env)
  size <- queryWindowLogicalSize (sdlWindow env) scale
  mouse <- queryMouseWindowPos
  let pos = maybe (V2 0 0) (windowToLogicalCoords scale) mouse
  readSdlDebug (sdlDebug env) size pos (sdlFontPath env) scale name
