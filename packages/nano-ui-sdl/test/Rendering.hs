{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE PackageImports #-}

module Main (main) where

import Control.Exception (IOException, SomeException, bracket, catch, displayException, throwIO, try)
import Control.Monad (forM_, replicateM_, unless, void, when)
import Data.ByteString qualified as BS
import Data.Functor ((<&>))
import Data.IORef (readIORef)
import Data.List (isInfixOf, nub)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Primitive.PrimArray (primArrayFromList)
import Data.Vector.Unboxed qualified as U
import Data.Word (Word8)
import Foreign.C.Types (CBool (..), CFloat (..), CInt (..), CUInt (..))
import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (allocaArray, peekArray, pokeArray)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (getAllocationCounter)
import Input (inputChecks, queueChecks)
import NanoUI
  ( Color, ImageId (..), Rect (..), ShaderCapabilities (..), ShaderConfig (..), V2 (..), box, canvas, colorA, colorBlack
  , colorB, colorG, colorR, colorRGBA, colorWhite, column, columnWith, defaultLightTheme, defaultShaderConfig
  , defaultTheme, drawStrokePathWith, fixedWH, lightDark, padAll, pageColor, respRect
  , shaderBox, shaderConfigured, tight, withShader
  )
import NanoUI.Internal.Shader (CpuFragCtx (..), Shader (..))
import NanoUI.Shader
  ( MeshGradientParams (..), SdfBoxParams (..), TintParams (..), composeColor, gaussianBlur, grayscale
  , groupOpacity, meshGradient4, sdfRoundedBox, tint
  )
import NanoUI.Path qualified as P
import NanoUI.Backend
  ( Appearance (..), cursorFallback, emptyEffectSpans, emptyInput, emptyShaderCalls, emptyUniformBytes
  , followSystemThemeIO, getShaderCapabilities, getSystemAppearance, getThemeIO, setSystemAppearance, setThemeIO, snapDamage
  )
import NanoUI.Internal.Context (lookupImageUv)
import NanoUI.Sdl.Internal.Cursor (SdlCursors (..), destroyCursors, initCursors, sdlSystemCursor, showCursorKind)
import NanoUI.Sdl.Internal.Error (sdlErrorText)
import NanoUI.Sdl.Internal.Image (ImageAtlas, destroyImageAtlas, newImageAtlas, syncImageAtlas)
import NanoUI.Sdl.Internal.Render (destroyRenderBatch, flushRenderBatch, newRenderBatch, renderDrawDataPass)
import NanoUI.Testing
  ( Context, Damage (..), DrawCmd (..), DrawData (..), Layer (..), UiCursorKind (..), atlasTextureId
  , glyphPageTextureId, newPixelContext, registerImage
  )
import NanoUI.Testing.Assert (withInput)
import NanoUI.Testing.Harness (warmupDraw)
import SDL3.Sys.Bindgen.Mouse qualified as M
import SDL3.Sys.Bindgen.Render (SDL_Renderer)
import SDL3.Sys.Bindgen.Runtime.PtrConst qualified as PtrConst
import SDL3.Sys.Bindgen.Video qualified as Video
import SDL3.Sys.Error (clearError)
import SDL3.Sys.Mouse (cursorVisibleSafe)
import SDL3.Sys.Render (renderPresentSafe, renderReadPixels)
import SDL3.Sys.Video (getSystemTheme)
import System.Environment (getArgs, lookupEnv, setEnv)
import System.IO (BufferMode (LineBuffering), hFlush, hPutStrLn, hSetBuffering, stderr, stdout)
import System.Mem (performGC)
import Text.Printf (printf)
import Text.Read (readMaybe)
import WindowChecks (windowChecks)
import "nano-ui-sdl" NanoUI.Backend.Sdl (SdlEnv, syncDisplay, withSdlBench)
import "nano-ui-sdl" NanoUI.Sdl.Internal.Window (sdlRenderer, sdlRendererName)

foreign import ccall unsafe "SDL_ReadSurfacePixel"
  readPixel :: Ptr () -> CInt -> CInt -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO CBool
foreign import ccall unsafe "SDL_DestroySurface" freeSurface :: Ptr () -> IO ()
foreign import capi unsafe "SDL3/SDL.h value SDL_PIXELFORMAT_RGBA32" rgba32 :: CUInt
foreign import capi unsafe "SDL3/SDL.h SDL_CreateSurfaceFrom"
  surfaceFrom :: CInt -> CInt -> CUInt -> Ptr Word8 -> CInt -> IO (Ptr ())
foreign import capi unsafe "SDL3/SDL.h SDL_FillSurfaceRect" fillSurface :: Ptr () -> Ptr () -> CUInt -> IO CBool
foreign import ccall unsafe "nano_ui_text_atlas_create" newAtlas :: Ptr SDL_Renderer -> IO (Ptr ())
foreign import ccall unsafe "nano_ui_text_atlas_destroy" freeAtlas :: Ptr () -> IO ()
foreign import ccall unsafe "nano_ui_text_atlas_reset" resetAtlas :: Ptr () -> IO ()
foreign import ccall unsafe "nano_ui_text_atlas_insert_surface"
  insertAtlas :: Ptr () -> Ptr () -> Ptr CFloat -> IO CBool

-- A padded pitch deliberately differs from both the glyph width and atlas pitch.
withGlyphSurface :: (Ptr () -> IO a) -> IO a
withGlyphSurface action = allocaBytes 64 $ \pixels ->
  bracket (surfaceFrom 3 2 rgba32 pixels 32) freeSurface $ \surface -> do
    unless (surface /= nullPtr) (fail "glyph surface creation failed")
    ok <- fillSurface surface nullPtr 0xffffffff
    unless (ok /= 0) (fail "glyph surface fill failed")
    action surface

-- | Insert a surface into the atlas: its page and pixel bounds, read back
-- from the UVs it returns, or 'Nothing' when no page has room.
insertInto :: Ptr () -> Ptr () -> IO (Maybe (CInt, [CFloat]))
insertInto atlas surface =
  allocaArray 4 $ \uv -> do
    ok <- insertAtlas atlas surface uv
    if ok /= 0
      then do
        [u0, v0, u1, v1] <- peekArray 4 uv
        let page = floor u0
            px a = a * 2048
        pure (Just (page, [px (u0 - fromIntegral page), px v0, px (u1 - u0), px (v1 - v0)]))
      else pure Nothing

atlasChecks :: SdlEnv -> (Ptr () -> DrawData -> IO ()) -> IO ()
atlasChecks env draw = withGlyphSurface $ \surface ->
  bracket (newAtlas (sdlRenderer env)) freeAtlas $ \atlas -> do
    unless (atlas /= nullPtr) (fail "atlas creation failed")
    let firstGlyph name =
          insertInto atlas surface >>= \r -> unless (r == Just (0, [5, 1, 3, 2])) (fail (name ++ ": " ++ show r))
        sampleOn page name x y expected = do
          draw atlas =<< texturedTriangle (glyphPageTextureId page) [(24, x / 2048), (28, y / 2048)]
          expectPixel env name expected
        sample = sampleOn 0
        fill = insertInto atlas surface >>= \case
          Just (0, _) -> fill
          other -> pure other
    firstGlyph "unexpected glyph bounds"
    sample "white patch" 0.5 0.5 (255, 0, 0)
    sample "glyph with padded source pitch" 6.5 2.5 (255, 0, 0)
    sample "transparent glyph padding" 8.5 2.5 (0, 0, 0)
    -- A full page opens the next one rather than failing.
    fill >>= \case
      Just (1, [x, y, _, _]) ->
        sampleOn 1 "glyph on the second page" (realToFrac x + 1.5) (realToFrac y + 0.5) (255, 0, 0)
      next -> fail ("a full page did not open another: " ++ show next)
    resetAtlas atlas
    sample "white patch after reset" 0.5 0.5 (255, 0, 0)
    sample "old glyph cleared by reset" 6.5 2.5 (0, 0, 0)
    firstGlyph "a reset atlas did not start on page 0"

-- | Images reach the atlas texture however they change: rewritten in place,
-- added beside others, or added by growing the atlas. Each is sampled at its
-- centre through white vertices.
imageChecks :: SdlEnv -> Context -> ImageAtlas -> (DrawData -> IO ()) -> IO ()
imageChecks env ctx images draw = do
  let register tid w h px = registerImage ctx (ImageId tid) w h px >>= (`unless` fail "image registration failed")
      solid tid w h (r, g, b) = register tid w h (BS.concat (replicate (w * h) (BS.pack [r, g, b, 255])))
      sample name tid expected = do
        syncImageAtlas (sdlRenderer env) images ctx
        (u0, v0, u1, v1) <- lookupImageUv ctx (ImageId tid) >>= maybe (fail "missing image") pure
        let white = [(o, 1) | o <- [8, 12, 16, 20]]
        draw =<< texturedTriangle atlasTextureId (white ++ [(24, (u0 + u1) / 2), (28, (v0 + v1) / 2)])
        expectPixel env name expected
  solid 1 4 4 (255, 0, 0)
  solid 2 4 4 (0, 255, 0)
  sample "first upload" 1 (255, 0, 0)
  solid 1 4 4 (0, 0, 255)
  sample "written in place" 1 (0, 0, 255)
  sample "neighbour kept" 2 (0, 255, 0)
  solid 3 4 4 (255, 255, 0)
  sample "added beside" 3 (255, 255, 0)
  solid 4 300 2 (0, 255, 255)
  sample "added by growing" 4 (0, 255, 255)
  sample "kept across growth" 1 (0, 0, 255)

-- | An atlas texture SDL will not make (here, for want of a renderer) is
-- asked for once per atlas generation: the second sync makes no SDL call,
-- so SDL's error, cleared between them, stays clear.
imageFailureChecks :: IO ()
imageFailureChecks = do
  ctx <- newPixelContext
  registerImage ctx (ImageId 1) 2 2 (BS.replicate 16 255) >>= (`unless` fail "image registration failed")
  bracket newImageAtlas destroyImageAtlas $ \atlas -> do
    syncImageAtlas nullPtr atlas ctx
    first <- sdlErrorText
    void clearError
    syncImageAtlas nullPtr atlas ctx
    again <- sdlErrorText
    unless (not (null first) && null again) $
      fail ("failed image atlas upload retried: " ++ show (first, again))

-- | At a fractional scale the pass clips to the device pixels the damage
-- was snapped to, and draws geometry at that scale: a red quad over layout
-- units 7 to 13 of damage at scale 1.5 fills device pixels 10 to 19 only.
fractionalScaleChecks :: SdlEnv -> (Float -> DrawData -> Damage -> IO ()) -> IO ()
fractionalScaleChecks env draw = do
  let scale = 1.5
      quad x0 y0 x1 y1 = [(x0, y0), (x1, y0), (x0, y1), (x1, y0), (x1, y1), (x0, y1)]
      row name xs expected = do
        got <- mapM (\x -> pixel env x 15) xs
        unless (got == expected) (fail (name ++ ": " ++ show (zip xs got)))
      red = (255, 0, 0)
      black = (0, 0, 0)
  empty <- geometry []
  draw 1 empty DamageFull
  big <- geometry (quad 0 0 60 60)
  draw scale big (snapDamage scale (DamageClip (Rect 7 7 6 6)))
  row "damage clip at scale 1.5" [9, 10, 19, 20] [black, red, red, black]
  small <- geometry (quad 0 0 20 20)
  draw scale small DamageFull
  row "geometry at scale 1.5" [29, 30] [red, black]

-- | A translucent canvas line as SDL draws it has even alpha across its
-- round caps and join: a cap drawn over the line's end would darken it.
pathChecks :: SdlEnv -> Context -> (DrawData -> IO ()) -> IO ()
pathChecks env ctx draw = do
  (resp, dd) <-
    warmupDraw ctx (withInput 400 300) . column . canvas (fixedWH 200 100) $ \(Rect x y _ _) ->
      drawStrokePathWith
        (P.stroke 20) {P.strokeCap = P.RoundCap, P.strokeJoin = P.RoundJoin}
        (P.polyline [V2 (x + 100) (y + 30), V2 (x + 150) (y + 30), V2 (x + 150) (y + 80)])
        (P.Solid (colorRGBA 255 255 255 128))
  draw dd
  let at dx dy = let Rect x y _ _ = respRect resp in pixel env (floor (x + dx)) (floor (y + dy))
  backdrop <- at 80 5
  line <- mapM (uncurry at) [(125, 30), (103, 30), (93, 30), (152, 28), (156, 24), (150, 88)]
  case nub line of
    [one] | one /= backdrop -> pure ()
    _ -> fail ("translucent line, caps and join: " ++ show line)

atlasBench :: SdlEnv -> IO ()
atlasBench env = withGlyphSurface $ \surface ->
  bracket (newAtlas (sdlRenderer env)) freeAtlas $ \atlas -> allocaArray 4 $ \uv -> do
    unless (atlas /= nullPtr) (fail "atlas creation failed")
    benchmark "atlas-reset-1024-glyphs" 5 100 . (resetAtlas atlas >>) . replicateM_ 1024 $
      insertAtlas atlas surface uv >>= \ok -> unless (ok /= 0) (fail "benchmark atlas insertion failed")

-- | Run an action @warm@ times, then @n@ times, and print the mean time and
-- allocation per run.
benchmark :: String -> Int -> Int -> IO () -> IO ()
benchmark name warm n action = do
  replicateM_ warm action
  performGC
  bytes0 <- getAllocationCounter
  t0 <- getMonotonicTimeNSec
  replicateM_ n action
  t1 <- getMonotonicTimeNSec
  bytes1 <- getAllocationCounter
  let perRun :: Integral a => a -> Double
      perRun x = fromIntegral x / fromIntegral n
  printf "%s: %.6f ms/frame | %.1f B/frame\n" name (perRun (t1 - t0) / 1e6) (perRun (bytes0 - bytes1))

-- | Every cursor kind maps to a system cursor SDL declares (SDL 3.2 does not
-- check), each kind SDL has a cursor for (its own fallback, bar the default
-- and hidden ones) gets a distinct one, and showing every kind twice creates
-- each system cursor once, NULL ones (this driver's) included.
cursorChecks :: IO ()
cursorChecks = do
  let kinds = [minBound .. maxBound] :: [UiCursorKind]
      wanted = mapMaybe sdlSystemCursor kinds
      own = [sdlSystemCursor k | k <- kinds, cursorFallback k == k, k /= UiCursorDefault, k /= UiCursorHidden]
  unless (all (\c -> c >= M.SDL_SYSTEM_CURSOR_DEFAULT && c < M.SDL_SYSTEM_CURSOR_COUNT) wanted) $
    fail ("cursor kinds ask for undeclared SDL cursors: " ++ show wanted)
  unless (all isJust own && length (nub own) == length own) $
    fail ("cursor kinds do not show their own SDL cursors: " ++ show own)
  bracket initCursors destroyCursors $ \cursors -> do
    mapM_ (showCursorKind cursors) (kinds ++ kinds)
    created <- map fst <$> readIORef (scSystem cursors)
    unless (length created == length (nub wanted) && all (`elem` created) wanted) $
      fail ("system cursors created more than once or not at all: " ++ show created)
    -- The hidden kind hides the pointer, and any other shows it again.
    showCursorKind cursors UiCursorHidden
    hidden <- not <$> cursorVisibleSafe
    showCursorKind cursors UiCursorPointer
    shownAgain <- cursorVisibleSafe
    unless (hidden && shownAgain) $
      fail ("the hidden cursor kind does not hide the pointer and show it again: " ++ show (hidden, shownAgain))

-- The public draw format is SDL_Vertex-compatible: xy, rgba, uv floats.
geometry :: [(Float, Float)] -> IO DrawData
geometry points = do
  let n = length points
  vertices <- mallocForeignPtrBytes (32 * n)
  indices <- mallocForeignPtrBytes (4 * n)
  withForeignPtr vertices $ \p -> pokeArray (castPtr p) [CFloat v | (x, y) <- points, v <- [x, y, 1, 0, 0, 1, 0, 0]]
  withForeignPtr indices $ \p -> pokeArray (castPtr p) [0 .. fromIntegral n - 1 :: CInt]
  let cmds = U.singleton (DrawCmd 0 0 800 600 0 0 (fromIntegral n) LayerContent 0)
  pure (DrawData vertices n indices n cmds (primArrayFromList [0, 0, 1, 1, 1]) colorBlack emptyShaderCalls emptyUniformBytes emptyEffectSpans)

-- | A triangle covering pixel (20, 20) that samples @texture@, with floats
-- written at each vertex's byte offsets: colour at 8 to 20, UV at 24 and 28.
texturedTriangle :: Int -> [(Int, Float)] -> IO DrawData
texturedTriangle texture writes = do
  dd <- geometry [(10, 10), (50, 10), (10, 50)]
  withForeignPtr (drawVertices dd) $ \p ->
    forM_ [0 .. 2] $ \i -> forM_ writes $ \(o, v) -> pokeByteOff p (i * 32 + o) (CFloat v)
  pure dd {drawCommands = U.map (\cmd -> cmd {cmdTextureId = texture}) (drawCommands dd)}

pixel :: SdlEnv -> Int -> Int -> IO (Word8, Word8, Word8)
pixel env x y = do
  let readBack = castPtr <$> renderReadPixels (sdlRenderer env) (PtrConst.unsafeFromPtr nullPtr)
  bracket readBack freeSurface $ \surface -> do
    unless (surface /= nullPtr) (fail "SDL readback failed")
    allocaBytes 4 $ \p -> do
      ok <- readPixel surface (fromIntegral x) (fromIntegral y) p (p `plusPtr` 1) (p `plusPtr` 2) (p `plusPtr` 3)
      unless (ok /= 0) (fail "SDL pixel read failed")
      (,,) <$> peekByteOff p 0 <*> peekByteOff p 1 <*> peekByteOff p 2

-- | Fail unless pixel (20, 20) has the expected colour.
expectPixel :: SdlEnv -> String -> (Word8, Word8, Word8) -> IO ()
expectPixel env name expected =
  pixel env 20 20 >>= \actual -> unless (actual == expected) (fail (name ++ ": " ++ show actual))

-- | A display sync passes SDL's appearance to the context, switching the
-- theme of a context that follows the system. The context starts with the
-- other appearance (light when SDL reports none, as the dummy driver does),
-- so a sync that did not report would leave the appearance and theme stale.
systemThemeChecks :: SdlEnv -> Context -> IO ()
systemThemeChecks env ctx = do
  want <-
    getSystemTheme <&> \case
      Video.SDL_SYSTEM_THEME_LIGHT -> Just AppearanceLight
      Video.SDL_SYSTEM_THEME_DARK -> Just AppearanceDark
      _ -> Nothing
  setSystemAppearance ctx (Just (if want == Just AppearanceLight then AppearanceDark else AppearanceLight))
  followSystemThemeIO ctx (lightDark defaultLightTheme defaultTheme)
  (synced, _) <- syncDisplay ctx env emptyInput
  got <- getSystemAppearance synced
  theme <- getThemeIO synced
  unless (got == want) (fail ("system appearance " ++ show got ++ ", SDL reports " ++ show want))
  unless (theme == lightDark defaultLightTheme defaultTheme want) $
    fail "a context following the system kept the stale theme after the sync"

shaderChecks :: SdlEnv -> Context -> (DrawData -> IO ()) -> IO ()
shaderChecks env ctx draw = do
  caps <- getShaderCapabilities ctx
  let mgParams =
        MeshGradientParams
          { mgTopLeft = colorRGBA 240 40 40 255
          , mgTopRight = colorRGBA 40 220 40 255
          , mgBottomLeft = colorRGBA 40 60 240 255
          , mgBottomRight = colorRGBA 240 220 40 255
          }
      fallbackCol = colorRGBA 12 90 180 255
      cfg =
        defaultShaderConfig
          { scLayout = fixedWH 100 80
          , scFallback = fallbackCol
          }
  if not (capSurfaceShaders caps)
    then do
      (_, dd) <- warmupDraw ctx (withInput 800 600) (columnWith tight (shaderConfigured cfg meshGradient4 mgParams))
      draw dd
      (r, g, b) <- pixel env 50 40
      let near c1 c2 = abs (fromIntegral c1 - fromIntegral c2 :: Int) <= 2
      unless (near r (colorR fallbackCol) && near g (colorG fallbackCol) && near b (colorB fallbackCol)) $
        fail ("software shader fallback mismatch: got " ++ show (r, g, b))
    else do
      -- Verify SDL 3.4 GPU SPIR-V/MSL execution matches CPU reference evaluator.
      let evalCpu :: Shader k u -> u -> Float -> Float -> Color
          evalCpu sh u px py = case shaderCpuEval sh of
            Nothing -> error "missing CPU evaluator"
            Just f ->
              f
                u
                CpuFragCtx
                  { cfcUv = V2 (px / 100) (py / 80)
                  , cfcPos = V2 px py
                  , cfcSize = V2 100 80
                  , cfcScale = 1.0
                  , cfcColor = colorWhite
                  , cfcCornerRadius = 0.0
                  }
                (const colorBlack)
          near3 (r1, g1, b1) col =
            let a = fromIntegral (colorA col) :: Int
                blend ch = (fromIntegral ch * a + 127) `div` 255
                d x y = abs (fromIntegral x - y)
             in d r1 (blend (colorR col)) <= 4 && d g1 (blend (colorG col)) <= 4 && d b1 (blend (colorB col)) <= 4
      (_, dd1) <- warmupDraw ctx (withInput 800 600) (columnWith tight (shaderBox (fixedWH 100 80) meshGradient4 mgParams))
      draw dd1
      forM_ [(10, 10), (89, 10), (10, 69), (89, 69), (50, 40)] $ \(x, y) -> do
        gpuPx <- pixel env x y
        let cpuPx = evalCpu meshGradient4 mgParams (fromIntegral x + 0.5) (fromIntegral y + 0.5)
        unless (near3 gpuPx cpuPx) $
          fail ("meshGradient4 GPU vs CPU mismatch at " ++ show (x, y) ++ ": gpu=" ++ show gpuPx ++ " cpu=" ++ show (colorR cpuPx, colorG cpuPx, colorB cpuPx))

      let sdfParams =
            SdfBoxParams
              { sbFill = colorRGBA 30 160 220 255
              , sbBorder = colorRGBA 250 180 40 255
              , sbBorderWidth = 4
              , sbRadius = 16
              }
      (_, dd2) <- warmupDraw ctx (withInput 800 600) (columnWith tight (shaderBox (fixedWH 100 80) sdfRoundedBox sdfParams))
      draw dd2
      -- Center is fill, top-center at y=2 is border, corner (1, 1) is outside rounded rect (black clear).
      forM_ [(50, 40), (50, 2), (1, 1)] $ \(x, y) -> do
        gpuPx <- pixel env x y
        let cpuPx = evalCpu sdfRoundedBox sdfParams (fromIntegral x + 0.5) (fromIntegral y + 0.5)
        unless (near3 gpuPx cpuPx) $
          fail ("sdfRoundedBox GPU vs CPU mismatch at " ++ show (x, y) ++ ": gpu=" ++ show gpuPx ++ " cpu=" ++ show (colorR cpuPx, colorG cpuPx, colorB cpuPx))

      -- Tier 2 subtree effects (offscreen targets, fused ColorEffect, 2-pass gaussianBlur, nested effects).
      when (capNodeEffects caps) $ do
        let srcCol = colorRGBA 200 40 20 255
            evalEffectCpu :: Shader k u -> u -> Color
            evalEffectCpu sh u = case shaderCpuEval sh of
              Nothing -> error "missing CPU evaluator"
              Just f ->
                f
                  u
                  CpuFragCtx
                    { cfcUv = V2 0.5 0.5
                    , cfcPos = V2 40 30
                    , cfcSize = V2 80 60
                    , cfcScale = 1.0
                    , cfcColor = colorWhite
                    , cfcCornerRadius = 0.0
                    }
                  (const srcCol)

        -- 1. Single-pass ColorEffect (grayscale)
        (_, ddGray) <- warmupDraw ctx (withInput 800 600) $
          columnWith tight (withShader grayscale 1.0 (box (fixedWH 80 60) srcCol))
        draw ddGray
        grayPx <- pixel env 40 30
        let grayCpu = evalEffectCpu grayscale 1.0
        unless (near3 grayPx grayCpu) $
          fail ("Tier 2 grayscale GPU vs CPU mismatch: gpu=" ++ show grayPx ++ " cpu=" ++ show (colorR grayCpu, colorG grayCpu, colorB grayCpu))

        -- 2. Fused 1-pass composeColor grayscale tint
        let fused = composeColor grayscale tint
            fusedU = (1.0, TintParams (colorRGBA 0 80 240 255) 0.5)
        (_, ddFused) <- warmupDraw ctx (withInput 800 600) $
          columnWith tight (withShader fused fusedU (box (fixedWH 80 60) srcCol))
        draw ddFused
        fusedPx <- pixel env 40 30
        let fusedCpu = evalEffectCpu fused fusedU
        unless (near3 fusedPx fusedCpu) $
          fail ("Tier 2 fused composeColor GPU vs CPU mismatch: gpu=" ++ show fusedPx ++ " cpu=" ++ show (colorR fusedCpu, colorG fusedCpu, colorB fusedCpu))

        -- 3. Separable 2-pass gaussianBlur with bleed margin halo outside the box
        (_, ddBlur) <- warmupDraw ctx (withInput 800 600) $
          columnWith (padAll 30 . tight) $
            withShader gaussianBlur 4.0 (box (fixedWH 40 40) (colorRGBA 255 0 0 255))
        draw ddBlur
        (cr, cg, cb) <- pixel env 50 50
        (hr, hg, hb) <- pixel env 27 50
        farPx <- pixel env 5 50
        unless (cr > 200 && cg < 10 && cb < 10) $
          fail ("Tier 2 gaussianBlur center unexpected: " ++ show (cr, cg, cb))
        unless (hr > 15 && hr < 245 && hg < 10 && hb < 10) $
          fail ("Tier 2 gaussianBlur bleed halo missing at (27,50): " ++ show (hr, hg, hb))
        unless (farPx == (0, 0, 0)) $
          fail ("Tier 2 gaussianBlur leaked past margin at (5,50): " ++ show farPx)

        -- 4. Translucent child under Tier 2 effects verifies premultiplied-alpha
        -- offscreen targets do not double-darken on composite.
        let halfCol = colorRGBA 200 100 50 128
        (_, ddAlpha) <- warmupDraw ctx (withInput 800 600) $
          columnWith tight (withShader groupOpacity 1.0 (box (fixedWH 80 60) halfCol))
        draw ddAlpha
        alphaPx <- pixel env 40 30
        unless (near3 alphaPx halfCol) $
          fail ("Tier 2 translucent groupOpacity 1.0 double-darkened: gpu=" ++ show alphaPx ++ " expected=" ++ show halfCol)

        -- 5. Nested subtree effects: groupOpacity 0.5 around grayscale 1.0
        (_, ddNest) <- warmupDraw ctx (withInput 800 600) $
          columnWith tight $
            withShader groupOpacity 0.5 $
              withShader grayscale 1.0 (box (fixedWH 80 60) srcCol)
        draw ddNest
        nestPx <- pixel env 40 30
        let halfGray = colorRGBA (colorR grayCpu) (colorG grayCpu) (colorB grayCpu) 128
        unless (near3 nestPx halfGray) $
          fail ("Tier 2 nested groupOpacity(grayscale) mismatch: gpu=" ++ show nestPx ++ " expected~" ++ show (colorR halfGray `div` 2))

      putStrLn ("  (verified live GPU Tier 1 + Tier 2 shaders on SDL renderer: " ++ show (sdlRendererName env) ++ ")")

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  args <- getArgs
  iterations <- max 1 . fromMaybe 500 . (>>= readMaybe) <$> lookupEnv "NANO_RENDER_ITERATIONS"
  let bench = args == ["--bench"] || args == ["--atlas-bench"]
      gpuShadersOnly = args == ["--gpu-shaders"]
      native = args == ["--native"] || gpuShadersOnly
  unless (bench || native) $ do
    setEnv "SDL_VIDEO_DRIVER" "dummy"
    setEnv "SDL_RENDER_DRIVER" "software"
  unless (bench || gpuShadersOnly) inputChecks
  ctx <- newPixelContext
  -- These checks read pixels over a black clear: a black page makes the
  -- frame's clear black.
  getThemeIO ctx >>= setThemeIO ctx . pageColor colorBlack
  withSdlBench ctx $ \_ env -> bracket newImageAtlas destroyImageAtlas $ \images ->
    bracket (newRenderBatch (sdlRenderer env)) destroyRenderBatch $ \batch -> do
      let drawScaled scale glyphs dd dmg =
            renderDrawDataPass batch (sdlRenderer env) scale dd images glyphs dmg >> flushRenderBatch batch
          drawWithGlyph = drawScaled 1
          draw = drawWithGlyph nullPtr
          damage = DamageClip (Rect 15 15 10 10)
          step name act =
            (act >> putStrLn ("SDL " ++ name ++ ": ok")) `catch` \(e :: SomeException) -> do
              hPutStrLn stderr ("SDL " ++ name ++ ": FAIL: " ++ displayException e)
              hFlush stderr
              hFlush stdout
              throwIO e
      case args of
        ["--gpu-shaders"] ->
          step "surface shader GPU/fallback readback" (shaderChecks env ctx (`draw` DamageFull))
        ["--atlas-bench"] -> atlasBench env
        ["--bench"] -> do
          let quad x y = [(x, y), (x + 40, y), (x + 40, y + 40), (x + 40, y + 40), (x, y + 40), (x, y)]
              offscreen = concat (replicate 4096 (quad 100 100))
          sparse <- geometry offscreen
          mixed <- geometry (offscreen ++ quad 10 10)
          dense <- geometry (concat (replicate 256 (quad 10 10)))
          forM_
            [ ("sparse-partial", sparse, damage), ("mixed-partial", mixed, damage), ("dense-partial", dense, damage)
            , ("dense-full", dense, DamageFull)
            ]
            $ \(name, dd, dmg) ->
              benchmark name 30 iterations (draw dd dmg >> void (renderPresentSafe (sdlRenderer env)))
        _ -> do
          -- A six-index group can hold independent triangles, and a command can
          -- end in three indices. Neither may be culled as an offscreen quad.
          step "partial-damage triangle readback" $ do
            let visible = [(10, 10), (50, 10), (10, 50)]
                outside = [(100, 100), (120, 100), (100, 120)]
            empty <- geometry []
            forM_ [("independent triangles", [outside]), ("triangle tail", [outside, outside])] $ \(name, before) -> do
              let points = concat before ++ visible
              dd <- geometry points
              draw empty DamageFull
              draw dd damage
              inside <- pixel env 20 20
              untouched <- pixel env 12 12
              unless (inside == (255, 0, 0) && untouched == (0, 0, 0)) $
                fail (name ++ ": clipped geometry lost or escaped damage: " ++ show (inside, untouched))
          step "glyph upload, padding and reset readback" $
            atlasChecks env (\glyphs dd -> drawWithGlyph glyphs dd DamageFull)
          step "image atlas upload readback" (imageChecks env ctx images (`draw` DamageFull))
          step "failed image atlas upload reported once" imageFailureChecks
          step "fractional-scale clip and geometry readback" (fractionalScaleChecks env (\s -> drawScaled s nullPtr))
          step "canvas line caps and joins readback" (pathChecks env ctx (`draw` DamageFull))
          step "surface shader GPU/fallback readback" (shaderChecks env ctx (`draw` DamageFull))
          step "cursor mapping and creation" cursorChecks
          step "native events and the system theme" (queueChecks >> systemThemeChecks env ctx)
  unless (bench || gpuShadersOnly) $ do
    windowChecks (if native then "native drivers" else "dummy video, software renderer") False
    -- With a display, repeat on the GPU renderers, which give a transparent
    -- window custom blend modes: OpenGL blends alpha with the colour's
    -- operation, and OpenGL ES keeps the larger alpha. Missing renderers are
    -- skipped.
    display <- lookupEnv "DISPLAY"
    when (isJust display && not native) $
      forM_ ["opengl", "opengles2"] $ \renderer -> do
        setEnv "SDL_VIDEO_DRIVER" "x11"
        setEnv "SDL_RENDER_DRIVER" renderer
        try (windowChecks ("x11, " ++ renderer) True) >>= \case
          Left (err :: IOException)
            | any (`isInfixOf` show err) ["SDL_Init", "SDL_CreateWindowAndRenderer"] ->
                putStrLn ("SDL window checks on " ++ renderer ++ " skipped: " ++ show err)
            | otherwise -> throwIO err
          Right () -> pure ()
