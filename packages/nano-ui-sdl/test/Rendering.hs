{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE PackageImports #-}

module Main (main) where

import Control.Exception (IOException, bracket, throwIO, try)
import Control.Monad (forM_, replicateM_, unless, void, when)
import Data.ByteString qualified as BS
import Data.Functor ((<&>))
import Data.IORef (readIORef)
import Data.List (isInfixOf, nub)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Primitive.PrimArray (primArrayFromList)
import Data.Vector.Unboxed qualified as U
import Data.Word (Word32, Word8)
import Foreign.C.Types (CBool (..), CFloat (..), CInt (..), CUInt (..))
import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (allocaArray, peekArray, pokeArray)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (getAllocationCounter)
import Keyboard (keyboardTranslation)
import NanoUI
  ( Color, ImageConfig (..), ImageId (..), Rect (..), Rotation (..), V2 (..), canvas, colorRGBA, column, defaultImageConfig
  , defaultLightTheme, defaultTheme, drawPathWith, drawStrokePathWith, fixedWH, followSystemTheme, getTheme, lightDark
  , imageConfigured', respRect
  )
import NanoUI.Path qualified as P
import NanoUI.Backend (Appearance (..), emptyInput, getSystemAppearance, setSystemAppearance)
import NanoUI.Internal.Context (lookupImageUv)
import NanoUI.Sdl.Internal.Cursor (SdlCursors (..), destroyCursors, initCursors, sdlSystemCursor, showCursorKind)
import NanoUI.Sdl.Internal.Image (ImageAtlas, destroyImageAtlas, newImageAtlas, syncImageAtlas)
import NanoUI.Sdl.Internal.Render (destroyRenderBatch, flushRenderBatch, newRenderBatch, renderDrawDataPass)
import NanoUI.Testing
  ( Context, Damage (..), DrawCmd (..), DrawData (..), Layer (..), UiCursorKind (..), atlasTextureId, glyphPageTextureId
  , newPixelContext, registerImage
  )
import NanoUI.Testing.Assert (withInput)
import NanoUI.Testing.Harness (warmupDraw)
import SDL3.Sys.Bindgen.Mouse qualified as M
import SDL3.Sys.Bindgen.Render (SDL_Renderer, SDL_Texture)
import SDL3.Sys.Bindgen.Runtime.PtrConst qualified as PtrConst
import SDL3.Sys.Bindgen.Video qualified as Video
import SDL3.Sys.Events (pushEvent)
import SDL3.Sys.Mouse (cursorVisibleSafe)
import SDL3.Sys.Render (renderPresentSafe, renderReadPixels)
import SDL3.Sys.Video (getSystemTheme)
import System.Environment (getArgs, lookupEnv, setEnv)
import System.Mem (performGC)
import Text.Printf (printf)
import Text.Read (readMaybe)
import WindowChecks (windowChecks)
import "nano-ui-sdl" NanoUI.Backend.Sdl (SdlEnv (..), syncDisplay, withSdlBench)
import "nano-ui-sdl" NanoUI.Sdl.Internal.Input (SdlEvent (..), pollEvents)

foreign import ccall unsafe "SDL_ReadSurfacePixel"
  readPixel :: Ptr () -> CInt -> CInt -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO CBool
foreign import ccall unsafe "SDL_DestroySurface" freeSurface :: Ptr () -> IO ()
foreign import capi unsafe "SDL3/SDL.h value SDL_PIXELFORMAT_RGBA32" rgba32 :: CUInt
foreign import capi unsafe "SDL3/SDL.h SDL_CreateSurfaceFrom" surfaceFrom :: CInt -> CInt -> CUInt -> Ptr Word8 -> CInt -> IO (Ptr ())
foreign import capi unsafe "SDL3/SDL.h SDL_FillSurfaceRect" fillSurface :: Ptr () -> Ptr () -> CUInt -> IO CBool
foreign import ccall unsafe "nano_ui_text_atlas_create" newAtlas :: Ptr SDL_Renderer -> IO (Ptr ())
foreign import ccall unsafe "nano_ui_text_atlas_destroy" freeAtlas :: Ptr () -> IO ()
foreign import ccall unsafe "nano_ui_text_atlas_reset" resetAtlas :: Ptr () -> IO ()
foreign import ccall unsafe "nano_ui_text_atlas_texture" atlasTexture :: Ptr () -> CInt -> IO (Ptr SDL_Texture)
foreign import ccall unsafe "nano_ui_text_atlas_insert_surface" insertAtlas :: Ptr () -> Ptr () -> Ptr CFloat -> IO CBool

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

atlasChecks :: SdlEnv -> ((Int -> Ptr SDL_Texture) -> DrawData -> IO ()) -> IO ()
atlasChecks env draw = withGlyphSurface $ \surface ->
  bracket (newAtlas (sdlRenderer env)) freeAtlas $ \atlas -> do
    unless (atlas /= nullPtr) (fail "atlas creation failed")
    let firstGlyph name = insertInto atlas surface >>= \r -> unless (r == Just (0, [5, 1, 3, 2])) (fail (name ++ ": " ++ show r))
        sampleOn page name x y expected = do
          textures <- mapM (atlasTexture atlas) [0 .. 3]
          draw (textures !!) =<< texturedTriangle (glyphPageTextureId page) [(24, x / 2048), (28, y / 2048)]
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
      Just (1, [x, y, _, _]) -> sampleOn 1 "glyph on the second page" (realToFrac x + 1.5) (realToFrac y + 0.5) (255, 0, 0)
      next -> fail ("a full page did not open another: " ++ show next)
    resetAtlas atlas
    sample "white patch after reset" 0.5 0.5 (255, 0, 0)
    sample "old glyph cleared by reset" 6.5 2.5 (0, 0, 0)
    firstGlyph "a reset atlas did not start on page 0"

-- | Images reach the atlas texture however they change: rewritten in place,
-- added beside others, or added by growing the atlas. Each is sampled at its
-- centre through white vertices. Then a view's images: a clockwise quarter
-- turn puts the left half on top, and a half-opaque image shows half the backdrop.
imageChecks :: SdlEnv -> Context -> ImageAtlas -> (DrawData -> IO ()) -> IO ()
imageChecks env ctx images draw = do
  let register tid w h px = registerImage ctx (ImageId tid) w h px >>= \ok -> unless ok (fail "image registration failed")
      solid tid w h (r, g, b) = register tid w h (BS.concat (replicate (w * h) (BS.pack [r, g, b, 255])))
      sample name tid expected = do
        syncImageAtlas (sdlRenderer env) images ctx
        (u0, v0, u1, v1) <- lookupImageUv ctx (ImageId tid) >>= maybe (fail "missing image") pure
        draw =<< texturedTriangle atlasTextureId ([(o, 1) | o <- [8, 12, 16, 20]] ++ [(24, (u0 + u1) / 2), (28, (v0 + v1) / 2)])
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
  -- Red on the left half, blue on the right.
  register 5 8 4 (BS.concat [BS.pack (if x < 4 then [255, 0, 0, 255] else [0, 0, 255, 255]) | _ <- [0 .. 3 :: Int], x <- [0 .. 7 :: Int]])
  let image h cfg = imageConfigured' cfg {icLayout = fixedWH 80 h} (ImageId 5)
  ((turned, faded), dd) <-
    warmupDraw ctx (withInput 400 300) . column $
      (,) <$> image 80 defaultImageConfig {icRotation = RotateSolid (pi / 2)} <*> image 40 defaultImageConfig {icOpacity = 0.5}
  syncImageAtlas (sdlRenderer env) images ctx
  draw dd
  let at resp dx dy = let Rect x y _ _ = respRect resp in pixel env (floor (x + dx)) (floor (y + dy))
  top <- at turned 40 15
  bottom <- at turned 40 65
  unless (top == (255, 0, 0) && bottom == (0, 0, 255)) (fail ("turned image: " ++ show (top, bottom)))
  half@(r, g, b) <- at faded 10 20
  unless (r >= 120 && r <= 136 && g == 0 && b == 0) (fail ("half-opaque image: " ++ show half))

-- | Canvas paths as SDL draws them: an even-odd ring leaves its hole
-- unfilled; a translucent line has even alpha across its round caps and join
-- (a cap drawn over the line's end would darken it); a dashed line has gaps.
pathChecks :: SdlEnv -> Context -> (DrawData -> IO ()) -> IO ()
pathChecks env ctx draw = do
  let white = colorRGBA 255 255 255 255
      glass = colorRGBA 255 255 255 128
  (resp, dd) <-
    warmupDraw ctx (withInput 400 300) . column . canvas (fixedWH 200 100) $ \(Rect x y _ _) -> do
      let c = V2 (x + 40) (y + 50)
      drawPathWith P.EvenOdd (P.circle c 30 <> P.circle c 15) (P.Solid white)
      drawStrokePathWith
        (P.stroke 20) {P.strokeCap = P.RoundCap, P.strokeJoin = P.RoundJoin}
        (P.polyline [V2 (x + 100) (y + 30), V2 (x + 150) (y + 30), V2 (x + 150) (y + 80)])
        (P.Solid glass)
      drawStrokePathWith (P.stroke 4) {P.strokeDash = [6, 6]} (P.polyline [V2 (x + 100) (y + 95), V2 (x + 190) (y + 95)]) (P.Solid white)
  draw dd
  let at dx dy = let Rect x y _ _ = respRect resp in pixel env (floor (x + dx)) (floor (y + dy))
  backdrop <- at 80 5
  hole <- at 40 50
  ring <- at 40 27
  unless (hole == backdrop && ring == (255, 255, 255)) (fail ("even-odd ring: " ++ show (hole, ring, backdrop)))
  line <- mapM (uncurry at) [(125, 30), (103, 30), (93, 30), (152, 28), (156, 24), (150, 88)]
  case nub line of
    [one] | one /= backdrop -> pure ()
    _ -> fail ("translucent line, caps and join: " ++ show line)
  dashes <- mapM (\dx -> at dx 95) [103, 109, 115, 121]
  unless (dashes == [(255, 255, 255), backdrop, (255, 255, 255), backdrop]) (fail ("dashes: " ++ show dashes))

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
-- check), each kind SDL has a cursor for gets a distinct one, and showing
-- every kind twice creates each system cursor once, NULL ones (this driver's)
-- included.
cursorChecks :: IO ()
cursorChecks = do
  let kinds = [minBound .. maxBound] :: [UiCursorKind]
      wanted = mapMaybe sdlSystemCursor kinds
      native =
        [ (UiCursorPointer, M.SDL_SYSTEM_CURSOR_POINTER), (UiCursorText, M.SDL_SYSTEM_CURSOR_TEXT)
        , (UiCursorNsResize, M.SDL_SYSTEM_CURSOR_NS_RESIZE), (UiCursorEwResize, M.SDL_SYSTEM_CURSOR_EW_RESIZE)
        , (UiCursorNwseResize, M.SDL_SYSTEM_CURSOR_NWSE_RESIZE), (UiCursorNeswResize, M.SDL_SYSTEM_CURSOR_NESW_RESIZE)
        , (UiCursorNotAllowed, M.SDL_SYSTEM_CURSOR_NOT_ALLOWED), (UiCursorWait, M.SDL_SYSTEM_CURSOR_WAIT)
        , (UiCursorProgress, M.SDL_SYSTEM_CURSOR_PROGRESS), (UiCursorCrosshair, M.SDL_SYSTEM_CURSOR_CROSSHAIR)
        , (UiCursorMove, M.SDL_SYSTEM_CURSOR_MOVE), (UiCursorNResize, M.SDL_SYSTEM_CURSOR_N_RESIZE)
        , (UiCursorNeResize, M.SDL_SYSTEM_CURSOR_NE_RESIZE), (UiCursorEResize, M.SDL_SYSTEM_CURSOR_E_RESIZE)
        , (UiCursorSeResize, M.SDL_SYSTEM_CURSOR_SE_RESIZE), (UiCursorSResize, M.SDL_SYSTEM_CURSOR_S_RESIZE)
        , (UiCursorSwResize, M.SDL_SYSTEM_CURSOR_SW_RESIZE), (UiCursorWResize, M.SDL_SYSTEM_CURSOR_W_RESIZE)
        , (UiCursorNwResize, M.SDL_SYSTEM_CURSOR_NW_RESIZE)
        ]
  unless (all (\c -> c >= M.SDL_SYSTEM_CURSOR_DEFAULT && c < M.SDL_SYSTEM_CURSOR_COUNT) wanted) $
    fail ("cursor kinds ask for undeclared SDL cursors: " ++ show wanted)
  unless (all (\(k, c) -> sdlSystemCursor k == Just c) native && length (nub (map snd native)) == length native) $
    fail ("cursor kinds do not show their own SDL cursors: " ++ show [(k, sdlSystemCursor k) | (k, _) <- native])
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

black :: Color
black = colorRGBA 0 0 0 255

-- The public draw format is SDL_Vertex-compatible: xy, rgba, uv floats.
geometry :: [(Float, Float)] -> IO DrawData
geometry points = do
  let n = length points
  vertices <- mallocForeignPtrBytes (32 * n)
  indices <- mallocForeignPtrBytes (4 * n)
  withForeignPtr vertices $ \p -> pokeArray (castPtr p) [CFloat v | (x, y) <- points, v <- [x, y, 1, 0, 0, 1, 0, 0]]
  withForeignPtr indices $ \p -> pokeArray (castPtr p) [0 .. fromIntegral n - 1 :: CInt]
  pure (DrawData vertices n indices n (U.singleton (DrawCmd 0 0 800 600 0 0 (fromIntegral n) LayerContent)) (primArrayFromList [0, 0, 1, 1, 1]))

-- | A triangle covering pixel (20, 20) that samples @texture@, with floats
-- written at each vertex's byte offsets: colour at 8 to 20, UV at 24 and 28.
texturedTriangle :: Int -> [(Int, Float)] -> IO DrawData
texturedTriangle texture writes = do
  dd <- geometry [(10, 10), (50, 10), (10, 50)]
  withForeignPtr (drawVertices dd) $ \p ->
    forM_ [0 .. 2] $ \i -> forM_ writes $ \(o, v) -> pokeByteOff p (i * 32 + o) (CFloat v)
  pure dd {drawCommands = U.map (\cmd -> cmd {cmdTextureId = texture}) (drawCommands dd)}

pixel :: SdlEnv -> Int -> Int -> IO (Word8, Word8, Word8)
pixel env x y =
  bracket (castPtr <$> renderReadPixels (sdlRenderer env) (PtrConst.unsafeFromPtr nullPtr)) freeSurface $ \surface -> do
    unless (surface /= nullPtr) (fail "SDL readback failed")
    allocaBytes 4 $ \p -> do
      ok <- readPixel surface (fromIntegral x) (fromIntegral y) p (p `plusPtr` 1) (p `plusPtr` 2) (p `plusPtr` 3)
      unless (ok /= 0) (fail "SDL pixel read failed")
      (,,) <$> peekByteOff p 0 <*> peekByteOff p 1 <*> peekByteOff p 2

-- | Fail unless pixel (20, 20) has the expected colour.
expectPixel :: SdlEnv -> String -> (Word8, Word8, Word8) -> IO ()
expectPixel env name expected = pixel env 20 20 >>= \actual -> unless (actual == expected) (fail (name ++ ": " ++ show actual))

-- | SDL's theme-change event reaches the loop as 'EvSystemThemeChanged', and a
-- display sync passes SDL's appearance to the context, switching the theme of
-- a context that follows the system.
systemThemeChecks :: SdlEnv -> Context -> IO ()
systemThemeChecks env ctx = do
  -- SDL_EVENT_SYSTEM_THEME_CHANGED; only the type field is set.
  pushed <- alloca $ \ev -> pokeByteOff ev 0 (0x108 :: Word32) >> pushEvent ev
  unless pushed (fail "SDL_PushEvent failed")
  events <- pollEvents
  unless (EvSystemThemeChanged `elem` events) (fail ("theme event not translated: " ++ show events))
  want <-
    getSystemTheme <&> \case
      Video.SDL_SYSTEM_THEME_LIGHT -> Just AppearanceLight
      Video.SDL_SYSTEM_THEME_DARK -> Just AppearanceDark
      _ -> Nothing
  -- Start the context with the opposite appearance, so a sync that did not
  -- report would leave the appearance and theme stale.
  setSystemAppearance ctx (Just (if want == Just AppearanceDark then AppearanceLight else AppearanceDark))
  followSystemTheme ctx (lightDark defaultLightTheme defaultTheme)
  (synced, _) <- syncDisplay ctx env emptyInput
  got <- getSystemAppearance synced
  unless (got == want) (fail ("system appearance " ++ show got ++ ", SDL reports " ++ show want))
  theme <- getTheme synced
  unless (theme == lightDark defaultLightTheme defaultTheme want) $
    fail "a context following the system kept the stale theme after the sync"

main :: IO ()
main = do
  args <- getArgs
  iterations <- max 1 . fromMaybe 500 . (>>= readMaybe) <$> lookupEnv "NANO_RENDER_ITERATIONS"
  let bench = args == ["--bench"] || args == ["--atlas-bench"]
      native = args == ["--native"]
  unless (bench || native) $ do
    setEnv "SDL_VIDEODRIVER" "dummy"
    setEnv "SDL_RENDER_DRIVER" "software"
  unless bench keyboardTranslation
  ctx <- newPixelContext
  withSdlBench ctx $ \_ env -> bracket newImageAtlas destroyImageAtlas $ \images ->
    bracket (newRenderBatch (sdlRenderer env)) destroyRenderBatch $ \batch -> do
      let drawWithGlyph tex dd dmg = renderDrawDataPass batch (sdlRenderer env) black dd images tex dmg >> flushRenderBatch batch
          draw = drawWithGlyph (const nullPtr)
          damage = DamageClip (Rect 15 15 10 10)
          step name act = act >> putStrLn ("SDL " ++ name ++ ": ok")
      case args of
        ["--atlas-bench"] -> atlasBench env
        ["--bench"] -> do
          let quad x y = [(x, y), (x + 40, y), (x + 40, y + 40), (x + 40, y + 40), (x, y + 40), (x, y)]
              offscreen = concat (replicate 4096 (quad 100 100))
          sparse <- geometry offscreen
          mixed <- geometry (offscreen ++ quad 10 10)
          dense <- geometry (concat (replicate 256 (quad 10 10)))
          forM_ [("sparse-partial", sparse, damage), ("mixed-partial", mixed, damage), ("dense-partial", dense, damage), ("dense-full", dense, DamageFull)] $
            \(name, dd, dmg) -> benchmark name 30 iterations (draw dd dmg >> void (renderPresentSafe (sdlRenderer env)))
        _ -> do
          -- A six-index group can hold independent triangles, and a command can
          -- end in three indices. Neither may be culled as an offscreen quad.
          step "partial-damage triangle readback" $ do
            let visible = [(10, 10), (50, 10), (10, 50)]
                outside = [(100, 100), (120, 100), (100, 120)]
            empty <- geometry []
            forM_ [("independent triangles", outside ++ visible), ("triangle tail", outside ++ outside ++ visible)] $ \(name, points) -> do
              dd <- geometry points
              draw empty DamageFull
              draw dd damage
              inside <- pixel env 20 20
              untouched <- pixel env 12 12
              unless (inside == (255, 0, 0) && untouched == (0, 0, 0)) $
                fail (name ++ ": clipped geometry lost or escaped damage: " ++ show (inside, untouched))
          step "glyph upload, padding and reset readback" (atlasChecks env (\tex dd -> drawWithGlyph tex dd DamageFull))
          step "image atlas upload, and turned and faded image readback" (imageChecks env ctx images (`draw` DamageFull))
          step "canvas paths, caps, joins and dashes readback" (pathChecks env ctx (`draw` DamageFull))
          step "cursor mapping and creation" cursorChecks
          step "system theme event" (systemThemeChecks env ctx)
  unless bench $ do
    windowChecks (if native then "native drivers" else "dummy video, software renderer") False
    -- With a display, repeat on the GPU renderers, which give a transparent
    -- window custom blend modes: OpenGL blends alpha with the colour's
    -- operation, and OpenGL ES keeps the larger alpha. Missing renderers are
    -- skipped.
    display <- lookupEnv "DISPLAY"
    when (isJust display && not native) $
      forM_ ["opengl", "opengles2"] $ \renderer -> do
        setEnv "SDL_VIDEODRIVER" "x11"
        setEnv "SDL_RENDER_DRIVER" renderer
        try (windowChecks ("x11, " ++ renderer) True) >>= \case
          Left (err :: IOException)
            | any (`isInfixOf` show err) ["SDL_Init", "SDL_CreateWindowAndRenderer"] ->
                putStrLn ("SDL window checks on " ++ renderer ++ " skipped: " ++ show err)
            | otherwise -> throwIO err
          Right () -> pure ()
