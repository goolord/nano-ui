{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE PackageImports #-}

module Main (main) where

import Control.Exception (bracket)
import Control.Monad (forM_, replicateM_, unless, void)
import Data.ByteString qualified as BS
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
import NanoUI (Color, ImageId (..), Rect (..), colorRGBA)
import NanoUI.Internal.Context (lookupImageUv)
import NanoUI.Sdl.Internal.Image (ImageAtlas, destroyImageAtlas, newImageAtlas, syncImageAtlas)
import NanoUI.Sdl.Internal.Render
  ( destroyRenderBatch
  , flushRenderBatch
  , newRenderBatch
  , renderDrawDataPass
  )
import NanoUI.Testing
  ( Context
  , Damage (..)
  , DrawCmd (..)
  , DrawData (..)
  , Layer (..)
  , atlasTextureId
  , glyphPageTextureId
  , newPixelContext
  , registerImage
  )
import SDL3.Sys.Bindgen.Render (SDL_Renderer, SDL_Texture)
import SDL3.Sys.Bindgen.Runtime.PtrConst qualified as PtrConst
import SDL3.Sys.Render (renderPresentSafe, renderReadPixels)
import System.Environment (getArgs, lookupEnv, setEnv)
import System.Mem (performGC)
import Text.Printf (printf)
import Text.Read (readMaybe)
import "nano-ui-sdl" NanoUI.Backend.Sdl (SdlEnv (..), withSdlBench)

foreign import ccall unsafe "SDL_ReadSurfacePixel"
  readPixel ::
    Ptr ()
    -> CInt
    -> CInt
    -> Ptr Word8
    -> Ptr Word8
    -> Ptr Word8
    -> Ptr Word8
    -> IO CBool

foreign import ccall unsafe "SDL_DestroySurface"
  freeSurface :: Ptr () -> IO ()

foreign import capi unsafe "SDL3/SDL.h value SDL_PIXELFORMAT_RGBA32"
  rgba32 :: CUInt

foreign import capi unsafe "SDL3/SDL.h SDL_CreateSurfaceFrom"
  surfaceFrom :: CInt -> CInt -> CUInt -> Ptr Word8 -> CInt -> IO (Ptr ())

foreign import capi unsafe "SDL3/SDL.h SDL_FillSurfaceRect"
  fillSurface :: Ptr () -> Ptr () -> CUInt -> IO CBool

foreign import ccall unsafe "nano_ui_text_atlas_create"
  newAtlas :: Ptr SDL_Renderer -> IO (Ptr ())

foreign import ccall unsafe "nano_ui_text_atlas_destroy"
  freeAtlas :: Ptr () -> IO ()

foreign import ccall unsafe "nano_ui_text_atlas_reset"
  resetAtlas :: Ptr () -> IO ()

foreign import ccall unsafe "nano_ui_text_atlas_texture"
  atlasTexture :: Ptr () -> CInt -> IO (Ptr SDL_Texture)

foreign import ccall unsafe "nano_ui_text_atlas_insert_surface"
  insertAtlas ::
    Ptr ()
    -> Ptr ()
    -> Ptr CFloat
    -> IO CBool

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
    placed <- insertInto atlas surface
    unless
      (placed == Just (0, [5, 1, 3, 2]))
      (fail ("unexpected glyph bounds: " ++ show placed))
    let sampleOn page name x y expected = do
          dd0 <- geometry [(10, 10), (50, 10), (10, 50)]
          let dd =
                dd0
                  { drawCommands =
                      U.map (\cmd -> cmd {cmdTextureId = glyphPageTextureId page}) (drawCommands dd0)
                  }
          withForeignPtr (drawVertices dd) $ \p -> forM_ [0 .. 2] $ \i -> do
            pokeByteOff p (i * 32 + 24) (CFloat (x / 2048))
            pokeByteOff p (i * 32 + 28) (CFloat (y / 2048))
          textures <- mapM (atlasTexture atlas) [0 .. 3]
          draw (\p -> textures !! p) dd
          actual <- pixel env 20 20
          unless (actual == expected) (fail (name ++ ": " ++ show actual))
        sample = sampleOn 0
    sample "white patch" 0.5 0.5 (255, 0, 0)
    sample "glyph with padded source pitch" 6.5 2.5 (255, 0, 0)
    sample "transparent glyph padding" 8.5 2.5 (0, 0, 0)
    -- A full page opens the next one rather than failing.
    let fill n = do
          r <- insertInto atlas surface
          case r of
            Just (0, _) -> fill (n + 1 :: Int)
            other -> pure (n, other)
    (_, next) <- fill 0
    unless (fmap fst next == Just 1) (fail ("a full page did not open another: " ++ show next))
    case next of
      Just (_, [x, y, _, _]) -> do
        let CFloat gx = x + 1.5
            CFloat gy = y + 0.5
        sampleOn 1 "glyph on the second page" gx gy (255, 0, 0)
      _ -> fail "missing second-page bounds"
    resetAtlas atlas
    sample "white patch after reset" 0.5 0.5 (255, 0, 0)
    sample "old glyph cleared by reset" 6.5 2.5 (0, 0, 0)
    after <- insertInto atlas surface
    unless (after == Just (0, [5, 1, 3, 2])) (fail ("a reset atlas did not start on page 0: " ++ show after))

-- | Images reach the texture however they changed: written again in place
-- and added beside the others, which upload only their rects, and added by
-- growing the atlas, which remakes the texture. Each image is sampled at its
-- centre through white vertices.
imageChecks :: SdlEnv -> Context -> ImageAtlas -> (DrawData -> IO ()) -> IO ()
imageChecks env ctx images draw = do
  let solid w h (r, g, b) = BS.concat (replicate (w * h) (BS.pack [r, g, b, 255]))
      register tid w h rgb = do
        ok <- registerImage ctx (ImageId tid) w h (solid w h rgb)
        unless ok (fail "image registration failed")
      sample name tid expected = do
        syncImageAtlas (sdlRenderer env) images ctx
        (u0, v0, u1, v1) <- lookupImageUv ctx (ImageId tid) >>= maybe (fail "missing image") pure
        dd0 <- geometry [(10, 10), (50, 10), (10, 50)]
        let dd = dd0 {drawCommands = U.map (\cmd -> cmd {cmdTextureId = atlasTextureId}) (drawCommands dd0)}
        withForeignPtr (drawVertices dd) $ \p -> forM_ [0 .. 2] $ \i -> do
          forM_ [8, 12, 16, 20] $ \o -> pokeByteOff p (i * 32 + o) (CFloat 1)
          pokeByteOff p (i * 32 + 24) (CFloat ((u0 + u1) / 2))
          pokeByteOff p (i * 32 + 28) (CFloat ((v0 + v1) / 2))
        draw dd
        actual <- pixel env 20 20
        unless (actual == expected) (fail (name ++ ": " ++ show actual))
  register 1 4 4 (255, 0, 0)
  register 2 4 4 (0, 255, 0)
  sample "first upload" 1 (255, 0, 0)
  register 1 4 4 (0, 0, 255)
  sample "written in place" 1 (0, 0, 255)
  sample "neighbour kept" 2 (0, 255, 0)
  register 3 4 4 (255, 255, 0)
  sample "added beside" 3 (255, 255, 0)
  register 4 300 2 (0, 255, 255)
  sample "added by growing" 4 (0, 255, 255)
  sample "kept across growth" 1 (0, 0, 255)

atlasBench :: SdlEnv -> IO ()
atlasBench env = withGlyphSurface $ \surface ->
  bracket (newAtlas (sdlRenderer env)) freeAtlas $ \atlas -> allocaArray 4 $ \uv -> do
    unless (atlas /= nullPtr) (fail "atlas creation failed")
    let
      fill = replicateM_ 1024 $ do
        ok <-
          insertAtlas
            atlas
            surface
            uv
        unless (ok /= 0) (fail "benchmark atlas insertion failed")
      action = resetAtlas atlas >> fill
    replicateM_ 5 action
    performGC
    before <- getAllocationCounter
    t0 <- getMonotonicTimeNSec
    replicateM_ 100 action
    t1 <- getMonotonicTimeNSec
    after <- getAllocationCounter
    printf
      "atlas-reset-1024-glyphs: %.6f ms/frame | %.1f B/frame\n"
      (fromIntegral (t1 - t0) / 1e8 :: Double)
      (fromIntegral (before - after) / 100 :: Double)

black :: Color
black = colorRGBA 0 0 0 255

-- The public draw format is SDL_Vertex-compatible: xy, rgba, uv floats.
geometry :: [(Float, Float)] -> IO DrawData
geometry points = do
  let
    n = length points
  vertices <- mallocForeignPtrBytes (32 * n)
  indices <- mallocForeignPtrBytes (4 * n)
  withForeignPtr vertices $ \p ->
    pokeArray
      (castPtr p)
      [CFloat v | (x, y) <- points, v <- [x, y, 1, 0, 0, 1, 0, 0]]
  withForeignPtr indices $ \p -> pokeArray (castPtr p) [0 .. fromIntegral n - 1 :: CInt]
  pure $
    DrawData
      vertices
      n
      indices
      n
      (U.singleton (DrawCmd 0 0 800 600 0 0 (fromIntegral n) LayerContent))
      (primArrayFromList [0, 0, 1, 1, 1])

pixel :: SdlEnv -> Int -> Int -> IO (Word8, Word8, Word8)
pixel env x y = bracket
  (castPtr <$> renderReadPixels (sdlRenderer env) (PtrConst.unsafeFromPtr nullPtr))
  freeSurface
  $ \surface -> do
    unless (surface /= nullPtr) (fail "SDL readback failed")
    allocaBytes 4 $ \p -> do
      ok <-
        readPixel
          surface
          (fromIntegral x)
          (fromIntegral y)
          p
          (p `plusPtr` 1)
          (p `plusPtr` 2)
          (p `plusPtr` 3)
      unless (ok /= 0) (fail "SDL pixel read failed")
      (,,) <$> peekByteOff p 0 <*> peekByteOff p 1 <*> peekByteOff p 2

main :: IO ()
main = do
  args <- getArgs
  requested <- lookupEnv "NANO_RENDER_ITERATIONS"
  let
    iterations = max 1 (maybe 500 id (requested >>= readMaybe))
  let
    bench = args == ["--bench"] || args == ["--atlas-bench"]
  unless (bench || args == ["--native"]) $ do
    setEnv "SDL_VIDEODRIVER" "dummy"
    setEnv "SDL_RENDER_DRIVER" "software"
  ctx <- newPixelContext
  withSdlBench ctx $ \_ env -> bracket newImageAtlas destroyImageAtlas $ \images ->
    bracket (newRenderBatch (sdlRenderer env)) destroyRenderBatch $ \batch -> do
      let
        drawWithGlyph tex dd dmg = do
          renderDrawDataPass batch (sdlRenderer env) black dd images tex dmg
          flushRenderBatch batch
        draw = drawWithGlyph (const nullPtr)
        visible = [(10, 10), (50, 10), (10, 50)]
        outside = [(100, 100), (120, 100), (100, 120)]
        damage = DamageClip (Rect 15 15 10 10)
      if args == ["--atlas-bench"]
        then atlasBench env
        else
          if bench
            then do
              let
                quad x y =
                  [(x, y), (x + 40, y), (x + 40, y + 40), (x + 40, y + 40), (x, y + 40), (x, y)]
                offscreen = concat (replicate 4096 (quad 100 100))
              sparse <- geometry offscreen
              mixed <- geometry (offscreen ++ quad 10 10)
              dense <- geometry (concat (replicate 256 (quad 10 10)))
              forM_
                [ ("sparse-partial", sparse, damage)
                , ("mixed-partial", mixed, damage)
                , ("dense-partial", dense, damage)
                , ("dense-full", dense, DamageFull)
                ]
                $ \(name, dd, dmg) -> do
                  let
                    action = draw dd dmg >> void (renderPresentSafe (sdlRenderer env))
                  replicateM_ 30 action
                  performGC
                  bytes0 <- getAllocationCounter
                  t0 <- getMonotonicTimeNSec
                  replicateM_ iterations action
                  t1 <- getMonotonicTimeNSec
                  bytes1 <- getAllocationCounter
                  printf
                    "%s: %.6f ms/frame | %.1f B/frame\n"
                    (name :: String)
                    (fromIntegral (t1 - t0) / (1e6 * fromIntegral iterations) :: Double)
                    (fromIntegral (bytes0 - bytes1) / fromIntegral iterations :: Double)
            else do
              empty <- geometry []
              -- A six-index group can contain independent triangles, and a command
              -- can end in three indices. Neither may be discarded as an offscreen quad.
              forM_
                [ ("independent triangles", outside ++ visible)
                , ("triangle tail", outside ++ outside ++ visible)
                ]
                $ \(name, points) -> do
                  dd <- geometry points
                  draw empty DamageFull
                  draw dd damage
                  inside <- pixel env 20 20
                  untouched <- pixel env 12 12
                  unless (inside == (255, 0, 0) && untouched == (0, 0, 0)) $
                    fail
                      ( name
                          ++ ": clipped geometry lost or escaped damage: "
                          ++ show (inside, untouched)
                      )
              putStrLn "SDL partial-damage triangle readback: ok"
              atlasChecks env (\tex dd -> drawWithGlyph tex dd DamageFull)
              putStrLn "SDL glyph upload, padding and reset readback: ok"
              imageChecks env ctx images (`draw` DamageFull)
              putStrLn "SDL image atlas upload readback: ok"
