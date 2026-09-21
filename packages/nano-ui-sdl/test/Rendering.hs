{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE PackageImports #-}

module Main (main) where

import Control.Exception (bracket)
import Control.Monad (forM_, replicateM_, unless, void)
import Data.Primitive.PrimArray (primArrayFromList)
import Data.Vector.Unboxed qualified as U
import Data.Word (Word8)
import Foreign.C.Types (CBool (..), CFloat (..), CInt (..), CUInt (..))
import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (advancePtr, allocaArray, peekArray, pokeArray)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (getAllocationCounter)
import NanoUI (Color, Rect (..), colorRGBA)
import NanoUI.Sdl.Internal.Image (destroyImageAtlas, newImageAtlas)
import NanoUI.Sdl.Internal.Render
  ( destroyRenderBatch
  , flushRenderBatch
  , newRenderBatch
  , renderDrawDataPass
  )
import NanoUI.Testing
  ( Damage (..)
  , DrawCmd (..)
  , DrawData (..)
  , Layer (..)
  , glyphAtlasTextureId
  , newPixelContext
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
  atlasTexture :: Ptr () -> IO (Ptr SDL_Texture)

foreign import ccall unsafe "nano_ui_text_atlas_insert_surface"
  insertAtlas ::
    Ptr ()
    -> Ptr ()
    -> Ptr CFloat
    -> Ptr CFloat
    -> Ptr CFloat
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

atlasChecks :: SdlEnv -> (Ptr SDL_Texture -> DrawData -> IO ()) -> IO ()
atlasChecks env draw = withGlyphSurface $ \surface ->
  bracket (newAtlas (sdlRenderer env)) freeAtlas $ \atlas -> allocaArray 4 $ \out -> do
    unless (atlas /= nullPtr) (fail "atlas creation failed")
    ok <-
      insertAtlas
        atlas
        surface
        out
        (advancePtr out 1)
        (advancePtr out 2)
        (advancePtr out 3)
    unless (ok /= 0) (fail "glyph insertion failed")
    bounds <- peekArray 4 out
    unless
      (bounds == [5, 1, 3, 2])
      (fail ("unexpected glyph bounds: " ++ show bounds))
    texture <- atlasTexture atlas
    dd0 <- geometry [(10, 10), (50, 10), (10, 50)]
    let
      dd =
        dd0
          { drawCommands =
              U.map (\cmd -> cmd {cmdTextureId = glyphAtlasTextureId}) (drawCommands dd0)
          }
      sample name x y expected = do
        withForeignPtr (drawVertices dd) $ \p -> forM_ [0 .. 2] $ \i -> do
          pokeByteOff p (i * 32 + 24) (CFloat (x / 2048))
          pokeByteOff p (i * 32 + 28) (CFloat (y / 2048))
        draw texture dd
        actual <- pixel env 20 20
        unless (actual == expected) (fail (name ++ ": " ++ show actual))
    sample "white patch" 0.5 0.5 (255, 0, 0)
    sample "glyph with padded source pitch" 6.5 2.5 (255, 0, 0)
    sample "transparent glyph padding" 8.5 2.5 (0, 0, 0)
    resetAtlas atlas
    sample "white patch after reset" 0.5 0.5 (255, 0, 0)
    sample "old glyph cleared by reset" 6.5 2.5 (0, 0, 0)

atlasBench :: SdlEnv -> IO ()
atlasBench env = withGlyphSurface $ \surface ->
  bracket (newAtlas (sdlRenderer env)) freeAtlas $ \atlas -> allocaArray 4 $ \out -> do
    unless (atlas /= nullPtr) (fail "atlas creation failed")
    let
      fill = replicateM_ 1024 $ do
        ok <-
          insertAtlas
            atlas
            surface
            out
            (advancePtr out 1)
            (advancePtr out 2)
            (advancePtr out 3)
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
          renderDrawDataPass batch (sdlRenderer env) (Just black) dd images tex dmg
          flushRenderBatch batch
        draw = drawWithGlyph nullPtr
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
