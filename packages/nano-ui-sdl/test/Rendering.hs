{-# LANGUAGE PackageImports #-}

module Main (main) where

import Control.Exception (bracket)
import Control.Monad (forM_, replicateM_, unless, void)
import Data.Primitive.PrimArray (primArrayFromList)
import Data.Vector.Unboxed qualified as U
import Data.Word (Word8)
import Foreign.C.Types (CBool (..), CFloat (..), CInt (..))
import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (pokeArray)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (peekByteOff)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (getAllocationCounter)
import NanoUI (Color, Rect (..), colorRGBA)
import NanoUI.Sdl.Image (destroyImageAtlas, newImageAtlas)
import NanoUI.Sdl.Render
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
  , newPixelContext
  )
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
    bench = args == ["--bench"]
  unless bench $ do
    setEnv "SDL_VIDEODRIVER" "dummy"
    setEnv "SDL_RENDER_DRIVER" "software"
  ctx <- newPixelContext
  withSdlBench ctx $ \_ env -> bracket newImageAtlas destroyImageAtlas $ \images ->
    bracket (newRenderBatch (sdlRenderer env)) destroyRenderBatch $ \batch -> do
      let
        draw dd dmg = do
          renderDrawDataPass batch (sdlRenderer env) (Just black) dd images nullPtr dmg
          flushRenderBatch batch
        visible = [(10, 10), (50, 10), (10, 50)]
        outside = [(100, 100), (120, 100), (100, 120)]
        damage = DamageClip (Rect 15 15 10 10)
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
