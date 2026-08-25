module NanoUI.Sdl.Render
  ( renderDrawData
  ) where

import Control.Monad (void, when)
import Data.Bits (shiftR, (.&.))
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Word (Word32, Word8)
import Foreign.C.Types (CFloat, CInt)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable (..), peekByteOff, poke)
import SDL3.Sys.Bindgen.Runtime.PtrConst qualified as PtrConst
import NanoUI
  ( Color (..)
  , DrawCmd (..)
  , DrawData (..)
  , Layer (..)
  , indexSize
  , vertexSize
  )
import SDL3.Sys.Bindgen.Rect (SDL_FRect (..), SDL_Rect (..))
import SDL3.Sys.Bindgen.Render (SDL_Renderer)
import SDL3.Sys.Render
  ( renderClearSafe
  , renderFillRectSafe
  , setRenderClipRectSafe
  , setRenderDrawColorSafe
  )

nullClip :: PtrConst.PtrConst SDL_Rect
nullClip = PtrConst.unsafeFromPtr (nullPtr :: Ptr SDL_Rect)

renderDrawData :: Ptr SDL_Renderer -> Color -> DrawData -> IO ()
renderDrawData ren clearColor drawData = do
  void $ setRenderClipRectSafe ren nullClip
  let (cr, cg, cb, ca) = unpackColor clearColor
  void $ setRenderDrawColorSafe ren cr cg cb ca
  void $ renderClearSafe ren
  mapM_ (drawCmd ren drawData) (sortBy (comparing layerOrder) (drawCommands drawData))
  void $ setRenderClipRectSafe ren nullClip

layerOrder :: DrawCmd -> Int
layerOrder cmd =
  case cmdLayer cmd of
    LayerBackground -> 0
    LayerContent -> 1
    LayerOverlay -> 2

drawCmd :: Ptr SDL_Renderer -> DrawData -> DrawCmd -> IO ()
drawCmd ren dd cmd = do
  let count = fromIntegral (cmdIndexCount cmd)
  when (count > 0 && count `mod` 6 == 0) $ do
    setCmdClip ren cmd
    let start = fromIntegral (cmdIndexOffset cmd)
    mapM_ (fillQuad ren dd) [start, start + 6 .. start + count - 1]

setCmdClip :: Ptr SDL_Renderer -> DrawCmd -> IO ()
setCmdClip ren cmd
  | cmdClipW cmd >= 1e8 || cmdClipH cmd >= 1e8 =
      void $ setRenderClipRectSafe ren nullClip
  | otherwise =
      alloca $ \(r :: Ptr SDL_Rect) -> do
        poke
          r
          SDL_Rect
            { x = ci (round (cmdClipX cmd))
            , y = ci (round (cmdClipY cmd))
            , w = ci (max (0 :: Int) (round (cmdClipW cmd)))
            , h = ci (max (0 :: Int) (round (cmdClipH cmd)))
            }
        void $ setRenderClipRectSafe ren (PtrConst.unsafeFromPtr r)

fillQuad :: Ptr SDL_Renderer -> DrawData -> Int -> IO ()
fillQuad ren dd i = do
  v0 <- vertexAt dd i
  v1 <- vertexAt dd (i + 1)
  v2 <- vertexAt dd (i + 2)
  v3 <- vertexAt dd (i + 5)
  case (v0, v1, v2, v3) of
    (Just (x0, y0, rgba), Just (x1, y1, _), Just (x2, y2, _), Just (x3, y3, _)) -> do
      let xs = [x0, x1, x2, x3]
          ys = [y0, y1, y2, y3]
          xmin = minimum xs
          xmax = maximum xs
          ymin = minimum ys
          ymax = maximum ys
          w = xmax - xmin
          h = ymax - ymin
      when (w > 0 && h > 0) $ do
        let (r, g, b, a) = unpackColor (Color rgba)
        void $ setRenderDrawColorSafe ren r g b a
        alloca $ \(rect :: Ptr SDL_FRect) -> do
          poke
            rect
            SDL_FRect
              { x = cf xmin
              , y = cf ymin
              , w = cf w
              , h = cf h
              }
          void $ renderFillRectSafe ren (PtrConst.unsafeFromPtr rect)
    _ -> pure ()

cf :: Float -> CFloat
cf = realToFrac

ci :: Int -> CInt
ci = fromIntegral

vertexAt :: DrawData -> Int -> IO (Maybe (Float, Float, Word32))
vertexAt dd slot = do
  mVi <- readIndexAt dd slot
  case mVi of
    Nothing -> pure Nothing
    Just vi
      | vi < 0 || vi >= drawVertexCount dd -> pure Nothing
      | otherwise -> do
          x <- peekFloatAt (drawVertices dd) (vi * vertexSize)
          y <- peekFloatAt (drawVertices dd) (vi * vertexSize + 4)
          rgba <- peekWord32At (drawVertices dd) (vi * vertexSize + 16)
          pure (Just (x, y, rgba))

readIndexAt :: DrawData -> Int -> IO (Maybe Int)
readIndexAt dd i
  | i < 0 || i >= drawIndexCount dd = pure Nothing
  | otherwise =
      Just . fromIntegral <$> peekWord32At (drawIndices dd) (i * indexSize)

peekWord32At :: ForeignPtr Word8 -> Int -> IO Word32
peekWord32At fp off = withForeignPtr fp $ \p -> peekByteOff p off

peekFloatAt :: ForeignPtr Word8 -> Int -> IO Float
peekFloatAt fp off = withForeignPtr fp $ \p -> peekByteOff p off

unpackColor :: Color -> (Word8, Word8, Word8, Word8)
unpackColor (Color w) =
  ( fromIntegral $ (w `shiftR` 24) .&. 0xFF
  , fromIntegral $ (w `shiftR` 16) .&. 0xFF
  , fromIntegral $ (w `shiftR` 8) .&. 0xFF
  , fromIntegral $ w .&. 0xFF
  )
