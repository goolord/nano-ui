module NanoUI.Sdl.Text
  ( renderTextSpans
  ) where

import Control.Monad (forM_, void)
import Data.Bits (shiftR, (.&.))
import Data.Char (ord)
import Data.Text (Text)
import Data.Word (Word8)
import Foreign.C.String (withCString)
import SDL3.Sys.Bindgen.Render (SDL_Renderer)
import SDL3.Sys.Bindgen.Runtime.PtrConst qualified as PtrConst
import Foreign.Ptr (Ptr)
import NanoUI (Color (..), Rect (..))
import SDL3.Sys.Render
  ( renderDebugTextSafe
  , setRenderDrawColorSafe
  , setRenderScaleSafe
  )
import qualified Data.Text as T

renderTextSpans :: Ptr SDL_Renderer -> Float -> [(Rect, Text, Color, Color)] -> IO ()
renderTextSpans ren lineHeight spans = do
  let scale = lineHeight / 8
  if null spans
    then pure ()
    else do
      void $ setRenderScaleSafe ren scale scale
      forM_ spans $ \(Rect x y _ _, txt, fg, _bg) -> do
        let ascii = T.filter isDebugChar txt
        if T.null ascii
          then pure ()
          else do
            let (r, g, b, a) = unpackColor fg
            void $ setRenderDrawColorSafe ren r g b a
            withCString (T.unpack ascii) $ \cstr ->
              void $
                renderDebugTextSafe
                  ren
                  (x / scale)
                  (y / scale)
                  (PtrConst.unsafeFromPtr cstr)
      void $ setRenderScaleSafe ren 1 1

isDebugChar :: Char -> Bool
isDebugChar c =
  let o = ord c
   in o >= 32 && o <= 126

unpackColor :: Color -> (Word8, Word8, Word8, Word8)
unpackColor (Color w) =
  ( fromIntegral $ (w `shiftR` 24) .&. 0xFF
  , fromIntegral $ (w `shiftR` 16) .&. 0xFF
  , fromIntegral $ (w `shiftR` 8) .&. 0xFF
  , fromIntegral $ w .&. 0xFF
  )
