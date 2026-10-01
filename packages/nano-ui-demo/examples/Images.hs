-- | Images: show pixels generated in Haskell, fit them to a box, regenerate them, and draw SVG icons.
--
-- An image is RGBA bytes (4 a pixel, rows top to bottom) registered with
-- the backend's atlas. 'useImageRgba' does that as a lease: each image owns
-- an 'ImageHandle', allocated once in setup, and the hook returns the
-- image's id on every frame it is called. The key says which image the
-- handle holds:
--
-- * the same key keeps the image registered, and its pixel argument is
--   never looked at (it is lazy, so it is not even computed);
-- * a changed key registers the new pixels and releases the old image;
-- * a frame that skips the call releases the image and frees its space.
--
-- So regenerating an image means changing its key, as the "Cell size"
-- slider does, and hiding one (the checkbox) gives its atlas space back.
--
-- 'image' stretches an image to its rect. 'imageConfigured' fits it like CSS
-- @object-fit@ ('ContentFit'), and can also crop, fade, rotate and zoom.
--
-- SVG icons come from 'parseSvg' (or 'loadSvg' for a file). A one-colour
-- icon draws in the text colour, so a 'fontColor' tints it, and
-- 'iconButton' puts one in a button.
--
-- Run it with @cabal run nano-ui-example-images@.
module Main (main) where

import Control.Monad (forM_, when)
import Data.Bits (xor)
import qualified Data.ByteString as BS
import Data.Foldable (for_)
import Data.Word (Word8)
import NanoUI
import NanoUI.Backend.Sdl (SdlOptions (..), defaultSdlOptions, runSdlApp)
import qualified Data.Text as T

main :: IO ()
main = do
  images <- newImages
  runSdlApp
    defaultSdlOptions
      { sdlAppShouldQuit = pressedOnceIn KeyEscape
      , sdlWindowSettings = defaultWindowSettings {wsTitle = "Images", wsSize = Size 760 620}
      }
    (view images)

-- | One handle per image the view shows. The type parameter is the key's
-- type: the gradient never changes, the checkerboard is keyed by its cell size.
data Images = Images
  { gradientHandle :: !(ImageHandle ())
  , checkerHandle :: !(ImageHandle Int)
  }

newImages :: IO Images
newImages = Images <$> newImageHandle <*> newImageHandle

-- | Build @w@ by @h@ RGBA bytes from a function of each pixel's position.
rgba :: Int -> Int -> (Int -> Int -> (Word8, Word8, Word8, Word8)) -> BS.ByteString
rgba w h pixel =
  BS.pack [c | y <- [0 .. h - 1], x <- [0 .. w - 1], let (r, g, b, a) = pixel x y, c <- [r, g, b, a]]

-- | 128x64: red grows to the right, green downward.
gradient :: BS.ByteString
gradient = rgba 128 64 $ \x y -> (fromIntegral (x * 2), fromIntegral (y * 4), 170, 255)

-- | 64x64 squares of @cell@ pixels.
checker :: Int -> BS.ByteString
checker cell = rgba 64 64 $ \x y ->
  if odd ((x `div` cell) `xor` (y `div` cell)) then (235, 235, 235, 255) else (60, 64, 72, 255)

-- | A one-colour star: with no fill given, the icon takes the text colour.
star :: Either String Svg
star =
  parseSvg
    "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24'>\
    \<polygon points='12,2 15,9 22,9.5 16.5,14.5 18.5,21.5 12,17.5 5.5,21.5 7.5,14.5 2,9.5 9,9'/>\
    \</svg>"

view :: Images -> NanoUI ()
view images = do
  (cellSize, setCellSize) <- useFloat 8
  (showChecker, setShowChecker) <- useFlag True
  theme <- uiTheme
  scrollWith (padAll 20 . grow) $
    columnWith (tight . gap 14 . fillW) $ do
      heading "Generated pixels"
      grad <- useImageRgba (gradientHandle images) () 128 64 gradient
      -- 'Nothing' means the atlas refused it; the scope keeps later ids
      -- stable either way.
      scope . for_ grad $ \iid -> do
        rowWith (tight . gap 12 . alignMid) $ do
          image (fixedWH 128 64) iid
          muted "image: stretched to its rect (here, its own size)"
        rowWith (wrap . tight . gap 12 . fillW) $
          forM_ [minBound .. maxBound] $ \fit ->
            columnWith (tight . gap 4) $ do
              panelWith (padAll 0) $
                imageConfigured defaultImageConfig {icLayout = fixedWH 72 72, icFit = fit} iid
              muted (T.drop 3 (T.pack (show fit)))

      separator
      heading "Regenerated on change"
      setShowChecker =<< checkbox "Show the checkerboard" showChecker
      setCellSize =<< slider 2 32 cellSize
      let cell = round cellSize :: Int
      -- The key is the cell size: moving the slider to a new whole number
      -- builds new pixels and releases the old image. Unticking the box
      -- skips the hook, which releases the image too.
      scope . when showChecker $ do
        board <- useImageRgba (checkerHandle images) cell 64 64 (checker cell)
        rowWith (tight . gap 12 . alignMid) $ do
          for_ board (image (fixedWH 128 128))
          muted ("64x64, cells of " <> T.pack (show cell) <> " px, drawn at 2x")

      separator
      heading "SVG icons"
      case star of
        Left err -> labelWith (fillW . fontDanger) (T.pack err)
        Right icon -> do
          rowWith (tight . gap 10 . alignMid) $ do
            svgIcon 24 icon
            -- Each colour (and size) rasterises once and is then cached.
            forM_ (themeSeries theme) $ \c -> svgIconWith (fixedWH 32 32 . fontColor c) icon
          rowWith (tight . gap 8 . alignMid) $ do
            _ <- iconButton icon "Favourite"
            _ <- iconButton icon ""
            _ <- styled primary (iconButton icon "Starred")
            pure ()
