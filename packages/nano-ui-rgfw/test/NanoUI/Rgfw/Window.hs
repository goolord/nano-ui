-- | Window tests, skipped without a display. The OpenGL renderer draws into
-- a hidden window and the frame is read back as a screenshot: the window
-- stays opaque even where a translucent window colour is painted twice, and
-- rows run top to bottom. A session window is sized in layout units.
module NanoUI.Rgfw.Window (testGlWindow, testSessionWindow) where

import Control.Exception (bracket)
import Control.Monad (void)
import Data.ByteString qualified as BS
import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Primitive.SmallArray (smallArrayFromList)
import NanoUI
  ( DrawOp (..), Size (..), Theme (..), WindowMode (..), WindowSettings (..), WindowState (..), askWindow, defaultWindowSettings
  , quitUi, rgbaBytes, rgbaHeight, rgbaWidth, setWindowTitleUi, windowSize, colorB, colorG, colorR, colorRGBA, columnWith, drawing, fixedWH, tight
  , tomorrowNightMinDarkTheme, windowColor
  )
import NanoUI.Backend (Damage (..))
import NanoUI.Backend.Rgfw (RgfwOptions (..), defaultRgfwOptions, runRgfwApp)
import NanoUI.Internal.Context (Context (..))
import NanoUI.Rgfw.Internal.Context (newRgfwContext)
import NanoUI.Rgfw.Internal.Font.Cozette (getCozetteFont)
import NanoUI.Rgfw.Internal.Gl (freeGlRenderer, newGlRenderer, readWindowPixels, renderArenaGl, retainedPixels)
import NanoUI.Testing (collectRasterSpans, runFrame)
import NanoUI.Testing.Assert (withInput)
import RGFW qualified as R
import System.Environment (lookupEnv)

testGlWindow :: (String -> Bool -> IO ()) -> IO ()
testGlWindow assert =
  lookupEnv "DISPLAY" >>= \case
    Nothing -> putStrLn "[SKIP] OpenGL window frames: no display"
    Just _ -> bracket (R.createWindowGL "nano-ui-rgfw-test" 0 0 w h R.rgfw_windowHide 3 2) (mapM_ R.closeWindow) $ \case
      Nothing -> putStrLn "[SKIP] OpenGL window frames: no OpenGL 3.2 window"
      Just _ -> bracket newGlRenderer freeGlRenderer $ \renderer -> do
        let theme = windowColor backdrop tomorrowNightMinDarkTheme
            inp = withInput (fromIntegral w) (fromIntegral h)
            opsBox ops = void (drawing (fixedWH 40 20) (smallArrayFromList . ops))
            -- An opaque box, then the window colour painted twice, as a
            -- page's scroller paints over the backdrop.
            view = columnWith tight (opsBox (\r -> [FillRect r red]) >> opsBox (\r -> [FillRect r backdrop, FillRect r backdrop]))
        ctx <- newRgfwContext theme
        writeIORef (ctxPaintFull ctx) True
        (_, _, draw, _) <- runFrame ctx inp view
        (base, overlay) <- collectRasterSpans ctx inp
        renderArenaGl renderer getCozetteFont 1 w h (themeWindow theme) DamageFull [] draw base overlay
        img <- maybe (fail "OpenGL window: no screenshot") pure =<< retainedPixels renderer w h
        let name s = "OpenGL window: " ++ s
            at x y = [fromIntegral (BS.index (rgbaBytes img) ((y * w + x) * 4 + c)) | c <- [0 .. 3]]
            pixels = [(y, at x y) | y <- [0 .. h - 1], x <- [0 .. w - 1]]
            reds = [y | (y, p) <- pixels, take 3 p == rgb red]
        assert (name "screenshot size") (rgbaWidth img == w && rgbaHeight img == h)
        assert (name "the window colour, opaque") (at (w - 1) (h - 1) == rgb backdrop ++ [255])
        assert (name "the window colour stays opaque, painted twice") (and [p !! 3 == 255 | (_, p) <- pixels, take 3 p == rgb backdrop])
        assert (name "an opaque box") (length reds == 800 && all (< 20) reds)
        -- The present copies the retained frame; read it from the back
        -- buffer before any swap. Compare colour only, since the window's
        -- visual may have no alpha.
        win <- readWindowPixels renderer w h
        let shown x y = [fromIntegral (BS.index win (((h - 1 - y) * w + x) * 4 + c)) | c <- [0 .. 2]]
            near want got = and (zipWith (\a b -> abs (a - b) <= 2) want got)
        assert (name "the present shows the window colour") (near (rgb backdrop) (shown (w - 1) (h - 1)))
        assert (name "the present shows an opaque box") (near (rgb red) (shown 5 5))
  where
    w = 200
    h = 100
    backdrop = colorRGBA 10 20 30 128
    red = colorRGBA 200 30 60 255
    rgb c = map fromIntegral [colorR c, colorG c, colorB c] :: [Int]

-- | A session window opens at its layout-unit size and requested scale (both
-- visible to the view), accepts a title from the view, and closes when the
-- view quits.
testSessionWindow :: (String -> Bool -> IO ()) -> IO ()
testSessionWindow assert =
  lookupEnv "DISPLAY" >>= \case
    Nothing -> putStrLn "[SKIP] RGFW session window: no display"
    Just _ -> do
      seen <- newIORef Nothing
      let view = do
            size <- windowSize
            st <- askWindow
            setWindowTitleUi "renamed"
            liftIO (writeIORef seen (Just (size, winScale st)))
            quitUi
      runRgfwApp defaultRgfwOptions {optScale = 2, optWindow = defaultWindowSettings {wsSize = Size 200 150, wsMode = Hidden}} view
      readIORef seen >>= \case
        Nothing -> putStrLn "[SKIP] RGFW session window: no OpenGL 3.2 window"
        Just got -> assert "RGFW session window: its size in layout units at its scale" (got == (Size 200 150, 2))
