-- | Window tests, skipped without a display or an OpenGL 3.2 window. The
-- OpenGL renderer draws into a hidden window and the frame is read back as a
-- screenshot: the window stays opaque even where a translucent window colour
-- is painted twice, and rows run top to bottom. A session window is sized in
-- layout units.
module NanoUI.Rgfw.Window (hasDisplay, testGlWindow, testSessionWindow, testSessionModifiers) where

import Control.Exception (IOException, bracket, try)
import Control.Monad (void)
import Data.ByteString qualified as BS
import Control.Monad.IO.Class (liftIO)
import Data.Either (isRight)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)
import Data.Primitive.SmallArray (smallArrayFromList)
import Foreign.C.Types (CUChar (..))
import Foreign.Ptr (Ptr)
import NanoUI
  ( DrawOp (..), Input (..), Modifiers (..), MouseButton (..), Pressable (..), Size (..), Theme (..), V2 (..), WindowMode (..), WindowSettings (..), WindowState (..), askInput, askWindow
  , defaultWindowSettings, quitUi, requestFrame, rgbaBytes, rgbaHeight, rgbaWidth, setWindowTitleUi, windowSize, colorB, colorG, colorR, colorRGBA, columnWith, drawing, fixedWH, tight
  , tomorrowNightMinDarkTheme, windowColor
  )
import NanoUI.Backend (Damage (..))
import NanoUI.Backend.Rgfw (RgfwOptions (..), defaultRgfwOptions, runRgfwApp)
import NanoUI.Internal.Context (Context (..))
import NanoUI.Rgfw.Internal.Context (newRgfwContext)
import NanoUI.Rgfw.Internal.Font.Cozette (getCozetteFont)
import NanoUI.Rgfw.Internal.Gl (freeGlRenderer, newGlRenderer, readRetainedPixels, readWindowPixels, renderArenaGl, retainedPixels)
import NanoUI.Testing (collectRasterSpans, runFrame)
import NanoUI.Testing.Assert (withInput)
import RGFW qualified as R
import System.Environment (lookupEnv)
import System.Info (os)

-- RGFW's own entry points for a key or button event: they record the key or
-- button and queue the event, as the platform code does.
foreign import ccall "RGFW_getRootWindow" c_RGFW_getRootWindow :: IO (Ptr R.RGFW_window)

foreign import ccall "RGFW_keyCallback"
  c_RGFW_keyCallback :: Ptr R.RGFW_window -> CUChar -> CUChar -> CUChar -> CUChar -> IO ()

foreign import ccall "RGFW_mouseButtonCallback"
  c_RGFW_mouseButtonCallback :: Ptr R.RGFW_window -> CUChar -> CUChar -> IO ()

-- | Whether to try a window: on Linux only with an X display. Elsewhere the
-- tests try one and skip when it does not open.
hasDisplay :: IO Bool
hasDisplay = if os == "linux" then isJust <$> lookupEnv "DISPLAY" else pure True

testGlWindow :: (String -> Bool -> IO ()) -> IO ()
testGlWindow assert =
  hasDisplay >>= \case
    False -> putStrLn "[SKIP] OpenGL window frames: no display"
    True -> bracket (R.createWindowGL "nano-ui-rgfw-test" 0 0 w h R.rgfw_windowHide 3 2) (mapM_ R.closeWindow) $ \case
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
        (_, draw, _) <- runFrame ctx inp view
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
        -- A read smaller than the frame fills only its own buffer.
        corner <- readRetainedPixels renderer 2 1
        full <- readRetainedPixels renderer w h
        assert (name "a smaller read is the bottom-left corner") (corner == BS.take 8 full)
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
-- view quits. Until the pointer comes, it is off the window, not on the
-- widget in the top-left corner.
testSessionWindow :: (String -> Bool -> IO ()) -> IO ()
testSessionWindow assert =
  hasDisplay >>= \case
    False -> putStrLn "[SKIP] RGFW session window: no display"
    True -> do
      seen <- newIORef Nothing
      let view = do
            size <- windowSize
            st <- askWindow
            V2 mx my <- inputMousePos <$> askInput
            setWindowTitleUi "renamed"
            liftIO (writeIORef seen (Just (size, winScale st, mx < -1000 && my < -1000)))
            quitUi
      opened <- try (runRgfwApp defaultRgfwOptions {optScale = 2, optWindow = defaultWindowSettings {wsSize = Size 200 150, wsMode = Hidden}} view)
      readIORef seen >>= \case
        Nothing -> putStrLn "[SKIP] RGFW session window: no OpenGL 3.2 window"
        Just (size, scale, offWindow) -> do
          assert "RGFW session window: its size in layout units at its scale"
            ((size, scale) == (Size 200 150, 2) && isRight (opened :: Either IOException ()))
          assert "RGFW session window: the pointer starts off the window" offWindow

-- | Letting go of one Shift while the other is down leaves Shift held, so a
-- click after it is a Shift-click. The keys come with the modifier bits from
-- before each, as RGFW reads them on X11 and Windows.
testSessionModifiers :: (String -> Bool -> IO ()) -> IO ()
testSessionModifiers assert =
  hasDisplay >>= \case
    False -> putStrLn "[SKIP] RGFW session modifiers: no display"
    True -> do
      frames <- newIORef (0 :: Int)
      clicked <- newIORef Nothing
      let key k m down = c_RGFW_getRootWindow >>= \win -> c_RGFW_keyCallback win (CUChar (fromIntegral k)) (CUChar m) 0 (flag down)
          click down = c_RGFW_getRootWindow >>= \win -> c_RGFW_mouseButtonCallback win (CUChar R.rgfw_mouseLeft) (flag down)
          flag down = if down then 1 else 0 :: CUChar
          view = do
            inp <- askInput
            n <- liftIO (readIORef frames)
            liftIO (writeIORef frames (n + 1))
            case n of
              -- RGFW queues events only once the session has polled.
              0 -> requestFrame
              1 -> do
                liftIO $ do
                  key R.rgfw_keyShiftR 0 True
                  key R.rgfw_keyShiftL R.rgfw_modShift True
                  key R.rgfw_keyShiftL R.rgfw_modShift False
                  click True
                requestFrame
              _
                | heldIn MouseLeft inp || n > 20 -> do
                    liftIO $ do
                      writeIORef clicked (Just (heldIn MouseLeft inp && modShift (inputModifiers inp)))
                      click False
                      key R.rgfw_keyShiftR R.rgfw_modShift False
                    quitUi
                | otherwise -> requestFrame
      _ <- try (runRgfwApp defaultRgfwOptions {optWindow = defaultWindowSettings {wsSize = Size 100 100, wsMode = Hidden}} view) :: IO (Either IOException ())
      readIORef clicked >>= \case
        Nothing -> putStrLn "[SKIP] RGFW session modifiers: no OpenGL 3.2 window"
        Just shiftClick -> assert "RGFW session: one Shift let go while the other is down keeps Shift" shiftClick
