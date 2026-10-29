-- | Window tests, skipped without a display or an OpenGL 3.2 window.
module NanoUI.Rgfw.Window (hasDisplay, testGlWindow, testSessionWindow) where

import Control.Exception (IOException, bracket, try)
import Control.Monad (forM, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as BS
import Data.Either (isRight)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)
import Data.Primitive.SmallArray (smallArrayFromList)
import Data.Text qualified as T
import Foreign.C.Types (CUChar (..))
import Foreign.Ptr (Ptr)
import NanoUI
  ( AlignX (..), AlignY (..), DrawOp (..), Input (..), Modifiers (..), MouseButton (..), NanoUI, Pressable (..)
  , Rect (..), Size (..), V2 (..), WindowMode (..), WindowSettings (..), WindowState (..), askInput, askWindow
  , button', canvas, colorB, colorG, colorR, colorRGBA, column, columnWith, defaultWindowSettings, drawText, drawing
  , fixedWH, inlineText, pageColor, quitUi, requestFrame, respRect, rgbaBytes, rgbaHeight, rgbaWidth, richText
  , setWindowTitleUi, strong, tight, tomorrowNightMinDarkTheme, windowColor, windowSize
  )
import NanoUI.Backend (Damage (..))
import NanoUI.Backend.Rgfw (RgfwOptions (..), defaultRgfwOptions, runRgfwApp)
import NanoUI.Internal.Context (Context (..))
import NanoUI.Rgfw.Internal.Context (newRgfwContext)
import NanoUI.Rgfw.Internal.Font.Cozette (getCozetteFont)
import NanoUI.Rgfw.Internal.Gl
  (freeGlRenderer, newGlRenderer, readRetainedPixels, readWindowPixels, renderArenaGl, retainedPixels)
import NanoUI.Testing (collectRasterSpans, damageIsEmpty, runFrame, takeDamage, takeDamagePieces)
import NanoUI.Testing.Harness (withInputOff)
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

-- | The OpenGL renderer draws into a hidden window, read back as a
-- screenshot. The window stays opaque even where a translucent page colour
-- is painted twice, rows run top to bottom, and text in drawings repaints
-- with them: settled through partial frames while the pointer hovers a
-- button and a counter changes, a page has the pixels a full repaint draws.
testGlWindow :: (String -> Bool -> IO ()) -> IO ()
testGlWindow assert =
  hasDisplay >>= \case
    False -> putStrLn "[SKIP] OpenGL window: no display"
    True -> bracket (R.createWindowGL "nano-ui-rgfw-test" 0 0 w h R.rgfw_windowHide 3 2) (mapM_ R.closeWindow) $ \case
      Nothing -> putStrLn "[SKIP] OpenGL window: no OpenGL 3.2 window"
      Just _ -> bracket newGlRenderer freeGlRenderer $ \renderer -> do
        let inp = withInputOff (fromIntegral w) (fromIntegral h)
            -- As the session draws a frame: a forced one in full, any other
            -- only its damage. Returns whether it drew.
            present ctx force frameInp ui = do
              writeIORef (ctxPaintFull ctx) force
              (_, draw, _) <- runFrame ctx frameInp ui
              damage <- takeDamage ctx
              let drawn = force || not (damageIsEmpty damage)
              when drawn $ do
                (base, overlay) <- collectRasterSpans ctx frameInp
                pieces <- if force then pure [] else takeDamagePieces ctx
                let whole = if force then DamageFull else damage
                renderArenaGl renderer getCozetteFont 1 w h whole pieces draw base overlay
              pure drawn
            name s = "OpenGL window: " ++ s
        -- An opaque box, then the page colour painted twice, as a page's
        -- scroller paints over the backdrop.
        boxes <- newRgfwContext (pageColor backdrop (windowColor backdrop tomorrowNightMinDarkTheme))
        let opsBox ops = void (drawing (fixedWH 40 20) (smallArrayFromList . ops))
        _ <- present boxes True inp . columnWith tight $ do
          opsBox (\r -> [FillRect r red])
          opsBox (\r -> [FillRect r backdrop, FillRect r backdrop])
        img <- maybe (fail "OpenGL window: no screenshot") pure =<< retainedPixels renderer w h
        let at x y = [fromIntegral (BS.index (rgbaBytes img) ((y * w + x) * 4 + c)) | c <- [0 .. 3]]
            pixels = [(y, at x y) | y <- [0 .. h - 1], x <- [0 .. w - 1]]
            reds = [y | (y, p) <- pixels, take 3 p == rgb red]
        assert (name "screenshot size") (rgbaWidth img == w && rgbaHeight img == h)
        assert (name "the page colour, opaque") (at (w - 1) (h - 1) == rgb backdrop ++ [255])
        assert (name "the page colour stays opaque, painted twice") $
          and [p !! 3 == 255 | (_, p) <- pixels, take 3 p == rgb backdrop]
        assert (name "an opaque box") (length reds == 800 && all (< 20) reds)
        -- A read smaller than the frame fills only its own buffer.
        corner <- readRetainedPixels renderer 2 1
        full <- readRetainedPixels renderer w h
        assert (name "a smaller read is the bottom-left corner") (corner == BS.take 8 full)
        -- The present copies the retained frame; read it from the back buffer
        -- before any swap, colour only, as the visual may have no alpha.
        win <- readWindowPixels renderer w h
        let shown x y = [fromIntegral (BS.index win (((h - 1 - y) * w + x) * 4 + c)) | c <- [0 .. 2]]
            near want got = and (zipWith (\a b -> abs (a - b) <= 2) want got)
        assert (name "the present shows the page colour") (near (rgb backdrop) (shown (w - 1) (h - 1)))
        assert (name "the present shows an opaque box") (near (rgb red) (shown 5 5))
        ctx <- newRgfwContext tomorrowNightMinDarkTheme
        btn <- newIORef (V2 0 0)
        let view k = column $ do
              b <- button' "hover"
              liftIO (writeIORef btn (let Rect x y bw bh = respRect b in V2 (x + bw / 2) (y + bh / 2)))
              let counter = T.pack (" at " ++ show k)
              void $ richText [inlineText "rich text beside ", strong "a counter", inlineText counter]
              void $ canvas (fixedWH 160 20) $ \(Rect x y _ _) ->
                drawText (V2 x y) AlignStart AlignTop (T.pack ("canvas " ++ show k)) ink
            settle n frameInp ui = do
              drawn <- present ctx False frameInp {inputDeltaTime = 0.016} ui
              when (drawn && n < (200 :: Int)) (settle (n + 1) frameInp ui)
            -- Partial frames until nothing changes, then the same state in full.
            same frameInp ui = do
              settle 0 frameInp ui
              partial <- readRetainedPixels renderer w h
              _ <- present ctx True frameInp ui
              (partial ==) <$> readRetainedPixels renderer w h
        _ <- present ctx True inp (view 0)
        over <- readIORef btn
        let hovered = inp {inputMousePos = over}
        results <- forM [(1, hovered), (2, hovered), (3, inp), (4 :: Int, hovered)] $ \(k, i) -> same i (view k)
        texts <- readRetainedPixels renderer w h
        let isInk i = map (BS.index texts . (i +)) [0 .. 2] == [colorR ink, colorG ink, colorB ink]
            inked = filter isInk [0, 4 .. BS.length texts - 4]
        assert (name "partial repaints of drawing text match full ones") (and results)
        assert (name "the canvas text is drawn") (not (null inked))
  where
    w = 240
    h = 120
    backdrop = colorRGBA 10 20 30 128
    red = colorRGBA 200 30 60 255
    ink = colorRGBA 0x12 0xEE 0x34 255
    rgb c = map fromIntegral [colorR c, colorG c, colorB c] :: [Int]

-- | A session window opens at its layout-unit size and requested scale (both
-- visible to the view), takes a title from the view, and closes when the
-- view quits. Until the pointer comes, it is off the window, not on the
-- widget in the top-left corner. Letting go of one Shift while the other is
-- down leaves Shift held, so a click after it is a Shift-click; the keys
-- come with the modifier bits from before each, as RGFW reads them on X11
-- and Windows.
testSessionWindow :: (String -> Bool -> IO ()) -> IO ()
testSessionWindow assert =
  hasDisplay >>= \case
    False -> putStrLn "[SKIP] RGFW session window: no display"
    True -> do
      frames <- newIORef (0 :: Int)
      opened <- newIORef Nothing
      clicked <- newIORef Nothing
      let root f = c_RGFW_getRootWindow >>= f
          key k m down = root (\win -> c_RGFW_keyCallback win (CUChar (fromIntegral k)) (CUChar m) 0 (flag down))
          click down = root (\win -> c_RGFW_mouseButtonCallback win (CUChar R.rgfw_mouseLeft) (flag down))
          flag down = if down then 1 else 0 :: CUChar
          view :: NanoUI ()
          view = do
            inp <- askInput
            n <- liftIO (readIORef frames <* modifyIORef' frames (+ 1))
            case n of
              -- RGFW queues events only once the session has polled.
              0 -> do
                size <- windowSize
                st <- askWindow
                let V2 mx my = inputMousePos inp
                setWindowTitleUi "renamed"
                liftIO (writeIORef opened (Just (size, winScale st, mx < -1000 && my < -1000)))
                requestFrame
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
          hidden = defaultWindowSettings {wsSize = Size 200 150, wsMode = Hidden}
          options = defaultRgfwOptions {optScale = 2, optWindow = hidden}
      ended <- try (runRgfwApp options view)
      readIORef opened >>= \case
        Nothing -> putStrLn "[SKIP] RGFW session window: no OpenGL 3.2 window"
        Just (size, scale, offWindow) -> do
          shiftClick <- readIORef clicked
          assert "RGFW session window: its size in layout units at its scale"
            ((size, scale) == (Size 200 150, 2) && isRight (ended :: Either IOException ()))
          assert "RGFW session window: the pointer starts off the window" offWindow
          assert "RGFW session: one Shift let go while the other is down keeps Shift" (shiftClick == Just True)
