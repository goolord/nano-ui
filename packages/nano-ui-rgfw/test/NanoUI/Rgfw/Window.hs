{-# LANGUAGE DataKinds #-}
-- | Window tests, skipped without a display or an OpenGL 3.2 window.
module NanoUI.Rgfw.Window (hasDisplay, testChildWindow, testGlWindow, testSessionWindow) where

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
  , Rect (..), Shader, ShaderKind (..), Size (..), V2 (..), V4 (..), WindowMode (..), WindowSettings (..)
  , WindowState (..), askInput, askWindow, box, button', canvas, colorB, colorBlack, colorG, colorR, colorRGBA, column
  , columnWith, defaultWindowSettings, drawText, drawing, emptyShaderProgram, fixedWH, inlineText, padAll, pageColor
  , quitUi, rawShader, requestFrame, rgbaBytes, rgbaHeight, rgbaWidth, richText, setWindowTitleUi, shaderBox, strong
  , tight, tomorrowNightMinDarkTheme, windowColor, windowSize, withShader
  )
import NanoUI.Shader (TintParams (..), composeColor, gaussianBlur, grayscale, groupOpacity, tint)
import NanoUI.Backend (Damage (..), ShaderProgram (..))
import NanoUI.Backend.Rgfw (RgfwOptions (..), defaultRgfwOptions, runRgfwApp, runRgfwAppReduce)
import NanoUI qualified as UI
import NanoUI.Emit (NanoUIE, emit)
import GHC.Clock (getMonotonicTime)
import NanoUI.Internal.Context (Context (..))
import NanoUI.Rgfw.Internal.Context (newRgfwContext)
import NanoUI.Rgfw.Internal.Font.Cozette (getCozetteFont)
import NanoUI.Rgfw.Internal.Gl
  (freeGlRenderer, newGlRenderer, readRetainedPixels, readWindowPixels, renderArenaGl, retainedPixels)
import NanoUI.Testing (collectRasterSpans, damageIsEmpty, runFrame, takeDamage, takeDamagePieces)
import NanoUI.Testing.Harness (centerOf, withInputOff)
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
              liftIO (writeIORef btn (centerOf b))
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
        -- Tier 1 surface shader: uniform block layout + UV interpolation + partial repaint.
        let glsl150Src =
              T.unlines
                [ "#version 150"
                , "in vec4 vColor;"
                , "in vec2 vUV;"
                , "layout(std140) uniform NanoShaderBlock {"
                , "  vec4 uRect;"
                , "  vec4 uUvRect;"
                , "  vec4 uMeta;"
                , "  vec4 uTint;"
                , "};"
                , "out vec4 fragColor;"
                , "void main() {"
                , "  vec2 uv = (vUV - uUvRect.xy) / uUvRect.zw;"
                , "  fragColor = vec4(uv.x, uTint.g, uTint.b, 1.0) * vColor;"
                , "}"
                ]
            surfShader :: Shader 'Surface V4
            surfShader = rawShader emptyShaderProgram {spGlsl150 = Just glsl150Src}
        shCtx <- newRgfwContext (pageColor backdrop (windowColor backdrop tomorrowNightMinDarkTheme))
        let shUi u = columnWith tight (shaderBox (fixedWH 40 20) surfShader u)
        _ <- present shCtx True inp (shUi (V4 0 0.5 0.75 1))
        shImg1 <- maybe (fail "OpenGL window: no shader screenshot 1") pure =<< retainedPixels renderer w h
        let pxOf im x y = [fromIntegral (BS.index (rgbaBytes im) ((y * w + x) * 4 + c)) | c <- [0 .. 3]] :: [Int]
            near4 a b = and (zipWith (\u v -> abs (u - v) <= 3) a b)
        -- Left edge x=0 -> uv.x ~ 0.0125 -> R ~ 3; right edge x=39 -> uv.x ~ 0.9875 -> R ~ 252.
        assert (name "surface shader left pixel") (near4 [3, 128, 191, 255] (pxOf shImg1 0 10))
        assert (name "surface shader right pixel") (near4 [252, 128, 191, 255] (pxOf shImg1 39 10))
        assert (name "surface shader outside stays backdrop") (near4 (rgb backdrop ++ [255]) (pxOf shImg1 60 10))
        -- Identical uniform produces zero damage (idle costs nothing).
        drawnIdle <- present shCtx False inp (shUi (V4 0 0.5 0.75 1))
        assert (name "surface shader idle draws nothing") (not drawnIdle)
        -- Updating uniform repaints the shader box partially.
        drawnDirty <- present shCtx False inp (shUi (V4 0 0.25 1.0 1))
        assert (name "surface shader uniform change redraws") drawnDirty
        shImg2 <- maybe (fail "OpenGL window: no shader screenshot 2") pure =<< retainedPixels renderer w h
        assert (name "surface shader updated pixel") (near4 [252, 64, 255, 255] (pxOf shImg2 39 10))

        -- Tier 2 subtree effects on OpenGL 3.2: grayscale, fused ColorEffect,
        -- separable 2-pass gaussianBlur bleed halo, and nested groupOpacity.
        efCtx <- newRgfwContext (pageColor colorBlack (windowColor colorBlack tomorrowNightMinDarkTheme))
        let srcCol = colorRGBA 200 40 20 255
        _ <- present efCtx True inp (columnWith tight (withShader grayscale 1.0 (box (fixedWH 60 40) srcCol)))
        efGray <- maybe (fail "OpenGL window: no grayscale screenshot") pure =<< retainedPixels renderer w h
        -- 0.2126*200 + 0.7152*40 + 0.0722*20 = 72.57 -> ~73
        assert (name "Tier 2 grayscale OpenGL readback") (near4 [73, 73, 73, 255] (pxOf efGray 30 20))

        let fused = composeColor grayscale tint
            fusedU = (1.0, TintParams (colorRGBA 0 80 240 255) 0.5)
        _ <- present efCtx True inp (columnWith tight (withShader fused fusedU (box (fixedWH 60 40) srcCol)))
        efFused <- maybe (fail "OpenGL window: no fused screenshot") pure =<< retainedPixels renderer w h
        -- mix (73, 73, 73) (0, 80, 240) 0.5 = (36, 76, 156)
        assert (name "Tier 2 fused composeColor OpenGL readback") (near4 [36, 76, 156, 255] (pxOf efFused 30 20))

        _ <-
          present efCtx True inp $
            columnWith (padAll 30 . tight) $
              withShader gaussianBlur 4.0 (box (fixedWH 40 40) (colorRGBA 255 0 0 255))
        efBlur <- maybe (fail "OpenGL window: no blur screenshot") pure =<< retainedPixels renderer w h
        let centerOk = case pxOf efBlur 50 50 of
              [cr, cg, cb, _] -> cr > 200 && cg < 10 && cb < 10
              _ -> False
            haloOk = case pxOf efBlur 27 50 of
              [hr, hg, hb, _] -> hr > 15 && hr < 245 && hg < 10 && hb < 10
              _ -> False
            farPx = pxOf efBlur 5 50
        assert (name "Tier 2 gaussianBlur center stays bright red") centerOk
        assert (name "Tier 2 gaussianBlur bleed halo outside box") haloOk
        assert (name "Tier 2 gaussianBlur stays within margin") (farPx == [0, 0, 0, 255])

        _ <-
          present efCtx True inp $
            columnWith tight $
              withShader groupOpacity 0.5 $
                withShader grayscale 1.0 (box (fixedWH 60 40) srcCol)
        efNest <- maybe (fail "OpenGL window: no nested screenshot") pure =<< retainedPixels renderer w h
        assert (name "Tier 2 nested groupOpacity(grayscale) OpenGL readback") (near4 [36, 36, 36, 255] (pxOf efNest 30 20))
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

-- | A reducer session's further window ('UI.nativeWindow'), hidden: it runs
-- its view on its own OpenGL context, a message it emits reaches the model,
-- and it sees the new model through the declaring view's frame, which it
-- closes over. 'quitUi' in it closes it and writes 'False', which the first
-- window shows promptly though nothing else wakes it.
testChildWindow :: (String -> Bool -> IO ()) -> IO ()
testChildWindow assert =
  hasDisplay >>= \case
    False -> putStrLn "[SKIP] RGFW further window: no display"
    True -> do
      open <- UI.newVar True
      passes <- newIORef (0 :: Int)
      childSaw <- newIORef []
      closedAt <- newIORef Nothing
      noticedAt <- newIORef Nothing
      let hidden = defaultWindowSettings {wsSize = Size 200 150, wsMode = Hidden}
          options = defaultRgfwOptions {optScale = 1, optWindow = hidden}
          view :: Int -> NanoUIE () ()
          view m = do
            n <- liftIO (readIORef passes <* modifyIORef' passes (+ 1))
            UI.nativeWindow open hidden {wsTitle = "second"} $ do
              UI.wakeAfter 0.02
              liftIO (modifyIORef' childSaw (m :))
              when (m == 0) (emit ())
              when (m >= 1) $ do
                liftIO (getMonotonicTime >>= writeIORef closedAt . Just)
                quitUi
            stillOpen <- UI.readVar open
            when (not stillOpen) $ liftIO (getMonotonicTime >>= writeIORef noticedAt . Just)
            when (not stillOpen || n > 2000) quitUi
      ended <- try (runRgfwAppReduce options (\() k -> k + 1) (0 :: Int) view)
      saw <- readIORef childSaw
      if null saw && not (isRight (ended :: Either IOException ()))
        then putStrLn "[SKIP] RGFW further window: no OpenGL 3.2 window"
        else do
          assert "RGFW further window: runs its view" (0 `elem` saw)
          assert "RGFW further window: its message reaches the model, which its view sees" (any (>= 1) saw)
          shut <- not <$> UI.readVarIO open
          lag <- (\a b -> (-) <$> b <*> a) <$> readIORef closedAt <*> readIORef noticedAt
          assert "RGFW further window: quitUi in it closes it, which the first window shows" (shut && maybe False (< 1) lag)
