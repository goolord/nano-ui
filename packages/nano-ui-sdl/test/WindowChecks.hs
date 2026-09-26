{-# LANGUAGE PackageImports #-}

-- | Window options as SDL reports them back, screenshots read back from
-- sessions that draw retained and straight to the window, and the alpha a
-- transparent window keeps.
module WindowChecks (windowChecks) where

import Control.Monad (unless, void, when)
import Data.Bits (zeroBits, (.&.))
import Data.ByteString qualified as BS
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (fromJust)
import Data.Primitive.SmallArray (smallArrayFromList)
import Foreign.C.String (peekCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (castPtr)
import Foreign.Storable (peek)
import NanoUI
import NanoUI.Backend (emptyInput)
import NanoUI.Testing (Context, isDirty, newPixelContext)
import SDL3.Sys.Blendmode qualified as Blend
import SDL3.Sys.Bindgen.Runtime.PtrConst qualified as PtrConst
import SDL3.Sys.Video (getWindowFlags, getWindowMaximumSize, getWindowMinimumSize, getWindowOpacity, getWindowSize, getWindowTitle)
import SDL3.Sys.Video qualified as SDL
import "nano-ui-sdl" NanoUI.Backend.Sdl

-- | The checks on the drivers named; @gpu@ says they are a display's, with a
-- GPU renderer, which have what the dummy driver lacks: icons, opacity, the
-- position, and custom blend modes for a transparent window.
windowChecks :: String -> Bool -> IO ()
windowChecks drivers gpu = do
  mapM_ (optionChecks gpu) [1, 2]
  mapM_ screenshotChecks [False, True]
  transparencyChecks gpu
  putStrLn ("SDL window options, screenshots and transparency (" ++ drivers ++ "): ok")

-- | A small hidden window with the bundled font at scale 1, so a layout
-- unit is a pixel.
options :: SdlOptions
options =
  defaultSdlOptions
    { sdlWindowSettings = defaultWindowSettings {wsMode = Hidden, wsSize = Size 200 100}
    , sdlAppFont = DefaultFont, sdlAppMonoFont = DefaultFont, sdlAppVsync = False, sdlAppUiScale = 1
    }

-- | @options@ with other window settings.
withWindow :: (WindowSettings -> WindowSettings) -> SdlOptions
withWindow f = options {sdlWindowSettings = f (sdlWindowSettings options)}

-- | A session whose theme has a window colour of @backdrop@.
session :: SdlOptions -> Color -> (Context -> SdlEnv -> IO a) -> IO a
session opts backdrop act = do
  ctx <- newPixelContext >>= (`withTheme` windowColor backdrop defaultTheme)
  withSdl opts ctx act

-- | Draw one frame of a view; @full@ forces a full repaint. Returns what the
-- view did.
frame :: Context -> SdlEnv -> Bool -> NanoUI a -> IO a
frame ctx env full ui = do
  (ctx', inp) <- syncDisplay ctx env emptyInput {inputWindowSize = Size 200 100}
  out <- newIORef Nothing
  void (sdlDrawFrame ctx' (ui >>= uiIO . writeIORef out . Just) env inp full)
  maybe (fail "the view did not run") pure =<< readIORef out

-- | Draw a frame of a view that asks for a screenshot, and the one screenshot
-- it is answered with.
shoot :: String -> Context -> SdlEnv -> Bool -> NanoUI () -> IO Screenshot
shoot name ctx env full ui = do
  shots <- newIORef []
  frame ctx env full (requestScreenshot (\s -> modifyIORef' shots (s :)) >> ui)
  readIORef shots >>= \case
    [Just img] -> pure img
    other -> fail (name ++ ": one screenshot wanted, got " ++ show (length other))

check :: String -> Bool -> IO ()
check name ok = unless ok (fail name)

expect :: (Eq a, Show a) => String -> a -> a -> IO ()
expect name want got = unless (want == got) (fail (name ++ ": wanted " ++ show want ++ ", got " ++ show got))

-- | Size limits at creation and from a view, in layout units: at a zoom of 2
-- each is twice as many window coordinates. The title, the scale views read,
-- a resize and the mode. On a display, the opacity, the icon and the
-- position too.
optionChecks :: Bool -> Int -> IO ()
optionChecks gpu zoom =
  session
    (withWindow (\w -> w {wsMinSize = Just (Size 150 80), wsMaxSize = Just (Size 900 700), wsOpacity = 0.5, wsPosition = WindowPositionAt 30 20}))
      { sdlAppUiScale = fromIntegral zoom }
    (colorRGBA 0 0 0 255)
    $ \ctx env -> do
      let after :: (Eq a, Show a) => String -> IO () -> IO a -> a -> IO ()
          after name act get want = act >> get >>= expect (name ++ " at zoom " ++ show zoom) want
          ui v = frame ctx env True (v >> label "window")
          pair get = alloca $ \pw -> alloca $ \ph -> do
            get (sdlWindow env) pw ph >>= check "SDL did not report a size of the window"
            (,) <$> (fromIntegral <$> peek pw) <*> (fromIntegral <$> peek ph)
          zoomed (w, h) = (zoom * w, zoom * h)
          minimal name act = after name act (pair getWindowMinimumSize) . zoomed
          maximal name act = after name act (pair getWindowMaximumSize) . zoomed
          flags = getWindowFlags (sdlWindow env)
          hidden = (\f -> f .&. SDL.SDL_WINDOW_HIDDEN /= zeroBits) <$> flags
          title = getWindowTitle (sdlWindow env) >>= peekCString . castPtr . PtrConst.unsafeToPtr
          askedWindow = frame ctx env True askWindow
      minimal "minimum size at creation" (pure ()) (150, 80)
      maximal "maximum size at creation" (pure ()) (900, 700)
      minimal "minimum size from a view" (ui (setWindowMinSizeUi (Just (Size 170 90)))) (170, 90)
      minimal "minimum size taken off from a view" (ui (setWindowMinSizeUi Nothing)) (0, 0)
      maximal "maximum width alone from a view" (ui (setWindowMaxSizeUi (Just (Size 600 0)))) (600, 0)
      after "the title from a view" (ui (setWindowTitleUi "renamed")) title "renamed"
      after "the scale a view reads" (pure ()) (winScale <$> askedWindow) (fromIntegral zoom)
      -- X11 resizes a window when the server gets to it.
      after "a resize from a view" (ui (resizeWindowUi (Size 300 150))) (SDL.syncWindowSafe (sdlWindow env) >> pair getWindowSize) (zoomed (300, 150))
      after "a hidden window" (pure ()) hidden True
      after "shown from a view" (ui (setWindowModeUi Windowed)) hidden False
      after "hidden from a view" (ui (setWindowModeUi Hidden)) hidden True
      when gpu $ do
        let opacity = round . (* 100) <$> getWindowOpacity (sdlWindow env) :: IO Int
            moved name act at = after name act (winPosition <$> askedWindow) (Just at)
        after "the opacity at creation" (pure ()) opacity 50
        after "the opacity from a view" (ui (setWindowOpacityUi 0.75)) opacity 75
        ui (setWindowIconUi (fromJust (rgbaPixels 2 2 (BS.pack (concat (replicate 4 [200, 30, 60, 255]))))))
        moved "the position at creation" (pure ()) (30, 20)
        moved "the position from a view" (ui (moveWindowUi 60 70)) (60, 70)

red :: Color
red = colorRGBA 200 30 60 255

-- | The RGBA bytes of a pixel of a screenshot.
pixelAt :: Screenshot -> Int -> Int -> [Int]
pixelAt shot x y =
  [fromIntegral (BS.index (rgbaBytes img) ((y * rgbaWidth img + x) * 4 + c)) | c <- [0 .. 3]]
  where
    img = screenshotPixels shot

-- | The alphas of the pixels whose red, green and blue are a colour's.
alphasOf :: Color -> Screenshot -> [Int]
alphasOf c shot =
  [p !! 3 | y <- [0 .. rgbaHeight img - 1], x <- [0 .. rgbaWidth img - 1], let p = pixelAt shot x y, take 3 p == rgb]
  where
    img = screenshotPixels shot
    rgb = map fromIntegral [colorR c, colorG c, colorB c]

-- | A 40 x 20 box of a colour, versioned by it so that a new colour
-- repaints it.
solidBox :: Color -> NanoUI ()
solidBox c = void (drawingVersioned (fromIntegral (colorToWord32 c)) (fixedWH 40 20) (\r -> smallArrayFromList [FillRect r c]))

-- | A view's screenshot is answered once its frame is drawn, with its pixels
-- (a skipped frame's with the frame on screen), and the answer asks for a frame
-- after. A continuous session's frame is read before it is presented.
screenshotChecks :: Bool -> IO ()
screenshotChecks continuous =
  session options {sdlAppContinuous = continuous} (colorRGBA 12 34 56 255) $ \ctx env -> do
    let name s = (if continuous then "continuous: " else "retained: ") ++ s
    frame ctx env True (solidBox (colorRGBA 0 0 255 255))
    img <- shoot (name "a frame that repaints the box") ctx env False (solidBox red)
    expect (name "screenshot size") (200, 100, 1) (rgbaWidth (screenshotPixels img), rgbaHeight (screenshotPixels img), screenshotScale img)
    expect (name "the box's pixels") (replicate 800 255) (alphasOf red img)
    expect (name "the window's colour") [12, 34, 56, 255] (pixelAt img 199 99)
    isDirty ctx >>= check (name "an answered screenshot asks for another frame")
    unless continuous $ do
      captureScreenshot env >>= check (name "captureScreenshot reads the same frame") . (== Just img)
      shoot (name "a skipped frame") ctx env False (solidBox red) >>= check (name "a skipped frame's screenshot") . (== img)

-- | A transparent window's frames keep the window colour's alpha, a partial
-- repaint comes out as a full one would, and a GPU renderer keeps the alpha
-- where the window colour is painted over itself.
transparencyChecks :: Bool -> IO ()
transparencyChecks gpu =
  session (withWindow (\w -> w {wsTransparent = True})) backdrop $ \ctx env -> do
    -- The software renderer has no custom blend modes and draws as for an
    -- opaque window.
    let exact = maybe False ((/= Blend.SDL_BLENDMODE_BLEND) . fst) (sdlTransparent env)
        -- An opaque box, a translucent one of a tint, and the window colour
        -- twice over, as a page's scroller paints it over the backdrop.
        shot full tint = shoot "transparent" ctx env full . column $ do
          solidBox red
          solidBox tint
          when exact $ void (drawing (fixedWH 60 20) (\r -> smallArrayFromList [FillRect r backdrop, FillRect r backdrop]))
        keepsAlpha img = all (== 128) (alphasOf backdrop img)
    check "a transparent window has blend modes" (sdlTransparent env /= Nothing)
    when gpu $ check "a GPU renderer draws a transparent window with custom blend modes" exact
    first <- shot True (colorRGBA 0 200 0 100)
    expect "transparent: the window's colour" [10, 20, 30, 128] (pixelAt first 199 99)
    check "transparent: the backdrop keeps its alpha" (keepsAlpha first)
    expect "transparent: an opaque box stays opaque" (replicate 800 255) (alphasOf red first)
    -- Repainting the translucent box alone must not blend the backdrop under
    -- it over the old box or the old backdrop.
    second <- shot False (colorRGBA 0 0 200 100)
    check "transparent: the change was drawn" (second /= first)
    shot True (colorRGBA 0 0 200 100) >>= check "transparent: a change comes out as a full repaint does" . (== second)
    check "transparent: the backdrop keeps its alpha after a change" (keepsAlpha second)
  where
    backdrop = colorRGBA 10 20 30 128
