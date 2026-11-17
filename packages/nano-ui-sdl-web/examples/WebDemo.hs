{-# LANGUAGE CPP #-}

-- | A page for checking the web backend: a counter, text entry, a slider
-- that sets a font size, and a checkbox, in the bundled Inter.
module Main (main) where

import Data.Text qualified as T
import NanoUI
import NanoUI.Backend.SdlWeb (SdlWebOptions (..), defaultSdlWebOptions, runSdlWebApp)

main :: IO ()
main =
  runSdlWebApp
    defaultSdlWebOptions {webWindowSettings = defaultWindowSettings {wsTitle = "nano-ui on the web"}}
    demo

#if defined(wasm32_HOST_ARCH)
foreign export javascript "nanoMain" main :: IO ()
#endif

demo :: NanoUI ()
demo = columnWith (gap 12 . padAll 24) $ do
  heading "nano-ui-sdl-web"
  label "SDL_ttf shapes this text; WebGL2 draws it."
  row $ do
    n <- useVar (0 :: Int)
    whenM (button "-") (modifyVar n (subtract 1))
    label . T.pack . show =<< readVar n
    whenM (button "+") (modifyVar n (+ 1))
  name <- useVar "world"
  size <- useVar 16
  shout <- useVar False
  textInput name
  slider 10 48 size
  checkbox "Shout" shout
  greeting <- ("Hello, " <>) . (<> "!") <$> readVar name
  loud <- readVar shout
  px <- readVar size
  labelWith (fontSize px) (if loud then T.toUpper greeting else greeting)
