{-# LANGUAGE CPP #-}

-- | Minimal smoke-test view for the browser backend: a counter, text input,
-- font-size slider, and checkbox rendered with the bundled Inter font.
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
  label "Text shaped by SDL_ttf, rendered with WebGL2."
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
