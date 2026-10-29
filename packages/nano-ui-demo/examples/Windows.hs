{-# LANGUAGE OverloadedRecordDot #-}

-- | More windows: 'nativeWindow' opens a native window beside the first,
-- with its own view, while a @Var Bool@ holds True. Call it every frame, as
-- for 'modal'; its close button writes False.
--
-- The windows share state through 'Var's. Type in the note or drag the
-- slider in either window and the other follows on its next frame. Each
-- window keeps its own focus and widget state, and a window can open its
-- own: the inspector opens the palette.
--
-- Run it with @cabal run nano-ui-example-windows@.
module Main (main) where

import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import NanoUI
import NanoUI.Backend.Sdl (SdlOptions (..), defaultSdlOptions, runSdlApp)

data Shared = Shared {note :: !Text, level :: !Float}
  deriving (Eq, Generic)

data App = App
  { shared :: Var Shared
  , inspector :: Var Bool
  , palette :: Var Bool
  , accent :: Var Int
  }

main :: IO ()
main = do
  app <- App <$> newVar (Shared "Edit me in either window" 40) <*> newVar False <*> newVar False <*> newVar 0
  runSdlApp
    defaultSdlOptions {sdlWindowSettings = defaultWindowSettings {wsTitle = "Windows", wsSize = Size 520 300}}
    (mainView app)

mainView :: App -> NanoUI ()
mainView app = accented app $ columnWith (gap 10 . padAll 16 . fillW) $ do
  heading "Main window"
  textInput app.shared.note
  slider 0 100 app.shared.level
  checkbox "Show the inspector" app.inspector
  nativeWindow app.inspector defaultWindowSettings {wsTitle = "Inspector", wsSize = Size 360 260} (inspectorView app)

inspectorView :: App -> NanoUI ()
inspectorView app = accented app $ columnWith (gap 10 . padAll 16 . fillW) $ do
  heading "Inspector"
  Shared {note, level} <- readVar app.shared
  kv "Note" note
  kv "Level" (T.pack (show (round level :: Int)))
  slider 0 100 app.shared.level
  checkbox "Show the palette" app.palette
  nativeWindow app.palette defaultWindowSettings {wsTitle = "Palette", wsSize = Size 240 200} (paletteView app)

paletteView :: App -> NanoUI ()
paletteView app = accented app $ columnWith (gap 8 . padAll 16 . fillW) $ do
  label "Accent for every window"
  forM_ (zip [0 ..] accents) $ \(i, (name, _)) ->
    whenM (button name) (writeVar app.accent i)

-- | The chosen accent, applied in each window's view.
accented :: App -> NanoUI () -> NanoUI ()
accented app body = do
  i <- readVar app.accent
  styled (accentColor (snd (accents !! i))) body

accents :: [(Text, Color)]
accents = [("Blue", colorRGB 0x3b 0x82 0xf6), ("Green", colorRGB 0x22 0xc5 0x5e), ("Orange", colorRGB 0xf9 0x73 0x16)]
