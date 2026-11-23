-- | The widget cookbook ("Demo") in an SDL window. Run
-- @cabal run nano-ui-sdl-demo -- --help@ for the options. @--record DIR@
-- records the README video's frames instead (see "SdlRecord").
module Main (main) where

import NanoUI
import NanoUI.Backend.Sdl
import System.Console.GetOpt (ArgDescr (..), ArgOrder (Permute), OptDescr (..), getOpt, usageInfo)
import System.Environment (getArgs)
import Text.Read (readMaybe)

import Demo (demoUi, newDemo)
import SdlRecord qualified

main :: IO ()
main = do
  args <- getArgs
  case dropWhile (/= "--record") args of
    _ : dir : _ -> SdlRecord.record dir . demoUi =<< newDemo
    _ -> case getOpt Permute options args of
      (updates, [], []) | Just fs <- sequence updates -> runSdlApp (foldl' (flip id) demoOptions fs) . demoUi =<< newDemo
      (_, _, errors) -> putStr (concat errors <> usageInfo usage options)
  where
    usage = "Usage: nano-ui-sdl-demo [OPTIONS]\n       nano-ui-sdl-demo --record DIR  (record the README video's frames)"

demoOptions :: SdlOptions
demoOptions =
  defaultSdlOptions
    { sdlAppShouldQuit = pressedOnceIn KeyEscape
    , sdlAppTheme = Just defaultTheme
    , sdlWindowSettings = defaultWindowSettings {wsSize = Size 1280 800, wsMinSize = Just (Size 480 360)}
    }

-- | Each option's change to 'demoOptions'; 'Nothing' asks for the help text,
-- as does a size that is not a number.
options :: [OptDescr (Maybe (SdlOptions -> SdlOptions))]
options =
  [ Option "v" ["vsync"] (ReqArg (\s -> Just (\o -> o {sdlAppVsync = s `elem` ["true", "True", "1"]})) "BOOL")
      "Turn vsync on or off (default: true)"
  , Option "cbf" ["continuous", "benchmark", "fps"] (NoArg (Just (\o -> o {sdlAppContinuous = True, sdlAppVsync = False})))
      "Render continuously without vsync, to show uncapped FPS"
  , Option "" ["explain"] (NoArg (Just (\o -> o {sdlExplainLayout = True})))
      "Start with the layout overlay on"
  , Option "F" ["fullscreen"] (NoArg (Just (onWindow (\w -> w {wsMode = Fullscreen}))))
      "Start fullscreen"
  , Option "W" ["width"] (ReqArg (resize (\px sz -> sz {sizeW = px})) "PX") "Initial window width (default: 1280)"
  , Option "H" ["height"] (ReqArg (resize (\px sz -> sz {sizeH = px})) "PX") "Initial window height (default: 800)"
  , Option "h?" ["help"] (NoArg Nothing) "Show this help"
  ]
  where
    onWindow f o = o {sdlWindowSettings = f (sdlWindowSettings o)}
    resize f s = (\px -> onWindow (\w -> w {wsSize = f px (wsSize w)})) <$> readMaybe s
