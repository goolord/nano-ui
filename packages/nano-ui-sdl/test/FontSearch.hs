-- | Installed-font discovery over empty fixture files in a fake home
-- directory: discovery reads only file names, so no display or font data.
module Main (main) where

import Control.Exception (bracket)
import Control.Monad (unless)
import Data.Maybe (isJust)
import NanoUI.Sdl.Internal.Font.Search (boldFaceCandidates, listFontFamilies, searchFonts)
import System.Directory
  (createDirectory, createDirectoryIfMissing, getTemporaryDirectory, removeFile, removePathForcibly)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath (takeDirectory, (</>))
import System.IO (hClose, openTempFile)
import System.Info (os)

main :: IO ()
main = bracket temporaryRoot removePathForcibly $ \root ->
  bracket (lookupEnv homeVar) (maybe (unsetEnv homeVar) (setEnv homeVar)) $ \_ -> do
    setEnv homeVar root
    let fonts =
          root </> case os of
            "mingw32" -> "Microsoft" </> "Windows" </> "Fonts"
            "darwin" -> "Library" </> "Fonts"
            _ -> ".local" </> "share" </> "fonts"
        font = (fonts </>)
        found = Just . font
        -- Only the Linux search reads the XDG data directories.
        xdg = root </> "xdg" </> "fonts" </> "NanoXdgFixture.ttf"
        searches =
          [ ("regular face", ["Nano Search Fixture"], found "NanoSearchFixture-Regular.ttf")
          , -- Windows' file for a family whose name it does not start with.
            ("family by file name", ["Segoe UI Symbol"], found "seguisym.ttf")
          , ("regular over medium", ["NanoWeightFixture"], found "NanoWeightFixture-Regular.ttf")
          , ("book over bold", ["NanoBookFixture"], found "NanoBookFixture-Book.ttf")
          , ("upright variable font", ["NanoVariableFixture"], found "NanoVariableFixture-VariableFont_wght.ttf")
          , -- The google/fonts names, whose italic sorts first.
            ("upright font named by its axes", ["NanoBracketFixture"], found "NanoBracketFixture[wght].ttf")
          , ("XDG data directory", ["NanoXdgFixture"], if os `elem` ["mingw32", "darwin"] then Nothing else Just xdg)
          , ( "ordered fallback"
            , ["", "NanoMissingFixture", "NanoFallbackFixture", "NanoSearchFixture"]
            , found "NanoFallbackFixture.otf"
            )
          , ("empty request", [], Nothing)
          , ("missing family", ["NanoMissingFixture"], Nothing)
          , ("non-regular fallback", ["NanoBoldOnlyFixture"], found "NanoBoldOnlyFixture-Bold.ttf")
          ]
        others =
          [ "NanoSearchFixture-Bold.ttf", "NanoBookFixture-Bold.ttf", "NanoBracketFixture-Italic[wght].ttf"
          , "NanoVariableFixture-Italic-VariableFont_wght.ttf", "NanoWeightFixture-Medium.ttf", "times.ttf"
          , "nanoshort.ttf", "nanoshortbd.ttf", "nanoshortb.ttf", "nanoshortz.ttf"
          ]
    setEnv "XDG_DATA_DIRS" (root </> "xdg")
    unsetEnv "XDG_DATA_HOME"
    -- The font directories are walked once per process, so every fixture
    -- exists before the first search.
    mapM_ (createDirectoryIfMissing True) [fonts, takeDirectory xdg]
    mapM_ (`writeFile` "") (xdg : map font others ++ [f | (_, _, Just f) <- searches, f /= xdg])
    mapM_ (\(name, request, want) -> expect name want =<< searchFonts request) searches
    -- An installed Times New Roman named in full may win over the fixture.
    expect "Times New Roman by its short file name" True . isJust =<< searchFonts ["Times New Roman"]
    expect "deduplicated family" ["Nano Search Fixture"] . filter (== "Nano Search Fixture") =<< listFontFamilies
    -- A bold face is looked for beside its regular face by name, the
    -- family's own file first and Windows' short names after.
    mapM_
      (\(name, regular, bolds) -> expect name (map font bolds) =<< boldFaceCandidates (font regular))
      [ ("family bold", "NanoSearchFixture-Regular.ttf", ["NanoSearchFixture-Bold.ttf"])
      , ("short-name bold", "nanoshort.ttf", ["nanoshortbd.ttf", "nanoshortb.ttf"])
      , ("no bold", "NanoFallbackFixture.otf", [])
      ]
    putStrLn "font search: ok"

expect :: (Eq a, Show a) => String -> a -> a -> IO ()
expect name want got = unless (got == want) (fail (name ++ ": expected " ++ show want ++ ", got " ++ show got))

homeVar :: String
homeVar = if os == "mingw32" then "LOCALAPPDATA" else "HOME"

temporaryRoot :: IO FilePath
temporaryRoot = do
  (path, handle) <- (`openTempFile` "nano-ui-font-search") =<< getTemporaryDirectory
  hClose handle >> removeFile path >> createDirectory path
  pure path
