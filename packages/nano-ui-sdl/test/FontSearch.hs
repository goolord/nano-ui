module Main (main) where

import Control.Exception (bracket)
import Control.Monad (unless)
import NanoUI.Sdl.Font.Search (listFontFamilies, searchFonts)
import System.Directory
  ( createDirectory
  , createDirectoryIfMissing
  , getTemporaryDirectory
  , removeFile
  , removePathForcibly
  )
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.Info (os)

-- Discovery only inspects filenames; no native display or valid font data is needed.
main :: IO ()
main = bracket temporaryRoot removePathForcibly $ \root ->
  bracket (lookupEnv homeVar) restoreHome $ \_ -> do
    setEnv homeVar root
    let
      fonts =
        root </> case os of
          "mingw32" -> "Microsoft" </> "Windows" </> "Fonts"
          "darwin" -> "Library" </> "Fonts"
          _ -> ".local" </> "share" </> "fonts"
      regular = fonts </> "NanoSearchFixture-Regular.ttf"
      bold = fonts </> "NanoSearchFixture-Bold.ttf"
      fallback = fonts </> "NanoFallbackFixture.otf"
      boldOnly = fonts </> "NanoBoldOnlyFixture-Bold.ttf"
    -- The font directories are walked once per process, so every fixture
    -- exists before the first search.
    createDirectoryIfMissing True fonts
    mapM_ (`writeFile` "") [regular, bold, fallback, boldOnly]
    expect "regular face" (Just regular) =<< searchFonts ["Nano Search Fixture"]
    expect "ordered fallback" (Just fallback)
      =<< searchFonts
        ["", "NanoMissingFixture", "NanoFallbackFixture", "NanoSearchFixture"]
    expect "empty request" Nothing =<< searchFonts []
    expect "missing family" Nothing =<< searchFonts ["NanoMissingFixture"]
    families <- listFontFamilies
    expect
      "deduplicated family"
      ["Nano Search Fixture"]
      (filter (== "Nano Search Fixture") families)
    expect "non-regular fallback" (Just boldOnly) =<< searchFonts ["NanoBoldOnlyFixture"]
    putStrLn "font search: ok"

expect :: (Eq a, Show a) => String -> a -> a -> IO ()
expect label expected actual =
  unless (actual == expected) $
    fail (label ++ ": expected " ++ show expected ++ ", got " ++ show actual)

homeVar :: String
homeVar = if os == "mingw32" then "LOCALAPPDATA" else "HOME"

restoreHome :: Maybe String -> IO ()
restoreHome = maybe (unsetEnv homeVar) (setEnv homeVar)

temporaryRoot :: IO FilePath
temporaryRoot = do
  tmp <- getTemporaryDirectory
  (path, handle) <- openTempFile tmp "nano-ui-font-search"
  hClose handle
  removeFile path
  createDirectory path
  pure path
