module Main (main) where

import Control.Exception (bracket)
import Control.Monad (unless)
import NanoUI.Sdl.Internal.Font.Search (boldFaceCandidates, listFontFamilies, searchFonts)
import System.Directory
  ( createDirectory
  , createDirectoryIfMissing
  , getTemporaryDirectory
  , removeFile
  , removePathForcibly
  )
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath (takeDirectory, (</>))
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
      short = fonts </> "nanoshort.ttf"
      shortBd = fonts </> "nanoshortbd.ttf"
      shortB = fonts </> "nanoshortb.ttf"
      shortItalic = fonts </> "nanoshortz.ttf"
      -- Windows' file for a family whose name it does not start with.
      symbols = fonts </> "seguisym.ttf"
      medium = fonts </> "NanoWeightFixture-Medium.ttf"
      weightRegular = fonts </> "NanoWeightFixture-Regular.ttf"
      book = fonts </> "NanoBookFixture-Book.ttf"
      bookBold = fonts </> "NanoBookFixture-Bold.ttf"
      italicVariable = fonts </> "NanoVariableFixture-Italic-VariableFont_wght.ttf"
      variable = fonts </> "NanoVariableFixture-VariableFont_wght.ttf"
      -- Only the Linux search reads the XDG data directories.
      xdg = root </> "xdg" </> "fonts" </> "NanoXdgFixture.ttf"
    setEnv "XDG_DATA_DIRS" (root </> "xdg")
    unsetEnv "XDG_DATA_HOME"
    -- The font directories are walked once per process, so every fixture
    -- exists before the first search.
    mapM_ (createDirectoryIfMissing True) [fonts, takeDirectory xdg]
    mapM_
      (`writeFile` "")
      [regular, bold, fallback, boldOnly, short, shortBd, shortB, shortItalic, symbols, medium, weightRegular, book, bookBold, italicVariable, variable, xdg]
    expect "regular face" (Just regular) =<< searchFonts ["Nano Search Fixture"]
    expect "family by file name" (Just symbols) =<< searchFonts ["Segoe UI Symbol"]
    expect "regular over medium" (Just weightRegular) =<< searchFonts ["NanoWeightFixture"]
    expect "book over bold" (Just book) =<< searchFonts ["NanoBookFixture"]
    expect "upright variable font" (Just variable) =<< searchFonts ["NanoVariableFixture"]
    expect "XDG data directory" (if os `elem` ["mingw32", "darwin"] then Nothing else Just xdg)
      =<< searchFonts ["NanoXdgFixture"]
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
    -- A bold face is looked for beside its regular face by name, the
    -- family's own file first and Windows' short names after.
    expect "family bold" [bold] =<< boldFaceCandidates regular
    expect "short-name bold" [shortBd, shortB] =<< boldFaceCandidates short
    expect "no bold" [] =<< boldFaceCandidates fallback
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
