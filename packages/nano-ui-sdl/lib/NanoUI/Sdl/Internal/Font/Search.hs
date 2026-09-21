-- | Locate system font files by walking the standard font directories for the
-- current platform, without fontconfig. Each candidate file is matched
-- against the requested family name using a normalised-filename heuristic.
module NanoUI.Sdl.Internal.Font.Search
  ( searchFonts
  , searchFontFamilies
  , listFontFamilies
  ) where

import Control.Exception (IOException, catch)
import Data.Containers.ListUtils (nubOrd)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Char (isDigit, isLower, isSpace, isUpper, toLower)
import Data.List (isInfixOf, minimumBy, sort, stripPrefix)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Ord (Down (..), comparing)
import qualified Data.Set as Set
import System.Directory (getHomeDirectory)
import System.Directory.Recursive (getFilesRecursive)
import System.Environment (lookupEnv)
import System.FilePath (takeBaseName, takeExtension, (</>))
import System.Info (os)
import System.IO.Unsafe (unsafePerformIO)

-- | Try each family name in order, returning the first font file that
-- matches.  Generic families like @monospace@ are expanded to a list of
-- concrete families first.
searchFonts :: [String] -> IO (Maybe FilePath)
searchFonts names = case concatMap families names of
  [] -> pure Nothing
  candidates -> do
    files <- fontStems
    pure (listToMaybe (mapMaybe (`bestMatch` files) candidates))
  where
    families name =
      let norm = normalize name
       in if null norm then [] else maybe [norm] (concatMap families) (expandGeneric norm)

-- | The file for each family that is installed, in the order asked, reading
-- the font directories once.
searchFontFamilies :: [String] -> IO [FilePath]
searchFontFamilies names = do
  files <- fontStems
  pure (nubOrd (mapMaybe (\name -> bestMatch (normalize name) files) names))

-- | Human-readable names for every installed font family, deduped and sorted.
-- Each name is a usable 'searchFonts' token: the same normalization is applied
-- to both the requested family and the file stem, so a listed name always
-- resolves back to (at least) the file it came from. Non-text faces (icons,
-- colour emoji) are included; callers that need sans families can filter the
-- result themselves.
listFontFamilies :: IO [String]
listFontFamilies = do
  files <- fontStems
  pure (Set.toAscList (Set.fromList (map (prettyFamily . takeBaseName . snd) files)))

-- | Filename stem -> display family. Everything from the first @-@ is treated
-- as style (\"Regular\", \"Bold Italic\", ...); camel case is split so
-- @NotoSansArabic@ reads as @Noto Sans Arabic@. Kept case-insensitively
-- compatible with 'normalize'.
prettyFamily :: String -> String
prettyFamily = separateCamel . stripStyle
  where
    stripStyle s = case break (== '-') s of
      (base, _) -> base
    separateCamel = go
      where
        go [] = []
        go (c : cs) = c : goTail c cs
        goTail _ [] = []
        goTail prev (c : cs)
          | isUpper c && (isLower prev || isDigit prev) = ' ' : c : goTail c cs
          | otherwise = c : goTail c cs

-- ---------------------------------------------------------------------------
-- Directory traversal

-- | Every font file under every standard font directory for this platform,
-- with its normalised name, normalised once for all the families matched
-- against it. User directories come first so that user-installed fonts win
-- over system ones; missing or unreadable roots are skipped. The directories
-- are walked once per process.
fontStems :: IO [(String, FilePath)]
fontStems =
  readIORef fontStemsRef >>= \case
    Just stems -> pure stems
    Nothing -> do
      roots <- defaultFontDirs
      files <- concat <$> mapM (fmap (sort . filter isFontFile) . filesBelow) roots
      let stems = map (\path -> (normalize (takeBaseName path), path)) files
      writeIORef fontStemsRef (Just stems)
      pure stems

{-# NOINLINE fontStemsRef #-}
fontStemsRef :: IORef (Maybe [(String, FilePath)])
fontStemsRef = unsafePerformIO (newIORef Nothing)

filesBelow :: FilePath -> IO [FilePath]
filesBelow root =
  getFilesRecursive root `catch` \(_ :: IOException) -> pure []

defaultFontDirs :: IO [FilePath]
defaultFontDirs =
  case os of
    "darwin" -> macDirs
    "mingw32" -> winDirs
    _ -> linuxDirs
  where
    linuxDirs :: IO [FilePath]
    linuxDirs = do
      home <- getHomeDirectory
      pure
        [ home </> ".local/share/fonts"
        , "/usr/local/share/fonts"
        , "/usr/share/fonts"
        ]

    macDirs :: IO [FilePath]
    macDirs = do
      home <- getHomeDirectory
      pure
        [ home </> "Library/Fonts"
        , "/Library/Fonts"
        , "/System/Library/Fonts"
        ]

    winDirs :: IO [FilePath]
    winDirs = do
      mRoot <- lookupEnv "SystemRoot"
      let systemDir = fromMaybe "C:\\Windows" mRoot </> "Fonts"
      mLocal <- lookupEnv "LOCALAPPDATA"
      let userDirs =
            maybe [] (\l -> [l </> "Microsoft" </> "Windows" </> "Fonts"]) mLocal
      pure (userDirs ++ [systemDir])

isFontFile :: FilePath -> Bool
isFontFile path =
  map toLower (takeExtension path) `elem` [".ttf", ".otf", ".ttc", ".otc"]

-- ---------------------------------------------------------------------------
-- Family matching

-- | Pick the highest-scoring file for @norm@ (a normalised family name).
bestMatch :: String -> [(String, FilePath)] -> Maybe FilePath
bestMatch norm files =
  case [(score, path) | (stem, path) <- files, Just score <- [maximum (Nothing : map (`matchScore` stem) candidates)]] of
    [] -> Nothing
    scored ->
      -- minimumBy keeps the first tie; descending scores prefer the best face.
      let (_, best) = minimumBy (comparing (Down . fst)) scored
       in Just best
  where
    candidates = norm : familyAliases norm

matchScore :: String -> String -> Maybe Int
matchScore "" _ = Nothing
matchScore norm stem
  | stem == norm = Just 100
  | otherwise =
      case stripPrefix norm stem of
        Nothing -> Nothing
        Just t
          | t `elem` regularTails -> Just 90
          | t `elem` otherTails -> Just 70
          | "variable" `isInfixOf` t -> Just 20
          | otherwise -> Just 60

-- | Style tails that indicate the regular weight of a family.
regularTails :: [String]
regularTails = ["regular", "r", "normal", "medium", "text"]

-- | Style tails for non-regular weights; still worth preferring over an
-- unrelated font, but a regular (or exact) match wins.
otherTails :: [String]
otherTails =
  [ "bold"
  , "italic"
  , "light"
  , "semibold"
  , "semibolditalic"
  , "extrabold"
  , "thin"
  , "black"
  , "regularitalic"
  , "bolditalic"
  , "mediumitalic"
  , "oblique"
  ]

-- | Windows ships @Consolas@ as @consola.ttf@ and @Courier New@ as
-- @cour.ttf@, so those families need filename aliases that prefix-match
-- differently than their family names.
familyAliases :: String -> [String]
familyAliases "consolas" = ["consola"]
familyAliases "couriernew" = ["cour"]
familyAliases _ = []

-- | Expand a generic CSS family into concrete families to search in order.
expandGeneric :: String -> Maybe [String]
expandGeneric "monospace" =
  Just
    [ "DejaVu Sans Mono"
    , "Liberation Mono"
    , "Ubuntu Mono"
    , "Noto Sans Mono"
    , "Consolas"
    , "Courier New"
    ]
expandGeneric "sansserif" =
  Just
    [ "DejaVu Sans"
    , "Liberation Sans"
    , "Noto Sans"
    , "Open Sans"
    , "Helvetica Neue"
    ]
expandGeneric "serif" =
  Just
    [ "DejaVu Serif"
    , "Liberation Serif"
    , "Noto Serif"
    , "Times New Roman"
    ]
expandGeneric _ = Nothing

-- | Fold case, drop whitespace and separators, so family \/ file names can be
-- compared loosely ("DejaVu Sans Mono" vs @DejaVuSansMono.ttf@).
normalize :: String -> String
normalize =
  map toLower
    . filter (\c -> not (isSpace c) && c /= '-' && c /= '_')
