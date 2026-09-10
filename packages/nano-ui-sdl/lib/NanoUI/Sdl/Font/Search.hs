-- | Locate system font files by walking the standard font directories for the
-- current platform (no fontconfig dependency, no streaming library).  Each
-- candidate file is matched against the requested family name using a
-- normalised-filename heuristic.
module NanoUI.Sdl.Font.Search
  ( searchFonts
  , listFontFamilies
  ) where

import Control.Exception (IOException, catch)
import Data.Char (isDigit, isLower, isSpace, isUpper, toLower)
import Data.List (isInfixOf, maximumBy, sort, stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import System.Directory (getHomeDirectory)
import System.Directory.Recursive (getFilesRecursive)
import System.Environment (lookupEnv)
import System.FilePath (takeBaseName, takeExtension, (</>))
import System.Info (os)

-- | Try each family name in order, returning the first font file that
-- matches.  Generic families like @monospace@ are expanded to a list of
-- concrete families first.
searchFonts :: [String] -> IO (Maybe FilePath)
searchFonts names = go names
  where
    go [] = pure Nothing
    go (family : rest) =
      searchFamily family >>= \case
        Just path -> pure (Just path)
        Nothing -> go rest

-- | Human-readable names for every installed font family, deduped and sorted.
-- Each name is a usable 'searchFonts' token: the same normalization is applied
-- to both the requested family and the file stem, so a listed name always
-- resolves back to (at least) the file it came from. Non-text faces (icons,
-- colour emoji) are included; callers that need sans families can filter the
-- result themselves.
listFontFamilies :: IO [String]
listFontFamilies = do
  files <- allFontFiles
  pure (dedupe (sort (map (prettyFamily . takeBaseName) files)))

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

-- | Drop adjacent duplicates from a sorted list.
dedupe :: Eq a => [a] -> [a]
dedupe (x : y : rest)
  | x == y = dedupe (y : rest)
  | otherwise = x : dedupe (y : rest)
dedupe xs = xs

searchFamily :: String -> IO (Maybe FilePath)
searchFamily family =
  let norm = normalize family
   in if null norm
        then pure Nothing
        else
          case expandGeneric norm of
            Just concreteFamilies -> searchFonts concreteFamilies
            Nothing -> do
              files <- allFontFiles
              pure (bestMatch norm files)

-- ---------------------------------------------------------------------------
-- Directory traversal

-- | Every font file under every standard font directory for this platform.
-- User directories come first so that user-installed fonts win over system
-- ones; missing or unreadable roots are skipped.
allFontFiles :: IO [FilePath]
allFontFiles = do
  roots <- defaultFontDirs
  fmap (sort . filter isFontFile . concat) (mapM filesBelow roots)

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
bestMatch :: String -> [FilePath] -> Maybe FilePath
bestMatch norm files =
  case [(score, path) | path <- files, Just score <- [scoreFile norm path]] of
    [] -> Nothing
    scored ->
      let (_, best) = maximumBy (comparing fst) scored
       in Just best

scoreFile :: String -> FilePath -> Maybe Int
scoreFile norm path =
  let stem = normalize (takeBaseName path)
      candidates = norm : familyAliases norm
   in maxOver candidates stem
  where
    maxOver [] _ = Nothing
    maxOver (c : cs) stem =
      case (matchScore c stem, maxOver cs stem) of
        (Nothing, rest) -> rest
        (here, Nothing) -> here
        (Just a, Just b) -> Just (max a b)

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