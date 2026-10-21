-- | Locate system font files by walking the standard font directories for the
-- current platform, without fontconfig. Each candidate file is matched
-- against the requested family name using a normalised-filename heuristic.
module NanoUI.Sdl.Internal.Font.Search
  ( searchFonts
  , searchFontFamilies
  , listFontFamilies
  , boldFaceCandidates
  ) where

import Control.Exception (IOException, catch)
import Control.Monad (filterM, mfilter)
import Data.Containers.ListUtils (nubOrd)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Char (isDigit, isLower, isSpace, isUpper, toLower)
import Data.List (elemIndex, isInfixOf, partition, sort, sortOn, stripPrefix)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import qualified Data.Set as Set
import System.Directory (doesDirectoryExist, getHomeDirectory, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath (splitSearchPath, takeBaseName, takeDirectory, takeExtension, (</>))
import System.Info (os)
import System.IO.Unsafe (unsafePerformIO)

-- | Try each family name in order, returning the first font file that
-- matches.  Generic families like @monospace@ are expanded to a list of
-- concrete families first.
searchFonts :: [String] -> IO (Maybe FilePath)
searchFonts names = listToMaybe <$> searchFontFamilies (concatMap families names)
  where
    families name = maybe [name] (concatMap families) (expandGeneric (normalize name))

-- | The file for each family that is installed, in the order asked, reading
-- the font directories once.
searchFontFamilies :: [String] -> IO [FilePath]
searchFontFamilies names = do
  files <- fontStems
  pure (nubOrd (mapMaybe (\name -> bestMatch (normalize name) files) names))

-- | Files beside a regular face that may be its bold, most likely first: the
-- family's own @-Bold@ file, or Windows' short names, @consolab.ttf@ beside
-- @consola.ttf@ and @courbd.ttf@ beside @cour.ttf@. Names prove nothing, so
-- the caller opens each to check it is the family's bold.
boldFaceCandidates :: FilePath -> IO [FilePath]
boldFaceCandidates regular = do
  siblings <- filter isFontFile <$> listDirectory dir `catch` \(_ :: IOException) -> pure []
  let named = [(normalize (takeBaseName f), dir </> f) | f <- siblings]
  pure [path | want <- wanted, (stem', path) <- named, stem' == want]
  where
    dir = takeDirectory regular
    stem = normalize (takeBaseName regular)
    families = stem : [reverse family | t <- regularTails, Just family <- [stripPrefix (reverse t) (reverse stem)], not (null family)]
    wanted = nubOrd (map (<> "bold") families ++ [stem <> "bd", stem <> "b"])

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
prettyFamily file = concat (zipWith separate (' ' : stem) stem)
  where
    stem = takeWhile (/= '-') file
    separate prev c
      | isUpper c && (isLower prev || isDigit prev) = [' ', c]
      | otherwise = [c]

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
      files <- concat <$> mapM (fmap sort . fontFilesBelow) roots
      let stems = map (\path -> (normalize (takeBaseName path), path)) files
      writeIORef fontStemsRef (Just stems)
      pure stems

{-# NOINLINE fontStemsRef #-}
fontStemsRef :: IORef (Maybe [(String, FilePath)])
fontStemsRef = unsafePerformIO (newIORef Nothing)

-- | Every font file under a directory; returns @[]@ if the directory cannot be
-- read. Only entries that do not match 'isFontFile' are checked with
-- 'doesDirectoryExist' to avoid redundant @stat@ calls in large font
-- directories.
fontFilesBelow :: FilePath -> IO [FilePath]
fontFilesBelow dir = do
  entries <- map (dir </>) <$> listDirectory dir `catch` \(_ :: IOException) -> pure []
  let (fonts, others) = partition isFontFile entries
  subdirs <- filterM doesDirectoryExist others
  (fonts ++) . concat <$> mapM fontFilesBelow subdirs

defaultFontDirs :: IO [FilePath]
defaultFontDirs =
  case os of
    "darwin" -> inHome "Library/Fonts" ["/Library/Fonts", "/System/Library/Fonts"]
    "mingw32" -> do
      mRoot <- lookupEnv "SystemRoot"
      mLocal <- lookupEnv "LOCALAPPDATA"
      pure $
        [local </> "Microsoft" </> "Windows" </> "Fonts" | Just local <- [mLocal]]
          ++ [fromMaybe "C:\\Windows" mRoot </> "Fonts"]
    -- fontconfig's roots: @fonts@ in each XDG data directory, and ~/.fonts.
    _ -> do
      home <- getHomeDirectory
      let env name = mfilter (not . null) <$> lookupEnv name
      dataHome <- fromMaybe (home </> ".local/share") <$> env "XDG_DATA_HOME"
      dataDirs <- maybe ["/usr/local/share", "/usr/share"] splitSearchPath <$> env "XDG_DATA_DIRS"
      pure ([dataHome </> "fonts", home </> ".fonts"] ++ map (</> "fonts") dataDirs)
  where
    inHome dir system = (: system) . (</> dir) <$> getHomeDirectory

isFontFile :: FilePath -> Bool
isFontFile path =
  map toLower (takeExtension path) `elem` [".ttf", ".otf", ".ttc", ".otc"]

-- ---------------------------------------------------------------------------
-- Family matching

-- | Pick the highest-scoring file for @norm@ (a normalised family name).
bestMatch :: String -> [(String, FilePath)] -> Maybe FilePath
bestMatch norm files =
  -- The sort is stable, so of equal scores the file found first wins.
  snd <$> listToMaybe (sortOn (Down . fst) scored)
  where
    scored = [(score, path) | (stem, path) <- files, Just score <- [maximum (Nothing : map (`matchScore` stem) candidates)]]
    candidates = norm : familyAliases norm

matchScore :: String -> String -> Maybe Int
matchScore "" _ = Nothing
matchScore norm stem = score <$> stripPrefix norm stem
  where
    score t
      | null t = 100
      | Just i <- elemIndex t regularTails = 90 - i
      | t `elem` otherTails = 70
      | "variable" `isInfixOf` t = if "italic" `isInfixOf` t then 19 else 20
      | otherwise = 60

-- | Style tails that indicate the regular weight of a family, the likeliest
-- first: of a family's files, the one whose tail comes first wins.
regularTails :: [String]
regularTails = ["regular", "normal", "r", "book", "roman", "text", "medium"]

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

-- | Windows ships @Consolas@ as @consola.ttf@ and @Segoe UI Symbol@ as
-- @seguisym.ttf@, and macOS @PingFang SC@ as @PingFang.ttc@, so those
-- families need filename aliases that prefix-match differently than their
-- family names.
familyAliases :: String -> [String]
familyAliases "consolas" = ["consola"]
familyAliases "couriernew" = ["cour"]
familyAliases "segoeuisymbol" = ["seguisym"]
familyAliases "nirmalaui" = ["nirmala"]
familyAliases "leelawadeeui" = ["leelawui"]
familyAliases "microsoftyahei" = ["msyh"]
familyAliases "yugothic" = ["yugothr"]
familyAliases "malgungothic" = ["malgun"]
familyAliases "arialunicodems" = ["arialuni"]
familyAliases "pingfangsc" = ["pingfang"]
familyAliases "kohinoordevanagari" = ["kohinoor"]
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
