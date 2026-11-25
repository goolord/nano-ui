-- | Locate system font files by walking the standard font directories for the
-- current platform, without fontconfig. Each candidate file is matched
-- against the requested family name using a normalised-filename heuristic.
module NanoUI.Sdl.Internal.Font.Search
  ( searchFonts
  , searchFontFamilies
  , listFontFamilies
  , boldFaceCandidates
  , startFontIndex
  , fontFilesUnder
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, modifyMVar, newEmptyMVar, newMVar, putMVar, readMVar)
import Control.Exception (IOException, SomeException, catch, throwIO, try)
import Control.Monad (filterM, mfilter, void)
import Data.Containers.ListUtils (nubOrd)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Char (isDigit, isLower, isUpper, toLower)
import Data.List (elemIndex, find, isInfixOf, isPrefixOf, partition, sort, sortOn, stripPrefix, tails)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import qualified Data.Set as Set
import NanoUI.Ttf.Internal.Support (expandGenericFamily, normalizeFamily)
import System.Directory (canonicalizePath, doesDirectoryExist, getHomeDirectory, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath (isAbsolute, splitSearchPath, takeBaseName, takeDirectory, takeExtension, (</>))
import System.Info (os)
import System.IO.Unsafe (unsafePerformIO)

-- | Try each family name in order, returning the first font file that
-- matches.  Generic families like @monospace@ are expanded to a list of
-- concrete families first.
searchFonts :: [String] -> IO (Maybe FilePath)
searchFonts names = listToMaybe <$> searchFontFamilies (concatMap families names)
  where
    families name = maybe [name] (concatMap families) (expandGenericFamily (normalizeFamily name))

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

-- | Human-readable names for the installed font files, deduped and sorted.
-- Each is made from a file name, up to its first @-@, so a family whose files
-- are named like @NotoSans-Bold.ttf@ appears once, but one with a short name
-- per style, as on Windows, appears once per style (@arial@, @arialbd@).
-- Each name is a usable 'searchFonts' token: the same normalization is applied
-- to both the requested family and the file stem, so a listed name always
-- resolves back to (at least) the file it came from. Non-text faces (icons,
-- colour emoji) are included; callers that need sans families can filter the
-- result themselves. The font directories are read once per process, so a
-- font installed later is not listed.
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
-- are walked once per process, on a thread of their own ('startFontIndex');
-- this waits for that walk only while it is still running.
fontStems :: IO [(String, FilePath)]
fontStems = startFontIndex >>= readMVar >>= either throwIO pure

-- | Start walking the font directories on a background thread, unless a walk
-- has started already, and answer where its result goes. A session calls it
-- when it opens, so the first search, or the first character no loaded font
-- has, finds the walk done or under way.
startFontIndex :: IO (MVar (Either SomeException [(String, FilePath)]))
startFontIndex = modifyMVar fontIndexRef $ \case
  Just done -> pure (Just done, done)
  Nothing -> do
    done <- newEmptyMVar
    void $ forkIO $ putMVar done =<< try walk
    pure (Just done, done)
  where
    walk = do
      files <- fontFilesUnder =<< defaultFontDirs
      let stems = map (\path -> (normalize (takeBaseName path), path)) files
      -- Forced here, so a search that waits finds the work done.
      stems <$ mapM_ (\(stem, _) -> length stem `seq` pure ()) stems

{-# NOINLINE fontIndexRef #-}
fontIndexRef :: MVar (Maybe (MVar (Either SomeException [(String, FilePath)])))
fontIndexRef = unsafePerformIO (newMVar Nothing)

-- | Every font file under each of the directories, in their order, each
-- directory's sorted. A directory reached twice, by a symbolic link or as
-- two of the roots, is walked once.
fontFilesUnder :: [FilePath] -> IO [FilePath]
fontFilesUnder roots = do
  seen <- newIORef Set.empty
  concat <$> mapM (fmap sort . fontFilesBelow seen) roots

-- | Every font file under a directory; returns @[]@ if the directory cannot be
-- read, or was walked already under any name: @seen@ holds the canonical
-- paths walked, so a symbolic link back up the tree ends the walk there and
-- a directory linked from two places is walked once. Only entries that do
-- not match 'isFontFile' are checked with 'doesDirectoryExist' to avoid
-- redundant @stat@ calls in large font directories.
fontFilesBelow :: IORef (Set.Set FilePath) -> FilePath -> IO [FilePath]
fontFilesBelow seen dir = do
  canonical <- canonicalizePath dir `catch` \(_ :: IOException) -> pure dir
  walked <- Set.member canonical <$> readIORef seen
  if walked
    then pure []
    else do
      modifyIORef' seen (Set.insert canonical)
      entries <- map (dir </>) <$> listDirectory dir `catch` \(_ :: IOException) -> pure []
      let (fonts, others) = partition isFontFile entries
      subdirs <- filterM doesDirectoryExist others
      (fonts ++) . concat <$> mapM (fontFilesBelow seen) subdirs

defaultFontDirs :: IO [FilePath]
defaultFontDirs =
  case os of
    "darwin" -> (\home -> [home </> "Library/Fonts", "/Library/Fonts", "/System/Library/Fonts"]) <$> getHomeDirectory
    "mingw32" -> do
      mRoot <- lookupEnv "SystemRoot"
      mLocal <- lookupEnv "LOCALAPPDATA"
      pure $
        [local </> "Microsoft" </> "Windows" </> "Fonts" | Just local <- [mLocal]]
          ++ [fromMaybe "C:\\Windows" mRoot </> "Fonts"]
    -- @fonts@ in the XDG data home and ~/.fonts, then in each XDG data
    -- directory (NixOS keeps its fonts there) and in the system data
    -- directories, which fontconfig reads whatever XDG_DATA_DIRS says. The
    -- XDG spec ignores relative entries, an empty one among them.
    _ -> do
      home <- getHomeDirectory
      let env name = mfilter (not . null) <$> lookupEnv name
      dataHome <- fromMaybe (home </> ".local/share") <$> env "XDG_DATA_HOME"
      dataDirs <- maybe [] (filter isAbsolute . splitSearchPath) <$> env "XDG_DATA_DIRS"
      pure (nubOrd ([dataHome </> "fonts", home </> ".fonts"] ++ map (</> "fonts") (dataDirs ++ ["/usr/local/share", "/usr/share"])))

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

-- | How well a file stem matches a normalised family name: 'Nothing' unless
-- the stem starts with it. What follows the name is a style (@Regular@,
-- @Bold Italic@, axes in brackets) or the rest of a longer family's name
-- and then a style (@Mono@ in @RobotoMono-Regular@, @Hebrew@ in
-- @OpenSansHebrew-Regular@). Every file of the family itself outranks every
-- file of a longer one, whatever their styles; within each, the style
-- ranks the files.
matchScore :: String -> String -> Maybe Int
matchScore "" _ = Nothing
matchScore norm stem = score <$> stripPrefix norm stem
  where
    score t =
      let style = fromMaybe "" (find isStyleTail (tails t))
          longer = length style < length t
       in styleScore style - (if longer then 200 else 0)
    -- A slanted file ranks just below its upright twin, which by name can
    -- sort after it (@X-Italic[wght]@ before @X[wght]@).
    styleScore t = base t - fromEnum ("italic" `isInfixOf` t || "oblique" `isInfixOf` t)
    base t
      | null t = 100
      | Just i <- elemIndex t regularTails = 90 - i
      | t `elem` otherTails = 70
      | "variable" `isInfixOf` t = 20
      | otherwise = 60

-- | Whether a normalised tail after a family name names only a style: style
-- words, optionally followed by variable-font axes (@[wght]@,
-- @VariableFont_wght@), or one of Windows' one- and two-letter style
-- suffixes (@arialbd@, @consolaz@). @Mono@, @Condensed@ or a script name
-- in it make it the rest of a longer family's name.
isStyleTail :: String -> Bool
isStyleTail t = t `elem` ["", "r", "b", "bd", "i", "bi", "z"] || styleWords (dropAxes t)
  where
    dropAxes s
      | (before, '[' : _) <- break (== '[') s = dropVariable before
      | otherwise = dropVariable s
    dropVariable s = case [take n s | n <- [0 .. length s], "variable" `isPrefixOf` drop n s] of
      before : _ -> before
      [] -> s
    styleWords "" = True
    styleWords s = any (\w -> maybe False styleWords (stripPrefix w s)) styleVocabulary

-- | Words a style tail is made of.
styleVocabulary :: [String]
styleVocabulary =
  [ "regular", "normal", "book", "roman", "text", "medium", "bold", "italic", "oblique", "light"
  , "thin", "black", "heavy", "semi", "demi", "extra", "ultra"
  ]

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
familyAliases "timesnewroman" = ["times"]
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
-- Shared with @nano-ui-sdl-web@ via "NanoUI.Ttf.Internal.Support".
normalize :: String -> String
normalize = normalizeFamily
