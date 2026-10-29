-- | The book's sources hold together: code fences balance, Haskell fences
-- hold only includes of compiled code, every
-- @{{#include path:anchor}}@ names a file and a non-empty anchored region,
-- every interactive specimen tag is in nano-ui-banks.js, and, when mdbook is
-- installed, the book builds.
module Main (main) where

import Control.Monad (filterM, forM, unless)
import Data.Char (isDigit)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (doesDirectoryExist, doesFileExist, findExecutable, listDirectory, makeAbsolute)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath (dropFileName, normalise, takeDirectory, takeExtension, (</>))
import System.Process (proc, readCreateProcessWithExitCode)

-- | The nearest directory at or above the current one holding cabal.project.
findRepoRoot :: IO FilePath
findRepoRoot = makeAbsolute "." >>= go
  where
    go dir = do
      found <- doesFileExist (dir </> "cabal.project")
      if found
        then pure dir
        else
          if takeDirectory dir == dir
            then fail "no cabal.project above the current directory"
            else go (takeDirectory dir)

markdownFiles :: FilePath -> IO [FilePath]
markdownFiles dir = do
  paths <- map (dir </>) <$> listDirectory dir
  dirs <- filterM doesDirectoryExist paths
  nested <- concat <$> mapM markdownFiles dirs
  pure (filter ((== ".md") . takeExtension) paths ++ nested)

-- | The errors in one chapter, each prefixed with its file and line.
chapterErrors :: T.Text -> FilePath -> IO [String]
chapterErrors banks md = do
  content <- TIO.readFile md
  let numbered = zip [1 :: Int ..] (T.lines content)
      at n msg = md <> ":" <> show n <> ": " <> msg
      fences = length [() | (_, l) <- numbered, "```" `T.isPrefixOf` T.stripStart l]
      fenceErrors = [md <> ": unbalanced code fences (" <> show fences <> " markers)" | odd fences]
      inlineCode =
        [ at n "Haskell written inline; put it in a compiled module and {{#include}} it"
        | (n, l) <- haskellLines numbered
        , let t = T.strip l
        , not (T.null t || "{{#include " `T.isPrefixOf` t || "--" `T.isPrefixOf` t)
        ]
      specimens =
        [ at n (what <> " not found in nano-ui-banks.js: " <> T.unpack val)
        | (n, l) <- numbered
        , (tag, attr, key, what) <- specimenKinds
        , tag `T.isInfixOf` l
        , let rest = snd (T.breakOn attr l)
        , not (T.null rest)
        , let val = T.takeWhile (/= '"') (T.drop (T.length attr) rest)
        , not (key val `T.isInfixOf` banks)
        ]
  includes <- forM [(n, l) | (n, l) <- numbered, "{{#include " `T.isInfixOf` l] $ \(n, l) -> do
    let inside = T.strip (T.takeWhile (/= '}') (T.drop 11 (snd (T.breakOn "{{#include " l))))
        (rel, anchorPart) = T.breakOn ":" inside
        target = normalise (dropFileName md </> T.unpack rel)
    exists <- doesFileExist target
    if not exists
      then pure [at n ("target file not found: " <> target)]
      else
        if T.null anchorPart
          then pure []
          else map (at n . (<> " in " <> target)) . anchorErrors (T.drop 1 anchorPart) <$> TIO.readFile target
  pure (fenceErrors ++ inlineCode ++ concat includes ++ specimens)

-- | The numbered lines inside ```haskell fences. A comment line may join two
-- includes; anything else is code the compiler never sees.
haskellLines :: [(Int, T.Text)] -> [(Int, T.Text)]
haskellLines = go False
  where
    go _ [] = []
    go inside ((n, l) : rest)
      | "```" `T.isPrefixOf` T.stripStart l = go (not inside && T.strip l == "```haskell") rest
      | inside = (n, l) : go inside rest
      | otherwise = go inside rest

-- | What is wrong with an anchor's region in a file, if anything.
anchorErrors :: T.Text -> T.Text -> [String]
anchorErrors anchor file
  | not (any (start `T.isInfixOf`) ls) = ["start anchor not found: " <> T.unpack anchor]
  | not (any (end `T.isInfixOf`) ls) = ["end anchor not found: " <> T.unpack anchor]
  | all (T.null . T.strip) region = ["anchor region is empty: " <> T.unpack anchor]
  | any blank (take 1 region ++ take 1 (reverse region)) =
      ["anchor region starts or ends with a blank line: " <> T.unpack anchor]
  | otherwise = []
  where
    ls = T.lines file
    start = "ANCHOR: " <> anchor
    end = "ANCHOR_END: " <> anchor
    blank = T.null . T.strip
    region = takeWhile (not . (end `T.isInfixOf`)) (drop 1 (dropWhile (not . (start `T.isInfixOf`)) ls))

-- | A specimen tag, the attribute naming it, its key in nano-ui-banks.js, and
-- what an error calls it.
specimenKinds :: [(T.Text, T.Text, T.Text -> T.Text, String)]
specimenKinds =
  [ ("<nano-ui-trap", "id=\"", trapKey, "trap specimen")
  , ("<nano-ui-workbench", "widget=\"", \w -> "\"" <> w <> "\"", "workbench widget")
  ]
  where
    trapKey tagId =
      let ds = T.filter isDigit tagId in "\"id\":\"" <> (if T.length ds == 1 then "0" <> ds else ds) <> "\""

main :: IO ()
main = do
  root <- findRepoRoot
  chapters <- markdownFiles (root </> "book" </> "src")
  let banksPath = root </> "book" </> "theme" </> "nano-ui-banks.js"
  hasBanks <- doesFileExist banksPath
  banks <- if hasBanks then TIO.readFile banksPath else pure ""
  chapterErrs <- concat <$> mapM (chapterErrors banks) chapters
  mdbook <-
    findExecutable "mdbook" >>= \case
      Nothing -> [] <$ putStrLn "mdbook not in PATH: skipping the build"
      Just exe -> do
        (code, out, err) <- readCreateProcessWithExitCode (proc exe ["build", root </> "book"]) ""
        pure ["mdbook build failed:\n" <> out <> err | code /= ExitSuccess]
  let errors = ["book/theme/nano-ui-banks.js missing" | not hasBanks] ++ chapterErrs ++ mdbook
  mapM_ (putStrLn . ("ERROR: " <>)) errors
  unless (null errors) exitFailure
  putStrLn ("book: " <> show (length chapters) <> " chapters ok")
