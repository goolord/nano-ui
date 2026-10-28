{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM, when)
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , findExecutable
  , listDirectory
  , makeAbsolute
  )
import System.Exit (ExitCode (..), exitFailure, exitSuccess)
import System.FilePath (dropFileName, normalise, takeExtension, (</>))
import System.Process (proc, readCreateProcessWithExitCode)

-- | Locate the repository root by walking up to find cabal.project.
findRepoRoot :: IO FilePath
findRepoRoot = go "."
 where
  go dir = do
    hasProject <- doesFileExist (dir </> "cabal.project")
    if hasProject
      then makeAbsolute dir
      else do
        parent <- makeAbsolute (dir </> "..")
        curr <- makeAbsolute dir
        if parent == curr
          then error "Could not find repository root containing cabal.project"
          else go (dir </> "..")

-- | Recursively collect all markdown files in a directory.
collectMarkdownFiles :: FilePath -> IO [FilePath]
collectMarkdownFiles dir = do
  entries <- listDirectory dir
  paths <- forM entries $ \entry -> do
    let
      path = dir </> entry
    isDir <- doesDirectoryExist path
    if isDir
      then collectMarkdownFiles path
      else pure [path | takeExtension path == ".md"]
  pure (concat paths)

data IncludeRef = IncludeRef
  { incSourceMd :: !FilePath
  , incLineNum :: !Int
  , incTargetPath :: !FilePath
  , incAnchor :: !(Maybe T.Text)
  }
  deriving (Show, Eq)

-- | Parse include directives like `{{#include ../path/file.hs:anchor}}`
parseIncludes :: FilePath -> T.Text -> [IncludeRef]
parseIncludes mdFile content =
  [ makeRef lineNo line
  | (lineNo, line) <- zip [1 ..] (T.lines content)
  , prefix `T.isInfixOf` line
  ]
 where
  prefix = "{{#include "
  makeRef lineNo line =
    let
      after = T.drop (T.length prefix) (snd (T.breakOn prefix line))
      inside = T.takeWhile (/= '}') after
      trimmed = T.strip inside
      (pathPart, anchorPart) = T.breakOn ":" trimmed
      anchor =
        if T.null anchorPart
          then Nothing
          else Just (T.drop 1 anchorPart)
      targetRel = T.unpack pathPart
      targetFull = normalise (dropFileName mdFile </> targetRel)
     in
      IncludeRef
        { incSourceMd = mdFile
        , incLineNum = lineNo
        , incTargetPath = targetFull
        , incAnchor = anchor
        }

-- | Check if fences (```) are balanced in a markdown file.
checkFenceBalance :: FilePath -> T.Text -> Either String Int
checkFenceBalance mdFile content =
  let
    fences = [l | l <- T.lines content, "```" `T.isPrefixOf` T.stripStart l]
    count = length fences
   in
    if even count
      then Right (count `div` 2)
      else
        Left
          (mdFile <> ": unbalanced code fences (found " <> show count <> " ``` markers)")

-- | Verify an included file and optional anchor.
verifyInclude :: FilePath -> IncludeRef -> IO (Maybe String)
verifyInclude root inc = do
  exists <- doesFileExist (incTargetPath inc)
  if not exists
    then
      pure
        $ Just
        $ incSourceMd inc
          <> ":"
          <> show (incLineNum inc)
          <> ": target file not found: "
          <> incTargetPath inc
    else case incAnchor inc of
      Nothing -> pure Nothing
      Just anchor -> do
        fileContent <- TIO.readFile (incTargetPath inc)
        let
          fileLines = T.lines fileContent
          startTag = "ANCHOR: " <> anchor
          endTag = "ANCHOR_END: " <> anchor
          hasStart = any (startTag `T.isInfixOf`) fileLines
          hasEnd = any (endTag `T.isInfixOf`) fileLines
        if not hasStart
          then
            pure
              $ Just
              $ incSourceMd inc
                <> ":"
                <> show (incLineNum inc)
                <> ": start anchor not found: "
                <> T.unpack anchor
                <> " in "
                <> incTargetPath inc
          else
            if not hasEnd
              then
                pure
                  $ Just
                  $ incSourceMd inc
                    <> ":"
                    <> show (incLineNum inc)
                    <> ": end anchor not found: "
                    <> T.unpack anchor
                    <> " in "
                    <> incTargetPath inc
              else do
                -- Check that content between anchors is non-empty
                let
                  between =
                    takeWhile (not . (endTag `T.isInfixOf`))
                      $ drop 1
                      $ dropWhile (not . (startTag `T.isInfixOf`)) fileLines
                  nonBlank = filter (not . T.null . T.strip) between
                if null nonBlank
                  then
                    pure
                      $ Just
                      $ incSourceMd inc
                        <> ":"
                        <> show (incLineNum inc)
                        <> ": anchor region is empty: "
                        <> T.unpack anchor
                  else pure Nothing

main :: IO ()
main = do
  root <- findRepoRoot
  putStrLn $ "Repository root: " <> root
  let
    bookSrc = root </> "book" </> "src"
  mdFiles <- collectMarkdownFiles bookSrc
  putStrLn $ "Found " <> show (length mdFiles) <> " markdown documentation files."

  -- Check code fence balance
  putStrLn "\nChecking code fence balance across all book chapters..."
  fenceResults <- forM mdFiles $ \f -> do
    content <- TIO.readFile f
    pure (checkFenceBalance f content)

  let
    fenceErrors = [err | Left err <- fenceResults]
    totalBlocks = sum [n | Right n <- fenceResults]
  mapM_ putStrLn fenceErrors
  putStrLn $ "Total code blocks verified for balance: " <> show totalBlocks

  -- Check {{#include}} references and anchors
  putStrLn "\nChecking mdBook include directives and anchors..."
  allIncludes <- fmap concat $
    forM mdFiles $ \f -> do
      content <- TIO.readFile f
      pure (parseIncludes f content)

  putStrLn $ "Found " <> show (length allIncludes) <> " include directives."
  includeErrors <- fmap concat $
    forM allIncludes $ \inc -> do
      err <- verifyInclude root inc
      case err of
        Nothing -> pure []
        Just e -> pure [e]

  mapM_ (\e -> putStrLn ("  ERROR: " <> e)) includeErrors

  -- If mdbook is installed, verify `mdbook build book`
  putStrLn "\nChecking mdbook build..."
  mCmd <- findExecutable "mdbook"
  mdbookErrors <- case mCmd of
    Nothing -> do
      putStrLn "  (mdbook executable not in PATH, skipping mdbook build check)"
      pure []
    Just mdbookPath -> do
      putStrLn $ "  Running mdbook build with: " <> mdbookPath
      (code, out, err) <-
        readCreateProcessWithExitCode (proc mdbookPath ["build", "book"]) ""
      case code of
        ExitSuccess -> do
          putStrLn "  mdbook build succeeded without errors."
          pure []
        ExitFailure c -> do
          putStrLn $ "  mdbook build failed with code " <> show c
          putStrLn $ "  STDOUT:\n" <> out
          putStrLn $ "  STDERR:\n" <> err
          pure ["mdbook build failed"]

  let
    allErrors = fenceErrors <> includeErrors <> mdbookErrors
  if null allErrors
    then do
      putStrLn "\n========================================"
      putStrLn " SUCCESS: All book code examples verified!"
      putStrLn $ " - " <> show (length mdFiles) <> " markdown files checked"
      putStrLn $
        " - " <> show (length allIncludes) <> " included code anchors validated"
      putStrLn $ " - " <> show totalBlocks <> " code blocks balanced"
      putStrLn "========================================"
      exitSuccess
    else do
      putStrLn "\n========================================"
      putStrLn $
        " FAILURE: " <> show (length allErrors) <> " error(s) found in book examples:"
      mapM_ (\e -> putStrLn (" - " <> e)) allErrors
      putStrLn "========================================"
      exitFailure
