{-# LANGUAGE CPP #-}

module NanoUI.Sdl.Font.Search
  ( bracketFontconfig
  , searchFonts
  ) where

#if HAVE_FONTCONFIG

import Control.Exception (bracket_)
import Graphics.Text.Font.Choose (defaultSubstitute, getValue, nameParse)
import Graphics.Text.Font.Choose.Config (fini, initFonts)
import Graphics.Text.Font.Choose.Config.Accessors
  ( MatchKind (..)
  , SetName (..)
  , current
  , fonts
  , substitute
  )
import Graphics.Text.Font.Choose.FontSet (fontSetMatch)

bracketFontconfig :: IO a -> IO a
bracketFontconfig = bracket_ initFonts fini

searchFonts :: [String] -> IO (Maybe FilePath)
searchFonts names = go names
  where
    go [] = pure Nothing
    go (name : rest) =
      matchFontName name >>= \case
        Just path -> pure (Just path)
        Nothing -> go rest

matchFontName :: String -> IO (Maybe FilePath)
matchFontName family = do
  conf <- current
  fontSet <- fonts conf System
  let pat =
        substitute conf (defaultSubstitute (nameParse family)) Nothing MatchPattern
  pure $
    fontSetMatch conf [fontSet] pat >>= \case
      (matched : _) -> getValue "file" matched
      _ -> Nothing

#elif defined(mingw32_HOST_OS)

import Data.Char (isSpace, toLower)
import Data.Maybe (fromMaybe)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)

bracketFontconfig :: IO a -> IO a
bracketFontconfig act = act

searchFonts :: [String] -> IO (Maybe FilePath)
searchFonts names = do
  dirs <- winFontDirs
  findFirstM doesFileExist $
    [ dir ++ "\\" ++ stem ++ ext
    | name <- names
    , stem <- winStems name
    , dir <- dirs
    , ext <- [".ttf", ".otf", ".ttc"]
    ]

winFontDirs :: IO [FilePath]
winFontDirs = do
  mRoot <- lookupEnv "SystemRoot"
  let systemDir = fromMaybe "C:\\Windows" mRoot ++ "\\Fonts"
  mLocal <- lookupEnv "LOCALAPPDATA"
  let userDirs =
        maybe [] (\l -> [l ++ "\\Microsoft\\Windows\\Fonts"]) mLocal
  pure (systemDir : userDirs)

winStems :: String -> [String]
winStems "Consolas" = ["consola"]
winStems "Courier New" = ["cour"]
winStems "DejaVu Sans Mono" = ["dejavusansmono"]
winStems "Liberation Mono" = ["liberationmono-regular", "liberationmono"]
winStems "monospace" = ["consola", "cour"]
winStems n =
  let s = map toLower (filter (not . isSpace) n)
   in [s]

findFirstM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findFirstM _ [] = pure Nothing
findFirstM p (x : xs) = do
  ok <- p x
  if ok then pure (Just x) else findFirstM p xs

#else

bracketFontconfig :: IO a -> IO a
bracketFontconfig act = act

searchFonts :: [String] -> IO (Maybe FilePath)
searchFonts _ = pure Nothing

#endif
