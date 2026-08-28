-- | Pick the TUI icon tier from the environment.
--
-- A terminal cannot be asked which font is loaded, and a missing glyph renders
-- as a same-width box, so there is nothing to probe for. The tier is read from
-- the environment instead and stays ASCII unless something says otherwise:
--
-- 1. @NANOUI_ICONS@: @nerd@, @fontawesome@ (@fa@), @ascii@, or @auto@.
-- 2. @NERD_FONT@ / @NERDFONT@ / @NERD_FONTS@ set to a truthy value.
-- 3. A terminal that ships a Nerd Font by default (WezTerm, Ghostty).
module NanoUI.Term.Icons
  ( detectIconSet
  ) where

import Data.Char (toLower)
import NanoUI (IconSet (..), parseIconSet)
import System.Environment (lookupEnv)

detectIconSet :: IO IconSet
detectIconSet = do
  override <- lookupEnv "NANOUI_ICONS"
  case override >>= parseIconSet of
    Just set -> pure set
    Nothing -> do
      nerdVar <- firstEnv ["NERD_FONT", "NERDFONT", "NERD_FONTS"]
      if maybe False truthy nerdVar
        then pure IconsNerd
        else do
          bundled <- bundlesNerdFont
          pure (if bundled then IconsNerd else IconsAscii)

-- Terminals that ship a Nerd Font as the default face. Everything else has to
-- opt in: guessing wrong shows boxes where the chrome should be.
bundlesNerdFont :: IO Bool
bundlesNerdFont = do
  prog <- lookupEnv "TERM_PROGRAM"
  case fmap lower prog of
    Just "wezterm" -> pure True
    Just "ghostty" -> pure True
    _ -> do
      marker <- firstEnv ["WEZTERM_EXECUTABLE", "GHOSTTY_RESOURCES_DIR"]
      pure (maybe False (not . null) marker)

firstEnv :: [String] -> IO (Maybe String)
firstEnv [] = pure Nothing
firstEnv (name : rest) =
  lookupEnv name >>= \case
    Just v -> pure (Just v)
    Nothing -> firstEnv rest

truthy :: String -> Bool
truthy raw = lower raw `elem` ["1", "true", "yes", "on"]

lower :: String -> String
lower = map toLower
