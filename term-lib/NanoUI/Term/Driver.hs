{-# LANGUAGE CPP #-}

-- | Platform dispatch for the terminal driver.
module NanoUI.Term.Driver
  ( Driver (..)
  , withDriver
  ) where

import NanoUI.Term.Driver.Types (Driver (..))

#if defined(mingw32_HOST_OS)
import NanoUI.Term.Driver.Windows (withDriver)
#else

-- | Non-Windows builds use notcurses directly; this stub exists only so
-- imports resolve during cross-compilation checks.
withDriver :: (Driver -> IO a) -> IO a
withDriver _ =
  error "NanoUI.Term.Driver.withDriver is Windows-only; use NanoUI.Term.Notcurses"
#endif
