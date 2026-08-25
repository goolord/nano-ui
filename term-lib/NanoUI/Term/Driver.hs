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
import NanoUI.Term.Driver.Posix (withDriver)
#endif
