{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | Compile-time check that the public app surface stays narrow.
module ApiBoundary () where

import Control.Monad (void)
import NanoUI
import NanoUI.Backend.Term (defaultTermOptions, runTermApp, runTermAppReduce)

requiredAppSymbols :: IO ()
requiredAppSymbols =
  runTermApp defaultTermOptions (void (label "boundary"))

requiredReduceSymbols :: IO ()
requiredReduceSymbols =
  runTermAppReduce defaultTermOptions id () (\_ -> void (label "boundary"))
