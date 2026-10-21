{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

-- | compile-fail/WrongMessage.hs with its type error deferred to run time, so
-- the suite can check that emitting another message type is an error.
module DeferredWrongMessage (wrongMessage) where

import NanoUI.Emit (NanoUIE, emit)

wrongMessage :: NanoUIE Int ()
wrongMessage = emit True
