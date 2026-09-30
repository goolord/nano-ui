-- This module must fail to typecheck: the message type is part of the view.
module WrongMessage (wrongMessage) where

import NanoUI.Emit (NanoUIE, emit)

wrongMessage :: NanoUIE Int ()
wrongMessage = emit True
