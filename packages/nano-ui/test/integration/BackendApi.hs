-- A minimal backend must compile using only the supported public surface.
module BackendApi (backendApiCheck) where

import Control.Monad (unless)
import NanoUI (label)
import NanoUI.Backend
import NanoUI.Runner

backendApiCheck :: IO ()
backendApiCheck = do
  ctx <- newPixelContext
  (_, drawing, _) <- runFrame ctx emptyInput (label "backend")
  unless (drawCmdCount drawing >= 0) (fail "invalid draw commands")
  _ <- takeDamage ctx
  _ <- takeDamagePieces ctx
  _ <- collectRasterSpans ctx emptyInput
  debug <- newDebugSampler
  let driver = SessionDriver
        { sdPollEvents = pure [()]
        , sdWaitEvents = \_ -> pure [()]
        , sdApplyEvent = const
        , sdIsButtonEdge = const False
        , sdIsSessionQuit = const True
        , sdSyncDisplay = \c i -> pure (c, i)
        , sdDebug = debug
        , sdContinuous = False
        , sdPacingMs = 16
        , sdPresentPaces = pure False
        , sdAlignSec = 0
        , sdShouldDraw = \c before now anim refresh -> shouldRedrawFrame c before now anim False refresh
        , sdDraw = \c i _ -> do
            (_, _, dirty) <- runFrame c i (label "backend")
            pure dirty
        , sdOnCursor = \_ _ -> pure ()
        , sdShouldQuit = const False
        }
  runSessionLoop driver ctx emptyInput
