-- | Assertion and frame helpers for the integration test suite.
--
-- Assertions count failures in a shared 'IORef' instead of aborting, so one
-- test reports every broken expectation. Each failure prints the caller's
-- source location and, where there are any, the compared values.
module NanoUI.Testing.Assert
  ( bump
  , assert
  , assertEq
  , assertGt
  , assertLt
  , assertJust
  , assertJustM
  , withInput
  , run2Frames
  , evalUi
  , runClickReduce
  ) where

import Control.Monad (unless, when)
import Data.IORef (IORef, modifyIORef')
import Data.Typeable (Typeable)
import GHC.Stack (HasCallStack, callStack, prettyCallStack, withFrozenCallStack)
import NanoUI (Input (..), NanoUI, Response (..), Size (..), V2 (..))
import NanoUI.Backend (emptyInput)
import NanoUI.Testing (Context, DrawData, FrameMsg, runFrame, runFrameReduce)

-- | Increment a test's failure counter without printing a diagnostic.
bump :: IORef Int -> IO ()
bump r = modifyIORef' r (+ 1)

-- | Count a failure and report where it happened.
failWith :: HasCallStack => IORef Int -> String -> IO ()
failWith r detail = do
  putStrLn ("assertion failed" <> (if null detail then "" else ": " <> detail))
  putStrLn (prettyCallStack callStack)
  bump r

-- | Count and report a failure when the condition is false, then continue.
assert :: HasCallStack => IORef Int -> Bool -> IO ()
assert r ok = unless ok (withFrozenCallStack (failWith r ""))

-- | Require equality, printing both values and the call site on failure.
assertEq :: (HasCallStack, Eq a, Show a) => IORef Int -> a -> a -> IO ()
assertEq r a b = when (a /= b) (withFrozenCallStack (failWith r (show a <> " /= " <> show b)))

-- | Require the first value to be strictly greater than the second.
assertGt :: (HasCallStack, Ord a, Show a) => IORef Int -> a -> a -> IO ()
assertGt r a b = when (a <= b) (withFrozenCallStack (failWith r (show a <> " <= " <> show b)))

-- | Require the first value to be strictly less than the second.
assertLt :: (HasCallStack, Ord a, Show a) => IORef Int -> a -> a -> IO ()
assertLt r a b = when (a >= b) (withFrozenCallStack (failWith r (show a <> " >= " <> show b)))

-- | Run the rest of a test on a value it needs, or count a failure when
-- there is none.
assertJust :: HasCallStack => IORef Int -> Maybe a -> (a -> IO ()) -> IO ()
assertJust r m k = maybe (withFrozenCallStack (failWith r "Nothing")) k m

-- | 'assertJust' on the result of an action.
assertJustM :: HasCallStack => IORef Int -> IO (Maybe a) -> (a -> IO ()) -> IO ()
assertJustM r act k = act >>= \m -> withFrozenCallStack (assertJust r m k)

-- | Event-free input for a logical window width and height.
withInput :: Float -> Float -> Input
withInput w h = emptyInput {inputWindowSize = Size w h}

-- | Run twice with the same input and return the second frame. Use event-free
-- input for warmup; press/key events would otherwise be delivered twice.
run2Frames :: Context -> Input -> NanoUI a -> IO (a, [FrameMsg], DrawData, Bool)
run2Frames ctx inp ui = do
  _ <- runFrame ctx inp ui
  runFrame ctx inp ui

-- | Run a complete headless frame and return only the view's result.
evalUi :: Context -> Input -> NanoUI a -> IO a
evalUi ctx inp ui = do
  (a, _, _, _) <- runFrame ctx inp ui
  pure a

-- | Run left press and release frames through a reducer. Returns the final
-- model, release-frame messages, and release-frame dirty flag.
runClickReduce ::
  (Typeable msg, Eq model) =>
  (msg -> model -> model)
  -> Context
  -> Input
  -> model
  -> (model -> NanoUI Response)
  -> V2
  -> IO (model, [msg], Bool)
runClickReduce reduce ctx inp0 model0 view pos = do
  let
    press =
      inp0
        { inputMousePos = pos
        , inputMouseDown = True
        , inputMousePressed = True
        , inputMouseReleased = False
        }
    release =
      press
        { inputMousePressed = False
        , inputMouseDown = False
        , inputMouseReleased = True
        }
  (_, modelP, _, _, _) <- runFrameReduce reduce ctx press model0 view
  (_, modelR, msgs, _, dirty) <- runFrameReduce reduce ctx release modelP view
  pure (modelR, msgs, dirty)
