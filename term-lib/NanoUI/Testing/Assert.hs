-- | Concise assertion and frame helpers for the integration test suite.
module NanoUI.Testing.Assert
  ( bump
  , failWhen
  , assert
  , assertEq
  , assertNeq
  , assertGt
  , assertLt
  , withInput
  , runFrameDraw
  , run2Frames
  , measureResp
  , measureRespW
  , evalUi
  , eval2Ui
  , runClickReduce
  ) where

import Control.Monad (unless, when)
import Data.IORef (IORef, modifyIORef')
import Data.Typeable (Typeable)
import NanoUI (emptyInput, Input (..), NanoUI, Response (..), Size (..), V2 (..), rectW, respRect)
import NanoUI.Testing (Context, DrawData, FrameMsg, runFrame, runFrameReduce)

{-# INLINE bump #-}
bump :: IORef Int -> IO ()
bump r = modifyIORef' r (+ 1)

{-# INLINE failWhen #-}
failWhen :: IORef Int -> Bool -> IO ()
failWhen r bad = when bad (bump r)

{-# INLINE assert #-}
assert :: IORef Int -> Bool -> IO ()
assert r ok = unless ok (bump r)

{-# INLINE assertEq #-}
assertEq :: Eq a => IORef Int -> a -> a -> IO ()
assertEq r a b = failWhen r (a /= b)

{-# INLINE assertNeq #-}
assertNeq :: Eq a => IORef Int -> a -> a -> IO ()
assertNeq r a b = failWhen r (a == b)

{-# INLINE assertGt #-}
assertGt :: Ord a => IORef Int -> a -> a -> IO ()
assertGt r a b = failWhen r (a <= b)

{-# INLINE assertLt #-}
assertLt :: Ord a => IORef Int -> a -> a -> IO ()
assertLt r a b = failWhen r (a >= b)

{-# INLINE withInput #-}
withInput :: Float -> Float -> Input
withInput w h = emptyInput {inputWindowSize = Size w h}

eval2Ui :: Context -> Input -> NanoUI a -> IO a
eval2Ui ctx inp ui = do
  _ <- runFrame ctx inp ui
  evalUi ctx inp ui

runFrameDraw :: Context -> Input -> NanoUI a -> IO DrawData
runFrameDraw ctx inp ui = do
  (_, _, draw, _) <- runFrame ctx inp ui
  pure draw

run2Frames :: Context -> Input -> NanoUI a -> IO (a, [FrameMsg], DrawData, Bool)
run2Frames ctx inp ui = do
  _ <- runFrame ctx inp ui
  runFrame ctx inp ui

measureResp :: Context -> Input -> NanoUI Response -> IO Response
measureResp ctx inp ui = do
  (resp, _, _, _) <- run2Frames ctx inp ui
  pure resp

measureRespW :: Context -> Input -> NanoUI Response -> IO Float
measureRespW ctx inp ui = do
  resp <- measureResp ctx inp ui
  pure (rectW (respRect resp))

evalUi :: Context -> Input -> NanoUI a -> IO a
evalUi ctx inp ui = do
  (a, _, _, _) <- runFrame ctx inp ui
  pure a

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
