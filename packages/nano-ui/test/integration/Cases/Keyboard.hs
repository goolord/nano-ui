{-# LANGUAGE OverloadedStrings #-}

module Cases.Keyboard
  ( runKeyboardButtonTest
  , runKeyboardCheckboxTest
  , runKeyboardSliderTest
  , runKeyboardRadioTest
  , runKeyboardToggleTest
  , runKeyboardTabHeaderTest
  ) where

import Data.IORef (IORef)
import NanoUI
import NanoUI.Testing
import NanoUI.Testing.Assert (assert, assertEq, assertGt)
import NanoUI.Testing.Harness (warmup2, withInputOff)

-- | Step the tab focus to the next focusable.
tabInp :: Input -> Input
tabInp inp = inp {inputKeys = inputKeysFromList [KeyTab]}

-- | A single key-down frame.
keyInp :: Key -> Input -> Input
keyInp k inp = inp {inputKeys = inputKeysFromList [k]}

-- | A space key-down frame (space arrives as a character, not a Key).
spaceInp :: Input -> Input
spaceInp inp = inp {inputChars = " "}

-- | Plain buttons activate with Enter and Space while focused.
runKeyboardButtonTest :: Context -> IORef Int -> IO ()
runKeyboardButtonTest ctx failed = do
  let inp0 = withInputOff 200 120
      ui = column $ do
        a <- button "A"
        b <- button "B"
        pure (a, b)
  _ <- warmup2 ctx inp0 ui
  _ <- runFrame ctx (tabInp inp0) ui
  ((aEnter, _), _, _, _) <- runFrame ctx (keyInp KeyEnter inp0) ui
  assert failed aEnter
  ((aSpace, _), _, _, _) <- runFrame ctx (spaceInp inp0) ui
  assert failed aSpace
  _ <- runFrame ctx (tabInp inp0) ui
  ((_, bEnter), _, _, _) <- runFrame ctx (keyInp KeyEnter inp0) ui
  assert failed bEnter

-- | A focused checkbox toggles with Space and Enter.
runKeyboardCheckboxTest :: Context -> IORef Int -> IO ()
runKeyboardCheckboxTest ctx failed = do
  let inp0 = withInputOff 200 100
      ui = column (checkbox "Opt" False)
  _ <- warmup2 ctx inp0 ui
  _ <- runFrame ctx (tabInp inp0) ui
  ((_, checked1), _, _, _) <- runFrame ctx (spaceInp inp0) ui
  assert failed checked1
  ((_, checked2), _, _, _) <- runFrame ctx (keyInp KeyEnter inp0) ui
  assert failed (not checked2)

-- | A focused slider steps with the arrow keys.
runKeyboardSliderTest :: Context -> IORef Int -> IO ()
runKeyboardSliderTest ctx failed = do
  let inp0 = withInputOff 300 80
      ui = column (slider 0 100 50)
  (_, v0) <- warmup2 ctx inp0 ui
  assertEq failed v0 50
  _ <- runFrame ctx (tabInp inp0) ui
  ((_, v1), _, _, _) <- runFrame ctx (keyInp KeyRight inp0) ui
  assertGt failed v1 50
  ((_, v2), _, _, _) <- runFrame ctx (keyInp KeyLeft inp0) ui
  assertEq failed v2 50
  ((_, v3), _, _, _) <- runFrame ctx (keyInp KeyDown inp0) ui
  assertEq failed v3 49
  ((_, v4), _, _, _) <- runFrame ctx (keyInp KeyUp inp0) ui
  assertEq failed v4 50

-- | A focused radio group changes selection with the arrow keys.
runKeyboardRadioTest :: Context -> IORef Int -> IO ()
runKeyboardRadioTest ctx failed = do
  let inp0 = withInputOff 200 160
      ui = column (radioFieldset ["A", "B", "C"] 0)
  (_, sel0) <- warmup2 ctx inp0 ui
  assertEq failed sel0 0
  _ <- runFrame ctx (tabInp inp0) ui
  ((_, sel1), _, _, _) <- runFrame ctx (keyInp KeyDown inp0) ui
  assertEq failed sel1 1
  ((_, sel2), _, _, _) <- runFrame ctx (keyInp KeyDown inp0) ui
  assertEq failed sel2 2
  ((_, sel3), _, _, _) <- runFrame ctx (keyInp KeyUp inp0) ui
  assertEq failed sel3 1

-- | A focused toggle switch flips with Space and Enter.
runKeyboardToggleTest :: Context -> IORef Int -> IO ()
runKeyboardToggleTest ctx failed = do
  let inp0 = withInputOff 200 100
      ui = column (toggleSwitch False)
  (_, v0) <- warmup2 ctx inp0 ui
  assert failed (not v0)
  _ <- runFrame ctx (tabInp inp0) ui
  ((_, v1), _, _, _) <- runFrame ctx (spaceInp inp0) ui
  assert failed v1
  ((_, v2), _, _, _) <- runFrame ctx (keyInp KeyEnter inp0) ui
  assert failed (not v2)

data KB = KBA | KBB
  deriving (Eq, Show)

-- | Tab headers are focusable and switch the active tab with Enter.
runKeyboardTabHeaderTest :: Context -> IORef Int -> IO ()
runKeyboardTabHeaderTest ctx failed = do
  let inp0 = withInputOff 300 100
      ui cur =
        tabs cur
          [ tab KBA "Alpha" (label_ "BodyA")
          , tab KBB "Beta" (label_ "BodyB")
          ]
  _ <- warmup2 ctx inp0 (ui KBA)
  _ <- runFrame ctx (tabInp inp0) (ui KBA)
  ((_, active1), _, _, _) <- runFrame ctx (keyInp KeyEnter inp0) (ui KBA)
  assertEq failed active1 KBA
  _ <- runFrame ctx (tabInp inp0) (ui KBA)
  ((_, active2), _, _, _) <- runFrame ctx (keyInp KeyEnter inp0) (ui KBA)
  assertEq failed active2 KBB
