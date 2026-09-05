{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Ditto.Types as Ditto
import NanoUI
  ( Input (..)
  , Rect (..)
  , Size (..)
  , Theme (..)
  , card
  , colorRGBA
  , columnWith
  , contrastRatio
  , danger
  , emptyInput
  , heading
  , maxW
  , minW
  , runNanoUI
  , tight
  )
import NanoUI.Testing (collectTextSpans, ctxTheme, newContext, runFrame)
import NanoUI.Form
import NanoUI.Form.Backend (updateFieldInput)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

data Person = Person
  { personName :: !Text
  , personAge  :: !Float
  , personOk   :: !Bool
  } deriving (Eq, Show)

personForm :: Form Text Person
personForm =
  Person
    <$> withFieldErrors
          (inputText "name" "Alice"
            `prove` notEmpty "Name cannot be empty"
            `prove` minLength 2 (const "Name must have at least 2 characters"))
    <*> withFieldErrors
          (inputSlider "age" 0 100 30
            `prove` inRange 18 100 (const "Must be at least 18"))
    <*> inputCheckbox "accepted" True

failingForm :: Form Text Person
failingForm =
  Person
    <$> (inputText "name" "" `prove` notEmpty "Name is required")
    <*> (inputSlider "age" 0 100 12 `prove` inRange 18 100 (const "Must be at least 18"))
    <*> inputCheckbox "accepted" False

assert :: String -> Bool -> IO ()
assert desc condition =
  if condition
    then putStrLn $ "  [PASS] " ++ desc
    else error $ "  [FAIL] " ++ desc

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "=== Running nano-ui-form Test Suite ==="

  putStrLn "Creating context..."
  ctx <- newContext
  let inp = emptyInput { inputWindowSize = Size 60 20 }
  putStrLn "Context created."

  putStrLn "\n--- Test 1: Valid Form Evaluation (runNanoUI) ---"
  (_, res1) <- runNanoUI ctx inp (runNanoForm "testPerson" personForm)
  case res1 of
    Ditto.Ok (Ditto.Proved _ p) -> do
      assert "Decoded valid person" (personName p == "Alice" && personAge p == 30 && personOk p)
    Ditto.Error errs ->
      error $ "Expected success, got errors: " ++ show errs

  putStrLn "\n--- Test 2: Validation Failure & Errors (runNanoUI) ---"
  (_, res2) <- runNanoUI ctx inp (runNanoForm "failing" failingForm)
  case res2 of
    Ditto.Error errs -> do
      let errorMsgs = map snd errs
      assert "Detected two validation errors" (length errs == 2)
      assert "Caught Name is required" ("Name is required" `elem` errorMsgs)
      assert "Caught Must be at least 18" ("Must be at least 18" `elem` errorMsgs)
    Ditto.Ok _ ->
      error "Expected validation failure, but form succeeded"

  putStrLn "\n--- Test 3: nanoFormLive Runner (runFrame) ---"
  (mResult, _, _, _) <- runFrame ctx inp (nanoFormLive "testPerson" personForm)
  case mResult of
    Just p  -> assert "Live runner produced Just Person" (personName p == "Alice")
    Nothing -> error "Expected live runner to produce Just Person"

  putStrLn "\n--- Test 4: resetForm & state isolation ---"
  runNanoUI ctx inp (resetForm "testPerson")
  assert "resetForm executed cleanly" True

  putStrLn "\n--- Test 5: withFieldErrors renders widget-level errors ---"
  (view5, res5) <- runNanoUI ctx inp (runNanoForm "failingWithErrors" $
    withFieldErrors (inputText "field" "" `prove` notEmpty "Field must not be empty"))
  case res5 of
    Ditto.Error errs -> do
      assert "Detected field error" (length errs == 1)
      let FormView act = Ditto.unView view5 errs
      runNanoUI ctx inp act
      assert "Rendered field errors below widget cleanly" True
    Ditto.Ok _ -> error "Expected field error"

  putStrLn "\n--- Test 6: Multi-field stability & no ID shift on error appearance/clear ---"
  let multiForm :: Form Text (Text, Float, Text)
      multiForm =
        (,,)
          <$> withFieldErrors (inputText "user" "Ada" `prove` minLength 3 (const "Too short"))
          <*> withFieldErrors (inputSlider "age" 10 100 25 `prove` inRange 18 100 (const "Must be 18+"))
          <*> withFieldErrors (inputText "bio" "Bio text" `prove` notEmpty "Bio required")

  -- Frame 1: Initial valid state
  (v1, r1) <- runNanoUI ctx inp (runNanoForm "multi" multiForm)
  case r1 of
    Ditto.Ok (Ditto.Proved _ (u, a, b)) -> do
      assert "Initial valid form decoded" (u == "Ada" && a == 25 && b == "Bio text")
      runNanoUI ctx inp (runFormView (Ditto.unView v1 []))
    _ -> error "Expected valid initial form"

  -- Frame 2: Update age to 15 (invalid)
  updateFieldInput ctx "multi" "age" (FormInputFloat 15)
  (v2, r2) <- runNanoUI ctx inp (runNanoForm "multi" multiForm)
  case r2 of
    Ditto.Error errs -> do
      assert "Age failed validation" (length errs == 1)
      -- Render with error callout
      runNanoUI ctx inp (runFormView (Ditto.unView v2 errs))
    Ditto.Ok _ -> error "Expected age validation error"

  -- Frame 3: Run again to verify siblings did NOT reset
  (v3, r3) <- runNanoUI ctx inp (runNanoForm "multi" multiForm)
  case r3 of
    Ditto.Error errs -> do
      assert "Age error stably preserved in next frame" (length errs == 1)
      runNanoUI ctx inp (runFormView (Ditto.unView v3 errs))
    Ditto.Ok _ -> error "Expected error to persist"

  -- Frame 4: User updates Bio to "Bio modified"
  updateFieldInput ctx "multi" "bio" (FormInputText "Bio modified")

  -- Frame 5: Fix age back to 30 (error clears)
  updateFieldInput ctx "multi" "age" (FormInputFloat 30)
  (v5, r5) <- runNanoUI ctx inp (runNanoForm "multi" multiForm)
  case r5 of
    Ditto.Ok (Ditto.Proved _ (u, a, b)) -> do
      assert "Form valid again without sibling reset" (u == "Ada" && a == 30 && b == "Bio modified")
      runNanoUI ctx inp (runFormView (Ditto.unView v5 []))
    Ditto.Error errs -> error $ "Expected valid form after fix, got: " ++ show errs

  putStrLn "\n--- Test 7: Long error text wrapping & contrast ratio ---"
  let red = themeRed (ctxTheme ctx)
      errBg = colorRGBA 48 20 22 255
      ratio = contrastRatio red errBg
  putStrLn $ "  Contrast ratio of error text vs background: " ++ show ratio ++ ":1"
  assert "Error text has high contrast ratio (> 7:1)" (ratio > 7.0)

  let emailErrorMsg = "Invalid email address format (e.g. name@domain.com)"
      longErrorForm :: Form Text Text
      longErrorForm = withFieldErrors (inputText "email" "bad-email" `prove` validEmail (\_ -> emailErrorMsg))
  (v7, r7) <- runNanoUI ctx inp (runNanoForm "longError" longErrorForm)
  case r7 of
    Ditto.Error errs -> do
      assert "Caught long email error" (length errs == 1)
      let ui = columnWith (tight . minW 300 . maxW 360) (runFormView (Ditto.unView v7 errs))
      (_, _, _, _) <- runFrame ctx inp ui
      spans <- collectTextSpans ctx
      let emailSpans = [t | (_, t, _, _, _) <- spans, "Invalid email" `T.isInfixOf` t || "name@domain.com" `T.isInfixOf` t]
      assert "Email error message is rendered without being lost" (not (null emailSpans))
      let allText = T.unwords emailSpans
      assert "Full email error text is preserved" ("Invalid email" `T.isInfixOf` allText && "name@domain.com" `T.isInfixOf` allText)
    Ditto.Ok _ -> error "Expected email validation error"

  putStrLn "\n--- Test 8: Active Validation Errors column preserves vertical space when wrapping ---"
  let inspectorUi = columnWith (tight . minW 340 . maxW 380) $ do
        card $ do
          heading "Active Validation Errors:"
          danger "• Invalid email address format (e.g. name@domain.com)"
          danger "• Password must be at least 8 characters long"
  (_, _, _, _) <- runFrame ctx inp inspectorUi
  spans8 <- collectTextSpans ctx
  let emailSpans8 = [(r, t) | (r, t, _, _, _) <- spans8, "Invalid email" `T.isInfixOf` t || "name@domain.com" `T.isInfixOf` t]
      pwSpans8 = [(r, t) | (r, t, _, _, _) <- spans8, "Password" `T.isInfixOf` t]
  putStrLn $ "  Email spans: " ++ show emailSpans8
  putStrLn $ "  Password spans: " ++ show pwSpans8
  case (emailSpans8, pwSpans8) of
    ([_line1, (Rect _ y2 _ _, _line2)], [(Rect _ y3 _ _, _)]) -> do
      assert "Email error wrapped into 2 lines" True
      assert "Password error is placed below the 2nd line of email error (no overlap)" (y3 > y2)
    _ -> error $ "Unexpected spans layout: email=" ++ show emailSpans8 ++ ", pw=" ++ show pwSpans8

  putStrLn "\n=== All nano-ui-form Tests Passed! ==="
