module Main (main) where

import Data.Int (Int8)
import Data.Text (Text)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Ditto.Types as Ditto
import FormDemo (formDemoUi)
import NanoUI
  ( Input (..)
  , Rect (..)
  , Size (..)
  , Theme (..)
  , V2 (..)
  , card
  , colorRGBA
  , columnWith
  , contrastRatio
  , danger
  , emptyInput
  , heading
  , maxW
  , minW
  , monospaceMetrics
  , runNanoUI
  , tight
  )
import NanoUI.Testing (collectTextSpans, getTheme, newContext, runFrame, withFontMetrics)
import NanoUI.Form
import NanoUI.Form.Backend (emptyFormStateStore, getFormStore, updateFieldInput)
import qualified NanoUI.Form.Unnamed as Unnamed
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import Scope (check, runScopeTests)

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

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "=== Running nano-ui-form Test Suite ==="
  runScopeTests

  ctx <- newContext
  let inp = emptyInput { inputWindowSize = Size 60 20 }

  let collectionForm :: Form Text (Int, Int, Int)
      collectionForm = (,,)
        <$> inputSelect "select" (Seq.fromList ["First", "Second"]) 1
        <*> inputRadio "radio" (Seq.fromList ["First", "Second"]) 0
        <*> Unnamed.inputSelect (Just "Only") 0
  (_, collectionResult) <- runNanoUI ctx inp (runNanoForm "collections" collectionForm)
  case collectionResult of
    Ditto.Ok (Ditto.Proved _ values) ->
      check "Foldable form options preserve initial indices" (values == (1, 0, 0))
    Ditto.Error errs -> fail (show errs)

  putStrLn "\n--- Test 1: Valid Form Evaluation (runNanoUI) ---"
  (_, res1) <- runNanoUI ctx inp (runNanoForm "testPerson" personForm)
  case res1 of
    Ditto.Ok (Ditto.Proved _ p) ->
      check "Decoded valid person" (personName p == "Alice" && personAge p == 30 && personOk p)
    Ditto.Error errs ->
      fail $ "Expected success, got errors: " ++ show errs

  putStrLn "\n--- Test 2: Validation Failure & Errors (runNanoUI) ---"
  (_, res2) <- runNanoUI ctx inp (runNanoForm "failing" failingForm)
  case res2 of
    Ditto.Error errs -> do
      let errorMsgs = map snd errs
      check "Detected two validation errors" (length errs == 2)
      check "Caught Name is required" ("Name is required" `elem` errorMsgs)
      check "Caught Must be at least 18" ("Must be at least 18" `elem` errorMsgs)
    Ditto.Ok _ ->
      fail "Expected validation failure, but form succeeded"

  putStrLn "\n--- Test 3: nanoFormLive Runner (runFrame) ---"
  (mResult, _, _, _) <- runFrame ctx inp (nanoFormLive "testPerson" personForm)
  case mResult of
    Just p  -> check "Live runner produced Just Person" (personName p == "Alice")
    Nothing -> fail "Expected live runner to produce Just Person"

  putStrLn "\n--- Test 4: resetForm & state isolation ---"
  updateFieldInput ctx "testPerson" "name" (FormInputText "Changed")
  updateFieldInput ctx "otherPerson" "name" (FormInputText "Preserved")
  otherBefore <- getFormStore ctx "otherPerson"
  runNanoUI ctx inp (resetForm "testPerson")
  resetStore <- getFormStore ctx "testPerson"
  otherAfter <- getFormStore ctx "otherPerson"
  check "resetForm clears the selected form's state" (resetStore == emptyFormStateStore)
  check "resetForm preserves other forms" (otherAfter == otherBefore)

  let enumForm :: Form Text (Int8, Int8, Int8)
      enumForm = (,,)
        <$> inputEnumSelect "select" (-42)
        <*> inputEnumRadio "radio" 42
        <*> Unnamed.inputEnumSelect (-12)
      checkEnums expected = do
        (_, result) <- runNanoUI ctx inp (runNanoForm "enums" enumForm)
        case result of
          Ditto.Ok (Ditto.Proved _ values) ->
            check "Enum fields use zero-based widget indices independently of enum bounds" (values == expected)
          Ditto.Error errs -> fail (show errs)
  checkEnums (-42, 42, -12)
  updateFieldInput ctx "enums" "select" (FormInputInt 0)
  updateFieldInput ctx "enums" "radio" (FormInputInt 255)
  checkEnums (minBound, maxBound, -12)
  updateFieldInput ctx "enums" "select" (FormInputInt (-10))
  updateFieldInput ctx "enums" "radio" (FormInputInt 300)
  checkEnums (minBound, maxBound, -12)

  putStrLn "\n--- Test 5: withFieldErrors renders widget-level errors ---"
  (view5, res5) <- runNanoUI ctx inp (runNanoForm "failingWithErrors" $
    withFieldErrors (inputText "field" "" `prove` notEmpty "Field must not be empty"))
  case res5 of
    Ditto.Error errs -> do
      check "Detected field error" (length errs == 1)
      runNanoUI ctx inp (runFormView (Ditto.unView view5 errs))
    Ditto.Ok _ -> fail "Expected field error"

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
      check "Initial valid form decoded" (u == "Ada" && a == 25 && b == "Bio text")
      runNanoUI ctx inp (runFormView (Ditto.unView v1 []))
    _ -> fail "Expected valid initial form"

  -- Frame 2: Update age to 15 (invalid)
  updateFieldInput ctx "multi" "age" (FormInputFloat 15)
  (v2, r2) <- runNanoUI ctx inp (runNanoForm "multi" multiForm)
  case r2 of
    Ditto.Error errs -> do
      check "Age failed validation" (length errs == 1)
      -- Render with error callout
      runNanoUI ctx inp (runFormView (Ditto.unView v2 errs))
    Ditto.Ok _ -> fail "Expected age validation error"

  -- Frame 3: Run again to verify siblings did NOT reset
  (v3, r3) <- runNanoUI ctx inp (runNanoForm "multi" multiForm)
  case r3 of
    Ditto.Error errs -> do
      check "Age error stably preserved in next frame" (length errs == 1)
      runNanoUI ctx inp (runFormView (Ditto.unView v3 errs))
    Ditto.Ok _ -> fail "Expected error to persist"

  -- Frame 4: User updates Bio to "Bio modified"
  updateFieldInput ctx "multi" "bio" (FormInputText "Bio modified")

  -- Frame 5: Fix age back to 30 (error clears)
  updateFieldInput ctx "multi" "age" (FormInputFloat 30)
  (v5, r5) <- runNanoUI ctx inp (runNanoForm "multi" multiForm)
  case r5 of
    Ditto.Ok (Ditto.Proved _ (u, a, b)) -> do
      check "Form valid again without sibling reset" (u == "Ada" && a == 30 && b == "Bio modified")
      runNanoUI ctx inp (runFormView (Ditto.unView v5 []))
    Ditto.Error errs -> fail $ "Expected valid form after fix, got: " ++ show errs

  putStrLn "\n--- Test 7: Long error text wrapping & contrast ratio ---"
  th <- getTheme ctx
  let ratio = contrastRatio (themeRed th) (colorRGBA 48 20 22 255)
  check ("Error text contrast ratio " ++ show ratio ++ ":1 is not above 7:1") (ratio > 7.0)

  let emailErrorMsg = "Invalid email address format (e.g. name@domain.com)"
      longErrorForm :: Form Text Text
      longErrorForm = withFieldErrors (inputText "email" "bad-email" `prove` validEmail (\_ -> emailErrorMsg))
  (v7, r7) <- runNanoUI ctx inp (runNanoForm "longError" longErrorForm)
  case r7 of
    Ditto.Error errs -> do
      check "Caught long email error" (length errs == 1)
      let ui = columnWith (tight . minW 300 . maxW 360) (runFormView (Ditto.unView v7 errs))
      _ <- runFrame ctx inp ui
      spans <- collectTextSpans ctx
      let emailSpans = [t | (_, t, _, _, _) <- spans, "Invalid email" `T.isInfixOf` t || "name@domain.com" `T.isInfixOf` t]
      check "Email error message is rendered without being lost" (not (null emailSpans))
      let allText = T.unwords emailSpans
      check "Full email error text is preserved" ("Invalid email" `T.isInfixOf` allText && "name@domain.com" `T.isInfixOf` allText)
    Ditto.Ok _ -> fail "Expected email validation error"

  putStrLn "\n--- Test 8: Active Validation Errors column preserves vertical space when wrapping ---"
  let inspectorUi = columnWith (tight . minW 340 . maxW 380) $ do
        card $ do
          heading "Active Validation Errors:"
          danger "• Invalid email address format (e.g. name@domain.com)"
          danger "• Password must be at least 8 characters long"
  _ <- runFrame ctx inp inspectorUi
  spans8 <- collectTextSpans ctx
  let emailSpans8 = [(r, t) | (r, t, _, _, _) <- spans8, "Invalid email" `T.isInfixOf` t || "name@domain.com" `T.isInfixOf` t]
      pwSpans8 = [(r, t) | (r, t, _, _, _) <- spans8, "Password" `T.isInfixOf` t]
  case (emailSpans8, pwSpans8) of
    ([_line1, (Rect _ y2 _ _, _line2)], [(Rect _ y3 _ _, _)]) ->
      check "Password error is placed below the 2nd line of email error (no overlap)" (y3 > y2)
    _ -> fail $ "Unexpected spans layout: email=" ++ show emailSpans8 ++ ", pw=" ++ show pwSpans8

  putStrLn "\n--- Test 9: Form Demo Scroll Layout & Bottom Visibility ---"
  let ctx20 = withFontMetrics ctx (monospaceMetrics 20)
      inp9 = emptyInput { inputWindowSize = Size 1100 800 }
      inp9Scroll = inp9 { inputMousePos = V2 400 400, inputScroll = V2 0 10000 }
  _ <- runFrame ctx20 inp9 formDemoUi
  _ <- runFrame ctx20 inp9Scroll formDemoUi
  spans9After <- collectTextSpans ctx20
  case [r | (r, t, _, _, _) <- spans9After, "Submit" `T.isInfixOf` t] of
    Rect _ sy _ sh : _ ->
      check "Submit button is visible on screen when scrolled to bottom" (sy >= 0 && sy + sh <= 800)
    [] -> fail "Submit button span not found after scrolling to bottom"

  putStrLn "\n=== All nano-ui-form Tests Passed! ==="
