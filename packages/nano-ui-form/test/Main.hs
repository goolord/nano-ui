module Main (main) where

import Data.Int (Int8)
import Data.Text (Text)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Ditto.Types as Ditto
import NanoUI
  ( Input (..)
  , Size (..)
  , columnWith
  , emptyInput
  , maxW
  , minW
  , runNanoUI
  , tight
  )
import NanoUI.Testing (collectTextSpans, newContext, runFrame)
import NanoUI.Form
import NanoUI.Form.Backend (updateFieldInput)
import NanoUI qualified as NUI
import qualified NanoUI.Form.Unnamed as Unnamed
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import Scope (check, runScopeTests)

data Person = Person
  { personName :: !Text
  , personAge  :: !Float
  , personOk   :: !Bool
  } deriving (Eq, Show)

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

  customCtx <- newContext
  let decodeText (FormInputText t) = Right t
      decodeText _ = Left "Expected text"
      customForm :: Form Text (Text, Text)
      customForm = (,)
        <$> inputWidget (Just "internal-key") decodeText (const False) FormInputText
          (\value -> NUI.label "Visible caption" >> pure (mempty, if value == "initial" then "edited" else value)) "initial"
        <*> inputWidget Nothing decodeText NUI.respChanged FormInputText NUI.textInput' "automatic"
      runCustom = runNanoUI customCtx emptyInput (runNanoForm "custom" customForm)
  (customView, _) <- runCustom
  runNanoUI customCtx emptyInput (runFormView (Ditto.unView customView []))
  (_, customResult) <- runCustom
  case customResult of
    Ditto.Ok (Ditto.Proved _ values) ->
      check "Custom fields publish value changes without a response flag" (values == ("edited", "automatic"))
    Ditto.Error errs -> fail (show errs)
  updateFieldInput customCtx "custom" "internal-key" (FormInputText "external")
  (_, externalResult) <- runCustom
  case externalResult of
    Ditto.Ok (Ditto.Proved _ values) ->
      check "Custom field identity is independent of its visible label" (values == ("external", "automatic"))
    Ditto.Error errs -> fail (show errs)
  updateFieldInput customCtx "custom" "internal-key" (FormInputBool True)
  (_, invalidResult) <- runCustom
  case invalidResult of
    Ditto.Error errs -> check "Custom field decoder errors reach ditto" (map snd errs == ["Expected text"])
    Ditto.Ok _ -> fail "Expected custom decoder failure"

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

  putStrLn "\n--- Validation Failure & Errors (runNanoUI) ---"
  (_, res2) <- runNanoUI ctx inp (runNanoForm "failing" failingForm)
  case res2 of
    Ditto.Error errs -> do
      let errorMsgs = map snd errs
      check "Detected two validation errors" (length errs == 2)
      check "Caught Name is required" ("Name is required" `elem` errorMsgs)
      check "Caught Must be at least 18" ("Must be at least 18" `elem` errorMsgs)
    Ditto.Ok _ ->
      fail "Expected validation failure, but form succeeded"

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

  putStrLn "\n--- Multi-field stability & no ID shift on error appearance/clear ---"
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

  -- Frame 3: User updates Bio to "Bio modified"
  updateFieldInput ctx "multi" "bio" (FormInputText "Bio modified")

  -- Frame 4: Fix age back to 30 (error clears)
  updateFieldInput ctx "multi" "age" (FormInputFloat 30)
  (v4, r4) <- runNanoUI ctx inp (runNanoForm "multi" multiForm)
  case r4 of
    Ditto.Ok (Ditto.Proved _ (u, a, b)) -> do
      check "Form valid again without sibling reset" (u == "Ada" && a == 30 && b == "Bio modified")
      runNanoUI ctx inp (runFormView (Ditto.unView v4 []))
    Ditto.Error errs -> fail $ "Expected valid form after fix, got: " ++ show errs

  putStrLn "\n--- Long error text wrapping ---"
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

  putStrLn "\n=== All nano-ui-form Tests Passed! ==="
