module Scope (check, runScopeTests) where

import Control.Exception (IOException, try)
import Control.Monad (forM_, unless, void)
import Data.IORef (writeIORef)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Ditto.Core qualified as Ditto
import Ditto.Types qualified as Ditto
import NanoUI
  ( Input (..)
  , Key (KeyEnter, KeyUp)
  , NanoUI
  , Rect (..)
  , colorA
  , colorRGBA
  , columnWith
  , fillW
  , liftIO
  , respSubmitted
  , textInput'
  )
import NanoUI.Backend (runNanoUI)
import NanoUI.Internal.Context (ctxFocusId)
import NanoUI.Form
import NanoUI.Form.Internal.Backend
  ( FormStateStore (..)
  , emptyFormStateStore
  , FormScope (..)
  , askFormScope
  , runFormUI
  , getFormStore
  , markFormSubmitted
  , resetFormState
  , setFormStore
  , updateFieldInput
  )
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Layout.Arena
  ( NodeType (NodeButton, NodeColorPicker, NodeTextArea, NodeTextInput)
  )
import NanoUI.Testing (Context, clearDirty, collectTextSpans, isDirty, newPixelContext, runFrame)
import NanoUI.Testing.Harness (hasText, keyInp, nodesOfType, runClick, spanCenter, warmup2, withInputOff)

check :: String -> Bool -> IO ()
check message ok = unless ok (fail message)

runScopeTests :: IO ()
runScopeTests = do
  testDeferredViews
  testNestedViews
  testPrefixRestoration
  testFormInvalidation
  testSubmitPulse
  testSubmitSameFrameEdit
  testSubmitSkipsTextArea
  testColorKeepsAlpha
  testMarkSubmitted
  testResetWidgets
  testResetTextArea

-- | A checkbox is a button, and these forms have no other buttons.
checkboxes :: Context -> IO [(WidgetId, Rect)]
checkboxes = nodesOfType NodeButton

-- | Event-free input for a 400 by 240 window.
input :: Input
input = withInputOff 400 240

-- | Click the middle of a control.
clickOn :: Context -> NanoUI a -> Rect -> IO ()
clickOn ctx ui rect = void (runClick ctx input ui (spanCenter rect))

enabledForm :: Form Text Bool
enabledForm = inputCheckbox "enabled" False

readEnabled :: FormState -> Context -> Text -> IO Bool
readEnabled owner ctx prefix = do
  (_, result) <- runNanoUI ctx input (runNanoForm owner prefix enabledForm)
  case result of
    Ditto.Ok (Ditto.Proved _ value) -> pure value
    Ditto.Error _ -> fail "checkbox form unexpectedly failed validation"

testDeferredViews :: IO ()
testDeferredViews = do
  ctx <- newPixelContext
  owner <- newFormState
  let
    ui :: NanoUI ()
    ui = columnWith fillW $ do
      (left, _) <- runNanoForm owner "left" enabledForm
      (right, _) <- runNanoForm owner "right" enabledForm
      runFormView (Ditto.unView left [] <> Ditto.unView right [])
  _ <- warmup2 ctx input ui
  controls <- checkboxes ctx
  case controls of
    [(leftId, leftRect), (rightId, _)] -> do
      check
        "same-named fields in different forms share a widget ID"
        (leftId /= rightId)
      clickOn ctx ui leftRect
      _ <- warmup2 ctx input ui
      check "deferred view wrote to the wrong form" =<< readEnabled owner ctx "left"
      check "editing one form changed another form" . not =<< readEnabled owner ctx "right"
    _ -> fail "expected two deferred checkbox fields"

testNestedViews :: IO ()
testNestedViews = do
  ctx <- newPixelContext
  owner <- newFormState
  let
    nested = Ditto.view (FormView (void (nanoFormLive owner "inner" enabledForm)))
    outer = nested *> enabledForm
    ui = nanoFormLive owner "outer" outer
  _ <- warmup2 ctx input ui
  controls <- checkboxes ctx
  case controls of
    [_, (_, outerRect)] -> do
      clickOn ctx ui outerRect
      _ <- warmup2 ctx input ui
      check "field following a nested form lost its owner" =<< readEnabled owner ctx "outer"
      check "outer field wrote to the nested form" . not =<< readEnabled owner ctx "inner"
    _ -> fail "expected nested and outer checkbox fields"

testPrefixRestoration :: IO ()
testPrefixRestoration = do
  ctx <- newPixelContext
  owner <- newFormState
  (prefix, result) <- runNanoUI ctx input $ runFormUI owner "outer" $ do
    result <- liftNanoUI . liftIO . try $ runNanoUI ctx input $ runFormUI owner "inner" $ do
      FormScope innerOwner innerPrefix <- askFormScope
      liftNanoUI $ liftIO $ do
        updateFieldInput innerOwner ctx innerPrefix "value" (FormInputText "preserved")
        ioError (userError "form failed")
    FormScope _ prefix <- askFormScope
    pure (prefix, result :: Either IOException ())
  check "expected a form exception" (either (const True) (const False) result)
  check "exception changed the enclosing form owner" (prefix == "outer")
  store <- getFormStore owner "inner"
  check
    "prefix restoration discarded field updates"
    (Map.lookup "value" (fssInputs store) == Just (FormInputText "preserved"))

testFormInvalidation :: IO ()
testFormInvalidation = do
  ctx <- newPixelContext
  owner <- newFormState
  forM_
    [ updateFieldInput owner ctx "form" "field" (FormInputText "value")
    , markFormSubmitted owner ctx "form" True
    , resetFormState owner ctx "form"
    ]
    $ \update -> do
      clearDirty ctx
      update
      check "form mutation did not request a redraw" =<< isDirty ctx
      clearDirty ctx
      update
      check "an unchanged form mutation requested another redraw" . not
        =<< isDirty ctx

testSubmitPulse :: IO ()
testSubmitPulse = do
  ctx <- newPixelContext
  owner <- newFormState
  let
    ui = nanoFormSubmit owner "submit" "Save" (pure (42 :: Int))
  initial <- warmup2 ctx input ui
  check "form submitted before activation" (initial == Nothing)
  (submitted, _, _) <-
    runFrame ctx (keyInp KeyEnter input) ui
  check "valid submission did not return its value" (submitted == Just 42)
  idle <- warmup2 ctx input ui
  check "a submitted form kept emitting values on idle frames" (idle == Nothing)

-- | Text typed in the frame that presses Enter is part of the submission,
-- both for a field that publishes each edit and one that publishes on Enter.
testSubmitSameFrameEdit :: IO ()
testSubmitSameFrameEdit =
  forM_
    [ ("submit-typed", inputText "query" "")
    , ( "submit-on-enter"
      , inputWidget (Just "query") decodeText (OnlyWhen respSubmitted) FormInputText textInput' ""
      )
    ]
    $ \(prefix, field) -> do
      ctx <- newPixelContext
      owner <- newFormState
      let
        ui = nanoFormSubmit owner prefix "Save" (field `prove` notEmpty "Required")
      _ <- warmup2 ctx input ui
      controls <- nodesOfType NodeTextInput ctx
      case controls of
        [(wid, _)] -> do
          writeIORef (ctxFocusId ctx) wid
          _ <- warmup2 ctx input ui
          (submitted, _, _) <-
            runFrame ctx (keyInp KeyEnter input {inputChars = "abc"}) ui
          check
            ("Enter submitted the text from before its frame's typing: " <> show prefix)
            (submitted == Just "abc")
        _ -> fail "expected one submit-test text input"
 where
  decodeText (FormInputText t) = Right t
  decodeText _ = Left ("Expected text" :: Text)

-- | Enter in a focused text area inserts a newline; it does not submit.
testSubmitSkipsTextArea :: IO ()
testSubmitSkipsTextArea = do
  ctx <- newPixelContext
  owner <- newFormState
  let
    ui = nanoFormSubmit owner "submit-notes" "Save" (inputTextArea "notes" "")
  _ <- warmup2 ctx input ui
  controls <- nodesOfType NodeTextArea ctx
  case controls of
    [(wid, _)] -> do
      writeIORef (ctxFocusId ctx) wid
      _ <- warmup2 ctx input ui
      (submitted, _, _) <-
        runFrame ctx (keyInp KeyEnter input) ui
      check "Enter in a focused text area submitted the form" (submitted == Nothing)
    _ -> fail "expected one submit-test text area"

-- | Brightening a translucent colour field keeps its alpha.
testColorKeepsAlpha :: IO ()
testColorKeepsAlpha = do
  ctx <- newPixelContext
  owner <- newFormState
  let
    overlay = colorRGBA 0 0 0 128
    ui = nanoFormLive owner "colour" (inputColor "Overlay" overlay)
  _ <- warmup2 ctx input ui
  -- The picker's first part is its saturation / value field.
  controls <- nodesOfType NodeColorPicker ctx
  case controls of
    (wid, _) : _ -> do
      writeIORef (ctxFocusId ctx) wid
      _ <- warmup2 ctx input ui
      void (runFrame ctx (keyInp KeyUp input) ui)
      edited <- warmup2 ctx input ui
      check "Up did not brighten the colour" (edited /= Just overlay)
      check "a colour field dropped its alpha" (fmap colorA edited == Just 128)
    _ -> fail "expected a colour picker"

-- | The app's own button submits a 'FormOnSubmit' form through
-- 'markSubmitted', and the form then shows its errors.
testMarkSubmitted :: IO ()
testMarkSubmitted = do
  ctx <- newPixelContext
  owner <- newFormState
  let
    form = withFieldErrors (inputText "name" "" `prove` notEmpty "Name is required")
    ui = nanoFormEx owner defaultFormConfig {fcMode = FormOnSubmit} "on-submit" form
    -- The message wraps, so look for its last word.
    errorShown = hasText "required" <$> collectTextSpans ctx
  _ <- warmup2 ctx input ui
  check "a FormOnSubmit form showed errors before submission" . not =<< errorShown
  runNanoUI ctx input (markSubmitted owner "on-submit")
  _ <- warmup2 ctx input ui
  check "markSubmitted did not show the form's errors" =<< errorShown

testResetWidgets :: IO ()
testResetWidgets = do
  ctx <- newPixelContext
  owner <- newFormState
  let
    ui =
      columnWith fillW $
        (,)
          <$> nanoFormLive owner "reset-left" enabledForm
          <*> nanoFormLive owner "reset-right" enabledForm
  _ <- warmup2 ctx input ui
  controls <- checkboxes ctx
  case controls of
    [(_, leftRect), (rightId, rightRect)] -> do
      forM_ [leftRect, rightRect] (clickOn ctx ui)
      edited <- warmup2 ctx input ui
      check "checkboxes did not retain their edits" (edited == (Just True, Just True))
      runNanoUI ctx input (resetForm owner "reset-left")
      reset <- warmup2 ctx input ui
      check
        "reset did not restore defaults or changed another form"
        (reset == (Just False, Just True))
      after <- checkboxes ctx
      check
        "reset changed another form's widget identity"
        (map fst (drop 1 after) == [rightId])
      setFormStore owner ctx "reset-left" emptyFormStateStore
      persisted <- warmup2 ctx input ui
      check
        "writing form data revived a retired widget cache"
        (persisted == (Just False, Just True))
    _ -> fail "expected two reset-test checkboxes"

testResetTextArea :: IO ()
testResetTextArea = do
  ctx <- newPixelContext
  owner <- newFormState
  let
    ui = nanoFormLive owner "reset-editor" (inputTextArea "notes" "initial")
  _ <- warmup2 ctx input ui
  controls <- nodesOfType NodeTextArea ctx
  case controls of
    [(wid, _)] -> do
      writeIORef (ctxFocusId ctx) wid
      void (runFrame ctx input {inputChars = "edited"} ui)
      edited <- warmup2 ctx input ui
      check "text area did not retain its edit" (edited == Just "editedinitial")
      runNanoUI ctx input (resetForm owner "reset-editor")
      reset <- warmup2 ctx input ui
      check "reset retained the text area's cached buffer" (reset == Just "initial")
    _ -> fail "expected one reset-test text area"
