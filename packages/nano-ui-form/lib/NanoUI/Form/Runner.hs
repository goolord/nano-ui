{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Form.Runner
  ( runNanoForm
  , nanoFormLive
  , nanoFormSubmit
  , nanoFormEx
  , resetForm
  ) where

import Control.Monad (when)
import Data.Text (Text)
import qualified Ditto.Core as Ditto
import qualified Ditto.Types as Ditto
import NanoUI
  ( Key (KeyEnter)
  , NanoUI
  , button
  , column'
  , defaultLayout
  , inputKeys
  , inputKeysElem
  , uiIO
  , whenM
  )
import NanoUI.Monad (askContext, askInput)
import NanoUI.Form.Backend
  ( isFormSubmitted
  , markFormSubmitted
  , resetFormState
  , withFormPrefix
  , withFormWidgets
  )
import NanoUI.Form.Types
  ( Form
  , FormConfig (..)
  , FormMode (..)
  , FormStatus (..)
  , FormView (..)
  , defaultFormConfig
  )
import NanoUI.Form.Backend (FormUI (..))

-- | Evaluate a formlet and return its view and result. The view retains its
-- prefix even when rendered after other forms or inside another form's view.
runNanoForm :: Text -> Form err a -> NanoUI (Ditto.View err FormView, Ditto.Result err (Ditto.Proved a))
runNanoForm prefix form = withNanoForm prefix form $ \view result -> do
  let scopedView (FormView action) = FormView (withFormPrefix prefix action)
  pure (scopedView <$> view, result)

-- Immediate runners evaluate and render in one prefix scope. Only a deferred
-- view returned by runNanoForm needs to re-enter that scope when it is rendered.
withNanoForm ::
  Text
  -> Form err a
  -> (Ditto.View err FormView -> Ditto.Result err (Ditto.Proved a) -> NanoUI b)
  -> NanoUI b
withNanoForm prefix form consume = withFormPrefix prefix $ do
  (view, result) <- unFormUI (Ditto.runForm prefix form)
  let keyedView (FormView action) = FormView (withFormWidgets prefix action)
  consume (keyedView <$> view) result

-- | Default form runner: renders the form every frame with live validation
-- and yields @Just a@ whenever it is valid.
nanoFormLive :: Text -> Form Text a -> NanoUI (Maybe a)
nanoFormLive prefix form = do
  status <- nanoFormEx defaultFormConfig prefix form
  pure $ case status of
    FormValid a -> Just a
    FormInvalid _ -> Nothing

-- | Run a form with an integrated submit button.
-- Validation errors are only displayed after the first submission attempt.
-- Returns @Just a@ only on a valid submission.
nanoFormSubmit :: Text -> Text -> Form Text a -> NanoUI (Maybe a)
nanoFormSubmit prefix submitLabel form = do
  ctx <- askContext
  inp <- askInput
  submittedBefore <- uiIO (isFormSubmitted ctx prefix)
  withNanoForm prefix form $ \view' res -> do
    btnClicked <- column' defaultLayout $ do
      renderResult submittedBefore view' res
      button submitLabel
    let enterPressed = inputKeysElem KeyEnter (inputKeys inp)
        clickedSubmit = btnClicked || enterPressed
    when clickedSubmit $
      uiIO (markFormSubmitted ctx prefix True)
    pure $ case (clickedSubmit, res) of
      (True, Ditto.Ok (Ditto.Proved _ a)) -> Just a
      _                                  -> Nothing

-- | Detailed form runner with custom configuration.
nanoFormEx :: FormConfig -> Text -> Form Text a -> NanoUI (FormStatus a)
nanoFormEx cfg prefix form = do
  ctx <- askContext
  submittedBefore <- uiIO (isFormSubmitted ctx prefix)
  withNanoForm prefix form $ \view' res -> do
    let showErrors = case fcMode cfg of
          FormLive     -> True
          FormOnSubmit -> submittedBefore
    column' defaultLayout $ do
      renderResult showErrors view' res
      case fcSubmitButton cfg of
        Just lbl ->
          whenM (button lbl) $
            uiIO (markFormSubmitted ctx prefix True)
        Nothing -> pure ()
    pure $ case res of
      Ditto.Ok (Ditto.Proved _ a) -> FormValid a
      Ditto.Error errs -> FormInvalid errs

renderResult :: Bool -> Ditto.View err FormView -> Ditto.Result err a -> NanoUI ()
renderResult showErrors view result =
  runFormView (Ditto.unView view errorsToShow)
  where
    errorsToShow = case result of
      Ditto.Error errs | showErrors -> errs
      _ -> []

-- | Reset input values and the corresponding widget state for a form prefix.
-- Re-evaluate the form on the next frame to render its defaults.
resetForm :: Text -> NanoUI ()
resetForm prefix = do
  ctx <- askContext
  uiIO (resetFormState ctx prefix)
