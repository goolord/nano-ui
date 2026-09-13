{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Form.Runner
  ( runNanoForm
  , nanoForm
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
  , withKey
  )
import NanoUI.Monad (askContext, askInput)
import NanoUI.Form.Backend
  ( isFormSubmitted
  , markFormSubmitted
  , resetFormState
  , withFormPrefix
  )
import NanoUI.Form.Types
  ( Form
  , FormConfig (..)
  , FormMode (..)
  , FormStatus (..)
  , FormUI (..)
  , FormView (..)
  )

-- | Evaluate a formlet and return its view and result. The view retains its
-- prefix even when rendered after other forms or inside another form's view.
runNanoForm :: Text -> Form err a -> NanoUI (Ditto.View err FormView, Ditto.Result err (Ditto.Proved a))
runNanoForm prefix form = do
  (view, result) <- withFormPrefix prefix (unFormUI (Ditto.runForm prefix form))
  let scopedView (FormView action) =
        FormView (withFormPrefix prefix (withKey prefix action))
  pure (scopedView <$> view, result)

-- | Default form runner: runs live validation and renders the form in 'NanoUI'.
nanoForm :: Text -> Form Text a -> NanoUI (Maybe a)
nanoForm = nanoFormLive

-- | Run a form with live validation: renders every frame and yields @Just a@ whenever valid.
nanoFormLive :: Text -> Form Text a -> NanoUI (Maybe a)
nanoFormLive prefix form = do
  (view', res) <- runNanoForm prefix form
  column' defaultLayout (renderResult True view' res)
  pure $ case res of
    Ditto.Ok (Ditto.Proved _ a) -> Just a
    Ditto.Error _               -> Nothing

-- | Run a form with an integrated submit button.
-- Validation errors are only displayed after the first submission attempt.
-- Returns @Just a@ only on a valid submission.
nanoFormSubmit :: Text -> Text -> Form Text a -> NanoUI (Maybe a)
nanoFormSubmit prefix submitLabel form = do
  ctx <- askContext
  inp <- askInput
  submittedBefore <- uiIO (isFormSubmitted ctx prefix)
  (view', res) <- runNanoForm prefix form
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
  (view', res) <- runNanoForm prefix form
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

-- | Reset all stored input values for a form prefix.
resetForm :: Text -> NanoUI ()
resetForm prefix = do
  ctx <- askContext
  uiIO (resetFormState ctx prefix)
