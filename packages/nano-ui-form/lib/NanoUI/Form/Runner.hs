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
  , respClicked
  , uiIO
  )
import NanoUI.Monad (askContext, askInput)
import NanoUI.Form.Backend
  ( isFormSubmitted
  , markFormSubmitted
  , resetFormState
  , setActiveFormPrefix
  )
import NanoUI.Form.Types
  ( Form
  , FormConfig (..)
  , FormMode (..)
  , FormStatus (..)
  , FormUI (..)
  , FormView (..)
  )

-- | Low-level runner: evaluates the formlet in 'FormUI', returning the raw 'FormView' and 'Ditto.Result'.
runNanoForm :: Text -> Form err a -> NanoUI (Ditto.View err FormView, Ditto.Result err (Ditto.Proved a))
runNanoForm prefix form = do
  ctx <- askContext
  uiIO $ setActiveFormPrefix ctx prefix
  unFormUI $ Ditto.runForm prefix form

-- | Default form runner: runs live validation and renders the form in 'NanoUI'.
nanoForm :: Text -> Form Text a -> NanoUI (Maybe a)
nanoForm = nanoFormLive

-- | Run a form with live validation: renders every frame and yields @Just a@ whenever valid.
nanoFormLive :: Text -> Form Text a -> NanoUI (Maybe a)
nanoFormLive prefix form = do
  (view', res) <- runNanoForm prefix form
  let renderedView = case res of
        Ditto.Error errs -> Ditto.unView view' errs
        Ditto.Ok _       -> Ditto.unView view' []
  column' defaultLayout (runFormView renderedView)
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
  let shouldShowErrors = submittedBefore
      renderedView = if shouldShowErrors
        then case res of
          Ditto.Error errs -> Ditto.unView view' errs
          Ditto.Ok _       -> Ditto.unView view' []
        else Ditto.unView view' []
  btnResp <- column' defaultLayout $ do
    runFormView renderedView
    button submitLabel
  let enterPressed = inputKeysElem KeyEnter (inputKeys inp)
      clickedSubmit = respClicked btnResp || enterPressed
  when clickedSubmit $
    uiIO (markFormSubmitted ctx prefix True)
  pure $ case (clickedSubmit || submittedBefore, res) of
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
      renderedView = if showErrors
        then case res of
          Ditto.Error errs -> Ditto.unView view' errs
          Ditto.Ok _       -> Ditto.unView view' []
        else Ditto.unView view' []
  column' defaultLayout $ do
    runFormView renderedView
    case fcSubmitButton cfg of
      Just lbl -> do
        resp <- button lbl
        when (respClicked resp) $
          uiIO (markFormSubmitted ctx prefix True)
      Nothing -> pure ()
  pure $ case res of
    Ditto.Ok (Ditto.Proved _ a) -> FormValid a
    Ditto.Error errs ->
      let textErrs = map (\(range, msg) -> (range, msg)) errs
       in FormInvalid textErrs

-- | Reset all stored input values for a form prefix.
resetForm :: Text -> NanoUI ()
resetForm prefix = do
  ctx <- askContext
  uiIO (resetFormState ctx prefix)
