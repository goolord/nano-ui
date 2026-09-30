-- | Running a form in a view: live validation, a submit button, a configured
-- runner, a deferred view, and reset.
module NanoUI.Form.Runner
  ( runNanoForm
  , nanoFormLive
  , nanoFormSubmit
  , nanoFormEx
  , resetForm
  ) where

import Control.Monad (when)
import Data.Foldable (for_)
import Data.Text (Text)
import qualified Ditto.Core as Ditto
import qualified Ditto.Types as Ditto
import NanoUI
  ( Key (KeyEnter)
  , NanoUI
  , button
  , column
  , Pressable (..)
  , liftIO
  , whenM
  )
import NanoUI.Internal.Context
  ( FocusKind (..)
  , InteractionState (isFocusKind)
  , getsInteraction
  , pointerBlockedByModal
  )
import NanoUI.Internal.Monad (askContext, withContext)
import NanoUI.Monad (askInput)
import NanoUI.Form.Internal.Backend
  ( FormState
  , runFormUI
  , isFormSubmitted
  , markFormSubmitted
  , resetFormState
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

-- | Evaluate a formlet and return its view and result. The view retains its
-- prefix even when rendered after other forms or inside another form's view.
runNanoForm :: FormState -> Text -> Form err a -> NanoUI (Ditto.View err FormView, Ditto.Result err (Ditto.Proved a))
runNanoForm owner prefix form = withNanoForm owner prefix form $ \view result ->
  pure (view, result)

-- Field views capture their typed owner when the form is evaluated, so even
-- deferred and nested views write to the right form without ambient state.
withNanoForm ::
  FormState
  -> Text
  -> Form err a
  -> (Ditto.View err FormView -> Ditto.Result err (Ditto.Proved a) -> NanoUI b)
  -> NanoUI b
withNanoForm owner prefix form consume = do
  (view, result) <- runFormUI owner prefix (Ditto.runForm prefix form)
  let keyedView (FormView action) = FormView (withFormWidgets owner prefix action)
  consume (keyedView <$> view) result

-- | Default form runner: renders the form every frame with live validation
-- and yields @Just a@ whenever it is valid.
nanoFormLive :: FormState -> Text -> Form Text a -> NanoUI (Maybe a)
nanoFormLive owner prefix form = do
  status <- nanoFormEx owner defaultFormConfig prefix form
  pure $ case status of
    FormValid a -> Just a
    FormInvalid _ -> Nothing

-- | Run a form with an integrated submit button. Arguments are stable form
-- prefix, button label, and form. Enter also submits when nothing is focused
-- or a single-line field is, but not while a text area (a newline), an input
-- method, or another control (which Enter activates) has the key, nor behind
-- a modal. Enter is not tied to one form, so avoid treating multiple visible
-- forms as independent Enter targets.
-- Validation errors are only displayed after the first submission attempt.
-- Returns @Just a@ only on a valid submission.
nanoFormSubmit :: FormState -> Text -> Text -> Form Text a -> NanoUI (Maybe a)
nanoFormSubmit owner prefix submitLabel form = do
  ctx <- askContext
  inp <- askInput
  submittedBefore <- liftIO (isFormSubmitted owner prefix)
  withNanoForm owner prefix form $ \view' res -> do
    btnClicked <- column $ do
      renderResult submittedBefore view' res
      button submitLabel
    enterPressed <-
      if pressedOnceIn KeyEnter inp
        then liftIO $ do
          blocked <- pointerBlockedByModal ctx
          kind <- getsInteraction ctx isFocusKind
          pure (not blocked && (kind `elem` [FocusNone, FocusTextLine, FocusTextSelectable]))
        else pure False
    let clickedSubmit = btnClicked || enterPressed
    when clickedSubmit $
      liftIO (markFormSubmitted owner ctx prefix True)
    pure $ case (clickedSubmit, res) of
      (True, Ditto.Ok (Ditto.Proved _ a)) -> Just a
      _                                  -> Nothing

-- | Render with configured error visibility and an optional submit button.
-- Returns current validity every frame, not a one-frame submission event.
nanoFormEx :: FormState -> FormConfig -> Text -> Form Text a -> NanoUI (FormStatus a)
nanoFormEx owner cfg prefix form = do
  ctx <- askContext
  submittedBefore <- liftIO (isFormSubmitted owner prefix)
  withNanoForm owner prefix form $ \view' res -> do
    let showErrors = case fcMode cfg of
          FormLive     -> True
          FormOnSubmit -> submittedBefore
    column $ do
      renderResult showErrors view' res
      for_ (fcSubmitButton cfg) $ \lbl ->
        whenM (button lbl) (liftIO (markFormSubmitted owner ctx prefix True))
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
resetForm :: FormState -> Text -> NanoUI ()
resetForm owner prefix = withContext (\ctx -> resetFormState owner ctx prefix)
