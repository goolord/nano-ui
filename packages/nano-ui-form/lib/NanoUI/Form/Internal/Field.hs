-- | Shared widget-to-form plumbing. Naming and validation stay with ditto;
-- this module only adapts immediate-mode controls to persistent field values.
module NanoUI.Form.Internal.Field
  ( inputWidget
  , Publish (..)
  , labelled
  , fieldView
  , decodeBool
  , decodeFloatInput
  , decodeInt
  , fieldErrors
  , enumField
  )
where

import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Ditto.Backend (FormError)
import Ditto.Core qualified as Ditto
import Ditto.Generalized.Named qualified as Named
import Ditto.Generalized.Unnamed qualified as Unnamed
import Ditto.Types (FormId, encodeFormId)
import NanoUI
  ( NanoUI
  , Response
  , columnWith
  , fillW
  , gap
  , tight
  , liftIO
  , withKey
  )
import NanoUI qualified as NUI
import NanoUI.Form.Internal.Backend
  ( FormInput (..)
   , FormScope (..)
   , askFormScope
  , fieldDraft
  , setFieldDraft
  , updateFieldInput
  )
import NanoUI.Form.Types (Form, FormView (..))
import NanoUI.Form.Widgets (defaultErrorView)
import NanoUI.Internal.Monad (askContext)
import Text.Read (readMaybe)

-- | When a field's control writes its value into the form.
data Publish
  = -- | Whenever the control returns a value other than the one it was given.
    OnChange
  | -- | As 'OnChange', and also when the response reports an edit that kept
    -- the value, such as re-picking the selected option. Built-in inputs use
    -- @OnChangeOr respChanged@.
    OnChangeOr (Response -> Bool)
  | -- | Only when the response says so, such as @OnlyWhen respSubmitted@ for a
    -- text field that commits on Enter. The form keeps its last published
    -- value in between; the field keeps the edit and shows it to the control,
    -- until it publishes, the form value changes elsewhere, or the form resets.
    OnlyWhen (Response -> Bool)

-- | Adapt a controlled widget to a form. 'Just' supplies a stable field name;
-- 'Nothing' asks ditto to number it. Naming adds no visible label: include one
-- in the widget action when wanted. 'Publish' says when an edit reaches the
-- form. Decoding and validation errors follow ditto's normal path.
inputWidget ::
  (Eq a, FormError FormInput err) =>
  Maybe Text
  -> (FormInput -> Either err a)
  -> Publish
  -> (a -> FormInput)
  -> (a -> NanoUI (Response, a))
  -> a
  -> Form err a
inputWidget name decode publish encode widget initial = do
  owner <- Ditto.liftForm askFormScope
  let draftValue = either (const Nothing) Just . decode
  maybe Unnamed.input Named.input name decode (fieldView owner draftValue publish encode widget) initial

labelled :: Maybe Text -> (a -> NanoUI b) -> a -> NanoUI b
labelled caption widget value = mapM_ NUI.label caption >> widget value

-- | Keep the label and control in the same stable field scope, and publish
-- edits by the field's policy.
fieldView ::
  Eq a =>
  FormScope
  -> (FormInput -> Maybe a)
  -> Publish
  -> (a -> FormInput)
  -> (a -> NanoUI (Response, a))
  -> FormId
  -> a
  -> FormView
fieldView (FormScope owner prefix) draftValue publish encode widget formId value = FormView $ withKey fieldKey $ do
  ctx <- askContext
  case publish of
    OnlyWhen commit -> do
      let current = encode value
      pending <- liftIO (fieldDraft owner prefix fieldKey)
      -- A draft begun from another form value is stale: the form changed.
      let shown = case pending of
            Just (base, draft) | base == current, Just held <- draftValue draft -> held
            _ -> value
      (response, newValue) <- widget shown
      liftIO $
        if commit response
          then do
            setFieldDraft owner ctx prefix fieldKey Nothing
            updateFieldInput owner ctx prefix fieldKey (encode newValue)
          else
            setFieldDraft owner ctx prefix fieldKey $
              if newValue == value then Nothing else Just (current, encode newValue)
    _ -> do
      (response, newValue) <- widget value
      let signalled = case publish of
            OnChangeOr changed -> changed response
            _ -> False
      when (signalled || newValue /= value) $
        liftIO (updateFieldInput owner ctx prefix fieldKey (encode newValue))
 where
  fieldKey = encodeFormId formId

decodeBool :: Bool -> FormInput -> Bool
decodeBool _ (FormInputBool value) = value
decodeBool _ (FormInputText value) = value == "true"
decodeBool initial _ = initial

decodeFloatInput :: Float -> FormInput -> Float
decodeFloatInput _ (FormInputFloat value) = value
decodeFloatInput initial (FormInputText value) = fromMaybe initial (readMaybe (T.unpack value))
decodeFloatInput initial _ = initial

decodeInt :: Int -> FormInput -> Int
decodeInt _ (FormInputInt value) = value
decodeInt initial (FormInputText value) = fromMaybe initial (readMaybe (T.unpack value))
decodeInt initial _ = initial

fieldErrors :: FormView -> [Text] -> FormView
fieldErrors (FormView widget) errs = FormView $
  columnWith (tight . gap 4 . fillW) $ do
    widget
    runFormView (defaultErrorView errs)

-- | Widget indices are zero-based even when an Enum's bounds are not.
enumField ::
  forall a f.
  (Bounded a, Enum a, Show a, Functor f) => ([Text] -> Int -> f Int) -> a -> f a
enumField widget initial =
  fromIndex <$> widget options (fromEnum initial - lower)
 where
  values = [minBound .. maxBound] :: [a]
  options = map (T.pack . show) values
  lower = fromEnum (minBound :: a)
  fromIndex index = toEnum (lower + max 0 (min (length values - 1) index))
