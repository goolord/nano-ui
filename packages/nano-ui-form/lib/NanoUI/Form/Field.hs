-- | Shared widget-to-form plumbing. Naming and validation stay with ditto;
-- this module only adapts immediate-mode controls to persistent field values.
module NanoUI.Form.Field
  ( fieldView
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
import Ditto.Types (FormId, encodeFormId)
import NanoUI (NanoUI, Response, columnWith, fillW, gap, tight, uiIO, withKey)
import NanoUI.Form.Backend (FormInput (..), getActiveFormPrefix, updateFieldInput)
import NanoUI.Form.Types (FormView (..))
import NanoUI.Form.Widgets (defaultErrorView)
import NanoUI.Monad (askContext)
import Text.Read (readMaybe)

-- | Keep the label and control in the same stable field scope. Some controls
-- report activation rather than change, so callers supply the response flag.
fieldView ::
  Eq a =>
  (Response -> Bool)
  -> (a -> FormInput)
  -> (a -> NanoUI (Response, a))
  -> FormId
  -> a
  -> FormView
fieldView changed encode widget formId value = FormView $ withKey fieldKey $ do
  ctx <- askContext
  prefix <- uiIO (getActiveFormPrefix ctx)
  (response, newValue) <- widget value
  when (changed response || newValue /= value) $
    uiIO (updateFieldInput ctx prefix fieldKey (encode newValue))
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
