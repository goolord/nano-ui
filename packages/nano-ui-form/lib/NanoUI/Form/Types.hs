-- | Form, view, status and configuration types.
module NanoUI.Form.Types
  ( FormView (..)
  , Form
  , FieldName (..)
  , named
  , unnamed
  , FormStatus (..)
  , FormMode (..)
  , FormConfig (..)
  , defaultFormConfig
  )
where

import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Ditto.Core qualified as Ditto
import Ditto.Types (FormRange)
import NanoUI (NanoUI)
import NanoUI.Form.Backend (FormInput, FormUI)

-- | View representation for forms in nano-ui.
-- Forms compose sequentially via '<*>' by sequencing their widget rendering actions.
newtype FormView = FormView {runFormView :: NanoUI ()}

instance Semigroup FormView where
  FormView a <> FormView b = FormView (a >> b)

instance Monoid FormView where
  mempty = FormView (pure ())

-- | Type alias for a form producing @a@ with error type @err@.
type Form err a = Ditto.Form FormUI FormInput err FormView a

-- | Identity and caption for a built-in input. A string literal selects a
-- named, labelled field. 'unnamed' selects positional identity without a label.
-- Updating the caption leaves the form's persisted key unchanged.
data FieldName = FieldName
  { fieldKey :: !(Maybe Text)
  -- ^ 'Nothing' asks ditto to number the field; named keys must be unique in a form.
  , fieldLabel :: !(Maybe Text)
  -- ^ Optional caption, rendered above the control or beside a checkbox.
  }
  deriving (Eq, Show)

instance IsString FieldName where
  fromString = named . T.pack

-- | Use a dynamic text as both stable key and visible caption.
named :: Text -> FieldName
named name = FieldName (Just name) (Just name)

-- | A numbered input without a visible caption.
unnamed :: FieldName
unnamed = FieldName Nothing Nothing

-- | Outcome of evaluating a form.
data FormStatus a
  = FormValid !a
  | FormInvalid ![(FormRange, Text)]
  deriving stock (Eq, Show, Functor)

-- | Validation mode for a form.
data FormMode
  = FormLive
  | FormOnSubmit
  deriving stock (Eq, Show)

-- | Configuration options for form execution.
data FormConfig = FormConfig
  { fcMode :: !FormMode
  , fcSubmitButton :: !(Maybe Text)
  }
  deriving stock (Eq, Show)

-- | Default form configuration (live validation, no extra submit button).
defaultFormConfig :: FormConfig
defaultFormConfig =
  FormConfig
    { fcMode = FormLive
    , fcSubmitButton = Nothing
    }
