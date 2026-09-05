{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Form.Types
  ( FormView (..)
  , FormInput (..)
  , formInputToText
  , FormStateStore (..)
  , emptyFormStateStore
  , FormUI (..)
  , liftNanoUI
  , Form
  , FormStatus (..)
  , FormMode (..)
  , FormConfig (..)
  , defaultFormConfig
  ) where

import Data.Text (Text)
import qualified Ditto.Core as Ditto
import Ditto.Types (FormRange)
import NanoUI (NanoUI)
import NanoUI.Form.Backend
  ( FormInput (..)
  , FormStateStore (..)
  , FormUI (..)
  , emptyFormStateStore
  , formInputToText
  , liftNanoUI
  )

-- | View representation for forms in nano-ui.
-- Forms compose sequentially via '<*>' by sequencing their widget rendering actions.
newtype FormView = FormView { runFormView :: NanoUI () }

instance Semigroup FormView where
  FormView a <> FormView b = FormView (a >> b)

instance Monoid FormView where
  mempty = FormView (pure ())

-- | Type alias for a form producing @a@ with error type @err@.
type Form err a = Ditto.Form FormUI FormInput err FormView a

-- | Outcome of evaluating a form.
data FormStatus a
  = FormIdle !a
  | FormValid !a
  | FormInvalid ![(FormRange, Text)]
  deriving stock (Eq, Show, Functor)

-- | Validation mode for a form.
data FormMode
  = FormLive
  | FormOnSubmit
  deriving stock (Eq, Show)

-- | Configuration options for form execution.
data FormConfig = FormConfig
  { fcMode         :: !FormMode
  , fcSubmitButton :: !(Maybe Text)
  } deriving stock (Eq, Show)

-- | Default form configuration (live validation, no extra submit button).
defaultFormConfig :: FormConfig
defaultFormConfig = FormConfig
  { fcMode = FormLive
  , fcSubmitButton = Nothing
  }
