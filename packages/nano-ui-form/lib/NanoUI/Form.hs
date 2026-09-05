{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Form
  ( -- * Core Form Types
    Form
  , FormView (..)
  , FormInput (..)
  , FormUI (..)
  , liftNanoUI
  , FormStatus (..)
  , FormMode (..)
  , FormConfig (..)
  , defaultFormConfig

    -- * Named Form Inputs
  , inputText
  , inputTextWithPlaceholder
  , inputPassword
  , inputTextArea
  , inputCheckbox
  , inputSlider
  , inputSelect
  , inputEnumSelect
  , inputRadio
  , inputEnumRadio
  , inputColor
  , label
  , separator
  , errors
  , childErrors
  , withErrors
  , withChildErrors
  , withFieldErrors

    -- * Validation & Proofs
  , module NanoUI.Form.Validation

    -- * Presentation & Layout
  , module NanoUI.Form.Widgets

    -- * Form Runners
  , runNanoForm
  , nanoForm
  , nanoFormLive
  , nanoFormSubmit
  , nanoFormEx
  , resetForm

    -- * Re-exports from Ditto
  , Ditto.FormRange (..)
  , Ditto.FormId (..)
  , Ditto.Result (..)
  , Ditto.Proved (..)
  , Ditto.hoistForm
  , Ditto.view
  , Ditto.mapView
  , (Ditto.@$)
  ) where

import qualified Ditto.Core as Ditto
import qualified Ditto.Types as Ditto
import NanoUI.Form.Named
  ( childErrors
  , errors
  , inputCheckbox
  , inputColor
  , inputEnumRadio
  , inputEnumSelect
  , inputPassword
  , inputRadio
  , inputSelect
  , inputSlider
  , inputText
  , inputTextWithPlaceholder
  , inputTextArea
  , label
  , separator
  , withChildErrors
  , withErrors
  , withFieldErrors
  )
import NanoUI.Form.Runner
  ( nanoForm
  , nanoFormEx
  , nanoFormLive
  , nanoFormSubmit
  , resetForm
  , runNanoForm
  )
import NanoUI.Form.Types
  ( Form
  , FormConfig (..)
  , FormInput (..)
  , FormMode (..)
  , FormStatus (..)
  , FormUI (..)
  , FormView (..)
  , defaultFormConfig
  , liftNanoUI
  )
import NanoUI.Form.Validation
import NanoUI.Form.Widgets
