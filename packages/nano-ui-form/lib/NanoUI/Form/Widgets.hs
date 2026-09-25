-- | Layout helpers for form views: containers, labelled rows and fields,
-- titled groups, and the default error view.
module NanoUI.Form.Widgets
  ( defaultErrorView
  , formContainer
  , formRow
  , formField
  , formGroup
  ) where

import Data.Text (Text)
import NanoUI
  ( alignMid
  , card
  , columnWith
  , danger
  , fillW
  , gap
  , heading
  , label
  , padXY
  , calloutWith
  , themeRed
  , uiTheme
  , rowWith
  )
import NanoUI.Form.Types (FormView (..))

-- | Standard error view rendering a styled error callout directly below invalid fields.
defaultErrorView :: Foldable f => f Text -> FormView
defaultErrorView errs | null errs = FormView (pure ())
defaultErrorView errs = FormView $ do
  errColor <- themeRed <$> uiTheme
  calloutWith errColor (padXY 8 4 . gap 2) (mapM_ (danger . ("• " <>)) errs)

-- | Wrap a form view in a flex-growing column with standard form gap.
formContainer :: FormView -> FormView
formContainer (FormView inner) = FormView (columnWith (gap 10 . fillW) inner)

-- | Horizontal layout putting a field label on the left and form control on the right.
formRow :: Text -> FormView -> FormView
formRow lbl (FormView inner) = FormView (rowWith (gap 8 . fillW . alignMid) (label lbl >> inner))

-- | Vertical field layout placing a label directly above the form control.
formField :: Text -> FormView -> FormView
formField lbl (FormView inner) = FormView (columnWith (gap 3 . fillW) (label lbl >> inner))

-- | Group related form fields into a titled visual card.
formGroup :: Text -> FormView -> FormView
formGroup title (FormView inner) = FormView (card (heading title >> columnWith (gap 6 . fillW) inner))
