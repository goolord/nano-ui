{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Form.Widgets
  ( defaultErrorView
  , formContainer
  , formRow
  , formField
  , formGroup
  ) where

import Control.Monad (forM_)
import Data.Text (Text)
import NanoUI
  ( AlignY (AlignMiddle)
  , Layout (..)
  , Sizing (Grow)
  , card
  , colorRGBA
  , column'
  , danger
  , defaultLayout
  , fillW
  , gap
  , heading
  , label_
  , padXY
  , panelStyledWith
  , row'
  )
import NanoUI.Form.Types (FormView (..))

-- | Standard error view rendering a styled error callout directly below invalid fields.
defaultErrorView :: [Text] -> FormView
defaultErrorView [] = FormView (pure ())
defaultErrorView errs = FormView $ do
  let errBorder = colorRGBA 239 68 68 255
      errBg     = colorRGBA 48 20 22 255
  panelStyledWith errBg errBorder (padXY 8 4 . gap 2 . fillW) $ do
    forM_ errs $ \err ->
      danger ("• " <> err)

-- | Wrap a form view in a flex-growing column with standard form gap.
formContainer :: FormView -> FormView
formContainer (FormView inner) = FormView $ do
  column'
    (defaultLayout
      { layoutGap = 10
      , layoutWidth = Grow 1
      }
    )
    inner

-- | Horizontal layout putting a field label on the left and form control on the right.
formRow :: Text -> FormView -> FormView
formRow lbl (FormView inner) = FormView $ do
  row'
    (defaultLayout
      { layoutGap = 8
      , layoutWidth = Grow 1
      , layoutAlignY = AlignMiddle
      }
    )
    $ do
      label_ lbl
      inner

-- | Vertical field layout placing a label directly above the form control.
formField :: Text -> FormView -> FormView
formField lbl (FormView inner) = FormView $ do
  column'
    (defaultLayout
      { layoutGap = 3
      , layoutWidth = Grow 1
      }
    )
    $ do
      label_ lbl
      inner

-- | Group related form fields into a titled visual card.
formGroup :: Text -> FormView -> FormView
formGroup title (FormView inner) = FormView $ do
  card $ do
    heading title
    column'
      (defaultLayout
        { layoutGap = 6
        , layoutWidth = Grow 1
        }
      )
      inner
