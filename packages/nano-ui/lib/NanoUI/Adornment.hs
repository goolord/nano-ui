-- | Adornments: icons, short texts and views a text field draws before or
-- after its value, and a button beside its label. They are values, combined
-- side by side with '<>', and set in the widget's configuration. The names
-- are short, so import the module qualified:
--
-- > import NanoUI.Adornment qualified as A
-- >
-- > email' <- textInputConfigured defaultTextInputConfig
-- >   { ticAdornments = A.leading (A.icon mailIcon) <> A.trailing (A.affix "@example.com") } email
-- > password' <- textInputConfigured defaultTextInputConfig
-- >   { ticPassword = not shown
-- >   , ticAdornments = A.trailing (A.control (whenM (buttonWith tight "Show") (setShown (not shown))))
-- >   } password
--
-- The widget lays each side out as a row inside its own box, the layout's gap
-- from its text and between adornments, and draws them in its text colour,
-- muted in a field. A press on an icon, a text or a 'view' is a press on the
-- widget. A 'control' takes its own presses: the widget around it does not
-- report them, and a text field keeps its focus and caret.
module NanoUI.Adornment
  ( Adornment
  , Adornments (..)
  , leading
  , trailing
  , icon
  , iconSized
  , affix
  , view
  , control
  )
where

import NanoUI.Internal.Widgets.Adornment (Adornment, Adornments (..), affix, control, icon, iconSized, leading, trailing, view)
