-- | Boolean buttons: a checkbox, a button in the choice look
-- ('buttonFlagChoice'), and a toggle button, filled while it is on.
module NanoUI.Internal.Widgets.Checkbox
  ( checkbox
  , checkbox'
  , checkboxWith
  , checkboxWith'
  , toggleButton
  , toggleButton'
  , toggleButtonWith
  , toggleButtonWith'
  ) where

import Data.Functor (void)
import Data.Text (Text)
import NanoUI.Internal.Monad (NanoUI)
import NanoUI.Internal.Var (Binding (..), bindVar)
import NanoUI.Internal.Var.Types (Var)
import NanoUI.Internal.Style (Layout, Tone, defaultLayout)
import NanoUI.Internal.WidgetText (buttonFlagChoice, buttonToneStyle)
import NanoUI.Internal.Widgets.Combinators (choiceToggleWidget)
import NanoUI.Internal.Widgets.Node (Response)

-- | Checkbox with a caption, showing and editing a Var: a click or
-- Space/Enter flips it.
--
-- > checkbox "Dark mode" settings.darkMode
{-# INLINE checkbox #-}
checkbox :: Text -> Var Bool -> NanoUI ()
checkbox txt v = void (checkbox' txt v)

-- | 'checkbox' returning its 'Response'.
{-# INLINE checkbox' #-}
checkbox' :: Text -> Var Bool -> NanoUI Response
checkbox' = checkboxWith' id

-- | 'checkbox' with a layout modifier: @checkboxWith alignMid@ centres it in
-- a row taller than itself, and @checkboxWith fillW@ gives it the row.
{-# INLINE checkboxWith #-}
checkboxWith :: (Layout -> Layout) -> Text -> Var Bool -> NanoUI ()
checkboxWith f txt v = void (checkboxWith' f txt v)

-- | 'checkboxWith' returning its 'Response'.
{-# INLINE checkboxWith' #-}
checkboxWith' :: (Layout -> Layout) -> Text -> Var Bool -> NanoUI Response
checkboxWith' f txt v =
  bindVar RenderFinal v (choiceToggleWidget txt (f defaultLayout) buttonFlagChoice)

-- | A button that stays pressed: filled in the tone's colour while on, a
-- plain button while off. A click or Space/Enter flips the Var.
--
-- > toggleButton Warning "M" muted
{-# INLINE toggleButton #-}
toggleButton :: Tone -> Text -> Var Bool -> NanoUI ()
toggleButton t txt v = void (toggleButton' t txt v)

-- | 'toggleButton' returning its 'Response'.
{-# INLINE toggleButton' #-}
toggleButton' :: Tone -> Text -> Var Bool -> NanoUI Response
toggleButton' = toggleButtonWith' id

-- | 'toggleButton' with a layout modifier: @toggleButtonWith (fixedWH 32 30)@.
{-# INLINE toggleButtonWith #-}
toggleButtonWith :: (Layout -> Layout) -> Tone -> Text -> Var Bool -> NanoUI ()
toggleButtonWith f t txt v = void (toggleButtonWith' f t txt v)

-- | 'toggleButtonWith' returning its 'Response'.
{-# INLINE toggleButtonWith' #-}
toggleButtonWith' :: (Layout -> Layout) -> Tone -> Text -> Var Bool -> NanoUI Response
toggleButtonWith' f t txt v = bindVar RenderFinal v (choiceToggleWidget txt (f defaultLayout) (buttonToneStyle t))
