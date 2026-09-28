-- | Checkbox control: a button in the choice look ('buttonFlagChoice').
module NanoUI.Internal.Widgets.Checkbox (checkbox, checkbox', checkboxWith, checkboxWith') where

import Data.Text (Text)
import NanoUI.Internal.Monad (NanoUI)
import NanoUI.Internal.Style (Layout, defaultLayout)
import NanoUI.Internal.WidgetText (buttonFlagChoice)
import NanoUI.Internal.Widgets.Combinators (choiceToggleWidget)
import NanoUI.Internal.Widgets.Node (Response)

-- | Checkbox with a caption. Pass whether it is checked; the result is the
-- state after this frame's click or Space/Enter.
{-# INLINE checkbox #-}
checkbox :: Text -> Bool -> NanoUI Bool
checkbox txt checked = snd <$> checkbox' txt checked

-- | 'checkbox' returning @(response, checked)@. Store the returned flag each frame.
{-# INLINE checkbox' #-}
checkbox' :: Text -> Bool -> NanoUI (Response, Bool)
checkbox' = checkboxWith' id

-- | 'checkbox' with a layout modifier: @checkboxWith alignMid@ centres it in
-- a row taller than itself, and @checkboxWith fillW@ gives it the row.
{-# INLINE checkboxWith #-}
checkboxWith :: (Layout -> Layout) -> Text -> Bool -> NanoUI Bool
checkboxWith f txt checked = snd <$> checkboxWith' f txt checked

-- | 'checkboxWith' returning @(response, checked)@.
checkboxWith' :: (Layout -> Layout) -> Text -> Bool -> NanoUI (Response, Bool)
checkboxWith' f txt checked =
  choiceToggleWidget txt (f defaultLayout) buttonFlagChoice checked
