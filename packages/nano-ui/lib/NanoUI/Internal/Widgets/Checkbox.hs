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

import Data.Text (Text)
import NanoUI.Internal.Monad (NanoUI, styled)
import NanoUI.Internal.Style (Layout, Theme (..), Tone, defaultLayout, readableOn, toneColor)
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

-- | A button that stays pressed: filled in the tone's colour while on, a
-- plain button while off. Pass whether it is on; the result is the state
-- after this frame's click or Space/Enter.
--
-- > muted' <- toggleButton Warning "M" muted
{-# INLINE toggleButton #-}
toggleButton :: Tone -> Text -> Bool -> NanoUI Bool
toggleButton t txt on = snd <$> toggleButton' t txt on

-- | 'toggleButton' returning @(response, on)@.
{-# INLINE toggleButton' #-}
toggleButton' :: Tone -> Text -> Bool -> NanoUI (Response, Bool)
toggleButton' = toggleButtonWith' id

-- | 'toggleButton' with a layout modifier: @toggleButtonWith (fixedWH 32 30)@.
{-# INLINE toggleButtonWith #-}
toggleButtonWith :: (Layout -> Layout) -> Tone -> Text -> Bool -> NanoUI Bool
toggleButtonWith f t txt on = snd <$> toggleButtonWith' f t txt on

-- | 'toggleButtonWith' returning @(response, on)@.
toggleButtonWith' :: (Layout -> Layout) -> Tone -> Text -> Bool -> NanoUI (Response, Bool)
toggleButtonWith' f t txt on =
  -- A button drawn with a value fills in the accent; this one's is the tone.
  styled (\th -> let c = toneColor th t in th {themeAccent = c, themeOnAccent = readableOn th c}) $
    choiceToggleWidget txt (f defaultLayout) 0 on
