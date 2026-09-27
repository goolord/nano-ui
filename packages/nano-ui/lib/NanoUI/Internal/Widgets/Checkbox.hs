-- | Checkbox control: a button in the choice look ('buttonFlagChoice').
module NanoUI.Internal.Widgets.Checkbox (checkbox, checkbox', checkboxWith, checkboxWith') where

import Control.Monad (when)
import Data.Text (Text)
import NanoUI.Internal.Context (adoptSlot, registerFocusable)
import NanoUI.Internal.Layout.Arena (NodeType (..))
import NanoUI.Internal.Monad (NanoUI, freshWidget, liftIO)
import NanoUI.Internal.Store (fieldInt, boolInt, intBool)
import NanoUI.Internal.Style (Layout, defaultLayout)
import NanoUI.Internal.WidgetText (buttonFlagChoice)
import NanoUI.Internal.Widgets.Combinators (finishToggle)
import NanoUI.Internal.Widgets.Node (Response, addWidgetStyled, setWidgetValue)

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
checkboxWith' f txt checked = do
  (wid, ctx) <- freshWidget
  liftIO $ registerFocusable ctx wid
  current <- intBool <$> liftIO (adoptSlot fieldInt ctx wid (boolInt checked))
  resp <- addWidgetStyled wid NodeButton txt (if current then 1 else 0) (f defaultLayout) buttonFlagChoice
  result@(_, value) <- finishToggle ctx wid current resp
  -- The box shows this frame's click.
  when (value /= current) $ liftIO (setWidgetValue ctx wid (if value then 1 else 0))
  pure result
