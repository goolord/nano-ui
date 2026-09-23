-- | Checkbox control.
module NanoUI.Internal.Widgets.Checkbox (checkbox, checkbox', checkboxWith, checkboxWith') where

import Data.Text (Text)
import Effectful (Eff, type (:>))
import NanoUI.Internal.Context (adoptSlot, registerFocusable)
import NanoUI.Internal.Layout.Arena (NodeType (..))
import NanoUI.Internal.Monad (Ui, freshWidget, uiIO)
import NanoUI.Internal.Store (fieldInt, boolInt, intBool)
import NanoUI.Internal.Style (Layout, defaultLayout)
import NanoUI.Internal.Widgets.Combinators (finishToggle)
import NanoUI.Internal.Widgets.Node (Response, addWidget)

-- | Checkbox with a caption. Pass whether it is checked; the result is the
-- state after this frame's click or Space/Enter.
{-# INLINE checkbox #-}
checkbox :: Ui :> es => Text -> Bool -> Eff es Bool
checkbox txt checked = snd <$> checkbox' txt checked

-- | 'checkbox' returning @(response, checked)@. Store the returned flag each frame.
{-# INLINE checkbox' #-}
checkbox' :: Ui :> es => Text -> Bool -> Eff es (Response, Bool)
checkbox' = checkboxWith' id

-- | 'checkbox' with a layout modifier: @checkboxWith alignMid@ centres it in
-- a row taller than itself, and @checkboxWith fillW@ gives it the row.
{-# INLINE checkboxWith #-}
checkboxWith :: Ui :> es => (Layout -> Layout) -> Text -> Bool -> Eff es Bool
checkboxWith f txt checked = snd <$> checkboxWith' f txt checked

-- | 'checkboxWith' returning @(response, checked)@.
checkboxWith' :: Ui :> es => (Layout -> Layout) -> Text -> Bool -> Eff es (Response, Bool)
checkboxWith' f txt checked = do
  (wid, ctx) <- freshWidget
  uiIO $ registerFocusable ctx wid
  current <- intBool <$> uiIO (adoptSlot fieldInt ctx wid (boolInt checked))
  resp <- addWidget wid NodeCheckbox txt (if current then 1 else 0) (f defaultLayout)
  finishToggle ctx wid current resp
