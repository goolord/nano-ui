-- | Checkbox control.
module NanoUI.Widgets.Checkbox (checkbox) where

import Control.Monad (when)
import Data.Text (Text)
import Effectful (Eff, type (:>))
import NanoUI.Context (getStoreBool, registerFocusable, writeStoreBool)
import NanoUI.Layout.Arena (NodeType (..))
import NanoUI.Monad (Ui, askContext, nextId, uiIO)
import NanoUI.Style (defaultLayout)
import NanoUI.Widgets.Behavior (keyActivated)
import NanoUI.Widgets.Node (Response, addWidget, respClicked, setChanged)

-- | Uncontrolled checkbox initialized from the supplied value.
-- Returns @(response, currentValue)@; use @NanoUI.State.checkboxControlled@
-- when the caller owns the current value.
checkbox :: Ui :> es => Text -> Bool -> Eff es (Response, Bool)
checkbox txt initial = do
  wid <- nextId
  ctx <- askContext
  uiIO $ registerFocusable ctx wid
  current <- uiIO (getStoreBool ctx wid initial)
  resp <- addWidget wid NodeCheckbox txt (if current then 1 else 0) defaultLayout
  keyClick <- keyActivated wid
  let
    clicked = respClicked resp || keyClick
    display = current /= clicked
  when clicked $
    uiIO $ writeStoreBool ctx wid display
  pure (setChanged clicked resp, display)
