-- | Checkbox control.
module NanoUI.Widgets.Checkbox (checkbox, checkbox') where

import Data.Text (Text)
import Effectful (Eff, type (:>))
import NanoUI.Context (adoptStoreInt, intKey, recordStoreInt, registerFocusable, writeStoreBool)
import NanoUI.Layout.Arena (NodeType (..))
import NanoUI.Monad (Ui, askContext, nextId, uiIO)
import NanoUI.Store (boolInt, intBool)
import NanoUI.Style (defaultLayout)
import NanoUI.Widgets.Behavior (keyActivated)
import NanoUI.Widgets.Node (Response, addWidget, respClicked, setChanged)

-- | Checkbox with a caption. Pass whether it is checked; the result is the
-- state after this frame's click or Space/Enter.
{-# INLINE checkbox #-}
checkbox :: Ui :> es => Text -> Bool -> Eff es Bool
checkbox txt checked = snd <$> checkbox' txt checked

checkbox' :: Ui :> es => Text -> Bool -> Eff es (Response, Bool)
checkbox' txt checked = do
  wid <- nextId
  ctx <- askContext
  uiIO $ registerFocusable ctx wid
  let key = intKey wid
  current <- intBool <$> uiIO (adoptStoreInt ctx wid key (boolInt checked))
  resp <- addWidget wid NodeCheckbox txt (if current then 1 else 0) defaultLayout
  keyClick <- keyActivated wid
  let
    clicked = respClicked resp || keyClick
    display = current /= clicked
  uiIO $ do
    writeStoreBool ctx wid display
    recordStoreInt ctx key (boolInt display)
  pure (setChanged clicked resp, display)
