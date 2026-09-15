-- | Widgets for reducer-style applications.
--
-- Each function draws a widget from the model and, when the user changes it,
-- emits a message instead of returning the new value. The backend's reducer
-- runner (@runSdlAppReduce@, @runRgfwAppReduce@) folds the frame's messages
-- into the model with your update function.
--
-- The names match "NanoUI", so import this module qualified:
--
-- @
-- import NanoUI.Emit qualified as Emit
--
-- data Msg = Increment | Decrement
--
-- view :: Int -> NanoUI ()
-- view n = row $ do
--   Emit.button "-" Decrement
--   label (T.pack (show n))
--   Emit.button "+" Increment
-- @
module NanoUI.Emit
  ( emit
  , button
  , checkbox
  , slider
  , select
  , radio
  , textInput
  , textArea
  , tabs
  )
where

import Control.Monad (when)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Effectful (Eff, type (:>))
import NanoUI.Monad (Ui, emit)
import NanoUI.Widgets.Button qualified as W
import NanoUI.Widgets.Checkbox qualified as W
import NanoUI.Widgets.Node (respChanged)
import NanoUI.Widgets.Radio qualified as W
import NanoUI.Widgets.TextArea qualified as W
import NanoUI.Widgets.Select qualified as W
import NanoUI.Widgets.Slider qualified as W
import NanoUI.Widgets.Tabs (Tab)
import NanoUI.Widgets.Tabs qualified as W
import NanoUI.Widgets.TextInput qualified as W

-- | Emit @msg@ when the button is clicked.
button :: (Typeable msg, Ui :> es) => Text -> msg -> Eff es ()
button txt msg = do
  clicked <- W.button txt
  when clicked (emit msg)

checkbox :: (Typeable msg, Ui :> es) => Text -> Bool -> (Bool -> msg) -> Eff es ()
checkbox txt checked toMsg = do
  new <- W.checkbox txt checked
  when (new /= checked) (emit (toMsg new))

slider :: (Typeable msg, Ui :> es) => Float -> Float -> Float -> (Float -> msg) -> Eff es ()
slider minV maxV value toMsg = do
  new <- W.slider minV maxV value
  when (new /= value) (emit (toMsg new))

select :: (Foldable f, Typeable msg, Ui :> es) => f Text -> Int -> (Int -> msg) -> Eff es ()
select options index toMsg = do
  new <- W.select options index
  when (new /= index) (emit (toMsg new))

radio :: (Foldable f, Typeable msg, Ui :> es) => f Text -> Int -> (Int -> msg) -> Eff es ()
radio options index toMsg = do
  new <- W.radio options index
  when (new /= index) (emit (toMsg new))

textInput :: (Typeable msg, Ui :> es) => Text -> (Text -> msg) -> Eff es ()
textInput value toMsg = do
  new <- W.textInput value
  when (new /= value) (emit (toMsg new))

-- | Emit the new text after an edit. Caret and scroll changes emit nothing.
textArea :: (Typeable msg, Ui :> es) => Text -> (Text -> msg) -> Eff es ()
textArea value toMsg = do
  (resp, new) <- W.textArea' value
  when (respChanged resp && new /= value) (emit (toMsg new))

-- | Emit the newly active key when the user switches tabs.
tabs :: (Foldable f, Eq a, Typeable msg, Ui :> es) => a -> f (Tab a (Eff es ())) -> (a -> msg) -> Eff es ()
tabs active ts toMsg = do
  new <- W.tabs active ts
  when (new /= active) (emit (toMsg new))
