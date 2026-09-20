-- | Widgets for reducer-style applications.
--
-- Each function draws a widget from the model and, when the user changes it,
-- emits a message instead of returning the new value. The backend's reducer
-- runner (@runSdlAppReduce@, @runRgfwAppReduce@) folds the frame's messages
-- into the model with your update function.
--
-- The names match "NanoUI", so import this module qualified:
--
-- > import NanoUI.Emit qualified as Emit
-- >
-- > data Msg = Increment | Decrement
-- >
-- > view :: Int -> NanoUI ()
-- > view n = row $ do
-- >   Emit.button "-" Decrement
-- >   label (T.pack (show n))
-- >   Emit.button "+" Increment
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

-- | Run an input on the model's value, and emit the message for a new one.
{-# INLINE emitChanged #-}
emitChanged :: (Eq a, Typeable msg, Ui :> es) => a -> Eff es a -> (a -> msg) -> Eff es ()
emitChanged old widget toMsg = widget >>= \new -> when (new /= old) (emit (toMsg new))

-- | Emit @msg@ when the button is clicked.
button :: (Typeable msg, Ui :> es) => Text -> msg -> Eff es ()
button txt msg = do
  clicked <- W.button txt
  when clicked (emit msg)

-- | Emit the mapped checked state when it differs from the supplied value.
checkbox :: (Typeable msg, Ui :> es) => Text -> Bool -> (Bool -> msg) -> Eff es ()
checkbox txt checked = emitChanged checked (W.checkbox txt checked)

-- | Slider over minimum, maximum, and current value; emit only changed values.
slider :: (Typeable msg, Ui :> es) => Float -> Float -> Float -> (Float -> msg) -> Eff es ()
slider minV maxV value = emitChanged value (W.slider minV maxV value)

-- | Emit the selected zero-based option index when it changes.
select :: (Foldable f, Typeable msg, Ui :> es) => f Text -> Int -> (Int -> msg) -> Eff es ()
select options index = emitChanged index (W.select options index)

-- | Radio group that emits the selected zero-based index when it changes.
radio :: (Foldable f, Typeable msg, Ui :> es) => f Text -> Int -> (Int -> msg) -> Eff es ()
radio options index = emitChanged index (W.radio options index)

-- | Single-line field that emits updated text. Selection-only changes emit nothing.
textInput :: (Typeable msg, Ui :> es) => Text -> (Text -> msg) -> Eff es ()
textInput value = emitChanged value (W.textInput value)

-- | Emit the new text after an edit. Caret and scroll changes emit nothing.
textArea :: (Typeable msg, Ui :> es) => Text -> (Text -> msg) -> Eff es ()
textArea value toMsg = do
  (resp, new) <- W.textArea' value
  when (respChanged resp && new /= value) (emit (toMsg new))

-- | Emit the newly active key when the user switches tabs.
tabs :: (Foldable f, Eq a, Typeable msg, Ui :> es) => a -> f (Tab a (Eff es ())) -> (a -> msg) -> Eff es ()
tabs active ts = emitChanged active (W.tabs active ts)
