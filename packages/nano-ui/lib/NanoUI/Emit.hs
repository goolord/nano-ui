-- | Adapt ordinary widgets to reducer-style applications. The backend's
-- reducer runner folds emitted messages into the model in emission order.
-- These combinators also work with custom widgets and configured variants.
--
-- > Emit.emitWhen (button "Save") Save
-- > Emit.emitChanged (sliderWith (fixedW 200) 0 100) volume SetVolume
-- > Emit.emitEdited textArea' notes SetNotes
module NanoUI.Emit
  ( emit
  , emitWhen
  , emitChanged
  , emitEdited
  ) where

import Control.Monad (when)
import Data.Typeable (Typeable)
import Effectful (Eff, type (:>))
import NanoUI.Monad (Ui, emit, whenM)
import NanoUI.Widgets.Node (Response, respChanged)

-- | Emit a message when an action activates, for example a button or menu item.
{-# INLINE emitWhen #-}
emitWhen :: (Typeable msg, Ui :> es) => Eff es Bool -> msg -> Eff es ()
emitWhen widget msg = whenM widget (emit msg)

-- | Supply the model value to a control and emit only a different returned
-- value. The original value is passed once, including for configured widgets.
{-# INLINE emitChanged #-}
emitChanged :: (Eq a, Typeable msg, Ui :> es) => (a -> Eff es a) -> a -> (a -> msg) -> Eff es ()
emitChanged widget old toMsg = widget old >>= \new -> when (new /= old) (emit (toMsg new))

-- | Emit a different value only when the response also reports an edit. Use
-- for text areas or debounced controls; caret/scroll-only changes emit nothing.
{-# INLINE emitEdited #-}
emitEdited :: (Eq a, Typeable msg, Ui :> es) => (a -> Eff es (Response, a)) -> a -> (a -> msg) -> Eff es ()
emitEdited widget old toMsg = do
  (resp, new) <- widget old
  when (respChanged resp && new /= old) (emit (toMsg new))
