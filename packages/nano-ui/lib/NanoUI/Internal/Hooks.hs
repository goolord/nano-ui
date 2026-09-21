-- | State stored under a hook's widget id. Call hooks in a stable order, or
-- enclose them in keyed scopes. The initial value is used while the slot is
-- absent; changing that argument does not reset a populated slot. Setters
-- compare against the latest stored value and request a frame when it changes.
module NanoUI.Internal.Hooks
  ( useState
  , useFlag
  , useInt
  , useFloat
  , useEnum
  , useText
  , useToggle
  )
where

import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Effectful (Eff, type (:>))
import NanoUI.Internal.Context (getStore, intKey, setStore)
import NanoUI.Internal.Monad (Ui, askContext, nextId, uiIO)
import NanoUI.Internal.Store (WidgetStore, boolInt, bumpMirror, fieldFloat, fieldInt, fieldText, insertDyn, insertSlot, intBool, lookupDyn, lookupSlot)

-- | Read local state and its setter. Keep the value type stable at this id.
useState :: (Typeable a, Eq a, Ui :> es) => a -> Eff es (a, a -> Eff es ())
useState = useStored lookupDyn insertDyn

useStored ::
  (Eq a, Ui :> es) =>
  (Int -> WidgetStore -> Maybe a)
  -> (Int -> a -> WidgetStore -> WidgetStore)
  -> a
  -> Eff es (a, a -> Eff es ())
useStored lookupValue update initial = do
  wid <- nextId
  ctx <- askContext
  let
    key = intKey wid
    valueIn = fromMaybe initial . lookupValue key
    setValue value = uiIO $ do
      -- A setter can run more than once in a frame. Compare with the latest
      -- store, not the value captured when the hook was evaluated.
      store <- getStore ctx
      when (valueIn store /= value) $
        setStore ctx (bumpMirror (update key value store))
  value <- valueIn <$> uiIO (getStore ctx)
  pure (value, setValue)

-- | Boolean state stored as an integer flag, with an explicit setter.
useFlag :: Ui :> es => Bool -> Eff es (Bool, Bool -> Eff es ())
useFlag initial = do
  (value, setValue) <- useInt (boolInt initial)
  pure (intBool value, setValue . boolInt)

-- | Integer state and its setter, without runtime type lookup.
useInt :: Ui :> es => Int -> Eff es (Int, Int -> Eff es ())
useInt = useStored (lookupSlot fieldInt) (insertSlot fieldInt)

-- | Floating-point state and its setter, without runtime type lookup.
useFloat :: Ui :> es => Float -> Eff es (Float, Float -> Eff es ())
useFloat = useStored (lookupSlot fieldFloat) (insertSlot fieldFloat)

-- | Enum state stored through 'fromEnum'. Keep the enum type stable at this id.
useEnum :: (Enum a, Ui :> es) => a -> Eff es (a, a -> Eff es ())
useEnum initial = do
  (index, setIndex) <- useInt (fromEnum initial)
  pure (toEnum index, setIndex . fromEnum)

-- | Text state and its setter, without runtime type lookup.
useText :: Ui :> es => Text -> Eff es (Text, Text -> Eff es ())
useText = useStored (lookupSlot fieldText) (insertSlot fieldText)

-- | Boolean state and an action that writes the opposite of this frame's value.
-- Calling that action twice in one frame writes the same value twice.
useToggle :: Ui :> es => Bool -> Eff es (Bool, Eff es ())
useToggle initial = do
  (value, setValue) <- useFlag initial
  pure (value, setValue (not value))
