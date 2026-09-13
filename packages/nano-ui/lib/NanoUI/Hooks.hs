-- | Local state hooks. The store representation varies by value type, but
-- identity, equality checks, and invalidation follow one policy.
module NanoUI.Hooks
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
import Data.Dynamic (fromDynamic, toDyn)
import Data.IntMap.Strict qualified as IM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Effectful (Eff, type (:>))
import NanoUI.Context (getStore, intKey, setStore)
import NanoUI.Monad (Ui, askContext, nextId, uiIO)
import NanoUI.Store (WidgetStore (..), boolInt, bumpMirror, intBool)

useState :: (Typeable a, Eq a, Ui :> es) => a -> Eff es (a, a -> Eff es ())
useState =
  useStored
    (\key store -> IM.lookup key (storeDyn store) >>= fromDynamic)
    ( \key value store -> store {storeDyn = IM.insert key (toDyn value) (storeDyn store)}
    )

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

useFlag :: Ui :> es => Bool -> Eff es (Bool, Bool -> Eff es ())
useFlag initial = do
  (value, setValue) <- useInt (boolInt initial)
  pure (intBool value, setValue . boolInt)

useInt :: Ui :> es => Int -> Eff es (Int, Int -> Eff es ())
useInt =
  useStored
    (\key -> IM.lookup key . storeInt)
    (\key value store -> store {storeInt = IM.insert key value (storeInt store)})

useFloat :: Ui :> es => Float -> Eff es (Float, Float -> Eff es ())
useFloat =
  useStored
    (\key -> IM.lookup key . storeFloat)
    (\key value store -> store {storeFloat = IM.insert key value (storeFloat store)})

useEnum :: (Enum a, Ui :> es) => a -> Eff es (a, a -> Eff es ())
useEnum initial = do
  (index, setIndex) <- useInt (fromEnum initial)
  pure (toEnum index, setIndex . fromEnum)

useText :: Ui :> es => Text -> Eff es (Text, Text -> Eff es ())
useText =
  useStored
    (\key -> IM.lookup key . storeText)
    (\key value store -> store {storeText = IM.insert key value (storeText store)})

useToggle :: Ui :> es => Bool -> Eff es (Bool, Eff es ())
useToggle initial = do
  (value, setValue) <- useFlag initial
  pure (value, setValue (not value))
