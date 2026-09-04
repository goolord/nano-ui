module NanoUI.Widgets.Animate
  ( animate
  , animateEase
  , animateEaseDelay
  , animateTo
  , animateToEase
  , animateToEaseDelay
  , animateToSpring
  , animateToA
  , animateToSpringA
  , useState
  , useFlag
  , useInt
  , useFloat
  , useEnum
  , useText
  , useToggle
  ) where

import Control.Monad (when)
import Data.Dynamic (fromDynamic, toDyn)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Effectful (Eff, type (:>))
import qualified Data.IntMap.Strict as IM
import NanoUI.Animatable (Animatable (..))
import NanoUI.Context
  ( Ease (..)
  , approxEq
  , easeSameSpec
  , getAnimationValue
  , getStore
  , intKey
  , lookupAnimation
  , markDirty
  , setStore
  , startAnimationEaseDelay
  , startSpring
  )
import NanoUI.Animation (SpringParams)
import NanoUI.Monad (Ui, askContext, nextId, uiIO, withKey)
import NanoUI.Store (WidgetStore (..), boolInt, bumpMirror, intBool)

animate :: (Ui :> es) => Float -> Float -> Float -> Eff es Float
animate = animateEase EaseLinear

animateEase :: (Ui :> es) => Ease -> Float -> Float -> Float -> Eff es Float
animateEase ease from to dur = animateEaseDelay ease from to dur 0

animateEaseDelay :: (Ui :> es) => Ease -> Float -> Float -> Float -> Float -> Eff es Float
animateEaseDelay ease from to dur delay = do
  wid <- nextId
  ctx <- askContext
  uiIO $ do
    startAnimationEaseDelay ctx wid from to dur ease delay
    getAnimationValue ctx wid

animateTo :: (Ui :> es) => Float -> Float -> Eff es Float
animateTo = animateToEase EaseLinear

animateToEase :: (Ui :> es) => Ease -> Float -> Float -> Eff es Float
animateToEase ease target dur = animateToEaseDelay ease target dur 0

animateToEaseDelay :: (Ui :> es) => Ease -> Float -> Float -> Float -> Eff es Float
animateToEaseDelay ease target dur delay = do
  wid <- nextId
  ctx <- askContext
  uiIO $ do
    cur <- getAnimationValue ctx wid
    manim <- lookupAnimation ctx wid
    case manim of
      Just a
        | easeSameSpec a ease dur delay target -> pure cur
        | otherwise -> do
            startAnimationEaseDelay ctx wid cur target dur ease delay
            getAnimationValue ctx wid
      Nothing
        | approxEq cur target -> pure cur
        | otherwise -> do
            startAnimationEaseDelay ctx wid cur target dur ease delay
            getAnimationValue ctx wid

animateToSpring :: (Ui :> es) => SpringParams -> Float -> Eff es Float
animateToSpring params target = do
  wid <- nextId
  ctx <- askContext
  uiIO $ do
    startSpring ctx wid params target
    getAnimationValue ctx wid

animateToA :: (Animatable a, Ui :> es) => Ease -> Float -> a -> Eff es a
animateToA ease dur target = do
  comps <-
    mapM
      (\(i, c) -> withKey (i :: Int) (animateToEase ease c dur))
      (zip [0 ..] (toComponents target))
  pure (fromComponents comps)

animateToSpringA :: (Animatable a, Ui :> es) => SpringParams -> a -> Eff es a
animateToSpringA params target = do
  comps <-
    mapM
      (\(i, c) -> withKey (i :: Int) (animateToSpring params c))
      (zip [0 ..] (toComponents target))
  pure (fromComponents comps)

useState :: (Typeable a, Eq a, Ui :> es) => a -> Eff es (a, a -> Eff es ())
useState initial = do
  wid <- nextId
  ctx <- askContext
  let key = intKey wid
  st0 <- uiIO (getStore ctx)
  let val = case IM.lookup key (storeDyn st0) >>= fromDynamic of
        Just v -> v
        Nothing -> initial
      setVal newV = uiIO $ do
        st <- getStore ctx
        let prev = case IM.lookup key (storeDyn st) >>= fromDynamic of
              Just v -> v
              Nothing -> initial
        when (prev /= newV) $ do
          setStore ctx (bumpMirror (st {storeDyn = IM.insert key (toDyn newV) (storeDyn st)}))
          markDirty ctx
  pure (val, setVal)

useStoreField ::
  (Eq a, Ui :> es) =>
  (WidgetStore -> IM.IntMap a) ->
  (WidgetStore -> Int -> a -> WidgetStore) ->
  a ->
  Eff es (a, a -> Eff es ())
useStoreField field update initial = do
  wid <- nextId
  ctx <- askContext
  let key = intKey wid
  st0 <- uiIO (getStore ctx)
  let val = IM.findWithDefault initial key (field st0)
      set v = uiIO $ do
        st <- getStore ctx
        let prev = IM.findWithDefault initial key (field st)
        when (prev /= v) $ do
          setStore ctx (update st key v)
          markDirty ctx
  pure (val, set)

useFlag :: (Ui :> es) => Bool -> Eff es (Bool, Bool -> Eff es ())
useFlag initial = do
  (valI, setI) <-
    useStoreField
      storeInt
      (\st k v -> bumpMirror (st {storeInt = IM.insert k v (storeInt st)}))
      (boolInt initial)
  pure (intBool valI, setI . boolInt)

useInt :: (Ui :> es) => Int -> Eff es (Int, Int -> Eff es ())
useInt initial =
  useStoreField
    storeInt
    (\st k v -> bumpMirror (st {storeInt = IM.insert k v (storeInt st)}))
    initial

useFloat :: (Ui :> es) => Float -> Eff es (Float, Float -> Eff es ())
useFloat initial =
  useStoreField
    storeFloat
    (\st k v -> bumpMirror (st {storeFloat = IM.insert k v (storeFloat st)}))
    initial

useEnum :: (Enum a, Ui :> es) => a -> Eff es (a, a -> Eff es ())
useEnum initial = do
  (idx, setIdx) <- useInt (fromEnum initial)
  pure (toEnum idx, setIdx . fromEnum)

useText :: (Ui :> es) => Text -> Eff es (Text, Text -> Eff es ())
useText initial =
  useStoreField
    storeText
    (\st k v -> bumpMirror (st {storeText = IM.insert k v (storeText st)}))
    initial

useToggle :: (Ui :> es) => Bool -> Eff es (Bool, Eff es ())
useToggle initial = do
  (val, set) <- useFlag initial
  pure (val, set (not val))
