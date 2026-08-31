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
  , useFlag
  , useText
  , useToggle
  ) where

import Control.Monad (when)
import Data.IORef (readIORef)
import Data.Text (Text)
import Effectful (Eff, type (:>))
import qualified Data.IntMap.Strict as IM
import NanoUI.Animatable (Animatable (..))
import NanoUI.Context
  ( Context (..)
  , Ease (..)
  , approxEq
  , easeSameSpec
  , getAnimationValue
  , getStore
  , intKey
  , markDirty
  , setStore
  , startAnimationEaseDelay
  , startSpring
  )
import NanoUI.Spring (SpringParams)
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
    anims <- readIORef (ctxAnimations ctx)
    let manim = IM.lookup (intKey wid) anims
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

useStoreField ::
  (Eq a, Ui :> es) =>
  (WidgetStore -> IM.IntMap a) ->
  (WidgetStore -> Int -> a -> WidgetStore) ->
  a ->
  Eff es (Eff es a, a -> Eff es ())
useStoreField field update initial = do
  wid <- nextId
  ctx <- askContext
  let key = intKey wid
      get = uiIO $ do
        st <- getStore ctx
        pure (IM.findWithDefault initial key (field st))
      set v = uiIO $ do
        st <- getStore ctx
        let prev = IM.findWithDefault initial key (field st)
        when (prev /= v) $ do
          setStore ctx (update st key v)
          markDirty ctx
  pure (get, set)

useFlag :: (Ui :> es) => Bool -> Eff es (Eff es Bool, Bool -> Eff es ())
useFlag initial = do
  (getI, setI) <-
    useStoreField
      storeInt
      (\st k v -> bumpMirror (st {storeInt = IM.insert k v (storeInt st)}))
      (boolInt initial)
  pure (fmap intBool getI, setI . boolInt)

useText :: (Ui :> es) => Text -> Eff es (Eff es Text, Text -> Eff es ())
useText initial =
  useStoreField
    storeText
    (\st k v -> bumpMirror (st {storeText = IM.insert k v (storeText st)}))
    initial

useToggle :: (Ui :> es) => Bool -> Eff es (Eff es Bool, Eff es ())
useToggle initial = do
  (get, set) <- useFlag initial
  let toggle = do
        cur <- get
        set (not cur)
  pure (get, toggle)
