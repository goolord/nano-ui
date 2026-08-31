{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Widgets.Tabs
  ( Tab (..), TabStyle (..), TabOrientation (..), TabResponse (..)
  , tabRespClicked, tabRespChanged, tab, closableTab, mkTab
  , tabs, tabsEx, tabBar, tabBarEx, tabsEmit, tabsEmitEx
  , useTab, useTabIdx, boundedTabs
  )
where

import Control.Monad (when)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Effectful (Eff, type (:>))
import NanoUI.Monad (Ui, emit)
import NanoUI.Style (defaultLayout, fillW, tight)
import NanoUI.Widgets.Behavior (useSelection)
import NanoUI.Widgets.Combinators
  ( Tab (..), TabOrientation (..), TabResponse (..), TabStyle (..)
  , closableTab, mkTab, tab, tabRespChanged, tabRespClicked, tabStrip
  )
import NanoUI.Widgets.Layout (column)

tabs :: (Eq a, Ui :> es) => a -> [Tab a (Eff es ())] -> Eff es (TabResponse a, a)
tabs = tabsEx TabUnderline TabTop

tabsEx :: (Eq a, Ui :> es) => TabStyle -> TabOrientation -> a -> [Tab a (Eff es ())] -> Eff es (TabResponse a, a)
tabsEx style orient cur ts = tabStrip style orient cur ts (Just (renderBody ts))

tabBar :: (Eq a, Ui :> es) => a -> [Tab a body] -> Eff es (TabResponse a, a)
tabBar = tabBarEx TabUnderline TabTop

tabBarEx :: (Eq a, Ui :> es) => TabStyle -> TabOrientation -> a -> [Tab a body] -> Eff es (TabResponse a, a)
tabBarEx style orient cur ts = tabStrip style orient cur ts Nothing

tabsEmit :: (Typeable action, Eq a, Ui :> es) => (a -> action) -> a -> [Tab a (Eff es ())] -> Eff es (TabResponse a, a)
tabsEmit = tabsEmitEx TabUnderline TabTop

tabsEmitEx :: (Typeable action, Eq a, Ui :> es) => TabStyle -> TabOrientation -> (a -> action) -> a -> [Tab a (Eff es ())] -> Eff es (TabResponse a, a)
tabsEmitEx style orient toAction cur ts = do
  (tabResp, nextTab) <- tabStrip style orient cur ts (Just (renderBody ts))
  when (tabRespClicked tabResp && nextTab /= cur) $ emit (toAction nextTab)
  pure (tabResp, nextTab)

useTabIdx :: (Ui :> es) => Int -> Eff es (Int, Int -> Eff es ())
useTabIdx = useSelection

useTab :: (Enum a, Ui :> es) => a -> Eff es (a, a -> Eff es ())
useTab initial = fmap (\(c, s) -> (toEnum c, s . fromEnum)) (useSelection (fromEnum initial))

boundedTabs :: (Bounded a, Enum a, Eq a, Ui :> es) => a -> (a -> Text) -> (a -> Eff es ()) -> Eff es ()
boundedTabs initial encodeTab tabf = do
  (curTab, setTab) <- useTab initial
  (tabResp, nextTab) <- tabs curTab (fmap (\x -> tab x (encodeTab x) (tabf x)) [minBound .. maxBound])
  when (tabRespChanged tabResp) (setTab nextTab)

renderBody :: (Eq a, Ui :> es) => [Tab a (Eff es ())] -> a -> Eff es ()
renderBody ts activeKey =
  column (tight . fillW $ defaultLayout) $
    case filter (\t -> tabKey t == activeKey) ts of
      (selected : _) -> tabBody selected
      [] -> case ts of { (firstTab : _) -> tabBody firstTab; [] -> pure () }
