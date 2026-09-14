{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Widgets.Radio (radioFieldset, boundedRadioFieldset, enumRadio, useRadio) where

import Control.Monad (foldM, when)
import Data.Foldable (toList)
import Data.Hashable (hash)
import Data.IntMap.Strict qualified as IM
import Data.Text (Text)
import Data.Text qualified as T
import Effectful (Eff, type (:>))
import NanoUI.Context (getStore, intKey, registerFocusable, setStore)
import NanoUI.Layout.Arena (NodeType (..))
import NanoUI.Monad (Ui, askContext, nextId, uiIO, withKey)
import NanoUI.Store (WidgetStore (..), slotKey)
import NanoUI.Style (Layout, defaultLayout, fillW, gap, tight)
import NanoUI.Widgets.Behavior (KeyNav (..), useKeyNav, useSelection)
import NanoUI.Widgets.Combinators (selectableItem)
import NanoUI.Widgets.Layout (column')
import NanoUI.Widgets.Node
  ( Response (..)
  , setChanged
  , tagContainer
  )

radioLay :: Layout
radioLay = tight (fillW defaultLayout)

radioGroupLay :: Layout
radioGroupLay = tight (gap 4 (fillW defaultLayout))

radioSalt :: Int
radioSalt = hash ("radio" :: Text)

radioFieldset ::
  (Foldable f, Ui :> es) => f Text -> Int -> Eff es (Response, Int)
radioFieldset options initial =
  withKey radioSalt $ do
    gid <- nextId
    ctx <- askContext
    let
      opts = case toList options of
        [] -> [""]
        xs -> xs
      !len = length opts
      !c0 = max 0 (min (len - 1) initial)
      !key = intKey gid
      !keyInit = slotKey 1 key
    st0 <- uiIO (getStore ctx)
    let
      lastInit = IM.lookup keyInit (storeInt st0)
      storedSel = IM.lookup key (storeInt st0)
      !sel = case (lastInit, storedSel) of
        (Just li, Just s) | li == c0 -> max 0 (min (len - 1) s)
        _ -> c0
    uiIO $ registerFocusable ctx gid
    nav <- useKeyNav gid
    let
      !navDelta =
        (if knDown nav || knRight nav then 1 else 0 :: Int)
          - (if knUp nav || knLeft nav then 1 else 0)
      !selNav = if navDelta == 0 then sel else max 0 (min (len - 1) (sel + navDelta))
    column' radioGroupLay $ do
      tagContainer gid
      (combinedResp, clickedIdx) <- addRadioOptions selNav opts
      let
        !finalSel = if clickedIdx >= 0 then clickedIdx else selNav
        !hasClick = clickedIdx >= 0
      uiIO $ do
        when (storedSel /= Just finalSel || lastInit /= Just c0) $ do
          st <- getStore ctx
          setStore
            ctx
            st
              { storeInt =
                  IM.insert key finalSel $
                    IM.insert keyInit c0 (storeInt st)
              }
      pure (setChanged (finalSel /= sel || hasClick) combinedResp, finalSel)

-- Use the ordinary widget path for every option, including singleton groups.
-- It owns IDs, node construction, and scroll-aware interaction geometry.
addRadioOptions :: Ui :> es => Int -> [Text] -> Eff es (Response, Int)
addRadioOptions sel opts = foldM addOption (mempty, -1) (zip [0 ..] opts)
 where
  addOption (!acc, !clickedIdx) (i, txt) = do
    r <- selectableItem NodeRadio txt (sel == i) radioLay i
    let
      clickedIdx' = if rawRespClicked r && clickedIdx < 0 then i else clickedIdx
    pure (acc <> r, clickedIdx')

boundedRadioFieldset ::
  forall a es.
  (Bounded a, Enum a, Ui :> es) => a -> (a -> Text) -> Eff es (Response, a)
boundedRadioFieldset initial encode =
  let
    vs = take 256 [minBound .. maxBound]
    lower = fromEnum (minBound :: a)
   in
    fmap
      (\(r, i) -> (r, toEnum (lower + i)))
      (radioFieldset (map encode vs) (fromEnum initial - lower))

enumRadio :: (Bounded a, Enum a, Show a, Ui :> es) => a -> Eff es (Response, a)
enumRadio initial = boundedRadioFieldset initial (T.pack . show)

useRadio :: (Enum a, Ui :> es) => a -> Eff es (a, a -> Eff es ())
useRadio initial = fmap (\(c, s) -> (toEnum c, s . fromEnum)) (useSelection (fromEnum initial))
