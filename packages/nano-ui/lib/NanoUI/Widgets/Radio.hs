{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Widgets.Radio (radioFieldset, boundedRadioFieldset, useRadio) where

import qualified Data.IntMap.Strict as IM
import Control.Monad (unless, void, zipWithM)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Effectful (Eff, type (:>))
import qualified Data.Text as T
import NanoUI.Context (Context (..), getStore, intKey, setStore)
import NanoUI.Store (WidgetStore (..), slotKey)
import NanoUI.Types (isCellHost)
import NanoUI.Icons (radioMark)
import NanoUI.Layout.Arena (NodeType (..))
import NanoUI.Monad (Ui, askContext, nextId, uiIO, withKey)
import NanoUI.Style (defaultLayout, fillW, fontMuted, gap, tight)
import NanoUI.Widgets.Behavior (useSelection)
import NanoUI.Widgets.Combinators (selectableItem)
import NanoUI.Widgets.Layout (column, labelEx)
import NanoUI.Widgets.Node (Response (..), setChanged, tagContainer)

radioFieldset :: (Ui :> es) => Text -> [Text] -> Int -> Eff es (Response, Int)
radioFieldset legend options initial =
  withKey ("radio:" <> legend) $ do
    gid <- nextId
    ctx <- askContext
    let opts = if null options then [""] else options
        c0 = max 0 (min (length opts - 1) initial)
        key = intKey gid
        keyInit = slotKey 1 key
    st0 <- uiIO (getStore ctx)
    let lastInit = IM.lookup keyInit (storeInt st0)
        storedSel = IM.lookup key (storeInt st0)
        sel = case (lastInit, storedSel) of
          (Just li, Just s) | li == c0 -> max 0 (min (length opts - 1) s)
          _                            -> c0
    column (tight . gap 4 . fillW $ defaultLayout) $ do
      tagContainer gid
      unless (T.null legend) $ void (labelEx (tight . fillW . fontMuted $ defaultLayout) legend)
      rs <- zipWithM (\i l -> withKey i (bit ctx sel i l)) [0 ..] opts
      let mClicked = listToMaybe [i | (r, i) <- rs, rawRespClicked r]
          finalSel = fromMaybe sel mClicked
      uiIO $ do
        st <- getStore ctx
        setStore ctx st
          { storeInt =
              IM.insert key finalSel $
                IM.insert keyInit c0 (storeInt st)
          }
      pure (setChanged (finalSel /= sel || mClicked /= Nothing) (mconcat (map fst rs)), finalSel)

bit :: (Ui :> es) => Context -> Int -> Int -> Text -> Eff es (Response, Int)
bit ctx sel i l = do
  let on = sel == i
  r <- selectableItem NodeRadio (if isCellHost (ctxHostProfile ctx) then radioMark (ctxIcons ctx) on <> l else l) on (tight . fillW $ defaultLayout) i
  pure (setChanged (rawRespClicked r) r, i)

boundedRadioFieldset :: (Bounded a, Enum a, Ui :> es) => Text -> a -> (a -> Text) -> Eff es (Response, a)
boundedRadioFieldset legend initial encode =
  let vs = [minBound .. maxBound]
   in fmap (\(r, i) -> (r, toEnum (max 0 (min (length vs - 1) i)))) (radioFieldset legend (map encode vs) (fromEnum initial))

useRadio :: (Enum a, Ui :> es) => a -> Eff es (a, a -> Eff es ())
useRadio initial = fmap (\(c, s) -> (toEnum c, s . fromEnum)) (useSelection (fromEnum initial))
