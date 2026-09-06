{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Widgets.Radio (radioFieldset, boundedRadioFieldset, enumRadio, useRadio) where

import Control.Monad (unless, void, when)
import qualified Data.IntMap.Strict as IM
import Data.Hashable (hash, hashWithSalt)
import Data.Text (Text)
import qualified Data.Text as T
import Effectful (Eff, type (:>))
import NanoUI.Context (Context (..), getStore, intKey, setStore)
import NanoUI.Icons (radioMark)
import NanoUI.Id (WidgetId (..))
import NanoUI.Layout.Arena (NodeType (..))
import NanoUI.Monad (Ui, askContext, nextId, uiIO, withKey)
import NanoUI.Store (WidgetStore (..), slotKey)
import NanoUI.Style (Layout, defaultLayout, fillW, fontMuted, gap, tight)
import NanoUI.Types (Rect (..), isCellHost, rectUnion)
import NanoUI.Widgets.Behavior (useSelection)
import NanoUI.Widgets.Combinators (selectableItem)
import NanoUI.Widgets.Layout (column', labelEx)
import NanoUI.Widgets.Node (Response (..), setChanged, tagContainer)

radioLay :: Layout
radioLay = tight (fillW defaultLayout)

radioGroupLay :: Layout
radioGroupLay = tight (gap 4 (fillW defaultLayout))

legendLay :: Layout
legendLay = tight (fillW (fontMuted defaultLayout))

radioFieldset :: (Ui :> es) => Text -> [Text] -> Int -> Eff es (Response, Int)
radioFieldset legend options initial =
  withKey (hashWithSalt (hash ("radio" :: Text)) legend) $ do
    gid <- nextId
    ctx <- askContext
    let opts = if null options then [""] else options
        !c0 = max 0 (min (length opts - 1) initial)
        !key = intKey gid
        !keyInit = slotKey 1 key
    st0 <- uiIO (getStore ctx)
    let lastInit = IM.lookup keyInit (storeInt st0)
        storedSel = IM.lookup key (storeInt st0)
        !sel = case (lastInit, storedSel) of
          (Just li, Just s) | li == c0 -> max 0 (min (length opts - 1) s)
          _                            -> c0
    column' radioGroupLay $ do
      tagContainer gid
      unless (T.null legend) $ void (labelEx legendLay legend)
      let unionRect a@(Rect _ _ w1 h1) b@(Rect _ _ w2 h2)
            | w1 <= 0 || h1 <= 0 = b
            | w2 <= 0 || h2 <= 0 = a
            | otherwise = rectUnion a b
          goOpts !_ [] !rid !rect !hov !press !click !submit !rightPress !rightClick !clickedIdx =
            pure (Response rid rect hov press click False submit rightPress rightClick, clickedIdx)
          goOpts !i (l:ls) _rid !rect !hov !press !click !submit !rightPress !rightClick !clickedIdx = do
            r <- withKey i (bit ctx sel i l)
            let !rect' = unionRect rect (rawRespRect r)
                !hov' = hov || rawRespHovered r
                !press' = press || rawRespPressed r
                !click' = click || rawRespClicked r
                !submit' = submit || rawRespSubmitted r
                !rightPress' = rightPress || rawRespRightPressed r
                !rightClick' = rightClick || rawRespRightClicked r
                !clickedIdx' = if rawRespClicked r && clickedIdx < 0 then i else clickedIdx
            goOpts (i + 1) ls (rawRespId r) rect' hov' press' click' submit' rightPress' rightClick' clickedIdx'
      (combinedResp, clickedIdx) <- goOpts 0 opts (WidgetId 0) (Rect 0 0 0 0) False False False False False False (-1)
      let !finalSel = if clickedIdx >= 0 then clickedIdx else sel
          !hasClick = clickedIdx >= 0
      uiIO $ do
        when (storedSel /= Just finalSel || lastInit /= Just c0) $ do
          st <- getStore ctx
          setStore ctx st
            { storeInt =
                IM.insert key finalSel $
                  IM.insert keyInit c0 (storeInt st)
            }
      pure (setChanged (finalSel /= sel || hasClick) combinedResp, finalSel)

bit :: (Ui :> es) => Context -> Int -> Int -> Text -> Eff es Response
bit ctx sel i l = do
  let on = sel == i
  selectableItem NodeRadio (if isCellHost (ctxHostProfile ctx) then radioMark (ctxIcons ctx) on <> l else l) on radioLay i

boundedRadioFieldset :: (Bounded a, Enum a, Ui :> es) => Text -> a -> (a -> Text) -> Eff es (Response, a)
boundedRadioFieldset legend initial encode =
  let vs = take 256 [minBound .. maxBound]
   in fmap (\(r, i) -> (r, toEnum (max 0 (min (length vs - 1) i)))) (radioFieldset legend (map encode vs) (fromEnum initial))

enumRadio :: (Bounded a, Enum a, Show a, Ui :> es) => Text -> a -> Eff es (Response, a)
enumRadio legend initial = boundedRadioFieldset legend initial (T.pack . show)

useRadio :: (Enum a, Ui :> es) => a -> Eff es (a, a -> Eff es ())
useRadio initial = fmap (\(c, s) -> (toEnum c, s . fromEnum)) (useSelection (fromEnum initial))
