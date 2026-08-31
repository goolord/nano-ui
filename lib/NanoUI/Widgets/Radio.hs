{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Widgets.Radio
  ( radioFieldset
  , boundedRadioFieldset
  , useRadio
  )
where

import Control.Monad (unless, void, when, zipWithM)
import Data.Text (Text)
import Effectful (Eff, type (:>))
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import NanoUI.Context (Context (..), getStore, intKey, setStore)
import NanoUI.Host (isCellHost)
import NanoUI.Icons (radioMark)
import NanoUI.Monad (Ui, askContext, nextId, uiIO, withKey)
import NanoUI.Layout.Arena (NodeType (..))
import NanoUI.Store (WidgetStore (..))
import NanoUI.Font (mutedFontMarker)
import NanoUI.Style (defaultLayout, fillW, gap, tight)
import NanoUI.Widgets.Behavior (useSelection)
import NanoUI.Widgets.Layout (column, labelEx)
import NanoUI.Widgets.Node
  ( Response (..)
  , addWidgetStyled
  , setChanged
  , tagContainer
  )

radioOption :: (Ui :> es) => Int -> Int -> Text -> Int -> Eff es (Response, Int)
radioOption groupKey optionIdx label selectedIdx = do
  wid <- nextId
  ctx <- askContext
  store <- uiIO (getStore ctx)
  let current = IM.findWithDefault selectedIdx groupKey (storeInt store)
      on = current == optionIdx
      body =
        if isCellHost (ctxHostProfile ctx)
          then radioMark (ctxIcons ctx) on <> label
          else label
  resp <-
    addWidgetStyled
      wid
      NodeRadio
      body
      (if on then 1 else 0)
      (tight . fillW $ defaultLayout)
      optionIdx
      Nothing
  let displayIdx = if rawRespClicked resp then optionIdx else current
  pure (setChanged (displayIdx /= current) resp, displayIdx)

-- | Mutually exclusive radio options grouped under a legend label.
radioFieldset ::
  (Ui :> es) =>
  Text ->
  [Text] ->
  Int ->
  Eff es (Response, Int)
radioFieldset legend options initial =
  withKey ("radio:" <> legend) $ do
    groupId <- nextId
    let groupKey = intKey groupId
        opts = if null options then [""] else options
        clamped = max 0 (min (length opts - 1) initial)
    ctx <- askContext
    store0 <- uiIO (getStore ctx)
    when (not (IM.member groupKey (storeInt store0))) $
      uiIO $ setStore ctx (store0 {storeInt = IM.insert groupKey clamped (storeInt store0)})
    store1 <- uiIO (getStore ctx)
    let selected = IM.findWithDefault clamped groupKey (storeInt store1)
    column (tight . gap 4 . fillW $ defaultLayout) $ do
      tagContainer groupId
      unless (T.null legend) $
        void (labelEx (tight . fillW $ defaultLayout) (mutedFontMarker <> legend))
      results <-
        zipWithM
          (\idx lbl -> withKey idx (radioOption groupKey idx lbl selected))
          [0 ..]
          opts
      let resp = mconcat (map fst results)
          finalIdx =
            case [idx | (r, idx) <- results, rawRespClicked r] of
              (idx : _) -> idx
              [] -> selected
      pure (resp, finalIdx)

-- | Radio fieldset for any bounded enumerable type.
boundedRadioFieldset ::
  (Bounded a, Enum a, Ui :> es) =>
  Text ->
  a ->
  (a -> Text) ->
  Eff es (Response, a)
boundedRadioFieldset legend initial encode = do
  let values = [minBound .. maxBound]
      labels = map encode values
      initialIdx = max 0 (min (length labels - 1) (fromEnum initial))
  (resp, idx) <- radioFieldset legend labels initialIdx
  let picked =
        if null values then initial else toEnum (max 0 (min (length values - 1) idx))
  pure (resp, picked)

-- | Uncontrolled radio hook. Stores the selected enum as an Int.
useRadio :: (Enum a, Ui :> es) => a -> Eff es (a, a -> Eff es ())
useRadio initial = do
  (cur, set) <- useSelection (fromEnum initial)
  pure (toEnum cur, set . fromEnum)
