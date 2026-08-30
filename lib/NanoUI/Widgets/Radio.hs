{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Widgets.Radio
  ( radioFieldset
  , boundedRadioFieldset
  , useRadio
  ) where

import Control.Monad (unless, void, when, zipWithM)
import Data.Text (Text)
import Effectful (Eff, type (:>))
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import NanoUI.Context
  ( Context (..)
  , getStore
  , intKey
  , setStore
  )
import NanoUI.Host (isCellHost)
import NanoUI.Icons (radioMark)
import NanoUI.Id (WidgetId (..))
import NanoUI.Monad (Ui, askContext, nextId, uiIO, withKey)
import NanoUI.Layout.Arena (NodeType (..))
import NanoUI.Store (WidgetStore (..))
import NanoUI.Font (mutedFontMarker)
import NanoUI.Style (defaultLayout, fillW, gap, tight)
import NanoUI.Types (Rect (..), rectUnion)
import NanoUI.WidgetText (radioPackOption)
import NanoUI.Widgets.Animate (useText)
import NanoUI.Widgets.Layout (column, labelEx)
import NanoUI.Widgets.Node
  ( Response (..)
  , addWidgetResp
  , mkResponse
  , setChanged
  )

radioOption ::
  (Ui :> es) =>
  Int ->
  Int ->
  Text ->
  Int ->
  Eff es (Response, Int)
radioOption groupKey optionIdx label selectedIdx = do
  wid <- nextId
  ctx <- askContext
  store <- uiIO (getStore ctx)
  let current = IM.findWithDefault selectedIdx groupKey (storeRadio store)
      host = ctxHostProfile ctx
      body =
        if isCellHost host
          then radioMark (ctxIcons ctx) (current == optionIdx) <> label
          else label
      nodeText = radioPackOption groupKey optionIdx body
  resp <-
    addWidgetResp
      wid
      NodeRadio
      nodeText
      (if current == optionIdx then 1 else 0)
      (tight . fillW $ defaultLayout)
      Nothing
  let displayIdx = if rawRespClicked resp then optionIdx else current
  pure (setChanged (displayIdx /= current) resp, displayIdx)

mergeResponses :: [Response] -> Response
mergeResponses [] = mkResponse (WidgetId 0) (Rect 0 0 0 0) False False False False
mergeResponses (r : rs) =
  foldl
    ( \acc x ->
        Response
          { rawRespId = rawRespId x
          , rawRespRect = rectUnion (rawRespRect acc) (rawRespRect x)
          , rawRespHovered = rawRespHovered acc || rawRespHovered x
          , rawRespPressed = rawRespPressed acc || rawRespPressed x
          , rawRespClicked = rawRespClicked acc || rawRespClicked x
          , rawRespChanged = rawRespChanged acc || rawRespChanged x
          }
    )
    r
    rs

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
    when (not (IM.member groupKey (storeRadio store0))) $
      uiIO $
        setStore
          ctx
          (store0 {storeRadio = IM.insert groupKey clamped (storeRadio store0)})
    store1 <- uiIO (getStore ctx)
    let selected = IM.findWithDefault clamped groupKey (storeRadio store1)
    column (tight . gap 4 . fillW $ defaultLayout) $ do
      unless (T.null legend) $
        void (labelEx (tight . fillW $ defaultLayout) (mutedFontMarker <> legend))
      results <-
        zipWithM
          (\idx lbl -> withKey idx (radioOption groupKey idx lbl selected))
          [0 ..]
          opts
      store2 <- uiIO (getStore ctx)
      let stored = IM.findWithDefault clamped groupKey (storeRadio store2)
          (resps, nextIdxs) = unzip results
          resp = mergeResponses resps
          finalIdx =
            case [idx | (r, idx) <- zip resps nextIdxs, rawRespClicked r] of
              (idx : _) -> idx
              [] -> stored
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
        if null values
          then initial
          else toEnum (max 0 (min (length values - 1) idx))
  pure (resp, picked)

-- | Uncontrolled radio hook storing the selected value in component local store.
useRadio ::
  (Eq a, Show a, Read a, Ui :> es) =>
  a ->
  Eff es (a, a -> Eff es ())
useRadio initial = do
  (readTxt, setTxt) <- useText (T.pack (show initial))
  txt <- readTxt
  let current =
        case reads (T.unpack txt) of
          [(v, "")] -> v
          _ -> initial
      set v = when (v /= current) $ setTxt (T.pack (show v))
  pure (current, set)
