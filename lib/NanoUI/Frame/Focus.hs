{-# LANGUAGE DataKinds #-}

-- | Focus traversal and modal focus constraints.
module NanoUI.Frame.Focus
  ( filterModalFocusables
  , widgetIdInModal
  , constrainFocusToModal
  , syncWidgetLabels
  , tabNext
  , tabNextFocusables
  , unlessHit
  ) where

import Control.Monad (filterM, unless, when)
import Data.IORef (readIORef, writeIORef)
import Data.List (findIndex)
import Data.Primitive.PrimArray (readPrimArray)
import qualified Data.IntMap.Strict as IM
import NanoUI.Context (Context (..), WidgetStore (..), getStore, intKey)
import NanoUI.Frame.Hit (modalTreeOpen, widgetIdInModal)
import NanoUI.Host (isCellHost)
import NanoUI.Icons (checkboxMark, radioMark)
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Layout.Arena
  ( NodeType (NodeButton, NodeCheckbox, NodeRadio, NodeTree, NodeSlider)
  , forNodes_
  , getNodeType
  , getText
  , getWidgetId
  , setNodeText
  , setNodeValue
  )
import NanoUI.WidgetMarkers (stripButtonBrackets)
import NanoUI.WidgetText
  ( checkboxLabelText
  , radioLabelText
  , radioPackOption
  , radioParseOption
  , treeParseRow
  , sliderPackRange
  , sliderPackTerminal
  , sliderParseRange
  , sliderRangeSep
  )
import qualified Data.Text as T

tabNext :: WidgetId -> [WidgetId] -> Bool -> WidgetId
tabNext cur ids shift =
  case ids of
    [] -> WidgetId 0
    _ ->
      let idx = findIndex (== cur) ids
          n = length ids
          pick i = ids !! (i `mod` n)
       in case idx of
            Nothing -> ids !! 0
            Just i ->
              if shift
                then pick (i - 1 + n)
                else pick (i + 1)

-- | Scan the live focus buffer. Skip zero ids. No freeze or list copy.
tabNextFocusables :: Context -> WidgetId -> Bool -> IO WidgetId
tabNextFocusables ctx cur shift = do
  n <- readIORef (ctxFocusablesCount ctx)
  arr <- readIORef (ctxFocusables ctx)
  let at i = readPrimArray arr i
      findCur !i
        | i >= n = pure Nothing
        | otherwise = do
            w <- at i
            if w == cur && hashWidgetId w /= 0 then pure (Just i) else findCur (i + 1)
      firstLive !i
        | i >= n = pure (WidgetId 0)
        | otherwise = do
            w <- at i
            if hashWidgetId w /= 0 then pure w else firstLive (i + 1)
      step !i !left
        | left <= 0 = firstLive 0
        | otherwise = do
            let j = if shift then (i - 1 + n) `mod` n else (i + 1) `mod` n
            w <- at j
            if hashWidgetId w /= 0 then pure w else step j (left - 1)
  if n <= 0
    then pure (WidgetId 0)
    else do
      found <- findCur 0
      case found of
        Nothing -> firstLive 0
        Just i -> step i n

unlessHit :: Bool -> IO () -> IO ()
unlessHit b act = when (not b) act

filterModalFocusables :: Context -> [WidgetId] -> IO [WidgetId]
filterModalFocusables ctx ids = do
  open <- modalTreeOpen ctx
  if not open
    then pure ids
    else filterM (widgetIdInModal ctx) ids

constrainFocusToModal :: Context -> IO ()
constrainFocusToModal ctx = do
  open <- modalTreeOpen ctx
  when open $ do
    focus <- readIORef (ctxFocusId ctx)
    when (hashWidgetId focus /= 0) $ do
      ok <- widgetIdInModal ctx focus
      unless ok $ writeIORef (ctxFocusId ctx) (WidgetId 0)

syncWidgetLabels :: Context -> IO ()
syncWidgetLabels ctx = do
  store <- getStore ctx
  let na = ctxNodeArena ctx
      terminal = isCellHost (ctxHostProfile ctx)
      icons = ctxIcons ctx
  forNodes_ na $ \idx -> do
    nt <- getNodeType na idx
    wid <- getWidgetId na idx
    let key = intKey wid
    case nt of
      NodeCheckbox -> do
        txt <- getText na idx
        let body = checkboxLabelText txt
            val = IM.findWithDefault False key (storeCheckbox store)
            mark = if terminal then checkboxMark icons val else ""
        setNodeText na idx (mark <> body)
        setNodeValue na idx (if val then 1 else 0)
      NodeRadio -> do
        txt <- getText na idx
        case T.splitOn sliderRangeSep txt of
          [_g, _i, _lbl] -> do
            let (groupKey, optIdx, _) = radioParseOption txt
                selected = IM.findWithDefault optIdx groupKey (storeRadio store)
                val = selected == optIdx
                label = radioLabelText txt
                display =
                  if terminal
                    then radioMark icons val <> label
                    else label
            setNodeText na idx (radioPackOption groupKey optIdx display)
            setNodeValue na idx (if val then 1 else 0)
          _ -> pure ()
      NodeTree -> do
        txt <- getText na idx
        let (groupKey, nodeIdx, _, _, _, _) = treeParseRow txt
            selected = IM.findWithDefault nodeIdx groupKey (storeTreeSelected store)
        setNodeValue na idx (if selected == nodeIdx then 1 else 0)
      NodeSlider -> do
        let val = IM.findWithDefault 0 key (storeSlider store)
        txt <- getText na idx
        let (lbl, minV, maxV) = sliderParseRange txt
            frac = if maxV > minV then (val - minV) / (maxV - minV) else 0
            shown =
              if terminal
                then sliderPackTerminal lbl frac val minV maxV
                else sliderPackRange lbl minV maxV
        setNodeText na idx shown
        setNodeValue na idx frac
      NodeButton -> do
        txt <- getText na idx
        unless terminal $
          setNodeText na idx (stripButtonBrackets txt)
      _ -> pure ()
