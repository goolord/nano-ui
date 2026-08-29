{-# LANGUAGE DataKinds #-}

-- | Focus traversal and modal focus constraints.
module NanoUI.Frame.Focus
  ( filterModalFocusables
  , widgetIdInModal
  , constrainFocusToModal
  , syncWidgetLabels
  , tabNext
  , unlessHit
  ) where

import Control.Monad (filterM, unless, when)
import Data.IORef (readIORef, writeIORef)
import Data.List (findIndex)
import qualified Data.IntMap.Strict as IM
import NanoUI.Context (Context (..), WidgetStore (..), getStore, intKey)
import NanoUI.Frame.Hit (modalTreeOpen, widgetIdInModal)
import NanoUI.Host (isCellHost)
import NanoUI.Icons (checkboxMark)
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Layout.Arena
  ( NodeType (NodeButton, NodeCheckbox, NodeSlider)
  , arenaCount
  , getNodeType
  , getText
  , getWidgetId
  , setNodeText
  , setNodeValue
  )
import NanoUI.WidgetMarkers (stripButtonBrackets)
import NanoUI.WidgetText
  ( checkboxLabelText
  , sliderPackRange
  , sliderPackTerminal
  , sliderParseRange
  )

tabNext :: WidgetId -> [WidgetId] -> Bool -> WidgetId
tabNext cur ids shift =
  case ids of
    [] -> WidgetId 0
    _ ->
      let idx = findIndex (== cur) ids
          pick i = ids !! (i `mod` length ids)
       in case idx of
            Nothing -> ids !! 0
            Just i ->
              if shift
                then pick (i - 1 + length ids)
                else pick (i + 1)

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
  count <- arenaCount (ctxNodeArena ctx)
  let na = ctxNodeArena ctx
      terminal = isCellHost (ctxHostProfile ctx)
      icons = ctxIcons ctx
      go !idx
        | idx >= count = pure ()
        | otherwise = do
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
            go (idx + 1)
  go 0
