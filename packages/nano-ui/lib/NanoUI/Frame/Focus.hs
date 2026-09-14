{-# LANGUAGE DataKinds #-}

-- | Focus traversal and modal focus constraints.
module NanoUI.Frame.Focus
  ( filterModalFocusables
  , widgetIdInModal
  , constrainFocusToModal
  , syncWidgetLabels
  , tabNext
  , tabNextFocusables
  ) where

import Control.Monad (filterM, unless, when)
import Data.IORef (readIORef, writeIORef)
import Data.Primitive.PrimArray (readPrimArray)
import qualified Data.IntMap.Strict as IM
import NanoUI.Context (Context (..), WidgetStore (..), getStore, intBool, intKey)
import NanoUI.Frame.Hit (topmostModalIdx, widgetIdInModal, widgetIdInSubtree)
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Layout.Arena
  ( NodeType (NodeCheckbox, NodeRadio, NodeTree)
  , forNodes_
  , getNodeType
  , getParent
  , getStyleIdx
  , getWidgetId
  , setNodeValue
  )
import NanoUI.WidgetText (treeDecodeStyle)

tabNext :: WidgetId -> [WidgetId] -> Bool -> WidgetId
tabNext cur ids shift =
  case ids of
    [] -> WidgetId 0
    first : rest ->
      let lastId !prev [] = prev
          lastId _ (x : xs) = lastId x xs
          search _ [] = first
          search prev (x : xs)
            | x == cur = if shift then prev else case xs of
                next : _ -> next
                [] -> first
            | otherwise = search x xs
       in if cur == first && shift
            then lastId first rest
            else search first ids

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

filterModalFocusables :: Context -> [WidgetId] -> IO [WidgetId]
filterModalFocusables ctx ids = do
  -- Searching the arena once per focusable makes a large modal's Tab traversal
  -- quadratic. Resolve its root once, then test ancestry for each widget.
  top <- topmostModalIdx ctx
  case top of
    Nothing -> pure ids
    Just modal -> filterM (widgetIdInSubtree ctx modal) ids

constrainFocusToModal :: Context -> IO ()
constrainFocusToModal ctx = do
  top <- topmostModalIdx ctx
  case top of
    Nothing -> pure ()
    Just modal -> do
      focus <- readIORef (ctxFocusId ctx)
      when (hashWidgetId focus /= 0) $ do
        ok <- widgetIdInSubtree ctx modal focus
        unless ok $ writeIORef (ctxFocusId ctx) (WidgetId 0)

syncWidgetLabels :: Context -> IO ()
syncWidgetLabels ctx = do
  store <- getStore ctx
  let na = ctxNodeArena ctx
  forNodes_ na $ \idx -> do
    nt <- getNodeType na idx
    wid <- getWidgetId na idx
    let key = intKey wid
    case nt of
      NodeCheckbox ->
        -- Only sync when the widget owns stored state; otherwise keep the
        -- value set from the initial argument during the UI pass.
        case IM.lookup key (storeInt store) of
          Just v -> setNodeValue na idx (if intBool v then 1 else 0)
          Nothing -> pure ()
      NodeRadio -> do
        parent <- getParent na idx
        optIdx <- getStyleIdx na idx
        groupWid <- getWidgetId na parent
        let selected = IM.findWithDefault optIdx (intKey groupWid) (storeInt store)
            val = selected == optIdx
        setNodeValue na idx (if val then 1 else 0)
      NodeTree -> do
        parent <- getParent na idx
        si <- getStyleIdx na idx
        groupWid <- getWidgetId na parent
        let (nodeIdx, _, _, _) = treeDecodeStyle si
            selected = IM.findWithDefault nodeIdx (intKey groupWid) (storeInt store)
        setNodeValue na idx (if selected == nodeIdx then 1 else 0)
      _ -> pure ()
