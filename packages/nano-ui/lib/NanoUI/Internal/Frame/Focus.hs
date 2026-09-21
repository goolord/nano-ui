{-# LANGUAGE DataKinds #-}

-- | Keyboard focus order for Tab, keeping focus inside an open modal, and
-- copying selection state from the store into the nodes that paint it.
module NanoUI.Internal.Frame.Focus
  ( filterModalFocusables
  , constrainFocusToModal
  , syncWidgetLabels
  , tabNext
  , tabNextFocusables
  ) where

import Control.Monad (filterM, unless, when)
import Data.IORef (readIORef, writeIORef)
import Data.Primitive.PrimArray (readPrimArray)
import NanoUI.Internal.Context (Context (..), getStore, intBool, intKey)
import NanoUI.Internal.Frame.Hit (widgetIdInSubtree)
import NanoUI.Internal.Id (WidgetId (..), hashWidgetId)
import NanoUI.Internal.Layout.Arena
  ( NodeType (NodeCheckbox, NodeRadio, NodeTree)
  , forNodes_
  , getNodeType
  , getParent
  , getStyleIdx
  , getWidgetId
  , setNodeValue
  , topModalNode
  )
import NanoUI.Internal.Store (fieldInt, findSlot, lookupSlot)
import NanoUI.Internal.WidgetText (treeDecodeStyle)

-- | Next focus id, or previous with Shift, wrapping at both ends. An unknown
-- current id selects the first entry; an empty list returns @WidgetId 0@.
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

-- | 'tabNext' over the widgets that registered as focusable this frame. It
-- reads the context's mutable array in place, so it builds no list, and it
-- skips zero ids.
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

-- | The ids whose widgets are inside the top modal, or all of @ids@ when no
-- modal is open.
filterModalFocusables :: Context -> [WidgetId] -> IO [WidgetId]
filterModalFocusables ctx ids = do
  -- The modal's root is looked up once for the whole list. Each widget then
  -- costs one walk up its ancestors.
  top <- topModalNode (ctxNodeArena ctx)
  case top of
    Nothing -> pure ids
    Just modal -> filterM (widgetIdInSubtree ctx modal) ids

-- | While a modal is open, take keyboard focus away from a widget outside the
-- top modal. The frame runs this after the pointer steps, which can move
-- focus, and before 'NanoUI.Internal.Frame.Input.finalizeTabFocus'.
constrainFocusToModal :: Context -> IO ()
constrainFocusToModal ctx = do
  top <- topModalNode (ctxNodeArena ctx)
  case top of
    Nothing -> pure ()
    Just modal -> do
      focus <- readIORef (ctxFocusId ctx)
      when (hashWidgetId focus /= 0) $ do
        ok <- widgetIdInSubtree ctx modal focus
        unless ok $ writeIORef (ctxFocusId ctx) (WidgetId 0)

-- | Copy selection state from the store into the node values the painter
-- reads. A checkbox's value becomes its stored flag. A radio option or a tree
-- row gets 1 when its group's stored selection names it, and 0 otherwise.
-- The frame runs this after the view, before layout, and again after the
-- input steps when they changed the store, so what is painted matches the
-- store even when the change came after the widget was declared.
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
        -- A checkbox with no stored value keeps the value the view gave its
        -- node.
        case lookupSlot fieldInt key store of
          Just v -> setNodeValue na idx (if intBool v then 1 else 0)
          Nothing -> pure ()
      _
        -- A radio option's style index is its option index. A tree row packs
        -- its pre-order node index into the high bits of its style index.
        -- The group keeps its selection, as one of those indices, in the Int
        -- slot of the parent node's widget id.
        | nt == NodeRadio || nt == NodeTree -> do
            parent <- getParent na idx
            si <- getStyleIdx na idx
            groupWid <- getWidgetId na parent
            let own
                  | nt == NodeTree, (nodeIdx, _, _, _) <- treeDecodeStyle si = nodeIdx
                  | otherwise = si
                selected = findSlot fieldInt own (intKey groupWid) store
            setNodeValue na idx (if selected == own then 1 else 0)
      _ -> pure ()
