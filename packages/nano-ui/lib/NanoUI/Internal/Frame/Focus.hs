-- | Keyboard focus order for Tab, keeping focus inside an open modal, and
-- copying selection state from the store into the nodes that paint it.
module NanoUI.Internal.Frame.Focus
  ( constrainFocusToModal
  , syncWidgetLabels
  , tabNext
  ) where

import Control.Monad (forM_, unless, when)
import Data.IORef (readIORef, writeIORef)
import Data.Maybe (fromMaybe, listToMaybe)
import NanoUI.Internal.Context (Context (..), getStore, intBool, intKey)
import NanoUI.Internal.Frame.Hit (widgetIdInSubtree)
import NanoUI.Internal.Id (WidgetId (..), hashWidgetId)
import NanoUI.Internal.Layout.Arena
  ( NodeClass (SelectionNodes)
  , NodeType (NodeCheckbox, NodeRadio, NodeTree)
  , forClassNodes_
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
  fromMaybe (WidgetId 0) . listToMaybe $ case break (== cur) ids of
    (_, []) -> ids
    (before, _ : after)
      | shift -> reverse (if null before then ids else before)
      | otherwise -> after ++ ids

-- | While a modal is open, take keyboard focus away from a widget outside the
-- top modal. The frame runs this after the pointer steps, which can move
-- focus, and before 'NanoUI.Internal.Frame.Input.finalizeTabFocus'.
constrainFocusToModal :: Context -> IO ()
constrainFocusToModal ctx = do
  top <- topModalNode (ctxNodeArena ctx)
  forM_ top $ \modal -> do
    focus <- readIORef (ctxFocusId ctx)
    when (hashWidgetId focus /= 0) $ do
      ok <- widgetIdInSubtree ctx modal focus
      unless ok $ writeIORef (ctxFocusId ctx) (WidgetId 0)

-- | Copy selection state from the store into the node values the painter
-- reads. A checkbox's value becomes its stored flag. A radio option or a tree
-- row gets 1 when its group's stored selection names it, and 0 otherwise.
-- The frame runs this after the view, before layout, and again after the
-- input steps when they changed the store, so what is painted matches the
-- store even when the change came after the widget was declared. It visits
-- only the arena's 'SelectionNodes'.
syncWidgetLabels :: Context -> IO ()
syncWidgetLabels ctx = do
  store <- getStore ctx
  let na = ctxNodeArena ctx
  forClassNodes_ na SelectionNodes $ \idx -> do
    nt <- getNodeType na idx
    wid <- getWidgetId na idx
    let key = intKey wid
        -- The group keeps its selection, as the index @ownOf@ reads from a
        -- member's style index, in the Int slot of the parent node's widget
        -- id.
        syncGroup ownOf = do
          parent <- getParent na idx
          si <- getStyleIdx na idx
          groupWid <- getWidgetId na parent
          let own = ownOf si
              selected = findSlot fieldInt own (intKey groupWid) store
          setNodeValue na idx (if selected == own then 1 else 0)
    case nt of
      NodeCheckbox ->
        -- A checkbox with no stored value keeps the value the view gave its
        -- node.
        forM_ (lookupSlot fieldInt key store) $ \v ->
          setNodeValue na idx (if intBool v then 1 else 0)
      -- A radio option's style index is its option index.
      NodeRadio -> syncGroup id
      -- A tree row packs its pre-order node index into the high bits of its
      -- style index.
      NodeTree -> syncGroup (\si -> let (nodeIdx, _, _, _) = treeDecodeStyle si in nodeIdx)
      _ -> pure ()
