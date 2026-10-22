-- | Expandable tree rows with a controlled pre-order selection index. Each
-- row is a button in the row look ('buttonFlagRow').
module NanoUI.Internal.Widgets.Tree (TreeItem (..), tree, tree') where

import Control.Monad (void, when, zipWithM)
import Data.IORef (writeIORef)
import Data.Foldable (asum, find, fold, toList)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Primitive.SmallArray (SmallArray, indexSmallArray, sizeofSmallArray, smallArrayFromList)
import qualified Data.IntSet as IS
import NanoUI.Internal.Context (Context (..), adoptSlot, getPrevRect, getStore, intKey, registerFocusable, writeSlots)
import NanoUI.Internal.Font (treeChevronRect)
import NanoUI.Internal.Derived (TreeItem (..), TreeRow, TreeRows (..), treeCache)
import NanoUI.Internal.Id (WidgetId (..), hashWidgetId)
import NanoUI.Internal.Input (inputMousePos)
import NanoUI.Internal.Layout.Arena (NodeType (..))
import NanoUI.Internal.Store (fieldInt, fieldIntSet, lookupSlot, ptrEq, slotWrite)
import NanoUI.Internal.Monad (NanoUI, askInput, focusedWidget, freshWidget, liftIO, withKey)
import NanoUI.Internal.Var (Binding (..), bindVar)
import NanoUI.Internal.Var.Types (Var)
import NanoUI.Internal.Style (defaultLayout, fillW, gap, tight)
import NanoUI.Internal.Types (Rect (..), clamp, rectContains)
import NanoUI.Internal.WidgetText (treeEncodeStyle)
import NanoUI.Internal.Widgets.Behavior (KeyNav (..), useKeyNav)
import NanoUI.Internal.Widgets.Combinators (finishInput, readDerived, writeDerived)
import NanoUI.Internal.Widgets.Layout (columnWith)
import NanoUI.Internal.Widgets.Node (Response (..), addWidgetStyled, moveSelection, tagContainer)

subtreeSize :: TreeItem -> Int
subtreeSize item = 1 + forestSize (treeItemChildren item)

forestSize :: [TreeItem] -> Int
forestSize = foldl' (\acc x -> acc + subtreeSize x) 0

-- | Visible rows in pre-order, skipping the children of collapsed nodes.
-- Single pass: each subtree passes the next pre-order index to the
-- continuation for its remaining siblings.
visibleRows :: (Int -> Bool) -> [TreeItem] -> SmallArray TreeRow
visibleRows expanded items = smallArrayFromList (go 0 0 items (const []))
  where
    go !idx !_ [] k = k idx
    go !idx !depth (item@(TreeItem lbl kids) : rest) k =
      let hasKids = not (null kids)
       in (idx, depth, hasKids, lbl)
            : if hasKids && expanded idx
              then go (idx + 1) (depth + 1) kids (\next -> go next depth rest k)
              else go (idx + subtreeSize item) depth rest k

-- | The item count and visible rows, from the cache while the items are the
-- same container and the expansion set is equal. The caller's container is
-- compared, not a list made from it, so a vector or sequence hits too. The
-- items are compared evaluated: an argument that is a top-level constant is
-- a thunk until forced.
cachedRows :: Foldable f => Context -> Int -> f TreeItem -> IS.IntSet -> IO (Int, SmallArray TreeRow)
cachedRows ctx key !items !expanded = do
  cached <- readDerived treeCache ctx key
  case cached of
    Just (TreeRows items' expanded' total rows)
      | ptrEq items' items && expanded' == expanded -> pure (total, rows)
    _ -> do
      let list = toList items
          !total = forestSize list
          !rows = visibleRows (`IS.member` expanded) list
      (total, rows) <$ writeDerived treeCache ctx key (TreeRows items expanded total rows)

treeKeyNav ::
  KeyNav ->
  SmallArray TreeRow ->
  SmallArray Response ->
  WidgetId ->
  Int ->
  IS.IntSet ->
  (Int, IS.IntSet, Maybe WidgetId)
treeKeyNav nav rows resps focus selected expanded
  | hashWidgetId focus == 0 || not moving = stay
  | otherwise = maybe stay (\pos -> step pos (rowAt pos)) (find ((== focus) . widAt) [0 .. n - 1])
 where
  moving = knUp nav || knDown nav || knLeft nav || knRight nav || knEnter nav || knSpace nav
  n = sizeofSmallArray rows
  rowAt = indexSmallArray rows
  widAt i = rawRespId (indexSmallArray resps i)
  stay = (selected, expanded, Nothing)
  moveTo p = let (idx, _, _, _) = rowAt p in (idx, expanded, Just (widAt p))
  expandTo s = (selected, s, Nothing)
  step pos (nodeIdx, depth, hasKids, _)
    | knDown nav, pos + 1 < n = moveTo (pos + 1)
    | knUp nav, pos > 0 = moveTo (pos - 1)
    | knEnter nav || knSpace nav, hasKids = expandTo (toggle nodeIdx expanded)
    | knRight nav, hasKids, not open = expandTo (IS.insert nodeIdx expanded)
    | knLeft nav, hasKids, open = expandTo (IS.delete nodeIdx expanded)
    | knLeft nav, depth > 0 =
        maybe (nodeIdx, expanded, Nothing) moveTo $
          find (\i -> let (_, d, _, _) = rowAt i in d < depth) [pos - 1, pos - 2 .. 0]
    | otherwise = stay
   where
    open = IS.member nodeIdx expanded

toggle :: Int -> IS.IntSet -> IS.IntSet
toggle idx s = if IS.member idx s then IS.delete idx s else IS.insert idx s

-- | Render one visible row and return any selection or expansion change:
-- clicking a parent's chevron toggles expansion without changing selection.
treeRow ::
  Int -> TreeRow -> Int -> IS.IntSet -> NanoUI (Response, Maybe (Int, IS.IntSet))
treeRow rowIdx (nodeIdx, depth, hasKids, lbl) selected expanded = do
  (wid, ctx) <- freshWidget
  inp <- askInput
  let style = treeEncodeStyle depth hasKids (IS.member nodeIdx expanded) (odd rowIdx)
      value = if selected == nodeIdx then 1 else 0
  resp <- addWidgetStyled wid NodeButton lbl value (tight . fillW $ defaultLayout) style
  liftIO $ registerFocusable ctx wid
  if not (rawRespClicked resp)
    then pure (resp, Nothing)
    else liftIO $ do
      mrect <- getPrevRect ctx wid
      let mouse = inputMousePos inp
          onChevron = case mrect of
            Just rect@(Rect x y _ h) ->
              rectContains (treeChevronRect (ctxFontMetrics ctx) x y h depth) mouse
                && rectContains rect mouse
            _ -> False
      pure . (resp,) . Just $
        if hasKids && onChevron then (selected, toggle nodeIdx expanded) else (nodeIdx, expanded)

-- | Collapsible tree with the selected row bound to mutable state ('Var').
-- Rows are numbered in pre-order; a click or the arrow keys update the
-- selection in place. Expansion state is stored internally by the widget.
-- @key@ distinguishes trees in one scope.
{-# INLINE tree #-}
tree :: Foldable f => Text -> f TreeItem -> Var Int -> NanoUI ()
tree key items v = void (tree' key items v)

-- | 'tree' returning its 'Response'.
{-# INLINE tree' #-}
tree' :: Foldable f => Text -> f TreeItem -> Var Int -> NanoUI Response
tree' key items v = bindVar RenderFinal v (treeCore key items)

-- | 'tree' on a plain row index: the response and the selection after this
-- frame.
treeCore :: Foldable f => Text -> f TreeItem -> Int -> NanoUI (Response, Int)
treeCore key inputItems index =
  withKey ("tree:" <> key) $ do
    (groupId, ctx) <- freshWidget
    let groupKey = intKey groupId
        -- Every parent starts expanded. Any parent sits under one at the top
        -- level, so the top level alone tells a forest that has none.
        allParents
          | any (not . null . treeItemChildren) inputItems =
              IS.fromList [i | (i, _, True, _) <- toList (visibleRows (const True) (toList inputItems))]
          | otherwise = IS.empty
    storedExp <- lookupSlot fieldIntSet groupKey <$> liftIO (getStore ctx)
    let expandedSet = fromMaybe allParents storedExp
    (total, rows) <- liftIO (cachedRows ctx groupKey inputItems expandedSet)
    let clamped = if total <= 0 then 0 else clamp 0 (total - 1) index
    selected <- liftIO $ adoptSlot fieldInt ctx groupId clamped
    columnWith (tight . gap 0 . fillW) $ do
      tagContainer groupId
      results <-
        zipWithM
          (\rowIdx row@(i, _, _, _) -> withKey i (treeRow rowIdx row selected expandedSet))
          [0 ..]
          (toList rows)
      let resps = smallArrayFromList (map fst results)
          (clickSel, clickExp) = fromMaybe (selected, expandedSet) (asum (map snd results))
      focus <- focusedWidget
      nav <- useKeyNav focus
      let (keySel, keyExp, mFocus) = treeKeyNav nav rows resps focus clickSel clickExp
      liftIO (moveSelection ctx selected keySel [(i, r) | ((i, _, _, _), (r, _)) <- zip (toList rows) results])
      result <- finishInput fieldInt ctx groupId selected (fold resps) keySel
      -- Items that arrive after a first frame without parents start expanded
      -- too: nothing is stored until there is something to expand.
      when (isJust storedExp || not (IS.null keyExp)) $
        liftIO (writeSlots ctx (slotWrite fieldIntSet groupKey keyExp))
      mapM_ (liftIO . writeIORef (ctxFocusId ctx)) mFocus
      -- An index the items do not reach yet stays the caller's until the
      -- user moves the selection.
      pure (fmap (\s -> if s == clamped then index else s) result)
