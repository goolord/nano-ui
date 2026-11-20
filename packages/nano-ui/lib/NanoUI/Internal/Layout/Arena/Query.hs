-- | Arena traversal, paint-order queries, ancestor walks, and node-class loops.
module NanoUI.Internal.Layout.Arena.Query
  ( topModalNode
  , forFloatingNodes_
  , floatingNodesInPaintOrder
  , forChildNodes_
  , foldFlowChildrenM
  , flowChildrenInOrder
  , foldPlacedChildrenM
  , foldChildrenInBoxM
  , forChildrenInPaintOrder_
  , childrenTopFirst
  , firstChildOnTopJustM
  , drawnOver
  , AdornRows (..)
  , adornRows
  , foldSeqM
  , findSeqM
  , findNodeRevM
  , findClassNodeM
  , findClassNodeRevM
  , foldClassNodesM
  , classNodes
  , foldClassNodeRevM
  , forClassNodes_
  , walkFloatingAncestors
  , findNodeM
  , foldNodesM
  , firstChildJustM
  , walkAncestors
  ) where

import Control.Monad (unless, when)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Primitive.PrimArray (readPrimArray)
import NanoUI.Internal.Layout.Arena.Types
import NanoUI.Internal.Style (AlignX (..), Flow (..))

-- | The topmost (last added) modal node, if any.
{-# INLINE topModalNode #-}
topModalNode :: NodeArena -> IO (Maybe NodeIdx)
topModalNode na = findClassNodeRevM na FloatingNodes (fmap (== NodeModal) . getNodeType na)

-- | Visit the nodes of a floating type (modal, window, popup) in arena
-- order, looking only at the floating nodes.
{-# INLINE forFloatingNodes_ #-}
forFloatingNodes_ :: NodeArena -> NodeType -> (NodeIdx -> IO ()) -> IO ()
forFloatingNodes_ na t f =
  forClassNodes_ na FloatingNodes $ \idx -> getNodeType na idx >>= \nt -> when (nt == t) (f idx)

-- | Floating nodes (windows, then modals, then popups, each kind in arena
-- order): bottom to top in paint order.
floatingNodesInPaintOrder :: NodeArena -> IO [NodeIdx]
floatingNodesInPaintOrder na = do
  tagged <- foldClassNodeRevM na FloatingNodes (\acc idx -> (\nt -> (rank nt, idx) : acc) <$> getNodeType na idx) []
  pure [idx | r <- [0, 1, 2 :: Int], (r', idx) <- tagged, r' == r]
  where
    rank nt = case nt of
      NodeWindow -> 0
      NodeModal -> 1
      _ -> 2



-- | Visit direct children in reverse declaration order, including floating nodes.
{-# INLINE forChildNodes_ #-}
forChildNodes_ :: NodeArena -> NodeIdx -> (NodeIdx -> IO ()) -> IO ()
forChildNodes_ na parentIdx f = do
  fc <- getFirstChild na parentIdx
  let go !ci
        | ci < 0 = pure ()
        | otherwise = do
            f ci
            ns <- getNextSibling na ci
            go ns
  go fc

-- | Fold over a node's children in sibling order, skipping floating
-- (modal, window, popup) and pinned children, neither of which is part of
-- the flow ('foldPlacedChildrenM' includes pinned ones).
{-# INLINE foldFlowChildrenM #-}
foldFlowChildrenM :: NodeArena -> NodeIdx -> (acc -> NodeIdx -> IO acc) -> acc -> IO acc
foldFlowChildrenM na = foldChildrenInBoxM na True

-- | 'foldFlowChildrenM' including pinned children: every child placed
-- inside the node's box.
{-# INLINE foldPlacedChildrenM #-}
foldPlacedChildrenM :: NodeArena -> NodeIdx -> (acc -> NodeIdx -> IO acc) -> acc -> IO acc
foldPlacedChildrenM na = foldChildrenInBoxM na False

-- | Fold over the non-floating children, also skipping pinned ones when
-- @skipPinned@ is set.
{-# INLINE foldChildrenInBoxM #-}
foldChildrenInBoxM :: NodeArena -> Bool -> NodeIdx -> (acc -> NodeIdx -> IO acc) -> acc -> IO acc
foldChildrenInBoxM na skipPinned parentIdx f z = do
  fc <- getFirstChild na parentIdx
  let go !ci !acc
        | ci < 0 = pure acc
        | otherwise = do
            nt <- getNodeType na ci
            ns <- getNextSibling na ci
            pinned <- if skipPinned then isPinnedNode na ci else pure False
            if isFloatingNode nt || pinned
              then go ns acc
              else f acc ci >>= go ns
  go fc z

-- | A node's flow children in the order they were added. Siblings are linked
-- last first, so consing them up as they are visited restores that order.
flowChildrenInOrder :: NodeArena -> NodeIdx -> IO [NodeIdx]
flowChildrenInOrder na parentIdx = foldFlowChildrenM na parentIdx (\acc ci -> pure (ci : acc)) []

-- | Visit a node's children, floating ones included, in paint order: each is
-- drawn over those visited before it. A layered container visits unpinned
-- children in declaration order, so later ones end up on top. Other nodes
-- visit them last to first, so the earlier of two overlapping children is on
-- top. Pinned children come after the rest, in declaration order.
{-# INLINE forChildrenInPaintOrder_ #-}
forChildrenInPaintOrder_ :: NodeArena -> NodeIdx -> (NodeIdx -> IO ()) -> IO ()
forChildrenInPaintOrder_ na parentIdx f = do
  flow <- getFlow na parentIdx
  pinnedBelow <- hasPinnedBelow na parentIdx
  fc <- getFirstChild na parentIdx
  -- Sibling links run last to first, so recursing before visiting yields
  -- declaration order.
  let inOrderIf pinned !ci = when (ci >= 0) $ do
        getNextSibling na ci >>= inOrderIf pinned
        isPinnedNode na ci >>= \p -> when (p == pinned) (f ci)
      pinnedLast !ci = when (ci >= 0) $ do
        ns <- getNextSibling na ci
        pinned <- isPinnedNode na ci
        if pinned then pinnedLast ns >> f ci else f ci >> pinnedLast ns
  case flow of
    Layered -> inOrderIf False fc >> when pinnedBelow (inOrderIf True fc)
    _
      | pinnedBelow -> pinnedLast fc
      | otherwise -> forChildNodes_ na parentIdx f

-- | A node's non-floating children, topmost first (the reverse of
-- 'forChildrenInPaintOrder_'). Floating children are painted as separate
-- layers.
childrenTopFirst :: NodeArena -> NodeIdx -> IO [NodeIdx]
childrenTopFirst na parentIdx = do
  acc <- newIORef []
  forChildrenInPaintOrder_ na parentIdx $ \ci -> do
    nt <- getNodeType na ci
    unless (isFloatingNode nt) $ modifyIORef' acc (ci :)
  readIORef acc

-- | The first 'Just' that @f@ returns over @parentIdx@'s children in
-- 'childrenTopFirst' order. Use it instead of 'firstChildJustM' where
-- children can overlap.
{-# INLINE firstChildOnTopJustM #-}
firstChildOnTopJustM :: NodeArena -> NodeIdx -> (NodeIdx -> IO (Maybe a)) -> IO (Maybe a)
firstChildOnTopJustM na parentIdx f =
  foldr (\ci rest -> f ci >>= maybe rest (pure . Just)) (pure Nothing) =<< childrenTopFirst na parentIdx

-- | Whether paint draws node @b@ over the earlier node @a@. At their common
-- ancestor, this checks whether 'forChildrenInPaintOrder_' visits @b@'s
-- branch last. False when @b@ is inside @a@ or they are in different trees.
drawnOver :: NodeArena -> NodeIdx -> NodeIdx -> IO Bool
drawnOver na b a = climb a b (-1) (-1)
  where
    -- Parents have lower indices, so climb from the higher of @x@ and @y@
    -- until they meet, remembering the child each side came from.
    climb !x !y !ca !cb
      | x > y = getParent na x >>= \p -> climb p y x cb
      | x < y = getParent na y >>= \p -> climb x p ca y
      | x < 0 || ca < 0 = pure False
      | otherwise = do
          lastSide <- newIORef ca
          forChildrenInPaintOrder_ na x $ \ci -> when (ci == ca || ci == cb) (writeIORef lastSide ci)
          (== cb) <$> readIORef lastSide

-- | A button's or text field's adornments, the rows it holds as children
-- ("NanoUI.Internal.Widgets.Adornment"): its leading row and its trailing row
-- (-1 for a side without one), each row's width, and the taller row's height.
-- The leading row aligns to the start and the trailing one to the end.
data AdornRows = AdornRows !NodeIdx !Float !NodeIdx !Float !Float

adornRows :: NodeArena -> NodeIdx -> IO AdornRows
adornRows na idx = do
  a <- arenaArrays na
  let add (AdornRows li lw ti tw rowH) ci = do
        ax <- readTagEnum a ci TagAlignX
        cw <- readGeom a ci GeomW
        ch <- readGeom a ci GeomH
        pure $
          if ax == AlignEnd
            then AdornRows li lw ci cw (max rowH ch)
            else AdornRows ci cw ti tw (max rowH ch)
  foldFlowChildrenM na idx add (AdornRows (-1) 0 (-1) 0 0)

-- | Strict fold over the nodes @at 0@ to @at (k - 1)@, or from the last to
-- the first with @rev@.
{-# INLINE foldSeqM #-}
foldSeqM :: Bool -> Int -> (Int -> IO NodeIdx) -> (a -> NodeIdx -> IO a) -> a -> IO a
foldSeqM rev k at f = go (if rev then k - 1 else 0)
  where
    go !i !acc
      | i < 0 || i >= k = pure acc
      | otherwise = at i >>= f acc >>= go (if rev then i - 1 else i + 1)

-- | The first of the nodes @at 0@ to @at (k - 1)@, or the last with @rev@,
-- that satisfies the predicate.
{-# INLINE findSeqM #-}
findSeqM :: Bool -> Int -> (Int -> IO NodeIdx) -> (NodeIdx -> IO Bool) -> IO (Maybe NodeIdx)
findSeqM rev k at p = go (if rev then k - 1 else 0)
  where
    go !i
      | i < 0 || i >= k = pure Nothing
      | otherwise = do
          idx <- at i
          ok <- p idx
          if ok then pure (Just idx) else go (if rev then i - 1 else i + 1)

-- | First node, in arena order, satisfying the predicate.
{-# INLINE findNodeM #-}
findNodeM :: NodeArena -> (NodeIdx -> IO Bool) -> IO (Maybe NodeIdx)
findNodeM na p = arenaCount na >>= \n -> findSeqM False n pure p

-- | Find the last declared matching node, or 'Nothing'. Stops at the first match
-- while scanning backwards.
{-# INLINE findNodeRevM #-}
findNodeRevM :: NodeArena -> (NodeIdx -> IO Bool) -> IO (Maybe NodeIdx)
findNodeRevM na p = arenaCount na >>= \n -> findSeqM True n pure p

-- | Left fold over every node in arena order.
{-# INLINE foldNodesM #-}
foldNodesM :: NodeArena -> (a -> NodeIdx -> IO a) -> a -> IO a
foldNodesM na f z = arenaCount na >>= \n -> foldSeqM False n pure f z


-- | The length of the list of class @c@ in 'naClassNodes', and a reader for
-- its @i@th node.
{-# INLINE classNodes #-}
classNodes :: NodeArena -> NodeClass -> IO (Int, Int -> IO NodeIdx)
classNodes na c = do
  let ci = fromEnum c
  arr <- readIORef (naClassNodes na)
  cap <- readIORef (naCapacity na)
  k <- readPrimArray (naClassCounts na) ci
  -- Forced here: a lazy offset would be a thunk and a box on every walk.
  let !base = ci * cap
  pure (k, \i -> readPrimArray arr (base + i))

-- | 'findNodeM' over the nodes of one class: the first in arena order that
-- satisfies the predicate.
{-# INLINE findClassNodeM #-}
findClassNodeM :: NodeArena -> NodeClass -> (NodeIdx -> IO Bool) -> IO (Maybe NodeIdx)
findClassNodeM na c p = classNodes na c >>= \(k, at) -> findSeqM False k at p

-- | 'findNodeRevM' over the nodes of one class: the last in arena order that
-- satisfies the predicate.
{-# INLINE findClassNodeRevM #-}
findClassNodeRevM :: NodeArena -> NodeClass -> (NodeIdx -> IO Bool) -> IO (Maybe NodeIdx)
findClassNodeRevM na c p = classNodes na c >>= \(k, at) -> findSeqM True k at p

-- | 'foldNodesM' over the nodes of one class, in arena order.
{-# INLINE foldClassNodesM #-}
foldClassNodesM :: NodeArena -> NodeClass -> (a -> NodeIdx -> IO a) -> a -> IO a
foldClassNodesM na c f z = classNodes na c >>= \(k, at) -> foldSeqM False k at f z

-- | 'foldClassNodesM' from last declared to first.
{-# INLINE foldClassNodeRevM #-}
foldClassNodeRevM :: NodeArena -> NodeClass -> (a -> NodeIdx -> IO a) -> a -> IO a
foldClassNodeRevM na c f z = classNodes na c >>= \(k, at) -> foldSeqM True k at f z

-- | Visit the nodes of one class in arena order.
{-# INLINE forClassNodes_ #-}
forClassNodes_ :: NodeArena -> NodeClass -> (NodeIdx -> IO ()) -> IO ()
forClassNodes_ na c f = foldClassNodesM na c (\() idx -> f idx) ()

-- | The first result @step@ finds walking up from @idx@, the node itself
-- first.
{-# INLINE walkAncestors #-}
walkAncestors :: NodeArena -> NodeIdx -> (NodeIdx -> IO (Maybe a)) -> IO (Maybe a)
walkAncestors na idx step = go idx
  where
    go !i
      | i < 0 = pure Nothing
      | otherwise = step i >>= maybe (getParent na i >>= go) (pure . Just)

-- | 'walkAncestors' that offers @step@ only the floating nodes (modal,
-- window, popup), with their type. Most frames have no floating node, and
-- then this skips the walk.
{-# INLINE walkFloatingAncestors #-}
walkFloatingAncestors :: NodeArena -> NodeIdx -> (NodeIdx -> NodeType -> IO (Maybe a)) -> IO (Maybe a)
walkFloatingAncestors na idx step = do
  floating <- floatingNodeCount na
  if floating <= 0
    then pure Nothing
    else walkAncestors na idx $ \i -> do
      nt <- getNodeType na i
      if isFloatingNode nt then step i nt else pure Nothing

-- | First 'Just' result of @f@ over @parentIdx@'s direct children.
{-# INLINE firstChildJustM #-}
firstChildJustM :: NodeArena -> NodeIdx -> (NodeIdx -> IO (Maybe a)) -> IO (Maybe a)
firstChildJustM na parentIdx f = getFirstChild na parentIdx >>= go
  where
    go !ci
      | ci < 0 = pure Nothing
      | otherwise = f ci >>= maybe (getNextSibling na ci >>= go) (pure . Just)
