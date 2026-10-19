-- | A Var for each element of a list held in a Var, found by key.
module NanoUI.Internal.Var.List
  ( forEachVar
  , withElemVars
  )
where

import Data.Foldable (for_)
import Data.Hashable (Hashable, hash)
import NanoUI.Internal.Monad (NanoUI, keyedTag, liftIO)
import NanoUI.Internal.Var (readCell, readVar)
import NanoUI.Internal.Var.Types (Lens (..), Var (..), newElemCtx, stepBit, structSig)

-- | Run @body@ for each element of the list under 'withKey' of its key,
-- passing a 'Var' focused on that element. Each element 'Var' is addressed by
-- key rather than index: reordering the list preserves each element's target,
-- and mutating an element whose key was removed is a no-op. Keys must be
-- unique.
--
-- The outer pass records only the list's keys and order, so mutating a single
-- element in place does not invalidate sibling rows.
--
-- > forEachVar todoId app.todos $ \todo -> checkbox "" todo.done
forEachVar :: Hashable k => (a -> k) -> Var [a] -> (Var a -> NanoUI ()) -> NanoUI ()
forEachVar key v body = do
  (xs, at) <- elemVars True key v
  -- One hash per key serves both the row's id, as 'withKey' would make it,
  -- and its Var.
  for_ xs $ \x -> let !k = key x; !h = hash k in keyedTag (fromIntegral h) (body (at k h x))

-- | Read the full list and pass @body@ both the elements and a key-addressed
-- 'Var' constructor. Use when filtering, reordering, or inspecting adjacent
-- elements; wrap each rendered row in 'withKey' manually.
--
-- Because @body@ receives the element values directly, this records a read of
-- every element in the list.
withElemVars :: Hashable k => (a -> k) -> Var [a] -> ([a] -> (a -> Var a) -> NanoUI r) -> NanoUI r
withElemVars key v body = do
  (xs, at) <- elemVars False key v
  body xs (\x -> let k = key x in at k (hash k) x)

-- | Read the list and build an element 'Var' constructor from a key, its hash,
-- and its initial value. When @keysOnly@ is 'True', only the list's structure
-- (keys and order) is recorded as read.
elemVars :: Hashable k => Bool -> (a -> k) -> Var [a] -> NanoUI ([a], k -> Int -> a -> Var a)
elemVars keysOnly key v = case v of
  VarConst xs -> pure (xs, \_ _ x -> VarConst x)
  VarRef cell l -> do
    xs <- if keysOnly then readCell (structSig l) cell l else readVar v
    ec <- liftIO (newElemCtx l key xs)
    pure (xs, \k h x -> VarRef cell (Elem (stepBit h) ec k x))
