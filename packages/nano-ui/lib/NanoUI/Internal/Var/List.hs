-- | A Var for each element of a list held in a Var, found by key.
module NanoUI.Internal.Var.List
  ( forEachVar
  , withElemVars
  )
where

import Data.Foldable (for_)
import Data.Hashable (Hashable)
import NanoUI.Internal.Monad (NanoUI, liftIO, withKey)
import NanoUI.Internal.Var (readVar, readVarSig)
import NanoUI.Internal.Var.Types (Lens (..), Var (..), keyStep, newElemCtx, structSig)

-- | Run @body@ for each element of the list, under 'withKey' of its key,
-- with a Var for that element. The element's Var follows its key: if the
-- list is reordered, it keeps naming the same element, and a write to an
-- element whose key has left the list is dropped. Keys must be unique.
--
-- Laying out the rows reads only the list's keys and order, so a row that
-- edits its own element reruns nothing for the rows before it.
--
-- > forEachVar todoId app.todos $ \todo -> checkbox "" todo.done
forEachVar :: Hashable k => (a -> k) -> Var [a] -> (Var a -> NanoUI ()) -> NanoUI ()
forEachVar key v body = do
  (xs, at) <- elemVars True key v
  for_ xs $ \x -> let !k = key x in withKey k (body (at k x))

-- | Read the list and give @body@ the elements with a function from an
-- element to its Var, for when the view shows some of the elements, in
-- another order, or needs each element's neighbours. Run each element under
-- 'withKey' of its key yourself.
--
-- The body gets the elements themselves, so this reads all of the list: a
-- row that edits its element reruns the view when an earlier row read it.
withElemVars :: Hashable k => (a -> k) -> Var [a] -> ([a] -> (a -> Var a) -> NanoUI r) -> NanoUI r
withElemVars key v body = do
  (xs, at) <- elemVars False key v
  body xs (\x -> at (key x) x)

-- | The list, read, and an element's Var given its key and the element.
-- With @keysOnly@, the read records only the list's keys and order.
elemVars :: Hashable k => Bool -> (a -> k) -> Var [a] -> NanoUI ([a], k -> a -> Var a)
elemVars keysOnly key v = case v of
  Var xs -> pure (xs, \_ x -> Var x)
  VarRef cell l -> do
    xs <- if keysOnly then readVarSig (structSig l) v else readVar v
    ec <- liftIO (newElemCtx l key xs)
    pure (xs, \k x -> VarRef cell (Elem (keyStep k) ec k x))
