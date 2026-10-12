-- | A Var for each element of a list held in a Var, found by key.
module NanoUI.Internal.Var.List
  ( forEachVar
  , withElemVars
  )
where

import Data.Foldable (for_)
import Data.Hashable (Hashable)
import NanoUI.Internal.Monad (NanoUI, liftIO, withKey)
import NanoUI.Internal.Var (readVar)
import NanoUI.Internal.Var.Types (Lens (..), Var (..), newElemCtx)

-- | Run @body@ for each element of the list, under 'withKey' of its key,
-- with a Var for that element. The element's Var follows its key: if the
-- list is reordered, it keeps naming the same element, and a write to an
-- element whose key has left the list is dropped. Keys must be unique.
--
-- > forEachVar todoId app.todos $ \todo -> checkbox "" todo.done
forEachVar :: Hashable k => (a -> k) -> Var [a] -> (Var a -> NanoUI ()) -> NanoUI ()
forEachVar key v body = do
  (xs, at) <- elemVars key v
  for_ xs $ \x -> let !k = key x in withKey k (body (at k x))

-- | Read the list and give @body@ the elements with a function from an
-- element to its Var, for when the view shows some of the elements, in
-- another order, or needs each element's neighbours. Run each element under
-- 'withKey' of its key yourself.
withElemVars :: Hashable k => (a -> k) -> Var [a] -> ([a] -> (a -> Var a) -> NanoUI r) -> NanoUI r
withElemVars key v body = do
  (xs, at) <- elemVars key v
  body xs (\x -> at (key x) x)

-- | The list, read, and an element's Var given its key and the element.
elemVars :: Hashable k => (a -> k) -> Var [a] -> NanoUI ([a], k -> a -> Var a)
elemVars key v = do
  xs <- readVar v
  case v of
    Var _ -> pure (xs, \_ x -> Var x)
    VarRef cell l -> do
      ec <- liftIO (newElemCtx l key xs)
      pure (xs, \k x -> VarRef cell (Elem ec k x))
