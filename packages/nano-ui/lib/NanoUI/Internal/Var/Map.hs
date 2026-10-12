-- | Owned Vars by key, for state that belongs to dynamic children: one per
-- row of a list, made on first use and dropped by the application.
module NanoUI.Internal.Var.Map
  ( VarMap
  , newVarMap
  , varAt
  , dropVar
  , retainVars
  )
where

import Control.Monad (when)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import NanoUI.Internal.Context (damageFull)
import NanoUI.Internal.Monad (NanoUI, liftIO, withContext)
import NanoUI.Internal.Var (newVar, requestRerun)
import NanoUI.Internal.Var.Types (Var)

-- | Vars by key. Unlike positional Vars, an entry lives until the
-- application drops it, whatever the view shows.
newtype VarMap k a = VarMap (IORef (HashMap k (Var a)))

newVarMap :: IO (VarMap k a)
newVarMap = VarMap <$> newIORef HM.empty

-- | The Var under @k@, made with @initial@ the first time.
varAt :: Hashable k => VarMap k a -> k -> a -> NanoUI (Var a)
varAt (VarMap ref) k initial = liftIO $ do
  m <- readIORef ref
  case HM.lookup k m of
    Just v -> pure v
    Nothing -> do
      v <- newVar initial
      writeIORef ref $! HM.insert k v m
      pure v

-- | Forget the Var under @k@; the next 'varAt' starts it over. What showed
-- it is redrawn.
dropVar :: Hashable k => VarMap k a -> k -> NanoUI ()
dropVar (VarMap ref) k = forgetWhere ref (HM.member k) (HM.delete k)

-- | Keep only the Vars whose keys pass the test.
retainVars :: (k -> Bool) -> VarMap k a -> NanoUI ()
retainVars keep (VarMap ref) = forgetWhere ref (any (not . keep) . HM.keys) (HM.filterWithKey (\k _ -> keep k))

forgetWhere :: IORef (HashMap k v) -> (HashMap k v -> Bool) -> (HashMap k v -> HashMap k v) -> NanoUI ()
forgetWhere ref changes f = withContext $ \ctx -> do
  dropped <- atomicModifyIORef' ref (\m -> if changes m then (f m, True) else (m, False))
  -- A Var made again starts out unchanged, so nothing else would say that
  -- what showed the old one is stale.
  when dropped (damageFull ctx >> requestRerun ctx)
