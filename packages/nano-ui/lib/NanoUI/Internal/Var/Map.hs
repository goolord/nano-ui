-- | Keyed table of mutable 'Var's for dynamic children, allocated on first
-- lookup and removed explicitly by the application.
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

-- | Map from keys to mutable 'Var's. Unlike positional 'Var's, entries persist
-- across frames until explicitly removed with 'dropVar' or 'retainVars'.
newtype VarMap k a = VarMap (IORef (HashMap k (Var a)))

newVarMap :: IO (VarMap k a)
newVarMap = VarMap <$> newIORef HM.empty

-- | Look up the 'Var' for @k@, allocating it with @initial@ if absent.
varAt :: Hashable k => VarMap k a -> k -> a -> NanoUI (Var a)
varAt (VarMap ref) k initial = liftIO $ do
  m <- readIORef ref
  case HM.lookup k m of
    Just v -> pure v
    Nothing -> do
      v <- newVar initial
      writeIORef ref $! HM.insert k v m
      pure v

-- | Remove the 'Var' at @k@ so a subsequent 'varAt' allocates a fresh one, and
-- schedule a full rerun and repaint.
dropVar :: Hashable k => VarMap k a -> k -> NanoUI ()
dropVar (VarMap ref) k = forgetWhere ref (HM.member k) (HM.delete k)

-- | Retain only entries whose keys satisfy the predicate. Dropping anything
-- schedules a rerun and a full repaint, as 'dropVar' does; a call that drops
-- nothing does neither.
retainVars :: (k -> Bool) -> VarMap k a -> NanoUI ()
retainVars keep (VarMap ref) = forgetWhere ref (any (not . keep) . HM.keys) (HM.filterWithKey (\k _ -> keep k))

forgetWhere :: IORef (HashMap k v) -> (HashMap k v -> Bool) -> (HashMap k v -> HashMap k v) -> NanoUI ()
forgetWhere ref changes f = withContext $ \ctx -> do
  dropped <- atomicModifyIORef' ref (\m -> if changes m then (f m, True) else (m, False))
  -- A newly allocated replacement Var has no prior mutation stamp, so force a
  -- rerun and full damage when any entry is removed.
  when dropped (damageFull ctx >> requestRerun ctx)
