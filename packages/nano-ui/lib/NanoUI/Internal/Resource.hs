-- | Typed resource ownership and a payload-free session cleanup registry.
module NanoUI.Internal.Resource
  ( Resource (..)
  , newResource
  , Held (..)
  , newHeld
  , Holding (..)
  )
where

import Data.IORef (IORef, newIORef)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Primitive.PrimVar (PrimVar, newPrimVar)
import Data.Unique (hashUnique, newUnique)
import GHC.Exts (RealWorld)

-- | A typed owner allocated once per component in a session. The registry
-- retains only its cleanup action; its key and value never leave this cell.
data Resource k a = Resource !Int !(IORef (Maybe (k, a, Holding)))

instance Eq (Resource k a) where
  Resource a _ == Resource b _ = a == b

newResource :: IO (Resource k a)
newResource = Resource <$> (hashUnique <$> newUnique) <*> newIORef Nothing

-- | Release starts cleanup and returns an action waiting for completion.
data Holding = Holding
  { holdingRelease :: !(IO (IO ()))
  , holdingSeen :: !(PrimVar RealWorld Int)
  }

-- | Session registry and frame stamps. Built-in callback-only leases use a
-- widget-to-resource-id index; arbitrary values live in typed owners instead.
data Held = Held
  { heldEntries :: !(IORef (IntMap Holding))
  , heldWidgets :: !(IORef (IntMap Int))
  , heldFrame :: !(PrimVar RealWorld Int)
  , heldStamped :: !(PrimVar RealWorld Int)
  , heldCount :: !(PrimVar RealWorld Int)
  }

newHeld :: IO Held
newHeld =
  Held
    <$> newIORef IM.empty
    <*> newIORef IM.empty
    <*> newPrimVar 0
    <*> newPrimVar 0
    <*> newPrimVar 0
