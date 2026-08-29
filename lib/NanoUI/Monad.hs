{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module NanoUI.Monad
  ( NanoUI
  , Ui
  , runNanoUI
  , runUi
  , uiIO
  , uiFinally
  , emit
  , withKey
  , currentId
  , askContext
  , askInput
  , askHost
  ) where

import Control.Exception (finally)
import Data.Bits (xor)
import Data.Hashable (Hashable, hash)
import Data.IORef (readIORef, writeIORef)
import Data.Typeable (Typeable)
import Data.Word (Word64)
import Effectful (Dispatch (Static), DispatchOf, Eff, Effect, IOE, runEff, type (:>))
import Effectful.Dispatch.Static
  ( SideEffects (WithSideEffects)
  , StaticRep
  , evalStaticRep
  , getStaticRep
  , unsafeEff_
  )
import Effectful.Dispatch.Static.Unsafe (reallyUnsafeLiftMapIO)
import GHC.Stack (CallStack, HasCallStack, callStack, getCallStack)
import NanoUI.Messages (FrameMsg (..))
import NanoUI.Context (Context (..), askHostIO, pushMessage)
import NanoUI.Id (WidgetId (..), fnv1a, hashSrcLoc, hashWidgetId)
import NanoUI.Input (Input)

-- Concrete app stack: Ui plus IO. Widgets stay polymorphic so other
-- effects (State, Error, extra IO) can sit under Ui.
type NanoUI = Eff '[Ui, IOE]

data Ui :: Effect

type instance DispatchOf Ui = Static WithSideEffects

data instance StaticRep Ui = UiRep !Context !Input

{-# INLINE runUi #-}
runUi :: IOE :> es => Context -> Input -> Eff (Ui : es) a -> Eff es a
runUi ctx inp = evalStaticRep (UiRep ctx inp)

{-# INLINE runNanoUI #-}
runNanoUI :: Context -> Input -> NanoUI a -> IO a
runNanoUI ctx inp = runEff . runUi ctx inp

{-# INLINE uiIO #-}
uiIO :: Ui :> es => IO a -> Eff es a
uiIO m = do
  UiRep {} <- getStaticRep
  unsafeEff_ m

{-# INLINE uiFinally #-}
uiFinally :: Eff es a -> IO b -> Eff es a
uiFinally m cleanup = reallyUnsafeLiftMapIO (`finally` cleanup) m

{-# INLINE emit #-}
emit :: (Typeable msg, Ui :> es) => msg -> Eff es ()
emit msg = do
  ctx <- askContext
  uiIO (pushMessage ctx (FrameMsg msg))

{-# INLINE withKey #-}
withKey :: (Hashable k, Ui :> es) => k -> Eff es a -> Eff es a
withKey k m = do
  ctx <- askContext
  old <- uiIO (readIORef (ctxIdSalt ctx))
  uiIO (writeIORef (ctxIdSalt ctx) (old `mix64` fromIntegral (hash k)))
  uiFinally m (writeIORef (ctxIdSalt ctx) old)

-- The whole stack is hashed, not just its head: the head always points at this
-- module, so distinct user call sites are only distinguishable by outer frames.
{-# INLINE currentId #-}
currentId :: (HasCallStack, Ui :> es) => Eff es WidgetId
currentId = do
  ctx <- askContext
  salt <- uiIO (readIORef (ctxIdSalt ctx))
  let base = hashCallStack callStack
  pure (WidgetId (base `mix64` salt))

hashCallStack :: CallStack -> Word64
hashCallStack cs =
  foldl
    (\acc (fn, loc) -> acc `mix64` fnv1a fn `mix64` hashWidgetId (hashSrcLoc loc))
    14695981039346656037
    (getCallStack cs)

{-# INLINE askContext #-}
askContext :: Ui :> es => Eff es Context
askContext = do
  UiRep ctx _ <- getStaticRep
  pure ctx

{-# INLINE askInput #-}
askInput :: Ui :> es => Eff es Input
askInput = do
  UiRep _ inp <- getStaticRep
  pure inp

{-# INLINE askHost #-}
askHost :: (Typeable a, Ui :> es) => Eff es (Maybe a)
askHost = do
  ctx <- askContext
  uiIO (askHostIO ctx)

{-# INLINE mix64 #-}
mix64 :: Word64 -> Word64 -> Word64
mix64 h k = (h `xor` k) * 1099511628211
