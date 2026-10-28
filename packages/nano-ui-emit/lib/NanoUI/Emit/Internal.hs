-- | Core internal types and emission mechanics for 'NanoUIE'.
module NanoUI.Emit.Internal
  ( NanoUIE (..)
  , liftNanoUI
  , withNanoUI
  , withRunInNanoUIE
  , mapMessages
  , runNanoUIE
  , collectMessages
  , emit
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT (..), ask, mapReaderT)
import Data.IORef (modifyIORef', newIORef, readIORef)
import NanoUI (NanoUI)

-- | A UI computation that can emit only @msg@. The sink is shared by every
-- pass of a frame, so a local-state rebuild cannot discard earlier messages.
newtype NanoUIE msg a = NanoUIE (ReaderT (msg -> NanoUI ()) NanoUI a)
  deriving newtype (Functor, Applicative, Monad, MonadIO)

-- | Use an ordinary widget or UI operation in an emitting view.
{-# INLINE liftNanoUI #-}
liftNanoUI :: NanoUI a -> NanoUIE msg a
liftNanoUI = NanoUIE . lift

-- | Wrap a typed body in a layout, key, style, or other ordinary UI scope.
--
-- > withNanoUI column $ emitWhen (button "Save") Save
{-# INLINE withNanoUI #-}
withNanoUI :: (NanoUI a -> NanoUI b) -> NanoUIE msg a -> NanoUIE msg b
withNanoUI f (NanoUIE body) = NanoUIE (mapReaderT f body)

-- | Supply typed bodies to widgets taking callbacks or multiple bodies.
-- Call the supplied function only within the enclosing UI action.
{-# INLINE withRunInNanoUIE #-}
withRunInNanoUIE ::
  ((forall r. NanoUIE msg r -> NanoUI r) -> NanoUI a) -> NanoUIE msg a
withRunInNanoUIE body = NanoUIE $ ReaderT $ \sink ->
  body (\(NanoUIE action) -> runReaderT action sink)

-- | Embed a child's message type in its parent's. Messages go straight to
-- the parent sink in traversal order, including across frame rebuilds.
--
-- > mapMessages SettingsMessage (settingsView settings)
{-# INLINE mapMessages #-}
mapMessages :: (child -> parent) -> NanoUIE child a -> NanoUIE parent a
mapMessages f (NanoUIE body) = NanoUIE $ ReaderT $ \sink ->
  runReaderT body (sink . f)

-- | Collect messages from one UI action, in emission order. For complete
-- frames use 'runFrameE', which also retains messages across rebuilds.
runNanoUIE :: NanoUIE msg a -> NanoUI (a, [msg])
runNanoUIE (NanoUIE view) = collectMessages (runReaderT view)

-- | Run a body with a sink that queues its messages, and return them in
-- emission order.
collectMessages :: MonadIO m => ((msg -> NanoUI ()) -> m a) -> m (a, [msg])
collectMessages body = do
  messages <- liftIO (newIORef [])
  a <- body (\msg -> liftIO (modifyIORef' messages (msg :)))
  msgs <- liftIO (reverse <$> readIORef messages)
  pure (a, msgs)

-- | Emit a message of this view's message type.
{-# INLINE emit #-}
emit :: msg -> NanoUIE msg ()
emit msg = NanoUIE (ask >>= lift . ($ msg))
