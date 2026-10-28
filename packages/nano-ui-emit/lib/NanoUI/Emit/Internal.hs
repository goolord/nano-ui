{-# LANGUAGE ImplicitParams #-}

-- | Core types and emission mechanics for 'NanoUIE'.
module NanoUI.Emit.Internal
  ( NanoUIE
  , liftNanoUI
  , withNanoUI
  , withRunInNanoUIE
  , mapMessages
  , runNanoUIE
  , withSink
  , collectMessages
  , emit
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (modifyIORef', newIORef, readIORef)
import NanoUI (NanoUI)

-- | A UI action that can emit only @msg@: an ordinary 'NanoUI' action with
-- the view's message sink in scope as the implicit parameter @?emit@. Every
-- core widget, container and callback runs in it unchanged, and an emission
-- inside any of them goes to the sink of the view around it. The sink is
-- shared by every pass of a frame, so a local-state rebuild cannot discard
-- earlier messages.
type NanoUIE msg a = (?emit :: msg -> NanoUI ()) => NanoUI a

-- | Use an ordinary widget or UI operation in an emitting view. No longer
-- needed: an emitting view is a 'NanoUI' action.
{-# DEPRECATED liftNanoUI "NanoUIE views are NanoUI actions; use the action directly" #-}
{-# INLINE liftNanoUI #-}
liftNanoUI :: NanoUI a -> NanoUI a
liftNanoUI = id

-- | Wrap a typed body in a layout, key, style, or other ordinary UI scope.
-- No longer needed: apply the scope to the body.
{-# DEPRECATED withNanoUI "NanoUIE views are NanoUI actions; apply the scope directly" #-}
{-# INLINE withNanoUI #-}
withNanoUI :: (NanoUI a -> NanoUI b) -> NanoUI a -> NanoUI b
withNanoUI = id

-- | Supply typed bodies to widgets taking callbacks or multiple bodies. No
-- longer needed: emitting bodies are 'NanoUI' actions.
{-# DEPRECATED withRunInNanoUIE "NanoUIE views are NanoUI actions; pass the bodies directly" #-}
{-# INLINE withRunInNanoUIE #-}
withRunInNanoUIE :: ((forall r. NanoUI r -> NanoUI r) -> NanoUI a) -> NanoUI a
withRunInNanoUIE body = body id

-- | Embed a child's message type in its parent's. Messages go straight to
-- the parent sink in traversal order, including across frame rebuilds.
--
-- > mapMessages SettingsMessage (settingsView settings)
{-# INLINE mapMessages #-}
mapMessages :: (child -> parent) -> NanoUIE child a -> NanoUIE parent a
mapMessages f body = let ?emit = ?emit . f in body

-- | Run an emitting view with @sink@ as its message sink.
{-# INLINE withSink #-}
withSink :: (msg -> NanoUI ()) -> NanoUIE msg a -> NanoUI a
withSink sink body = let ?emit = sink in body

-- | Collect messages from one UI action, in emission order. For complete
-- frames use 'NanoUI.Emit.runFrameE', which also retains messages across
-- rebuilds.
runNanoUIE :: NanoUIE msg a -> NanoUI (a, [msg])
runNanoUIE view = collectMessages (`withSink` view)

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
emit msg = ?emit msg
