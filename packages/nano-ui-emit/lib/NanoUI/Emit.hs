-- | Reducer-style views with one statically checked message type. Ordinary
-- widgets are lifted with 'liftNanoUI'; containers use 'withNanoUI' or
-- 'withRunInNanoUIE'. No runtime type tests or message filtering are involved.
module NanoUI.Emit
  ( NanoUIE
  , liftNanoUI
  , withNanoUI
  , withRunInNanoUIE
  , runNanoUIE
  , emit
  , emitWhen
  , emitChanged
  , emitEdited
  , runFrameE
  , runFrameReduce
  , runClickReduce
  , reduceMessages
  , reduceUpdates
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT (..), ask, mapReaderT)
import Data.IORef (modifyIORef', newIORef, readIORef)
import NanoUI (Input, NanoUI, Response, V2, respChanged)
import NanoUI.Internal.Context (isDirty, markDirty)
import NanoUI.Testing (Context, DrawData, runFrame)
import NanoUI.Testing.Harness (clickPair)

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

-- | Collect messages from one UI action, in emission order. For complete
-- frames use 'runFrameE', which also retains messages across rebuilds.
runNanoUIE :: NanoUIE msg a -> NanoUI (a, [msg])
runNanoUIE (NanoUIE view) = do
  messages <- liftIO (newIORef [])
  a <- runReaderT view (\msg -> liftIO (modifyIORef' messages (msg :)))
  msgs <- liftIO (reverse <$> readIORef messages)
  pure (a, msgs)

-- | Emit a message of this view's message type.
{-# INLINE emit #-}
emit :: msg -> NanoUIE msg ()
emit msg = NanoUIE (ask >>= lift . ($ msg))

-- | Emit when an ordinary widget activates.
{-# INLINE emitWhen #-}
emitWhen :: NanoUI Bool -> msg -> NanoUIE msg ()
emitWhen widget msg = liftNanoUI widget >>= \active -> when active (emit msg)

-- | Run a control once and emit only a changed value.
{-# INLINE emitChanged #-}
emitChanged :: Eq a => (a -> NanoUI a) -> a -> (a -> msg) -> NanoUIE msg ()
emitChanged widget old toMsg = liftNanoUI (widget old) >>= \new -> when (new /= old) (emit (toMsg new))

-- | Emit a changed value only when the response also reports an edit.
{-# INLINE emitEdited #-}
emitEdited ::
  Eq a => (a -> NanoUI (Response, a)) -> a -> (a -> msg) -> NanoUIE msg ()
emitEdited widget old toMsg = do
  (resp, new) <- liftNanoUI (widget old)
  when (respChanged resp && new /= old) (emit (toMsg new))

-- | Run a complete frame and collect messages from all its passes in order.
-- The queue belongs to this invocation: an exception cannot leak messages
-- into a later frame, nor can another runner consume them.
runFrameE :: Context -> Input -> NanoUIE msg a -> IO (a, [msg], DrawData, Bool)
runFrameE ctx inp (NanoUIE view) = do
  messages <- newIORef []
  (a, draw, dirty) <-
    runFrame
      ctx
      inp
      (runReaderT view (\msg -> liftIO (modifyIORef' messages (msg :))))
  msgs <- reverse <$> readIORef messages
  pure (a, msgs, draw, dirty)

-- | View the model, then reduce all messages at frame end. The drawing is
-- from the pre-reduce model; a changed model requests a follow-up frame.
runFrameReduce ::
  Eq model =>
  (msg -> model -> model)
  -> Context
  -> Input
  -> model
  -> (model -> NanoUIE msg a)
  -> IO (a, model, [msg], DrawData, Bool)
runFrameReduce update ctx inp model view = do
  (a, msgs, draw, dirty) <- runFrameE ctx inp (view model)
  let
    model' = reduceMessages update model msgs
  when (model' /= model) (markDirty ctx)
  dirty' <- isDirty ctx
  pure (a, model', msgs, draw, dirty || dirty')

-- | Drive a left press and release through a reducer, returning the final
-- model, release-frame messages, and release-frame dirty flag.
runClickReduce ::
  Eq model =>
  (msg -> model -> model)
  -> Context
  -> Input
  -> model
  -> (model -> NanoUIE msg Response)
  -> V2
  -> IO (model, [msg], Bool)
runClickReduce update ctx inp model view pos = do
  let
    (press, release) = clickPair inp pos
  (_, modelP, _, _, _) <- runFrameReduce update ctx press model view
  (_, modelR, msgs, _, dirty) <- runFrameReduce update ctx release modelP view
  pure (modelR, msgs, dirty)

-- | Strictly fold messages through an update function in traversal order.
reduceMessages ::
  Foldable f => (msg -> model -> model) -> model -> f msg -> model
reduceMessages update = foldl' (flip update)

-- | Apply function-valued messages in traversal order.
reduceUpdates :: Foldable f => model -> f (model -> model) -> model
reduceUpdates = reduceMessages ($)
