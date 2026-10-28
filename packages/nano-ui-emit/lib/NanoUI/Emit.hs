-- | Reducer-style views with one statically checked message type.
--
-- All widgets exported by core "NanoUI" are wrapped and exported here,
-- operating directly in 'NanoUIE', so you do not have to manually lift or
-- wrap ordinary widgets.
--
-- Ordinary 'NanoUI' actions can still be lifted with 'liftNanoUI' or wrapped
-- with 'withNanoUI'.
module NanoUI.Emit
  ( NanoUIE
  , liftNanoUI
  , withNanoUI
  , withRunInNanoUIE
  , mapMessages
  , runNanoUIE
  , emit
  , emitWhen
  , emitWhenE
  , emitChanged
  , emitChangedE
  , emitEdited
  , emitEditedE
  , runFrameE
  , runFrameReduce
  , runClickReduce
  , reduceMessages
  , reduceUpdates
  , module NanoUI.Emit.Widgets
  )
where

import Control.Monad (when)
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import NanoUI (HasResponse (..), Input, NanoUI, Response, V2, Var, respChanged, withValue)
import NanoUI.Internal.Context (isDirty, markDirty)
import NanoUI.Testing (Context, DrawData, runFrame)
import NanoUI.Testing.Harness (clickPair)

import NanoUI.Emit.Internal
  ( NanoUIE (..)
  , collectMessages
  , emit
  , liftNanoUI
  , mapMessages
  , runNanoUIE
  , withNanoUI
  , withRunInNanoUIE
  )
import NanoUI.Emit.Widgets

-- | Emit when an ordinary 'NanoUI' widget activates.
{-# INLINE emitWhen #-}
emitWhen :: NanoUI Bool -> msg -> NanoUIE msg ()
emitWhen widget msg = liftNanoUI widget >>= \active -> when active (emit msg)

-- | Emit when a wrapped 'NanoUIE' widget activates.
--
-- > emitWhenE (button "Save") Save
{-# INLINE emitWhenE #-}
emitWhenE :: NanoUIE msg Bool -> msg -> NanoUIE msg ()
emitWhenE widget msg = widget >>= \active -> when active (emit msg)

-- | Run a 'Var'-based input widget on a temporary 'Var' initialized to @old@,
-- and emit only when the value changes.
--
-- > emitChanged (slider 0 100) volume SetVolume
{-# INLINE emitChanged #-}
emitChanged :: Eq a => (Var a -> NanoUI r) -> a -> (a -> msg) -> NanoUIE msg ()
emitChanged widget old toMsg = do
  (_, new) <- liftNanoUI (withValue old widget)
  when (new /= old) (emit (toMsg new))

-- | Run a wrapped 'Var'-based input widget on a temporary 'Var' initialized to
-- @old@, and emit only when the value changes.
--
-- > emitChangedE (slider 0 100) volume SetVolume
{-# INLINE emitChangedE #-}
emitChangedE :: Eq a => (Var a -> NanoUIE msg r) -> a -> (a -> msg) -> NanoUIE msg ()
emitChangedE widget old toMsg = do
  sink <- NanoUIE ask
  let runAction (NanoUIE (ReaderT f)) = f sink
  (_, new) <- liftNanoUI (withValue old (runAction . widget))
  when (new /= old) (emit (toMsg new))

-- | Emit a changed value only when the input's response also reports an
-- edit.
{-# INLINE emitEdited #-}
emitEdited ::
  (Eq a, HasResponse r) => (Var a -> NanoUI r) -> a -> (a -> msg) -> NanoUIE msg ()
emitEdited widget old toMsg = do
  (r, new) <- liftNanoUI (withValue old widget)
  when (respChanged (toResponse r) && new /= old) (emit (toMsg new))

-- | Emit a changed value only when the wrapped input's response also reports
-- an edit.
{-# INLINE emitEditedE #-}
emitEditedE ::
  (Eq a, HasResponse r) => (Var a -> NanoUIE msg r) -> a -> (a -> msg) -> NanoUIE msg ()
emitEditedE widget old toMsg = do
  sink <- NanoUIE ask
  let runAction (NanoUIE (ReaderT f)) = f sink
  (r, new) <- liftNanoUI (withValue old (runAction . widget))
  when (respChanged (toResponse r) && new /= old) (emit (toMsg new))

-- | Run a complete frame and collect messages from all its passes in order.
-- The queue belongs to this invocation: an exception cannot leak messages
-- into a later frame, nor can another runner consume them.
runFrameE :: Context -> Input -> NanoUIE msg a -> IO (a, [msg], DrawData, Bool)
runFrameE ctx inp (NanoUIE view) = do
  ((a, draw, dirty), msgs) <- collectMessages (runFrame ctx inp . runReaderT view)
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
