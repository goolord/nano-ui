{-# LANGUAGE ImplicitParams #-}

-- | Reducer-style views with one statically checked message type.
--
-- A 'NanoUIE' view is an ordinary 'NanoUI' action with the view's message
-- sink in scope, so every widget exported by core "NanoUI" (re-exported
-- here) runs in it unchanged, including containers, overlays and widgets
-- taking callbacks or several bodies. 'emit' and the adapters below send
-- messages of the view's type; emitting another type is a compile error.
module NanoUI.Emit
  ( NanoUIE
  , emit
  , emitWhen
  , emitChanged
  , emitEdited
  , mapMessages
  , runNanoUIE
  , runFrameE
  , runFrameReduce
  , runClickReduce
  , reduceMessages
  , reduceUpdates

    -- * Running a session
  , Reducer
  , newReducer
  , reducerModel
  , reduceFrame

    -- * Deprecated
  , emitWhenE
  , emitChangedE
  , emitEditedE
  , liftNanoUI
  , withNanoUI
  , withRunInNanoUIE

    -- * Core widgets
  , module NanoUI
  )
where

import Control.Monad (unless, when)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import NanoUI
import NanoUI.Internal.Context (isDirty, markDirty)
import NanoUI.Testing (Context, DrawData, runFrame)
import NanoUI.Testing.Harness (clickPair)

import NanoUI.Emit.Internal
  ( NanoUIE
  , collectMessages
  , emit
  , liftNanoUI
  , mapMessages
  , runNanoUIE
  , withNanoUI
  , withRunInNanoUIE
  , withSink
  )

-- | Emit when a widget activates.
--
-- > emitWhen (button "Save") Save
{-# INLINE emitWhen #-}
emitWhen :: NanoUI Bool -> msg -> NanoUIE msg ()
emitWhen widget msg = widget >>= \active -> when active (emit msg)

-- | Run a 'Var'-based input widget on a temporary 'Var' initialized to @old@,
-- and emit only when the value changes. A widget that emits itself, given by
-- name, needs a lambda: @emitChanged (\\v -> myInput v) old SetValue@.
--
-- > emitChanged (slider 0 100) volume SetVolume
{-# INLINE emitChanged #-}
emitChanged :: Eq a => (Var a -> NanoUI r) -> a -> (a -> msg) -> NanoUIE msg ()
emitChanged widget old toMsg = do
  (_, new) <- withValue old widget
  when (new /= old) (emit (toMsg new))

-- | Emit a changed value only when the input's response also reports an
-- edit.
{-# INLINE emitEdited #-}
emitEdited ::
  (Eq a, HasResponse r) => (Var a -> NanoUI r) -> a -> (a -> msg) -> NanoUIE msg ()
emitEdited widget old toMsg = do
  (r, new) <- withValue old widget
  when (respChanged (toResponse r) && new /= old) (emit (toMsg new))

-- | 'emitWhen', from when widgets needed wrapping.
{-# DEPRECATED emitWhenE "Use emitWhen" #-}
{-# INLINE emitWhenE #-}
emitWhenE :: NanoUI Bool -> msg -> NanoUIE msg ()
emitWhenE = emitWhen

-- | 'emitChanged', from when widgets needed wrapping.
{-# DEPRECATED emitChangedE "Use emitChanged" #-}
{-# INLINE emitChangedE #-}
emitChangedE :: Eq a => (Var a -> NanoUI r) -> a -> (a -> msg) -> NanoUIE msg ()
emitChangedE = emitChanged

-- | 'emitEdited', from when widgets needed wrapping.
{-# DEPRECATED emitEditedE "Use emitEdited" #-}
{-# INLINE emitEditedE #-}
emitEditedE ::
  (Eq a, HasResponse r) => (Var a -> NanoUI r) -> a -> (a -> msg) -> NanoUIE msg ()
emitEditedE = emitEdited

-- | Run a complete frame and collect messages from all its passes in order.
-- The queue belongs to this invocation: an exception cannot leak messages
-- into a later frame, nor can another runner consume them.
runFrameE :: Context -> Input -> NanoUIE msg a -> IO (a, [msg], DrawData, Bool)
runFrameE ctx inp view = do
  ((a, draw, dirty), msgs) <- collectMessages (\sink -> runFrame ctx inp (withSink sink view))
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
  (model', dirty') <- reduceAfterFrame update ctx model msgs dirty
  pure (a, model', msgs, draw, dirty')

-- | Reduce a frame's messages; a changed model asks for another frame.
-- Answers the model and whether the context needs another frame.
reduceAfterFrame :: Eq model => (msg -> model -> model) -> Context -> model -> [msg] -> Bool -> IO (model, Bool)
reduceAfterFrame update ctx model msgs dirty = do
  let model' = reduceMessages update model msgs
  when (model' /= model) (markDirty ctx)
  (model',) . (dirty ||) <$> isDirty ctx

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

-- | A reducer session's update function and model, for a backend's runner.
-- It also keeps the messages a view emitted after its frame ended: a further
-- window's view ('NanoUI.nativeWindow') runs in that window's own frames
-- and emits through the sink of the view that declared it.
data Reducer msg model = Reducer
  { rUpdate :: msg -> model -> model
  , rModel :: IORef model
  , rFrame :: IORef (Maybe [msg])
  -- ^ The running frame's messages, newest first; 'Nothing' between frames.
  , rLate :: IORef [msg]
  -- ^ Messages emitted between frames, newest first.
  }

newReducer :: (msg -> model -> model) -> model -> IO (Reducer msg model)
newReducer update model = Reducer update <$> newIORef model <*> newIORef Nothing <*> newIORef []

-- | The current model.
reducerModel :: Reducer msg model -> IO model
reducerModel = readIORef . rModel

-- | Run a frame of the view on the model, as 'runFrameReduce' does, and keep
-- the result. Messages emitted after an earlier frame ended are reduced
-- first, so the frame draws them; a message emitted after this frame ends
-- asks the context for another.
reduceFrame :: Eq model => Reducer msg model -> Context -> Input -> (model -> NanoUIE msg a) -> IO (a, DrawData, Bool)
reduceFrame r ctx inp view = do
  late <- readIORef (rLate r)
  unless (null late) (writeIORef (rLate r) [])
  model <- reduceMessages (rUpdate r) <$> readIORef (rModel r) <*> pure (reverse late)
  writeIORef (rFrame r) (Just [])
  (a, draw, dirty) <- runFrame ctx inp (withSink sink (view model))
  msgs <- maybe [] reverse <$> readIORef (rFrame r)
  writeIORef (rFrame r) Nothing
  (model', dirty') <- reduceAfterFrame (rUpdate r) ctx model msgs dirty
  writeIORef (rModel r) model'
  pure (a, draw, dirty')
  where
    sink msg = liftIO $ readIORef (rFrame r) >>= \case
      Just msgs -> writeIORef (rFrame r) (Just (msg : msgs))
      Nothing -> modifyIORef' (rLate r) (msg :) >> markDirty ctx

-- | Strictly fold messages through an update function in traversal order.
reduceMessages ::
  Foldable f => (msg -> model -> model) -> model -> f msg -> model
reduceMessages update = foldl' (flip update)

-- | Apply function-valued messages in traversal order.
reduceUpdates :: Foldable f => model -> f (model -> model) -> model
reduceUpdates = reduceMessages ($)
