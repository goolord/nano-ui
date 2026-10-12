-- | Reading and writing 'Var's from the view, and the bookkeeping that
-- decides what a write costs.
--
-- Within a pass, a tracked read ('readVar') stamps the cell as observed and
-- seen, and a widget bound to the Var ('bindVar') stamps it as observed. A
-- write that changes a cell observed earlier in the same pass is a
-- backwards write: something already drew the old value. In the first pass
-- that reruns the view; in the second it asks for a follow-up frame. A
-- write only repaints the whole window when a tracked read saw the old
-- value, or reads the new one later in the frame, since what a view does
-- with a value it read is beyond any diff. A widget that drew the value
-- it wrote damages only itself.
module NanoUI.Internal.Var
  ( newVar
  , useVar
  , useEnumVar
  , readVar
  , peekVar
  , writeVar
  , modifyVar
  , readVarIO
  , writeVarIO
  , modifyVarIO
  , withValue
  , Binding (..)
  , bindVar
  , bindVarBy
  , bindVarOn
  , requestRerun
  , beginVarPass
  , takeVarRerun
  , endVarFrame
  , lastFramePasses
  )
where

import Control.Monad (unless, when)
import Data.Bits (complement, (.&.), (.|.))
import Data.IORef (atomicModifyIORef', readIORef, writeIORef)
import Data.IntMap.Strict qualified as IM
import Data.Primitive.PrimArray (readPrimArray, writePrimArray)
import Data.Word (Word64)
import NanoUI.Internal.Context (Context (..), damageFull, markDirty, markDirtyCovered)
import NanoUI.Internal.Equality (eqByPtr, ptrEq)
import NanoUI.Internal.Id.Cursor (takeVarKey)
import NanoUI.Internal.Monad (NanoUI, askContext, liftIO, withContext)
import NanoUI.Internal.Var.Cell
import NanoUI.Internal.Var.Types

flagRerun, flagFull :: Word64
flagRerun = 1
flagFull = 2

{-# INLINE frameSlot #-}
frameSlot :: Context -> Int -> IO Word64
frameSlot ctx = readPrimArray (ctxVarFrame ctx)

{-# INLINE setFlag #-}
setFlag :: Context -> Word64 -> IO ()
setFlag ctx f = do
  fs <- frameSlot ctx flagSlot
  writePrimArray (ctxVarFrame ctx) flagSlot (fs .|. f)

-- | A Var with its own cell, for state the application owns. Make it once,
-- outside the view, and pass it in; a Var made inside the view would be new
-- on every frame.
newVar :: a -> IO (Var a)
newVar a = (\c -> VarRef c Here) <$> newCell a

-- | A Var kept at this position in the view, made with @initial@ the first
-- time the position is reached. Positional Vars count separately from
-- widgets, so widgets that come and go never move them; a 'useVar' that runs
-- on some frames and not others belongs in a 'NanoUI.scope'. A slot that
-- finds a value of another type starts over from @initial@.
{-# INLINE useVar #-}
useVar :: VarSlot a => a -> NanoUI (Var a)
useVar = slotVar slotMatch slotWrap

-- | 'useVar' for an enumeration, kept as its index. An index out of range
-- reads as @initial@.
useEnumVar :: forall a. (Bounded a, Enum a) => a -> NanoUI (Var a)
useEnumVar initial = do
  v <- slotVar (\case SEnum v -> Just v; _ -> Nothing) SEnum (fromEnum initial)
  let decode i
        | i >= fromEnum (minBound :: a) && i <= fromEnum (maxBound :: a) = toEnum i
        | otherwise = initial
  pure (zoomVar decode (\a _ -> fromEnum a) v)

-- | The Var at the next positional slot, if it holds the expected type, or
-- a new one holding @initial@.
{-# INLINE slotVar #-}
slotVar :: (SlotVar -> Maybe (Var b)) -> (Var b -> SlotVar) -> b -> NanoUI (Var b)
slotVar match wrap initial = withContext $ \ctx -> do
  key <- takeVarKey (ctxIdCursor ctx)
  slots <- readIORef (ctxVarSlots ctx)
  case IM.lookup key slots >>= match of
    Just v -> pure v
    Nothing -> do
      v <- newVar initial
      writeIORef (ctxVarSlots ctx) $! IM.insert key (wrap v) slots
      pure v

-- | The Var's current value. The view's read is recorded: a later write in
-- the same pass runs the view again so this read sees it. Read a Var where
-- you show it, after the widgets that edit it, and no rerun is needed.
readVar :: Var a -> NanoUI a
readVar (Var a) = pure a
readVar (VarRef cell@(Cell ref _) l) = withContext $ \ctx -> do
  unless (isInert cell) $ do
    p <- frameSlot ctx passSlot
    when (p /= 0) $ do
      writeStamp cell obsSlot p
      seen <- readStamp cell seenSlot
      when (seen /= p) $ do
        writeStamp cell seenSlot p
        chg <- readStamp cell chgSlot
        base <- frameSlot ctx baseSlot
        when (chg >= base) (setFlag ctx flagFull)
  peekLens l =<< readIORef ref

-- | The Var's current value, without recording the read: for a value used
-- in a decision whose effects show elsewhere, such as starting a task.
-- Nothing drawn from it is redrawn when the Var changes later in the
-- frame; draw from 'readVar'.
peekVar :: Var a -> NanoUI a
peekVar v = liftIO (readVarIO v)

-- | Replace the value. An equal value changes nothing.
writeVar :: Eq a => Var a -> a -> NanoUI ()
writeVar v a = modifyVar v (const a)

-- | Apply a change to the latest value, so successive changes in one frame
-- all land. A change that leaves the value equal does nothing.
modifyVar :: Eq a => Var a -> (a -> a) -> NanoUI ()
modifyVar (Var _) _ = pure ()
modifyVar (VarRef cell l) f = withContext $ \ctx -> do
  changed <- modifyRoot cell l f eqByPtr
  when changed (noteWrite ctx cell)

-- | Read from any thread. Not recorded.
readVarIO :: Var a -> IO a
readVarIO (Var a) = pure a
readVarIO (VarRef (Cell ref _) l) = peekLens l =<< readIORef ref

-- | Write from any thread, or from setup code. Nothing is woken: after
-- writing from another thread, run the action from 'NanoUI.askWake' so the
-- view runs again.
writeVarIO :: Eq a => Var a -> a -> IO ()
writeVarIO v a = modifyVarIO v (const a)

-- | 'modifyVar' from any thread. Concurrent changes all land.
modifyVarIO :: Eq a => Var a -> (a -> a) -> IO ()
modifyVarIO (Var _) _ = pure ()
modifyVarIO (VarRef cell l) f = do
  changed <- modifyRoot cell l f eqByPtr
  when (changed && not (isInert cell)) (stampChange cell)

-- | Run @body@ on a Var holding @a@ and return what it left there: for
-- giving a value from elsewhere (a model, a message) to a widget that takes
-- a Var. The Var is the body's alone, so it never reruns or repaints
-- anything by itself.
withValue :: a -> (Var a -> NanoUI r) -> NanoUI (r, a)
withValue a body = do
  cell@(Cell ref _) <- liftIO (newInertCell a)
  r <- body (VarRef cell Here)
  a' <- liftIO (readIORef ref)
  pure (r, a')

-- | Change the part of a cell a lens shows, atomically. Says whether it
-- changed. An inert cell is one body's alone and skips the atomic update.
{-# INLINE modifyRoot #-}
modifyRoot :: Cell s -> Lens s a -> (a -> a) -> (a -> a -> Bool) -> IO Bool
modifyRoot cell@(Cell ref _) l f eq
  | isInert cell = do
      (root', changed) <- step <$> readIORef ref
      when changed (writeIORef ref $! root')
      pure changed
  | otherwise = atomicModifyIORef' ref step
  where
    step root =
      let old = viewLens l root
          new = f old
       in if eq new old then (root, False) else (overLens l (const new) root, True)

-- | Stamp a cell changed outside a pass. The next frame's tracked reads see
-- the change as new and repaint whole; widgets bound to it diff themselves.
stampChange :: Cell s -> IO ()
stampChange cell = writeStamp cell chgSlot =<< nextTick

-- | Record a changing write made by view code.
noteWrite :: Context -> Cell s -> IO ()
noteWrite ctx cell = unless (isInert cell) $ do
  p <- frameSlot ctx passSlot
  if p == 0
    then stampChange cell >> markDirtyCovered ctx
    else do
      obs <- readStamp cell obsSlot
      seen <- readStamp cell seenSlot
      noteInPass ctx cell p (obs == p) (seen == p)

-- | Record a write in pass @p@: rerun if something observed the cell
-- earlier in the pass, and repaint whole if a tracked read saw it.
noteInPass :: Context -> Cell s -> Word64 -> Bool -> Bool -> IO ()
noteInPass ctx cell p observed seen = do
  when observed (backwards ctx seen)
  when seen (setFlag ctx flagFull)
  writeStamp cell chgSlot p

-- | Something in this pass drew the old value: run the view again, or, in
-- its last pass, ask for another frame. @opaque@ when a tracked read saw
-- it, so that frame repaints whole.
backwards :: Context -> Bool -> IO ()
backwards ctx opaque = do
  passNo <- frameSlot ctx passNoSlot
  if passNo <= 1
    then setFlag ctx flagRerun
    else if opaque then markDirty ctx else markDirtyCovered ctx

-- | How a widget bound to a Var draws it.
data Binding
  = -- | The widget draws the value it returns in the same pass.
    RenderFinal
  | -- | The widget draws the value it was given, so a change it makes needs
    -- the view run again to show.
    Conservative
  deriving (Eq, Show)

-- | 'bindVarBy' comparing with '=='.
{-# INLINE bindVar #-}
bindVar :: Eq a => Binding -> Var a -> (a -> NanoUI (r, a)) -> NanoUI r
bindVar = bindVarBy eqByPtr

-- | Run a value-in, value-out widget on a Var: give it the Var's value and
-- store what it returns. A constant Var gets its value shown and the edit
-- dropped. If something inside the widget (a control among its adornments,
-- a tab's body) wrote the Var while it ran, that write wins.
{-# INLINE bindVarBy #-}
bindVarBy :: (a -> a -> Bool) -> Binding -> Var a -> (a -> NanoUI (r, a)) -> NanoUI r
bindVarBy eq mode = bindWith eq mode fst snd

-- | 'bindVar' for a widget whose result carries its value, read with
-- @value@.
{-# INLINE bindVarOn #-}
bindVarOn :: Eq a => Binding -> (r -> a) -> Var a -> (a -> NanoUI r) -> NanoUI r
bindVarOn mode value = bindWith eqByPtr mode id value

{-# INLINE bindWith #-}
bindWith :: (a -> a -> Bool) -> Binding -> (x -> r) -> (x -> a) -> Var a -> (a -> NanoUI x) -> NanoUI r
bindWith _ _ result _ (Var a) widget = result <$> widget a
bindWith eq mode result value (VarRef cell@(Cell ref _) l) widget = do
  ctx <- askContext
  (root0, a0, before) <- liftIO $ do
    root0 <- readIORef ref
    a0 <- peekLens l root0
    before <- observeBinding ctx cell
    pure (root0, a0, before)
  x <- widget a0
  let a1 = value x
  liftIO $ do
    root1 <- readIORef ref
    -- While the root is the one the widget was given, the part it shows is
    -- still a0, so the comparison here is the only one needed.
    when (ptrEq root1 root0 && not (eq a1 a0)) $ do
      changed <-
        if isInert cell
          then True <$ (writeIORef ref $! overLens l (const a1) root0)
          else atomicModifyIORef' ref $ \root ->
            if ptrEq root root0 || not (eq a1 (viewLens l root))
              then (overLens l (const a1) root, True)
              else (root, False)
      when changed (noteBound ctx cell mode before)
  pure (result x)

-- | Stamp a bound cell observed, returning whether something observed it,
-- and whether a tracked read saw it, earlier in this pass.
observeBinding :: Context -> Cell s -> IO (Bool, Bool)
observeBinding ctx cell
  | isInert cell = pure (False, False)
  | otherwise = do
      p <- frameSlot ctx passSlot
      if p == 0
        then pure (False, False)
        else do
          obs <- readStamp cell obsSlot
          seen <- readStamp cell seenSlot
          writeStamp cell obsSlot p
          pure (obs == p, seen == p)

-- | Record a bound widget's changing write. Its own node shows the value,
-- so only an earlier observer needs the view rerun.
noteBound :: Context -> Cell s -> Binding -> (Bool, Bool) -> IO ()
noteBound ctx cell mode (obsBefore, seenBefore) = unless (isInert cell) $ do
  p <- frameSlot ctx passSlot
  if p == 0
    then stampChange cell >> markDirtyCovered ctx
    else noteInPass ctx cell p (obsBefore || mode == Conservative) seenBefore

-- | Ask for the view to run again this frame, or for a follow-up frame when
-- it already has. The follow-up repaints only what its diffs show.
requestRerun :: Context -> IO ()
requestRerun ctx = do
  p <- frameSlot ctx passSlot
  if p == 0 then markDirtyCovered ctx else backwards ctx False

-- | Start view pass @n@ of a frame. The first pass drops any requests left
-- by a frame an exception cut short; that frame never moved the base, so
-- reads in this one still see its changes as new. No handler is installed
-- per frame, since one costs an allocation every frame.
beginVarPass :: Context -> Int -> IO ()
beginVarPass ctx n = do
  t <- nextTick
  let arr = ctxVarFrame ctx
  writePrimArray arr passSlot t
  writePrimArray arr passNoSlot (fromIntegral n)
  when (n == 1) (writePrimArray arr flagSlot 0)

-- | Whether a write asked for the view to run again, clearing the request.
takeVarRerun :: Context -> IO Bool
takeVarRerun ctx = do
  fs <- frameSlot ctx flagSlot
  writePrimArray (ctxVarFrame ctx) flagSlot (fs .&. complement flagRerun)
  pure (fs .&. flagRerun /= 0)

-- | Finish the frame's passes: queue full damage if a read needs it, and
-- start the next frame's changes from here.
endVarFrame :: Context -> IO ()
endVarFrame ctx = do
  let arr = ctxVarFrame ctx
  fs <- frameSlot ctx flagSlot
  writePrimArray arr lastPassesSlot =<< frameSlot ctx passNoSlot
  writePrimArray arr passSlot 0
  writePrimArray arr flagSlot 0
  writePrimArray arr baseSlot =<< nextTick
  when (fs .&. flagFull /= 0) (damageFull ctx)

-- | How many view passes the last frame ran.
lastFramePasses :: Context -> IO Int
lastFramePasses ctx = fromIntegral <$> frameSlot ctx lastPassesSlot
