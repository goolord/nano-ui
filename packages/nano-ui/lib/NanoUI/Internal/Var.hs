-- | Reading and mutating 'Var's from the view, and tracking dependencies for
-- reruns and damage.
--
-- Within a pass, a tracked read ('readVar') stamps the part of the cell it
-- reads as observed and seen, and a widget bound to the 'Var' ('bindVar')
-- stamps its part as observed. Parts are record fields and list elements,
-- two levels deep ('lensSig'). A mutation that changes a part observed
-- earlier in the same pass is a backwards write: an earlier widget already
-- rendered the old value. In the first pass that reruns the view; in the
-- second it schedules a follow-up frame. What a change shows is damaged
-- like any other view output: the nodes whose text, value or style it
-- changed repaint ('NanoUI.Internal.Damage').
module NanoUI.Internal.Var
  ( newVar
  , useVar
  , useEnumVar
  , readVar
  , readCell
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
import Data.Bits (complement, (.&.))
import Data.IORef (atomicModifyIORef', readIORef, writeIORef)
import Data.IntMap.Strict qualified as IM
import Data.Primitive.PrimArray (readPrimArray, writePrimArray)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable, typeRep)
import Data.Word (Word64)
import NanoUI.Internal.Context (Context (..), markDirty)
import NanoUI.Internal.Equality (eqByPtr, ptrEq)
import NanoUI.Internal.Id.Cursor (takeVarKey)
import NanoUI.Internal.Monad (NanoUI, askContext, liftIO, withContext)
import NanoUI.Internal.Var.Cell
import NanoUI.Internal.Var.Types

flagRerun :: Word64
flagRerun = 1

{-# INLINE frameSlot #-}
frameSlot :: Context -> Int -> IO Word64
frameSlot ctx = readPrimArray (ctxVarFrame ctx)

{-# INLINE setFlag #-}
setFlag :: Context -> Word64 -> IO ()
setFlag ctx = orWord (ctxVarFrame ctx) flagSlot

-- | Allocate a new mutable 'Var'. Create it once outside the view and pass it
-- in; calling 'newVar' inside the view would allocate a fresh 'Var' every
-- frame.
newVar :: a -> IO (Var a)
newVar a = (\c -> VarRef c Here) <$> newCell a

-- | Mutable 'Var' persisted at the current call-site slot, initialized to
-- @initial@ on first reach. Positional 'Var' slots are indexed separately from
-- widgets, so conditional widgets do not shift them; wrap a conditional
-- 'useVar' in 'NanoUI.scope'. Reinitializes to @initial@ if the slot
-- previously held a different type.
--
-- A slot lasts as long as the context, so hidden content keeps its state:
-- each 'NanoUI.withKey' key a 'useVar' ever ran under keeps one. For rows of
-- a collection whose keys keep coming, such as a feed, hold their state in a
-- 'NanoUI.Var.VarMap' and drop it with 'NanoUI.Var.retainVars'.
{-# INLINE useVar #-}
useVar :: VarSlot a => a -> NanoUI (Var a)
useVar = slotVar slotMatch slotWrap

-- | 'useVar' for an enumeration, stored by its 'Enum' index. An out-of-range
-- index decodes as @initial@, and a slot that held another enumeration
-- starts over.
useEnumVar :: forall a. (Bounded a, Enum a, Typeable a) => a -> NanoUI (Var a)
useEnumVar initial = do
  let ty = typeRep (Proxy @a)
  v <- slotVar (\case SEnum t v | t == ty -> Just v; _ -> Nothing) (SEnum ty) (fromEnum initial)
  let decode i
        | i >= fromEnum (minBound :: a) && i <= fromEnum (maxBound :: a) = toEnum i
        | otherwise = initial
  pure (zoomVar decode (\a _ -> fromEnum a) v)

-- | Look up or initialize the 'Var' at the next positional slot.
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

-- | Read the current value and record the read for this pass: a later mutation
-- in the same pass triggers a rerun so this read observes the updated value.
-- Reading a 'Var' after the widgets that mutate it avoids a rerun.
readVar :: Var a -> NanoUI a
readVar (VarRef cell l) = readCell (lensSig l) cell l
readVar (VarConst a) = pure a

-- | 'readVar' of a cell through a path, recording the read as one of the
-- part @sig@ of the cell. A part already read whole in this pass records
-- nothing new.
readCell :: Sig -> Cell s -> Lens s a -> NanoUI a
readCell !sig cell@(Cell ref _) l = withContext $ \ctx -> do
  unless (isInert cell) $ do
    p <- frameSlot ctx passSlot
    when (p /= 0) $ do
      again <- seenWhole cell p sig
      unless again $ do
        addToSet cell obsSet p sig
        addToSet cell seenSet p sig
  peekLens l =<< readIORef ref

-- | Read the current value without recording a dependency. Use for
-- frame-to-frame state comparisons where a second pass in the same frame would
-- otherwise see the already-updated value. Do not use for rendering; use
-- 'readVar' instead.
peekVar :: Var a -> NanoUI a
peekVar v = liftIO (readVarIO v)

-- | Mutate the 'Var' in place to a new value. No-op if equal to the current value.
writeVar :: Eq a => Var a -> a -> NanoUI ()
writeVar v a = modifyVar v (const a)

-- | Mutate the 'Var' in place with a function. No-op if the result equals the
-- current value.
modifyVar :: Eq a => Var a -> (a -> a) -> NanoUI ()
modifyVar (VarConst _) _ = pure ()
modifyVar (VarRef cell l) f = withContext $ \ctx -> do
  touched <- modifyRoot cell l f eqByPtr
  mapM_ (noteWrite ctx cell) touched

-- | Read from any thread without recording a dependency.
readVarIO :: Var a -> IO a
readVarIO (VarConst a) = pure a
readVarIO (VarRef (Cell ref _) l) = peekLens l =<< readIORef ref

-- | Mutate a 'Var' from any thread or setup code. Does not wake the event
-- loop; call the action from 'NanoUI.askWake' after background mutations.
writeVarIO :: Eq a => Var a -> a -> IO ()
writeVarIO v a = modifyVarIO v (const a)

-- | 'modifyVar' from any thread.
modifyVarIO :: Eq a => Var a -> (a -> a) -> IO ()
modifyVarIO (VarConst _) _ = pure ()
modifyVarIO (VarRef cell l) f = mapM_ (const countChange) =<< modifyRoot cell l f eqByPtr

-- | Run @body@ with a temporary 'Var' initialized to @a@ and return the final
-- value. Used to adapt a widget that takes a 'Var' to external state;
-- mutations to this temporary 'Var' do not trigger reruns or damage.
withValue :: a -> (Var a -> NanoUI r) -> NanoUI (r, a)
withValue a body = do
  cell@(Cell ref _) <- liftIO (newInertCell a)
  r <- body (VarRef cell Here)
  a' <- liftIO (readIORef ref)
  pure (r, a')

-- | Atomically modify the part of a cell focused by a lens. Returns the
-- modified part signature ('overLensSig') if the value changed, or 'Nothing'
-- for unchanged or inert cells, and for an element whose key has left its
-- list, which the write leaves alone.
{-# INLINE modifyRoot #-}
modifyRoot :: Cell s -> Lens s a -> (a -> a) -> (a -> a -> Bool) -> IO (Maybe Sig)
modifyRoot cell@(Cell ref _) l f eq
  | isInert cell = do
      (root', touched) <- step <$> readIORef ref
      Nothing <$ mapM_ (\_ -> writeIORef ref root') touched
  | otherwise = atomicModifyIORef' ref step
  where
    step root = case previewLens l root of
      Just old
        | new <- f old
        , not (eq new old) ->
            case overLensSig l (const new) root of (!root', sig) -> (root', Just sig)
      _ -> (root, Nothing)

-- | A write outside a pass: count it, and ask for a frame to show it.
noteOutsidePass :: Context -> IO ()
noteOutsidePass ctx = countChange >> markDirty ctx

-- | Record a changing write to the part @sig@ made by view code.
noteWrite :: Context -> Cell s -> Sig -> IO ()
noteWrite ctx cell sig = do
  p <- frameSlot ctx passSlot
  if p == 0
    then noteOutsidePass ctx
    else noteInPass ctx =<< setHits cell obsSet p sig

-- | Record a write in a pass, rerunning when @observed@: something observed
-- the part earlier in the pass.
noteInPass :: Context -> Bool -> IO ()
noteInPass ctx observed = do
  when observed (backwards ctx)
  countChange

-- | A part observed earlier in this pass was mutated: rerun in pass 1, or
-- schedule a follow-up frame in pass 2.
backwards :: Context -> IO ()
backwards ctx = do
  passNo <- frameSlot ctx passNoSlot
  if passNo <= 1
    then setFlag ctx flagRerun
    else markDirty ctx

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

-- | Bind a value-in, value-out widget to a 'Var': pass the current value and
-- write back the returned value. Edits to a 'constVar' are ignored. Direct
-- mutations to the same part during the widget's execution take precedence
-- over the widget's returned value, while mutations to sibling fields are
-- preserved.
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
bindWith _ _ result _ (VarConst a) widget = result <$> widget a
bindWith eq mode result value (VarRef cell@(Cell ref _) l) widget = do
  ctx <- askContext
  let !sig = lensSig l
  (root0, a0, before) <- liftIO $ do
    root0 <- readIORef ref
    a0 <- peekLens l root0
    before <- observeBinding ctx cell sig
    pure (root0, a0, before)
  x <- widget a0
  let a1 = value x
  liftIO $ do
    root1 <- readIORef ref
    -- While the root is the one the widget was given, the part it shows is
    -- still a0. A write elsewhere in the cell (a sibling field, from a tab's
    -- body say) leaves that part alone, so only a write to the part itself
    -- wins over the widget's edit.
    untouched <- if ptrEq root1 root0 then pure True else eq a0 <$> peekLens l root1
    when (untouched && not (eq a1 a0)) $
      mapM_ (noteBound ctx cell mode sig before) =<< modifyRoot cell l (const a1) eq
  pure (result x)

-- | Stamp the part @sig@ of a bound cell observed, returning whether
-- something observed it earlier in this pass.
observeBinding :: Context -> Cell s -> Sig -> IO Bool
observeBinding ctx cell !sig
  | isInert cell = pure False
  | otherwise = do
      p <- frameSlot ctx passSlot
      if p == 0
        then pure False
        else do
          hit <- setHits cell obsSet p sig
          addToSet cell obsSet p sig
          pure hit

-- | Record a bound widget's changing write to the part @wsig@. Its own node
-- shows the value, so only an earlier observer needs the view rerun. A
-- write that rekeyed a list element touched more than the widget observed
-- ('overLensSig'), so it is checked again.
noteBound :: Context -> Cell s -> Binding -> Sig -> Bool -> Sig -> IO ()
noteBound ctx cell mode sig before wsig = do
  p <- frameSlot ctx passSlot
  if p == 0
    then noteOutsidePass ctx
    else do
      observed <-
        if wsig == sig
          then do
            -- A tracked read inside the widget (a tab's body reading the
            -- tab Var) saw the value before this write, so it counts like
            -- one before the widget. The binding's own observation is in
            -- the obs set, so only the seen set can tell.
            (before ||) <$> setHits cell seenSet p sig
          else setHits cell obsSet p wsig
      noteInPass ctx (observed || mode == Conservative)

-- | Request a same-frame pass rerun, or a follow-up frame if already in the
-- second pass.
requestRerun :: Context -> IO ()
requestRerun ctx = do
  p <- frameSlot ctx passSlot
  if p == 0 then markDirty ctx else backwards ctx

-- | Start view pass @n@ of a frame. The first pass drops any requests left
-- by a frame an exception cut short. No handler is installed per frame,
-- since one costs an allocation every frame.
beginVarPass :: Context -> Int -> IO ()
beginVarPass ctx n = do
  t <- nextTick
  let arr = ctxVarFrame ctx
  writePrimArray arr passSlot t
  writePrimArray arr passNoSlot (fromIntegral n)
  when (n == 1) (writePrimArray arr flagSlot 0)

-- | Consume and return whether a pass rerun was requested.
takeVarRerun :: Context -> IO Bool
takeVarRerun ctx = do
  fs <- frameSlot ctx flagSlot
  writePrimArray (ctxVarFrame ctx) flagSlot (fs .&. complement flagRerun)
  pure (fs .&. flagRerun /= 0)

-- | Finish the frame's passes: record how many ran, and leave the pass, so
-- writes until the next frame count as outside one.
endVarFrame :: Context -> IO ()
endVarFrame ctx = do
  let arr = ctxVarFrame ctx
  writePrimArray arr lastPassesSlot =<< frameSlot ctx passNoSlot
  writePrimArray arr passSlot 0
  writePrimArray arr flagSlot 0

-- | How many view passes the last frame ran.
lastFramePasses :: Context -> IO Int
lastFramePasses ctx = fromIntegral <$> frameSlot ctx lastPassesSlot
