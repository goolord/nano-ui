{-# OPTIONS_HADDOCK not-home #-}

-- | The view's position in the id tree, kept unboxed: the current path,
-- the next widget and Var positions within it, and a growable unboxed stack
-- of enclosing scopes so entering and leaving containers or keyed scopes
-- allocates nothing.
module NanoUI.Internal.Id.Cursor
  ( IdCursor
  , newIdCursor
  , readIdCursor
  , writeIdCursor
  , resetIdCursor
  , pushIdScope
  , pushKeyedIdScope
  , popIdScope
  , readIdDepth
  , writeIdDepth
  , peekWidgetId
  , takeWidgetId
  , skipSiblingIds
  , takeVarKey
  )
where

import Control.Monad (when)
import Control.Monad.Primitive (RealWorld)
import Data.Bits (xor)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Primitive.PrimArray
  ( MutablePrimArray
  , getSizeofMutablePrimArray
  , newPrimArray
  , readPrimArray
  , resizeMutablePrimArray
  , writePrimArray
  )
import Data.Primitive.PrimVar (PrimVar, newPrimVar, readPrimVar, writePrimVar)
import Data.Word (Word64)
import NanoUI.Internal.Id (IdContext (..), WidgetId, idContextWidgetId, initialIdContext, keyedTag, mix64)

-- | Active cursor slots (0 path, 1 next widget position, 2 next Var position)
-- plus an unboxed growable scope stack (3 words per pushed frame).
data IdCursor
  = IdCursor
      {-# UNPACK #-} !(MutablePrimArray RealWorld Word64)
      !(IORef (MutablePrimArray RealWorld Word64))
      {-# UNPACK #-} !(PrimVar RealWorld Int)

-- | A cursor positioned at the given 'IdContext'.
newIdCursor :: IdContext -> IO IdCursor
newIdCursor ic = do
  arr <- newPrimArray 3
  stk <- newPrimArray (32 * 3) >>= newIORef
  dep <- newPrimVar 0
  let cur = IdCursor arr stk dep
  writeIdCursor cur ic
  pure cur

-- | Reset the cursor to 'initialIdContext' and empty its scope stack.
{-# INLINE resetIdCursor #-}
resetIdCursor :: IdCursor -> IO ()
resetIdCursor cur@(IdCursor _ _ depVar) = do
  writeIdCursor cur initialIdContext
  writePrimVar depVar 0

-- | The cursor's current path and positions as an 'IdContext'.
{-# INLINE readIdCursor #-}
readIdCursor :: IdCursor -> IO IdContext
readIdCursor (IdCursor arr _ _) =
  IdContext <$> readPrimArray arr 0 <*> readPrimArray arr 1 <*> readPrimArray arr 2

-- | Move the cursor to the path and positions of an 'IdContext'.
{-# INLINE writeIdCursor #-}
writeIdCursor :: IdCursor -> IdContext -> IO ()
writeIdCursor (IdCursor arr _ _) (IdContext p s v) = do
  writePrimArray arr 0 p
  writePrimArray arr 1 s
  writePrimArray arr 2 v

-- | Current unboxed scope stack depth, for exception recovery ('catchView').
{-# INLINE readIdDepth #-}
readIdDepth :: IdCursor -> IO Int
readIdDepth (IdCursor _ _ depVar) = readPrimVar depVar

-- | Restore unboxed scope stack depth after exception recovery.
{-# INLINE writeIdDepth #-}
writeIdDepth :: IdCursor -> Int -> IO ()
writeIdDepth (IdCursor _ _ depVar) = writePrimVar depVar

{-# NOINLINE growIdStack #-}
growIdStack :: IORef (MutablePrimArray RealWorld Word64) -> Int -> IO (MutablePrimArray RealWorld Word64)
growIdStack ref needed = do
  arr <- readIORef ref
  cap <- getSizeofMutablePrimArray arr
  let !newCap = max needed (cap * 2)
  grown <- resizeMutablePrimArray arr newCap
  writeIORef ref grown
  pure grown

-- | Save the advanced parent context onto the unboxed stack and enter a fresh
-- positional child scope derived from the parent's sibling position and @tag@.
{-# INLINE pushIdScope #-}
pushIdScope :: IdCursor -> Word64 -> IO ()
pushIdScope (IdCursor arr stkRef depVar) tag = do
  p <- readPrimArray arr 0
  s <- readPrimArray arr 1
  v <- readPrimArray arr 2
  d <- readPrimVar depVar
  let !off = d * 3
  stk0 <- readIORef stkRef
  cap <- getSizeofMutablePrimArray stk0
  stk <- if off + 3 <= cap then pure stk0 else growIdStack stkRef (off + 3)
  writePrimArray stk off p
  writePrimArray stk (off + 1) (s + 1)
  writePrimArray stk (off + 2) v
  writePrimVar depVar (d + 1)
  writePrimArray arr 0 (mix64 (mix64 p s) tag)
  writePrimArray arr 1 0
  writePrimArray arr 2 0

-- | Save the parent context unchanged onto the unboxed stack and enter a fresh
-- keyed child scope derived from @keyHash@.
{-# INLINE pushKeyedIdScope #-}
pushKeyedIdScope :: IdCursor -> Word64 -> IO ()
pushKeyedIdScope (IdCursor arr stkRef depVar) keyHash = do
  p <- readPrimArray arr 0
  s <- readPrimArray arr 1
  v <- readPrimArray arr 2
  d <- readPrimVar depVar
  let !off = d * 3
  stk0 <- readIORef stkRef
  cap <- getSizeofMutablePrimArray stk0
  stk <- if off + 3 <= cap then pure stk0 else growIdStack stkRef (off + 3)
  writePrimArray stk off p
  writePrimArray stk (off + 1) s
  writePrimArray stk (off + 2) v
  writePrimVar depVar (d + 1)
  writePrimArray arr 0 (mix64 (mix64 p keyHash) keyedTag)
  writePrimArray arr 1 0
  writePrimArray arr 2 0

-- | Restore the parent context pushed by 'pushIdScope' or 'pushKeyedIdScope'.
{-# INLINE popIdScope #-}
popIdScope :: IdCursor -> IO ()
popIdScope (IdCursor arr stkRef depVar) = do
  d <- readPrimVar depVar
  when (d > 0) $ do
    let !d' = d - 1
        !off = d' * 3
    writePrimVar depVar d'
    stk <- readIORef stkRef
    p <- readPrimArray stk off
    s <- readPrimArray stk (off + 1)
    v <- readPrimArray stk (off + 2)
    writePrimArray arr 0 p
    writePrimArray arr 1 s
    writePrimArray arr 2 v

-- | The id the next widget would take, without taking it.
{-# INLINE peekWidgetId #-}
peekWidgetId :: IdCursor -> IO WidgetId
peekWidgetId (IdCursor arr _ _) = do
  p <- readPrimArray arr 0
  s <- readPrimArray arr 1
  pure $! idContextWidgetId (IdContext p s 0)

-- | The id of the next widget; advances the widget position by one.
{-# INLINE takeWidgetId #-}
takeWidgetId :: IdCursor -> IO WidgetId
takeWidgetId (IdCursor arr _ _) = do
  p <- readPrimArray arr 0
  s <- readPrimArray arr 1
  writePrimArray arr 1 (s + 1)
  pure $! idContextWidgetId (IdContext p s 0)

-- | Advance the widget position by @n@ without taking ids, for siblings a
-- reused subtree already accounts for.
{-# INLINE skipSiblingIds #-}
skipSiblingIds :: IdCursor -> Int -> IO ()
skipSiblingIds (IdCursor arr _ _) n = do
  s <- readPrimArray arr 1
  writePrimArray arr 1 (s + fromIntegral n)

-- | Allocate the next positional 'Var' key. 'Var' keys index a separate table
-- from widget ids and store keys, and use a distinct hash salt.
{-# INLINE takeVarKey #-}
takeVarKey :: IdCursor -> IO Int
takeVarKey (IdCursor arr _ _) = do
  p <- readPrimArray arr 0
  n <- readPrimArray arr 2
  writePrimArray arr 2 (n + 1)
  pure $! fromIntegral (mix64 (p `xor` 0x7661725F6B657973) n)
