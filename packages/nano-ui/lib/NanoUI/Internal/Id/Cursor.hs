-- | The view's position in the id tree, kept unboxed: the current path and
-- the next widget and Var positions within it. Taking a widget id or a Var
-- key allocates nothing; entering a scope boxes the parent once.
module NanoUI.Internal.Id.Cursor
  ( IdCursor
  , newIdCursor
  , readIdCursor
  , writeIdCursor
  , peekWidgetId
  , takeWidgetId
  , skipSiblingIds
  , takeVarKey
  )
where

import Control.Monad.Primitive (RealWorld)
import Data.Bits (xor)
import Data.Primitive.PrimArray (MutablePrimArray, newPrimArray, readPrimArray, writePrimArray)
import Data.Word (Word64)
import NanoUI.Internal.Id (IdContext (..), WidgetId, idContextWidgetId, mix64)

-- | Slots: 0 path, 1 next widget position, 2 next Var position.
newtype IdCursor = IdCursor (MutablePrimArray RealWorld Word64)

newIdCursor :: IdContext -> IO IdCursor
newIdCursor ic = do
  arr <- newPrimArray 3
  let cur = IdCursor arr
  writeIdCursor cur ic
  pure cur

{-# INLINE readIdCursor #-}
readIdCursor :: IdCursor -> IO IdContext
readIdCursor (IdCursor arr) =
  IdContext <$> readPrimArray arr 0 <*> readPrimArray arr 1 <*> readPrimArray arr 2

{-# INLINE writeIdCursor #-}
writeIdCursor :: IdCursor -> IdContext -> IO ()
writeIdCursor (IdCursor arr) (IdContext p s v) = do
  writePrimArray arr 0 p
  writePrimArray arr 1 s
  writePrimArray arr 2 v

-- | The id the next widget would take, without taking it.
{-# INLINE peekWidgetId #-}
peekWidgetId :: IdCursor -> IO WidgetId
peekWidgetId (IdCursor arr) = do
  p <- readPrimArray arr 0
  s <- readPrimArray arr 1
  pure $! idContextWidgetId (IdContext p s 0)

{-# INLINE takeWidgetId #-}
takeWidgetId :: IdCursor -> IO WidgetId
takeWidgetId (IdCursor arr) = do
  p <- readPrimArray arr 0
  s <- readPrimArray arr 1
  writePrimArray arr 1 (s + 1)
  pure $! idContextWidgetId (IdContext p s 0)

{-# INLINE skipSiblingIds #-}
skipSiblingIds :: IdCursor -> Int -> IO ()
skipSiblingIds (IdCursor arr) n = do
  s <- readPrimArray arr 1
  writePrimArray arr 1 (s + fromIntegral n)

-- | Allocate the next positional 'Var' key. 'Var' keys index a separate table
-- from widget ids and store keys, and use a distinct hash salt.
{-# INLINE takeVarKey #-}
takeVarKey :: IdCursor -> IO Int
takeVarKey (IdCursor arr) = do
  p <- readPrimArray arr 0
  n <- readPrimArray arr 2
  writePrimArray arr 2 (n + 1)
  pure $! fromIntegral (mix64 (p `xor` 0x7661725F6B657973) n)
