-- | Component-owned typed cells and identity-bound primitive hooks. All writes
-- compare against the latest value and invalidate the view only on change.
module NanoUI.Internal.Hooks
  ( StateCell
  , newState
  , useState
  , modifyState
  , useFlag
  , useInt
  , useFloat
  , useEnum
  , useText
  , useToggle
  )
where

import Control.Monad (when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import NanoUI.Internal.Context (Context, damageFull, getStore, intKey, modifyStore, setStore)
import NanoUI.Internal.Monad (NanoUI, askContext, freshWidget, liftIO)
import NanoUI.Internal.Store (WidgetStore, boolInt, bumpMirror, fieldFloat, fieldInt, fieldText, insertSlot, intBool, lookupSlot)

-- | A typed cell owned by one component instance in one UI session. Its
-- identity and lifetime follow the handle, not the widget id or call order.
-- Construct twice for independent state; retaining a handle retains its value
-- even while its view is hidden. Access cells only on the UI thread.
newtype StateCell a = StateCell (IORef a)

-- | Allocate during component setup, before entering the per-frame view.
-- The initial value is fixed here; allocating inside a view resets it on every
-- pass. Ordinary Haskell closures can keep a component's cells private.
newState :: a -> IO (StateCell a)
newState initial = StateCell <$> newIORef initial

-- | Read a snapshot and a setter for this session. This consumes no widget id.
-- Setters read the latest value when invoked, including setters retained from
-- an earlier pass. Use 'modifyState' for transitions based on current state.
useState :: Eq a => StateCell a -> NanoUI (a, a -> NanoUI ())
useState cell@(StateCell ref) = do
  ctx <- askContext
  value <- liftIO (readIORef ref)
  pure (value, \next -> liftIO (modifyStateIn ctx cell (const next)))

-- | Apply a pure transition to the latest value. Successive modifications
-- compose even within one frame; equal writes neither wake nor invalidate.
modifyState :: Eq a => StateCell a -> (a -> a) -> NanoUI ()
modifyState cell transition = do
  ctx <- askContext
  liftIO (modifyStateIn ctx cell transition)

modifyStateIn :: Eq a => Context -> StateCell a -> (a -> a) -> IO ()
modifyStateIn ctx (StateCell ref) transition = do
  old <- readIORef ref
  let !next = transition old
  when (old /= next) $ do
    writeIORef ref next
    -- The frame's immutable generation snapshot detects the write even though
    -- the cell itself is mutable. Its consumers can be anywhere in the view,
    -- so queue full damage rather than attributing it to a single widget.
    -- The queued damage covers this build; an opaque dirty mark would only
    -- force the following frame to repaint.
    modifyStore ctx bumpMirror
    damageFull ctx

useStored ::
  Eq a =>
  (Int -> WidgetStore -> Maybe a)
  -> (Int -> a -> WidgetStore -> WidgetStore)
  -> a
  -> NanoUI (a, a -> NanoUI ())
useStored lookupValue update initial = do
  (wid, ctx) <- freshWidget
  let
    key = intKey wid
    valueIn = fromMaybe initial . lookupValue key
    setValue value = liftIO $ do
      -- A setter can run more than once in a frame. Compare with the latest
      -- store, not the value captured when the hook was evaluated.
      store <- getStore ctx
      when (valueIn store /= value) $
        setStore ctx (bumpMirror (update key value store))
  value <- valueIn <$> liftIO (getStore ctx)
  pure (value, setValue)

-- | Boolean state stored as an integer flag, with an explicit setter.
useFlag :: Bool -> NanoUI (Bool, Bool -> NanoUI ())
useFlag initial = do
  (value, setValue) <- useInt (boolInt initial)
  pure (intBool value, setValue . boolInt)

-- | Integer state and its setter, without runtime type lookup.
useInt :: Int -> NanoUI (Int, Int -> NanoUI ())
useInt = useStored (lookupSlot fieldInt) (insertSlot fieldInt)

-- | Floating-point state and its setter, without runtime type lookup.
useFloat :: Float -> NanoUI (Float, Float -> NanoUI ())
useFloat = useStored (lookupSlot fieldFloat) (insertSlot fieldFloat)

-- | Enum state stored through 'fromEnum'. Keep the enum type stable at this id.
useEnum :: Enum a => a -> NanoUI (a, a -> NanoUI ())
useEnum initial = do
  (index, setIndex) <- useInt (fromEnum initial)
  pure (toEnum index, setIndex . fromEnum)

-- | Text state and its setter, without runtime type lookup.
useText :: Text -> NanoUI (Text, Text -> NanoUI ())
useText = useStored (lookupSlot fieldText) (insertSlot fieldText)

-- | Boolean state and an action that writes the opposite of this frame's value.
-- Calling that action twice in one frame writes the same value twice.
useToggle :: Bool -> NanoUI (Bool, NanoUI ())
useToggle initial = do
  (value, setValue) <- useFlag initial
  pure (value, setValue (not value))
