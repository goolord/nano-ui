-- | Dropdown select.
module NanoUI.Internal.Widgets.Select
  ( select
  , select'
  , selectWith
  , selectWith'
  , boundedSelect
  , boundedSelect'
  , enumSelect
  , enumSelect'
  )
where

import Control.Monad (forM_, void, when)
import Data.Foldable (toList)
import Data.IORef (writeIORef)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import NanoUI.Internal.Context
import NanoUI.Internal.Equality (sameEnum)
import NanoUI.Internal.Font (menuItemRowH)
import NanoUI.Internal.Frame.Select (selectDropPickIndex, selectDropRect)
import NanoUI.Internal.Input (MouseButton (..), Pressable (..), inputMousePos, inputWindowSize)
import NanoUI.Internal.Layout.Arena (NodeType (..))
import NanoUI.Internal.Monad (NanoUI, (<&&>), askInput, freshWidget, liftIO)
import NanoUI.Internal.Store (fieldInt, insertSlot)
import NanoUI.Internal.Style (Layout, defaultLayout)
import NanoUI.Internal.Types (Rect (..), clamp, rectContains, rectNonEmpty, v2Y)
import NanoUI.Internal.Var (Binding (..), bindVar, bindVarBy)
import NanoUI.Internal.Var.Types (Var)
import NanoUI.Internal.Widgets.Combinators (withBoundedIndex)
import NanoUI.Internal.Frame.Hit (findNodeByWidgetId)
import NanoUI.Internal.Widgets.Node (Response, addWidgetWithOptions, pointerOnWidget, respRect, setChanged)


-- | Dropdown over @options@ in fold order, bound to mutable state ('Var')
-- holding the zero-based selected index.
{-# INLINE select #-}
select :: Foldable f => f Text -> Var Int -> NanoUI ()
select options v = void (selectWith' id options v)

-- | 'select' returning its 'Response'.
{-# INLINE select' #-}
select' :: Foldable f => f Text -> Var Int -> NanoUI Response
select' = selectWith' id

-- | 'select' with a layout modifier.
{-# INLINE selectWith #-}
selectWith :: Foldable f => (Layout -> Layout) -> f Text -> Var Int -> NanoUI ()
selectWith f options v = void (selectWith' f options v)

-- | 'selectWith' returning its 'Response'.
{-# INLINE selectWith' #-}
selectWith' :: Foldable f => (Layout -> Layout) -> f Text -> Var Int -> NanoUI Response
selectWith' f options v = bindVar RenderFinal v (selectCore f options)

-- | 'selectWith' on a plain index: the response and the index after this
-- frame's pick.
selectCore ::
  Foldable f =>
  (Layout -> Layout) ->
  f Text ->
  Int ->
  NanoUI (Response, Int)
selectCore f options index = do
  (wid, ctx) <- freshWidget
  liftIO $ registerFocusable ctx wid
  let
    opts = case toList options of
      [] -> [""]
      xs -> xs
    n = length opts
    key = intKey wid
  -- The store holds the caller's index until a pick replaces it, even one
  -- the options do not reach: a pick of the option it shows is a change.
  stored <- liftIO $ adoptSlot fieldInt ctx wid index
  store0 <- liftIO (getStore ctx)
  let
    current = clamp 0 (n - 1) stored
    open = isSelectOpen store0 key
  resp <- addWidgetWithOptions wid NodeSelect "" opts 0 (f defaultLayout)
  inp <- askInput
  let
    rect@(Rect rx ry rw rh) = respRect resp
    mouse = inputMousePos inp
    dropRect = selectDropRect (inputWindowSize inp) rx ry rw rh n
    picked
      | open && rectNonEmpty rect && rectContains dropRect mouse && releasedIn MouseLeft inp =
          selectDropPickIndex dropRect menuItemRowH n (v2Y mouse)
      | otherwise = Nothing
    finalIdx = maybe current (clamp 0 (n - 1)) picked
    -- An index the options do not reach yet stays the caller's until a pick.
    result = if isNothing picked && stored == index then index else finalIdx
  -- Opening, closing or picking changes the store, which wakes the loop.
  liftIO $ do
    -- Ignore presses where layers or a pinned node cover the widget, or
    -- where it is scrolled out of its viewport.
    pressed <-
      pure (pressedIn MouseLeft inp)
        <&&> (findNodeByWidgetId ctx wid >>= \mIdx -> pointerOnWidget ctx mIdx wid rect mouse)
    when pressed $ do
      modifyStore ctx (\st -> setSelectOpen st key (not open))
      writeIORef (ctxFocusId ctx) wid
    forM_ picked $ \i -> do
      modifyStore ctx (\st -> setSelectOpen (insertSlot fieldInt key i st) key False)
      writeIORef (ctxFocusId ctx) wid
    recordSlot fieldInt ctx key result
  -- Compare with the caller's index, not 'current': a dropdown or keyboard
  -- pick lands in the store between frames and must still report a change.
  pure (setChanged (result /= index) resp, result)

-- | Select over every value of a bounded enum, labelled by @encode@.
{-# INLINE boundedSelect #-}
boundedSelect :: (Bounded a, Enum a) => (a -> Text) -> Var a -> NanoUI ()
boundedSelect encode v = void (boundedSelect' encode v)

-- | 'boundedSelect' returning its 'Response'.
{-# INLINE boundedSelect' #-}
boundedSelect' :: (Bounded a, Enum a) => (a -> Text) -> Var a -> NanoUI Response
boundedSelect' encode v = bindVarBy sameEnum RenderFinal v (\value -> withBoundedIndex encode value (selectCore id))

-- | 'boundedSelect' labelled with 'show'.
{-# INLINE enumSelect #-}
enumSelect :: (Bounded a, Enum a, Show a) => Var a -> NanoUI ()
enumSelect = boundedSelect (T.pack . show)

-- | 'enumSelect' returning its 'Response'.
{-# INLINE enumSelect' #-}
enumSelect' :: (Bounded a, Enum a, Show a) => Var a -> NanoUI Response
enumSelect' = boundedSelect' (T.pack . show)
