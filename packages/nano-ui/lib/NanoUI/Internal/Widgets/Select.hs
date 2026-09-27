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

import Control.Monad (forM_, when)
import Data.Foldable (toList)
import Data.IORef (writeIORef)
import Data.Text (Text)
import Data.Text qualified as T
import NanoUI.Internal.Context
import NanoUI.Internal.Font (menuItemRowH)
import NanoUI.Internal.Frame.Select (selectDropPickIndex, selectDropRect)
import NanoUI.Internal.Input (MouseButton (..), Pressable (..), inputMousePos)
import NanoUI.Internal.Layout.Arena (NodeType (..))
import NanoUI.Internal.Monad (NanoUI, (<&&>), askInput, freshWidget, liftIO)
import NanoUI.Internal.Store (fieldInt, insertSlot)
import NanoUI.Internal.Style (Layout, defaultLayout)
import NanoUI.Internal.Types (Rect (..), clamp, rectContains, rectHit, rectNonEmpty, v2Y)
import NanoUI.Internal.Widgets.Combinators (withBoundedIndex)
import NanoUI.Internal.Widgets.Node (Response, addWidgetWithOptions, respRect, setChanged)

-- | Dropdown over @options@ in fold order. Pass the selected index; the result
-- is the index after this frame's pick.
{-# INLINE select #-}
select :: Foldable f => f Text -> Int -> NanoUI Int
select options index = snd <$> selectWith' id options index

{-# INLINE select' #-}
-- | 'select' returning @(response, selectedIndex)@. Indices are zero-based.
select' :: Foldable f => f Text -> Int -> NanoUI (Response, Int)
select' = selectWith' id

-- | 'select' with a layout modifier.
{-# INLINE selectWith #-}
selectWith :: Foldable f => (Layout -> Layout) -> f Text -> Int -> NanoUI Int
selectWith f options index = snd <$> selectWith' f options index

-- | 'selectWith' returning the response and selected index.
selectWith' ::
  Foldable f =>
  (Layout -> Layout) ->
  f Text ->
  Int ->
  NanoUI (Response, Int)
selectWith' f options index = do
  (wid, ctx) <- freshWidget
  liftIO $ registerFocusable ctx wid
  let
    opts = case toList options of
      [] -> [""]
      xs -> xs
    n = length opts
    key = intKey wid
    given = clamp 0 (n - 1) index
  stored <- liftIO $ adoptSlot fieldInt ctx wid given
  store0 <- liftIO (getStore ctx)
  let
    current = clamp 0 (n - 1) stored
    open = isSelectOpen store0 key
  resp <- addWidgetWithOptions wid NodeSelect "" opts 0 (f defaultLayout)
  inp <- askInput
  let
    rect@(Rect rx ry rw rh) = respRect resp
    mouse = inputMousePos inp
    dropRect = selectDropRect rx ry rw rh n
    picked
      | open && rectNonEmpty rect && rectContains dropRect mouse && releasedIn MouseLeft inp =
          selectDropPickIndex dropRect menuItemRowH n (v2Y mouse)
      | otherwise = Nothing
    finalIdx = maybe current (clamp 0 (n - 1)) picked
  -- Opening, closing or picking changes the store, which wakes the loop.
  liftIO $ do
    -- Ignore presses where layers or a pinned node cover the widget.
    pressed <- pure (rectHit rect mouse && pressedIn MouseLeft inp) <&&> (not <$> pointerCovered ctx wid)
    when pressed $ do
      modifyStore ctx (\st -> setSelectOpen st key (not open))
      writeIORef (ctxFocusId ctx) wid
    forM_ picked $ \i -> do
      modifyStore ctx (\st -> setSelectOpen (insertSlot fieldInt key i st) key False)
      writeIORef (ctxFocusId ctx) wid
    recordSlot fieldInt ctx key finalIdx
  -- Compare with the caller's index, not 'current': a dropdown or keyboard
  -- pick lands in the store between frames and must still report a change.
  pure (setChanged (finalIdx /= given) resp, finalIdx)

-- | Select over every value of a bounded enum, labelled by @encode@.
{-# INLINE boundedSelect #-}
boundedSelect :: (Bounded a, Enum a) => (a -> Text) -> a -> NanoUI a
boundedSelect encode value = snd <$> boundedSelect' encode value

-- | 'boundedSelect' returning the response and selected enum value.
boundedSelect' :: (Bounded a, Enum a) => (a -> Text) -> a -> NanoUI (Response, a)
boundedSelect' encode value = withBoundedIndex encode value select'

-- | 'boundedSelect' labelled with 'show'.
{-# INLINE enumSelect #-}
enumSelect :: (Bounded a, Enum a, Show a) => a -> NanoUI a
enumSelect = boundedSelect (T.pack . show)

-- | 'enumSelect' returning the response and selected enum value.
enumSelect' :: (Bounded a, Enum a, Show a) => a -> NanoUI (Response, a)
enumSelect' = boundedSelect' (T.pack . show)
