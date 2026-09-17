{-# LANGUAGE OverloadedStrings #-}

-- | Dropdown select.
module NanoUI.Widgets.Select
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
import Data.IORef (writeIORef)
import Data.IntMap.Strict qualified as IM
import Data.Text (Text)
import Data.Text qualified as T
import Effectful (Eff, type (:>))
import NanoUI.Context
  ( Context (..)
  , adoptStoreInt
  , getStore
  , intKey
  , recordStoreInt
  , registerFocusable
  , modifyStore
  )
import NanoUI.Font (menuItemRowH)
import NanoUI.Frame.Select (selectDropPickIndex, selectDropRect)
import NanoUI.Input (inputMousePos, inputMousePressed, inputMouseReleased)
import NanoUI.Layout.Arena (NodeType (..))
import NanoUI.Monad (Ui, askContext, askInput, nextId, uiIO)
import NanoUI.Store (WidgetStore (..), isSelectOpen, setSelectOpen)
import NanoUI.Style (Layout, defaultLayout)
import NanoUI.Types (Rect (..), clamp, rectContains, rectHit, rectNonEmpty, v2Y)
import NanoUI.Widgets.Combinators (withBoundedIndex)
import NanoUI.Widgets.Node (Response, addWidgetWithOptions, respRect, setChanged)

-- | Dropdown over @options@ in fold order. Pass the selected index; the result
-- is the index after this frame's pick.
{-# INLINE select #-}
select :: (Foldable f, Ui :> es) => f Text -> Int -> Eff es Int
select options index = snd <$> selectWith' id options index

{-# INLINE select' #-}
select' :: (Foldable f, Ui :> es) => f Text -> Int -> Eff es (Response, Int)
select' = selectWith' id

-- | 'select' with a layout modifier.
{-# INLINE selectWith #-}
selectWith :: (Foldable f, Ui :> es) => (Layout -> Layout) -> f Text -> Int -> Eff es Int
selectWith f options index = snd <$> selectWith' f options index

selectWith' ::
  (Foldable f, Ui :> es) =>
  (Layout -> Layout) ->
  f Text ->
  Int ->
  Eff es (Response, Int)
selectWith' f options index = do
  wid <- nextId
  ctx <- askContext
  uiIO $ registerFocusable ctx wid
  let
    opts = case foldr (:) [] options of
      [] -> [""]
      xs -> xs
    n = length opts
    key = intKey wid
  stored <- uiIO $ adoptStoreInt ctx wid key (clamp 0 (n - 1) index)
  store0 <- uiIO (getStore ctx)
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
      | open && rectNonEmpty rect && rectContains dropRect mouse && inputMouseReleased inp =
          selectDropPickIndex dropRect menuItemRowH n (v2Y mouse)
      | otherwise = Nothing
    finalIdx = maybe current (clamp 0 (n - 1)) picked
  -- Opening, closing or picking changes the store, which wakes the loop.
  uiIO $ do
    when (rectHit rect mouse && inputMousePressed inp) $ do
      modifyStore ctx (\st -> setSelectOpen st key (not open))
      writeIORef (ctxFocusId ctx) wid
    forM_ picked $ \i -> do
      modifyStore ctx (\st -> setSelectOpen (st {storeInt = IM.insert key i (storeInt st)}) key False)
      writeIORef (ctxFocusId ctx) wid
    recordStoreInt ctx key finalIdx
  -- Compare with the caller's index, not 'current': a dropdown or keyboard
  -- pick lands in the store between frames and must still report a change.
  pure (setChanged (finalIdx /= clamp 0 (n - 1) index) resp, finalIdx)

-- | Select over every value of a bounded enum, labelled by @encode@.
{-# INLINE boundedSelect #-}
boundedSelect :: (Bounded a, Enum a, Ui :> es) => (a -> Text) -> a -> Eff es a
boundedSelect encode value = snd <$> boundedSelect' encode value

boundedSelect' :: (Bounded a, Enum a, Ui :> es) => (a -> Text) -> a -> Eff es (Response, a)
boundedSelect' encode value = withBoundedIndex encode value select'

-- | 'boundedSelect' labelled with 'show'.
{-# INLINE enumSelect #-}
enumSelect :: (Bounded a, Enum a, Show a, Ui :> es) => a -> Eff es a
enumSelect = boundedSelect (T.pack . show)

enumSelect' :: (Bounded a, Enum a, Show a, Ui :> es) => a -> Eff es (Response, a)
enumSelect' = boundedSelect' (T.pack . show)
