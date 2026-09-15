{-# LANGUAGE OverloadedStrings #-}

-- | Dropdown select.
module NanoUI.Widgets.Select
  ( select
  , selectWith
  , selectLabeled
  , boundedSelect
  , enumSelect
  )
where

import Control.Monad (forM_, when)
import Data.IORef (writeIORef)
import Data.IntMap.Strict qualified as IM
import Data.Text (Text)
import Data.Text qualified as T
import Effectful (Eff, type (:>))
import NanoUI.Context (Context (..), getStore, intKey, markDirty, registerFocusable, setStore, writeStoreInt)
import NanoUI.Frame.Select (selectDropPickIndex, selectDropRect, selectItemH)
import NanoUI.Input (inputMousePos, inputMousePressed, inputMouseReleased)
import NanoUI.Layout.Arena (NodeType (..))
import NanoUI.Monad (Ui, askContext, askInput, nextId, uiIO)
import NanoUI.Store (WidgetStore (..), isSelectOpen, setSelectOpen, slotKey, slotSelectSeen)
import NanoUI.Style (Layout, defaultLayout)
import NanoUI.Types (Rect (..), clamp, rectContains, rectHit, rectNonEmpty, v2Y)
import NanoUI.Widgets.Combinators (withBoundedIndex)
import NanoUI.Widgets.Node (Response, addWidgetWithOptions, respRect, setChanged)

select :: (Foldable f, Ui :> es) => f Text -> Int -> Eff es (Response, Int)
select = selectWith id

-- | Dropdown select with an inline caption: the closed control renders
-- @caption: option@, the way selects looked before labels were decoupled. The
-- plain 'select' stays caption-less; this is the opt-in for the old look.
selectLabeled :: (Foldable f, Ui :> es) => Text -> f Text -> Int -> Eff es (Response, Int)
selectLabeled caption = selectEx id caption

selectWith ::
  (Foldable f, Ui :> es) =>
  (Layout -> Layout) ->
  f Text ->
  Int ->
  Eff es (Response, Int)
selectWith modLayout = selectEx modLayout ""

selectEx ::
  (Foldable f, Ui :> es) =>
  (Layout -> Layout) ->
  Text ->
  f Text ->
  Int ->
  Eff es (Response, Int)
selectEx modLayout caption options initial = do
  wid <- nextId
  ctx <- askContext
  uiIO $ registerFocusable ctx wid
  let
    opts = case foldr (:) [] options of
      [] -> [""]
      xs -> xs
    n = length opts
    key = intKey wid
    seenKey = slotKey slotSelectSeen key
  store0 <- uiIO (getStore ctx)
  let
    clamped = clamp 0 (n - 1) (IM.findWithDefault initial key (storeInt store0))
    open = isSelectOpen store0 key
  when (not (IM.member key (storeInt store0))) $
    uiIO $ writeStoreInt ctx wid key clamped
  resp <- addWidgetWithOptions wid NodeSelect caption opts 0 (modLayout defaultLayout)
  inp <- askInput
  let
    rect@(Rect rx ry rw rh) = respRect resp
    mouse = inputMousePos inp
    dropRect = selectDropRect (ctxFontMetrics ctx) rx ry rw rh n
  when (rectHit rect mouse && inputMousePressed inp) $
    uiIO $ do
      st <- getStore ctx
      setStore ctx (setSelectOpen st key (not open))
      writeIORef (ctxFocusId ctx) wid
      markDirty ctx
  when (open && rectNonEmpty rect && rectContains dropRect mouse && inputMouseReleased inp) $
    forM_ (selectDropPickIndex dropRect (selectItemH rh) n (v2Y mouse)) $ \picked ->
      uiIO $ do
        st <- getStore ctx
        setStore ctx (setSelectOpen (st {storeInt = IM.insert key picked (storeInt st)}) key False)
        writeIORef (ctxFocusId ctx) wid
        markDirty ctx
  store1 <- uiIO (getStore ctx)
  let
    finalIdx = IM.findWithDefault clamped key (storeInt store1)
    seen = IM.lookup seenKey (storeInt store1)
  when (seen /= Just finalIdx) $
    uiIO $ writeStoreInt ctx wid seenKey finalIdx
  pure (setChanged (maybe False (/= finalIdx) seen) resp, finalIdx)

boundedSelect :: (Bounded a, Enum a, Ui :> es) => a -> (a -> Text) -> Eff es (Response, a)
boundedSelect initial encode = withBoundedIndex encode initial select

enumSelect :: (Bounded a, Enum a, Show a, Ui :> es) => a -> Eff es (Response, a)
enumSelect initial = boundedSelect initial (T.pack . show)
