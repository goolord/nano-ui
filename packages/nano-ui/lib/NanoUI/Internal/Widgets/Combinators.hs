-- | Button and selection helpers shared by the widget modules.
module NanoUI.Internal.Widgets.Combinators
  ( buttonStyled
  , buttonStyledEx
  , selectableItem
  , withBoundedIndex
  , finishToggle
  , finishInput
  )
where

import Control.Monad (when)
import Data.Text (Text)
import Effectful (Eff, type (:>))
import NanoUI.Internal.Context
  ( Context
  , intKey
  , isDisabled
  , recordSlot
  , registerFocusable
  , writeSlot
  , writeStoreBool
  )
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Store (fieldInt, Field, boolInt)
import NanoUI.Internal.Layout.Arena (NodeType (..))
import NanoUI.Internal.Monad (Ui, askContext, nextId, uiIO)
import NanoUI.Internal.Style (Layout (..))
import NanoUI.Internal.Widgets.Behavior (keyActivated)
import NanoUI.Internal.Widgets.Node
  ( Response (..)
  , addWidgetStyled
  , inertResponse
  , respClicked
  , setChanged
  , setClicked
  )

-- | Button with styleIdx for active, sort, badge, or close chrome. Focusable
-- and activatable with Enter or Space while focused.
buttonStyled :: (Ui :> es) => Text -> Float -> Layout -> Int -> Eff es Response
buttonStyled = buttonStyledEx True

-- | Shared activation path for ordinary buttons, menu items, and header chrome.
-- Disabled controls keep their identity and geometry but cannot take focus or
-- activate, including through a click queued before they became disabled.
{-# INLINE buttonStyledEx #-}
buttonStyledEx :: (Ui :> es) => Bool -> Text -> Float -> Layout -> Int -> Eff es Response
buttonStyledEx enabled txt value layout styleIdx = do
  wid <- nextId
  ctx <- askContext
  disabled <- uiIO (isDisabled ctx wid)
  let active = enabled && not disabled
  when active $ uiIO (registerFocusable ctx wid)
  resp <- addWidgetStyled wid NodeButton txt value layout styleIdx
  if active
    then do
      keyClick <- keyActivated wid
      pure (if keyClick then setClicked True resp else resp)
    else pure (inertResponse resp)

selectableItem :: (Ui :> es) => NodeType -> Text -> Bool -> Layout -> Int -> Eff es Response
selectableItem nt txt selected layout styleIdx = do
  wid <- nextId
  addWidgetStyled
    wid
    nt
    txt
    (if selected then 1 else 0)
    layout
    styleIdx

-- | Finish a boolean control after its node has registered focus eligibility.
-- Keyboard activation changes the value without inventing a pointer click.
{-# INLINE finishToggle #-}
finishToggle ::
  Ui :> es => Context -> WidgetId -> Bool -> Response -> Eff es (Response, Bool)
finishToggle ctx wid current resp = do
  keyClick <- keyActivated wid
  let
    clicked = respClicked resp || keyClick
    value = current /= clicked
  uiIO $ do
    writeStoreBool ctx wid value
    recordSlot fieldInt ctx (intKey wid) (boolInt value)
  pure (setChanged clicked resp, value)

-- | Publish an input's result and remember it for controlled adoption. The
-- caller chooses the comparison value: live state or the supplied model value.
{-# INLINE finishInput #-}
finishInput ::
  (Eq a, Ui :> es) =>
  Field a
  -> Context
  -> WidgetId
  -> Int
  -> a
  -> Response
  -> a
  -> Eff es (Response, a)
finishInput field ctx wid key original resp value = do
  uiIO $ do
    writeSlot field ctx wid key value
    recordSlot field ctx key value
  pure (setChanged (value /= original) resp, value)

-- | Run an index-based picker over every value of a bounded enum. Indices
-- are offset by @fromEnum minBound@, so enums that do not start at 0 map
-- correctly. Meant for small enums: every value becomes an option.
withBoundedIndex ::
  forall a r f.
  (Bounded a, Enum a, Functor f) =>
  (a -> Text) -> a -> ([Text] -> Int -> f (r, Int)) -> f (r, a)
withBoundedIndex encode initial pick =
  fmap (toEnum . (+ lower))
    <$> pick (map encode [minBound .. maxBound]) (fromEnum initial - lower)
 where
  lower = fromEnum (minBound :: a)
