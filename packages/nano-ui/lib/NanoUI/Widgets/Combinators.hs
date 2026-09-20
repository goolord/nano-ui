{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Button and selection helpers shared by the widget modules.
module NanoUI.Widgets.Combinators
  ( buttonStyled
  , buttonStyledEx
  , selectableItem
  , withBoundedIndex
  )
where

import Control.Monad (when)
import Data.Text (Text)
import Effectful (Eff, type (:>))
import NanoUI.Context (isDisabled, registerFocusable)
import NanoUI.Layout.Arena (NodeType (..))
import NanoUI.Monad (Ui, askContext, nextId, uiIO)
import NanoUI.Style (Layout (..))
import NanoUI.Widgets.Behavior (keyActivated)
import NanoUI.Widgets.Node
  ( Response (..)
  , addWidgetStyled
  , inertResponse
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
