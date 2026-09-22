-- | Controlled radio groups over zero-based option indices or bounded enum values.
module NanoUI.Internal.Widgets.Radio
  ( radio
  , radio'
  , boundedRadio
  , boundedRadio'
  , enumRadio
  , enumRadio'
  )
where

import Control.Monad (zipWithM)
import Data.Foldable (toList)
import Data.Hashable (hash)
import Data.List (findIndex)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful (Eff, type (:>))
import NanoUI.Internal.Context (adoptSlot, registerFocusable)
import NanoUI.Internal.Store (fieldInt)
import NanoUI.Internal.Layout.Arena (NodeType (..))
import NanoUI.Internal.Monad (Ui, askContext, nextId, uiIO, withKey)
import NanoUI.Internal.Style (Layout, defaultLayout, fillW, gap, tight)
import NanoUI.Internal.Types (clamp)
import NanoUI.Internal.Widgets.Behavior (KeyNav (..), useKeyNav)
import NanoUI.Internal.Widgets.Combinators (finishInput, withBoundedIndex)
import NanoUI.Internal.Widgets.Layout (column')
import NanoUI.Internal.Widgets.Node (Response (..), addWidgetStyled, tagContainer)

radioLay :: Layout
radioLay = tight (fillW defaultLayout)

radioGroupLay :: Layout
radioGroupLay = tight (gap 4 (fillW defaultLayout))

radioSalt :: Int
radioSalt = hash ("radio" :: Text)

-- | A column of radio buttons over @options@ in fold order. Pass the selected
-- index; the result is the index after this frame's click or arrow keys.
{-# INLINE radio #-}
radio :: (Foldable f, Ui :> es) => f Text -> Int -> Eff es Int
radio options index = snd <$> radio' options index

-- | 'radio' returning the group response and selected option index.
radio' ::
  (Foldable f, Ui :> es) => f Text -> Int -> Eff es (Response, Int)
radio' options index =
  withKey radioSalt $ do
    gid <- nextId
    ctx <- askContext
    let
      opts = case toList options of
        [] -> [""]
        xs -> xs
      !len = length opts
      !given = clamp 0 (len - 1) index
    stored <- uiIO $ adoptSlot fieldInt ctx gid given
    let
      !sel = clamp 0 (len - 1) stored
    uiIO $ registerFocusable ctx gid
    nav <- useKeyNav gid
    let
      !navDelta = fromEnum (knDown nav || knRight nav) - fromEnum (knUp nav || knLeft nav)
      !selNav = clamp 0 (len - 1) (sel + navDelta)
      option i txt = do
        wid <- nextId
        addWidgetStyled wid NodeRadio txt (if selNav == i then 1 else 0) radioLay i
    column' radioGroupLay $ do
      tagContainer gid
      resps <- zipWithM option [0 ..] opts
      -- Compare with the caller's index, as 'NanoUI.Internal.Widgets.Select' does, so a
      -- selection stored between frames still reports a change.
      let clicked = findIndex rawRespClicked resps
      finishInput fieldInt ctx gid given (mconcat resps) (fromMaybe selNav clicked)

-- | Radio buttons for every value of a bounded enum, labelled by @encode@.
{-# INLINE boundedRadio #-}
boundedRadio :: (Bounded a, Enum a, Ui :> es) => (a -> Text) -> a -> Eff es a
boundedRadio encode value = snd <$> boundedRadio' encode value

-- | 'boundedRadio' returning the response and selected enum value.
boundedRadio' :: (Bounded a, Enum a, Ui :> es) => (a -> Text) -> a -> Eff es (Response, a)
boundedRadio' encode value = withBoundedIndex encode value radio'

-- | 'boundedRadio' labelled with 'show'.
{-# INLINE enumRadio #-}
enumRadio :: (Bounded a, Enum a, Show a, Ui :> es) => a -> Eff es a
enumRadio = boundedRadio (T.pack . show)

-- | 'enumRadio' returning the response and selected enum value.
enumRadio' :: (Bounded a, Enum a, Show a, Ui :> es) => a -> Eff es (Response, a)
enumRadio' = boundedRadio' (T.pack . show)
