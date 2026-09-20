{-# LANGUAGE OverloadedStrings #-}

-- | Controlled radio groups over zero-based option indices or bounded enum values.
module NanoUI.Widgets.Radio
  ( radio
  , radio'
  , boundedRadio
  , boundedRadio'
  , enumRadio
  , enumRadio'
  )
where

import Control.Monad (foldM)
import Data.Foldable (toList)
import Data.Hashable (hash)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful (Eff, type (:>))
import NanoUI.Context (adoptStoreInt, intKey, registerFocusable)
import NanoUI.Store (fieldInt)
import NanoUI.Layout.Arena (NodeType (..))
import NanoUI.Monad (Ui, askContext, nextId, uiIO, withKey)
import NanoUI.Style (Layout, defaultLayout, fillW, gap, tight)
import NanoUI.Types (clamp)
import NanoUI.Widgets.Behavior (KeyNav (..), useKeyNav)
import NanoUI.Widgets.Combinators
  ( finishInput
  , selectableItem
  , withBoundedIndex
  )
import NanoUI.Widgets.Layout (column')
import NanoUI.Widgets.Node
  ( Response (..)
  , tagContainer
  )

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
      !key = intKey gid
    stored <- uiIO $ adoptStoreInt ctx gid key (clamp 0 (len - 1) index)
    let
      !sel = clamp 0 (len - 1) stored
    uiIO $ registerFocusable ctx gid
    nav <- useKeyNav gid
    let
      !navDelta =
        (if knDown nav || knRight nav then 1 else 0 :: Int)
          - (if knUp nav || knLeft nav then 1 else 0)
      !selNav = if navDelta == 0 then sel else clamp 0 (len - 1) (sel + navDelta)
    column' radioGroupLay $ do
      tagContainer gid
      (combinedResp, clickedIdx) <- addRadioOptions selNav opts
      let
        !finalSel = if clickedIdx >= 0 then clickedIdx else selNav
      -- Compare with the caller's index, as 'NanoUI.Widgets.Select' does, so a
      -- selection stored between frames still reports a change.
      finishInput fieldInt ctx gid key (clamp 0 (len - 1) index) combinedResp finalSel

-- Use the ordinary widget path for every option, including singleton groups.
-- It owns IDs, node construction, and scroll-aware interaction geometry.
addRadioOptions :: Ui :> es => Int -> [Text] -> Eff es (Response, Int)
addRadioOptions sel opts = foldM addOption (mempty, -1) (zip [0 ..] opts)
 where
  addOption (!acc, !clickedIdx) (i, txt) = do
    r <- selectableItem NodeRadio txt (sel == i) radioLay i
    let
      clickedIdx' = if rawRespClicked r && clickedIdx < 0 then i else clickedIdx
    pure (acc <> r, clickedIdx')

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
