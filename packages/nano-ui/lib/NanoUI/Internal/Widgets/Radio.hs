-- | Controlled radio groups over zero-based option indices or bounded enum
-- values. Each option is a button in the choice look ('buttonFlagChoice').
module NanoUI.Internal.Widgets.Radio
  ( radio
  , radio'
  , enumRadioBy
  , enumRadioBy'
  , enumRadio
  , enumRadio'
  )
where

import Control.Monad (void, zipWithM)
import Data.List (findIndex)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import NanoUI.Internal.Context (adoptSlot, registerFocusable)
import NanoUI.Internal.Store (fieldInt)
import NanoUI.Internal.Layout.Arena (NodeType (..))
import NanoUI.Internal.Monad (NanoUI, freshWidget, nextId, scope, liftIO)
import NanoUI.Internal.Style (Layout, defaultLayout, fillW, gap, tight)
import NanoUI.Internal.Types (clamp)
import NanoUI.Internal.WidgetText (buttonFlagChoice)
import NanoUI.Internal.Widgets.Behavior (KeyNav (..), useKeyNav)
import NanoUI.Internal.Var (Binding (..), bindVar)
import NanoUI.Internal.Var.Types (Var)
import NanoUI.Internal.Widgets.Combinators (bindEnumPicker, finishInput, nonEmptyOpts, widgetBaseLayout)
import NanoUI.Internal.Widgets.Layout (column')
import NanoUI.Internal.Widgets.Node (Response (..), addWidgetStyled, moveSelection, setChanged, tagContainer)

radioLay :: Layout
radioLay = tight (fillW defaultLayout)

radioGroupLay :: Layout
radioGroupLay = tight (gap 4 (fillW defaultLayout))

-- | A column of radio buttons over @options@ in fold order, bound to mutable
-- state ('Var'): a click or the arrow keys update the zero-based selected index.
{-# INLINE radio #-}
radio :: Foldable f => f Text -> Var Int -> NanoUI ()
radio options = void . radio' options

-- | 'radio' returning the group's 'Response'.
{-# INLINE radio' #-}
radio' :: Foldable f => f Text -> Var Int -> NanoUI Response
radio' options v = bindVar RenderFinal v (radioCore options)

-- | 'radio' on a plain index: the response and the index after this frame.
radioCore ::
  Foldable f => f Text -> Int -> NanoUI (Response, Int)
radioCore options index =
  -- A positional scope: groups declared side by side keep separate ids.
  scope $ do
    (gid, ctx) <- freshWidget
    itemLay <- widgetBaseLayout radioLay id
    let opts = nonEmptyOpts options
        !len = length opts
        !given = clamp 0 (len - 1) index
    stored <- liftIO $ adoptSlot fieldInt ctx gid given
    let !sel = clamp 0 (len - 1) stored
    liftIO $ registerFocusable ctx gid
    nav <- useKeyNav gid
    let !navDelta = fromEnum (knDown nav || knRight nav) - fromEnum (knUp nav || knLeft nav)
        !selNav = clamp 0 (len - 1) (sel + navDelta)
        option i txt = do
          wid <- nextId
          -- Visual style 1 draws a ring.
          addWidgetStyled wid NodeButton txt (if selNav == i then 1 else 0) itemLay (buttonFlagChoice + 1)
    column' radioGroupLay $ do
      tagContainer gid
      resps <- zipWithM option [0 ..] opts
      let clicked = findIndex rawRespClicked resps
          final = fromMaybe selNav clicked
      liftIO (moveSelection ctx selNav final (zip [0 ..] resps))
      -- Compare with the caller's index, as 'NanoUI.Internal.Widgets.Select' does, so a
      -- selection stored between frames still reports a change. An index the
      -- options do not reach yet stays the caller's until the user picks,
      -- even the option it shows.
      (resp, s) <- finishInput fieldInt ctx gid given (mconcat resps) final
      pure $
        if isNothing clicked && navDelta == 0 && s == given
          then (resp, index)
          else (setChanged (s /= index) resp, s)

-- | Radio buttons for every value of a bounded enum, labeled by @encode@.
{-# INLINE enumRadioBy #-}
enumRadioBy :: (Bounded a, Enum a) => (a -> Text) -> Var a -> NanoUI ()
enumRadioBy encode = void . enumRadioBy' encode

-- | 'enumRadioBy' returning the group's 'Response'.
{-# INLINE enumRadioBy' #-}
enumRadioBy' :: (Bounded a, Enum a) => (a -> Text) -> Var a -> NanoUI Response
enumRadioBy' = bindEnumPicker radioCore

-- | 'enumRadioBy' labeled with 'show'.
{-# INLINE enumRadio #-}
enumRadio :: (Bounded a, Enum a, Show a) => Var a -> NanoUI ()
enumRadio = enumRadioBy (T.pack . show)

-- | 'enumRadio' returning the group's 'Response'.
{-# INLINE enumRadio' #-}
enumRadio' :: (Bounded a, Enum a, Show a) => Var a -> NanoUI Response
enumRadio' = enumRadioBy' (T.pack . show)
