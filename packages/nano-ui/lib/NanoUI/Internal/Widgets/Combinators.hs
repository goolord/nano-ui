-- | Button, selection and cache helpers shared by the widget modules.
module NanoUI.Internal.Widgets.Combinators
  ( widgetBaseLayout
  , buttonStyledEx
  , choiceToggleWidget
  , bindEnumPicker
  , nonEmptyOpts
  , finishToggle
  , finishInput
  , readDerived
  , writeDerived
  )
where

import Control.Monad (when)
import Data.Foldable (toList)
import Data.IORef (modifyIORef', readIORef)
import Data.Text (Text)
import Data.Word (Word64)
import NanoUI.Internal.Context
import NanoUI.Internal.Derived (DerivedField, insertDerived, lookupDerived)
import NanoUI.Internal.Equality (sameEnum)
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Layout.Arena (NodeType (..))
import NanoUI.Internal.Monad (NanoUI, askDefaultLayout, freshWidget, liftIO)
import NanoUI.Internal.Store (Field, boolInt, fieldInt)
import NanoUI.Internal.Style (Layout (..), inheritFont)
import NanoUI.Internal.Var (Binding (..), bindVarBy)
import NanoUI.Internal.Var.Types (Var)
import NanoUI.Internal.Widgets.Behavior (keyActivated)
import NanoUI.Internal.Widgets.Node

-- | Apply @f@ over @base@ after inheriting the ambient default layout's font.
{-# INLINE widgetBaseLayout #-}
widgetBaseLayout :: Layout -> (Layout -> Layout) -> NanoUI Layout
widgetBaseLayout base f = do
  dl <- askDefaultLayout
  pure (f (inheritFont dl base))

-- | A button, given whether it is enabled, its value, and a style index for
-- active, sort, badge, or close chrome: the shared activation path for
-- ordinary buttons, menu items, and header chrome. An enabled button is
-- focusable and activatable with Enter or Space while focused.
-- Disabled controls keep their identity and geometry but cannot take focus or
-- activate, including through a click queued before they became disabled.
{-# INLINE buttonStyledEx #-}
buttonStyledEx :: Bool -> Text -> Float -> Layout -> Word64 -> NanoUI Response
buttonStyledEx enabled txt value layout styleIdx = do
  (wid, ctx) <- freshWidget
  disabled <- liftIO (isDisabled ctx wid)
  let active = enabled && not disabled
  when active $ liftIO (registerFocusable ctx wid)
  resp <- addWidgetStyled wid NodeButton txt value layout styleIdx
  if active
    then do
      keyClick <- keyActivated wid
      pure (if keyClick then setClicked True resp else resp)
    else pure (inertResponse resp)

-- | Controlled toggle widget: registers focus, adopts the boolean model value,
-- lays out and styles the node, resolves toggle clicks and keyboard activation,
-- and updates the node's visual value when toggled.
{-# INLINE choiceToggleWidget #-}
choiceToggleWidget :: Text -> Layout -> Word64 -> Bool -> NanoUI (Response, Bool)
choiceToggleWidget txt lay flag on = do
  (wid, ctx) <- freshWidget
  liftIO $ registerFocusable ctx wid
  current <- liftIO (intBool <$> adoptSlot fieldInt ctx wid (boolInt on))
  resp <- addWidgetStyled wid NodeButton txt (if current then 1 else 0) lay flag
  result@(_, value) <- finishToggle ctx wid current resp
  when (value /= current) . liftIO $ setWidgetValue ctx wid (if value then 1 else 0)
  pure result

-- | Finish a boolean control after its node has registered focus eligibility.
-- Keyboard activation changes the value without inventing a pointer click.
{-# INLINE finishToggle #-}
finishToggle ::
  Context -> WidgetId -> Bool -> Response -> NanoUI (Response, Bool)
finishToggle ctx wid current resp = do
  keyClick <- keyActivated wid
  let
    clicked = respClicked resp || keyClick
    value = current /= clicked
  liftIO $ do
    writeStoreBool ctx wid value
    recordSlot fieldInt ctx (intKey wid) (boolInt value)
  pure (setChanged clicked resp, value)

-- | Publish an input's result and record it for controlled adoption. The
-- caller chooses the comparison value: live state or the supplied model value.
{-# INLINE finishInput #-}
finishInput ::
  Eq a =>
  Field a
  -> Context
  -> WidgetId
  -> a
  -> Response
  -> a
  -> NanoUI (Response, a)
finishInput field ctx wid original resp value = do
  liftIO $ do
    writeSlot field ctx wid (intKey wid) value
    recordSlot field ctx (intKey wid) value
  pure (setChanged (value /= original) resp, value)

-- | The value widget @key@ cached in 'ctxDerivedCache', if present.
readDerived :: DerivedField a -> Context -> Int -> IO (Maybe a)
readDerived field ctx key = lookupDerived field key <$> readIORef (ctxDerivedCache ctx)

-- | Cache a value derived by widget @key@. Entries of widgets no longer
-- built are never removed one by one, so a full cache is cleared instead,
-- when a new key would grow it; replacing a key's entry never clears it.
writeDerived :: DerivedField a -> Context -> Int -> a -> IO ()
writeDerived field ctx key value = modifyIORef' (ctxDerivedCache ctx) (insertDerived field key value)

-- | Non-empty option list, defaulting an empty foldable to a single blank option.
nonEmptyOpts :: Foldable f => f Text -> [Text]
nonEmptyOpts xs = case toList xs of
  [] -> [""]
  ys -> ys

-- | Bind a 'Var' over every value of a bounded enum to an index-based picker.
-- Indices are offset by @fromEnum minBound@, so enums that do not start at 0
-- map accurately.
{-# INLINE bindEnumPicker #-}
bindEnumPicker ::
  forall a.
  (Bounded a, Enum a) =>
  ([Text] -> Int -> NanoUI (Response, Int)) -> (a -> Text) -> Var a -> NanoUI Response
bindEnumPicker pick encode v =
  bindVarBy sameEnum RenderFinal v $ \val ->
    fmap (toEnum . (+ lower)) <$> pick opts (fromEnum val - lower)
  where
    lower = fromEnum (minBound :: a)
    opts = map encode [minBound .. maxBound]
