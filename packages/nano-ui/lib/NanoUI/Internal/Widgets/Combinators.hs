-- | Button, selection and cache helpers shared by the widget modules.
module NanoUI.Internal.Widgets.Combinators
  ( buttonStyledEx
  , withBoundedIndex
  , finishToggle
  , finishInput
  , readDerived
  , writeDerived
  )
where

import Control.Monad (when, (>=>))
import Data.Dynamic (fromDynamic, toDyn)
import Data.IORef (modifyIORef', readIORef)
import Data.IntMap.Strict qualified as IM
import Data.Text (Text)
import Data.Typeable (Typeable)
import NanoUI.Internal.Context
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Store (fieldInt, Field, boolInt)
import NanoUI.Internal.Layout.Arena (NodeType (..))
import NanoUI.Internal.Monad (NanoUI, freshWidget, liftIO)
import NanoUI.Internal.Style (Layout (..))
import NanoUI.Internal.Widgets.Behavior (keyActivated)
import NanoUI.Internal.Widgets.Node

-- | A button, given whether it is enabled, its value, and a style index for
-- active, sort, badge, or close chrome: the shared activation path for
-- ordinary buttons, menu items, and header chrome. An enabled button is
-- focusable and activatable with Enter or Space while focused.
-- Disabled controls keep their identity and geometry but cannot take focus or
-- activate, including through a click queued before they became disabled.
{-# INLINE buttonStyledEx #-}
buttonStyledEx :: Bool -> Text -> Float -> Layout -> Int -> NanoUI Response
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

-- | Publish an input's result and remember it for controlled adoption. The
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
readDerived :: Typeable a => Context -> Int -> IO (Maybe a)
readDerived ctx key = (IM.lookup key >=> fromDynamic) <$> readIORef (ctxDerivedCache ctx)

-- | Cache a value derived by widget @key@. Entries of widgets no longer
-- built are never removed one by one, so a full cache is cleared instead,
-- when a new key would grow it; replacing a key's entry never clears it.
writeDerived :: Typeable a => Context -> Int -> a -> IO ()
writeDerived ctx key v = modifyIORef' (ctxDerivedCache ctx) $ \cache ->
  IM.insert key (toDyn v) (if IM.size cache >= 64 && IM.notMember key cache then IM.empty else cache)

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
