
-- | The layout overlay and arena ranges, from inside a view.
module NanoUI.Internal.Monad.Explain
  ( explainLayout
  , explainingLayout
  , explainedNode
  , explainScope
  , withArenaRange
  , inArenaRange
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.IORef (modifyIORef')
import NanoUI.Internal.Context (Context (ctxNodeArena, ctxExplain), ExplainState (esScopes), ExplainedNode, getExplainLayout, getExplainedNode, setExplainLayout)
import NanoUI.Internal.Layout.Arena (arenaCount)
import NanoUI.Internal.Monad.Core (NanoUI, withContext, askContext)

-- | Turn the layout overlay on or off. It outlines every layout node one pixel
-- inside its edge, coloured by depth and drawn in the node's layer, and tints
-- the node under the pointer with its content box outlined. It does not
-- affect layout or input. A change repaints the whole window; setting the
-- current value is a no-op, so a view can call this every frame:
--
-- > on <- explainingLayout
-- > (_, on') <- withValue on (checkbox "Outline layout nodes")
-- > explainLayout on'
--
-- Backend options can enable it at startup (@sdlExplainLayout@,
-- @optExplainLayout@); on your own context use 'setExplainLayout' from
-- "NanoUI.Backend".
explainLayout :: Bool -> NanoUI ()
explainLayout on = withContext (\ctx -> setExplainLayout ctx on)

-- | Whether the layout overlay is on ('explainLayout').
explainingLayout :: NanoUI Bool
explainingLayout = withContext getExplainLayout

-- | The node under the pointer while the layout overlay is on, as laid out
-- last frame: its kind, rect and padding. 'Nothing' when the overlay is off
-- or the pointer is over no node. A change requests a frame, so a debug panel
-- showing it keeps up with the pointer.
explainedNode :: NanoUI (Maybe ExplainedNode)
explainedNode = withContext getExplainedNode

-- | Limit the layout overlay ('explainLayout') to the nodes the body adds and
-- their descendants. With several scopes, all of them are shown; with none,
-- the whole view is. With the overlay off this just runs the body, so it can
-- stay in a view:
--
-- > explainScope (settingsPanel model)
explainScope :: NanoUI a -> NanoUI a
explainScope body = do
  ctx <- askContext
  on <- liftIO (getExplainLayout ctx)
  if not on
    then body
    else withArenaRange (\from below -> modifyIORef' (ctxExplain ctx) (\es -> es {esScopes = (from, below) : esScopes es})) body

-- | Run @body@, then pass @record@ the range of arena indices it added,
-- @from@ up to but not including @below@. The arena appends nodes in
-- declaration order and a subtree follows its root, so the range covers
-- exactly the nodes the body declared and their descendants.
withArenaRange :: (Int -> Int -> IO ()) -> NanoUI a -> NanoUI a
withArenaRange record body = do
  ctx <- askContext
  let count = liftIO (arenaCount (ctxNodeArena ctx))
  !from <- count
  a <- body
  !below <- count
  liftIO (record from below)
  pure a

-- | Whether arena index @idx@ is in a range 'withArenaRange' recorded.
{-# INLINE inArenaRange #-}
inArenaRange :: Int -> Int -> Int -> Bool
inArenaRange from below idx = from <= idx && idx < below

