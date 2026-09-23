-- | What every integration test module imports: the library, the test
-- harness and assertions, and the shape of a test with its two helpers.
module Spec
  ( Spec
  , spec
  , pixelSpec
  , arenaRects
  , buttonValues
  , widgetValue
  , withMonospaceFonts
  , module Control.Monad
  , module Data.IORef
  , module NanoUI
  , module NanoUI.Backend
  , module NanoUI.Testing
  , module NanoUI.Testing.Assert
  , module NanoUI.Testing.Harness
  ) where

import Control.Monad
import Data.IORef
import Data.Text qualified as T
import NanoUI
import NanoUI.Backend
import NanoUI.Testing
import NanoUI.Testing.Assert
import NanoUI.Testing.Harness
import NanoUI.Internal.Context (Context (..))
import NanoUI.Internal.Layout.Arena (NodeType (NodeButton), arenaCount, getNodeRect, getNodeType, getNodeValue, lookupNodeByWidgetId)

-- | A test's name, the context it runs on, and the test, which bumps the
-- failure counter for each failed check.
type Spec = (String, IO Context, Context -> IORef Int -> IO ())

-- | A test on a headless context, and one on a pixel-snapped context.
spec, pixelSpec :: String -> (Context -> IORef Int -> IO ()) -> Spec
spec name run = (name, newContext, run)
pixelSpec name run = (name, newPixelContext, run)

-- | @base@ with a monospace base font of @baseCell@ px cells, and
-- @sizedCell@ px cells for text given a font size of its own, so a test can
-- tell which font measured or drew something.
withMonospaceFonts :: Float -> Float -> Context -> Context
withMonospaceFonts baseCell sizedCell base =
  withFontResolver
    (withFontMetrics base (monospaceMetrics baseCell))
    (\_ _ _ _ -> pure (monospaceMetrics sizedCell, False))
    (\_ _ _ _ txt -> pure (sizedCell * fromIntegral (T.length txt), sizedCell))

-- | Every node's laid-out rect, in arena order.
arenaRects :: Context -> IO [Rect]
arenaRects ctx = do
  n <- arenaCount (ctxNodeArena ctx)
  mapM (getNodeRect (ctxNodeArena ctx)) [0 .. n - 1]

-- | The value of every button in arena order: which of a group's options,
-- rows or headers is marked selected, in a view of that group alone.
buttonValues :: Context -> IO [Float]
buttonValues ctx = do
  let na = ctxNodeArena ctx
  n <- arenaCount na
  fmap concat . forM [0 .. n - 1] $ \i -> do
    nt <- getNodeType na i
    if nt == NodeButton then pure <$> getNodeValue na i else pure []

-- | The value of widget @wid@'s node this frame, such as whether a checkbox
-- is drawn checked.
widgetValue :: Context -> WidgetId -> IO (Maybe Float)
widgetValue ctx wid = lookupNodeByWidgetId (ctxNodeArena ctx) wid >>= traverse (getNodeValue (ctxNodeArena ctx))
