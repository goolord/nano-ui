{-# OPTIONS_GHC -O0 #-}
-- | What every integration test module imports: the library, the test
-- harness and assertions, and the shape of a test with its two helpers.
module Spec
  ( Spec
  , spec
  , pixelSpec
  , simSpec
  , pixelSimSpec
  , varSimSpec
  , everyControl
  , arenaRects
  , nodeLayout
  , sameAsFreshSolve
  , fillsIn
  , buttonValues
  , widgetValue
  , wheelIntoView
  , assertPaintedLines
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
import GHC.Stack (HasCallStack, withFrozenCallStack)
import NanoUI
import NanoUI.Backend
import NanoUI.Testing
import NanoUI.Testing.Assert
import NanoUI.Testing.Harness
import NanoUI.Testing.Sim (Sim, runSim)
import NanoUI.Internal.Context (Context (..))
import NanoUI.Internal.Layout.Arena
  ( arenaCount
  , getNodeRect
  , getNodeType
  , getNodeValue
  , getScrollContentW
  , isScrollNode
  , lookupNodeByWidgetId
  )

-- | A test's name, the context it runs on, and the test, which bumps the
-- failure counter for each failed check.
type Spec = (String, IO Context, Context -> IORef Int -> IO ())

-- | A test on a headless context, and one on a pixel-snapped context.
spec, pixelSpec :: String -> (Context -> IORef Int -> IO ()) -> Spec
spec name run = (name, newContext, run)
pixelSpec name run = (name, newPixelContext, run)

-- | A 'NanoUI.Testing.Sim' script against @view@ in a @w@ x @h@ window, on a
-- headless or a pixel-snapped context.
simSpec, pixelSimSpec :: String -> Float -> Float -> NanoUI a -> Sim a () -> Spec
simSpec name w h view script = spec name (\ctx failed -> runSim ctx failed (withInput w h) view script)
pixelSimSpec name w h view script = pixelSpec name (\ctx failed -> runSim ctx failed (withInput w h) view script)

-- | 'simSpec' on a 'Var' holding @initial@, which the view and the script
-- share.
varSimSpec :: String -> Float -> Float -> v -> (Var v -> NanoUI a) -> (Var v -> Sim a ()) -> Spec
varSimSpec name w h initial view script =
  spec name (\ctx failed -> newVar initial >>= \v -> runSim ctx failed (withInput w h) (view v) (script v))

-- | One of each control a user can focus and work, with its value shown, for
-- the properties every control has: focus from code, no keys while
-- disabled, no pointer through an overlay.
everyControl :: [(String, NanoUI (Response, String))]
everyControl =
  [ ("button", (,"") <$> button' "Go")
  , ("checkbox", shown (withValue False (checkbox' "Check")))
  , ("toggle", shown (withValue False (toggleSwitchWith' id)))
  , ("slider", shown (withValue 50 (slider' 0 100)))
  , ("knob", shown (withValue 50 (knobWith' id 36 0 100)))
  , ("radio", shown (withValue 0 (radio' ["one", "two", "three"])))
  , ("select", shown (withValue 0 (selectWith' fillW ["a", "b", "c"])))
  , ("text input", shown (withValue "some text" textInput'))
  , ("search input", shown (withValue "some text" (searchInput' "Search")))
  , ("numeric input", shown (withValue 5 numericInput'))
  , ("combo box", shown (withValue "a" (comboBox' "Choose" ["a", "b"])))
  , ("text area", shown (withValue (T.unlines (replicate 30 "a line")) (textAreaWith' (fixedH 120))))
  , ("color picker", shown (withValue (colorRGBA 200 40 40 255) colorPicker'))
  ]
  where
    shown :: Show v => NanoUI (Response, v) -> NanoUI (Response, String)
    shown = fmap (fmap show)

-- | @base@ with a monospace base font of @baseCell@ px cells, and
-- @sizedCell@ px cells for text given a font size of its own, so a test can
-- tell which font measured or drew something.
withMonospaceFonts :: Float -> Float -> Context -> Context
withMonospaceFonts baseCell sizedCell base =
  withFontResolver
    (withFontMetrics base (monospaceMetrics baseCell))
    (\_ _ _ _ -> pure (monospaceMetrics sizedCell, False))
    (\_ _ _ _ txt -> pure (sizedCell * fromIntegral (T.length txt), sizedCell))

-- | The colours of the quads that overlap @r@.
fillsIn :: Rect -> [(Rect, Color)] -> [Color]
fillsIn r quads = [c | (q, c) <- quads, rectIntersect q r /= Nothing]

-- | Every node's laid-out rect, in arena order.
arenaRects :: Context -> IO [Rect]
arenaRects ctx = do
  n <- arenaCount (ctxNodeArena ctx)
  mapM (getNodeRect (ctxNodeArena ctx)) [0 .. n - 1]

-- | Every node's rect and each scroll container's content extents.
nodeLayout :: Context -> IO [(Rect, Maybe (Float, Float))]
nodeLayout ctx = do
  let na = ctxNodeArena ctx
      scrollOf i = getNodeType na i >>= \nt ->
        if isScrollNode nt
          then Just <$> ((,) <$> getNodeValue na i <*> getScrollContentW na i)
          else pure Nothing
  n <- arenaCount na
  forM [0 .. n - 1] $ \i -> (,) <$> getNodeRect na i <*> scrollOf i

-- | Run a frame on @ctx@, which reuses what its last solve placed, and on
-- @fresh@ with its layout cache dropped, so it solves from scratch, and
-- check the two lay out the same ('nodeLayout'). The result on @ctx@.
sameAsFreshSolve :: HasCallStack => IORef Int -> Context -> Context -> Input -> NanoUI a -> IO a
sameAsFreshSolve failed fresh ctx inp ui = do
  (a, _, _) <- runFrame ctx inp ui
  writeIORef (ctxLayoutCache fresh) Nothing
  _ <- runFrame fresh inp ui
  expected <- nodeLayout fresh
  withFrozenCallStack (assertEq failed expected) =<< nodeLayout ctx
  pure a

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

-- | Wheel down over the scroller @sid@ a notch at a time until the button
-- @pick@ takes from the view's result lies inside it, with @margin@ to spare
-- below, or it stops scrolling. The offset reached and the button, or
-- 'Nothing' before the scroller is laid out.
wheelIntoView :: Context -> Input -> NanoUI a -> (a -> Response) -> WidgetId -> Float -> IO (Maybe (Float, Response))
wheelIntoView ctx inp ui pick sid margin =
  getPrevRect ctx sid >>= traverse (\scroller@(Rect _ sy _ sh) -> do
    let hover = inp {inputMousePos = spanCenter scroller}
        wheel = hover {inputScroll = V2 0 1}
        inView (Rect _ y _ h) = y >= sy + 4 && y + h + margin <= sy + sh
        pump = do
          before <- getScrollOffset ctx sid
          _ <- runFrame ctx wheel ui
          after <- getScrollOffset ctx sid
          btn <- pick <$> evalUi ctx hover ui
          if inView (respRect btn) || after <= before then pure (after, btn) else pump
    pump)

-- | Check that paint wrapped a label's text where the solve placed it: the
-- frame's text spans other than @others@ are as many lines as the label's
-- rect @r@ is tall, none wider than it. The headless font's glyphs are as
-- wide as its 12 px line is tall.
assertPaintedLines :: IORef Int -> Context -> [T.Text] -> Rect -> IO ()
assertPaintedLines failed ctx others r = do
  lns <- (\spans -> [t | (_, t, _, _, _) <- spans, t `notElem` others]) <$> collectTextSpans ctx
  assertEq failed (rectH r) (12 * fromIntegral (length lns))
  forM_ lns $ \t -> assert failed (12 * fromIntegral (T.length t) <= rectW r)
