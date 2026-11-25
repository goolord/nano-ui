-- | Regression helpers specific to nano-ui's own integration test suite.
module TestSupport
  ( assertScrollGutterPad
  , assertWheelTitlePinned
  , checkLabelAlignEndInk
  , checkIdleFullDamage
  , windowTitleGrab
  , scrollArea
  , scrollArea2D
  , scrollAreaConfigured
  ) where

import Control.Monad (forM_, when)
import Data.IORef (IORef)
import Data.Maybe (listToMaybe)
import Data.Text qualified as T
import GHC.Stack (HasCallStack)
import NanoUI
import NanoUI.Backend
import NanoUI.Testing
import NanoUI.Testing.Assert (assert, assertEq, assertJustM, assertLt, bump)
import NanoUI.Testing.Harness (spanYOf)

-- | Check that a child's right edge ends at the scroller's content edge,
-- given gutter width and end padding in logical pixels.
assertScrollGutterPad ::
  HasCallStack
  => IORef Int
  -> Context
  -> WidgetId
  -> Response
  -> Float
  -> Float
  -> IO ()
assertScrollGutterPad failed ctx sid child gutter endPad =
  assertJustM failed (getPrevRect ctx sid) $ \(Rect sx _ sw _) -> do
    let Rect cx _ cw _ = respRect child
        contentRight = sx + sw - endPad - gutter
    assert failed (cx + cw >= contentRight - 0.5)
    assert failed (cx + cw <= contentRight + 0.01)

-- | Scroll an overlay by one wheel step and require its title to stay fixed
-- while its first body line moves upward (if it is still shown).
assertWheelTitlePinned ::
  HasCallStack
  => IORef Int
  -> Context
  -> Input
  -> NanoUI a
  -> T.Text
  -> T.Text
  -> V2
  -> IO ()
assertWheelTitlePinned failed ctx inp0 ui title line1 wheelAt = do
  let wheel = inp0 {inputMousePos = wheelAt, inputScroll = V2 0 1}
  spans0 <- collectOverlayTextSpans ctx inp0
  _ <- runFrame ctx wheel ui
  spans1 <- collectOverlayTextSpans ctx wheel
  case (spanYOf title spans0, spanYOf title spans1, spanYOf line1 spans0) of
    (y0 : _, y1 : _, b0 : _) -> do
      assertEq failed y1 y0
      forM_ (listToMaybe (spanYOf line1 spans1)) $ \b1 -> assertLt failed b1 b0
    _ -> assert failed False

-- | Require the transition to idle input to need a frame, then require that
-- frame's damage to cover the whole window.
checkIdleFullDamage ::
  HasCallStack => IORef Int -> Context -> Input -> Input -> NanoUI a -> IO ()
checkIdleFullDamage failed ctx inpAfter inpIdle ui = do
  need <- needsRedraw ctx inpAfter inpIdle
  assert failed need
  _ <- runFrame ctx inpIdle ui
  dmg <- takeDamage ctx
  assert failed (dmg == DamageFull)

-- | Check that right-aligned labels share an ink edge despite different glyph advances.
checkLabelAlignEndInk :: IORef Int -> IO ()
checkLabelAlignEndInk failed =
  forM_ ["10", "1i", "1."] $ \txt -> do
    let (tx, _) = alignedTextPen AlignEnd 0 boxW 0 fm txt
    when (abs (tx + textInkEnd fm txt - boxW) > 0.01) $ bump failed
  where
    boxW = 100
    glyph xoff gw = Just (GlyphQuad xoff 0 gw 10 0 0 1 1)
    fm =
      (monospaceMetrics 10)
        { fmAdvance = \c -> if c == 'i' || c == '.' || c == ' ' then 4 else 10
        , fmGlyph = \c -> case c of
            'i' -> glyph 0.5 3
            '.' -> glyph 1 1
            _ -> glyph 1 8
        }

-- | Point inside the standard floating-window title bar, away from its close button.
windowTitleGrab :: Rect -> V2
windowTitleGrab (Rect x0 y0 _ _) = V2 (x0 + 24) (y0 + padT windowPad + 19.5)

-- | Adapter for integration tests written against the pre-0.2 @(WidgetId, a)@ tuple.
scrollArea :: LayoutModifier -> NanoUI a -> NanoUI (WidgetId, a)
scrollArea f body = (\(a, r) -> (respId r, a)) <$> scrollWith' f body

scrollArea2D :: LayoutModifier -> NanoUI a -> NanoUI (WidgetId, a)
scrollArea2D f body = (\(a, r) -> (respId r, a)) <$> scroll2DWith' f body

scrollAreaConfigured :: ScrollConfig -> LayoutModifier -> NanoUI a -> NanoUI (WidgetId, a)
scrollAreaConfigured cfg f body = (\(a, r) -> (respId r, a)) <$> scrollConfigured' cfg f body
