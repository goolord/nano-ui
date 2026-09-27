module Cases.Caption (tests) where

import Spec

tests :: [Spec]
tests =
  [ spec "caption-drag-spans" runDragSpansTest
  , spec "caption-buttons" runCaptionButtonsTest
  ]

-- | What is left of a title bar to drag a window by: the gaps between the
-- widgets in it, and nothing where a widget is.
runDragSpansTest :: Context -> IORef Int -> IO ()
runDragSpansTest _ failed = do
  let bar = Rect 0 0 400 30
      menus = Rect 0 0 100 30
      buttons = Rect 280 0 120 30
  -- One gap, between the two.
  assertEq failed (dragSpans bar [menus, buttons]) [Rect 100 0 180 30]
  -- The order they are given in does not matter.
  assertEq failed (dragSpans bar [buttons, menus]) [Rect 100 0 180 30]
  -- A bar with nothing in it drags by all of itself.
  assertEq failed (dragSpans bar []) [bar]
  -- Two gaps, where something sits in the middle.
  assertEq failed
    (dragSpans bar [menus, Rect 150 4 50 22, buttons])
    [Rect 100 0 50 30, Rect 200 0 80 30]
  -- Overlapping widgets leave one gap, not a negative one.
  assertEq failed (dragSpans bar [Rect 0 0 200 30, Rect 100 0 150 30]) [Rect 250 0 150 30]
  -- A widget covering the bar leaves nothing to drag by.
  assertEq failed (dragSpans bar [Rect 0 0 400 30]) []
  -- Rectangles off the bar are not in the way: a row below it, and one left
  -- of where it starts.
  assertEq failed (dragSpans bar [Rect 150 40 50 20]) [bar]
  assertEq failed (dragSpans (Rect 100 0 300 30) [Rect 0 0 60 30]) [Rect 100 0 300 30]

-- | The three caption buttons: the rectangle they hand back spans all of
-- them, and a click on the last one is a close.
runCaptionButtonsTest :: Context -> IORef Int -> IO ()
runCaptionButtonsTest ctx failed = do
  let inp0 = withInput 400 200
      ui = rowWith (tight . fillW . fixedH captionBarHeight) $ do
        flex
        captionButtons False
  (action0, buttons) <- warmup2 ctx inp0 ui
  assertEq failed action0 Nothing
  -- The three buttons and whatever the row puts between them, in its
  -- right-hand end.
  assert failed (rectW buttons >= 3 * capButtonW defaultCaptionConfig)
  assertEq failed (rectH buttons) (capButtonH defaultCaptionConfig)
  assert failed (rectX buttons > 0 && rectX buttons + rectW buttons <= 400)

  -- The close button is the last of the three.
  let closeW = capButtonW defaultCaptionConfig
      close = Rect (rectX buttons + rectW buttons - closeW) (rectY buttons) closeW (rectH buttons)
  (action1, _) <- runClick ctx inp0 ui (spanCenter close)
  assertEq failed action1 (Just CaptionClose)
