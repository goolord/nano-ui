module Cases.CustomWidget
  ( runCustomWidgetCanvasTest
  , runCustomWidgetMeasureTest
  , runCustomWidgetCursorTest
  , runCustomWidgetInteractionTest
  , runReferenceKnobTest
  , runReferenceToggleSwitchTest
  , runReferenceProgressAndSparklineTest
  , runDropTargetTest
  ) where

import Control.Monad (void)
import Data.IORef (IORef)
import Data.Vector qualified as V
import NanoUI
import NanoUI.Testing
  ( Context
  , UiCursorKind (..)
  , cursorKindIs
  , drawCmdCount
  , drawIndexCount
  , runFrame
  )
import NanoUI.Testing.Assert (assert, withInput)
import NanoUI.Testing.Harness (clickPair, warmup2)

-- | Verifies CanvasM declarative drawing primitives produce valid DrawOps.
runCustomWidgetCanvasTest :: Context -> IORef Int -> IO ()
runCustomWidgetCanvasTest ctx failed = do
  let inp = withInput 200 200
      ui = column $ do
        void $ canvas (fixedWH 100 100 defaultLayout) $ \r -> do
          drawRect r (colorRGBA 30 30 30 255)
          drawRoundedRect r 8 (colorRGBA 60 60 60 255)
          drawCircle (V2 50 50) 20 (colorRGBA 200 100 50 255)
          drawStrokeCircle (V2 50 50) 25 2 (colorRGBA 255 255 255 255)
          drawStrokeAA (V2 10 10) (V2 90 90) 2 (colorRGBA 0 200 255 255)
          drawQuadGradient
            (Rect 10 10 40 40)
            (colorRGBA 255 0 0 255)
            (colorRGBA 0 255 0 255)
            (colorRGBA 0 0 255 255)
            (colorRGBA 255 255 0 255)
  (_, _, draw, _) <- runFrame ctx inp ui
  assert failed (drawCmdCount draw > 0)
  assert failed (drawIndexCount draw >= 6)

-- | Verifies custom intrinsic layout measurement via widgetMeasure hook.
runCustomWidgetMeasureTest :: Context -> IORef Int -> IO ()
runCustomWidgetMeasureTest ctx failed = do
  let inp = withInput 400 400
      ui = column $ do
        customWidget_ defaultCustomWidgetSpec
          { widgetMeasure = Just $ \_ _ _ -> (160, 48)
          , widgetLayout = defaultLayout
          }
  resp <- warmup2 ctx inp ui
  let r = respRect resp
  assert failed (rectW r == 160 && rectH r == 48)

-- | Verifies dynamic cursor resolution on custom widgets.
runCustomWidgetCursorTest :: Context -> IORef Int -> IO ()
runCustomWidgetCursorTest ctx failed = do
  let inp0 = withInput 300 300
      ui = column $ do
        customWidget_ defaultCustomWidgetSpec
          { widgetLayout = fixedWH 80 80 defaultLayout
          , widgetCursor = Just (\_ -> UiCursorNsResize)
          }
  resp <- warmup2 ctx inp0 ui
  let Rect rx ry rw rh = respRect resp
      hoverInp = inp0 { inputMousePos = V2 (rx + rw / 2) (ry + rh / 2) }
  _ <- runFrame ctx hoverInp ui
  hoverOk <- cursorKindIs ctx hoverInp UiCursorNsResize
  assert failed hoverOk

  let outInp = inp0 { inputMousePos = V2 (rx + rw + 50) (ry + rh + 50) }
  _ <- runFrame ctx outInp ui
  outOk <- cursorKindIs ctx outInp UiCursorNsResize
  assert failed (not outOk)

-- | Verifies interaction state propagation (hover, press, click) and CustomDrawContext.
runCustomWidgetInteractionTest :: Context -> IORef Int -> IO ()
runCustomWidgetInteractionTest ctx failed = do
  let inp0 = withInput 300 300
      ui = column $ do
        canvasWith (fixedWH 80 40 defaultLayout) $ \cdc _r -> do
          pure (cdcHovered cdc, cdcPressed cdc)
  (resp0, _) <- warmup2 ctx inp0 ui
  let Rect rx ry rw rh = respRect resp0
      pos = V2 (rx + rw / 2) (ry + rh / 2)
      (pressInp, releaseInp) = clickPair inp0 pos

  _ <- runFrame ctx pressInp ui
  ((respClick, (hovered, pressed)), _, _, _) <- runFrame ctx releaseInp ui
  assert failed (respClicked respClick)
  assert failed hovered
  assert failed (not pressed)

-- | Verifies the reference rotary knob widget.
runReferenceKnobTest :: Context -> IORef Int -> IO ()
runReferenceKnobTest ctx failed = do
  let inp0 = withInput 300 300
      ui = column $ knob 0 100 25
  (resp0, val0) <- warmup2 ctx inp0 ui
  assert failed (val0 == 25)

  let Rect rx ry rw rh = respRect resp0
      pos = V2 (rx + rw / 2) (ry + rh / 2)
      dragStart = inp0 { inputMousePos = pos, inputMouseDown = True, inputMousePressed = True }
      -- Drag upward (negative dy in screen coords) to increase knob value
      dragUp = inp0 { inputMousePos = V2 (v2X pos) (v2Y pos - 30), inputMouseDown = True, inputMousePressed = False }
      release = inp0 { inputMousePos = V2 (v2X pos) (v2Y pos - 30), inputMouseDown = False, inputMouseReleased = True }

  _ <- runFrame ctx dragStart ui
  ((respDragged, valDragged), _, _, _) <- runFrame ctx dragUp ui
  assert failed (valDragged > 25)
  assert failed (respChanged respDragged)
  void $ runFrame ctx release ui

-- | Verifies the reference toggle switch widget.
runReferenceToggleSwitchTest :: Context -> IORef Int -> IO ()
runReferenceToggleSwitchTest ctx failed = do
  let inp0 = withInput 300 300
      ui = column $ toggleSwitch False
  (resp0, val0) <- warmup2 ctx inp0 ui
  assert failed (not val0)

  let Rect rx ry rw rh = respRect resp0
      pos = V2 (rx + rw / 2) (ry + rh / 2)
      (press, release) = clickPair inp0 pos

  _ <- runFrame ctx press ui
  ((respToggled, val1), _, _, _) <- runFrame ctx release ui
  assert failed val1
  assert failed (respClicked respToggled)

  -- Click again to toggle back to False
  _ <- runFrame ctx press ui
  ((respToggled2, val2), _, _, _) <- runFrame ctx release ui
  assert failed (not val2)
  assert failed (respClicked respToggled2)

-- | Verifies circular progress ring and sparkline widgets render properly.
runReferenceProgressAndSparklineTest :: Context -> IORef Int -> IO ()
runReferenceProgressAndSparklineTest ctx failed = do
  let inp0 = withInput 300 300
      ui = column $ do
        void $ circularProgress 0.65
        void $ sparkline [10, 40, 25, 80, 55, 95]
  (_, _, draw, _) <- runFrame ctx inp0 ui
  assert failed (drawCmdCount draw > 0)
  assert failed (drawIndexCount draw >= 12)

-- | Verifies the composable drag-and-drop hook: hover, file, text, and bounds.
runDropTargetTest :: Context -> IORef Int -> IO ()
runDropTargetTest ctx failed = do
  let inp0 = withInput 300 300
      bounds = Rect 10 10 100 100
      dropPoint = V2 60 60
      ui = column (useDrop bounds)
      dropsInp ds = inp0 {inputDrops = V.fromList ds}
  _ <- warmup2 ctx inp0 ui

  let beginInp = dropsInp [DropEvent DropBegin Nothing ""]
  (tgtBegin, _, _, _) <- runFrame ctx beginInp ui
  assert failed (not (dropHovered tgtBegin))

  let hoverInp = dropsInp [DropEvent DropPosition (Just dropPoint) ""]
  (tgtHover, _, _, _) <- runFrame ctx hoverInp ui
  assert failed (dropHovered tgtHover)
  assert failed (dropPosition tgtHover == Just dropPoint)

  -- Payload coordinates are ignored; attribution follows the last drag position.
  let dropInp =
        dropsInp
          [ DropEvent DropFile (Just dropPoint) "/tmp/a.txt"
          , DropEvent DropText (Just dropPoint) "hello"
          ]
  (tgtDrop, _, _, _) <- runFrame ctx dropInp ui
  assert failed (dropReceived tgtDrop)
  assert failed (dropFiles tgtDrop == ["/tmp/a.txt"])
  assert failed (dropTexts tgtDrop == ["hello"])

  -- A drop whose coordinates SDL reports as (0,0) (no final position seen)
  -- still lands on the target the pointer was hovering.
  let originDrop =
        dropsInp
          [ DropEvent DropFile (Just (V2 0 0)) "/tmp/origin.txt"
          , DropEvent DropComplete Nothing ""
          ]
  (tgtOrigin, _, _, _) <- runFrame ctx originDrop ui
  assert failed (dropFiles tgtOrigin == ["/tmp/origin.txt"])

  -- The completing drop clears hover state.
  (tgtDone, _, _, _) <- runFrame ctx inp0 ui
  assert failed (not (dropHovered tgtDone))

  -- A fresh drag that moves outside the target no longer delivers to it.
  _ <- runFrame ctx (dropsInp [DropEvent DropBegin Nothing ""]) ui
  _ <- runFrame ctx (dropsInp [DropEvent DropPosition (Just (V2 250 250)) ""]) ui
  let outInp =
        dropsInp
          [ DropEvent DropFile (Just (V2 250 250)) "/tmp/out.txt"
          , DropEvent DropComplete Nothing ""
          ]
  (tgtOut, _, _, _) <- runFrame ctx outInp ui
  assert failed (not (dropReceived tgtOut))
  assert failed (null (dropFiles tgtOut))
