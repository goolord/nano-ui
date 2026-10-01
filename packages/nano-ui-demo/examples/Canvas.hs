-- | Canvas: a tiny paint program with freehand strokes, colours, undo and clear.
--
-- A canvas is a widget whose drawing you write with 'CanvasM': here, each
-- stroke is a "NanoUI.Path" polyline stroked with round caps and joins. The
-- canvas returns a 'Response' like any widget, and 'useDrag2DOn' turns that
-- into a drag: it starts only with a press on the canvas itself, lasts until
-- the button is released wherever the pointer goes, and reports the pointer
-- clamped to the canvas. Points are stored relative to the canvas corner, so
-- the drawing stays put if the layout moves it.
--
-- The finished strokes live in one 'StateCell' and the stroke being drawn in
-- another, both allocated once in setup. A press starts the live stroke,
-- each move adds a point, and the release moves it onto the finished list.
--
-- 'canvasContent' is the drawing's content key: while it stays the same the
-- frame reuses the last drawing instead of building its paths again, so it
-- must change whenever anything the drawing reads changes. A revision number
-- bumped on every edit (and the live stroke's length) covers that cheaply,
-- without hashing every point. The whole canvas redraws while you draw; a
-- very large picture could split the finished strokes and the live one into
-- two canvases with their own keys.
--
-- Ctrl+Z undoes, like the Undo button, through 'shortcut'.
--
-- Run it with @cabal run nano-ui-example-canvas@.
module Main (main) where

import Control.Monad (forM_, unless, when)
import NanoUI
import NanoUI.Backend.Sdl (SdlOptions (..), defaultSdlOptions, runSdlApp)
import qualified NanoUI.Path as P
import NanoUI.Shortcut (ctrl, key)

main :: IO ()
main = do
  st <- newPaintState
  runSdlApp
    defaultSdlOptions
      { sdlAppShouldQuit = pressedOnceIn KeyEscape
      , sdlWindowSettings = defaultWindowSettings {wsTitle = "Canvas", wsSize = Size 760 600}
      }
    (view st)

-- | One stroke: its colour, width, and points relative to the canvas
-- corner, newest first (adding a point is a cons).
data Ink = Ink
  { inkColor :: !Color
  , inkWidth :: !Float
  , inkPoints :: ![V2]
  }
  deriving (Eq)

-- | The finished strokes, newest first, and a revision counted up on every
-- change, for the content key.
data Sketch = Sketch
  { sketchStrokes :: ![Ink]
  , sketchRevision :: !Int
  }
  deriving (Eq)

data PaintState = PaintState
  { sketchCell :: !(StateCell Sketch)
  , liveCell :: !(StateCell (Maybe Ink))
  }

newPaintState :: IO PaintState
newPaintState = PaintState <$> newState (Sketch [] 0) <*> newState Nothing

-- | Apply an edit to the stroke list and bump the revision.
edit :: ([Ink] -> [Ink]) -> Sketch -> Sketch
edit f (Sketch strokes rev) = Sketch (f strokes) (rev + 1)

palette :: [Color]
palette =
  [ colorRGB 30 30 34
  , colorRGB 214 51 52
  , colorRGB 236 136 32
  , colorRGB 40 150 80
  , colorRGB 40 110 220
  , colorRGB 140 70 200
  ]

paper :: Color
paper = colorRGB 250 248 242

view :: PaintState -> NanoUI ()
view st = do
  (sketch, _) <- useState (sketchCell st)
  (live, setLive) <- useState (liveCell st)
  (colourIx, setColourIx) <- useInt 0
  (width, setWidth) <- useFloat 6
  theme <- uiTheme
  let strokes = sketchStrokes sketch
      undo = unless (null strokes) (modifyState (sketchCell st) (edit (drop 1)))
      colour = palette !! colourIx
  columnWith (padAll 16 . gap 12 . grow) $ do
    rowWith (tight . gap 8 . alignMid . fillW) $ do
      -- Swatches are ordinary buttons filled with their colour; the chosen
      -- one gets a border in the theme's text colour.
      forM_ (zip [0 ..] palette) $ \(i, c) -> do
        let ring = if i == colourIx then buttonStyle (borderWidth 3 . borderColor (styleFg (themePanel theme))) else id
        whenM (styled (ring . tinted (const c)) (buttonWith (fixedWH 28 28) "")) (setColourIx i)
      labelWith (tight . alignMid . padLeft 12) "Width"
      setWidth =<< sliderWith (fixedW 140) 1 30 width
      flex
      disabledWhen (null strokes) $ whenM (button "Undo") undo
      disabledWhen (null strokes) $
        whenM (button "Clear") (modifyState (sketchCell st) (edit (const [])))
    whenM (shortcut (ctrl <> key 'z')) undo

    -- Everything the drawing reads: the finished strokes (by revision) and
    -- the live stroke's shape. The paper colour is a constant.
    let liveKey = fmap (\s -> (colorToWord32 (inkColor s), inkWidth s, length (inkPoints s))) live
        cfg =
          defaultCanvasConfig
            { canvasLayout = (fillW . grow) defaultLayout
            , canvasContent = contentKeyOf [keyPart (sketchRevision sketch), keyPart liveKey]
            }
    resp <-
      withCursorShape UiCursorCrosshair $
        canvasConfigured cfg $ \r@(Rect x y _ _) -> do
          drawRect r paper
          withClip r $ forM_ (reverse strokes ++ maybe [] pure live) (drawInk (V2 x y))

    drag <- useDrag2DOn resp
    let Rect rx ry _ _ = respRect resp
        here = v2Sub (dragPosition drag) (V2 rx ry)
    case (dragActive drag, live) of
      (True, Nothing) -> setLive (Just (Ink colour width [here]))
      -- Skip moves under a pixel: they add points without changing the line.
      (True, Just s@(Ink _ _ (prev : _))) ->
        when (far prev here) (setLive (Just s {inkPoints = here : inkPoints s}))
      (False, Just s) -> do
        modifyState (sketchCell st) (edit (s :))
        setLive Nothing
      _ -> pure ()

far :: V2 -> V2 -> Bool
far (V2 ax ay) (V2 bx by) = abs (ax - bx) + abs (ay - by) >= 1

-- | Draw a stroke offset by the canvas corner. A click without a move has
-- one point, which a path cannot stroke, so it becomes a dot.
drawInk :: V2 -> Ink -> CanvasM ()
drawInk origin (Ink c w pts) = case map (v2Add origin) (reverse pts) of
  [p] -> drawCircle p (w / 2) c
  ps ->
    drawStrokePathWith
      (P.stroke w) {P.strokeCap = P.RoundCap, P.strokeJoin = P.RoundJoin}
      (P.polyline ps)
      (P.Solid c)
