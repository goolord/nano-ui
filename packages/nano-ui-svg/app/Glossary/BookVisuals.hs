-- | SVGs for the documentation book: the layout chapter's examples, the
-- chart and canvas examples, and the widget gallery.
module Glossary.BookVisuals
  ( generateBookVisuals
  ) where

import CanvasExamples (canvasExamples)
import ChartExamples (chartExamples)
import Control.Monad (forM_, void)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Glossary.Render (cropSvg, exportFrame, frameInkBounds, newSvgContext, renderSvg)
import LayoutExamples (layoutExamples)
import NanoUI
import NanoUI.Plot (PlotCache, newPlotCache)
import NanoUI.Testing (runFrame)
import NanoUI.Testing.Harness (withInputOff)
import System.Directory (createDirectoryIfMissing)

-- ---------------------------------------------------------------------------
-- Book visuals
-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- Layout chapter examples
-- ---------------------------------------------------------------------------

-- | One layout example: a padded frame whose root column fills the canvas.
layoutDemo :: NanoUI () -> NanoUI ()
layoutDemo = columnWith (fillW . fillH . padAll 14 . gap 10)

-- | Render one layout example, cropped to its painted height. @hoverPos@
-- parks the pointer on a control for the hover and pointer-mode examples.
layoutSvg :: Text -> Float -> Float -> Maybe V2 -> NanoUI () -> IO Text
layoutSvg tag w h hoverPos ui = do
  (svg, bounds) <- settledFrame 1 tag w h hoverPos ui
  pure (cropSvg w h bounds svg)

-- | The uncropped SVG of a view after @n@ warm-up frames, and the bounds of
-- what it painted. A chart sizes itself from the envelope of the drawing it
-- made the frame before, so it takes more than one frame to settle.
settledFrame :: Int -> Text -> Float -> Float -> Maybe V2 -> NanoUI () -> IO (Text, Maybe Rect)
settledFrame n tag w h hoverPos ui = do
  ctx <- newSvgContext
  let base = withInputOff w h
      capture = case hoverPos of
        Nothing -> base
        Just pos -> base {inputMousePos = pos}
      warm = replicate n base <> maybe [] (const [capture]) hoverPos
  mapM_ (\inp -> runFrame ctx inp ui) warm
  (svg, dd) <- exportFrame tag ctx capture ui
  bounds <- frameInkBounds ctx capture dd
  pure (svg, bounds)

-- | A chart example narrowed to what it draws. A chart's width comes from
-- its drawing's proportions, so the view is rendered once at the widest
-- width @w@ to find its right edge, then again at that edge plus the 12 px
-- padding. Each render gets a fresh 'PlotCache', as an application
-- allocates one during setup.
chartSvg :: Text -> Float -> Maybe V2 -> (PlotCache -> NanoUI ()) -> IO Text
chartSvg tag w pointerPos ui = do
  (_, ink) <- render w
  let fittedW = maybe w (\(Rect bx _ bw _) -> min w (fromIntegral (ceiling (bx + bw) :: Int) + 12)) ink
  (svg, ink') <- render fittedW
  pure (cropSvg fittedW h ink' svg)
  where
    h = 1200
    render width = do
      cache <- newPlotCache
      settledFrame 3 tag width h pointerPos (ui cache)


-- | Controls in the book's widget gallery.
galleryControlsSpecimen :: NanoUI ()
galleryControlsSpecimen = do
  enabled <- useVar True
  boldVar <- useVar False
  query <- useVar ""
  priority <- useVar (1 :: Int)
  volume <- useVar (62 :: Float)
  columnWith (fillW . padAll 18 . gap 14) $ do
    heading "Controls"
    rowWith (tight . gap 10 . fillW . wrap . alignMiddle) $ do
      void (button "Primary")
      void (button "Secondary")
      toggleButtonWith (fixedW 90) Accent "Bold" boldVar
      checkbox "Enabled" enabled
    rowWith (tight . gap 14 . fillW . wrap . alignMiddle) $ do
      textInputConfigured
        defaultTextInputConfig {ticPlaceholder = "Project name", ticLayout = fixedW 210}
        query
      selectWith (fixedW 150) ["Low", "Medium", "High"] priority
      sliderWith (fixedW 190) 0 100 volume
    rowWith (tight . gap 12 . fillW) $ do
      panelWith (fillW . padAll 12 . gap 10) $ do
        muted "CUSTOM CONTROLS"
        rowWith (tight . gap 16 . alignMiddle) $ do
          toggleSwitch enabled
          knob 0 100 volume
          circularProgress 0.62
      panelWith (fillW . padAll 12 . gap 10) $ do
        muted "TELEMETRY"
        progressBar 0.62
        sparkline [18, 22, 19, 27, 24, 31, 28, 35, 33, 41, 38, 46]
        muted "12 samples, last 46"

-- | Tabs, a table, and a tree in one frame.
galleryDataSpecimen :: NanoUI ()
galleryDataSpecimen = do
  page <- useVar (0 :: Int)
  selected <- useVar (0 :: Int)
  columnWith (fillW . fillH . padAll 18 . gap 12) $ do
    heading "Collections"
    tabs
      page
      [ tab 0 "Overview" (label "Deployment health across three regions.")
      , tab 1 "Queries" (label "Slow query log for the last hour.")
      , tab 2 "Settings" (label "Retention and sampling policy.")
      ]
    rowWith (tight . gap 12 . fillW . fillH) $ do
      panelWith (fillW . fillH . padAll 12 . gap 8) $ do
        _ <-
          simpleTable
            ["Service", "Status", "Latency"]
            [ ["api", "running", "12 ms"]
            , ["worker", "running", "48 ms"]
            , ["scheduler", "idle", "2 ms"]
            ]
        pure ()
      panelWith (fixedW 230 . fillH . padAll 12 . gap 8) $ do
        muted "TREE"
        tree
          "book-tree"
          [ TreeItem "src" [TreeItem "Main.hs" [], TreeItem "Widgets" []]
          , TreeItem "README.md" []
          ]
          selected

-- | Every tab header style, one per row.
tabStylesSpecimen :: NanoUI ()
tabStylesSpecimen =
  columnWith (fillW . padAll 18 . gap 12) $ do
    heading "Tab styles"
    tabStyleRow "Underline" 0 TabUnderline
    tabStyleRow "Pill" 1 TabPill
    tabStyleRow "Segmented" 2 TabSegmented
    tabStyleRow "Contained" 1 TabContained
  where
    tabStyleRow :: Text -> Int -> TabStyle -> NanoUI ()
    tabStyleRow caption initial style = do
      active <- useVar initial
      labelWith fontMuted caption
      tabsConfigured defaultTabsConfig {tabsStyle = style} active
        [ tab 0 "Overview" (muted "Overview")
        , tab 1 "Activity" (muted "Activity")
        , tab 2 "Settings" (muted "Settings")
        ]

-- | Write the layout, canvas, chart and gallery specimens to the book's images directory.
generateBookVisuals :: IO ()
generateBookVisuals = do
  createDirectoryIfMissing True "book/src/images"
  createDirectoryIfMissing True "book/src/images/layout"
  forM_ layoutExamples $ \(name, w, h, hoverPos, ui) -> do
    svg <- layoutSvg ("book-layout-" <> name) w h hoverPos (layoutDemo ui)
    let path = "book/src/images/layout/" <> T.unpack name <> ".svg"
    TIO.writeFile path svg
    putStrLn ("wrote " <> path)
  createDirectoryIfMissing True "book/src/images/canvas"
  forM_ canvasExamples $ \(name, w, ui) -> do
    svg <- layoutSvg ("book-canvas-" <> name) w 600 Nothing ui
    let path = "book/src/images/canvas/" <> T.unpack name <> ".svg"
    TIO.writeFile path svg
    putStrLn ("wrote " <> path)
  createDirectoryIfMissing True "book/src/images/charts"
  forM_ chartExamples $ \(name, w, pointerPos, ui) -> do
    svg <- chartSvg ("book-chart-" <> name) w pointerPos ui
    let path = "book/src/images/charts/" <> T.unpack name <> ".svg"
    TIO.writeFile path svg
    putStrLn ("wrote " <> path)
  writeSvg "gallery-controls" 880 260 galleryControlsSpecimen
  writeSvg "gallery-data" 880 420 galleryDataSpecimen
  writeSvg "tab-styles" 880 500 tabStylesSpecimen
  where
    writeSvg name w h ui = do
      svg <- renderSvg ("book-" <> name) w h ui
      TIO.writeFile ("book/src/images/" <> T.unpack name <> ".svg") svg
      putStrLn ("wrote book/src/images/" <> T.unpack name <> ".svg")
