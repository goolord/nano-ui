-- | Layout: how to arrange widgets, and how to see why a layout looks wrong.
--
-- Every widget and container takes a layout modifier, a function
-- @Layout -> Layout@. Modifiers compose with @(.)@ and apply right to left,
-- so the leftmost wins: @padAll 8 . tight@ is padded, @tight . padAll 8@
-- is not.
--
-- A container lays out its children along one axis: 'row' left to right,
-- 'column' top to bottom. Each child sizes itself: by default it fits its
-- content ('Fit'), and 'fixedW', 'fillW' and friends change that. A child
-- also places itself, by its own alignment, across its parent's axis.
--
-- The page below is one long scroller of captioned sections. Turn on
-- "Explain layout" at the top to outline every layout node, coloured by
-- depth; the node under the pointer is tinted with its content box
-- outlined. It changes nothing else, so it is safe to leave in a debug menu.
--
-- See Components.hs for passing a modifier through your own widgets.
-- Escape quits.
--
-- Run it with @cabal run nano-ui-example-layout@.
module Main (main) where

import Control.Monad (forM_, void)
import qualified Data.Text as T
import NanoUI
import NanoUI.Backend.Sdl (SdlOptions (..), defaultSdlOptions, runSdlApp)

main :: IO ()
main =
  runSdlApp
    defaultSdlOptions
      { sdlAppShouldQuit = pressedOnceIn KeyEscape
      , sdlWindowSettings = defaultWindowSettings {wsTitle = "Layout", wsSize = Size 760 820}
      }
    view

view :: NanoUI ()
view =
  -- The page scroller fills the window ('grow'); its content is as tall as
  -- it needs to be, and the scroller shows a bar when that is taller.
  scrollWith (padAll 12 . grow) $
    columnWith (tight . gap 20 . fillW) $ do
      toolbar $ do
        labelWith (alignMid . fontSize 22 . fontMedium) "Layout"
        flex
        explainLayout =<< checkbox "Explain layout" =<< explainingLayout

      section "Rows, columns, gap and padding" "gap spaces the children; padAll pads inside the container's edge." $ do
        rowWith (gap 4) (mapM_ chip ["gap 4", "B", "C"])
        rowWith (gap 24) (mapM_ chip ["gap 24", "B", "C"])
        panelWith (padAll 24 . gap 8) (mapM_ chip ["padAll 24", "B"])

      section "Sizing" "Fit by default; fixedW is exact; fillW children share what is left; minW and maxW bound any of them. grow is fillW . fillH." $ do
        rowWith (tight . gap 8 . fillW) $ do
          btn (fixedW 120) "fixedW 120"
          btn fillW "fillW"
          btn fillW "fillW"
        rowWith (tight . gap 8 . fillW) $ do
          btn (fillW . maxW 200) "fillW . maxW 200"
          btn (minW 160) "minW 160"
          btn id "Fit"
        -- Every container starts with 3 pixels of padding, so each nesting
        -- level indents its content a little more. 'tight' drops a
        -- container's padding and keeps its gap: use it on rows and columns
        -- that only arrange things, and pad the panel around them instead.
        rowWith (tight . gap 8) $ do
          panel . column . column $ chip "Nested, default padding"
          panel . columnWith tight . columnWith tight $ chip "Nested, tight"

      section "Pushing things apart" "flex takes the room left in a row, so the row must be wider than its content: give it fillW, as toolbar does." $ do
        toolbar $ do
          labelWith alignMid "report.pdf"
          flex
          btn id "Share"
          btn id "Download"
        -- Two flexes split the room, centring what sits between them.
        toolbar $ do
          btn id "Back"
          flex
          labelWith alignMid "Page 2 of 5"
          flex
          btn id "Next"

      section "Centring" "A child aligns itself: alignCenter across a column, alignMid down a row. Layers place a child by its alignment on both axes." $ do
        columnWith (tight . fillW) $ btn alignCenter "alignCenter in a column"
        rowWith (tight . gap 8 . fillW . fixedH 56) $ do
          btn alignTop "alignTop"
          btn alignMid "alignMid"
          btn alignBottom "alignBottom"
        panelWith (layered . fillW . fixedH 80) $
          labelWith (alignCenter . alignMid) "Centred both ways in a layered panel"

      section "Wrapping" "wrap starts a new line when the next child would overflow. Narrow the window to see it." $
        rowWith (wrap . tight . gap 6 . fillW) $
          mapM_ chip ["haskell", "gui", "immediate-mode", "layout", "sdl", "text", "widgets", "themes", "scrolling", "animation"]

      section "Grids" "gridWith n places children in n equal columns, filling each row left to right. A fillW child fills its cell." $
        gridWith 3 (tight . gap 8 . fillW) $
          forM_ [1 .. 7 :: Int] $ \i -> btn fillW ("Cell " <> T.pack (show i))

      section "Responsive" "responsiveRowCol is a row while the window is at least that wide and a column below it. Make the window narrower than 640 to see it switch." $
        responsiveRowCol 640 (tight . gap 8 . fillW) $ do
          panelWith (fillW . padAll 12) (label "Sidebar")
          panelWith (fillW . padAll 12) (label "Content")

      section "Badges" "In layers the badge is an ordinary child, inside the box. pinAt takes it out of the flow, so it neither sizes its parent nor stays inside it." $
        rowWith (tight . gap 32 . alignMid) $ do
          layersWith tight $ do
            btn (fixedWH 110 40) "Inbox"
            badge (alignEnd . alignTop) "3"
          columnWith tight $ do
            btn (fixedWH 110 40) "Alerts"
            -- Anchored at the top-right corner, then moved 8 right and 8 up.
            badge (pinAt 8 (-8) . alignEnd . alignTop) "12"

      section "A fixed-height scroller" "A scroller needs a bounded size on its axis (fixedH, maxH, or grow in a bounded parent). Otherwise it grows to fit its content and never scrolls." $
        scrollWith (fixedH 140 . fillW) $
          columnWith (tight . gap 4 . fillW) $
            forM_ [1 .. 30 :: Int] $ \i -> label ("Line " <> T.pack (show i))

-- | A heading, a note, and the example in a panel below them.
section :: T.Text -> T.Text -> NanoUI () -> NanoUI ()
section title note body =
  columnWith (tight . gap 6 . fillW) $ do
    heading title
    muted note
    panelWith (padAll 12 . gap 8 . fillW) body

-- | A button whose click these examples ignore.
btn :: LayoutModifier -> T.Text -> NanoUI ()
btn f = void . buttonWith f

-- | A small labelled box.
chip :: T.Text -> NanoUI ()
chip = panelWith (padXY 8 3) . label

badge :: LayoutModifier -> T.Text -> NanoUI ()
badge f txt =
  styled (panelStyle (background badgeRed . borderColor badgeRed)) $
    panelWith (f . padXY 6 1) (labelWith (fontSize 11 . fontMedium . fontColor colorWhite) txt)

badgeRed :: Color
badgeRed = colorRGB 204 64 64
