module Main (main) where

import Control.Monad (forM_, when)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Cases
import NanoUI.Testing (Context, newContext, newPixelContext)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

data TestSpec
  = TestSpec
      { specName :: String
      , specSdl :: Bool
      , specRun :: Context -> IORef Int -> IO ()
      }

main :: IO ()
main = do
  args <- getArgs
  let
    wantAll = null args
    want name = wantAll || name `elem` args
  failed <- newIORef (0 :: Int)
  failedTests <- newIORef (0 :: Int)
  forM_ testSpecs $ \TestSpec{specName = name, specSdl, specRun} ->
    when (want name) $ do
      putStrLn ("RUN: " ++ name)
      hFlush stdout
      before <- readIORef failed
      ctx <- if specSdl then newPixelContext else newContext
      specRun ctx failed
      after <- readIORef failed
      when (after > before) $ do
        modifyIORef' failedTests (+ 1)
        putStrLn ("FAIL: " ++ name)
  n <- readIORef failedTests
  if n == 0
    then putStrLn "All tests passed."
    else do
      putStrLn $ show n ++ " test(s) failed."
      fail "tests failed"

testSpecs :: [TestSpec]
testSpecs =
  [ TestSpec "id-stability" False runIdStabilityTest
  , TestSpec "id-uniqueness" False runIdUniquenessTest
  , TestSpec "id-zero-alloc" False runIdZeroAllocTest
  , TestSpec "id-keyed-list" False runIdKeyedListTest
  , TestSpec "fit-sizing" False runFitSizingTest
  , TestSpec "with-key" False runWithKeyTest
  , TestSpec "layout" False runLayoutTest
  , TestSpec "row-panel-layout" False runRowPanelLayoutTest
  , TestSpec "draw" False runDrawTest
  , TestSpec "overlay" False runOverlayTest
  , TestSpec "interaction" False runInteractionTest
  , TestSpec "hover" False runHoverTest
  , TestSpec "pointer-cursor" False runPointerCursorTest
  , TestSpec "pointer-cursor-checkbox" False runPointerCursorCheckboxTest
  , TestSpec "text-input-cursor" False runTextInputCursorTest
  , TestSpec "text-input-selection" False runTextInputSelectionTest
  , TestSpec "text-input-ctrl-a" False runTextInputCtrlATest
  , TestSpec "text-input-mouse-selection" False runTextInputMouseSelectionTest
  , TestSpec "text-input-click-select" False runTextInputClickSelectTest
  , TestSpec "modal-overlay" False runModalOverlayTest
  , TestSpec "modal-no-phantom-scroll" False runModalNoPhantomScrollTest
  , TestSpec "image" False runImageTest
  , TestSpec "text-input-clipboard" False runTextInputClipboardTest
  , TestSpec "text-input-menu" False runTextInputMenuTest
  , TestSpec "select-dropdown-cursor" False runSelectDropdownCursorTest
  , TestSpec "slider-cursor" True runSliderCursorTest
  , TestSpec "scroll-thumb-cursor" False runScrollThumbCursorTest
  , TestSpec "text-input-span" True runTextInputSpanTest
  , TestSpec "text-input-focus-sdl" True runTextInputFocusSdlTest
  , TestSpec "button-hover-anim" False runButtonHoverAnimTest
  , TestSpec "button-press-release-hover" False runButtonPressReleaseHoverTest
  , TestSpec "text-input-focus" False runTextInputFocusTest
  , TestSpec "idle" False runIdleTest
  , TestSpec "hover-skip" False runHoverSkipTest
  , TestSpec "hover-damage" False runHoverDamageTest
  , TestSpec "scroll-damage" False runScrollDamageTest
  , TestSpec "damage-bounds-resolution" False runDamageBoundsResolutionTest
  , TestSpec "damage-bounds-monoid" False runDamageBoundsMonoidTest
  , TestSpec "damage-widget-explicit" False runExplicitDamageWidgetTest
  , TestSpec "damage-rect-explicit" False runExplicitDamageRectTest
  , TestSpec "damage-full-explicit" False runExplicitDamageFullTest
  , TestSpec "damage-queue-cleared" False runDamageQueueClearedPerFrameTest
  , TestSpec "damage-state-change" False runStateChangeDamageTest
  , TestSpec "table-scroll" False runTableScrollTest
  , TestSpec "table-first-col" False runTableFirstColWidthTest
  , TestSpec "table-fill-width" False runTableFillWidthTest
  , TestSpec "scroll-top-clip" True runScrollTopClipTest
  , TestSpec "select-overlay-damage" False runSelectOverlayDamageTest
  , TestSpec "text-input-dirty" False runTextInputDirtyTest
  , TestSpec "modal-close-damage" False runModalCloseDamageTest
  , TestSpec "modal-open-damage" False runModalOpenDamageTest
  , TestSpec "window-close-damage" False runWindowCloseDamageTest
  , TestSpec "window-drag-damage" False runWindowDragDamageTest
  , TestSpec "overlay-panel-live" False runOverlayPanelLiveTest
  , TestSpec "animation-idle" False runAnimationIdleTest
  , TestSpec "animation-settle" False runAnimationSettleTest
  , TestSpec "animation-ease" False runAnimationEaseTest
  , TestSpec "animation-hold" False runAnimationHoldTest
  , TestSpec "animation-damage" False runAnimationDamageTest
  , TestSpec "animation-delay" False runAnimationDelayTest
  , TestSpec "animation-stagger" False runAnimationStaggerTest
  , TestSpec "animation-shared-ctx" False runAnimationSharedCtxTest
  , TestSpec "animation-bezier" False runAnimationBezierTest
  , TestSpec "animation-spring" False runAnimationSpringTest
  , TestSpec "animation-spring-retarget" False runAnimationSpringRetargetTest
  , TestSpec "animation-spring-dt" False runAnimationSpringDtTest
  , TestSpec "animation-spring-hold" False runAnimationSpringHoldTest
  , TestSpec "animation-spring-a" False runAnimationSpringATest
  , TestSpec "ascii" False runAsciiTest
  , TestSpec "vt-decode" False runVtTest
  , TestSpec "cells-and-diff" False runCellsTest
  , TestSpec "checkbox-toggle" False runCheckboxTest
  , TestSpec "slider-store" True runSliderTest
  , TestSpec "slider-fill-width" True runSliderFillWidthTest
  , TestSpec "scroll-wheel" False runScrollTest
  , TestSpec "nested-scroll" False runNestedScrollTest
  , TestSpec "nested-scroll-focus" False runNestedScrollFocusTest
  , TestSpec "scroll-hover-clip" False runScrollHoverClipTest
  , TestSpec "scroll-hit-offset" False runScrollHitOffsetTest
  , TestSpec "scroll-button-click" False runScrollButtonClickTest
  , TestSpec "scroll-scrolled-out-click" False runScrolledOutClickImmunityTest
  , TestSpec "scroll-scrolled-out-hover" False runScrolledOutHoverImmunityTest
  , TestSpec "scroll-scrolled-out-cursor" False runScrolledOutCursorImmunityTest
  , TestSpec "scroll-localized-damage" False runLocalizedScrollDamageTest
  , TestSpec "scroll-child-damage-offset" False runScrollChildDamageOffsetTest
  , TestSpec "scroll-2d-wheel" False run2DScrollWheelTest
  , TestSpec "table-2d-scroll-sync" False runTable2DScrollSyncTest
  , TestSpec "tab-focus" False runTabFocusTest
  , TestSpec "select-initial" False runSelectTest
  , TestSpec "select-dropdown" False runSelectDropdownTest
  , TestSpec "select-dropdown-hover" False runSelectDropdownHoverTest
  , TestSpec "select-drop-flush" True runSelectDropFlushTest
  , TestSpec "select-pick-low" False runSelectPickLowTest
  , TestSpec "select-keyboard" False runSelectKeyboardTest
  , TestSpec "tree-initial" False runTreeInitialTest
  , TestSpec "tree-select" False runTreeSelectTest
  , TestSpec "tree-expand-damage" False runTreeExpandDamageTest
  , TestSpec "tree-keyboard" False runTreeKeyboardTest
  , TestSpec "text-wrap" False runTextWrapTest
  , TestSpec "text-wrap-width" False runTextWrapAssignedTest
  , TestSpec "text-multiline" False runTextMultilineTest
  , TestSpec "flex-wrap" False runFlexWrapTest
  , TestSpec "flex-shrink" False runFlexShrinkTest
  , TestSpec "grow-fits-window" False runGrowFitsWindowTest
  , TestSpec "percent-layout" False runPercentLayoutTest
  , TestSpec "aspect-layout" False runAspectLayoutTest
  , TestSpec "label-align-end" False runLabelAlignEndTest
  , TestSpec "grow-wrap-sibling" False runGrowWrapPushesSiblingTest
  , TestSpec "controls-tab-height" True runControlsTabHeightTest
  , TestSpec "scroll-grow-click" True runScrollButtonClickSdlTest
  , TestSpec "terminal-default-gap" False runTerminalDefaultGapTest
  , TestSpec "terminal-slider-track" False runTerminalSliderTrackTest
  , TestSpec "terminal-text-input" False runTerminalTextInputDisplayTest
  , TestSpec "terminal-modal-overlay" False runTerminalModalOverlayTest
  , TestSpec "terminal-modal-scroll" False runTerminalModalScrollTest
  , TestSpec "terminal-modal-tight" False runTerminalModalTightTest
  , TestSpec "terminal-modal-open-redraw" False runTerminalModalOpenRedrawTest
  , TestSpec "terminal-window-overlay" False runTerminalWindowOverlayTest
  , TestSpec "terminal-window-drag" False runTerminalWindowDragTest
  , TestSpec "terminal-window-drag-icons" False runTerminalWindowDragIconTest
  , TestSpec "terminal-close-button" False runTerminalCloseButtonTest
  , TestSpec "icon-set" False runIconSetTest
  , TestSpec "terminal-icon-chrome" False runTerminalIconChromeTest
  , TestSpec "terminal-icon-close" False runTerminalIconCloseTest
  , TestSpec "terminal-button-brackets" False runTerminalButtonBracketTest
  , TestSpec "terminal-wide-clear-bracket" False runTerminalWideClearBracketTest
  , TestSpec "terminal-wide-cursor-cup" False runTerminalWideCursorCupTest
  , TestSpec "terminal-wide-transitions" False runTerminalWideTransitionTest
  , TestSpec "terminal-wide-pairs" False runTerminalWidePairTest
  , TestSpec "terminal-theme-contrast" False runTerminalThemeContrastTest
  , TestSpec "scroll-bar-gutter" False runScrollBarGutterTest
  , TestSpec "scroll-bar-gutter-grow" True runGrowScrollGutterTest
  , TestSpec "column-card-wrap" True runColumnCardWrapTest
  , TestSpec "two-card-wrap" True runTwoCardWrapTest
  , TestSpec "demo-wrap-wide-order" True runDemoWrapWideOrderTest
  , TestSpec "scroll-bar-gutter-panel" True runPanelGrowScrollGutterTest
  , TestSpec "window-scroll-gutter" True runWindowScrollGutterTest
  , TestSpec "use-flag-click" False runUseFlagClickTest
  , TestSpec "tabs-laziness" False runTabsLazinessTest
  , TestSpec "tabs-interaction" False runTabsInteractionTest
  , TestSpec "tabs-emit" False runTabsEmitTest
  , TestSpec "tabs-closable" False runTabsClosableTest
  , TestSpec "tabs-state-persistence" False runTabsStatePersistenceTest
  , TestSpec "tabs-damage" False runTabsDamageTest
  , TestSpec "tabs-content-damage" False runTabsContentDamageTest
  , TestSpec "host-slot" False runHostSlotTest
  , TestSpec "host-profile-gap" False runHostProfileGapTest
  , TestSpec "host-profile-measure" False runHostProfileMeasureTest
  , TestSpec "compact-host" False runCompactHostTest
  , TestSpec "embed-state" False runEmbedStateTest
  , TestSpec "reduce-messages" False runReduceMessagesTest
  , TestSpec "reduce-updates" False runReduceUpdatesTest
  , TestSpec "reduce-click" False runReduceClickTest
  , TestSpec "reduce-identity" False runReduceIdentityTest
  , TestSpec "widget-no-string-emit" False runWidgetNoStringEmitTest
  , TestSpec "panel-paints" False runPanelPaintsTest
  , TestSpec "separator-span" False runSeparatorSpanTest
  , TestSpec "terminal-separator-span" False runTerminalSeparatorSpanTest
  , TestSpec "header-top-pad" False runHeaderTopPadTest
  , TestSpec "fit-header-no-shrink" False runFitHeaderNoShrinkTest
  , TestSpec "window-overlay" False runWindowOverlayTest
  , TestSpec "overlay-click-through" False runOverlayClickThroughTest
  , TestSpec "window-drag" False runWindowDragTest
  , TestSpec "window-scroll-wheel" False runWindowScrollWheelTest
  , TestSpec "page-window-scroll" False runPageWindowScrollTest
  , TestSpec "sibling-window-scroll" False runSiblingWindowScrollTest
  , TestSpec "window-scroll-only-damage" False runWindowScrollOnlyDamageTest
  , TestSpec "scrolled-debug-toggle" False runScrolledDebugToggleTest
  , TestSpec "window-resize" False runWindowResizeTest
  , TestSpec "window-resize-halo-hit" False runWindowResizeHaloHitTest
  , TestSpec "context-menu-open" False runContextMenuOpenTest
  , TestSpec "context-menu-dismiss" False runContextMenuDismissTest
  , TestSpec "context-menu-right-dismiss" False runContextMenuRightDismissTest
  , TestSpec "context-menu-pick" False runContextMenuPickTest
  , TestSpec "context-menu-area" False runContextMenuAreaTest
  , TestSpec "context-menu-spans" False runContextMenuSpansTest
  , TestSpec "context-menu-scroll-pos" False runContextMenuScrollPosTest
  , TestSpec "tooltip-hover" False runTooltipHoverTest
  , TestSpec "tooltip-widget" False runTooltipWidgetTest
  , TestSpec "tooltip-spans" False runTooltipSpansTest
  , TestSpec "tooltip-id-stable" False runTooltipIdStableTest
  , TestSpec "tooltip-scroll-pos" False runTooltipScrollPosTest
  ]
