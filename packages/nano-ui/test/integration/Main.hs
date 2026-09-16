module Main (main) where

import Cases
import Cases.Atlas (runAtlasGrowthTest)
import Cases.Grid
  ( runFontCompositionTest
  , runGridColumnsWithFontColorTest
  , runNestedGridTest
  , runStaleFontColorTest
  )
import Cases.HostDraw (runExternalTextTest, runSquareGeometryTest)
import Cases.PointerRelease
  ( runReleaseElsewhereTest
  , runReleaseReturnsTest
  , runRightReleaseElsewhereTest
  )
import Cases.Runner (runDrawingLockTest, runSessionLoopTest)
import Cases.SIMD (runSimdWritesTest)
import Cases.State
  ( runCollectionApiTest
  , runControlledInputsTest
  , runControlledStateTest
  , runHookStateTest
  )
import Cases.Table
  ( runPageWheelAboveTableTest
  , runTableCellPadTest
  , runTableColResizeDemoReproTest
  , runTableFillWidthTest
  , runTableFirstColWidthTest
  , runTableHBarReachTest
  , runTableReorderTest
  , runTableResizeOverflowTest
  , runTableScrollRevealTest
  , runTableSortTest
  , runTableWrapRowStretchTest
  )
import Data.IORef (IORef)
import NanoUI.Testing (Context, newContext, newPixelContext)
import NanoUI.Testing.Runner (runTests)

data TestSpec
  = TestSpec
  { specName :: String
  , specSdl :: Bool
  , specRun :: Context -> IORef Int -> IO ()
  }

main :: IO ()
main =
  runTests
    [ (specName, if specSdl then newPixelContext else newContext, specRun)
    | TestSpec {specName, specSdl, specRun} <- testSpecs
    ]

testSpecs :: [TestSpec]
testSpecs =
  -- Runner, state and messages
  [ TestSpec "session-loop" False runSessionLoopTest
  , TestSpec "drawing-lock" False runDrawingLockTest
  , TestSpec "simd-writes" False runSimdWritesTest
  , TestSpec "no-thunks" False runNoThunksTest
  , TestSpec "controlled-state" False runControlledStateTest
  , TestSpec "controlled-inputs" False runControlledInputsTest
  , TestSpec "hook-state" False runHookStateTest
  , TestSpec "collection-api" False runCollectionApiTest
  , TestSpec "embed-state" False runEmbedStateTest
  , TestSpec "host-slot" False runHostSlotTest
  , TestSpec "reduce-messages" False runReduceMessagesTest
  , TestSpec "reduce-click" False runReduceClickTest
  , TestSpec "widget-no-string-emit" False runWidgetNoStringEmitTest
  -- Ids, layout and caches
  , TestSpec "id-keyed-list" False runIdKeyedListTest
  , TestSpec "fit-muted-width" False runFitMutedWidthTest
  , TestSpec "fit-header-no-shrink" False runFitHeaderNoShrinkTest
  , TestSpec "layout-reuse" False runLayoutReuseTest
  , TestSpec "metric-cache-invalidation" False runMetricCacheInvalidationTest
  , TestSpec "widget-placement-cache" False runWidgetPlacementCacheTest
  , TestSpec "layout-cache-paint-state" False runLayoutPaintStateTest
  , TestSpec "deep-nesting" False runDeepNestingTest
  , TestSpec "grow-split" False runGrowSplitTest
  , TestSpec "percent-gap-shrink" False runPercentGapShrinkTest
  , TestSpec "aspect-layout" False runAspectLayoutTest
  , TestSpec "label-align-end" False runLabelAlignEndTest
  , TestSpec "responsive-wrap" True runResponsiveWrapTest
  , TestSpec "kv-multiline-height" True runKvMultilineHeightTest
  , TestSpec "separator-span" False runSeparatorSpanTest
  , TestSpec "panel-paints" False runPanelPaintsTest
  , TestSpec "grid-columns-font-color" False runGridColumnsWithFontColorTest
  , TestSpec "grid-nested" False runNestedGridTest
  , TestSpec "stale-font-color" False runStaleFontColorTest
  , TestSpec "font-composition" True runFontCompositionTest
  -- Drawing
  , TestSpec "draw-square-geometry" False runSquareGeometryTest
  , TestSpec "draw-external-text" False runExternalTextTest
  , TestSpec "drawing" False runDrawingTest
  , TestSpec "image" False runImageTest
  , TestSpec "empty-frame" False runEmptyFrameTest
  , TestSpec "image-swap-damage" False runImageSwapDamageTest
  , TestSpec "atlas-growth" False runAtlasGrowthTest
  -- Pointer, redraw and damage
  , TestSpec "pointer-cursor" False runPointerCursorTest
  , TestSpec "hover-damage" False runHoverDamageTest
  , TestSpec "damage-bounds-resolution" False runDamageBoundsResolutionTest
  , TestSpec "damage-widget-explicit" False runExplicitDamageWidgetTest
  , TestSpec "damage-queue-cleared" False runDamageQueueClearedPerFrameTest
  , TestSpec "damage-state-change" False runStateChangeDamageTest
  , TestSpec "damage-orphan-anim-settles" False runOrphanAnimationDamageSettlesTest
  , TestSpec "versioned-drawing-damage" False runVersionedDrawingDamageTest
  , TestSpec "panel-body-swap-damage" False runPanelBodySwapDamageTest
  , TestSpec "refresh-forces-redraw" False runRefreshRedrawTest
  -- Animation
  , TestSpec "animation-settle" False runAnimationSettleTest
  , TestSpec "animation-damage" False runAnimationDamageTest
  , TestSpec "animation-stagger" False runAnimationStaggerTest
  , TestSpec "animation-bezier" False runAnimationBezierTest
  , TestSpec "animation-spring-retarget" False runAnimationSpringRetargetTest
  , TestSpec "animation-spring-dt" False runAnimationSpringDtTest
  , TestSpec
      "composite-animation-isolation"
      False
      runCompositeAnimationIsolationTest
  , TestSpec "button-hover-anim" False runButtonHoverAnimTest
  -- Keyboard
  , TestSpec "keyboard-disabled" False runKeyboardDisabledTest
  , TestSpec "keyboard-modal-eligibility" False runKeyboardModalEligibilityTest
  , TestSpec "keyboard-focus-ring" False runKeyboardFocusRingTest
  , TestSpec "keyboard-button" False runKeyboardButtonTest
  , TestSpec "keyboard-checkbox" False runKeyboardCheckboxTest
  , TestSpec "keyboard-slider" True runKeyboardSliderTest
  , TestSpec "keyboard-radio" False runKeyboardRadioTest
  , TestSpec "keyboard-toggle" False runKeyboardToggleTest
  , TestSpec "keyboard-tab-header" False runKeyboardTabHeaderTest
  -- Controls
  , TestSpec "checkbox-initial" False runCheckboxInitialTest
  , TestSpec "slider-cursor" True runSliderCursorTest
  , TestSpec "slider-fill-width" True runSliderFillWidthTest
  , TestSpec "search-field-clear" False runSearchFieldClearTest
  , TestSpec "search-field-debounce" False runSearchFieldDebounceTest
  , TestSpec "select-drag-to-select" False runSelectDragToSelectTest
  , TestSpec "select-keyboard" False runSelectKeyboardTest
  , TestSpec "select-change-once" False runSelectChangeOnceTest
  , TestSpec "select-close-keeps-focus" False runSelectCloseKeepsFocusTest
  , TestSpec "select-overlay-damage" False runSelectOverlayDamageTest
  , TestSpec "tree-select" False runTreeSelectTest
  , TestSpec "tree-keyboard" False runTreeKeyboardTest
  , TestSpec "combo-filter" False runComboFilterTest
  , TestSpec "combo-keyboard-pick" False runComboKeyboardPickTest
  , TestSpec "combo-mouse-pick" False runComboMousePickTest
  , TestSpec "combo-blur-commit" False runComboBlurCommitTest
  , TestSpec "combo-escape-revert" False runComboEscapeRevertTest
  , TestSpec "combo-hover-highlight" False runComboHoverHighlightTest
  , TestSpec "combo-scrollbar-drag" False runComboScrollbarDragTest
  , TestSpec "combo-wheel-scroll" False runComboWheelScrollTest
  , TestSpec "color-picker-commit" True runColorPickerCommitTest
  , TestSpec "color-picker-rgba" True runColorPickerRgbaTest
  , TestSpec "color-picker-edit" True runColorPickerEditTest
  , TestSpec "color-picker-change-once" True runColorPickerChangeOnceTest
  , TestSpec "color-picker-bar-keys" True runColorPickerBarKeysTest
  , TestSpec "color-picker-drag-after-field" True runColorPickerDragAfterFieldTest
  , TestSpec "controls-tab-height" True runControlsTabHeightTest
  , TestSpec "bounded-radio-offset" True runBoundedRadioTest
  -- Text input and text area
  , TestSpec "text-input-cursor" False runTextInputCursorTest
  , TestSpec "text-input-batch" False runTextInputBatchTest
  , TestSpec "text-input-selection" False runTextInputSelectionTest
  , TestSpec "text-input-mouse-selection" False runTextInputMouseSelectionTest
  , TestSpec "text-input-click-select" False runTextInputClickSelectTest
  , TestSpec "text-input-word-keys" False runTextInputWordKeysTest
  , TestSpec
      "text-input-cut-clears-selection"
      False
      runTextInputCutClearsSelectionTest
  , TestSpec "text-input-clipboard" False runTextInputClipboardTest
  , TestSpec "text-input-password" False runTextInputPasswordTest
  , TestSpec "numeric-input" False runNumericInputTest
  , TestSpec "numeric-input-hex" False runNumericInputHexTest
  , TestSpec "text-input-menu" False runTextInputMenuTest
  , TestSpec "text-input-ff-caret" False runTextInputFfCaretTest
  , TestSpec "text-input-focus-sdl" True runTextInputFocusSdlTest
  , TestSpec "text-input-scroll" True runTextInputScrollTest
  , TestSpec "text-input-dirty" False runTextInputDirtyTest
  , TestSpec
      "text-area-cut-clears-selection"
      False
      runTextAreaCutClearsSelectionTest
  , TestSpec "text-area-scroll-wheel" True runTextAreaScrollWheelTest
  , TestSpec "text-area-zoom-scroll" True runTextAreaZoomScrollTest
  , TestSpec "text-area-remount-scroll" True runTextAreaRemountScrollTest
  , TestSpec "text-area-menu-pulse" True runTextAreaMenuPulseTest
  , TestSpec "text-area-scroll-drag" True runTextAreaScrollDragTest
  , TestSpec "text-area-cursor-on-scrollbar" True runTextAreaCursorOnScrollBarTest
  , TestSpec "text-area-hscroll-wheel" True runTextAreaHScrollWheelTest
  , TestSpec "text-area-hscroll-drag" True runTextAreaHScrollDragTest
  , TestSpec "text-area-2d-scroll" True runTextArea2DScrollTest
  , TestSpec
      "text-area-scroll-cursor-leaves-viewport"
      True
      runTextAreaScrollCursorLeavesViewportTest
  -- Scrolling
  , TestSpec "scroll-thumb-cursor" False runScrollThumbCursorTest
  , TestSpec "scroll-bar-gutter" True runScrollBarGutterTest
  , TestSpec "window-scroll-gutter" True runWindowScrollGutterTest
  , TestSpec "scroll-damage" False runScrollDamageTest
  , TestSpec "scroll-top-clip" True runScrollTopClipTest
  , TestSpec "nested-scroll" False runNestedScrollTest
  , TestSpec "nested-scroll-focus" False runNestedScrollFocusTest
  , TestSpec "scroll-hover-clip" False runScrollHoverClipTest
  , TestSpec "scroll-button-click" False runScrollButtonClickTest
  , TestSpec "scroll-scrolled-out" False runScrolledOutImmunityTest
  , TestSpec "scroll-lockstep-probe" False runScrollLockstepProbeTest
  , TestSpec "page-scroll-backdrop-coverage" False runPageScrollBackdropCoverageTest
  , TestSpec "scroll-2d-pad-fill-overflow" False run2DPadFillOverflowTest
  , TestSpec "scroll-2d-pad-overflow-scrolls" False run2DPadOverflowScrollsTest
  -- Tables
  , TestSpec "table-sort" False runTableSortTest
  , TestSpec "table-reorder" True runTableReorderTest
  , TestSpec "table-scroll-reveal" False runTableScrollRevealTest
  , TestSpec "page-wheel-above-table" False runPageWheelAboveTableTest
  , TestSpec "table-wrap-row-stretch" False runTableWrapRowStretchTest
  , TestSpec "table-first-col" False runTableFirstColWidthTest
  , TestSpec "table-fill-width" False runTableFillWidthTest
  , TestSpec "table-cell-pad" True runTableCellPadTest
  , TestSpec "table-resize-overflow" True runTableResizeOverflowTest
  , TestSpec "table-col-resize-body" False runTableColResizeDemoReproTest
  , TestSpec "table-hbar-reach" False runTableHBarReachTest
  -- Tabs
  , TestSpec "tabs-laziness" False runTabsLazinessTest
  , TestSpec "tabs-emit" False runTabsEmitTest
  , TestSpec "tabs-closable" False runTabsClosableTest
  , TestSpec "tabs-disabled" True runTabsDisabledTest
  , TestSpec "tabs-scroll" False runTabsScrollTest
  , TestSpec "tabs-state-persistence" False runTabsStatePersistenceTest
  , TestSpec "tabs-damage" False runTabsDamageTest
  , TestSpec "tab-response-forwarding" False runTabResponseForwardingTest
  -- Modals, windows and panes
  , TestSpec "modal-overlay" False runModalOverlayTest
  , TestSpec "modal-fits-text" False runModalFitsTextTest
  , TestSpec "modal-no-phantom-scroll" False runModalNoPhantomScrollTest
  , TestSpec "modal-close-damage" False runModalCloseDamageTest
  , TestSpec "modal-fractional-scale-no-scroll" False runModalFractionalScaleNoScrollTest
  , TestSpec "window-overlay" False runWindowOverlayTest
  , TestSpec "overlay-click-through" False runOverlayClickThroughTest
  , TestSpec "overlay-panel-live" False runOverlayPanelLiveTest
  , TestSpec "window-drag" False runWindowDragTest
  , TestSpec "window-close-damage" False runWindowCloseDamageTest
  , TestSpec "page-window-scroll" False runPageWindowScrollTest
  , TestSpec "window-scroll-only-damage" False runWindowScrollOnlyDamageTest
  , TestSpec "window-content-churn" False runWindowContentChurnTest
  , TestSpec "scrolled-debug-toggle" False runScrolledDebugToggleTest
  , TestSpec "window-resize" False runWindowResizeTest
  , TestSpec "window-resize-halo-hit" False runWindowResizeHaloHitTest
  , TestSpec "heading-mono-truncate" False runHeadingMonoTruncateTest
  , TestSpec "pane-grid-mixed-drag" False runPaneGridMixedDragTest
  , TestSpec "pane-grid-clipped-control" False runPaneGridClippedControlTest
  -- Context menus and tooltips
  , TestSpec "context-menu-open" False runContextMenuOpenTest
  , TestSpec "context-menu-scroll-pos" False runContextMenuScrollPosTest
  , TestSpec "release-elsewhere" False runReleaseElsewhereTest
  , TestSpec "right-release-elsewhere" False runRightReleaseElsewhereTest
  , TestSpec "release-returns" False runReleaseReturnsTest
  , TestSpec "tooltip-hover" False runTooltipHoverTest
  , TestSpec "tooltip-id-stable" False runTooltipIdStableTest
  , TestSpec "tooltip-scroll-pos" False runTooltipScrollPosTest
  -- Custom widgets
  , TestSpec "custom-widget-measure" False runCustomWidgetMeasureTest
  , TestSpec "custom-widget-cursor" False runCustomWidgetCursorTest
  , TestSpec "custom-widget-interaction" False runCustomWidgetInteractionTest
  , TestSpec "custom-widget-queued-click" False runCustomWidgetQueuedClickTest
  , TestSpec "custom-widget-content-damage" False runCustomWidgetContentDamageTest
  , TestSpec "custom-widget-content-key" False runCustomWidgetContentKeyTest
  , TestSpec "custom-widget-knob" False runReferenceKnobTest
  , TestSpec "drop-target" False runDropTargetTest
  ]
