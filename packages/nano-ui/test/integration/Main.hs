module Main (main) where

import Cases
import Cases.Animation
import Cases.Atlas
import Cases.Cache
import Cases.Caption
import Cases.Combo
import Cases.ContextMenu
import Cases.CustomWidget
import Cases.Damage
import Cases.Demo
import Cases.Grid
import Cases.HostDraw
import Cases.Keyboard
import Cases.Modal
import Cases.NoThunks
import Cases.NumericInput
import Cases.PointerOwnership
import Cases.PointerRelease
import Cases.RichText
import Cases.Runner
import Cases.SIMD
import Cases.Scroll
import Cases.Select
import Cases.Shaping
import Cases.State
import Cases.Styling
import Cases.Svg
import Cases.Table
import Cases.Tabs
import Cases.TextInput
import Cases.Tooltip
import Cases.ViewApi
import Cases.Window
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
  , TestSpec "session-loop-wake" False runSessionLoopWakeTest
  , TestSpec "drawing-lock" False runDrawingLockTest
  , TestSpec "simd-writes" False runSimdWritesTest
  , TestSpec "draw-layer-offsets" False runDrawLayersTest
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
  , -- Ids, layout and caches
    TestSpec "id-keyed-list" False runIdKeyedListTest
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
  , TestSpec "label-align-end" True runLabelAlignEndTest
  , TestSpec "responsive-wrap" True runResponsiveWrapTest
  , TestSpec "kv-multiline-height" True runKvMultilineHeightTest
  , TestSpec "separator-span" False runSeparatorSpanTest
  , TestSpec "panel-paints" False runPanelPaintsTest
  , TestSpec "grid-columns-font-color" False runGridColumnsWithFontColorTest
  , TestSpec "grid-nested" False runNestedGridTest
  , TestSpec "stale-font-color" False runStaleFontColorTest
  , TestSpec "font-composition" True runFontCompositionTest
  , TestSpec "align-baseline" True runAlignBaselineTest
  , TestSpec "caption-drag-spans" False runDragSpansTest
  , TestSpec "caption-buttons" False runCaptionButtonsTest
  , -- The view API a self-driven widget leans on
    TestSpec "last-rect" False runLastRectTest
  , TestSpec "hold-focus" False runHoldFocusTest
  , TestSpec "hold-focus-modal" False runHoldFocusModalTest
  , TestSpec "hold-focus-tab" False runHoldFocusTabTest
  , TestSpec "clipboard" False runClipboardTest
  , TestSpec "pointer-track" False runPointerTrackTest
  , TestSpec "content-key-of" False runContentKeyOfTest
  , TestSpec "checkbox-with" False runCheckboxWithTest
  , TestSpec "pane-grid-initial" False runPaneGridInitialTest
  , TestSpec "pane-grid-initial-once" False runPaneGridInitialOnceTest
  , TestSpec "pane-grid-unfocusable" False runPaneGridUnfocusableTest
  , TestSpec "scroll-ui" False runScrollUiTest
  , TestSpec "take-escape" False runTakeEscapeTest
  , TestSpec "modal-with" False runModalWithTest
  , -- Drawing
    TestSpec "draw-square-geometry" False runSquareGeometryTest
  , TestSpec "draw-external-text" False runExternalTextTest
  , TestSpec "draw-concentric-circles" False runConcentricCirclesTest
  , TestSpec "drawing" False runDrawingTest
  , TestSpec "image" False runImageTest
  , TestSpec "rich-text-wrap" False runRichTextWrapTest
  , TestSpec "rich-text-link" False runRichTextLinkTest
  , TestSpec "bidi-runs" False runBidiRunsTest
  , TestSpec "shaped-carets" False runShapedCaretTest
  , TestSpec "svg-raster" False runSvgRasterTest
  , TestSpec "svg-icon" False runSvgIconTest
  , TestSpec "empty-frame" False runEmptyFrameTest
  , TestSpec "image-swap-damage" False runImageSwapDamageTest
  , TestSpec "atlas-growth" False runAtlasGrowthTest
  , -- Pointer, redraw and damage
    TestSpec "pointer-cursor" False runPointerCursorTest
  , TestSpec "hover-damage" False runHoverDamageTest
  , TestSpec "damage-bounds-resolution" False runDamageBoundsResolutionTest
  , TestSpec "damage-widget-explicit" False runExplicitDamageWidgetTest
  , TestSpec "damage-queue-cleared" False runDamageQueueClearedPerFrameTest
  , TestSpec "damage-state-change" False runStateChangeDamageTest
  , TestSpec "damage-orphan-anim-settles" False runOrphanAnimationDamageSettlesTest
  , TestSpec "versioned-drawing-damage" False runVersionedDrawingDamageTest
  , TestSpec "clip-frame-backdrop" False runClipFrameBackdropTest
  , TestSpec "textarea-select-all-damage" False runTextAreaSelectAllDamageTest
  , TestSpec "panel-body-swap-damage" False runPanelBodySwapDamageTest
  , TestSpec "refresh-forces-redraw" False runRefreshRedrawTest
  , -- Animation
    TestSpec "animation-settle" False runAnimationSettleTest
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
  , TestSpec "spinner" False runSpinnerTest
  , TestSpec "keep-animating-lapse" False runKeepAnimatingLapseTest
  , TestSpec "wake-after" False runWakeAfterTest
  , -- Keyboard
    TestSpec "keyboard-disabled" False runKeyboardDisabledTest
  , TestSpec "disabled-pointer" False runDisabledPointerTest
  , TestSpec "disabled-focus-order" False runDisabledFocusOrderTest
  , TestSpec "disabled-look" False runDisabledLookTest
  , TestSpec "styled-paint" False runStyledPaintTest
  , TestSpec "styled-nesting" False runStyledNestingTest
  , TestSpec "styled-damage" False runStyledDamageTest
  , TestSpec "text-undo" False runTextUndoTest
  , TestSpec "text-area-width-tracking" False runTextAreaWidthTrackingTest
  , TestSpec "text-area-document" False runTextAreaDocumentTest
  , TestSpec "keyboard-modal-eligibility" False runKeyboardModalEligibilityTest
  , TestSpec "keyboard-focus-ring" False runKeyboardFocusRingTest
  , TestSpec "keyboard-button" False runKeyboardButtonTest
  , TestSpec "keyboard-checkbox" False runKeyboardCheckboxTest
  , TestSpec "keyboard-slider" True runKeyboardSliderTest
  , TestSpec "keyboard-radio" False runKeyboardRadioTest
  , TestSpec "keyboard-toggle" False runKeyboardToggleTest
  , TestSpec "keyboard-tab-header" False runKeyboardTabHeaderTest
  , -- Controls
    TestSpec "checkbox-initial" False runCheckboxInitialTest
  , TestSpec "slider-cursor" True runSliderCursorTest
  , TestSpec "slider-fill-width" True runSliderFillWidthTest
  , TestSpec "search-input-clear" False runSearchInputClearTest
  , TestSpec "search-input-debounce" False runSearchInputDebounceTest
  , TestSpec
      "search-input-set-text-debounce"
      False
      runSearchInputSetTextDebounceTest
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
  , -- Text input and text area
    TestSpec "text-input-cursor" False runTextInputCursorTest
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
  , TestSpec "text-input-drag-wake" False runTextInputDragWakeTest
  , TestSpec
      "text-area-cut-clears-selection"
      False
      runTextAreaCutClearsSelectionTest
  , TestSpec "text-area-scroll-wheel" True runTextAreaScrollWheelTest
  , TestSpec "text-area-zoom-scroll" True runTextAreaZoomScrollTest
  , TestSpec "text-area-remount-scroll" True runTextAreaRemountScrollTest
  , TestSpec "text-area-menu-pulse" True runTextAreaMenuPulseTest
  , TestSpec "text-area-menu-select-all" True runTextAreaMenuSelectAllTest
  , TestSpec "text-command-focus" False runTextCommandFocusTest
  , TestSpec "text-area-scroll-drag" True runTextAreaScrollDragTest
  , TestSpec "text-area-cursor-on-scrollbar" True runTextAreaCursorOnScrollBarTest
  , TestSpec "text-area-hscroll-wheel" True runTextAreaHScrollWheelTest
  , TestSpec "text-area-hscroll-drag" True runTextAreaHScrollDragTest
  , TestSpec "text-area-2d-scroll" True runTextArea2DScrollTest
  , TestSpec
      "text-area-scroll-cursor-leaves-viewport"
      True
      runTextAreaScrollCursorLeavesViewportTest
  , -- Scrolling
    TestSpec "scroll-thumb-cursor" False runScrollThumbCursorTest
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
  , TestSpec "scroll-2d-grow-min-width" False runScroll2DGrowMinWidthTest
  , TestSpec "page-scroll-backdrop-coverage" False runPageScrollBackdropCoverageTest
  , TestSpec "scroll-2d-pad-fill-overflow" True run2DPadFillOverflowTest
  , TestSpec "scroll-2d-pad-overflow-scrolls" True run2DPadOverflowScrollsTest
  , TestSpec "scroll-step" True runScrollStepTest
  , TestSpec "scroll-smooth" True runScrollSmoothTest
  , TestSpec "scroll-metrics" True runScrollMetricsTest
  , TestSpec "scroll-into-view" True runScrollIntoViewTest
  , TestSpec "scroll-glide-clamp" True runScrollGlideClampTest
  , -- Tables
    TestSpec "table-sort" False runTableSortTest
  , TestSpec "table-reorder" True runTableReorderTest
  , TestSpec "table-scroll-reveal" True runTableScrollRevealTest
  , TestSpec "table-shared-scroll-metrics" True runTableSharedScrollMetricsTest
  , TestSpec "page-wheel-above-table" True runPageWheelAboveTableTest
  , TestSpec "table-wrap-row-stretch" True runTableWrapRowStretchTest
  , TestSpec "table-first-col" False runTableFirstColWidthTest
  , TestSpec "table-fill-width" False runTableFillWidthTest
  , TestSpec "table-cell-pad" True runTableCellPadTest
  , TestSpec "table-rules-tile" False runTableRulesTileTest
  , TestSpec "table-resize-overflow" True runTableResizeOverflowTest
  , TestSpec "table-col-resize-body" False runTableColResizeDemoReproTest
  , TestSpec "table-hbar-reach" True runTableHBarReachTest
  , -- Tabs
    TestSpec "tabs-laziness" False runTabsLazinessTest
  , TestSpec "tabs-emit" False runTabsEmitTest
  , TestSpec "tabs-closable" False runTabsClosableTest
  , TestSpec "tabs-disabled" True runTabsDisabledTest
  , TestSpec "tabs-scroll" False runTabsScrollTest
  , TestSpec "tabs-state-persistence" False runTabsStatePersistenceTest
  , TestSpec "tabs-damage" False runTabsDamageTest
  , TestSpec "tab-response-forwarding" False runTabResponseForwardingTest
  , -- Modals, windows and panes
    TestSpec "modal-overlay" False runModalOverlayTest
  , TestSpec "modal-fits-text" False runModalFitsTextTest
  , TestSpec "modal-no-phantom-scroll" False runModalNoPhantomScrollTest
  , TestSpec "modal-close-damage" False runModalCloseDamageTest
  , TestSpec
      "modal-fractional-scale-no-scroll"
      False
      runModalFractionalScaleNoScrollTest
  , TestSpec "modal-fill-label-fits" False runModalFillLabelFitsTest
  , TestSpec "window-overlay" False runWindowOverlayTest
  , TestSpec "overlay-sibling-state" False runOverlaySiblingStateTest
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
  , TestSpec "window-fit-scroll-gutter" False runWindowFitScrollGutterTest
  , TestSpec "pane-grid-mixed-drag" False runPaneGridMixedDragTest
  , TestSpec "pane-grid-clipped-control" False runPaneGridClippedControlTest
  , TestSpec "pane-grid-drop-preview" False runPaneGridDropPreviewTest
  , TestSpec "pane-grid-pinned-pane" False runPaneGridPinnedPaneTest
  , -- Context menus and tooltips
    TestSpec "context-menu-open" False runContextMenuOpenTest
  , TestSpec "context-menu-scroll-pos" False runContextMenuScrollPosTest
  , TestSpec "context-menu-disabled-row" False runContextMenuDisabledRowTest
  , TestSpec "release-elsewhere" False runReleaseElsewhereTest
  , TestSpec "right-release-elsewhere" False runRightReleaseElsewhereTest
  , TestSpec "release-returns" False runReleaseReturnsTest
  , TestSpec "pointer-ownership" True runPointerOwnershipTest
  , TestSpec "pointer-routing-lint" False runPointerRoutingLintTest
  , TestSpec "pointer-capture" True runPointerCaptureTest
  , TestSpec "overlap-press" False runOverlapPressTest
  , TestSpec "tooltip-hover" False runTooltipHoverTest
  , TestSpec "tooltip-id-stable" False runTooltipIdStableTest
  , TestSpec "tooltip-scroll-pos" False runTooltipScrollPosTest
  , -- Custom widgets
    TestSpec "custom-widget-measure" False runCustomWidgetMeasureTest
  , TestSpec "custom-widget-cursor" False runCustomWidgetCursorTest
  , TestSpec "custom-widget-interaction" False runCustomWidgetInteractionTest
  , TestSpec "custom-widget-queued-click" False runCustomWidgetQueuedClickTest
  , TestSpec "custom-widget-content-damage" False runCustomWidgetContentDamageTest
  , TestSpec "custom-widget-content-key" False runCustomWidgetContentKeyTest
  , TestSpec "custom-widget-knob" False runReferenceKnobTest
  , TestSpec "drop-target" False runDropTargetTest
  ]
