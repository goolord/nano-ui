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

main :: IO ()
main = runTests testSpecs

-- | A test on a headless context, and one on a pixel-snapped context.
t, px :: String -> (Context -> IORef Int -> IO ()) -> (String, IO Context, Context -> IORef Int -> IO ())
t name run = (name, newContext, run)
px name run = (name, newPixelContext, run)

testSpecs :: [(String, IO Context, Context -> IORef Int -> IO ())]
testSpecs =
  -- Runner, state and messages
  [ t "session-loop" runSessionLoopTest
  , t "session-loop-wake" runSessionLoopWakeTest
  , t "drawing-lock" runDrawingLockTest
  , t "simd-writes" runSimdWritesTest
  , t "draw-layer-offsets" runDrawLayersTest
  , t "no-thunks" runNoThunksTest
  , t "controlled-state" runControlledStateTest
  , t "controlled-inputs" runControlledInputsTest
  , t "hook-state" runHookStateTest
  , t "collection-api" runCollectionApiTest
  , t "embed-state" runEmbedStateTest
  , t "host-slot" runHostSlotTest
  , t "reduce-messages" runReduceMessagesTest
  , t "reduce-click" runReduceClickTest
  , t "widget-no-string-emit" runWidgetNoStringEmitTest
  , -- Ids, layout and caches
    t "id-keyed-list" runIdKeyedListTest
  , t "fit-muted-width" runFitMutedWidthTest
  , t "fit-header-no-shrink" runFitHeaderNoShrinkTest
  , t "layout-reuse" runLayoutReuseTest
  , t "metric-cache-invalidation" runMetricCacheInvalidationTest
  , t "widget-placement-cache" runWidgetPlacementCacheTest
  , t "layout-cache-paint-state" runLayoutPaintStateTest
  , t "deep-nesting" runDeepNestingTest
  , t "grow-split" runGrowSplitTest
  , t "percent-gap-shrink" runPercentGapShrinkTest
  , t "aspect-layout" runAspectLayoutTest
  , px "label-align-end" runLabelAlignEndTest
  , px "responsive-wrap" runResponsiveWrapTest
  , px "kv-multiline-height" runKvMultilineHeightTest
  , t "separator-span" runSeparatorSpanTest
  , t "panel-paints" runPanelPaintsTest
  , t "grid-columns-font-color" runGridColumnsWithFontColorTest
  , t "grid-nested" runNestedGridTest
  , t "stale-font-color" runStaleFontColorTest
  , px "font-composition" runFontCompositionTest
  , px "align-baseline" runAlignBaselineTest
  , t "caption-drag-spans" runDragSpansTest
  , t "caption-buttons" runCaptionButtonsTest
  , -- The view API a self-driven widget leans on
    t "last-rect" runLastRectTest
  , t "hold-focus" runHoldFocusTest
  , t "hold-focus-modal" runHoldFocusModalTest
  , t "hold-focus-tab" runHoldFocusTabTest
  , t "clipboard" runClipboardTest
  , t "pointer-track" runPointerTrackTest
  , t "content-key-of" runContentKeyOfTest
  , t "checkbox-with" runCheckboxWithTest
  , t "pane-grid-initial" runPaneGridInitialTest
  , t "pane-grid-initial-once" runPaneGridInitialOnceTest
  , t "pane-grid-unfocusable" runPaneGridUnfocusableTest
  , t "scroll-ui" runScrollUiTest
  , t "take-escape" runTakeEscapeTest
  , t "modal-with" runModalWithTest
  , -- Drawing
    t "draw-square-geometry" runSquareGeometryTest
  , t "draw-external-text" runExternalTextTest
  , t "draw-concentric-circles" runConcentricCirclesTest
  , t "drawing" runDrawingTest
  , t "image" runImageTest
  , t "rich-text-wrap" runRichTextWrapTest
  , t "rich-text-link" runRichTextLinkTest
  , t "bidi-runs" runBidiRunsTest
  , t "shaped-carets" runShapedCaretTest
  , t "svg-raster" runSvgRasterTest
  , t "svg-icon" runSvgIconTest
  , t "empty-frame" runEmptyFrameTest
  , t "image-swap-damage" runImageSwapDamageTest
  , t "atlas-growth" runAtlasGrowthTest
  , -- Pointer, redraw and damage
    t "pointer-cursor" runPointerCursorTest
  , t "hover-damage" runHoverDamageTest
  , t "damage-bounds-resolution" runDamageBoundsResolutionTest
  , t "damage-widget-explicit" runExplicitDamageWidgetTest
  , t "damage-queue-cleared" runDamageQueueClearedPerFrameTest
  , t "damage-state-change" runStateChangeDamageTest
  , t "damage-orphan-anim-settles" runOrphanAnimationDamageSettlesTest
  , t "versioned-drawing-damage" runVersionedDrawingDamageTest
  , t "clip-frame-backdrop" runClipFrameBackdropTest
  , t "textarea-select-all-damage" runTextAreaSelectAllDamageTest
  , t "panel-body-swap-damage" runPanelBodySwapDamageTest
  , t "refresh-forces-redraw" runRefreshRedrawTest
  , -- Animation
    t "animation-settle" runAnimationSettleTest
  , t "animation-damage" runAnimationDamageTest
  , t "animation-stagger" runAnimationStaggerTest
  , t "animation-bezier" runAnimationBezierTest
  , t "animation-spring-retarget" runAnimationSpringRetargetTest
  , t "animation-spring-dt" runAnimationSpringDtTest
  , t "composite-animation-isolation" runCompositeAnimationIsolationTest
  , t "button-hover-anim" runButtonHoverAnimTest
  , t "spinner" runSpinnerTest
  , t "keep-animating-lapse" runKeepAnimatingLapseTest
  , t "wake-after" runWakeAfterTest
  , -- Keyboard
    t "keyboard-disabled" runKeyboardDisabledTest
  , t "disabled-pointer" runDisabledPointerTest
  , t "disabled-focus-order" runDisabledFocusOrderTest
  , t "disabled-look" runDisabledLookTest
  , t "styled-paint" runStyledPaintTest
  , t "styled-nesting" runStyledNestingTest
  , t "styled-damage" runStyledDamageTest
  , t "text-undo" runTextUndoTest
  , t "text-area-width-tracking" runTextAreaWidthTrackingTest
  , t "text-area-document" runTextAreaDocumentTest
  , t "keyboard-modal-eligibility" runKeyboardModalEligibilityTest
  , t "keyboard-focus-ring" runKeyboardFocusRingTest
  , t "keyboard-button" runKeyboardButtonTest
  , t "keyboard-checkbox" runKeyboardCheckboxTest
  , px "keyboard-slider" runKeyboardSliderTest
  , t "keyboard-radio" runKeyboardRadioTest
  , t "keyboard-toggle" runKeyboardToggleTest
  , t "keyboard-tab-header" runKeyboardTabHeaderTest
  , -- Controls
    t "checkbox-initial" runCheckboxInitialTest
  , px "slider-cursor" runSliderCursorTest
  , px "slider-fill-width" runSliderFillWidthTest
  , t "search-input-clear" runSearchInputClearTest
  , t "search-input-debounce" runSearchInputDebounceTest
  , t "search-input-set-text-debounce" runSearchInputSetTextDebounceTest
  , t "select-drag-to-select" runSelectDragToSelectTest
  , t "select-keyboard" runSelectKeyboardTest
  , t "select-change-once" runSelectChangeOnceTest
  , t "select-close-keeps-focus" runSelectCloseKeepsFocusTest
  , t "select-overlay-damage" runSelectOverlayDamageTest
  , t "tree-select" runTreeSelectTest
  , t "tree-keyboard" runTreeKeyboardTest
  , t "combo-filter" runComboFilterTest
  , t "combo-keyboard-pick" runComboKeyboardPickTest
  , t "combo-mouse-pick" runComboMousePickTest
  , t "combo-blur-commit" runComboBlurCommitTest
  , t "combo-escape-revert" runComboEscapeRevertTest
  , t "combo-hover-highlight" runComboHoverHighlightTest
  , t "combo-scrollbar-drag" runComboScrollbarDragTest
  , t "combo-wheel-scroll" runComboWheelScrollTest
  , px "color-picker-commit" runColorPickerCommitTest
  , px "color-picker-rgba" runColorPickerRgbaTest
  , px "color-picker-edit" runColorPickerEditTest
  , px "color-picker-change-once" runColorPickerChangeOnceTest
  , px "color-picker-bar-keys" runColorPickerBarKeysTest
  , px "color-picker-drag-after-field" runColorPickerDragAfterFieldTest
  , px "controls-tab-height" runControlsTabHeightTest
  , px "bounded-radio-offset" runBoundedRadioTest
  , -- Text input and text area
    t "text-input-cursor" runTextInputCursorTest
  , t "text-input-batch" runTextInputBatchTest
  , t "text-input-selection" runTextInputSelectionTest
  , t "text-input-mouse-selection" runTextInputMouseSelectionTest
  , t "text-input-click-select" runTextInputClickSelectTest
  , t "text-input-word-keys" runTextInputWordKeysTest
  , t "text-input-cut-clears-selection" runTextInputCutClearsSelectionTest
  , t "text-input-clipboard" runTextInputClipboardTest
  , t "text-input-password" runTextInputPasswordTest
  , t "numeric-input" runNumericInputTest
  , t "numeric-input-hex" runNumericInputHexTest
  , t "text-input-menu" runTextInputMenuTest
  , t "text-input-ff-caret" runTextInputFfCaretTest
  , px "text-input-focus-sdl" runTextInputFocusSdlTest
  , px "text-input-scroll" runTextInputScrollTest
  , t "text-input-dirty" runTextInputDirtyTest
  , t "text-input-drag-wake" runTextInputDragWakeTest
  , t "text-area-cut-clears-selection" runTextAreaCutClearsSelectionTest
  , px "text-area-scroll-wheel" runTextAreaScrollWheelTest
  , px "text-area-zoom-scroll" runTextAreaZoomScrollTest
  , px "text-area-remount-scroll" runTextAreaRemountScrollTest
  , px "text-area-menu-pulse" runTextAreaMenuPulseTest
  , px "text-area-menu-select-all" runTextAreaMenuSelectAllTest
  , t "text-command-focus" runTextCommandFocusTest
  , px "text-area-scroll-drag" runTextAreaScrollDragTest
  , px "text-area-cursor-on-scrollbar" runTextAreaCursorOnScrollBarTest
  , px "text-area-hscroll-wheel" runTextAreaHScrollWheelTest
  , px "text-area-hscroll-drag" runTextAreaHScrollDragTest
  , px "text-area-2d-scroll" runTextArea2DScrollTest
  , px "text-area-scroll-cursor-leaves-viewport" runTextAreaScrollCursorLeavesViewportTest
  , -- Scrolling
    t "scroll-thumb-cursor" runScrollThumbCursorTest
  , px "scroll-bar-gutter" runScrollBarGutterTest
  , px "window-scroll-gutter" runWindowScrollGutterTest
  , t "scroll-damage" runScrollDamageTest
  , px "scroll-top-clip" runScrollTopClipTest
  , t "nested-scroll" runNestedScrollTest
  , t "nested-scroll-focus" runNestedScrollFocusTest
  , t "scroll-hover-clip" runScrollHoverClipTest
  , t "scroll-button-click" runScrollButtonClickTest
  , t "scroll-scrolled-out" runScrolledOutImmunityTest
  , t "scroll-lockstep-probe" runScrollLockstepProbeTest
  , t "scroll-2d-grow-min-width" runScroll2DGrowMinWidthTest
  , t "page-scroll-backdrop-coverage" runPageScrollBackdropCoverageTest
  , px "scroll-2d-pad-fill-overflow" run2DPadFillOverflowTest
  , px "scroll-2d-pad-overflow-scrolls" run2DPadOverflowScrollsTest
  , px "scroll-step" runScrollStepTest
  , px "scroll-smooth" runScrollSmoothTest
  , px "scroll-metrics" runScrollMetricsTest
  , px "scroll-into-view" runScrollIntoViewTest
  , px "scroll-glide-clamp" runScrollGlideClampTest
  , -- Tables
    t "table-sort" runTableSortTest
  , px "table-reorder" runTableReorderTest
  , px "table-scroll-reveal" runTableScrollRevealTest
  , px "table-shared-scroll-metrics" runTableSharedScrollMetricsTest
  , px "page-wheel-above-table" runPageWheelAboveTableTest
  , px "table-wrap-row-stretch" runTableWrapRowStretchTest
  , t "table-first-col" runTableFirstColWidthTest
  , t "table-fill-width" runTableFillWidthTest
  , px "table-cell-pad" runTableCellPadTest
  , t "table-rules-tile" runTableRulesTileTest
  , px "table-resize-overflow" runTableResizeOverflowTest
  , t "table-col-resize-body" runTableColResizeDemoReproTest
  , px "table-hbar-reach" runTableHBarReachTest
  , -- Tabs
    t "tabs-laziness" runTabsLazinessTest
  , t "tabs-emit" runTabsEmitTest
  , t "tabs-closable" runTabsClosableTest
  , px "tabs-disabled" runTabsDisabledTest
  , t "tabs-scroll" runTabsScrollTest
  , t "tabs-state-persistence" runTabsStatePersistenceTest
  , t "tabs-damage" runTabsDamageTest
  , t "tab-response-forwarding" runTabResponseForwardingTest
  , -- Modals, windows and panes
    t "modal-overlay" runModalOverlayTest
  , t "modal-fits-text" runModalFitsTextTest
  , t "modal-no-phantom-scroll" runModalNoPhantomScrollTest
  , t "modal-close-damage" runModalCloseDamageTest
  , t "modal-fractional-scale-no-scroll" runModalFractionalScaleNoScrollTest
  , t "modal-fill-label-fits" runModalFillLabelFitsTest
  , t "window-overlay" runWindowOverlayTest
  , t "overlay-sibling-state" runOverlaySiblingStateTest
  , t "overlay-click-through" runOverlayClickThroughTest
  , t "overlay-panel-live" runOverlayPanelLiveTest
  , t "window-drag" runWindowDragTest
  , t "window-close-damage" runWindowCloseDamageTest
  , t "page-window-scroll" runPageWindowScrollTest
  , t "window-scroll-only-damage" runWindowScrollOnlyDamageTest
  , t "window-content-churn" runWindowContentChurnTest
  , t "scrolled-debug-toggle" runScrolledDebugToggleTest
  , t "window-resize" runWindowResizeTest
  , t "window-resize-halo-hit" runWindowResizeHaloHitTest
  , t "heading-mono-truncate" runHeadingMonoTruncateTest
  , t "window-fit-scroll-gutter" runWindowFitScrollGutterTest
  , t "pane-grid-mixed-drag" runPaneGridMixedDragTest
  , t "pane-grid-clipped-control" runPaneGridClippedControlTest
  , t "pane-grid-drop-preview" runPaneGridDropPreviewTest
  , t "pane-grid-pinned-pane" runPaneGridPinnedPaneTest
  , -- Context menus and tooltips
    t "context-menu-open" runContextMenuOpenTest
  , t "context-menu-scroll-pos" runContextMenuScrollPosTest
  , t "context-menu-disabled-row" runContextMenuDisabledRowTest
  , t "release-elsewhere" runReleaseElsewhereTest
  , t "right-release-elsewhere" runRightReleaseElsewhereTest
  , t "release-returns" runReleaseReturnsTest
  , px "pointer-ownership" runPointerOwnershipTest
  , t "pointer-routing-lint" runPointerRoutingLintTest
  , px "pointer-capture" runPointerCaptureTest
  , t "overlap-press" runOverlapPressTest
  , t "tooltip-hover" runTooltipHoverTest
  , t "tooltip-id-stable" runTooltipIdStableTest
  , t "tooltip-scroll-pos" runTooltipScrollPosTest
  , -- Custom widgets
    t "custom-widget-measure" runCustomWidgetMeasureTest
  , t "custom-widget-cursor" runCustomWidgetCursorTest
  , t "custom-widget-interaction" runCustomWidgetInteractionTest
  , t "custom-widget-queued-click" runCustomWidgetQueuedClickTest
  , t "custom-widget-content-damage" runCustomWidgetContentDamageTest
  , t "custom-widget-content-key" runCustomWidgetContentKeyTest
  , t "custom-widget-knob" runReferenceKnobTest
  , t "drop-target" runDropTargetTest
  ]
