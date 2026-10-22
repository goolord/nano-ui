-- |
-- Module      : NanoUI
-- Description : Immediate-mode GUI toolkit
-- Copyright   : (c) 2026 Zachary Churchill
-- License     : MIT
-- Maintainer  : zacharyachurchill@gmail.com
--
-- Views are written in the 'NanoUI' monad, and the backend re-runs the view
-- function when events arrive. Each widget call emits a layout node, reads the
-- current frame's input, and returns the user interaction directly.
--
-- > counter :: NanoUI ()
-- > counter = row $ do
-- >   n <- useVar (0 :: Int)
-- >   whenM (button "-") (modifyVar n (subtract 1))
-- >   label . T.pack . show =<< readVar n
-- >   whenM (button "+") (modifyVar n (+ 1))
--
-- Run a view with a backend: @runSdlApp@ from @nano-ui-sdl@ or @runRgfwApp@
-- from @nano-ui-rgfw@.
--
-- This module re-exports the view API. For custom backends or frame harnesses
-- (input construction, font callbacks, damage tracking, and layout metrics),
-- see "NanoUI.Backend".
--
-- = Conventions
--
-- * Simple widgets return 'Bool' (buttons, menu items) or @()@ (inputs, text,
--   decoration).
-- * A primed name returns the widget's 'Response' as well or instead, for
--   hover state, geometry, tooltips, and change or submit flags:
--   @button'@, @slider'@.
-- * Input widgets take a mutable 'Var' holding their state: @textInput draft@
--   reads and mutates @draft@ in place. Pass @'constVar' x@ for a read-only
--   value. Transient UI state such as caret position, an active drag, or an
--   open dropdown is stored internally per widget.
-- * Layout arguments are modifiers, as in @buttonWith (fixedW 120)@ or
--   @columnWith (gap 8 . padAll 12)@. Widgets with more options take a
--   configuration record: 'textInputConfigured', 'tabsConfigured'.
--
-- = State
--
-- Keep mutable state in 'Var's: 'useVar' allocates one tied to its call site
-- in the view, and 'newVar' allocates one in 'IO'. Read a 'Var' with 'readVar'.
-- Or use a pure model and reducer with @nano-ui-emit@. See "NanoUI.Var".
module NanoUI
  ( -- * Views
    NanoUI
  , liftIO
  , whenM
  , unlessM
  , ifM
  , windowSize
  , windowWidth
  , windowHeight
  , uiMousePos
  , askInput
  , uiTime
  , requestFrame
  , lastRect

    -- * Focus

    -- | Tab moves focus between focusable widgets. Clicking a text field, a
    -- select, or a focusable custom widget (a knob, or a canvas with
    -- 'canvasFocusable') focuses it; clicking anywhere else unfocuses the
    -- current field.
    -- A view that handles keyboard input directly can call 'holdFocus' each
    -- frame to claim focus and receive Tab events.
    --
    -- 'requestFocus' focuses a widget, identified by its response's 'respId' or
    -- by 'currentId' just before declaring it. 'focusNext' and
    -- 'focusPrevious' act like Tab and Shift+Tab, and 'clearFocus' unfocuses.
    -- Focus changes apply at the end of the frame and behave like Tab: the
    -- target widget shows the focus ring, a text field preserves its caret,
    -- the previously focused field drops its selection and commits, and the
    -- next Tab continues from the newly focused widget. Disabled widgets and
    -- widgets behind an open modal cannot be focused. Check focus state with
    -- 'isFocused' on the next frame:
    --
    -- > resp <- searchInput' "Find" query
    -- > findPressed <- shortcut (ctrl <> key 'f')
    -- > when findPressed (requestFocus (respId resp))
    --
    -- 'NanoUI.Monad.releaseFocus' clears focus immediately mid-frame.
  , holdFocus
  , requestFocus
  , focusNext
  , focusPrevious
  , clearFocus
  , isFocused

    -- * Clipboard
  , getClipboard
  , setClipboard

    -- * Widget identity

    -- | Every widget receives the next 'WidgetId' in its container: IDs count
    -- up in call order among siblings, and each container starts a new counter
    -- for its children. Internal widget state is keyed by 'WidgetId', so
    -- widgets should be called in a consistent order across frames. Positional
    -- 'useVar' calls use a separate counter so widget and 'useVar' IDs do not
    -- affect each other.
    --
    -- A widget (or 'useVar') that runs conditionally shifts the IDs of
    -- subsequent siblings in the same container. Wrap conditional subtrees in
    -- 'scope', which consumes a single ID regardless of whether its body adds
    -- widgets. For dynamic lists where items are added, removed, or reordered,
    -- wrap each item in 'withKey' with a sibling-unique key so state follows
    -- the key rather than the index.
  , scope
  , keyedTag
  , withKey
  , nextId
  , currentId
  , WidgetId (..)

    -- * Responses
  , Response (..)
  , HasResponse (..)
  , respId
  , respRect
  , respHovered
  , respPressed
  , respClicked
  , respChanged
  , respSubmitted
  , respHeldWith
  , respClickedWith
  , respRightPressed
  , respRightClicked
  , setChanged
  , setClicked
  , setSubmitted

    -- * Containers
  , row
  , rowWith
  , rowWith'
  , column
  , columnWith
  , columnWith'
  , hstack
  , vstack
  , grid
  , gridWith
  , panel
  , panelWith
  , card
  , callout
  , calloutWith
  , toolbar
  , center
  , responsive
  , responsiveRowCol
  , scroll
  , scrollWith
  , scroll2D
  , scroll2DWith
  , scrollArea
  , scrollArea2D
  , separator
  , spacer
  , flex
  , layers
  , layersWith
  , mouseArea

    -- * Text
  , label
  , label'
  , labelWith
  , labelWith'
  , heading
  , muted
  , mono
  , danger
  , bold
  , italic
  , underline
  , kv
  , kvMono
  , kvBlock
  , selectableText
  , selectableText'
  , selectableTextWith
  , selectableTextWith'

    -- * Buttons and menus
  , button
  , button'
  , buttonWith
  , buttonWith'
  , iconButton
  , iconButton'
  , buttonContent
  , buttonContent'
  , buttonContentWith
  , buttonContentWith'
  , ButtonConfig (..)
  , defaultButtonConfig
  , buttonConfigured
  , buttonConfigured'
  , menuButton
  , menuButton'
  , menuButtonWith
  , menuButtonWith'
  , menuItem
  , menuItem'
  , menuItemShortcut
  , menuItemDisabled
  , menuSeparator
  , menuHeader
  , contextMenu
  , contextMenuArea

    -- * Inputs
  , checkbox
  , checkbox'
  , checkboxWith
  , checkboxWith'
  , toggleButton
  , toggleButton'
  , toggleButtonWith
  , toggleButtonWith'
  , radio
  , radio'
  , boundedRadio
  , boundedRadio'
  , enumRadio
  , enumRadio'
  , select
  , select'
  , selectWith
  , selectWith'
  , boundedSelect
  , boundedSelect'
  , enumSelect
  , enumSelect'
  , slider
  , slider'
  , sliderWith
  , sliderWith'
  , TextInputConfig (..)
  , defaultTextInputConfig
  , textInput
  , textInput'
  , textInputConfigured
  , textInputConfigured'
  , NumericInputConfig (..)
  , defaultNumericInputConfig
  , numericInput
  , numericInput'
  , numericInputConfigured
  , numericInputConfigured'
  , SearchInputConfig (..)
  , defaultSearchInputConfig
  , searchInput
  , searchInput'
  , searchInputConfigured
  , searchInputConfigured'
  , comboBox
  , comboBox'
  , textArea
  , textArea'
  , textAreaWith
  , textAreaWith'
  , textAreaDocument
  , textAreaDocument'
  , textAreaDocumentWith
  , textAreaDocumentWith'
  , module NanoUI.Widgets.TextDocument
  , colorPicker
  , colorPicker'
  , colorPickerRGBA
  , colorPickerRGBA'
  , colorToHex
  , colorToHexA
  , colorFromHex

    -- * Text editing

    -- | Text fields apply edits through 'TextCommand's. Keyboard input
    -- (Backspace is @'Delete' 'CharLeft'@, Ctrl+Z is 'Undo'), the right-click
    -- context menu, and programmatic calls via 'runTextCommand' all dispatch
    -- through the same command set:
    --
    -- > resp <- textArea' body
    -- > canUndo <- textCanUndo (respId resp)
    -- > whenM (menuItem "Undo") (runTextCommand (respId resp) Undo)
    -- > whenM (menuItem "Insert date") (runTextCommand (respId resp) (InsertText today))
    --
    -- Every command that mutates text is recorded for undo. Consecutive typing
    -- is grouped by word and deletions by run; undo entries store only the
    -- diffs rather than full document snapshots. Modifying the 'Var' from
    -- outside the widget clears its undo history.
  , TextCommand (..)
  , TextMotion (..)
  , Cursor (..)
  , module NanoUI.Widgets.TextField

    -- * Tabs, trees, and tables
  , Tab (..)
  , TabStyle (..)
  , TabOrientation (..)
  , TabResponse (..)
  , TabsConfig (..)
  , defaultTabsConfig
  , tab
  , closableTab
  , tabs
  , tabs'
  , tabsConfigured
  , tabsConfigured'
  , tabBar
  , tabBar'
  , tabBarConfigured
  , tabBarConfigured'
  , boundedTabs
  , boundedTabsConfigured
  , enumTabs
  , boundedTabBar
  , boundedTabBarConfigured
  , enumTabBar
  , TreeItem (..)
  , tree
  , tree'
  , SortDir (..)
  , SortCol (..)
  , ColSize (..)
  , TableConfig (..)
  , TableResponse (..)
  , defaultTableConfig
  , table
  , tableWith
  , tableConfigured
  , table'
  , tableWith'
  , tableConfigured'
  , simpleTable
  , tableHiddenIndices
  , sortRows
  , Colonnade
  , Headed (..)
  , headed
  , headless

    -- * Overlays
  , modal
  , modal'
  , modalWith
  , modalWith'
  , modalPanel
  , modalPanel'
  , modalPanelWith
  , modalPanelWith'
  , window
  , window'
  , PopupAnchor (..)
  , PopupPlacement (..)
  , PopupConfig (..)
  , defaultPopupConfig
  , popup
  , popup'
  , popupWith
  , popupWith'
  , tooltip
  , tooltipAt
  , tooltipWidget
  , withTooltip

    -- | A tooltip opens after the pointer rests on its target for
    -- 'tooltipDelay', or immediately if another tooltip was recently open. It
    -- closes when the pointer leaves or a mouse button is pressed; scrolling
    -- restarts the delay timer via a timed wake without spinning the frame
    -- loop. 'PlacementAtCursor' positions the tooltip 'tooltipGap' below the
    -- cursor. Tooltips also appear on disabled widgets.
  , TooltipConfig (..)
  , defaultTooltipConfig
  , tooltipConfigured
  , tooltipWidgetConfigured

    -- * Window caption

    -- | Minimize, maximize, and close buttons for custom title bars, along
    -- with hit-test geometry so the backend can drag and resize the window.
  , CaptionGlyph (..)
  , CaptionAction (..)
  , CaptionConfig (..)
  , defaultCaptionConfig
  , captionButton
  , captionButtons
  , captionButtonsConfigured
  , captionBarHeight
  , dragSpans
  , WindowFrame (..)
  , defaultWindowFrame
  , windowFrame

    -- * Pane grids
  , PaneGridConfig (..)
  , defaultPaneGridConfig
  , PaneGridCtx (..)
  , PaneView (..)
  , PaneGridResponse (..)
  , PaneEdge (..)
  , PaneDropLocation (..)
  , PaneGridDrop, pgdLocation, pgdRect, pgdPane
  , commitPaneDrop
  , GridAxis (..)
  , GridNode (..)
  , PaneTree (..)
  , validateGridTree
  , paneGrid
  , paneDragHandle

    -- * Rich text
  , module NanoUI.Widgets.RichText

    -- * Images and drawing
  , ImageId (..)
  , image
  , image'
  , freshImageId
  , registerImageRgba
  , releaseImageRgba
  , useImageRgba
  , ImageHandle
  , newImageHandle
  , Svg
  , parseSvg
  , loadSvg
  , svgIcon
  , svgIconWith
  , svgIconWith'
  , svgIconConfigured
  , svgIconConfigured'
  , svgSize
  , box
  , drawing
  , drawingVersioned
  , drawingCached
    -- | 'DrawOp' values use window coordinates. Construct them through the
    -- canvas API ('canvas', 'runCanvasFor') rather than directly; constructors
    -- after 'DrawTextStyled' are internal encodings of paths, clips, and
    -- transforms and may change.
  , DrawOp (..)
  , TextFont (..)
  , defaultTextFont
  , DrawingBuild
  , shiftDrawOp

    -- * Image fit, opacity and rotation

    -- | 'imageConfigured' fits an image into its layout rect (similar to CSS
    -- @object-fit@ via 'ContentFit'), with alignment, cropping, zoom, opacity,
    -- and 'Rotation'. Unlike 'image', an axis left at fit sizing uses the
    -- image's natural dimension, and a fit height preserves aspect ratio:
    --
    -- > imageConfigured defaultImageConfig {icFit = FitContain, icLayout = fixedWH 200 120} photo
    -- > imageConfigured defaultImageConfig {icRotation = RotateSolid (pi / 2), icOpacity = 0.5} photo
    -- > imageConfigured defaultImageConfig {icLayout = fillW, icCrop = Just (Rect 0 0 64 64)} sheet
    --
    -- 'svgIconConfigured' applies the same rules to SVG icons. 'fitRect'
    -- computes the destination rectangle for canvases drawing images with
    -- 'drawImageWith'.
  , ContentFit (..)
  , Rotation (..)
  , rotationAngle
  , ImageConfig (..)
  , defaultImageConfig
  , imageConfigured
  , imageConfigured'
  , fitRect

    -- * Custom widgets

    -- | "NanoUI.Widgets.Custom" also provides canvas-drawn widgets: toggle
    -- switches, knobs, progress bars, spinners, and sparklines.
    --
    -- Measure callbacks and 'cdcFont' provide 'FontMetrics' for sizing text
    -- via 'lineWidth' and 'fmLineHeight'. 'widgetCursor' sets the cursor shape
    -- when hovered.
    --
    -- Paths, transforms, fill rules, strokes, and paints for 'drawPath',
    -- 'drawStrokePath', 'drawPathWith', 'drawStrokePathWith', and
    -- 'withTransform' live in "NanoUI.Path" (designed for qualified import).
  , module NanoUI.Widgets.Custom
  , FontMetrics (fmLineHeight, fmAscent)
  , lineWidth
  , lineWidthUi
  , truncateTextUi
  , uiFontMetrics
  , uiFontSize
  , resolveFontUi
  , UiCursorKind (..)

    -- * Cursors

    -- | Built-in widgets set an appropriate cursor when hovered (pointer hand
    -- on buttons, I-beam on text fields, or 'widgetCursor' on custom widgets).
    -- 'withCursorShape' sets a fallback cursor for a subtree when no inner
    -- widget overrides it:
    --
    -- > withCursorShape UiCursorMove (drawing (fixedWH 320 200) board)
    --
    -- 'UiCursorDefault' indicates no widget override (or resets to the default
    -- arrow inside 'withCursorShape'). 'UiCursorHidden' hides the cursor.
    -- Disabled widgets fall back to the default arrow unless wrapped in
    -- @withCursorShape UiCursorNotAllowed@.
  , withCursorShape

    -- * Drag and drop
  , DropType (..)
  , DropEvent (..)
  , DropTarget (..)
  , useDrop
  , dropZone
  , Reorder (..)
  , useReorder
  , Drag (..)
  , DragPhase (..)
  , DragAxis (..)
  , useDrag
  , DragHandle
  , newDrag
  , insertionIndex

    -- * State

    -- | Mutable state references passed to input widgets; see "NanoUI.Var".
  , Var
  , constVar
  , newVar
  , useVar
  , useEnumVar
  , VarSlot
  , readVar
  , writeVar
  , modifyVar
  , readVarIO
  , writeVarIO
  , modifyVarIO
  , zoomVar
  , zoomVarL
  , forEachVar
  , withElemVars
  , VarMap
  , newVarMap
  , varAt
  , dropVar
  , retainVars
  , withValue

    -- ** Background work

    -- | Run IO work on a background thread and wake the frame loop on
    -- completion. 'useTaskStatus' reports whether the task is running, done,
    -- or failed; 'useTask' returns the latest completed result. A task starts
    -- on the first frame it is called with a key, restarts when the key
    -- changes, and is cancelled if the call is omitted on a later frame.
    -- Allocate @searchTask <- newTask@ during setup:
    --
    -- > query <- useVar ""
    -- > textInput query
    -- > q <- readVar query
    -- > hits <- useTask searchTask q (searchIndex index q)
    -- > mapM_ (label . hitTitle) (fromMaybe [] hits)
    --
    -- While a new key's task is running, 'useTask' continues returning the
    -- previous result so the UI does not flash empty between queries.
    -- 'useStream' runs a background producer that pushes values into state,
    -- and 'askWake' returns an IO callback that schedules a frame from any
    -- thread.
  , TaskStatus (..)
  , useTaskStatus
  , Task
  , newTask
  , Stream
  , newStream
  , useTask
  , useStream
  , askWake

    -- * Scrolling

    -- | Scroll containers ('scroll', 'scroll2D') handle mouse wheel input and
    -- scrollbars automatically. The functions below control scroll position
    -- programmatically using the 'WidgetId' returned by 'scrollArea' or
    -- 'scrollArea2D'. They take the 'NanoUI.Monad.Context', which a view gets
    -- from 'NanoUI.Monad.askContext'.
    --
    -- 'setScrollTuning' configures wheel step distance and smooth-scroll
    -- duration globally:
    --
    -- > ctx <- askContext
    -- > liftIO $ setScrollTuning ctx defaultScrollTuning
    -- >   { scrollWheelStep = 3 * rowHeight  -- three rows a notch
    -- >   , scrollSmoothTime = 0.12
    -- >   }
    --
    -- 'setScrollStep' overrides the wheel step for a single scroll container.
    -- When 'scrollSmoothTime' is positive, wheel steps and 'ScrollSmooth'
    -- commands ease toward the target over that duration.
    --
    -- Tab and 'requestFocus' scroll the widget they focus into view on
    -- their own. 'scrollIntoView' scrolls any child widget into view (for
    -- example, a keyboard-selected row). For virtualized lists that only
    -- instantiate visible rows, use 'scrollRectIntoView' with a rectangle in
    -- content coordinates, and 'getScrollMetrics' to read the viewport size
    -- and offset needed to compute the visible range.
  , ScrollTuning (..)
  , defaultScrollTuning
  , getScrollTuning
  , setScrollTuning
  , getScrollStep
  , setScrollStep
  , ScrollMetrics (..)
  , ScrollAxes (..)
  , getScrollMetrics
  , ScrollBehavior (..)
  , ScrollAlign (..)
  , scrollTo
  , scrollBy
  , scrollPages
  , scrollToStart
  , scrollToEnd
  , scrollIntoView
  , scrollRectIntoView
  , scrollGliding
  , getScrollOffset
  , setScrollOffset
  , getScrollOffset2D
  , setScrollOffset2D

    -- ** From a view

    -- | View-monad variants of the scroll functions above. To apply a scroll
    -- command on the current frame (or read metrics before building the
    -- scroller), query 'currentId' immediately before declaring the scroll
    -- container:
    --
    -- > sid <- currentId
    -- > when moved (scrollRectIntoViewUi sid (Rect 0 (row * rowH) 1 rowH) ScrollNearest ScrollInstant)
    -- > metrics <- getScrollMetricsUi sid
    -- > (_, picked) <- scrollArea (fillW . fillH) (visibleRows metrics)
  , getScrollMetricsUi
  , setScrollOffsetUi
  , scrollToUi
  , scrollByUi
  , scrollPagesUi
  , scrollRectIntoViewUi
  , setScrollStepUi

    -- * Visibility

    -- | A visibility sensor reports whether a widget's bounds intersect the
    -- window and all enclosing clip rects (scroll containers, panels, and
    -- floating windows). 'sensor' wraps a subtree in a monitored container;
    -- 'useVisibility' monitors an existing widget by 'WidgetId'.
    --
    -- Because layout runs after the view pass, visibility reflects the
    -- previous frame's layout (like 'respRect'). When layout changes a
    -- sensor's visibility, the frame loop schedules a follow-up pass and
    -- reports the transition in 'visEvent' on the first pass of that frame.
    -- If a sensor call is omitted on a frame (e.g. inside an inactive tab or
    -- skipped 'scope'), its state is cleared and it will report
    -- 'BecameVisible' again when re-entered.
    --
    -- 'sensorAnticipate' expands the intersection margin outward (in pixels)
    -- for prefetching ahead of scroll. 'sensorDelay' requires the target to
    -- remain in view for a minimum duration before firing, avoiding work for
    -- rows scrolled past quickly. 'visRect' is the visible intersection and
    -- 'visBounds' is the full widget rect:
    --
    -- Make @pictureVar <- newVar Nothing@ once during component setup.
    --
    -- > thumbnail :: Var (Maybe ImageId) -> FilePath -> NanoUI ()
    -- > thumbnail pictureVar path = do
    -- >   picture <- readVar pictureVar
    -- >   let cfg = defaultSensorConfig {sensorAnticipate = 200, sensorLayout = fixedWH 96 96}
    -- >   (vis, _) <- sensorConfigured cfg $
    -- >     maybe (label "Loading") (image (fixedWH 96 96)) picture
    -- >   when (becameVisible vis && isNothing picture) $ do
    -- >     (w, h, rgba) <- liftIO (decodeRgba path)
    -- >     iid <- freshImageId
    -- >     whenM (registerImageRgba iid w h rgba) (writeVar pictureVar (Just iid))
    -- >   when (becameHidden vis) $ do
    -- >     mapM_ releaseImageRgba picture
    -- >     writeVar pictureVar Nothing
  , Visibility (..)
  , VisibilityEvent (..)
  , becameVisible
  , becameHidden
  , sensor
  , sensorWith
  , SensorConfig (..)
  , defaultSensorConfig
  , sensorConfigured
  , useVisibility

    -- * Animation
  , Transition (..)
  , animate
  , animateTo
  , animateToA
  , pulse
  , keepAnimating
  , wakeAfter
  , Animatable (..)
  , Ease (..)
  , applyEase
  , SpringParams (..)
  , presetBouncy
  , presetSmooth
  , presetStiff

    -- * Layout

    -- | Containers arrange children along a 'Row' or 'Column', either on a
    -- single 'Line' or wrapping across lines with 'Wrap' ('wrap', 'lineGap',
    -- 'lineAlign'). A 'Layered' container ('layers', or 'layered' on a panel
    -- or card) stacks children in z-order with later children on top. 'pinAt'
    -- removes a child from flow layout and offsets it from the corner selected
    -- by its alignment; @pinAt 0 0 . grow@ overlays the parent's bounds
    -- without affecting its measured size. 'aspect' constrains a fit height to
    -- a fixed width-to-height ratio.
    --
    -- When nodes overlap, interactive widgets capture pointer events while
    -- non-interactive nodes pass them through to elements beneath. 'pointer'
    -- overrides this: 'PointerBlock' captures pointer events across a
    -- container's full rect, and 'PointerPass' makes a subtree transparent to
    -- pointer hit-testing.
  , Layout (..)
  , LayoutModifier
  , Sizing (..)
  , Direction (..)
  , Flow (..)
  , AlignX (..)
  , AlignY (..)
  , Padding (..)
  , defaultLayout
  , askDefaultLayout
  , withDefaultLayout
  , padAll
  , padXY
  , padLeft
  , padRight
  , padTop
  , padBottom
  , padLRTB
  , gap
  , fillW
  , fillH
  , grow
  , minW
  , maxW
  , fixedW
  , minH
  , maxH
  , fixedH
  , fixedWH
  , alignMid
  , alignEnd
  , alignStart
  , alignCenter
  , alignTop
  , alignBottom
  , alignBaseline
  , tight
  , percent
  , gridMinColW
  , gridCols
  , fixedAspectW
  , fixedAspectH
  , aspect
  , wrap
  , layered
  , lineGap
  , lineAlign
  , LineAlign (..)
  , pinAt
  , PointerMode (..)
  , pointer

    -- * Text style
  , FontVariant (..)
  , FontWeight (..)
  , FontStyle (..)
  , TextDecoration (..)
  , fontRegular
  , fontHeading
  , fontMuted
  , fontMono
  , fontDanger
  , fontSize
  , fontSizeScale
  , fontColor
  , fontWeight
  , fontBold
  , fontLight
  , fontMedium
  , fontSemiBold
  , fontExtraBold
  , fontBlack
  , fontStyle
  , fontItalic
  , fontOblique
  , textDecoration
  , fontUnderline
  , fontStrike
  , TextBreak (..)
  , textBreak
  , breakAnywhere
  , Tone (..)
  , fontTone

    -- * Styling

    -- | A 'Theme' defines surface styles ('Style' for buttons, inputs, panels,
    -- floating windows, popups) and semantic colors (accents, text selection,
    -- links). The context holds the active theme ('setTheme'), and 'styled'
    -- applies a 'Theme' -> 'Theme' modifier to a subtree. Style and theme
    -- modifiers compose with @(.)@:
    --
    -- > toolbar = styled (subtle . buttonStyle (cornerRadius 6)) $ row $ do
    -- >   whenM (button "Open") openFile
    -- >   styled primary (whenM (button "Save") save)
    --
    -- Nested 'styled' calls modify the enclosing theme, so semantic modifiers
    -- ('primary', 'destructive', etc.) adapt to whichever base theme is
    -- active. 'uiTheme' reads the current theme in the view.
    --
    -- 'disabledWhen' disables all widgets in a subtree: they keep their layout
    -- and state, ignore input, and render with dimmed colors.
  , styled
  , themed
  , disabledWhen
  , uiTheme

    -- ** Style modifiers
  , background
  , foreground
  , borderColor
  , borderWidth
  , borderSides
  , borderLeft
  , borderRight
  , borderTop
  , borderBottom
  , Side (..)
  , cornerRadius
  , hoverBackground
  , pressBackground
  , fillColor

    -- ** Theme modifiers
  , buttonStyle
  , inputStyle
  , panelStyle
  , windowStyle
  , popupStyle
  , everyStyle
  , accentColor
  , textColor
  , mutedColor
  , linkColor
  , selectionColor
  , windowColor
  , rounded
  , tone
  , toneColor
  , primary
  , destructive
  , success
  , warning
  , subtle
  , tinted
  , readableOn
  , disabledTheme

    -- * Themes
  , Theme (..)
  , Style (..)
  , defaultTheme
  , tomorrowNightMinDarkTheme
  , tomorrowMinLightTheme
  , tomorrowMidnightMinDarkTheme
  , Base16 (..)
  , themeFromBase16
  , themeFromBase16Dark
  , themeFromBase16Light
  , base16TomorrowNight
  , base16TomorrowLight
  , withTheme
  , setTheme
  , getTheme
  , setUiTheme
  , themeSeries
  , scrollBarTrackColor
  , scrollBarThumbColor
  , scrollBarThumbHoverColor

    -- ** Following the system

    -- | 'systemAppearance' queries the OS light/dark preference (supported by
    -- SDL; RGFW returns 'Nothing'). 'lightDark' selects a theme for an
    -- 'Appearance', defaulting to dark when 'Nothing':
    --
    -- > setUiTheme . lightDark defaultLightTheme defaultTheme =<< systemAppearance
    --
    -- Alternatively, 'followSystemTheme' (or the backends' @sdlAppThemeFor@
    -- and @optThemeFor@ options) updates the context theme automatically when
    -- the OS appearance changes. Like 'setTheme', it takes the context, which
    -- a view gets from 'NanoUI.Monad.askContext':
    --
    -- > followSystemTheme ctx (lightDark defaultLightTheme defaultTheme)
    --
    -- 'setTheme' switches back to a fixed theme.
  , defaultLightTheme
  , Appearance (..)
  , lightDark
  , defaultThemeFor
  , themeAppearance
  , systemAppearance
  , followSystemTheme

    -- * Geometry and colour
  , V2 (..)
  , Rect (..)
  , Size (..)
  , Color (..)
  , colorRGBA
  , colorRGB
  , withAlpha
  , colorWhite
  , colorBlack
  , colorTransparent
  , colorToWord32
  , colorLuminance
  , colorR
  , colorG
  , colorB
  , colorA
  , lerpColor
  , contrastRatio
  , rectContains
  , rectInflate
  , rectIntersect
  , rectUnion
  , v2Add
  , v2Sub
  , onGrid
  , roundHalfUp

    -- * Input

    -- | Per-frame input state populated by the backend (see "NanoUI.Backend").
    --
    -- During IME text composition ('inputComposition'), the focused text field
    -- renders the active 'Composition' at the caret, and key events in
    -- 'inputKeys', 'inputKeysReleased', and 'inputKeysHeld' are suppressed
    -- until composition finishes so shortcuts do not trigger mid-composition.
    -- Custom text widgets opt into IME via 'useInputMethod'.
    -- 'pressedIn', 'releasedIn', and 'heldIn' test a key or mouse button in an
    -- 'Input'; 'pressedOnceIn' ignores key auto-repeat:
    --
    -- > runSdlApp defaultSdlOptions {sdlAppShouldQuit = pressedOnceIn KeyEscape} view
  , Input (..)
  , Pressable (..)
  , Key (..)
  , Modifiers (..)
  , inputKeysElem
  , foldInputKeys
  , takeEscape
  , Composition (..)
  , InputPurpose (..)
  , useInputMethod

    -- * Mouse buttons

    -- | 'respHeldWith' and 'respClickedWith' report which mouse buttons are
    -- held on or clicked a widget. 'mousePressed', 'mouseReleased', and
    -- 'mouseHeld' check mouse button state in the current view scope:
    --
    -- > whenM (mousePressed MouseBack) goBack
    --
    -- 'Input' stores held, pressed, and released buttons as 'MouseButtons'
    -- sets, which can also be queried with 'pressedIn', 'releasedIn', and
    -- 'heldIn'.
  , MouseButton (..)
  , mousePressed
  , mouseReleased
  , mouseHeld
  , MouseButtons
  , noButtons
  , buttonsMember
  , buttonsNull
  , buttonsToList
  , buttonsFromList
  , anyButtonPressed
  , anyButtonReleased
  , inputPointerHeld
  , inputMouseDown
  , inputMousePressed
  , inputMouseReleased
  , inputMouseRightDown
  , inputMouseRightPressed
  , inputMouseRightReleased

    -- * Keyboard

    -- | 'keyPressed', 'keyReleased', and 'keyHeld' check key state, and
    -- 'shortcut' matches a key chord built with "NanoUI.Shortcut":
    --
    -- > whenM (shortcut (ctrl <> key 's')) save
    -- > whenM (shortcut (cmdOrCtrl <> shift <> key 'p')) (setPaletteOpen True)
    --
    -- A shortcut matches when its exact modifiers are held, and only the first
    -- matching 'shortcut' call in a frame fires. Shortcuts and key listeners
    -- are suppressed behind an open modal or when the focused widget claims the
    -- key combination (for example, Ctrl+A in a focused text field). Focused
    -- controls only claim unmodified or Shift-modified keys, leaving chords
    -- like Alt+Left available for shortcuts. Custom widgets declare claimed
    -- keys via 'widgetKeys'. 'menuItemShortcut' binds its chord while its menu
    -- is open.
    --
    -- Key auto-repeat counts as a press (so holding Ctrl+Z repeats undo). Use
    -- 'keyPressedOnce' and 'shortcutOnce' to ignore auto-repeat.
  , keyPressed
  , keyPressedOnce
  , keyReleased
  , keyHeld

    -- | Focused custom widgets read their claimed keys using
    -- 'focusedKeyPressed', 'useKeyNav' (arrow keys, Enter, Space), or
    -- 'keyActivated' (Enter/Space activation).
  , focusedKeyPressed
  , focusedKeyPressedOnce
  , useKeyNav
  , KeyNav (..)
  , navStep
  , keyActivated
  , shortcut
  , shortcutOnce
  , noModifiers
  , modPrimary
  , primaryModifiers
  , modJump
  , modMacCommand

    -- * Debugging

    -- | 'explainLayout' draws depth-colored outlines around layout nodes and
    -- highlights the node under the pointer. 'explainedNode' returns that
    -- node's 'ExplainedNode' record (widget ID, rect, padding, sizing, gap,
    -- flow, pin, and pointer mode). 'explainScope' restricts the overlay to a
    -- subtree.
  , explainLayout
  , explainingLayout
  , explainedNode
  , explainScope
  , ExplainedNode (..)

    -- * The native window

    -- | Initial window configuration is passed via 'WindowSettings'
    -- (@sdlWindowSettings@ in @SdlOptions@, @optWindow@ in @RgfwOptions@).
    -- Inside a view, 'askWindow' reads the current 'WindowState', the setters
    -- and commands below update the window, and 'quitUi' requests exit. In
    -- headless test contexts, window operations are no-ops and
    -- 'requestScreenshot' answers its callback with 'Nothing' (as
    -- 'askScreenshot' does).
    --
    -- Setters ('setWindowTitleUi', 'setWindowModeUi', ...) only issue a
    -- backend call when the value changes, so they can be called every frame.
    -- Action commands ('moveWindowUi', 'resizeWindowUi', 'maximizeWindowUi',
    -- ...) run on every call and should be triggered by events. To confirm
    -- before closing, set 'wsExitOnCloseRequest' to 'False', check
    -- 'winCloseRequested', and call 'quitUi' when confirmed:
    --
    -- > editor :: NanoUI ()
    -- > editor = do
    -- >   asking <- useVar False
    -- >   closing <- winCloseRequested <$> askWindow
    -- >   when closing (writeVar asking True)
    -- >   modal asking "Discard your changes?" $ do
    -- >     whenM (button "Discard") quitUi
    -- >     whenM (button "Keep editing") (writeVar asking False)
  , WindowSettings (..)
  , defaultWindowSettings
  , WindowPosition (..)
  , WindowMode (..)
  , WindowState (..)
  , askWindow
  , WindowCapabilities (..)
  , askWindowCapabilities
  , setWindowTitleUi
  , setWindowIconUi
  , setWindowMinSizeUi
  , setWindowMaxSizeUi
  , setWindowOpacityUi
  , setWindowModeUi
  , moveWindowUi
  , centerWindowUi
  , resizeWindowUi
  , minimizeWindowUi
  , maximizeWindowUi
  , restoreWindowUi
  , toggleMaximizedUi
  , quitUi

    -- ** Pixels and screenshots
  , RgbaPixels
  , rgbaPixels
  , rgbaWidth
  , rgbaHeight
  , rgbaBytes
  , Screenshot (..)
  , requestScreenshot
  , askScreenshot
  , useScreenshot
  )
where

import NanoUI.Internal.Animatable
import NanoUI.Internal.Animation
import NanoUI.Internal.Context
import NanoUI.Internal.Draw
import NanoUI.Internal.Font (FontMetrics (..), lineWidth)
import NanoUI.Internal.Id (WidgetId (..))
import NanoUI.Internal.Input
import NanoUI.Internal.Monad
import NanoUI.Internal.NativeWindow
import NanoUI.Internal.Style
import NanoUI.Internal.Tasks
import NanoUI.Svg
import NanoUI.Var
import NanoUI.Internal.Types
import NanoUI.Internal.WidgetText
import NanoUI.Internal.Widgets.Animate
import NanoUI.Internal.Widgets.Behavior (DragAxis (..), KeyNav (..), Reorder (..), focusedKeyPressed, focusedKeyPressedOnce, keyActivated, navStep, useInputMethod, useKeyNav, useReorder)
import NanoUI.Internal.Widgets.Button
import NanoUI.Internal.Widgets.Caption
import NanoUI.Internal.Widgets.Checkbox
import NanoUI.Internal.Widgets.ColorPicker
import NanoUI.Internal.Widgets.Cursor
import NanoUI.Widgets.Combo
import NanoUI.Widgets.Custom
import NanoUI.Internal.Widgets.Display
import NanoUI.Internal.Widgets.Drawing
import NanoUI.Internal.Widgets.Drop
import NanoUI.Internal.Widgets.Image
import NanoUI.Internal.Widgets.Layout
import NanoUI.Internal.Widgets.Menu
import NanoUI.Internal.Widgets.Node
import NanoUI.Internal.Widgets.NumericInput
import NanoUI.Internal.Widgets.Overlay
import NanoUI.Widgets.PaneGrid
import NanoUI.Internal.Widgets.Popup
import NanoUI.Internal.Widgets.Radio
import NanoUI.Widgets.RichText
import NanoUI.Internal.Widgets.Select
import NanoUI.Internal.Widgets.Sensor
import NanoUI.Internal.Widgets.Shortcut
import NanoUI.Internal.Widgets.Slider
import NanoUI.Internal.Widgets.Table
import NanoUI.Internal.Widgets.Tabs
import NanoUI.Widgets.TextArea
import NanoUI.Widgets.TextBuffer
import NanoUI.Widgets.TextCommand
import NanoUI.Widgets.TextDocument
import NanoUI.Widgets.TextField
import NanoUI.Internal.Widgets.TextInput
import NanoUI.Internal.Widgets.Tree
