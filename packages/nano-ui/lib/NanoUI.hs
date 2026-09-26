-- |
-- Module      : NanoUI
-- Description : Immediate-mode GUI toolkit
-- Copyright   : (c) 2026 Zachary Churchill
-- License     : MIT
-- Maintainer  : zacharyachurchill@gmail.com
--
-- A view is a function that runs every frame. Widgets are ordinary calls:
-- each one adds a layout node, reads this frame's input, and returns what the
-- user did. There are no widget objects to keep and no callbacks to register.
--
-- > counter :: NanoUI ()
-- > counter = do
-- >   (n, setN) <- useInt 0
-- >   row $ do
-- >     whenM (button "-") (setN (n - 1))
-- >     label (T.pack (show n))
-- >     whenM (button "+") (setN (n + 1))
--
-- Run a view with a backend: @runSdlApp@ from @nano-ui-sdl@ or @runRgfwApp@
-- from @nano-ui-rgfw@.
--
-- This module is the view API and nothing else. Writing a backend of your
-- own, or a harness that drives frames itself, needs "NanoUI.Backend"
-- instead: input construction, font callbacks, damage, and the metrics the
-- widgets lay themselves out by.
--
-- = Conventions
--
-- * Widgets return what you usually need: 'Bool' for buttons and menu items,
--   the new value for inputs, and @()@ for text and decoration.
-- * A primed name also returns the widget's 'Response', for hover state,
--   geometry, tooltips, and change or submit flags: @button'@, @slider'@.
-- * Inputs are controlled. Pass the current value and keep the result; a
--   change you do not store is undone on the next frame. Editing state such
--   as the caret, a drag in progress, or an open dropdown stays inside the
--   widget.
-- * Layout arguments are modifiers, as in @buttonWith (fixedW 120)@ or
--   @columnWith (gap 8 . padAll 12)@. Widgets with more options take a
--   configuration record: 'textInputConfigured', 'tabsConfigured'.
--
-- = State
--
-- Keep state in local hooks ('useInt', 'useText', 'useState'), in a model you
-- pass down through the view, or in a reducer: "NanoUI.Emit" adapts widgets to
-- emit messages, and the backends' reducer runners fold them into the model.
module NanoUI
  ( -- * Views
    NanoUI
  , NanoUIEs
  , Ui
  , uiIO
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

    -- | Tab moves focus between focusable widgets. Clicking a text field or
    -- select focuses it; clicking anywhere else unfocuses the current field.
    -- A view that routes typing itself (an editor that keeps the keyboard
    -- while its find bar is shut) calls 'holdFocus' on each frame it wants
    -- the keyboard, and receives that frame's Tab.
    --
    -- 'requestFocus' focuses a widget, named by its response's 'respId' or
    -- by 'currentId' just before declaring it. 'focusNext' and
    -- 'focusPrevious' act like Tab and Shift+Tab, and 'clearFocus' unfocuses.
    -- These moves behave like Tab: the widget shows the focus ring, a text
    -- field keeps its old caret, the field losing focus drops its selection
    -- and commits, and the next Tab continues from the new widget. Disabled
    -- widgets and widgets behind an open modal refuse focus. The move applies
    -- at the end of the frame; check it with 'isFocused' on the next frame:
    --
    -- > (resp, query') <- searchInput' "Find" query
    -- > findPressed <- shortcut (ctrl <> key 'f')
    -- > when findPressed (requestFocus (respId resp))
    --
    -- 'NanoUI.Monad.releaseFocus' unfocuses a widget immediately, mid-view,
    -- and changes nothing else.
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

    -- | Every widget and hook takes the next 'WidgetId' in its container:
    -- ids count up in call order among siblings, and a container starts a
    -- new count for its children. Widget state is stored under that id, so
    -- the same widgets and hooks must run in the same order every frame.
    --
    -- A widget that runs on some frames and not others moves the ids of the
    -- siblings after it. Put the conditional part inside 'scope', which takes
    -- one id whether or not its body adds anything. For a list whose items
    -- are added, removed or reordered, run each item under 'withKey' with a
    -- key unique among its siblings, so the item's state follows its key
    -- instead of its position.
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
  , column
  , columnWith
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
  , useContextMenu

    -- * Inputs
  , checkbox
  , checkbox'
  , checkboxWith
  , checkboxWith'
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

    -- | Text fields change their text only through 'TextCommand's. Keys run
    -- them (Backspace is @'Delete' 'CharLeft'@, Ctrl+Z is 'Undo'), the
    -- right-click menu runs them, and an app can run them on a field by its
    -- id:
    --
    -- > (resp, body') <- textArea' body
    -- > canUndo <- textCanUndo (respId resp)
    -- > whenM (menuItem "Undo") (runTextCommand (respId resp) Undo)
    -- > whenM (menuItem "Insert date") (runTextCommand (respId resp) (InsertText today))
    --
    -- Every command that changes text is recorded for undo. Typing joins one
    -- undo step per word and deleting one per run; the steps keep the edits
    -- themselves, not copies of the document, so a long history of a large
    -- document stays small. Replacing the value a field is passed clears its
    -- history.
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
  , simpleTable
  , useTableSort
  , tableHiddenIndices
  , sortRows
  , Colonnade
  , Headed (..)
  , headed
  , headless

    -- * Overlays
  , modal
  , modalWith
  , window
  , PopupAnchor (..)
  , PopupPlacement (..)
  , PopupConfig (..)
  , defaultPopupConfig
  , popup
  , popupWith
  , tooltip
  , tooltipAt
  , tooltipWidget
  , withTooltip

    -- | A tooltip opens after the pointer rests on its target for
    -- 'tooltipDelay', or immediately if another tooltip was open moments
    -- before. It closes when the pointer leaves or a button goes down; a
    -- wheel turn restarts the delay. The delay costs no frames: the loop
    -- sleeps until a timed wake. 'PlacementAtCursor' keeps the tooltip
    -- 'tooltipGap' below the moving pointer. Disabled widgets show their
    -- tooltips too, so they can say why they are off.
  , TooltipConfig (..)
  , defaultTooltipConfig
  , tooltipConfigured
  , tooltipWidgetConfigured

    -- * Window caption

    -- | The minimize, maximize and close buttons of a window that draws its
    -- own title bar, and the geometry the backend needs to make the rest of
    -- that bar drag and resize the window.
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
  , GridAxis (..)
  , GridNode (..)
  , paneGrid

    -- * Rich text
  , module NanoUI.Widgets.RichText

    -- * Images and drawing
  , ImageId (..)
  , image
  , image'
  , freshImageId
  , registerImageRgba
  , useImageRgba
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
    -- | A drawing's ops are 'DrawOp' values in window coordinates. Build
    -- them with a canvas ('canvas', 'runCanvasFor') rather than by hand. The
    -- constructors after 'DrawTextStyled' are the canvas's internal encoding
    -- of paths, clips and transforms, and may change.
  , DrawOp (..)
  , TextFont (..)
  , defaultTextFont
  , DrawingBuild
  , shiftDrawOp

    -- * Image fit, opacity and rotation

    -- | 'imageConfigured' fits an image to its rect like CSS @object-fit@
    -- ('ContentFit'), with alignment, cropping, zoom, opacity and
    -- 'Rotation'. Unlike 'image', an axis the layout leaves unsized takes
    -- the image's own size, and a fit height keeps the image's aspect ratio:
    --
    -- > imageConfigured defaultImageConfig {icFit = FitContain, icLayout = fixedWH 200 120} photo
    -- > imageConfigured defaultImageConfig {icRotation = RotateSolid (pi / 2), icOpacity = 0.5} photo
    -- > imageConfigured defaultImageConfig {icLayout = fillW, icCrop = Just (Rect 0 0 64 64)} sheet
    --
    -- 'svgIconConfigured' does the same for SVG icons. 'fitRect' computes a
    -- fit's placement, for canvases that draw images with 'drawImageWith'.
  , ContentFit (..)
  , Rotation (..)
  , rotationAngle
  , ImageConfig (..)
  , defaultImageConfig
  , imageConfigured
  , imageConfigured'
  , fitRect

    -- * Custom widgets

    -- | "NanoUI.Widgets.Custom" also holds the canvas-drawn widgets: toggle
    -- switches, knobs, progress bars, spinners and sparklines.
    --
    -- A measure function and 'cdcFont' hand a widget the context's
    -- 'FontMetrics'; 'lineWidth' and 'fmLineHeight' size text with them.
    -- 'widgetCursor' picks the pointer shown over the widget.
    --
    -- Paths, transforms, fill rules, strokes and paints for 'drawPath',
    -- 'drawStrokePath', 'drawPathWith', 'drawStrokePathWith' and
    -- 'withTransform' come from "NanoUI.Path". Import it qualified; it
    -- exports short names such as @circle@, @rotate@ and @stroke@.
  , module NanoUI.Widgets.Custom
  , FontMetrics (fmLineHeight, fmAscent)
  , lineWidth
  , lineWidthUi
  , uiFontMetrics
  , uiFontSize
  , resolveFontUi
  , UiCursorKind (..)

    -- * Cursors

    -- | Widgets choose the pointer shape shown over them: a hand over a
    -- button, an I-beam over a text field, and for a custom widget whatever
    -- its 'widgetCursor' returns. 'withCursorShape' sets a shape over part of
    -- a view, used where no widget inside chooses one:
    --
    -- > withCursorShape UiCursorMove (drawing (fixedWH 320 200) board)
    --
    -- 'UiCursorDefault' means no preference from a widget, and the arrow
    -- from 'withCursorShape'. 'UiCursorHidden' hides the pointer. Disabled
    -- widgets show the arrow; wrap them in @withCursorShape UiCursorNotAllowed@
    -- to show they are off.
  , withCursorShape

    -- * Drag and drop
  , DropType (..)
  , DropEvent (..)
  , DropTarget (..)
  , useDrop
  , dropZone

    -- * Local state
  , useState
  , useFlag
  , useToggle
  , useInt
  , useFloat
  , useEnum
  , useText

    -- ** Background work

    -- | These hooks run slow work on a background thread. 'useTaskStatus'
    -- reports whether the job is running, done or failed; 'useTask' returns
    -- its result once available. The loop sleeps while a job runs and wakes
    -- when it ends. A job starts on the first frame its hook sees a key,
    -- restarts when the key changes, and is killed when the view stops
    -- calling the hook:
    --
    -- > (query, setQuery) <- useText ""
    -- > setQuery =<< textInput query
    -- > hits <- useTask query (searchIndex index query)
    -- > mapM_ (label . hitTitle) (fromMaybe [] hits)
    --
    -- While a new key's job runs, 'useTask' keeps returning the previous
    -- result, so the list above does not flash empty as the query changes.
    -- 'useStream' runs a producer that pushes a stream of values into state
    -- the view reads. 'askWake' returns an action any thread can call to
    -- schedule another frame.
  , TaskStatus (..)
  , useTaskStatus
  , useTask
  , useStream
  , askWake

    -- * Scrolling

    -- | A scroll container ('scroll', 'scroll2D') handles the wheel and its
    -- own scrollbars. These move one from the outside, keyed by the
    -- 'WidgetId' that 'scrollArea' and 'scrollArea2D' hand back.
    --
    -- How far the wheel goes, and whether a scroll glides onto its target
    -- instead of jumping, is one setting for the whole app:
    --
    -- > setScrollTuning ctx defaultScrollTuning
    -- >   { scrollWheelStep = 3 * rowHeight  -- three rows a notch
    -- >   , scrollSmoothTime = 0.12
    -- >   }
    --
    -- 'setScrollStep' gives one list a step of its own. With a glide time
    -- set, every wheel notch and every 'ScrollSmooth' command eases onto its
    -- target over that many seconds, and the frame loop keeps drawing until
    -- it lands.
    --
    -- 'scrollIntoView' brings a widget inside the scroller into view: the row
    -- a keyboard selection just moved to, say. A list that only builds the
    -- rows it shows has no widget to point at for the rest, so scroll to
    -- where the row would be with 'scrollRectIntoView', whose rectangle is in
    -- content coordinates. 'getScrollMetrics' reports the viewport, range and
    -- offset such a list needs to pick its visible rows in the first place.
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

    -- | The same commands, run from inside a view rather than handed the
    -- context. A command that should act on this frame is run before the
    -- scroller is declared, with the id 'currentId' says it will take; the
    -- rows a list builds for itself are worked out from 'getScrollMetricsUi'
    -- the same way:
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

    -- | A sensor reports whether a widget is on screen, meaning it overlaps
    -- the window and every enclosing scroller, panel and floating panel.
    -- 'sensor' wraps part of the view in a watched container;
    -- 'useVisibility' watches an existing widget by id.
    --
    -- Layout is solved after the view runs, so like 'respRect' a sensor
    -- reports the previous frame's layout. When layout changes what a sensor
    -- sees (a scroll, a resize, content growing above it), the loop runs one
    -- more frame and the view reads the change in 'visEvent'. The event goes
    -- to the first view pass that reads the sensor, not to a rerun caused by
    -- a hook write reacting to it. Sensors cost no frames while nothing
    -- moves. A sensor skipped on some frame (in a hidden tab, or a 'scope'
    -- that left it out) is forgotten, and reports 'BecameVisible' again once
    -- it is rebuilt and seen.
    --
    -- 'sensorAnticipate' counts a widget as visible while it is still that
    -- far outside, giving lazy loading a head start. 'sensorDelay' requires
    -- it to stay in view that long, so a list flung past loads none of the
    -- rows that flash by. 'visRect' is the on-screen part of the widget and
    -- 'visBounds' all of it. A thumbnail that decodes its image when it
    -- first comes within 200 pixels of the viewport (@decodeRgba@ stands for
    -- an image decoder):
    --
    -- > thumbnail :: FilePath -> NanoUI ()
    -- > thumbnail path = do
    -- >   (picture, setPicture) <- useState Nothing
    -- >   let cfg = defaultSensorConfig {sensorAnticipate = 200, sensorLayout = fixedWH 96 96}
    -- >   (vis, _) <- sensorConfigured cfg $
    -- >     maybe (label "Loading") (image (fixedWH 96 96)) picture
    -- >   when (becameVisible vis && isNothing picture) $ do
    -- >     (w, h, rgba) <- uiIO (decodeRgba path)
    -- >     iid <- freshImageId
    -- >     whenM (registerImageRgba iid w h rgba) (setPicture (Just iid))
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

    -- | A container lays its children out along a 'Row' or 'Column', either
    -- in a single 'Line' or in lines that 'Wrap' when the next child would
    -- overflow, like a list of tags ('wrap'; 'lineGap' spaces the lines and
    -- 'lineAlign' aligns each). A 'Layered' container stacks its children
    -- instead, later ones on top: 'layers' is one, and 'layered' turns a
    -- panel or card into one. 'pinAt' takes a child out of the flow and
    -- places it at an offset from the corner its alignment picks, over its
    -- siblings, as for a badge or floating button; @pinAt 0 0 . grow@ covers
    -- the parent without affecting its size. 'aspect' keeps a fit height at
    -- a ratio to the width.
    --
    -- Where nodes overlap, a control on top takes the pointer and anything
    -- else lets it through to controls beneath. 'pointer' overrides this for
    -- a node and its contents: 'PointerBlock' makes a card or scrim take the
    -- pointer over its whole box, and 'PointerPass' makes a decoration let it
    -- through.
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
  , Tone (..)
  , fontTone

    -- * Styling

    -- | A 'Theme' says how every kind of widget looks: a 'Style' for each
    -- surface (buttons, inputs, panels, floating windows) and colours for
    -- accents, text selection, links and so on. The context holds one theme
    -- for the whole app ('setTheme'); 'styled' changes it for part of the
    -- view. Style and theme modifiers compose with @(.)@ like layout
    -- modifiers do:
    --
    -- > toolbar = styled (subtle . buttonStyle (cornerRadius 6)) $ row $ do
    -- >   whenM (button "Open") openFile
    -- >   styled primary (whenM (button "Save") save)
    --
    -- Scopes nest, and each one modifies the theme around it, so a modifier
    -- written once ('primary', 'destructive', or one of your own) works in any
    -- theme. 'uiTheme' reads the theme where it is called.
    --
    -- 'disabledWhen' switches the widgets inside it off: they keep their
    -- layout and state, take no input, and fade toward the window colour.
  , styled
  , themed
  , disabledWhen
  , uiTheme

    -- ** Style modifiers
  , background
  , foreground
  , borderColor
  , borderWidth
  , cornerRadius
  , hoverBackground
  , pressBackground
  , fillColor

    -- ** Theme modifiers
  , buttonStyle
  , inputStyle
  , panelStyle
  , windowStyle
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

    -- | 'systemAppearance' reads whether the desktop uses light or dark
    -- colours. The SDL backend reports it; RGFW cannot and reports 'Nothing'.
    -- 'lightDark' maps an appearance to a theme and picks the dark one when
    -- the appearance is unknown, matching nano-ui's dark default. Setting
    -- the theme every frame is free while it does not change:
    --
    -- > setUiTheme . lightDark defaultLightTheme defaultTheme =<< systemAppearance
    --
    -- Alternatively, 'followSystemTheme' makes the context track the system,
    -- as do the backends' @sdlAppThemeFor@ and @optThemeFor@ options:
    --
    -- > followSystemTheme ctx (lightDark defaultLightTheme defaultTheme)
    --
    -- A theme switch repaints the whole window. 'setTheme' returns to a
    -- fixed theme.
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

    -- | The input a view reads: where the pointer is, which keys came in
    -- this frame, and what was typed. A backend fills one of these in every
    -- frame with the functions in "NanoUI.Backend".
    --
    -- While an input method is composing text ('inputComposition'), the
    -- focused text field draws the 'Composition' at its caret, and the frame
    -- drops the keys in 'inputKeys', 'inputKeysReleased' and 'inputKeysHeld'
    -- until the text is committed or cancelled, so no shortcut fires. A custom text
    -- widget requests the input method with 'useInputMethod' and draws the
    -- composition it returns. 'pressedIn', 'releasedIn' and 'heldIn' query
    -- one key or mouse button; 'pressedOnceIn' ignores auto-repeat:
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

    -- | 'respHeldWith' and 'respClickedWith' say which buttons are held on a
    -- widget and which clicked it. 'mousePressed', 'mouseReleased' and
    -- 'mouseHeld' listen for a button anywhere in the part of the view being
    -- declared, like 'keyPressed' for keys:
    --
    -- > whenM (mousePressed MouseBack) goBack
    --
    -- An 'Input' stores held, pressed and released buttons as 'MouseButtons'
    -- sets, which 'pressedIn', 'releasedIn' and 'heldIn' also read.
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

    -- | 'keyPressed', 'keyReleased' and 'keyHeld' listen for a key, and
    -- 'shortcut' binds a command to a chord. Chords combine modifiers and a
    -- key with '<>', using "NanoUI.Shortcut":
    --
    -- > whenM (shortcut (ctrl <> key 's')) save
    -- > whenM (shortcut (cmdOrCtrl <> shift <> key 'p')) (setPaletteOpen True)
    --
    -- A shortcut fires once per press, only with exactly its modifiers held,
    -- and only for the first shortcut declared for that chord. It is silent
    -- behind a modal and for chords the focused widget handles itself, so
    -- Ctrl+A in a focused text field selects its text ('shortcut' has the
    -- full rules). Key listeners skip those keys too: a view hears only keys
    -- no widget took. A focused control takes its keys alone or with Shift,
    -- so a chord such as Alt+Left still reaches shortcuts. A custom widget
    -- declares the keys it takes with 'widgetKeys'. A 'menuItemShortcut' row
    -- binds its chord the same way while its menu is open.
    --
    -- Auto-repeat counts as a press, so holding Ctrl+Z undoes repeatedly.
    -- 'keyPressedOnce' and 'shortcutOnce' ignore repeats, for actions such
    -- as toggles that should happen once per press.
  , keyPressed
  , keyPressedOnce
  , keyReleased
  , keyHeld
  , shortcut
  , shortcutOnce
  , noModifiers
  , modPrimary
  , primaryModifiers
  , modJump
  , modMacCommand

    -- * Debugging

    -- | 'explainLayout' outlines every layout node, coloured by depth, and
    -- tints the node under the pointer. 'explainedNode' describes that node:
    -- its widget id (to match a 'respId'), rect, padding, sizing, gap, flow,
    -- pin and pointer mode. 'explainScope' limits the overlay to part of a
    -- view. The overlay changes nothing else and costs nothing when off, so
    -- its toggle can live in a debug panel.
  , explainLayout
  , explainingLayout
  , explainedNode
  , explainScope
  , ExplainedNode (..)

    -- * The native window

    -- | A window opens with a 'WindowSettings', shared by all backends
    -- (@sdlWindowSettings@ in @SdlOptions@, @optWindow@ in @RgfwOptions@).
    -- A view reads the window with 'askWindow', changes it with the setters
    -- and commands below, and ends the session with 'quitUi'. Backends
    -- support what they can. Without a window, as in a test context, these
    -- do nothing and 'requestScreenshot' returns 'Nothing'.
    --
    -- Setters ('setWindowTitleUi', 'setWindowModeUi', ...) act only on a
    -- change, so they can run every frame. Commands ('moveWindowUi',
    -- 'resizeWindowUi', 'maximizeWindowUi', ...) act on every call, so run
    -- them from events. To confirm before closing, turn
    -- 'wsExitOnCloseRequest' off, watch 'winCloseRequested', and call
    -- 'quitUi' when ready:
    --
    -- > editor :: NanoUI ()
    -- > editor = do
    -- >   (asking, setAsking) <- useFlag False
    -- >   closing <- winCloseRequested <$> askWindow
    -- >   when closing (setAsking True)
    -- >   _ <- modal asking "Discard your changes?" $ do
    -- >     whenM (button "Discard") quitUi
    -- >     whenM (button "Keep editing") (setAsking False)
  , WindowSettings (..)
  , defaultWindowSettings
  , WindowPosition (..)
  , WindowMode (..)
  , WindowState (..)
  , askWindow
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
import NanoUI.Internal.Hooks
import NanoUI.Internal.Id (WidgetId (..))
import NanoUI.Internal.Input
import NanoUI.Internal.Monad
import NanoUI.Internal.NativeWindow
import NanoUI.Internal.Style
import NanoUI.Internal.Tasks
import NanoUI.Svg
import NanoUI.Internal.Types
import NanoUI.Internal.WidgetText
import NanoUI.Internal.Widgets.Animate
import NanoUI.Internal.Widgets.Behavior (useInputMethod)
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
