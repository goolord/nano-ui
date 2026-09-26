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

    -- | Tab moves the keyboard between focusable widgets, and a click gives it
    -- to the widget clicked. A view that decides for itself where typing goes
    -- -- an editor that keeps the keyboard while its find bar is shut -- says
    -- so with 'holdFocus' each frame it should, and gets that frame's Tab.
    --
    -- A view sends the keyboard somewhere once with 'requestFocus', naming
    -- the widget by the 'respId' of its response, or nowhere with
    -- @'WidgetId' 0@. Focus moves as Tab would move it: the widget shows the
    -- focus ring, a text field takes the keys with its caret where it left
    -- it, the field that had them commits, and the next Tab goes on from
    -- there. A disabled widget, or one behind an open modal, refuses it. The
    -- move happens at the end of the frame, and the widget has the keyboard
    -- from the next ('NanoUI.Monad.focusedWidget' says which has it):
    --
    -- > (resp, query') <- searchInput' "Find" query
    -- > findPressed <- shortcut (ctrl <> key 'f')
    -- > when findPressed (requestFocus (respId resp))
  , holdFocus
  , requestFocus

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
  , stack
  , stackWith

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

    -- | A tooltip opens once the pointer has rested on its target for
    -- 'tooltipDelay', and opens at once when another was up moments before.
    -- It shuts when the pointer leaves the target or a button goes down, and
    -- a wheel turn starts the wait again. Waiting costs nothing: the frame the
    -- tooltip opens on is a timed wake. 'PlacementAtCursor' keeps it just below
    -- the pointer as the pointer moves.
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
  , Svg
  , parseSvg
  , loadSvg
  , svgIcon
  , svgIconWith
  , svgIconWith'
  , svgSize
  , box
  , drawing
  , drawingVersioned
  , drawingCached
  , DrawOp (..)
  , TextFont (..)
  , defaultTextFont
  , DrawingBuild
  , shiftDrawOp

    -- * Image fit, opacity and rotation

    -- | 'imageConfigured' draws an image fitted to its rect the way CSS's
    -- @object-fit@ does ('ContentFit'), aligned where the fit leaves room,
    -- faded, and turned ('Rotation'). Unlike 'image', an axis its layout
    -- leaves unsized takes the image's own size:
    --
    -- > imageConfigured defaultImageConfig {icFit = FitContain, icLayout = fixedWH 200 120 defaultLayout} photo
    -- > imageConfigured defaultImageConfig {icRotation = RotateSolid (pi / 2), icOpacity = 0.5} photo
  , ContentFit (..)
  , Rotation (..)
  , rotationAngle
  , ImageConfig (..)
  , defaultImageConfig
  , imageConfigured
  , imageConfigured'

    -- * Custom widgets

    -- | "NanoUI.Widgets.Custom" also holds the canvas-drawn widgets: toggle
    -- switches, knobs, progress bars, spinners and sparklines.
    --
    -- A measure function and 'cdcFont' hand a widget the context's
    -- 'FontMetrics'; 'lineWidth' and 'fmLineHeight' size text with them.
    -- 'widgetCursor' picks the pointer shown over the widget.
    --
    -- The paths 'drawPath' and 'drawStrokePath' draw, and the transforms
    -- 'withTransform' takes, are built with "NanoUI.Path", imported
    -- qualified, which keeps names such as @circle@ and @rotate@ out of this
    -- module.
  , module NanoUI.Widgets.Custom
  , FontMetrics (fmLineHeight, fmAscent)
  , lineWidth
  , lineWidthUi
  , uiFontMetrics
  , uiFontSize
  , resolveFontUi
  , UiCursorKind (..)

    -- * Cursors

    -- | Widgets pick the pointer shape shown over them: the pointing hand over
    -- a button, the I-beam over a text field. 'withCursorShape' asks for a
    -- shape over any part of a view, where the widgets inside do not pick
    -- one:
    --
    -- > withCursorShape UiCursorMove (drawing (fixedWH 320 200) board)
    --
    -- A 'CursorShape' is a 'UiCursorKind'. Disabled widgets keep the arrow;
    -- wrap them in @withCursorShape UiCursorNotAllowed@ to show that they are
    -- off.
  , withCursorShape
  , CursorShape

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

    -- | Work that should not hold up a frame runs on a thread of its own.
    -- 'useTask' runs an action there and returns its result once there is
    -- one; the loop sleeps while it runs and wakes when it finishes. A job
    -- starts the first frame its hook is called with a key, is replaced when
    -- the key changes, and is killed once the view stops calling its hook:
    --
    -- > (query, setQuery) <- useText ""
    -- > setQuery =<< textInput query
    -- > hits <- useTask query (searchIndex index query)
    -- > mapM_ (label . hitTitle) (fromMaybe [] hits)
    --
    -- 'askWake' hands the view an action any thread may call to run it
    -- again: what a stream or a poller of the app's own needs.
  , useTask
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

    -- | A sensor reports whether a widget is on screen: whether it overlaps
    -- the window and the inside of every scroller, panel and floating panel
    -- around it. 'sensor' runs part of the view in a container that watches
    -- itself, and 'useVisibility' watches a widget already built, by its id.
    --
    -- Layout is solved after the view runs, so a sensor reports what the
    -- last frame's layout showed, as 'respRect' does. A layout that changes
    -- what a sensor sees (a scroll, a resize, content growing above it)
    -- makes the loop run one more frame, whose view reads the change in
    -- 'visEvent'. The event goes to the first view pass that reads the
    -- sensor, not to the pass a hook write in reaction to it runs again.
    -- While nothing moves, sensors cost no frames. A sensor that is not
    -- built on some frame, in a tab that is not shown or a 'scope' that left
    -- it out, is forgotten, and reports 'BecameVisible' again once it is
    -- built and seen.
    --
    -- An anticipate margin ('sensorAnticipate') counts a widget as visible
    -- while it is still that far outside, which gives lazy loading a head
    -- start. A thumbnail that decodes its picture the first time it comes
    -- within 200 pixels of the viewport (@decodeRgba@ stands for an image
    -- decoder):
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

    -- | A container lays its children out along a 'Row' or a 'Column', one
    -- after another, or layers them in a 'Stack' ('stack'), later children on
    -- top. 'wrap' breaks a row or column into lines where the next child
    -- would overflow it, as a list of tags does, and 'pinAt' takes a child out
    -- of its parent's flow to sit at an offset in the parent, over its
    -- siblings, as a badge or a floating button does.
  , Layout (..)
  , LayoutModifier
  , Sizing (..)
  , Direction (..)
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
  , wrap
  , lineGap
  , pinAt

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
  , fontWarning

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
  , primary
  , destructive
  , success
  , subtle
  , tinted
  , readableOn
  , disabledTheme
  , warning

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

    -- | Where the platform says whether the desktop is set to light or dark
    -- colours, the backend reports it and 'systemAppearance' reads it: the
    -- SDL backend does, RGFW cannot tell and reports 'Nothing'.
    -- 'followSystemTheme' makes the base theme switch with it, or the SDL
    -- option @sdlAppFollowSystemTheme@ from the start:
    --
    -- > followSystemTheme ctx defaultLightTheme defaultTheme
    --
    -- The dark theme is used while the system asks for dark and the light one
    -- otherwise. A switch repaints the whole window; 'setTheme' goes back to
    -- a fixed theme.
  , defaultLightTheme
  , Appearance (..)
  , systemAppearance
  , followSystemTheme
  , followSystemThemeUi

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
    -- frame with the functions in "NanoUI.Backend". While an input method
    -- composes text ('inputComposition'), the focused text field draws the
    -- 'Composition' at its caret and the keys go to the input method: the
    -- frame drops the keys pressed, released and held ('inputKeys',
    -- 'inputKeysReleased', 'inputKeysHeld') until the text is committed or
    -- cancelled, so no shortcut fires on them.
  , Input (..)
  , Key (..)
  , Modifiers (..)
  , inputKeysElem
  , foldInputKeys
  , takeEscape
  , Composition (..)

    -- * Mouse buttons

    -- | A widget's 'Response' says which buttons went down on it and are
    -- held ('respHeldWith') and which clicked it ('respClickedWith'), and
    -- 'mousePressed', 'mouseReleased' and 'mouseHeld' listen for a button
    -- anywhere on the part of the view being declared, as 'keyPressed' does
    -- for a key:
    --
    -- > whenM (mousePressed MouseBack) goBack
    --
    -- An 'Input' holds the buttons held, pressed and released as
    -- 'MouseButtons' sets, which 'buttonHeld', 'buttonPressed' and
    -- 'buttonReleased' read.
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
  , buttonHeld
  , buttonPressed
  , buttonReleased
  , anyButtonPressed
  , anyButtonReleased
  , inputPointerHeld

    -- * Keyboard

    -- | A view listens for a key with 'keyPressed', 'keyReleased' and
    -- 'keyHeld', and binds a command to a chord with 'shortcut'. A chord is
    -- modifiers and a key put together with '<>', from "NanoUI.Shortcut":
    --
    -- > whenM (shortcut (ctrl <> key 's')) save
    -- > whenM (shortcut (cmdOrCtrl <> shift <> key 'p')) (setPaletteOpen True)
    --
    -- A shortcut fires once per press, with exactly its modifiers held, for
    -- the first shortcut declared for the chord. It stays quiet behind a
    -- modal and for a chord the widget with the keyboard acts on itself, so
    -- Ctrl+A in a focused text field selects its text rather than running a
    -- shortcut bound to Ctrl+A ('shortcut' has the rules). A
    -- 'menuItemShortcut' row binds its chord the same way while its menu is
    -- open.
  , keyPressed
  , keyReleased
  , keyHeld
  , shortcut
  , noModifiers
  , modPrimary
  , primaryModifiers

    -- * Debugging

    -- | 'explainLayout' shows how a view was laid out: it outlines every
    -- layout node, coloured by how deep the node is, and tints the node under
    -- the pointer, whose rect and padding 'explainedNode' reports. Put the
    -- toggle in a debug panel; the overlay changes nothing else, and with it
    -- off a frame costs what it did.
  , explainLayout
  , explainingLayout
  , explainedNode
  , ExplainedNode (..)

    -- * The native window

    -- | What a view can ask of the window it runs in: a screenshot of it,
    -- an icon, limits on its size, a position and an opacity. The backend
    -- does what it can with each; a view that is not running in a window,
    -- as under a test context, gets nothing done, and 'requestScreenshot'
    -- answers 'Nothing'. What a window opens with is the backend's options
    -- record: @SdlOptions@ or @RgfwOptions@.
  , RgbaImage (..)
  , WindowPosition (..)
  , requestScreenshot
  , setWindowIconUi
  , setWindowMinSizeUi
  , setWindowMaxSizeUi
  , setWindowPositionUi
  , setWindowOpacityUi
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
