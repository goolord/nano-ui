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
  , holdFocus

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
    -- are added, removed or reordered, run each item under 'withKey' (or
    -- 'keyed') with a key unique among its siblings, so the item's state
    -- follows its key instead of its position.
  , scope
  , keyed
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

    -- * Custom widgets

    -- | "NanoUI.Widgets.Custom" also holds the canvas-drawn widgets: toggle
    -- switches, knobs, progress bars, spinners and sparklines.
    --
    -- A measure function and 'cdcFont' hand a widget the context's
    -- 'FontMetrics'; 'lineWidth' and 'fmLineHeight' size text with them.
    -- 'widgetCursor' picks the pointer shown over the widget.
  , module NanoUI.Widgets.Custom
  , FontMetrics (fmLineHeight, fmAscent)
  , lineWidth
  , lineWidthUi
  , uiFontMetrics
  , resolveFontUi
  , UiCursorKind (..)

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
  , Input (..)
  , Key (..)
  , Modifiers (..)
  , inputKeysElem
  , foldInputKeys
  , takeEscape
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
import NanoUI.Internal.Style
import NanoUI.Svg
import NanoUI.Internal.Types
import NanoUI.Internal.WidgetText
import NanoUI.Internal.Widgets.Animate
import NanoUI.Internal.Widgets.Button
import NanoUI.Internal.Widgets.Caption
import NanoUI.Internal.Widgets.Checkbox
import NanoUI.Internal.Widgets.ColorPicker
import NanoUI.Widgets.Combo
import NanoUI.Widgets.Custom
import NanoUI.Internal.Widgets.Display
import NanoUI.Internal.Widgets.Drawing
import NanoUI.Internal.Widgets.Drop
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
