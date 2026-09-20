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
  , Ui
  , runUi
  , runNanoUI
  , uiIO
  , whenM
  , unlessM
  , ifM
  , windowSize
  , windowWidth
  , windowHeight
  , uiMousePos

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
  , burstNextIds
  , WidgetId (..)
  , IdContext
  , initialIdContext
  , widgetId
  , hashWidgetId
  , mix64
  , mixFnv

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
  , menuButton
  , menuButton'
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
  , toggleSwitch
  , toggleSwitch'
  , toggleSwitchWith
  , toggleSwitchWith'
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
  , knob
  , knob'
  , knobWith
  , knobWith'
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
  , SearchFieldConfig (..)
  , defaultSearchFieldConfig
  , searchField
  , searchField'
  , searchFieldConfigured
  , searchFieldConfigured'
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
  , TextDocument
  , textDocument
  , emptyDocument
  , documentText
  , documentLines
  , documentLine
  , documentLineCount
  , sameDocument
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
  , runTextCommand
  , textCanUndo
  , textCanRedo

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

    -- * Pane grids
  , PaneGridConfig (..)
  , defaultPaneGridConfig
  , PaneGridCtx (..)
  , PaneView (..)
  , PaneGridResponse (..)
  , GridAxis (..)
  , paneGrid

    -- * Progress and sparklines
  , progressBar
  , progressBar'
  , progressBarWith
  , progressBarWith'
  , circularProgress
  , circularProgress'
  , circularProgressWith
  , circularProgressWith'
  , spinner
  , spinner'
  , spinnerWith
  , spinnerWith'
  , Inline
  , inlineText
  , inlineWith
  , restyle
  , strong
  , emphasis
  , inlineCode
  , hyperlink
  , richText
  , richText'
  , richTextWith
  , richTextWith'
  , sparkline
  , sparkline'
  , sparklineWith
  , sparklineWith'

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
  , drawTextBox
  , shiftDrawOp

    -- * Custom widgets
  , CustomWidgetSpec (..)
  , defaultCustomWidgetSpec
  , customWidget
  , customWidgetWithId
  , contentKey
  , CustomDrawContext (..)
  , CustomMeasureFn
  , CustomDrawBuild
  , CanvasM
  , runCanvas
  , canvas
  , drawRect
  , drawRoundedRect
  , drawCircle
  , drawStroke
  , drawStrokeRoundedRect
  , drawStrokeCircle
  , drawStrokeAA
  , drawQuadGradient
  , drawLinearGradientH
  , drawLinearGradientV
  , drawImage
  , drawImageUV
  , drawText
  , useDrag2D
  , Drag2D (..)
  , useWheelDelta

    -- * Drag and drop
  , DropType (..)
  , DropEvent (..)
  , emptyDropEvents
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
  , Input (..)
  , Key (..)
  , Modifiers (..)
  , emptyInput
  , inputInteracted
  , inputPointerHeld
  , appendInputKey
  , appendDropEvent
  , emptyInputKeys
  , inputKeysElem
  , inputKeysFromList
  , inputKeysNull
  , foldInputKeys

    -- * Damage
  , Damage (..)
  , DamageBounds (..)
  , defaultDamageSlop
  , sliderDamageSlop
  , haloDamageSlop
  , resolveDamageRect
  , damageWidgetNow
  , damageKeyNow
  , damageRectNow
  , damageGroupNow
  , damageFullNow

    -- * Backend support
  , FontMetrics (..)
  , FontBackend (..)
  , prepareFontMetrics
  , prepareFontMetricsMany
  , measureTextIO
  , lineWidthIO
  , lineWidth
  , drawShaped
  , drawGlyph
  , GlyphQuad (..)
  , ShapedText (..)
  , ShapedGlyphs (..)
  , scaleFontMetrics
  , monospaceMetrics
  , uiFontMetrics
  , widgetContentInset
  , widgetPadding
  , treeItemPadding
  , ScrollBarSlot (..)
  , scrollBarGutter
  , scrollBarWidth
  , windowPad
  , windowMargin
  , Compact
  , compactHost
  , askCompact
  )
where

import NanoUI.Animatable
import NanoUI.Animation
import NanoUI.Compact
import NanoUI.Context
import NanoUI.Draw
import NanoUI.Font
import NanoUI.Hooks
import NanoUI.Id (IdContext, WidgetId (..), hashWidgetId, initialIdContext, mix64, mixFnv, widgetId)
import NanoUI.Input
import NanoUI.Monad
import NanoUI.Style
import NanoUI.Svg
import NanoUI.Types
import NanoUI.WidgetText
import NanoUI.Widgets.Animate
import NanoUI.Widgets.Button
import NanoUI.Widgets.Checkbox
import NanoUI.Widgets.ColorPicker
import NanoUI.Widgets.Combo
import NanoUI.Widgets.Custom
import NanoUI.Widgets.Display
import NanoUI.Widgets.Drawing
import NanoUI.Widgets.Drop
import NanoUI.Widgets.Layout
import NanoUI.Widgets.Menu
import NanoUI.Widgets.Node
import NanoUI.Widgets.NumericInput
import NanoUI.Widgets.Overlay
import NanoUI.Widgets.PaneGrid
import NanoUI.Widgets.Popup
import NanoUI.Widgets.Radio
import NanoUI.Widgets.RichText
import NanoUI.Widgets.Select
import NanoUI.Widgets.Slider
import NanoUI.Widgets.Table
import NanoUI.Widgets.Tabs
import NanoUI.Widgets.TextArea
import NanoUI.Widgets.TextBuffer
import NanoUI.Widgets.TextCommand
import NanoUI.Widgets.TextDocument
import NanoUI.Widgets.TextField
import NanoUI.Widgets.TextInput
import NanoUI.Widgets.Tree
