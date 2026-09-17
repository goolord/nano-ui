-- |
-- Module      : NanoUI
-- Description : Immediate-mode GUI toolkit
-- Copyright   : (c) 2026 Zachary Churchill
-- License     : MIT
-- Maintainer  : zacharyachurchill@gmail.com
--
-- A view is a function that runs every frame. Widgets are ordinary calls:
-- each one lays itself out, reads this frame's input, and returns what the
-- user did. There are no widget objects to keep and no callbacks to register.
--
-- @
-- counter :: NanoUI ()
-- counter = do
--   (n, setN) <- useInt 0
--   row $ do
--     whenM (button "-") (setN (n - 1))
--     label (T.pack (show n))
--     whenM (button "+") (setN (n + 1))
-- @
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
-- pass down through the view, or in a reducer: "NanoUI.Emit" has widgets that
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
  , MenuAction (..)

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
  , colorPicker
  , colorPicker'
  , colorPickerRGBA
  , colorPickerRGBA'
  , colorToHex
  , colorToHexA
  , colorFromHex

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
  , box
  , drawing
  , drawingVersioned
  , drawingCached
  , DrawOp (..)
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
    -- @
    -- 'setScrollTuning' ctx 'defaultScrollTuning'
    --   { 'scrollWheelStep' = 3 * rowHeight  -- three rows a notch
    --   , 'scrollSmoothTime' = 0.12          -- glide onto it
    --   }
    -- @
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
    -- @
    -- toolbar = 'styled' ('subtle' . 'buttonStyle' ('cornerRadius' 6)) $ 'row' $ do
    --   'whenM' ('button' \"Open\") openFile
    --   'styled' 'primary' ('whenM' ('button' \"Save\") save)
    -- @
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

    -- * Input
  , Input (..)
  , Key (..)
  , Modifiers (..)
  , emptyInput
  , inputInteracted
  , inputPointerHeld
  , appendInputKey
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
  , drawRun
  , drawGlyph
  , GlyphQuad (..)
  , RunQuad (..)
  , scaleFontMetrics
  , monospaceMetrics
  , uiFontMetrics
  , labelContentInset
  , tableCellInset
  , widgetContentInset
  , widgetPadding
  , treeItemPadding
  , resolveLayoutGap
  , resolveLayoutPadding
  , ScrollBarSlot (..)
  , scrollBarGutter
  , scrollBarWidth
  , panelPaintPad
  , windowPad
  , windowMargin
  , Compact
  , compactHost
  , askCompact
  )
where

import NanoUI.Animatable (Animatable (..))
import NanoUI.Animation
  ( SpringParams (..)
  , presetBouncy
  , presetSmooth
  , presetStiff
  )
import NanoUI.Compact (Compact, askCompact, compactHost)
import NanoUI.Context
  ( Ease (..)
  , ScrollAlign (..)
  , ScrollAxes (..)
  , ScrollBehavior (..)
  , ScrollMetrics (..)
  , ScrollTuning (..)
  , applyEase
  , defaultScrollTuning
  , getScrollMetrics
  , getScrollOffset
  , getScrollOffset2D
  , getScrollStep
  , getScrollTuning
  , getTheme
  , scrollBy
  , scrollGliding
  , scrollIntoView
  , scrollPages
  , scrollRectIntoView
  , scrollTo
  , scrollToEnd
  , scrollToStart
  , setScrollOffset
  , setScrollOffset2D
  , setScrollStep
  , setScrollTuning
  , setTheme
  , withTheme
  )
import NanoUI.Draw (drawTextBox, shiftDrawOp)
import NanoUI.Font
  ( FontBackend (..)
  , FontMetrics (..)
  , GlyphQuad (..)
  , RunQuad (..)
  , ScrollBarSlot (..)
  , drawGlyph
  , drawRun
  , labelContentInset
  , lineWidth
  , lineWidthIO
  , measureTextIO
  , monospaceMetrics
  , prepareFontMetrics
  , prepareFontMetricsMany
  , resolveLayoutGap
  , resolveLayoutPadding
  , scaleFontMetrics
  , scrollBarGutter
  , scrollBarWidth
  , tableCellInset
  , treeItemPadding
  , widgetContentInset
  , widgetPadding
  )
import NanoUI.Id
  ( IdContext
  , WidgetId (..)
  , hashWidgetId
  , initialIdContext
  , mix64
  , mixFnv
  , widgetId
  )
import NanoUI.Input
  ( DropEvent (..)
  , DropType (..)
  , Input (..)
  , Key (..)
  , Modifiers (..)
  , appendInputKey
  , emptyDropEvents
  , emptyInput
  , emptyInputKeys
  , foldInputKeys
  , inputInteracted
  , inputKeysElem
  , inputKeysFromList
  , inputKeysNull
  , inputPointerHeld
  )
import NanoUI.Monad
  ( NanoUI
  , Ui
  , askDefaultLayout
  , burstNextIds
  , currentId
  , damageFullNow
  , disabledWhen
  , styled
  , themed
  , damageGroupNow
  , damageKeyNow
  , damageRectNow
  , damageWidgetNow
  , ifM
  , keyed
  , keyedTag
  , nextId
  , runNanoUI
  , runUi
  , scope
  , setUiTheme
  , uiFontMetrics
  , uiIO
  , uiMousePos
  , uiTheme
  , unlessM
  , whenM
  , windowHeight
  , windowSize
  , windowWidth
  , withDefaultLayout
  , withKey
  )
import NanoUI.Style
  ( AlignX (..)
  , AlignY (..)
  , Base16 (..)
  , Direction (..)
  , FontStyle (..)
  , FontVariant (..)
  , FontWeight (..)
  , Layout (..)
  , LayoutModifier
  , Padding (..)
  , Sizing (..)
  , Style (..)
  , TextDecoration (..)
  , Theme (..)
  , accentColor
  , background
  , borderColor
  , borderWidth
  , buttonStyle
  , cornerRadius
  , destructive
  , disabledTheme
  , everyStyle
  , fillColor
  , foreground
  , hoverBackground
  , inputStyle
  , linkColor
  , mutedColor
  , panelStyle
  , pressBackground
  , primary
  , readableOn
  , rounded
  , selectionColor
  , subtle
  , success
  , textColor
  , tinted
  , windowColor
  , windowStyle
  , alignBottom
  , alignCenter
  , alignEnd
  , alignMid
  , alignStart
  , alignTop
  , base16TomorrowLight
  , base16TomorrowNight
  , defaultLayout
  , defaultTheme
  , fillH
  , fillW
  , fixedAspectH
  , fixedAspectW
  , fixedH
  , fixedW
  , fixedWH
  , fontBlack
  , fontBold
  , fontColor
  , fontDanger
  , fontExtraBold
  , fontHeading
  , fontItalic
  , fontLight
  , fontMedium
  , fontMono
  , fontMuted
  , fontOblique
  , fontRegular
  , fontSemiBold
  , fontSize
  , fontSizeScale
  , fontStrike
  , fontStyle
  , fontUnderline
  , fontWeight
  , gap
  , gridCols
  , gridMinColW
  , grow
  , maxH
  , maxW
  , minH
  , minW
  , padAll
  , padXY
  , panelPaintPad
  , percent
  , scrollBarThumbColor
  , scrollBarTrackColor
  , textDecoration
  , themeFromBase16
  , themeFromBase16Dark
  , themeFromBase16Light
  , themeSeries
  , tight
  , tomorrowMidnightMinDarkTheme
  , tomorrowMinLightTheme
  , tomorrowNightMinDarkTheme
  , windowMargin
  , windowPad
  )
import NanoUI.Types
  ( Color (..)
  , Damage (..)
  , DamageBounds (..)
  , ImageId (..)
  , Rect (..)
  , Size (..)
  , V2 (..)
  , colorA
  , colorB
  , colorG
  , colorLuminance
  , colorR
  , colorRGBA
  , colorToWord32
  , contrastRatio
  , defaultDamageSlop
  , haloDamageSlop
  , lerpColor
  , rectContains
  , rectInflate
  , rectIntersect
  , rectUnion
  , resolveDamageRect
  , sliderDamageSlop
  , v2Add
  , v2Sub
  )
import NanoUI.Widgets
import NanoUI.Widgets.Drop (DropTarget (..), dropZone, useDrop)
import NanoUI.Widgets.Node (setChanged, setClicked, setSubmitted)
import NanoUI.Widgets.Tabs
  ( Tab (..)
  , TabOrientation (..)
  , TabResponse (..)
  , TabStyle (..)
  , TabsConfig (..)
  , closableTab
  , defaultTabsConfig
  , tab
  , tabBar
  , tabBar'
  , tabBarConfigured
  , tabBarConfigured'
  , tabs
  , tabs'
  , tabsConfigured
  , tabsConfigured'
  )
import NanoUI.Widgets.TextCommon (MenuAction (..))
