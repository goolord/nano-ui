-- | The widget layer as one import: re-exports the widget modules.
module NanoUI.Widgets
  ( Response (..)
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
  , panel
  , panelWith
  , callout
  , calloutWith
  , row
  , rowWith
  , column
  , columnWith
  , hstack
  , vstack
  , grid
  , gridWith
  , center
  , responsive
  , responsiveRowCol
  , scroll
  , scrollWith
  , scroll2D
  , scroll2DWith
  , scrollArea
  , scrollArea2D
  , label
  , label'
  , labelWith
  , labelWith'
  , selectableText
  , selectableText'
  , selectableTextWith
  , selectableTextWith'
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
  , card
  , toolbar
  , separator
  , spacer
  , flex
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
  , button
  , button'
  , buttonWith
  , buttonWith'
  , checkbox
  , checkbox'
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
  , TreeItem (..)
  , tree
  , tree'
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
  , contextMenu
  , contextMenuArea
  , useContextMenu
  , menuButton
  , menuButton'
  , menuItem
  , menuItem'
  , menuItemShortcut
  , menuItemDisabled
  , menuSeparator
  , menuHeader
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
  , headed
  , headless
  , Colonnade
  , Headed (..)
  , PaneGridConfig (..)
  , defaultPaneGridConfig
  , PaneGridCtx (..)
  , PaneView (..)
  , PaneGridResponse (..)
  , GridAxis (..)
  , paneGrid
  , drawing
  , drawingVersioned
  , drawingCached
  , DrawOp (..)
  , DrawingBuild
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
  , knob
  , knob'
  , knobWith
  , knobWith'
  , toggleSwitch
  , toggleSwitch'
  , toggleSwitchWith
  , toggleSwitchWith'
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
  , progressBar
  , progressBar'
  , progressBarWith
  , progressBarWith'
  , sparkline
  , sparkline'
  , sparklineWith
  , sparklineWith'
  , Transition (..)
  , animate
  , animateTo
  , animateToA
  , pulse
  , keepAnimating
  , useState
  , useFlag
  , useText
  , useInt
  , useFloat
  , useEnum
  , useToggle
  )
where

import NanoUI.Hooks (useEnum, useFlag, useFloat, useInt, useState, useText, useToggle)
import NanoUI.WidgetText (colorFromHex, colorToHex, colorToHexA)
import NanoUI.Widgets.Animate (Transition (..), animate, animateTo, animateToA, keepAnimating, pulse)
import NanoUI.Widgets.Button (button, button', buttonWith, buttonWith')
import NanoUI.Widgets.Checkbox (checkbox, checkbox')
import NanoUI.Widgets.ColorPicker (colorPicker, colorPicker', colorPickerRGBA, colorPickerRGBA')
import NanoUI.Widgets.Combo (comboBox, comboBox')
import NanoUI.Widgets.RichText (Inline, emphasis, hyperlink, inlineCode, inlineText, inlineWith, restyle, richText, richText', richTextWith, richTextWith', strong)
import NanoUI.Widgets.Custom
  ( CanvasM
  , CustomDrawBuild
  , CustomDrawContext (..)
  , CustomMeasureFn
  , CustomWidgetSpec (..)
  , Drag2D (..)
  , canvas
  , circularProgress
  , circularProgress'
  , circularProgressWith
  , circularProgressWith'
  , spinner
  , spinner'
  , spinnerWith
  , spinnerWith'
  , customWidget
  , customWidgetWithId
  , contentKey
  , defaultCustomWidgetSpec
  , drawCircle
  , drawImage
  , drawImageUV
  , drawLinearGradientH
  , drawLinearGradientV
  , drawQuadGradient
  , drawRect
  , drawRoundedRect
  , drawStroke
  , drawStrokeAA
  , drawStrokeCircle
  , drawStrokeRoundedRect
  , drawText
  , knob
  , knob'
  , knobWith
  , knobWith'
  , progressBar
  , progressBar'
  , progressBarWith
  , progressBarWith'
  , runCanvas
  , sparkline
  , sparkline'
  , sparklineWith
  , sparklineWith'
  , toggleSwitch
  , toggleSwitch'
  , toggleSwitchWith
  , toggleSwitchWith'
  , useDrag2D
  , useWheelDelta
  )
import NanoUI.Svg (Svg, parseSvg, svgSize)
import NanoUI.Widgets.Display
  ( bold
  , box
  , card
  , danger
  , freshImageId
  , heading
  , image
  , image'
  , italic
  , kv
  , kvBlock
  , kvMono
  , mono
  , muted
  , registerImageRgba
  , loadSvg
  , svgIcon
  , svgIconWith
  , svgIconWith'
  , toolbar
  , underline
  )
import NanoUI.Widgets.Drawing (DrawOp (..), DrawingBuild, drawing, drawingCached, drawingVersioned)
import NanoUI.Widgets.Layout
  ( callout
  , calloutWith
  , center
  , column
  , columnWith
  , flex
  , grid
  , gridWith
  , hstack
  , label
  , label'
  , vstack
  , labelWith
  , labelWith'
  , panel
  , panelWith
  , responsive
  , responsiveRowCol
  , row
  , rowWith
  , scroll
  , scroll2D
  , scroll2DWith
  , scrollArea
  , scrollArea2D
  , scrollWith
  , separator
  , spacer
  )
import NanoUI.Widgets.Menu
  ( contextMenu
  , contextMenuArea
  , menuButton
  , menuButton'
  , menuHeader
  , menuItem
  , menuItem'
  , menuItemDisabled
  , menuItemShortcut
  , menuSeparator
  , useContextMenu
  )
import NanoUI.Widgets.Node
  ( HasResponse (..)
  , Response (..)
  , respChanged
  , respClicked
  , respHovered
  , respId
  , respPressed
  , respRect
  , respRightClicked
  , respRightPressed
  , respSubmitted
  )
import NanoUI.Widgets.NumericInput (NumericInputConfig (..), defaultNumericInputConfig, numericInput, numericInput', numericInputConfigured, numericInputConfigured')
import NanoUI.Widgets.Overlay (modal, window)
import NanoUI.Widgets.PaneGrid
  ( GridAxis (..)
  , PaneGridConfig (..)
  , PaneGridCtx (..)
  , PaneGridResponse (..)
  , PaneView (..)
  , defaultPaneGridConfig
  , paneGrid
  )
import NanoUI.Widgets.Popup
  ( PopupAnchor (..)
  , PopupConfig (..)
  , PopupPlacement (..)
  , defaultPopupConfig
  , popup
  , popupWith
  , tooltip
  , tooltipAt
  , tooltipWidget
  , withTooltip
  )
import NanoUI.Widgets.Radio (boundedRadio, boundedRadio', enumRadio, enumRadio', radio, radio')
import NanoUI.Widgets.Select
  ( boundedSelect
  , boundedSelect'
  , enumSelect
  , enumSelect'
  , select
  , select'
  , selectWith
  , selectWith'
  )
import NanoUI.Widgets.Slider (slider, slider', sliderWith, sliderWith')
import NanoUI.Widgets.Table
  ( ColSize (..)
  , Colonnade
  , Headed (..)
  , SortCol (..)
  , SortDir (..)
  , TableConfig (..)
  , TableResponse (..)
  , defaultTableConfig
  , headed
  , headless
  , simpleTable
  , sortRows
  , table
  , tableConfigured
  , tableHiddenIndices
  , tableWith
  , useTableSort
  )
import NanoUI.Widgets.TextArea (textArea, textArea', textAreaWith, textAreaWith')
import NanoUI.Widgets.TextInput
  ( SearchFieldConfig (..)
  , TextInputConfig (..)
  , defaultSearchFieldConfig
  , defaultTextInputConfig
  , searchField
  , searchField'
  , searchFieldConfigured
  , searchFieldConfigured'
  , selectableText
  , selectableText'
  , selectableTextWith
  , selectableTextWith'
  , textInput
  , textInput'
  , textInputConfigured
  , textInputConfigured'
  )
import NanoUI.Widgets.Tree (TreeItem (..), tree, tree')
