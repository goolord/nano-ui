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
  , onClick
  , onRightClick
  , panel
  , panelWith
  , panel'
  , panelStyled
  , panelStyledWith
  , panelStyled'
  , callout
  , calloutWith
  , row
  , rowWith
  , row'
  , column
  , columnWith
  , column'
  , grid
  , gridWith
  , grid'
  , label
  , labelWith
  , labelEx
  , selectableText
  , selectableTextWith
  , selectableTextEx
  , button
  , button'
  , buttonWith
  , buttonWith'
  , button_
  , buttonEx
  , checkbox
  , slider
  , sliderWith
  , sliderEx
  , textInput
  , SearchFieldConfig (..)
  , defaultSearchFieldConfig
  , searchField
  , searchFieldConfigured
  , comboBox
  , textArea
  , textAreaWith
  , separator
  , spacer
  , tooltip
  , tooltipWidget
  , tooltipWith
  , withTooltip
  , popup
  , popupEx
  , PopupAnchor (..)
  , PopupPlacement (..)
  , PopupConfig (..)
  , defaultPopupConfig
  , contextMenu
  , contextMenuArea
  , useContextMenu
  , menuButton
  , MenuItem (..)
  , menuItemWith
  , menuItem
  , menuItemWithShortcut
  , menuItemDisabled
  , menuSeparator
  , menuHeader
  , scroll
  , scrollWith
  , scroll'
  , scroll2DWith
  , scroll2D'
  , scrollArea
  , scrollArea2D
  , scrollAreaIdConfigured
  , scrollConfigured
  , select
  , selectWith
  , selectLabeled
  , boundedSelect
  , enumSelect
  , radioFieldset
  , boundedRadioFieldset
  , enumRadio
  , TreeItem (..)
  , tree
  , colorPicker
  , colorPickerRGBA
  , modal
  , window
  , image
  , label_
  , useState
  , useFlag
  , useText
  , useInt
  , useFloat
  , useEnum
  , useToggle
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
  , TextInputConfig (..)
  , defaultTextInputConfig
  , textInputConfigured
  , textInputWithPlaceholder
  , textInputPassword
  , card
  , toolbar
  , sep
  , flex
  , image_
  , box
  , drawing
  , drawingVersioned
  , drawingCached
  , DrawOp (..)
  , DrawingBuild
  , CustomWidgetSpec (..)
  , defaultCustomWidgetSpec
  , customWidget
  , customWidget_
  , customWidgetWithId
  , CustomDrawContext (..)
  , CustomMeasureFn
  , CustomDrawBuild
  , CanvasM
  , runCanvas
  , canvas
  , canvasWith
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
  , knobWith
  , toggleSwitch
  , toggleSwitchWith
  , circularProgress
  , circularProgressWith
  , progressBar
  , progressBarWith
  , sparkline
  , sparklineWith
  , Transition (..)
  , animate
  , animateTo
  , animateToA
  , pulse
  , keepAnimating
  , colorPickerToHex
  , colorPickerToHexA
  , colorPickerFromHex
  , SortDir (..)
  , SortCol (..)
  , ColSize (..)
  , TableCfg (..)
  , TableResponse (..)
  , defaultTableCfg
  , table
  , tableEx
  , tableCfg
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
  )
where

import NanoUI.Hooks (useEnum, useFlag, useFloat, useInt, useState, useText, useToggle)
import NanoUI.WidgetText (colorPickerFromHex, colorPickerToHex, colorPickerToHexA)
import NanoUI.Widgets.Animate (Transition (..), animate, animateTo, animateToA, keepAnimating, pulse)
import NanoUI.Widgets.Button (button, button', buttonEx, buttonWith, buttonWith', button_)
import NanoUI.Widgets.Checkbox (checkbox)
import NanoUI.Widgets.ColorPicker (colorPicker, colorPickerRGBA)
import NanoUI.Widgets.Combo (comboBox)
import NanoUI.Widgets.Custom
  ( CanvasM
  , CustomDrawBuild
  , CustomDrawContext (..)
  , CustomMeasureFn
  , CustomWidgetSpec (..)
  , Drag2D (..)
  , canvas
  , canvasWith
  , circularProgress
  , circularProgressWith
  , customWidget
  , customWidgetWithId
  , customWidget_
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
  , knobWith
  , progressBar
  , progressBarWith
  , runCanvas
  , sparkline
  , sparklineWith
  , toggleSwitch
  , toggleSwitchWith
  , useDrag2D
  , useWheelDelta
  )
import NanoUI.Widgets.Display
  ( bold
  , box
  , card
  , danger
  , heading
  , image
  , image_
  , italic
  , kv
  , kvBlock
  , kvMono
  , label_
  , mono
  , muted
  , toolbar
  , underline
  )
import NanoUI.Widgets.Drawing (DrawOp (..), DrawingBuild, drawing, drawingCached, drawingVersioned)
import NanoUI.Widgets.Layout
  ( callout
  , calloutWith
  , column
  , column'
  , columnWith
  , flex
  , grid
  , grid'
  , gridWith
  , label
  , labelEx
  , labelWith
  , panel
  , panel'
  , panelStyled
  , panelStyled'
  , panelStyledWith
  , panelWith
  , row
  , row'
  , rowWith
  , scroll
  , scroll'
  , scroll2D'
  , scroll2DWith
  , scrollArea
  , scrollArea2D
  , scrollAreaIdConfigured
  , scrollConfigured
  , scrollWith
  , sep
  , separator
  , spacer
  )
import NanoUI.Widgets.Menu
  ( MenuItem (..)
  , contextMenu
  , contextMenuArea
  , menuButton
  , menuHeader
  , menuItem
  , menuItemDisabled
  , menuItemWith
  , menuItemWithShortcut
  , menuSeparator
  , useContextMenu
  )
import NanoUI.Widgets.Node
  ( HasResponse (..)
  , Response (..)
  , onClick
  , onRightClick
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
  , popupEx
  , tooltip
  , tooltipWidget
  , tooltipWith
  , withTooltip
  )
import NanoUI.Widgets.Radio (boundedRadioFieldset, enumRadio, radioFieldset)
import NanoUI.Widgets.Select (boundedSelect, enumSelect, select, selectLabeled, selectWith)
import NanoUI.Widgets.Slider (slider, sliderEx, sliderWith)
import NanoUI.Widgets.Table
  ( ColSize (..)
  , Colonnade
  , Headed (..)
  , SortCol (..)
  , SortDir (..)
  , TableCfg (..)
  , TableResponse (..)
  , defaultTableCfg
  , headed
  , headless
  , simpleTable
  , sortRows
  , table
  , tableCfg
  , tableEx
  , tableHiddenIndices
  , useTableSort
  )
import NanoUI.Widgets.TextArea (textArea, textAreaWith)
import NanoUI.Widgets.TextInput
  ( SearchFieldConfig (..)
  , TextInputConfig (..)
  , defaultSearchFieldConfig
  , defaultTextInputConfig
  , searchField
  , searchFieldConfigured
  , selectableText
  , selectableTextEx
  , selectableTextWith
  , textInput
  , textInputConfigured
  , textInputPassword
  , textInputWithPlaceholder
  )
import NanoUI.Widgets.Tree (TreeItem (..), tree)
