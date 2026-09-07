{-# LANGUAGE OverloadedStrings #-}

module NanoUI
  ( -- Types
    V2 (..)
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
  , ImageId (..)
  , Damage (..)
  , DamageBounds (..)
  , defaultDamageSlop
  , sliderDamageSlop
  , haloDamageSlop
  , resolveDamageRect
  , rectContains
  , rectInflate
  , rectIntersect
  , rectUnion
  , v2Add
  , v2Sub
  -- Input
  , Input (..)
  , Key (..)
  , Modifiers (..)
  , DropType (..)
  , DropEvent (..)
  , emptyDropEvents
  , emptyInput
  , inputInteracted
  , inputPointerHeld
  , appendInputKey
  , emptyInputKeys
  , inputKeysElem
  , inputKeysFromList
  , inputKeysNull
  , foldInputKeys
  -- Style
  , Sizing (..)
  , Direction (..)
  , AlignX (..)
  , AlignY (..)
  , Padding (..)
  , FontVariant (..)
  , FontWeight (..)
  , FontStyle (..)
  , TextDecoration (..)
  , Layout (..)
  , defaultLayout
  , askDefaultLayout
  , withDefaultLayout
  , withLayout
  , Style (..)
  , Theme (..)
  , defaultTheme
  , tomorrowNightMinTheme
  , tomorrowNightMinDarkTheme
  , tomorrowLightTheme
  , tomorrowMinLightTheme
  , tomorrowMidnightMinTheme
  , tomorrowMidnightMinDarkTheme
  , Base16 (..)
  , Base16ColorScheme
  , base0a
  , base0b
  , base0c
  , base0d
  , base0e
  , base0f
  , themeFromBase16
  , themeFromBase16Dark
  , themeFromBase16Light
  , base16Theme
  , base16ToTheme
  , base16TomorrowNight
  , base16TomorrowLight
  , withTheme
  , setTheme
  , getTheme
  , themeSeries
  , scrollBarTrackColor
  , scrollBarThumbColor
  , panelPaintPad
  , windowPad
  , windowMargin
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
  , fixedAspectW
  , fixedAspectH
  , gridCols
  , cols
  , LayoutModifier
  , fontRegular
  , fontHeading
  , fontMuted
  , fontMono
  , fontDanger
  , fontSize
  , fontSizeScale
  , fontColor
  , textColor
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
  , fontStrikethrough
  -- ID
  , WidgetId (..)
  , IdContext
  , initialIdContext
  , mix64
  , mixFnv
  , widgetId
  , hashWidgetId
  -- Monad
  , NanoUI
  , Ui
  , runUi
  , runNanoUI
  , uiIO
  , emit
  , nextId
  , burstNextIds
  , currentId
  , scope
  , keyed
  , keyedTag
  , withKey
  , uiFontMetrics
  , uiTheme
  , setUiTheme
  , uiMousePos
  , damageWidgetNow
  , damageKeyNow
  , damageRectNow
  , damageGroupNow
  , damageFullNow
  -- Widgets
  , Response (..)
  , Responding (..)
  , Clickable (..)
  , RightClickable (..)
  , onRightClick
  , setSubmitted
  , panel
  , panel_
  , panelWith
  , panel'
  , panelBg
  , panelBgWith
  , panelBg'
  , panelStyled
  , panelStyledWith
  , panelStyled'
  , boxWith
  , callout
  , calloutWith
  , row
  , row_
  , rowWith
  , row'
  , column
  , column_
  , columnWith
  , column'
  , grid
  , grid_
  , gridWith
  , grid'
  , gridResponse
  , gridPanel
  , gridPanel_
  , gridPanelWith
  , gridPanel'
  , gridPanelResponse
  , gridAutoFit
  , gridAutoFit_
  , gridAutoFitWith
  , gridAutoFit'
  , gridAutoFitResponse
  , responsive
  , responsiveRowCol
  , windowAspect
  , hstack
  , hstackWith
  , vstack
  , vstackWith
  , label
  , label_
  , labelWith
  , labelEx
  , label'
  , button
  , checkbox
  , slider
  , sliderEx
  , textInput
  , TextInputConfig (..)
  , defaultTextInputConfig
  , textInputConfigured
  , textInputWithPlaceholder
  , textInputPassword
  , SearchFieldConfig (..)
  , defaultSearchFieldConfig
  , searchField
  , searchFieldConfigured
  , textArea
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
  , withContextMenu
  , contextMenuArea
  , useContextMenu
  , menuItem
  , menuItemWithShortcut
  , menuItemWithIcon
  , menuItemDisabled
  , menuSeparator
  , menuHeader
  , scroll
  , scroll_
  , scrollWith
  , scroll'
  , scroll2D
  , scroll2D_
  , scroll2DWith
  , scroll2D'
  , scrollArea
  , scrollArea2D
  , scrollAreaId
  , scrollAreaIdConfigured
  , scrollConfigured
  , select
  , selectWith
  , boundedSelect
  , enumSelect
  , useEnumSelect
  , colorPicker
  , colorPickerToHex
  , colorPickerFromHex
  , radioFieldset
  , boundedRadioFieldset
  , enumRadio
  , useEnumRadio
  , useRadio
  , stripedRow
  , TreeItem (..)
  , tree
  , modal
  , window
  , windowSize
  , windowWidth
  , windowHeight
  , Tab (..)
  , TabStyle (..)
  , TabOrientation (..)
  , TabResponse (..)
  , tabRespClicked
  , tabRespChanged
  , tab
  , closableTab
  , mkTab
  , tabs
  , tabsEx
  , tabBar
  , tabBarEx
  , tabsEmit
  , tabsEmitEx
  , useTab
  , useTabIdx
  , boundedTabs
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
  , tableRespChanged
  , tableRespClicked
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
  , image
  , box
  , drawing
  , drawingCached
  , DrawOp (..)
  , DrawingBuild
  , drawTextBox
  , shiftDrawOp
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
  , useClickGesture
  , ClickGesture (..)
  , DropTarget (..)
  , useDrop
  , onDrop
  , onDropHover
  , dropZone
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
  , onClick
  , clickButton
  , useFlag
  , useText
  , useToggle
  , animate
  , animateEase
  , animateEaseDelay
  , animateTo
  , animateToEase
  , animateToEaseDelay
  , animateToSpring
  , animateToA
  , animateToSpringA
  , Animatable (..)
  , heading
  , muted
  , mono
  , danger
  , bold
  , italic
  , underline
  , styledLabel
  , kv
  , kvMono
  , kvBlock
  , card
  , toolbar
  , sep
  , flex
  , center
  , flexRow
  , flexCol
  , hGroup
  , vGroup
  , image_
  , useState
  , useInt
  , useFloat
  , useEnum
  , checkboxControlled
  , sliderControlled
  , textInputControlled
  , buttonEmit
  , checkboxEmit
  , sliderEmit
  , selectEmit
  , textInputEmit
  , searchFieldEmit
  -- Animation
  , Ease (..)
  , applyEase
  , SpringParams (..)
  , presetBouncy
  , presetSmooth
  , presetStiff
  -- Compact
  , Compact
  , compactHost
  , askCompact
  -- Icons
  , IconSet (..)
  , Icons (..)
  , asciiIcons
  , glyphIcons
  , iconsFor
  , iconSetName
  , parseIconSet
  , checkboxMark
  , radioMark
  , treeExpandMark
  , fontAwesomeIcon
  , loneFontAwesome
  -- Font
  , FontMetrics (..)
  , GlyphQuad (..)
  , RunQuad (..)
  , scaleFontMetrics
  , monospaceMetrics
  , lineWidth
  , labelContentInset
  , tableCellInset
  , widgetContentInset
  , widgetPadding
  , treeItemPadding
  , resolveLayoutGap
  , resolveLayoutPadding
  , scrollBarGutter
  , scrollBarPageExtra
  , scrollBarListExtra
  , scrollBarWidth
  , scrollBarWindowGutter
  -- Frame
  , FrameResult (..)
  , FrameReduceResult (..)
  , runFrameResult
  , runFrameReduceResult
  )
where

import NanoUI.Animatable (Animatable (..))
import NanoUI.Compact (Compact, askCompact, compactHost)
import NanoUI.Context (Ease (..), applyEase, getTheme, setTheme, withTheme)
import NanoUI.Draw (drawTextBox, shiftDrawOp)
import NanoUI.Font
  ( FontMetrics (..)
  , GlyphQuad (..)
  , RunQuad (..)
  , scaleFontMetrics
  , labelContentInset
  , tableCellInset
  , lineWidth
  , monospaceMetrics
  , resolveLayoutGap
  , resolveLayoutPadding
  , scrollBarGutter
  , scrollBarListExtra
  , scrollBarPageExtra
  , scrollBarWidth
  , scrollBarWindowGutter
  , widgetContentInset
  , widgetPadding
  , treeItemPadding
  )
import NanoUI.Icons
  ( IconSet (..)
  , Icons (..)
  , asciiIcons
  , checkboxMark
  , fontAwesomeIcon
  , glyphIcons
  , iconSetName
  , iconsFor
  , loneFontAwesome
  , parseIconSet
  , radioMark
  , treeExpandMark
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
  ( Input (..)
  , Key (..)
  , Modifiers (..)
  , DropEvent (..)
  , DropType (..)
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
  , withDefaultLayout
  , withLayout
  , currentId
  , damageFullNow
  , damageGroupNow
  , damageKeyNow
  , damageRectNow
  , damageWidgetNow
  , emit
  , keyed
  , keyedTag
  , nextId
  , burstNextIds
  , runNanoUI
  , runUi
  , scope
  , uiIO
  , uiFontMetrics
  , uiTheme
  , setUiTheme
  , uiMousePos
  , windowSize
  , windowWidth
  , windowHeight
  , withKey
  )
import NanoUI.Animation
  ( SpringParams (..)
  , presetBouncy
  , presetSmooth
  , presetStiff
  )
import NanoUI.Style
  ( AlignX (..)
  , AlignY (..)
  , Direction (..)
  , FontVariant (..)
  , FontWeight (..)
  , FontStyle (..)
  , TextDecoration (..)
  , Layout (..)
  , LayoutModifier
  , Padding (..)
  , Sizing (..)
  , Style (..)
  , Theme (..)
  , alignBottom
  , alignCenter
  , alignEnd
  , alignMid
  , alignStart
  , alignTop
  , fixedAspectW
  , fixedAspectH
  , gridMinColW
  , cols
  , gridCols
  , defaultLayout
  , defaultTheme
  , tomorrowNightMinTheme
  , tomorrowNightMinDarkTheme
  , tomorrowLightTheme
  , tomorrowMinLightTheme
  , tomorrowMidnightMinTheme
  , tomorrowMidnightMinDarkTheme
  , Base16 (..)
  , Base16ColorScheme
  , base0a
  , base0b
  , base0c
  , base0d
  , base0e
  , base0f
  , themeFromBase16
  , themeFromBase16Dark
  , themeFromBase16Light
  , base16Theme
  , base16ToTheme
  , base16TomorrowNight
  , base16TomorrowLight
  , fontHeading
  , fontMuted
  , fontMono
  , fontDanger
  , fontRegular
  , fontSize
  , fontSizeScale
  , fontColor
  , textColor
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
  , fontStrikethrough
  , themeSeries
  , fillH
  , fillW
  , fixedH
  , fixedW
  , fixedWH
  , gap
  , grow
  , minW
  , maxW
  , minH
  , maxH
  , padAll
  , padXY
  , panelPaintPad
  , percent
  , scrollBarThumbColor
  , scrollBarTrackColor
  , tight
  , windowMargin
  , windowPad
  , gridMinColW
  , fixedAspectW
  , fixedAspectH
  )
import NanoUI.Types
  ( Color (..)
  , Damage (..)
  , DamageBounds (..)
  , ImageId (..)
  , Rect (..)
  , Size (..)
  , V2 (..)
  , colorB
  , colorG
  , colorLuminance
  , colorR
  , colorA
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
  ( Clickable (..)
  , Responding (..)
  , Response (..)
  , TreeItem (..)
  , animate
  , animateEase
  , animateEaseDelay
  , animateTo
  , animateToA
  , animateToEase
  , animateToEaseDelay
  , animateToSpring
  , animateToSpringA
  , boundedRadioFieldset
  , box
  , drawing
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
  , useClickGesture
  , ClickGesture (..)
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
  , button
  , card
  , checkbox
  , clickButton
  , colorPicker
  , colorPickerFromHex
  , colorPickerToHex
  , column
  , flex
  , heading
  , image
  , image_
  , kv
  , kvMono
  , kvBlock
  , label
  , label_
  , labelWith
  , labelEx
  , label'
  , modal
  , muted
  , mono
  , bold
  , italic
  , underline
  , styledLabel
  , onClick
  , panel
  , panelBg
  , panelBgWith
  , panelBg'
  , panelStyled
  , panelStyledWith
  , panelStyled'
  , boxWith
  , callout
  , calloutWith
  , danger
  , boundedSelect
  , enumSelect
  , useEnumSelect
  , radioFieldset
  , boundedRadioFieldset
  , enumRadio
  , useEnumRadio
  , row
  , RightClickable (..)
  , onRightClick
  , popup
  , popupEx
  , PopupAnchor (..)
  , PopupPlacement (..)
  , PopupConfig (..)
  , defaultPopupConfig
  , tooltip
  , tooltipWidget
  , tooltipWith
  , withTooltip
  , contextMenu
  , withContextMenu
  , contextMenuArea
  , useContextMenu
  , menuItem
  , menuItemWithShortcut
  , menuItemWithIcon
  , menuItemDisabled
  , menuSeparator
  , menuHeader
  , scroll
  , scroll_
  , scrollWith
  , scroll'
  , scroll2D
  , scroll2D_
  , scroll2DWith
  , scroll2D'
  , scrollArea
  , scrollArea2D
  , scrollAreaIdConfigured
  , scrollConfigured
  , select
  , selectWith
  , sep
  , separator
  , slider
  , sliderEx
  , spacer
  , textInput
  , TextInputConfig (..)
  , defaultTextInputConfig
  , textInputConfigured
  , textInputWithPlaceholder
  , textInputPassword
  , SearchFieldConfig (..)
  , defaultSearchFieldConfig
  , searchField
  , searchFieldConfigured
  , textArea
  , toolbar
  , tree
  , useFlag
  , useRadio
  , useText
  , useToggle
  , window
  , PaneGridConfig (..)
  , defaultPaneGridConfig
  , PaneGridCtx (..)
  , PaneView (..)
  , PaneGridResponse (..)
  , GridAxis (..)
  , paneGrid
  )
import NanoUI.Widgets.Node (setSubmitted)
import NanoUI.Widgets.Combinators (stripedRow)
import NanoUI.Widgets.Drop
  ( DropTarget (..)
  , dropZone
  , onDrop
  , onDropHover
  , useDrop
  )
import NanoUI.Widgets.Tabs
  ( Tab (..)
  , TabOrientation (..)
  , TabResponse (..)
  , TabStyle (..)
  , boundedTabs
  , closableTab
  , mkTab
  , tab
  , tabBar
  , tabBarEx
  , tabRespChanged
  , tabRespClicked
  , tabs
  , tabsEmit
  , tabsEmitEx
  , tabsEx
  , useTab
  , useTabIdx
  )
import NanoUI.Widgets.Table
  ( Colonnade
  , ColSize (..)
  , Headed (..)
  , SortCol (..)
  , SortDir (..)
  , TableCfg (..)
  , TableResponse (..)
  , defaultTableCfg
  , headed
  , headless
  , sortRows
  , table
  , tableCfg
  , tableEx
  , simpleTable
  , tableRespChanged
  , tableRespClicked
  , tableHiddenIndices
  , useTableSort
  )
import NanoUI.Widgets.Layout
  ( center
  , column'
  , columnWith
  , column_
  , grid
  , grid_
  , gridWith
  , grid'
  , gridResponse
  , gridPanel
  , gridPanel_
  , gridPanelWith
  , gridPanel'
  , gridPanelResponse
  , gridAutoFit
  , gridAutoFit_
  , gridAutoFitWith
  , gridAutoFit'
  , gridAutoFitResponse
  , responsive
  , responsiveRowCol
  , windowAspect
  , flexCol
  , flexRow
  , hGroup
  , hstack
  , hstackWith
  , panel'
  , panelWith
  , panel_
  , row'
  , row_
  , rowWith
  , scrollAreaId
  , vGroup
  , vstack
  , vstackWith
  )
import NanoUI.State
  ( buttonEmit
  , checkboxControlled
  , checkboxEmit
  , selectEmit
  , sliderControlled
  , sliderEmit
  , textInputControlled
  , textInputEmit
  , searchFieldEmit
  , useEnum
  , useFloat
  , useInt
  , useState
  )
import NanoUI.Frame
  ( FrameResult (..)
  , FrameReduceResult (..)
  , runFrameResult
  , runFrameReduceResult
  )
