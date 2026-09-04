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
  , Layout (..)
  , defaultLayout
  , Style (..)
  , Theme (..)
  , defaultTheme
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
  , fixedW
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
  , label
  , labelEx
  , label_
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
  , scroll2D
  , scrollArea
  , scrollArea2D
  , scrollAreaId
  , scrollAreaIdConfigured
  , scrollConfigured
  , select
  , colorPicker
  , colorPickerToHex
  , colorPickerFromHex
  , radioFieldset
  , boundedRadioFieldset
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
  , image
  , box
  , drawing
  , drawingCached
  , DrawOp (..)
  , DrawingBuild
  , drawTextBox
  , shiftDrawOp
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
  , checkboxControlled
  , sliderControlled
  , textInputControlled
  , buttonEmit
  , checkboxEmit
  , sliderEmit
  , selectEmit
  , textInputEmit
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
  , monospaceMetrics
  , lineWidth
  , labelContentInset
  , tableCellInset
  , widgetContentInset
  , widgetPadding
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
import NanoUI.Context (Ease (..), applyEase)
import NanoUI.Draw (drawTextBox, shiftDrawOp)
import NanoUI.Font
  ( FontMetrics (..)
  , GlyphQuad (..)
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
  , appendInputKey
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
  , fontHeading
  , fontMono
  , fontMuted
  , fontRegular
  , themeSeries
  , fillH
  , fillW
  , fixedH
  , fixedW
  , fixedWH
  , gap
  , grow
  , minW
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
  , labelEx
  , label_
  , modal
  , muted
  , mono
  , styledLabel
  , onClick
  , panel
  , radioFieldset
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
  , scroll2D
  , scrollArea
  , scrollArea2D
  , scrollAreaIdConfigured
  , scrollConfigured
  , select
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
  , textArea
  , toolbar
  , tree
  , useFlag
  , useRadio
  , useText
  , useToggle
  , window
  )
import NanoUI.Widgets.Node (setSubmitted)
import NanoUI.Widgets.Combinators (stripedRow)
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
  , panel'
  , panelWith
  , panel_
  , row'
  , rowWith
  , row_
  , scrollAreaId
  , scrollWith
  , scroll_
  , vGroup
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
  , useState
  )
import NanoUI.Frame
  ( FrameResult (..)
  , FrameReduceResult (..)
  , runFrameResult
  , runFrameReduceResult
  )
