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
  , wrap
  , tight
  , percent
  , aspect
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
  , panel
  , row
  , column
  , label
  , labelEx
  , label_
  , button
  , checkbox
  , slider
  , sliderEx
  , textInput
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
  , scroll2D
  , scrollArea
  , scrollArea2D
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
  , kv
  , kvBlock
  , card
  , toolbar
  , sep
  , flex
  , image_
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
  , monoFontMarker
  , hasMonoFontMarker
  , stripMonoFontMarker
  , headingFontMarker
  , mutedFontMarker
  , stripWidgetMarkers
  , scrollBarGutter
  , scrollBarPageExtra
  , scrollBarListExtra
  , scrollBarWidth
  , scrollBarWindowGutter
  )
where

import NanoUI.Animatable (Animatable (..))
import NanoUI.Compact (Compact, askCompact, compactHost)
import NanoUI.Context (Ease (..), applyEase)
import NanoUI.Draw (drawTextBox, shiftDrawOp)
import NanoUI.Font
  ( FontMetrics (..)
  , GlyphQuad (..)
  , hasMonoFontMarker
  , headingFontMarker
  , labelContentInset
  , tableCellInset
  , lineWidth
  , monoFontMarker
  , monospaceMetrics
  , mutedFontMarker
  , resolveLayoutGap
  , resolveLayoutPadding
  , scrollBarGutter
  , scrollBarListExtra
  , scrollBarPageExtra
  , scrollBarWidth
  , scrollBarWindowGutter
  , stripMonoFontMarker
  , stripWidgetMarkers
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
  , Layout (..)
  , Padding (..)
  , Sizing (..)
  , Style (..)
  , Theme (..)
  , alignEnd
  , alignMid
  , aspect
  , defaultLayout
  , defaultTheme
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
  , wrap
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
  , kvBlock
  , label
  , labelEx
  , label_
  , modal
  , muted
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
  , textArea
  , toolbar
  , tree
  , useFlag
  , useRadio
  , useText
  , useToggle
  , window
  )
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
  , tableRespChanged
  , tableRespClicked
  , tableHiddenIndices
  , useTableSort
  )
