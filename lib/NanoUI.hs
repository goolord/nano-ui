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
  , lerpColor
  , contrastRatio
  , ImageId (..)
  , rectContains
  , rectIntersect
  , v2Add
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
  , currentId
  , scope
  , keyed
  , keyedTag
  , withKey
  -- Widgets
  , Response (..)
  , Responding (..)
  , Clickable (..)
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
  , separator
  , spacer
  , tooltip
  , scroll
  , scrollArea
  , select
  , colorPicker
  , colorPickerToHex
  , colorPickerFromHex
  , radioFieldset
  , boundedRadioFieldset
  , useRadio
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
  , image
  , box
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
  , labelContentInset
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
import NanoUI.Font
  ( FontMetrics (..)
  , GlyphQuad (..)
  , hasMonoFontMarker
  , headingFontMarker
  , labelContentInset
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
  , emit
  , keyed
  , keyedTag
  , nextId
  , runNanoUI
  , runUi
  , scope
  , uiIO
  , withKey
  )
import NanoUI.Spring
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
  , ImageId (..)
  , Rect (..)
  , Size (..)
  , V2 (..)
  , colorB
  , colorG
  , colorLuminance
  , colorR
  , colorRGBA
  , colorToWord32
  , contrastRatio
  , lerpColor
  , rectContains
  , rectIntersect
  , v2Add
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
  , scroll
  , scrollArea
  , select
  , sep
  , separator
  , slider
  , sliderEx
  , spacer
  , textInput
  , toolbar
  , tooltip
  , tree
  , useFlag
  , useRadio
  , useText
  , useToggle
  , window
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
