{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : NanoUI
-- Description : Purely functional immediate-mode GUI toolkit for Haskell
-- Copyright   : (c) 2026 Zachary Churchill
-- License     : MIT
-- Maintainer  : zacharyachurchill@gmail.com
--
-- @nano-ui@ is an immediate-mode graphical user interface (IMGUI) toolkit for Haskell.
--
-- = Immediate-Mode Mental Model
--
-- Unlike traditional retained-mode frameworks (such as the HTML DOM, Qt, or GTK),
-- @nano-ui@ has:
--
-- * __Zero retained widget objects__: You do not instantiate or store widget handles.
-- * __Zero mutable synchronization__: Widget state is not synchronized through getters and setters.
-- * __Zero callback chains__: Widgets do not register event listeners.
--
-- Instead, your application describes the entire user interface /every single frame/ as a pure
-- function of state. Widgets evaluate on the spot and return what the user did during that frame.
--
-- = Two State Paradigms
--
-- @nano-ui@ supports two complementary state management architectures:
--
-- == 1. Local Component Hooks
--
-- Best suited for transient UI state (dialog visibility, tab selection, form field drafts).
--
-- @
-- counterApp :: NanoUI ()
-- counterApp = do
--   (count, setCount) <- useInt 0
--   column $ do
--     heading "Counter"
--     rowWith (gap 8) $ do
--       whenM (button "-") (setCount (count - 1))
--       label (T.pack (show count))
--       whenM (button "+") (setCount (count + 1))
-- @
--
-- == 2. Pure Elm Architecture (Reducers & Emitters)
--
-- Best suited for deterministic, replayable, application-wide domain state.
--
-- @
-- data Msg = Increment | Decrement
--
-- update :: Msg -> Int -> Int
-- update Increment n = n + 1
-- update Decrement n = n - 1
--
-- view :: Int -> NanoUI ()
-- view count = column $ do
--   heading "Elm-Style Counter"
--   rowWith (gap 8) $ do
--     buttonEmit "-" Decrement
--     label (T.pack (show count))
--     buttonEmit "+" Increment
-- @
--
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
  , getStoreBool
  , setStoreBool
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
  , LayoutModifier
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
  , setChanged
  , setClicked
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
  , onRightClick
  , setSubmitted
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
  , responsive
  , responsiveRowCol
  , hstack
  , vstack
  , label
  , label_
  , labelWith
  , labelEx
  , selectableText
  , selectableTextWith
  , selectableTextEx
  , MenuAction (..)
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
  , TextInputConfig (..)
  , defaultTextInputConfig
  , textInputConfigured
  , textInputWithPlaceholder
  , textInputPassword
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
  , scrollAreaId
  , scrollAreaIdConfigured
  , scrollConfigured
  , select
  , selectWith
  , selectLabeled
  , boundedSelect
  , enumSelect
  , colorPicker
  , colorPickerRGBA
  , colorPickerToHex
  , colorPickerToHexA
  , colorPickerFromHex
  , radioFieldset
  , boundedRadioFieldset
  , enumRadio
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
  , TabsConfig (..)
  , defaultTabsConfig
  , tab
  , closableTab
  , tabs
  , tabsWith
  , tabBar
  , tabBarWith
  , tabsEmit
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
  , drawingVersioned
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
  , whenM
  , unlessM
  , ifM
  , useFlag
  , useText
  , useToggle
  , Transition (..)
  , animate
  , animateTo
  , animateToA
  , pulse
  , keepAnimating
  , Animatable (..)
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
  , sep
  , flex
  , center
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
  -- Font
  , FontMetrics (..)
  , FontBackend (..)
  , prepareFontMetrics
  , prepareFontMetricsMany
  , measureTextIO
  , lineWidthIO
  , drawRun
  , drawGlyph
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
  )
where

import NanoUI.Animatable (Animatable (..))
import NanoUI.Compact (Compact, askCompact, compactHost)
import NanoUI.Context (Ease (..), applyEase, getStoreBool, getTheme, setStoreBool, setTheme, withTheme)
import NanoUI.Draw (drawTextBox, shiftDrawOp)
import NanoUI.Font
  ( FontMetrics (..)
  , FontBackend (..)
  , prepareFontMetrics
  , prepareFontMetricsMany
  , measureTextIO
  , lineWidthIO
  , drawRun
  , drawGlyph
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
  , whenM
  , unlessM
  , ifM
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
  , gridCols
  , defaultLayout
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
  , fontHeading
  , fontMuted
  , fontMono
  , fontDanger
  , fontRegular
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
  ( HasResponse (..)
  , Response (..)
  , TreeItem (..)
  , respId
  , respRect
  , respHovered
  , respPressed
  , respClicked
  , respChanged
  , respSubmitted
  , respRightPressed
  , respRightClicked
  , Transition (..)
  , animate
  , animateTo
  , animateToA
  , pulse
  , keepAnimating
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
  , button
  , button'
  , buttonWith
  , buttonWith'
  , button_
  , buttonEx
  , card
  , checkbox
  , colorPicker
  , colorPickerFromHex
  , colorPickerRGBA
  , colorPickerToHex
  , colorPickerToHexA
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
  , selectableText
  , selectableTextWith
  , selectableTextEx
  , modal
  , muted
  , mono
  , bold
  , italic
  , underline
  , onClick
  , panel
  , panelStyled
  , panelStyledWith
  , panelStyled'
  , callout
  , calloutWith
  , danger
  , boundedSelect
  , enumSelect
  , radioFieldset
  , boundedRadioFieldset
  , enumRadio
  , row
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
  , sep
  , separator
  , slider
  , sliderWith
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
  , comboBox
  , textArea
  , textAreaWith
  , toolbar
  , tree
  , useFlag
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
import NanoUI.Widgets.Node (setChanged, setClicked, setSubmitted)
import NanoUI.Widgets.TextCommon (MenuAction (..))
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
  , TabsConfig (..)
  , boundedTabs
  , closableTab
  , defaultTabsConfig
  , tab
  , tabBar
  , tabBarWith
  , tabs
  , tabsEmit
  , tabsWith
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
  , tableHiddenIndices
  , useTableSort
  )
import NanoUI.Widgets.Layout
  ( center
  , column'
  , columnWith
  , grid
  , gridWith
  , grid'
  , responsive
  , responsiveRowCol
  , hstack
  , panel'
  , panelWith
  , row'
  , rowWith
  , scrollAreaId
  , vstack
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
