-- | Wrapped nano-ui widgets operating directly in 'NanoUIE'.
--
-- Every widget exported by core "NanoUI" is wrapped here so that views
-- using reducer-style emission do not have to manually lift or wrap ordinary
-- widgets.
module NanoUI.Emit.Widgets
  ( -- * Containers
    row
  , rowWith
  , rowWith'
  , column
  , columnWith
  , columnWith'
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
  , layers
  , layersWith
  , mouseArea

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

    -- * Inputs
  , checkbox
  , checkbox'
  , checkboxWith
  , checkboxWith'
  , toggleButton
  , toggleButton'
  , toggleButtonWith
  , toggleButtonWith'
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
  , textInput
  , textInput'
  , textInputConfigured
  , textInputConfigured'
  , numericInput
  , numericInput'
  , numericInputConfigured
  , numericInputConfigured'
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
  , colorPicker
  , colorPicker'
  , colorPickerRGBA
  , colorPickerRGBA'

    -- * Tabs, trees, and tables
  , tabs
  , tabs'
  , tabsConfigured
  , tabsConfigured'
  , tabBar
  , tabBar'
  , tabBarConfigured
  , tabBarConfigured'
  , boundedTabs
  , boundedTabsConfigured
  , enumTabs
  , boundedTabBar
  , boundedTabBarConfigured
  , enumTabBar
  , tree
  , tree'
  , table
  , table'
  , tableWith
  , tableWith'
  , tableConfigured
  , tableConfigured'
  , simpleTable

    -- * Overlays
  , modal
  , modal'
  , modalWith
  , modalWith'
  , modalPanel
  , modalPanel'
  , modalPanelWith
  , modalPanelWith'
  , window
  , window'
  , popup
  , popup'
  , popupWith
  , popupWith'
  , tooltip
  , tooltipAt
  , tooltipWidget
  , withTooltip
  , tooltipConfigured
  , tooltipWidgetConfigured

    -- * Window caption
  , captionButton
  , captionButtons
  , captionButtonsConfigured
  , dragSpans
  , windowFrame

    -- * Pane grids
  , paneGrid
  , paneDragHandle

    -- * Rich text
  , richText
  , richText'
  , richTextWith
  , richTextWith'
  , selectableRichText
  , selectableRichText'
  , selectableRichTextWith
  , selectableRichTextWith'

    -- * Images and drawing
  , image
  , image'
  , imageConfigured
  , imageConfigured'
  , svgIcon
  , svgIconWith
  , svgIconWith'
  , svgIconConfigured
  , svgIconConfigured'
  , box
  , drawing
  , drawingVersioned
  , drawingCached

    -- * Custom widgets
  , toggleSwitch
  , toggleSwitch'
  , toggleSwitchWith
  , toggleSwitchWith'
  , switch
  , switch'
  , switchWith
  , switchWith'
  , knob
  , knob'
  , knobWith
  , knobWith'
  , circularProgress
  , circularProgress'
  , circularProgressWith
  , circularProgressWith'
  , progressBar
  , progressBar'
  , progressBarWith
  , progressBarWith'
  , spinner
  , spinner'
  , spinnerWith
  , spinnerWith'
  , sparkline
  , sparkline'
  , sparklineWith
  , sparklineWith'
  , canvas
  , canvasConfigured
  , customWidget

    -- * Scopes and behavior
  , scope
  , withKey
  , styled
  , themed
  , disabledWhen
  , withDefaultLayout
  , withCursorShape
  , dropZone
  , useDrop
  , useDrag
  , useReorder
  , sensor
  , sensorWith
  , sensorConfigured
  , useVisibility

    -- * Focus and clipboard
  , holdFocus
  , requestFocus
  , focusNext
  , focusPrevious
  , clearFocus
  , isFocused
  , getClipboard
  , setClipboard

    -- * State and tasks
  , useVar
  , useEnumVar
  , readVar
  , writeVar
  , modifyVar
  , useTaskStatus
  , useTask
  , useStream
  , askWake

    -- * View queries and inputs
  , windowSize
  , windowWidth
  , windowHeight
  , uiMousePos
  , askInput
  , uiTime
  , requestFrame
  , lastRect
  , currentId
  , nextId
  , keyPressed
  , keyPressedOnce
  , keyReleased
  , keyHeld
  , focusedKeyPressed
  , focusedKeyPressedOnce
  , useKeyNav
  , keyActivated
  , shortcut
  , shortcutOnce
  , mousePressed
  , mouseReleased
  , mouseHeld
  , useInputMethod
  , keepAnimating
  , wakeAfter
  , animateTo
  , animateToA
  , animate
  , pulse
  , uiTheme
  , askDefaultLayout
  , askWindow
  , askWindowCapabilities
  , setWindowTitleUi
  , setWindowIconUi
  , setWindowMinSizeUi
  , setWindowMaxSizeUi
  , setWindowOpacityUi
  , setWindowModeUi
  , moveWindowUi
  , centerWindowUi
  , resizeWindowUi
  , minimizeWindowUi
  , maximizeWindowUi
  , restoreWindowUi
  , toggleMaximizedUi
  , quitUi
  , getScrollMetricsUi
  , setScrollOffsetUi
  , scrollToUi
  , scrollByUi
  , scrollPagesUi
  , scrollRectIntoViewUi
  , setScrollStepUi
  , useImageRgba
  , requestScreenshot
  , askScreenshot
  , useScreenshot
  , explainLayout
  , explainingLayout
  , explainedNode
  , explainScope
  )
where

import Data.ByteString (ByteString)
import Data.Foldable (toList)
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Typeable (Typeable)

import Colonnade (Colonnade, Headed)
import NanoUI qualified as UI
import NanoUI
  ( Animatable
  , ButtonConfig
  , CanvasConfig
  , CanvasM
  , CaptionAction
  , CaptionConfig
  , CaptionGlyph
  , Color
  , Composition
  , CustomWidgetSpec
  , Drag
  , DragHandle
  , DrawingBuild
  , DropTarget
  , ExplainedNode
  , HasResponse (..)
  , ImageConfig
  , ImageHandle
  , ImageId
  , Inline
  , Input
  , InputPurpose (..)
  , Key
  , KeyNav
  , Layout
  , LayoutModifier
  , MouseButton
  , NanoUI
  , NumericInputConfig
  , PaneGridConfig
  , PaneGridCtx
  , PaneGridResponse
  , PopupConfig
  , PopupPlacement (..)
  , Rect (..)
  , Reorder
  , Response
  , RgbaPixels (..)
  , Screenshot (..)
  , ScrollBehavior
  , ScrollAlign
  , ScrollMetrics
  , SearchInputConfig
  , SensorConfig
  , Size
  , Sizing (..)
  , SortCol
  , Stream
  , Svg
  , Tab (..)
  , TabResponse
  , TabsConfig
  , TableConfig
  , TableResponse
  , Task
  , TaskStatus
  , TextDocument
  , TextInputConfig
  , Theme
  , ToCell
  , Tone (..)
  , TooltipConfig
  , Transition
  , TreeItem
  , UiCursorKind
  , V2
  , Var
  , VarSlot
  , Visibility
  , WidgetId
  , WindowCapabilities
  , WindowFrame
  , WindowMode
  , WindowState
  )
import NanoUI.Shortcut (Shortcut)
import NanoUI.Emit.Internal (NanoUIE, liftNanoUI, withNanoUI, withRunInNanoUIE)

-- Containers

-- | Lay out children horizontally.
{-# INLINE row #-}
row :: NanoUIE msg a -> NanoUIE msg a
row = withNanoUI UI.row

-- | Horizontal layout with a modifier.
{-# INLINE rowWith #-}
rowWith :: LayoutModifier -> NanoUIE msg a -> NanoUIE msg a
rowWith = withNanoUI . UI.rowWith

-- | Horizontal layout returning the child's result and container's 'Response'.
{-# INLINE rowWith' #-}
rowWith' :: LayoutModifier -> NanoUIE msg a -> NanoUIE msg (a, Response)
rowWith' lm body = withNanoUI (UI.rowWith' lm) body

-- | Lay out children vertically.
{-# INLINE column #-}
column :: NanoUIE msg a -> NanoUIE msg a
column = withNanoUI UI.column

-- | Vertical layout with a modifier.
{-# INLINE columnWith #-}
columnWith :: LayoutModifier -> NanoUIE msg a -> NanoUIE msg a
columnWith = withNanoUI . UI.columnWith

-- | Vertical layout returning the child's result and container's 'Response'.
{-# INLINE columnWith' #-}
columnWith' :: LayoutModifier -> NanoUIE msg a -> NanoUIE msg (a, Response)
columnWith' lm body = withNanoUI (UI.columnWith' lm) body

-- | Compact horizontal stack.
{-# INLINE hstack #-}
hstack :: Foldable f => f (NanoUIE msg ()) -> NanoUIE msg ()
hstack = row . sequence_

-- | Compact vertical stack.
{-# INLINE vstack #-}
vstack :: Foldable f => f (NanoUIE msg ()) -> NanoUIE msg ()
vstack = column . sequence_

-- | Uniform grid layout.
{-# INLINE grid #-}
grid :: Int -> NanoUIE msg a -> NanoUIE msg a
grid = withNanoUI . UI.grid

-- | Grid layout with a modifier.
{-# INLINE gridWith #-}
gridWith :: Int -> LayoutModifier -> NanoUIE msg a -> NanoUIE msg a
gridWith n lm = withNanoUI (UI.gridWith n lm)

-- | Surface panel with theme styling.
{-# INLINE panel #-}
panel :: NanoUIE msg a -> NanoUIE msg a
panel = withNanoUI UI.panel

-- | Surface panel with a modifier.
{-# INLINE panelWith #-}
panelWith :: LayoutModifier -> NanoUIE msg a -> NanoUIE msg a
panelWith = withNanoUI . UI.panelWith

-- | Outlined card container.
{-# INLINE card #-}
card :: NanoUIE msg a -> NanoUIE msg a
card = withNanoUI UI.card

-- | Callout box.
{-# INLINE callout #-}
callout :: Color -> NanoUIE msg a -> NanoUIE msg a
callout col = withNanoUI (UI.callout col)

-- | Callout box with a modifier.
{-# INLINE calloutWith #-}
calloutWith :: Color -> LayoutModifier -> NanoUIE msg a -> NanoUIE msg a
calloutWith col lm = withNanoUI (UI.calloutWith col lm)

-- | Toolbar container.
{-# INLINE toolbar #-}
toolbar :: NanoUIE msg a -> NanoUIE msg a
toolbar = withNanoUI UI.toolbar

-- | Center child within available space.
{-# INLINE center #-}
center :: NanoUIE msg a -> NanoUIE msg a
center = withNanoUI UI.center

-- | Adapt layout based on window width breakpoint.
{-# INLINE responsive #-}
responsive :: Float -> (NanoUI a -> NanoUI a) -> (NanoUI a -> NanoUI a) -> NanoUIE msg a -> NanoUIE msg a
responsive bp wide narrow = withNanoUI (UI.responsive bp wide narrow)

-- | Switch between row and column based on breakpoint.
{-# INLINE responsiveRowCol #-}
responsiveRowCol :: Float -> (Layout -> Layout) -> NanoUIE msg a -> NanoUIE msg a
responsiveRowCol bp f = withNanoUI (UI.responsiveRowCol bp f)

-- | Vertically scrolling container.
{-# INLINE scroll #-}
scroll :: NanoUIE msg a -> NanoUIE msg a
scroll = withNanoUI UI.scroll

-- | Vertically scrolling container with a modifier.
{-# INLINE scrollWith #-}
scrollWith :: LayoutModifier -> NanoUIE msg a -> NanoUIE msg a
scrollWith = withNanoUI . UI.scrollWith

-- | Bidirectionally scrolling container.
{-# INLINE scroll2D #-}
scroll2D :: NanoUIE msg a -> NanoUIE msg a
scroll2D = withNanoUI UI.scroll2D

-- | Bidirectionally scrolling container with a modifier.
{-# INLINE scroll2DWith #-}
scroll2DWith :: LayoutModifier -> NanoUIE msg a -> NanoUIE msg a
scroll2DWith = withNanoUI . UI.scroll2DWith

-- | Scroll area returning the child result and scroller's 'WidgetId'.
{-# INLINE scrollArea #-}
scrollArea :: LayoutModifier -> NanoUIE msg a -> NanoUIE msg (WidgetId, a)
scrollArea lm body = withNanoUI (UI.scrollArea lm) body

-- | Bidirectional scroll area returning the child result and scroller's 'WidgetId'.
{-# INLINE scrollArea2D #-}
scrollArea2D :: LayoutModifier -> NanoUIE msg a -> NanoUIE msg (WidgetId, a)
scrollArea2D lm body = withNanoUI (UI.scrollArea2D lm) body

-- | Horizontal divider line.
{-# INLINE separator #-}
separator :: NanoUIE msg ()
separator = liftNanoUI UI.separator

-- | Expanding spacer.
{-# INLINE spacer #-}
spacer :: Sizing -> Sizing -> NanoUIE msg ()
spacer w h = liftNanoUI (UI.spacer w h)

-- | Flexible layout node.
{-# INLINE flex #-}
flex :: NanoUIE msg ()
flex = liftNanoUI UI.flex

-- | Stack children in z-order.
{-# INLINE layers #-}
layers :: NanoUIE msg a -> NanoUIE msg a
layers = withNanoUI UI.layers

-- | Stack children in z-order with a modifier.
{-# INLINE layersWith #-}
layersWith :: LayoutModifier -> NanoUIE msg a -> NanoUIE msg a
layersWith = withNanoUI . UI.layersWith

-- | Interactive mouse area over a subtree.
{-# INLINE mouseArea #-}
mouseArea :: LayoutModifier -> NanoUIE msg a -> NanoUIE msg (a, Response)
mouseArea lm body = withNanoUI (UI.mouseArea lm) body

-- Text

-- | Plain text label.
{-# INLINE label #-}
label :: Text -> NanoUIE msg ()
label = liftNanoUI . UI.label

-- | Plain text label returning its 'Response'.
{-# INLINE label' #-}
label' :: Text -> NanoUIE msg Response
label' = liftNanoUI . UI.label'

-- | Text label with layout modifier.
{-# INLINE labelWith #-}
labelWith :: LayoutModifier -> Text -> NanoUIE msg ()
labelWith lm = liftNanoUI . UI.labelWith lm

-- | Text label with layout modifier returning its 'Response'.
{-# INLINE labelWith' #-}
labelWith' :: LayoutModifier -> Text -> NanoUIE msg Response
labelWith' lm = liftNanoUI . UI.labelWith' lm

-- | Prominent section heading.
{-# INLINE heading #-}
heading :: Text -> NanoUIE msg ()
heading = liftNanoUI . UI.heading

-- | Muted secondary text.
{-# INLINE muted #-}
muted :: Text -> NanoUIE msg ()
muted = liftNanoUI . UI.muted

-- | Monospaced text.
{-# INLINE mono #-}
mono :: Text -> NanoUIE msg ()
mono = liftNanoUI . UI.mono

-- | Danger/error text.
{-# INLINE danger #-}
danger :: Text -> NanoUIE msg ()
danger = liftNanoUI . UI.danger

-- | Bold text.
{-# INLINE bold #-}
bold :: Text -> NanoUIE msg ()
bold = liftNanoUI . UI.bold

-- | Italic text.
{-# INLINE italic #-}
italic :: Text -> NanoUIE msg ()
italic = liftNanoUI . UI.italic

-- | Underlined text.
{-# INLINE underline #-}
underline :: Text -> NanoUIE msg ()
underline = liftNanoUI . UI.underline

-- | Key-value pair label.
{-# INLINE kv #-}
kv :: Text -> Text -> NanoUIE msg ()
kv k = liftNanoUI . UI.kv k

-- | Key-value pair with monospaced value.
{-# INLINE kvMono #-}
kvMono :: Text -> Text -> NanoUIE msg ()
kvMono k = liftNanoUI . UI.kvMono k

-- | Key-value pairs rendered as a block.
{-# INLINE kvBlock #-}
kvBlock :: Foldable f => f (Text, Text) -> NanoUIE msg ()
kvBlock rows = liftNanoUI (UI.kvBlock rows)

-- | Selectable text.
{-# INLINE selectableText #-}
selectableText :: Text -> NanoUIE msg ()
selectableText = liftNanoUI . UI.selectableText

-- | Selectable text returning its 'Response'.
{-# INLINE selectableText' #-}
selectableText' :: Text -> NanoUIE msg Response
selectableText' = liftNanoUI . UI.selectableText'

-- | Selectable text with a layout modifier.
{-# INLINE selectableTextWith #-}
selectableTextWith :: LayoutModifier -> Text -> NanoUIE msg ()
selectableTextWith lm = liftNanoUI . UI.selectableTextWith lm

-- | Selectable text with a layout modifier returning its 'Response'.
{-# INLINE selectableTextWith' #-}
selectableTextWith' :: LayoutModifier -> Text -> NanoUIE msg Response
selectableTextWith' lm = liftNanoUI . UI.selectableTextWith' lm

-- Buttons and menus

-- | Push button returning 'True' when activated.
{-# INLINE button #-}
button :: Text -> NanoUIE msg Bool
button = liftNanoUI . UI.button

-- | Push button returning its 'Response'.
{-# INLINE button' #-}
button' :: Text -> NanoUIE msg Response
button' = liftNanoUI . UI.button'

-- | Push button with layout modifier.
{-# INLINE buttonWith #-}
buttonWith :: LayoutModifier -> Text -> NanoUIE msg Bool
buttonWith lm = liftNanoUI . UI.buttonWith lm

-- | Push button with layout modifier returning its 'Response'.
{-# INLINE buttonWith' #-}
buttonWith' :: LayoutModifier -> Text -> NanoUIE msg Response
buttonWith' lm = liftNanoUI . UI.buttonWith' lm

-- | Icon button.
{-# INLINE iconButton #-}
iconButton :: Svg -> Text -> NanoUIE msg Bool
iconButton doc txt = liftNanoUI (UI.iconButton doc txt)

-- | Icon button returning its 'Response'.
{-# INLINE iconButton' #-}
iconButton' :: Svg -> Text -> NanoUIE msg Response
iconButton' doc txt = liftNanoUI (UI.iconButton' doc txt)

-- | Button containing arbitrary UI body.
{-# INLINE buttonContent #-}
buttonContent :: NanoUIE msg () -> NanoUIE msg Bool
buttonContent body = withNanoUI UI.buttonContent body

-- | Button containing arbitrary UI body returning its 'Response'.
{-# INLINE buttonContent' #-}
buttonContent' :: NanoUIE msg () -> NanoUIE msg Response
buttonContent' body = withNanoUI UI.buttonContent' body

-- | Button containing arbitrary UI body with layout modifier.
{-# INLINE buttonContentWith #-}
buttonContentWith :: LayoutModifier -> NanoUIE msg () -> NanoUIE msg Bool
buttonContentWith lm body = withNanoUI (UI.buttonContentWith lm) body

-- | Button containing arbitrary UI body with layout modifier returning its 'Response'.
{-# INLINE buttonContentWith' #-}
buttonContentWith' :: LayoutModifier -> NanoUIE msg () -> NanoUIE msg Response
buttonContentWith' lm body = withNanoUI (UI.buttonContentWith' lm) body

-- | Configured button.
{-# INLINE buttonConfigured #-}
buttonConfigured :: ButtonConfig -> Text -> NanoUIE msg Bool
buttonConfigured cfg = liftNanoUI . UI.buttonConfigured cfg

-- | Configured button returning its 'Response'.
{-# INLINE buttonConfigured' #-}
buttonConfigured' :: ButtonConfig -> Text -> NanoUIE msg Response
buttonConfigured' cfg = liftNanoUI . UI.buttonConfigured' cfg

-- | Menu button that reveals a dropdown menu.
{-# INLINE menuButton #-}
menuButton :: Text -> Bool -> NanoUIE msg Bool
menuButton txt open = liftNanoUI (UI.menuButton txt open)

-- | Menu button returning its 'Response'.
{-# INLINE menuButton' #-}
menuButton' :: Text -> Bool -> NanoUIE msg Response
menuButton' txt open = liftNanoUI (UI.menuButton' txt open)

-- | Menu button with layout modifier.
{-# INLINE menuButtonWith #-}
menuButtonWith :: LayoutModifier -> Text -> Bool -> NanoUIE msg Bool
menuButtonWith lm txt open = liftNanoUI (UI.menuButtonWith lm txt open)

-- | Menu button with layout modifier returning its 'Response'.
{-# INLINE menuButtonWith' #-}
menuButtonWith' :: LayoutModifier -> Text -> Bool -> NanoUIE msg Response
menuButtonWith' lm txt open = liftNanoUI (UI.menuButtonWith' lm txt open)

-- | Menu item.
{-# INLINE menuItem #-}
menuItem :: Text -> NanoUIE msg Bool
menuItem = liftNanoUI . UI.menuItem

-- | Menu item returning its 'Response'.
{-# INLINE menuItem' #-}
menuItem' :: Text -> NanoUIE msg Response
menuItem' = liftNanoUI . UI.menuItem'

-- | Menu item bound to a keyboard shortcut.
{-# INLINE menuItemShortcut #-}
menuItemShortcut :: Text -> Shortcut -> NanoUIE msg Bool
menuItemShortcut txt sc = liftNanoUI (UI.menuItemShortcut txt sc)

-- | Disabled menu item.
{-# INLINE menuItemDisabled #-}
menuItemDisabled :: Text -> NanoUIE msg ()
menuItemDisabled = liftNanoUI . UI.menuItemDisabled

-- | Menu separator line.
{-# INLINE menuSeparator #-}
menuSeparator :: NanoUIE msg ()
menuSeparator = liftNanoUI UI.menuSeparator

-- | Menu section header.
{-# INLINE menuHeader #-}
menuHeader :: Text -> NanoUIE msg ()
menuHeader = liftNanoUI . UI.menuHeader

-- | Context menu for any widget response.
{-# INLINE contextMenu #-}
contextMenu :: HasResponse r => r -> NanoUIE msg a -> NanoUIE msg (Maybe a)
contextMenu target body = withNanoUI (UI.contextMenu target) body

-- | Region with an attached context menu.
{-# INLINE contextMenuArea #-}
contextMenuArea :: LayoutModifier -> NanoUIE msg a -> (V2 -> NanoUIE msg b) -> NanoUIE msg (a, Maybe b)
contextMenuArea lm target menu = withRunInNanoUIE $ \run -> UI.contextMenuArea lm (run target) (run . menu)

-- Inputs

-- | Checkbox control.
{-# INLINE checkbox #-}
checkbox :: Text -> Var Bool -> NanoUIE msg ()
checkbox txt = liftNanoUI . UI.checkbox txt

-- | Checkbox returning its 'Response'.
{-# INLINE checkbox' #-}
checkbox' :: Text -> Var Bool -> NanoUIE msg Response
checkbox' txt = liftNanoUI . UI.checkbox' txt

-- | Checkbox with layout modifier.
{-# INLINE checkboxWith #-}
checkboxWith :: LayoutModifier -> Text -> Var Bool -> NanoUIE msg ()
checkboxWith lm txt = liftNanoUI . UI.checkboxWith lm txt

-- | Checkbox with layout modifier returning its 'Response'.
{-# INLINE checkboxWith' #-}
checkboxWith' :: LayoutModifier -> Text -> Var Bool -> NanoUIE msg Response
checkboxWith' lm txt = liftNanoUI . UI.checkboxWith' lm txt

-- | Toggle button.
{-# INLINE toggleButton #-}
toggleButton :: Tone -> Text -> Var Bool -> NanoUIE msg ()
toggleButton t txt v = liftNanoUI (UI.toggleButton t txt v)

-- | Toggle button returning its 'Response'.
{-# INLINE toggleButton' #-}
toggleButton' :: Tone -> Text -> Var Bool -> NanoUIE msg Response
toggleButton' t txt v = liftNanoUI (UI.toggleButton' t txt v)

-- | Toggle button with layout modifier.
{-# INLINE toggleButtonWith #-}
toggleButtonWith :: LayoutModifier -> Tone -> Text -> Var Bool -> NanoUIE msg ()
toggleButtonWith lm t txt v = liftNanoUI (UI.toggleButtonWith lm t txt v)

-- | Toggle button with layout modifier returning its 'Response'.
{-# INLINE toggleButtonWith' #-}
toggleButtonWith' :: LayoutModifier -> Tone -> Text -> Var Bool -> NanoUIE msg Response
toggleButtonWith' lm t txt v = liftNanoUI (UI.toggleButtonWith' lm t txt v)

-- | Radio button option.
{-# INLINE radio #-}
radio :: Foldable f => f Text -> Var Int -> NanoUIE msg ()
radio opts = liftNanoUI . UI.radio opts

-- | Radio button option returning its 'Response'.
{-# INLINE radio' #-}
radio' :: Foldable f => f Text -> Var Int -> NanoUIE msg Response
radio' opts = liftNanoUI . UI.radio' opts

-- | Radio group over a 'Bounded' 'Enum'.
{-# INLINE boundedRadio #-}
boundedRadio :: (Bounded a, Enum a) => (a -> Text) -> Var a -> NanoUIE msg ()
boundedRadio fmt = liftNanoUI . UI.boundedRadio fmt

-- | Radio group over a 'Bounded' 'Enum' returning its 'Response'.
{-# INLINE boundedRadio' #-}
boundedRadio' :: (Bounded a, Enum a) => (a -> Text) -> Var a -> NanoUIE msg Response
boundedRadio' fmt = liftNanoUI . UI.boundedRadio' fmt

-- | Radio group over a 'Bounded' 'Enum' using 'Show'.
{-# INLINE enumRadio #-}
enumRadio :: (Bounded a, Enum a, Show a) => Var a -> NanoUIE msg ()
enumRadio = liftNanoUI . UI.enumRadio

-- | Radio group over a 'Bounded' 'Enum' using 'Show' returning its 'Response'.
{-# INLINE enumRadio' #-}
enumRadio' :: (Bounded a, Enum a, Show a) => Var a -> NanoUIE msg Response
enumRadio' = liftNanoUI . UI.enumRadio'

-- | Dropdown selection list.
{-# INLINE select #-}
select :: Foldable f => f Text -> Var Int -> NanoUIE msg ()
select opts = liftNanoUI . UI.select opts

-- | Dropdown selection list returning its 'Response'.
{-# INLINE select' #-}
select' :: Foldable f => f Text -> Var Int -> NanoUIE msg Response
select' opts = liftNanoUI . UI.select' opts

-- | Dropdown selection list with layout modifier.
{-# INLINE selectWith #-}
selectWith :: Foldable f => LayoutModifier -> f Text -> Var Int -> NanoUIE msg ()
selectWith lm opts = liftNanoUI . UI.selectWith lm opts

-- | Dropdown selection list with layout modifier returning its 'Response'.
{-# INLINE selectWith' #-}
selectWith' :: Foldable f => LayoutModifier -> f Text -> Var Int -> NanoUIE msg Response
selectWith' lm opts = liftNanoUI . UI.selectWith' lm opts

-- | Selection dropdown over a 'Bounded' 'Enum'.
{-# INLINE boundedSelect #-}
boundedSelect :: (Bounded a, Enum a) => (a -> Text) -> Var a -> NanoUIE msg ()
boundedSelect fmt = liftNanoUI . UI.boundedSelect fmt

-- | Selection dropdown over a 'Bounded' 'Enum' returning its 'Response'.
{-# INLINE boundedSelect' #-}
boundedSelect' :: (Bounded a, Enum a) => (a -> Text) -> Var a -> NanoUIE msg Response
boundedSelect' fmt = liftNanoUI . UI.boundedSelect' fmt

-- | Selection dropdown over a 'Bounded' 'Enum' using 'Show'.
{-# INLINE enumSelect #-}
enumSelect :: (Bounded a, Enum a, Show a) => Var a -> NanoUIE msg ()
enumSelect = liftNanoUI . UI.enumSelect

-- | Selection dropdown over a 'Bounded' 'Enum' using 'Show' returning its 'Response'.
{-# INLINE enumSelect' #-}
enumSelect' :: (Bounded a, Enum a, Show a) => Var a -> NanoUIE msg Response
enumSelect' = liftNanoUI . UI.enumSelect'

-- | Numeric slider.
{-# INLINE slider #-}
slider :: Float -> Float -> Var Float -> NanoUIE msg ()
slider minV maxV = liftNanoUI . UI.slider minV maxV

-- | Numeric slider returning its 'Response'.
{-# INLINE slider' #-}
slider' :: Float -> Float -> Var Float -> NanoUIE msg Response
slider' minV maxV = liftNanoUI . UI.slider' minV maxV

-- | Numeric slider with layout modifier.
{-# INLINE sliderWith #-}
sliderWith :: LayoutModifier -> Float -> Float -> Var Float -> NanoUIE msg ()
sliderWith lm minV maxV = liftNanoUI . UI.sliderWith lm minV maxV

-- | Numeric slider with layout modifier returning its 'Response'.
{-# INLINE sliderWith' #-}
sliderWith' :: LayoutModifier -> Float -> Float -> Var Float -> NanoUIE msg Response
sliderWith' lm minV maxV = liftNanoUI . UI.sliderWith' lm minV maxV

-- | Single-line text input field.
{-# INLINE textInput #-}
textInput :: Var Text -> NanoUIE msg ()
textInput = liftNanoUI . UI.textInput

-- | Single-line text input field returning its 'Response'.
{-# INLINE textInput' #-}
textInput' :: Var Text -> NanoUIE msg Response
textInput' = liftNanoUI . UI.textInput'

-- | Configured text input field.
{-# INLINE textInputConfigured #-}
textInputConfigured :: TextInputConfig -> Var Text -> NanoUIE msg ()
textInputConfigured cfg = liftNanoUI . UI.textInputConfigured cfg

-- | Configured text input field returning its 'Response'.
{-# INLINE textInputConfigured' #-}
textInputConfigured' :: TextInputConfig -> Var Text -> NanoUIE msg Response
textInputConfigured' cfg = liftNanoUI . UI.textInputConfigured' cfg

-- | Numeric text input.
{-# INLINE numericInput #-}
numericInput :: Var Double -> NanoUIE msg ()
numericInput = liftNanoUI . UI.numericInput

-- | Numeric text input returning its 'Response'.
{-# INLINE numericInput' #-}
numericInput' :: Var Double -> NanoUIE msg Response
numericInput' = liftNanoUI . UI.numericInput'

-- | Configured numeric text input.
{-# INLINE numericInputConfigured #-}
numericInputConfigured :: NumericInputConfig -> Var Double -> NanoUIE msg ()
numericInputConfigured cfg = liftNanoUI . UI.numericInputConfigured cfg

-- | Configured numeric text input returning its 'Response'.
{-# INLINE numericInputConfigured' #-}
numericInputConfigured' :: NumericInputConfig -> Var Double -> NanoUIE msg Response
numericInputConfigured' cfg = liftNanoUI . UI.numericInputConfigured' cfg

-- | Search input with debouncing.
{-# INLINE searchInput #-}
searchInput :: Text -> Var Text -> NanoUIE msg ()
searchInput ph = liftNanoUI . UI.searchInput ph

-- | Search input returning its 'Response'.
{-# INLINE searchInput' #-}
searchInput' :: Text -> Var Text -> NanoUIE msg Response
searchInput' ph = liftNanoUI . UI.searchInput' ph

-- | Configured search input.
{-# INLINE searchInputConfigured #-}
searchInputConfigured :: SearchInputConfig -> Var Text -> NanoUIE msg ()
searchInputConfigured cfg = liftNanoUI . UI.searchInputConfigured cfg

-- | Configured search input returning its 'Response'.
{-# INLINE searchInputConfigured' #-}
searchInputConfigured' :: SearchInputConfig -> Var Text -> NanoUIE msg Response
searchInputConfigured' cfg = liftNanoUI . UI.searchInputConfigured' cfg

-- | Editable combo box with dropdown suggestions.
{-# INLINE comboBox #-}
comboBox :: Foldable f => Text -> f Text -> Var Text -> NanoUIE msg ()
comboBox ph opts = liftNanoUI . UI.comboBox ph opts

-- | Editable combo box returning its 'Response'.
{-# INLINE comboBox' #-}
comboBox' :: Foldable f => Text -> f Text -> Var Text -> NanoUIE msg Response
comboBox' ph opts = liftNanoUI . UI.comboBox' ph opts

-- | Multi-line text area.
{-# INLINE textArea #-}
textArea :: Var Text -> NanoUIE msg ()
textArea = liftNanoUI . UI.textArea

-- | Multi-line text area returning its 'Response'.
{-# INLINE textArea' #-}
textArea' :: Var Text -> NanoUIE msg Response
textArea' = liftNanoUI . UI.textArea'

-- | Multi-line text area with layout modifier.
{-# INLINE textAreaWith #-}
textAreaWith :: LayoutModifier -> Var Text -> NanoUIE msg ()
textAreaWith lm = liftNanoUI . UI.textAreaWith lm

-- | Multi-line text area with layout modifier returning its 'Response'.
{-# INLINE textAreaWith' #-}
textAreaWith' :: LayoutModifier -> Var Text -> NanoUIE msg Response
textAreaWith' lm = liftNanoUI . UI.textAreaWith' lm

-- | Document-based text editor for large files.
{-# INLINE textAreaDocument #-}
textAreaDocument :: Var TextDocument -> NanoUIE msg ()
textAreaDocument = liftNanoUI . UI.textAreaDocument

-- | Document-based text editor returning its 'Response'.
{-# INLINE textAreaDocument' #-}
textAreaDocument' :: Var TextDocument -> NanoUIE msg Response
textAreaDocument' = liftNanoUI . UI.textAreaDocument'

-- | Document-based text editor with layout modifier.
{-# INLINE textAreaDocumentWith #-}
textAreaDocumentWith :: LayoutModifier -> Var TextDocument -> NanoUIE msg ()
textAreaDocumentWith lm = liftNanoUI . UI.textAreaDocumentWith lm

-- | Document-based text editor with layout modifier returning its 'Response'.
{-# INLINE textAreaDocumentWith' #-}
textAreaDocumentWith' :: LayoutModifier -> Var TextDocument -> NanoUIE msg Response
textAreaDocumentWith' lm = liftNanoUI . UI.textAreaDocumentWith' lm

-- | Color picker.
{-# INLINE colorPicker #-}
colorPicker :: Var Color -> NanoUIE msg ()
colorPicker = liftNanoUI . UI.colorPicker

-- | Color picker returning its 'Response'.
{-# INLINE colorPicker' #-}
colorPicker' :: Var Color -> NanoUIE msg Response
colorPicker' = liftNanoUI . UI.colorPicker'

-- | RGBA color picker including alpha channel.
{-# INLINE colorPickerRGBA #-}
colorPickerRGBA :: Var Color -> NanoUIE msg ()
colorPickerRGBA = liftNanoUI . UI.colorPickerRGBA

-- | RGBA color picker returning its 'Response'.
{-# INLINE colorPickerRGBA' #-}
colorPickerRGBA' :: Var Color -> NanoUIE msg Response
colorPickerRGBA' = liftNanoUI . UI.colorPickerRGBA'

-- Tabs, trees, and tables

-- | Tab bar with child view panels.
{-# INLINE tabs #-}
tabs :: (Foldable f, Hashable a) => Var a -> f (Tab a (NanoUIE msg ())) -> NanoUIE msg ()
tabs var ts = withRunInNanoUIE $ \run ->
  UI.tabs var [t { tabBody = run (tabBody t) } | t <- toList ts]

-- | Tab bar with child view panels returning its 'TabResponse'.
{-# INLINE tabs' #-}
tabs' :: (Foldable f, Hashable a) => Var a -> f (Tab a (NanoUIE msg ())) -> NanoUIE msg (TabResponse a)
tabs' var ts = withRunInNanoUIE $ \run ->
  UI.tabs' var [t { tabBody = run (tabBody t) } | t <- toList ts]

-- | Configured tab bar with child views.
{-# INLINE tabsConfigured #-}
tabsConfigured :: (Foldable f, Hashable a) => TabsConfig -> Var a -> f (Tab a (NanoUIE msg ())) -> NanoUIE msg ()
tabsConfigured cfg var ts = withRunInNanoUIE $ \run ->
  UI.tabsConfigured cfg var [t { tabBody = run (tabBody t) } | t <- toList ts]

-- | Configured tab bar with child views returning its 'TabResponse'.
{-# INLINE tabsConfigured' #-}
tabsConfigured' :: (Foldable f, Hashable a) => TabsConfig -> Var a -> f (Tab a (NanoUIE msg ())) -> NanoUIE msg (TabResponse a)
tabsConfigured' cfg var ts = withRunInNanoUIE $ \run ->
  UI.tabsConfigured' cfg var [t { tabBody = run (tabBody t) } | t <- toList ts]

-- | Tab strip header without view bodies.
{-# INLINE tabBar #-}
tabBar :: (Foldable f, Hashable a) => Var a -> f (Tab a ()) -> NanoUIE msg ()
tabBar var ts = liftNanoUI (UI.tabBar var ts)

-- | Tab strip header returning its 'TabResponse'.
{-# INLINE tabBar' #-}
tabBar' :: (Foldable f, Hashable a) => Var a -> f (Tab a ()) -> NanoUIE msg (TabResponse a)
tabBar' var ts = liftNanoUI (UI.tabBar' var ts)

-- | Configured tab strip header.
{-# INLINE tabBarConfigured #-}
tabBarConfigured :: (Foldable f, Hashable a) => TabsConfig -> Var a -> f (Tab a ()) -> NanoUIE msg ()
tabBarConfigured cfg var ts = liftNanoUI (UI.tabBarConfigured cfg var ts)

-- | Configured tab strip header returning its 'TabResponse'.
{-# INLINE tabBarConfigured' #-}
tabBarConfigured' :: (Foldable f, Hashable a) => TabsConfig -> Var a -> f (Tab a ()) -> NanoUIE msg (TabResponse a)
tabBarConfigured' cfg var ts = liftNanoUI (UI.tabBarConfigured' cfg var ts)

-- | Tab view over a 'Bounded' 'Enum'.
{-# INLINE boundedTabs #-}
boundedTabs :: (Bounded a, Enum a) => (a -> Text) -> Var a -> (a -> NanoUIE msg ()) -> NanoUIE msg ()
boundedTabs fmt var body = withRunInNanoUIE $ \run -> UI.boundedTabs fmt var (run . body)

-- | Configured tab view over a 'Bounded' 'Enum'.
{-# INLINE boundedTabsConfigured #-}
boundedTabsConfigured :: (Bounded a, Enum a) => TabsConfig -> (a -> Text) -> Var a -> (a -> NanoUIE msg ()) -> NanoUIE msg ()
boundedTabsConfigured cfg fmt var body = withRunInNanoUIE $ \run -> UI.boundedTabsConfigured cfg fmt var (run . body)

-- | Tab view over a 'Bounded' 'Enum' using 'Show'.
{-# INLINE enumTabs #-}
enumTabs :: (Bounded a, Enum a, Show a) => Var a -> (a -> NanoUIE msg ()) -> NanoUIE msg ()
enumTabs var body = withRunInNanoUIE $ \run -> UI.enumTabs var (run . body)

-- | Tab strip header over a 'Bounded' 'Enum'.
{-# INLINE boundedTabBar #-}
boundedTabBar :: (Bounded a, Enum a) => (a -> Text) -> Var a -> NanoUIE msg ()
boundedTabBar fmt var = liftNanoUI (UI.boundedTabBar fmt var)

-- | Configured tab strip header over a 'Bounded' 'Enum'.
{-# INLINE boundedTabBarConfigured #-}
boundedTabBarConfigured :: (Bounded a, Enum a) => TabsConfig -> (a -> Text) -> Var a -> NanoUIE msg ()
boundedTabBarConfigured cfg fmt var = liftNanoUI (UI.boundedTabBarConfigured cfg fmt var)

-- | Tab strip header over a 'Bounded' 'Enum' using 'Show'.
{-# INLINE enumTabBar #-}
enumTabBar :: (Bounded a, Enum a, Show a) => Var a -> NanoUIE msg ()
enumTabBar = liftNanoUI . UI.enumTabBar

-- | Hierarchical tree view.
{-# INLINE tree #-}
tree :: Foldable f => Text -> f TreeItem -> Var Int -> NanoUIE msg ()
tree title items = liftNanoUI . UI.tree title items

-- | Hierarchical tree view returning its 'Response'.
{-# INLINE tree' #-}
tree' :: Foldable f => Text -> f TreeItem -> Var Int -> NanoUIE msg Response
tree' title items = liftNanoUI . UI.tree' title items

-- | Colonnade-based data table.
{-# INLINE table #-}
table :: (Foldable f, ToCell c) => Text -> Colonnade Headed row c -> f row -> Var SortCol -> NanoUIE msg ()
table title cols rows v = liftNanoUI (UI.table title cols rows v)

-- | Colonnade-based data table returning its 'TableResponse'.
{-# INLINE table' #-}
table' :: (Foldable f, ToCell c) => Text -> Colonnade Headed row c -> f row -> Var SortCol -> NanoUIE msg TableResponse
table' title cols rows v = liftNanoUI (UI.table' title cols rows v)

-- | Data table with layout modifier.
{-# INLINE tableWith #-}
tableWith :: (Foldable f, ToCell c) => LayoutModifier -> Text -> Colonnade Headed row c -> f row -> Var SortCol -> NanoUIE msg ()
tableWith lm title cols rows v = liftNanoUI (UI.tableWith lm title cols rows v)

-- | Data table with layout modifier returning its 'TableResponse'.
{-# INLINE tableWith' #-}
tableWith' :: (Foldable f, ToCell c) => LayoutModifier -> Text -> Colonnade Headed row c -> f row -> Var SortCol -> NanoUIE msg TableResponse
tableWith' lm title cols rows v = liftNanoUI (UI.tableWith' lm title cols rows v)

-- | Configured data table.
{-# INLINE tableConfigured #-}
tableConfigured :: (Foldable f, ToCell c) => TableConfig -> LayoutModifier -> Text -> Colonnade Headed row c -> f row -> Var SortCol -> NanoUIE msg ()
tableConfigured cfg lm title cols rows v = liftNanoUI (UI.tableConfigured cfg lm title cols rows v)

-- | Configured data table returning its 'TableResponse'.
{-# INLINE tableConfigured' #-}
tableConfigured' :: (Foldable f, ToCell c) => TableConfig -> LayoutModifier -> Text -> Colonnade Headed row c -> f row -> Var SortCol -> NanoUIE msg TableResponse
tableConfigured' cfg lm title cols rows v = liftNanoUI (UI.tableConfigured' cfg lm title cols rows v)

-- | Simple text table from row lists.
{-# INLINE simpleTable #-}
simpleTable :: Foldable f => [Text] -> f [Text] -> NanoUIE msg TableResponse
simpleTable headers = liftNanoUI . UI.simpleTable headers

-- Overlays

-- | Modal dialog overlay.
{-# INLINE modal #-}
modal :: Var Bool -> Text -> NanoUIE msg () -> NanoUIE msg ()
modal open title body = withNanoUI (UI.modal open title) body

-- | Modal dialog overlay returning its 'Response' and child result.
{-# INLINE modal' #-}
modal' :: Var Bool -> Text -> NanoUIE msg a -> NanoUIE msg (Response, Maybe a)
modal' open title body = withNanoUI (UI.modal' open title) body

-- | Modal dialog with layout modifier.
{-# INLINE modalWith #-}
modalWith :: LayoutModifier -> Var Bool -> Text -> NanoUIE msg () -> NanoUIE msg ()
modalWith lm open title body = withNanoUI (UI.modalWith lm open title) body

-- | Modal dialog with layout modifier returning its 'Response' and child result.
{-# INLINE modalWith' #-}
modalWith' :: LayoutModifier -> Var Bool -> Text -> NanoUIE msg a -> NanoUIE msg (Response, Maybe a)
modalWith' lm open title body = withNanoUI (UI.modalWith' lm open title) body

-- | Modal dialog styled as a panel.
{-# INLINE modalPanel #-}
modalPanel :: Var Bool -> NanoUIE msg () -> NanoUIE msg ()
modalPanel open body = withNanoUI (UI.modalPanel open) body

-- | Modal dialog styled as a panel returning its 'Response' and child result.
{-# INLINE modalPanel' #-}
modalPanel' :: Var Bool -> NanoUIE msg a -> NanoUIE msg (Response, Maybe a)
modalPanel' open body = withNanoUI (UI.modalPanel' open) body

-- | Modal dialog styled as a panel with layout modifier.
{-# INLINE modalPanelWith #-}
modalPanelWith :: LayoutModifier -> Var Bool -> NanoUIE msg () -> NanoUIE msg ()
modalPanelWith lm open body = withNanoUI (UI.modalPanelWith lm open) body

-- | Modal dialog styled as a panel with layout modifier returning its 'Response' and child result.
{-# INLINE modalPanelWith' #-}
modalPanelWith' :: LayoutModifier -> Var Bool -> NanoUIE msg a -> NanoUIE msg (Response, Maybe a)
modalPanelWith' lm open body = withNanoUI (UI.modalPanelWith' lm open) body

-- | Floating window overlay.
{-# INLINE window #-}
window :: Var Bool -> Text -> NanoUIE msg () -> NanoUIE msg ()
window open title body = withNanoUI (UI.window open title) body

-- | Floating window overlay returning its caption and window responses and child result.
{-# INLINE window' #-}
window' :: Var Bool -> Text -> NanoUIE msg a -> NanoUIE msg (Response, Maybe a)
window' open title body = withNanoUI (UI.window' open title) body

-- | Context popup overlay.
{-# INLINE popup #-}
popup :: Var Bool -> PopupConfig -> NanoUIE msg () -> NanoUIE msg ()
popup open cfg body = withNanoUI (UI.popup open cfg) body

-- | Context popup overlay returning its 'Response' and child result.
{-# INLINE popup' #-}
popup' :: Var Bool -> PopupConfig -> NanoUIE msg a -> NanoUIE msg (Response, Maybe a)
popup' open cfg body = withNanoUI (UI.popup' open cfg) body

-- | Context popup overlay with layout modifier.
{-# INLINE popupWith #-}
popupWith :: Var Bool -> PopupConfig -> LayoutModifier -> NanoUIE msg () -> NanoUIE msg ()
popupWith open cfg lm body = withNanoUI (UI.popupWith open cfg lm) body

-- | Context popup overlay with layout modifier returning its 'Response' and child result.
{-# INLINE popupWith' #-}
popupWith' :: Var Bool -> PopupConfig -> LayoutModifier -> NanoUIE msg a -> NanoUIE msg (Response, Maybe a)
popupWith' open cfg lm body = withNanoUI (UI.popupWith' open cfg lm) body

-- | Tooltip text on a response.
{-# INLINE tooltip #-}
tooltip :: HasResponse r => r -> Text -> NanoUIE msg ()
tooltip r = liftNanoUI . UI.tooltip r

-- | Tooltip positioned at an explicit placement.
{-# INLINE tooltipAt #-}
tooltipAt :: HasResponse r => PopupPlacement -> r -> Text -> NanoUIE msg ()
tooltipAt placement r = liftNanoUI . UI.tooltipAt placement r

-- | Tooltip containing an arbitrary UI widget.
{-# INLINE tooltipWidget #-}
tooltipWidget :: HasResponse r => r -> NanoUIE msg a -> NanoUIE msg (Maybe a)
tooltipWidget r body = withNanoUI (UI.tooltipWidget r) body

-- | Configured tooltip text.
{-# INLINE tooltipConfigured #-}
tooltipConfigured :: HasResponse r => TooltipConfig -> r -> Text -> NanoUIE msg ()
tooltipConfigured cfg r = liftNanoUI . UI.tooltipConfigured cfg r

-- | Configured tooltip containing an arbitrary UI widget.
{-# INLINE tooltipWidgetConfigured #-}
tooltipWidgetConfigured :: HasResponse r => TooltipConfig -> r -> NanoUIE msg a -> NanoUIE msg (Maybe a)
tooltipWidgetConfigured cfg r body = withNanoUI (UI.tooltipWidgetConfigured cfg r) body

-- | Attach a rich tooltip widget to an inner UI computation.
{-# INLINE withTooltip #-}
withTooltip :: NanoUIE msg a -> NanoUIE msg b -> NanoUIE msg (a, Maybe b)
withTooltip mainChild tipChild = withRunInNanoUIE $ \run -> UI.withTooltip (run mainChild) (run tipChild)

-- Window caption

-- | Caption button (minimize, maximize, close).
{-# INLINE captionButton #-}
captionButton :: CaptionConfig -> CaptionGlyph -> NanoUIE msg Response
captionButton cfg glyph = liftNanoUI (UI.captionButton cfg glyph)

-- | Standard window caption buttons.
{-# INLINE captionButtons #-}
captionButtons :: Bool -> NanoUIE msg (Maybe CaptionAction, Rect)
captionButtons maxed = liftNanoUI (UI.captionButtons maxed)

-- | Configured window caption buttons.
{-# INLINE captionButtonsConfigured #-}
captionButtonsConfigured :: CaptionConfig -> Bool -> NanoUIE msg (Maybe CaptionAction, Rect)
captionButtonsConfigured cfg maxed = liftNanoUI (UI.captionButtonsConfigured cfg maxed)

-- | Hit-test regions for title bar dragging.
{-# INLINE dragSpans #-}
dragSpans :: Rect -> [Rect] -> [Rect]
dragSpans = UI.dragSpans

-- | Window frame decoration container.
{-# INLINE windowFrame #-}
windowFrame :: WindowFrame -> NanoUIE msg a -> NanoUIE msg a
windowFrame frame = withNanoUI (UI.windowFrame frame)

-- Pane grids

-- | Tiled pane grid.
{-# INLINE paneGrid #-}
paneGrid :: PaneGridConfig -> NanoUIE msg PaneGridResponse
paneGrid = liftNanoUI . UI.paneGrid

-- | Drag handle for a pane edge.
{-# INLINE paneDragHandle #-}
paneDragHandle :: PaneGridCtx -> LayoutModifier -> NanoUIE msg a -> NanoUIE msg a
paneDragHandle pctx lm body = withNanoUI (UI.paneDragHandle pctx lm) body

-- Rich text

-- | Formatted rich text paragraph.
{-# INLINE richText #-}
richText :: [Inline] -> NanoUIE msg (Maybe Text)
richText = liftNanoUI . UI.richText

-- | Formatted rich text returning its 'Response' and clicked link.
{-# INLINE richText' #-}
richText' :: [Inline] -> NanoUIE msg (Response, Maybe Text)
richText' = liftNanoUI . UI.richText'

-- | Formatted rich text with layout modifier.
{-# INLINE richTextWith #-}
richTextWith :: LayoutModifier -> [Inline] -> NanoUIE msg (Maybe Text)
richTextWith lm = liftNanoUI . UI.richTextWith lm

-- | Formatted rich text with layout modifier returning its 'Response' and clicked link.
{-# INLINE richTextWith' #-}
richTextWith' :: LayoutModifier -> [Inline] -> NanoUIE msg (Response, Maybe Text)
richTextWith' lm = liftNanoUI . UI.richTextWith' lm

-- | Selectable formatted rich text.
{-# INLINE selectableRichText #-}
selectableRichText :: [Inline] -> NanoUIE msg (Maybe Text)
selectableRichText = liftNanoUI . UI.selectableRichText

-- | Selectable formatted rich text returning its 'Response' and clicked link.
{-# INLINE selectableRichText' #-}
selectableRichText' :: [Inline] -> NanoUIE msg (Response, Maybe Text)
selectableRichText' = liftNanoUI . UI.selectableRichText'

-- | Selectable formatted rich text with layout modifier.
{-# INLINE selectableRichTextWith #-}
selectableRichTextWith :: LayoutModifier -> [Inline] -> NanoUIE msg (Maybe Text)
selectableRichTextWith lm = liftNanoUI . UI.selectableRichTextWith lm

-- | Selectable formatted rich text with layout modifier returning its 'Response' and clicked link.
{-# INLINE selectableRichTextWith' #-}
selectableRichTextWith' :: LayoutModifier -> [Inline] -> NanoUIE msg (Response, Maybe Text)
selectableRichTextWith' lm = liftNanoUI . UI.selectableRichTextWith' lm

-- Images and drawing

-- | Draw an image texture.
{-# INLINE image #-}
image :: LayoutModifier -> ImageId -> NanoUIE msg ()
image lm iid = liftNanoUI (UI.image lm iid)

-- | Draw an image texture returning its 'Response'.
{-# INLINE image' #-}
image' :: LayoutModifier -> ImageId -> NanoUIE msg Response
image' lm iid = liftNanoUI (UI.image' lm iid)

-- | Configured image with fit, crop, opacity, and rotation.
{-# INLINE imageConfigured #-}
imageConfigured :: ImageConfig -> ImageId -> NanoUIE msg ()
imageConfigured cfg = liftNanoUI . UI.imageConfigured cfg

-- | Configured image returning its 'Response'.
{-# INLINE imageConfigured' #-}
imageConfigured' :: ImageConfig -> ImageId -> NanoUIE msg Response
imageConfigured' cfg = liftNanoUI . UI.imageConfigured' cfg

-- | Render an SVG vector icon at a fixed size.
{-# INLINE svgIcon #-}
svgIcon :: Float -> Svg -> NanoUIE msg ()
svgIcon size = liftNanoUI . UI.svgIcon size

-- | Render an SVG vector icon with layout modifier.
{-# INLINE svgIconWith #-}
svgIconWith :: LayoutModifier -> Svg -> NanoUIE msg ()
svgIconWith lm = liftNanoUI . UI.svgIconWith lm

-- | Render an SVG vector icon with layout modifier returning its 'Response'.
{-# INLINE svgIconWith' #-}
svgIconWith' :: LayoutModifier -> Svg -> NanoUIE msg Response
svgIconWith' lm = liftNanoUI . UI.svgIconWith' lm

-- | Configured SVG icon with fit, crop, opacity, and rotation.
{-# INLINE svgIconConfigured #-}
svgIconConfigured :: ImageConfig -> Svg -> NanoUIE msg ()
svgIconConfigured cfg = liftNanoUI . UI.svgIconConfigured cfg

-- | Configured SVG icon returning its 'Response'.
{-# INLINE svgIconConfigured' #-}
svgIconConfigured' :: ImageConfig -> Svg -> NanoUIE msg Response
svgIconConfigured' cfg = liftNanoUI . UI.svgIconConfigured' cfg

-- | Colored layout box.
{-# INLINE box #-}
box :: LayoutModifier -> Color -> NanoUIE msg ()
box lm col = liftNanoUI (UI.box lm col)

-- | Custom draw operations.
{-# INLINE drawing #-}
drawing :: LayoutModifier -> DrawingBuild -> NanoUIE msg Response
drawing lm = liftNanoUI . UI.drawing lm

-- | Versioned custom draw operations.
{-# INLINE drawingVersioned #-}
drawingVersioned :: Int -> LayoutModifier -> DrawingBuild -> NanoUIE msg Response
drawingVersioned v lm = liftNanoUI . UI.drawingVersioned v lm

-- | Cached custom drawing builder.
{-# INLINE drawingCached #-}
drawingCached :: Double -> Double -> Float -> Int -> LayoutModifier -> IO Layout -> DrawingBuild -> NanoUIE msg Response
drawingCached dw dh lh v lm compute build = liftNanoUI (UI.drawingCached dw dh lh v lm compute build)

-- Custom widgets

-- | Boolean toggle switch backed by a 'Var'.
{-# INLINE toggleSwitch #-}
toggleSwitch :: Var Bool -> NanoUIE msg ()
toggleSwitch = liftNanoUI . UI.toggleSwitch

-- | 'toggleSwitch' returning its 'Response'.
{-# INLINE toggleSwitch' #-}
toggleSwitch' :: Var Bool -> NanoUIE msg Response
toggleSwitch' = liftNanoUI . UI.toggleSwitch'

-- | 'toggleSwitch' with a layout modifier.
{-# INLINE toggleSwitchWith #-}
toggleSwitchWith :: LayoutModifier -> Var Bool -> NanoUIE msg ()
toggleSwitchWith lm = liftNanoUI . UI.toggleSwitchWith lm

-- | 'toggleSwitch' with a layout modifier returning its 'Response'.
{-# INLINE toggleSwitchWith' #-}
toggleSwitchWith' :: LayoutModifier -> Var Bool -> NanoUIE msg Response
toggleSwitchWith' lm = liftNanoUI . UI.toggleSwitchWith' lm

-- | Synonym for 'toggleSwitch'.
{-# INLINE switch #-}
switch :: Var Bool -> NanoUIE msg ()
switch = toggleSwitch

-- | Synonym for 'toggleSwitch''.
{-# INLINE switch' #-}
switch' :: Var Bool -> NanoUIE msg Response
switch' = toggleSwitch'

-- | Synonym for 'toggleSwitchWith'.
{-# INLINE switchWith #-}
switchWith :: LayoutModifier -> Var Bool -> NanoUIE msg ()
switchWith = toggleSwitchWith

-- | Synonym for 'toggleSwitchWith''.
{-# INLINE switchWith' #-}
switchWith' :: LayoutModifier -> Var Bool -> NanoUIE msg Response
switchWith' = toggleSwitchWith'

-- | Rotary knob.
{-# INLINE knob #-}
knob :: Float -> Float -> Var Float -> NanoUIE msg ()
knob minV maxV = liftNanoUI . UI.knob minV maxV

-- | Rotary knob returning its 'Response'.
{-# INLINE knob' #-}
knob' :: Float -> Float -> Var Float -> NanoUIE msg Response
knob' minV maxV = liftNanoUI . UI.knob' minV maxV

-- | Rotary knob with layout modifier and diameter in pixels.
{-# INLINE knobWith #-}
knobWith :: LayoutModifier -> Float -> Float -> Float -> Var Float -> NanoUIE msg ()
knobWith lm d minV maxV = liftNanoUI . UI.knobWith lm d minV maxV

-- | Rotary knob with layout modifier and diameter returning its 'Response'.
{-# INLINE knobWith' #-}
knobWith' :: LayoutModifier -> Float -> Float -> Float -> Var Float -> NanoUIE msg Response
knobWith' lm d minV maxV = liftNanoUI . UI.knobWith' lm d minV maxV

-- | Circular progress ring.
{-# INLINE circularProgress #-}
circularProgress :: Float -> NanoUIE msg ()
circularProgress = liftNanoUI . UI.circularProgress

-- | Circular progress ring returning its 'Response'.
{-# INLINE circularProgress' #-}
circularProgress' :: Float -> NanoUIE msg Response
circularProgress' = liftNanoUI . UI.circularProgress'

-- | Circular progress ring with a layout modifier and diameter.
{-# INLINE circularProgressWith #-}
circularProgressWith :: LayoutModifier -> Float -> Float -> NanoUIE msg ()
circularProgressWith lm d = liftNanoUI . UI.circularProgressWith lm d

-- | Circular progress ring with a layout modifier and diameter returning its 'Response'.
{-# INLINE circularProgressWith' #-}
circularProgressWith' :: LayoutModifier -> Float -> Float -> NanoUIE msg Response
circularProgressWith' lm d = liftNanoUI . UI.circularProgressWith' lm d

-- | Progress bar.
{-# INLINE progressBar #-}
progressBar :: Float -> NanoUIE msg ()
progressBar = liftNanoUI . UI.progressBar

-- | Progress bar returning its 'Response'.
{-# INLINE progressBar' #-}
progressBar' :: Float -> NanoUIE msg Response
progressBar' = liftNanoUI . UI.progressBar'

-- | Progress bar with layout modifier and height.
{-# INLINE progressBarWith #-}
progressBarWith :: LayoutModifier -> Float -> Float -> NanoUIE msg ()
progressBarWith lm h = liftNanoUI . UI.progressBarWith lm h

-- | Progress bar with layout modifier and height returning its 'Response'.
{-# INLINE progressBarWith' #-}
progressBarWith' :: LayoutModifier -> Float -> Float -> NanoUIE msg Response
progressBarWith' lm h = liftNanoUI . UI.progressBarWith' lm h

-- | Loading spinner.
{-# INLINE spinner #-}
spinner :: NanoUIE msg ()
spinner = liftNanoUI UI.spinner

-- | Loading spinner returning its 'Response'.
{-# INLINE spinner' #-}
spinner' :: NanoUIE msg Response
spinner' = liftNanoUI UI.spinner'

-- | Loading spinner with layout modifier and diameter.
{-# INLINE spinnerWith #-}
spinnerWith :: LayoutModifier -> Float -> NanoUIE msg ()
spinnerWith lm = liftNanoUI . UI.spinnerWith lm

-- | Loading spinner with layout modifier and diameter returning its 'Response'.
{-# INLINE spinnerWith' #-}
spinnerWith' :: LayoutModifier -> Float -> NanoUIE msg Response
spinnerWith' lm = liftNanoUI . UI.spinnerWith' lm

-- | Sparkline chart.
{-# INLINE sparkline #-}
sparkline :: [Float] -> NanoUIE msg ()
sparkline = liftNanoUI . UI.sparkline

-- | Sparkline chart returning its 'Response'.
{-# INLINE sparkline' #-}
sparkline' :: [Float] -> NanoUIE msg Response
sparkline' = liftNanoUI . UI.sparkline'

-- | Sparkline chart with layout modifier, width and height.
{-# INLINE sparklineWith #-}
sparklineWith :: LayoutModifier -> Float -> Float -> [Float] -> NanoUIE msg ()
sparklineWith lm w h = liftNanoUI . UI.sparklineWith lm w h

-- | Sparkline chart with layout modifier, width and height returning its 'Response'.
{-# INLINE sparklineWith' #-}
sparklineWith' :: LayoutModifier -> Float -> Float -> [Float] -> NanoUIE msg Response
sparklineWith' lm w h = liftNanoUI . UI.sparklineWith' lm w h

-- | Custom canvas drawing returning its 'Response'.
{-# INLINE canvas #-}
canvas :: LayoutModifier -> (Rect -> CanvasM ()) -> NanoUIE msg Response
canvas lm = liftNanoUI . UI.canvas lm

-- | Configured custom canvas drawing returning its 'Response'.
{-# INLINE canvasConfigured #-}
canvasConfigured :: CanvasConfig -> (Rect -> CanvasM ()) -> NanoUIE msg Response
canvasConfigured cfg = liftNanoUI . UI.canvasConfigured cfg

-- | Custom widget specification.
{-# INLINE customWidget #-}
customWidget :: CustomWidgetSpec a -> NanoUIE msg (Response, a)
customWidget = liftNanoUI . UI.customWidget

-- Scopes and behavior

-- | Wrap a subtree in a stable ID scope.
{-# INLINE scope #-}
scope :: NanoUIE msg a -> NanoUIE msg a
scope = withNanoUI UI.scope

-- | Key a subtree for dynamic lists.
{-# INLINE withKey #-}
withKey :: Hashable k => k -> NanoUIE msg a -> NanoUIE msg a
withKey k = withNanoUI (UI.withKey k)

-- | Apply a theme modification to a subtree.
{-# INLINE styled #-}
styled :: (Theme -> Theme) -> NanoUIE msg a -> NanoUIE msg a
styled f = withNanoUI (UI.styled f)

-- | Set a fixed theme for a subtree.
{-# INLINE themed #-}
themed :: Theme -> NanoUIE msg a -> NanoUIE msg a
themed t = withNanoUI (UI.themed t)

-- | Conditionally disable interactions in a subtree.
{-# INLINE disabledWhen #-}
disabledWhen :: Bool -> NanoUIE msg a -> NanoUIE msg a
disabledWhen b = withNanoUI (UI.disabledWhen b)

-- | Set default layout properties for children in a subtree.
{-# INLINE withDefaultLayout #-}
withDefaultLayout :: LayoutModifier -> NanoUIE msg a -> NanoUIE msg a
withDefaultLayout lm = withNanoUI (UI.withDefaultLayout lm)

-- | Set the cursor shape for a subtree.
{-# INLINE withCursorShape #-}
withCursorShape :: UiCursorKind -> NanoUIE msg a -> NanoUIE msg a
withCursorShape c = withNanoUI (UI.withCursorShape c)

-- | Drag-and-drop drop zone.
{-# INLINE dropZone #-}
dropZone :: (Layout -> Layout) -> NanoUIE msg a -> NanoUIE msg (a, Response, DropTarget)
dropZone lm body = withNanoUI (UI.dropZone lm) body

-- | Poll for drop events at the current location.
{-# INLINE useDrop #-}
useDrop :: Rect -> NanoUIE msg DropTarget
useDrop = liftNanoUI . UI.useDrop

-- | Track drag source activation.
{-# INLINE useDrag #-}
useDrag :: Eq a => DragHandle a -> [(a, Response)] -> NanoUIE msg (Maybe (Drag a))
useDrag dh = liftNanoUI . UI.useDrag dh

-- | Reorder items by dragging.
{-# INLINE useReorder #-}
useReorder :: [Int] -> [(Int, Response)] -> NanoUIE msg Reorder
useReorder order = liftNanoUI . UI.useReorder order

-- | Visibility sensor container.
{-# INLINE sensor #-}
sensor :: NanoUIE msg a -> NanoUIE msg (Visibility, a)
sensor = withNanoUI UI.sensor

-- | Visibility sensor with layout modifier.
{-# INLINE sensorWith #-}
sensorWith :: LayoutModifier -> NanoUIE msg a -> NanoUIE msg (Visibility, a)
sensorWith lm = withNanoUI (UI.sensorWith lm)

-- | Configured visibility sensor.
{-# INLINE sensorConfigured #-}
sensorConfigured :: SensorConfig -> NanoUIE msg a -> NanoUIE msg (Visibility, a)
sensorConfigured sc = withNanoUI (UI.sensorConfigured sc)

-- | Query visibility of a widget by ID.
{-# INLINE useVisibility #-}
useVisibility :: SensorConfig -> WidgetId -> NanoUIE msg Visibility
useVisibility sc = liftNanoUI . UI.useVisibility sc

-- Focus and clipboard

{-# INLINE holdFocus #-}
holdFocus :: WidgetId -> NanoUIE msg ()
holdFocus wid = liftNanoUI (UI.holdFocus wid)

{-# INLINE requestFocus #-}
requestFocus :: WidgetId -> NanoUIE msg ()
requestFocus = liftNanoUI . UI.requestFocus

{-# INLINE focusNext #-}
focusNext :: NanoUIE msg ()
focusNext = liftNanoUI UI.focusNext

{-# INLINE focusPrevious #-}
focusPrevious :: NanoUIE msg ()
focusPrevious = liftNanoUI UI.focusPrevious

{-# INLINE clearFocus #-}
clearFocus :: NanoUIE msg ()
clearFocus = liftNanoUI UI.clearFocus

{-# INLINE isFocused #-}
isFocused :: WidgetId -> NanoUIE msg Bool
isFocused = liftNanoUI . UI.isFocused

{-# INLINE getClipboard #-}
getClipboard :: NanoUIE msg (Maybe Text)
getClipboard = liftNanoUI UI.getClipboard

{-# INLINE setClipboard #-}
setClipboard :: Text -> NanoUIE msg Bool
setClipboard = liftNanoUI . UI.setClipboard

-- State and tasks

{-# INLINE useVar #-}
useVar :: VarSlot a => a -> NanoUIE msg (Var a)
useVar = liftNanoUI . UI.useVar

{-# INLINE useEnumVar #-}
useEnumVar :: (Bounded a, Enum a, Typeable a) => a -> NanoUIE msg (Var a)
useEnumVar = liftNanoUI . UI.useEnumVar

{-# INLINE readVar #-}
readVar :: Var a -> NanoUIE msg a
readVar = liftNanoUI . UI.readVar

{-# INLINE writeVar #-}
writeVar :: Eq a => Var a -> a -> NanoUIE msg ()
writeVar v = liftNanoUI . UI.writeVar v

{-# INLINE modifyVar #-}
modifyVar :: Eq a => Var a -> (a -> a) -> NanoUIE msg ()
modifyVar v = liftNanoUI . UI.modifyVar v

{-# INLINE useTaskStatus #-}
useTaskStatus :: Eq k => Task k a -> k -> IO a -> NanoUIE msg (TaskStatus a)
useTaskStatus t k = liftNanoUI . UI.useTaskStatus t k

{-# INLINE useTask #-}
useTask :: Eq k => Task k a -> k -> IO a -> NanoUIE msg (Maybe a)
useTask t k = liftNanoUI . UI.useTask t k

{-# INLINE useStream #-}
useStream :: Eq k => Stream k s -> k -> s -> (((s -> s) -> IO ()) -> IO ()) -> NanoUIE msg s
useStream s k initVal = liftNanoUI . UI.useStream s k initVal

{-# INLINE askWake #-}
askWake :: NanoUIE msg (IO ())
askWake = liftNanoUI UI.askWake

-- View queries and inputs

{-# INLINE windowSize #-}
windowSize :: NanoUIE msg Size
windowSize = liftNanoUI UI.windowSize

{-# INLINE windowWidth #-}
windowWidth :: NanoUIE msg Float
windowWidth = liftNanoUI UI.windowWidth

{-# INLINE windowHeight #-}
windowHeight :: NanoUIE msg Float
windowHeight = liftNanoUI UI.windowHeight

{-# INLINE uiMousePos #-}
uiMousePos :: NanoUIE msg V2
uiMousePos = liftNanoUI UI.uiMousePos

{-# INLINE askInput #-}
askInput :: NanoUIE msg Input
askInput = liftNanoUI UI.askInput

{-# INLINE uiTime #-}
uiTime :: NanoUIE msg Double
uiTime = liftNanoUI UI.uiTime

{-# INLINE requestFrame #-}
requestFrame :: NanoUIE msg ()
requestFrame = liftNanoUI UI.requestFrame

{-# INLINE lastRect #-}
lastRect :: WidgetId -> NanoUIE msg (Maybe Rect)
lastRect wid = liftNanoUI (UI.lastRect wid)

{-# INLINE currentId #-}
currentId :: NanoUIE msg WidgetId
currentId = liftNanoUI UI.currentId

{-# INLINE nextId #-}
nextId :: NanoUIE msg WidgetId
nextId = liftNanoUI UI.nextId

{-# INLINE keyPressed #-}
keyPressed :: Key -> NanoUIE msg Bool
keyPressed = liftNanoUI . UI.keyPressed

{-# INLINE keyPressedOnce #-}
keyPressedOnce :: Key -> NanoUIE msg Bool
keyPressedOnce = liftNanoUI . UI.keyPressedOnce

{-# INLINE keyReleased #-}
keyReleased :: Key -> NanoUIE msg Bool
keyReleased = liftNanoUI . UI.keyReleased

{-# INLINE keyHeld #-}
keyHeld :: Key -> NanoUIE msg Bool
keyHeld = liftNanoUI . UI.keyHeld

{-# INLINE focusedKeyPressed #-}
focusedKeyPressed :: WidgetId -> Key -> NanoUIE msg Bool
focusedKeyPressed wid = liftNanoUI . UI.focusedKeyPressed wid

{-# INLINE focusedKeyPressedOnce #-}
focusedKeyPressedOnce :: WidgetId -> Key -> NanoUIE msg Bool
focusedKeyPressedOnce wid = liftNanoUI . UI.focusedKeyPressedOnce wid

{-# INLINE useKeyNav #-}
useKeyNav :: WidgetId -> NanoUIE msg KeyNav
useKeyNav wid = liftNanoUI (UI.useKeyNav wid)

{-# INLINE keyActivated #-}
keyActivated :: WidgetId -> NanoUIE msg Bool
keyActivated wid = liftNanoUI (UI.keyActivated wid)

{-# INLINE shortcut #-}
shortcut :: Shortcut -> NanoUIE msg Bool
shortcut = liftNanoUI . UI.shortcut

{-# INLINE shortcutOnce #-}
shortcutOnce :: Shortcut -> NanoUIE msg Bool
shortcutOnce = liftNanoUI . UI.shortcutOnce

{-# INLINE mousePressed #-}
mousePressed :: MouseButton -> NanoUIE msg Bool
mousePressed = liftNanoUI . UI.mousePressed

{-# INLINE mouseReleased #-}
mouseReleased :: MouseButton -> NanoUIE msg Bool
mouseReleased = liftNanoUI . UI.mouseReleased

{-# INLINE mouseHeld #-}
mouseHeld :: MouseButton -> NanoUIE msg Bool
mouseHeld = liftNanoUI . UI.mouseHeld

{-# INLINE useInputMethod #-}
useInputMethod :: WidgetId -> InputPurpose -> Rect -> NanoUIE msg (Maybe Composition)
useInputMethod wid purpose caret = liftNanoUI (UI.useInputMethod wid purpose caret)

{-# INLINE keepAnimating #-}
keepAnimating :: HasResponse r => r -> NanoUIE msg ()
keepAnimating r = liftNanoUI (UI.keepAnimating r)

{-# INLINE wakeAfter #-}
wakeAfter :: Double -> NanoUIE msg ()
wakeAfter = liftNanoUI . UI.wakeAfter

{-# INLINE animateTo #-}
animateTo :: Transition -> Float -> NanoUIE msg Float
animateTo t = liftNanoUI . UI.animateTo t

{-# INLINE animateToA #-}
animateToA :: Animatable a => Transition -> a -> NanoUIE msg a
animateToA t = liftNanoUI . UI.animateToA t

{-# INLINE animate #-}
animate :: Transition -> Float -> Float -> NanoUIE msg Float
animate t a = liftNanoUI . UI.animate t a

{-# INLINE pulse #-}
pulse :: Float -> NanoUIE msg Float
pulse = liftNanoUI . UI.pulse

{-# INLINE uiTheme #-}
uiTheme :: NanoUIE msg Theme
uiTheme = liftNanoUI UI.uiTheme

{-# INLINE askDefaultLayout #-}
askDefaultLayout :: NanoUIE msg Layout
askDefaultLayout = liftNanoUI UI.askDefaultLayout

{-# INLINE askWindow #-}
askWindow :: NanoUIE msg WindowState
askWindow = liftNanoUI UI.askWindow

{-# INLINE askWindowCapabilities #-}
askWindowCapabilities :: NanoUIE msg WindowCapabilities
askWindowCapabilities = liftNanoUI UI.askWindowCapabilities

{-# INLINE setWindowTitleUi #-}
setWindowTitleUi :: Text -> NanoUIE msg ()
setWindowTitleUi = liftNanoUI . UI.setWindowTitleUi

{-# INLINE setWindowIconUi #-}
setWindowIconUi :: RgbaPixels -> NanoUIE msg ()
setWindowIconUi = liftNanoUI . UI.setWindowIconUi

{-# INLINE setWindowMinSizeUi #-}
setWindowMinSizeUi :: Maybe Size -> NanoUIE msg ()
setWindowMinSizeUi = liftNanoUI . UI.setWindowMinSizeUi

{-# INLINE setWindowMaxSizeUi #-}
setWindowMaxSizeUi :: Maybe Size -> NanoUIE msg ()
setWindowMaxSizeUi = liftNanoUI . UI.setWindowMaxSizeUi

{-# INLINE setWindowOpacityUi #-}
setWindowOpacityUi :: Float -> NanoUIE msg ()
setWindowOpacityUi = liftNanoUI . UI.setWindowOpacityUi

{-# INLINE setWindowModeUi #-}
setWindowModeUi :: WindowMode -> NanoUIE msg ()
setWindowModeUi = liftNanoUI . UI.setWindowModeUi

{-# INLINE moveWindowUi #-}
moveWindowUi :: Int -> Int -> NanoUIE msg ()
moveWindowUi x y = liftNanoUI (UI.moveWindowUi x y)

{-# INLINE centerWindowUi #-}
centerWindowUi :: NanoUIE msg ()
centerWindowUi = liftNanoUI UI.centerWindowUi

{-# INLINE resizeWindowUi #-}
resizeWindowUi :: Size -> NanoUIE msg ()
resizeWindowUi = liftNanoUI . UI.resizeWindowUi

{-# INLINE minimizeWindowUi #-}
minimizeWindowUi :: NanoUIE msg ()
minimizeWindowUi = liftNanoUI UI.minimizeWindowUi

{-# INLINE maximizeWindowUi #-}
maximizeWindowUi :: NanoUIE msg ()
maximizeWindowUi = liftNanoUI UI.maximizeWindowUi

{-# INLINE restoreWindowUi #-}
restoreWindowUi :: NanoUIE msg ()
restoreWindowUi = liftNanoUI UI.restoreWindowUi

{-# INLINE toggleMaximizedUi #-}
toggleMaximizedUi :: NanoUIE msg ()
toggleMaximizedUi = liftNanoUI UI.toggleMaximizedUi

{-# INLINE quitUi #-}
quitUi :: NanoUIE msg ()
quitUi = liftNanoUI UI.quitUi

{-# INLINE getScrollMetricsUi #-}
getScrollMetricsUi :: WidgetId -> NanoUIE msg (Maybe ScrollMetrics)
getScrollMetricsUi = liftNanoUI . UI.getScrollMetricsUi

{-# INLINE setScrollOffsetUi #-}
setScrollOffsetUi :: WidgetId -> V2 -> NanoUIE msg ()
setScrollOffsetUi wid = liftNanoUI . UI.setScrollOffsetUi wid

{-# INLINE scrollToUi #-}
scrollToUi :: WidgetId -> V2 -> ScrollBehavior -> NanoUIE msg ()
scrollToUi wid axes = liftNanoUI . UI.scrollToUi wid axes

{-# INLINE scrollByUi #-}
scrollByUi :: WidgetId -> V2 -> ScrollBehavior -> NanoUIE msg ()
scrollByUi wid delta = liftNanoUI . UI.scrollByUi wid delta

{-# INLINE scrollPagesUi #-}
scrollPagesUi :: WidgetId -> V2 -> ScrollBehavior -> NanoUIE msg ()
scrollPagesUi wid delta = liftNanoUI . UI.scrollPagesUi wid delta

{-# INLINE scrollRectIntoViewUi #-}
scrollRectIntoViewUi :: WidgetId -> Rect -> ScrollAlign -> ScrollBehavior -> NanoUIE msg ()
scrollRectIntoViewUi wid r align = liftNanoUI . UI.scrollRectIntoViewUi wid r align

{-# INLINE setScrollStepUi #-}
setScrollStepUi :: WidgetId -> Float -> NanoUIE msg ()
setScrollStepUi wid = liftNanoUI . UI.setScrollStepUi wid

{-# INLINE useImageRgba #-}
useImageRgba :: Eq k => ImageHandle k -> k -> Int -> Int -> ByteString -> NanoUIE msg (Maybe ImageId)
useImageRgba h k w h' = liftNanoUI . UI.useImageRgba h k w h'

{-# INLINE requestScreenshot #-}
requestScreenshot :: (Maybe Screenshot -> IO ()) -> NanoUIE msg ()
requestScreenshot = liftNanoUI . UI.requestScreenshot

{-# INLINE askScreenshot #-}
askScreenshot :: NanoUIE msg (IO (Maybe Screenshot))
askScreenshot = liftNanoUI UI.askScreenshot

{-# INLINE useScreenshot #-}
useScreenshot :: Eq k => Task k (Maybe Screenshot) -> k -> NanoUIE msg (Maybe Screenshot)
useScreenshot owner k = liftNanoUI (UI.useScreenshot owner k)

{-# INLINE explainLayout #-}
explainLayout :: Bool -> NanoUIE msg ()
explainLayout = liftNanoUI . UI.explainLayout

{-# INLINE explainingLayout #-}
explainingLayout :: NanoUIE msg Bool
explainingLayout = liftNanoUI UI.explainingLayout

{-# INLINE explainedNode #-}
explainedNode :: NanoUIE msg (Maybe ExplainedNode)
explainedNode = liftNanoUI UI.explainedNode

{-# INLINE explainScope #-}
explainScope :: NanoUIE msg a -> NanoUIE msg a
explainScope = withNanoUI UI.explainScope
