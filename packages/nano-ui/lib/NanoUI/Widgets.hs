module NanoUI.Widgets
  ( Response (..)
  , Responding (..)
  , Clickable (..)
  , RightClickable (..)
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
  , button
  , checkbox
  , slider
  , sliderEx
  , textInput
  , SearchFieldConfig (..)
  , defaultSearchFieldConfig
  , searchField
  , searchFieldConfigured
  , comboBox
  , textArea
  , applyTextInputMenuAction
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
  , boundedSelect
  , enumSelect
  , useEnumSelect
  , radioFieldset
  , boundedRadioFieldset
  , enumRadio
  , useEnumRadio
  , useRadio
  , TreeItem (..)
  , tree
  , colorPicker
  , modal
  , window
  , image
  , label_
  , onClick
  , clickButton
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
  , animate
  , animateEase
  , animateEaseDelay
  , animateTo
  , animateToEase
  , animateToEaseDelay
  , animateToSpring
  , animateToA
  , animateToSpringA
  , pulse
  , keepAnimating
  , sliderValueText
  , textInputTerminalText
  , colorPickerLabelText
  , colorPickerCurrentLabel
  , colorPickerNewLabel
  , colorPickerDisplayText
  , colorPickerToHex
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
  )
where

import Control.Monad (void, when)
import Data.IORef (readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.IntMap.Strict qualified as IM
import Data.Text (Text)
import Data.Text qualified as T
import Effectful (Eff, type (:>))
import GHC.Clock (getMonotonicTime)
import NanoUI.Context
  ( Context (..)
  , getLastPointerBlocked
  , getStore
  , intKey
  , isDisabled
  , markDirty
  , markEscapeConsumed
  , pointerBlockedByModal
  , registerFocusable
  , setStore
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
import NanoUI.Widgets.Menu
  ( contextMenu
  , withContextMenu
  , contextMenuArea
  , useContextMenu
  , menuItem
  , menuItemWithShortcut
  , menuItemWithIcon
  , menuItemDisabled
  , menuSeparator
  , menuHeader
  )
import NanoUI.Widgets.Node (RightClickable (..), onRightClick, setSubmitted)
import NanoUI.Widgets.PaneGrid
  ( GridAxis (..)
  , PaneGridConfig (..)
  , PaneGridCtx (..)
  , PaneGridResponse (..)
  , PaneView (..)
  , defaultPaneGridConfig
  , paneGrid
  )
import NanoUI.Font
  ( fmLineHeight
  , sliderTrackBounds
  , sliderHandleSlack
  )
import NanoUI.Frame.Hit (findNodeByWidgetId, scrollHitRect)
import NanoUI.Types (isCellHost)
import NanoUI.Icons (checkboxMark)
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Input (Key (..), inputKeys, inputMouseDown, inputMousePos, inputMousePressed, inputMouseReleased, inputScroll)
import NanoUI.Layout.Arena (NodeType (..), setOptions)
import NanoUI.Monad (Ui, askContext, askInput, nextId, uiIO, withKey)
import NanoUI.Frame.Select (comboDropPickIndex, comboDropRect, comboScrollGeom, selectDropPickIndex, selectDropRect, selectItemH)
import NanoUI.Store
  ( WidgetStore (..)
  , boolInt
  , intBool
  , isSelectOpen
  , setSelectOpen
  , slotAnchor
  , slotComboContentW
  , slotComboCount
  , slotComboCommitted
  , slotComboDrag
  , slotComboDragOff
  , slotComboFocus
  , slotComboHighlight
  , slotComboLive
  , slotComboScroll
  , slotComboScrollX
  , slotCursor
  , slotKey
  , slotSearchAge
  , slotSearchCommitted
  , slotTextAreaViewport
  )
import NanoUI.Style
  ( Layout (..)
  , alignEnd
  , alignMid
  , defaultLayout
  , fillW
  , fontHeading
  , fontMono
  , fontMuted
  , fontDanger
  , fontBold
  , fontItalic
  , fontUnderline
  , gap
  , minW
  , padXY
  , tight
  )
import NanoUI.Types
  ( Color (..)
  , ImageId (..)
  , Rect (..)
  , colorToWord32
  , rectContains
  , v2X
  , v2Y
  )
import NanoUI.Widgets.Behavior (DragAxis (..), useDrag1D)
import NanoUI.WidgetText
  ( colorPickerCurrentLabel
  , colorPickerDisplayText
  , colorPickerFromHex
  , colorPickerLabelText
  , colorPickerNewLabel
  , colorPickerToHex
  , sliderValueText
  , textInputFlagSearch
  , textInputTerminalText
  )
import NanoUI.Widgets.Animate
  ( animate
  , animateEase
  , animateEaseDelay
  , animateTo
  , animateToA
  , animateToEase
  , animateToEaseDelay
  , animateToSpring
  , animateToSpringA
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
import NanoUI.Widgets.ColorPicker (colorPicker)
import NanoUI.Widgets.Drawing (DrawOp (..), DrawingBuild, drawing, drawingVersioned, drawingCached)
import NanoUI.Widgets.Custom
import NanoUI.Widgets.Layout
  ( column
  , columnWith
  , column'
  , flex
  , grid
  , gridWith
  , grid'
  , label
  , labelWith
  , labelEx
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
  , scroll
  , scrollWith
  , scroll'
  , scroll2DWith
  , scroll2D'
  , scrollArea
  , scrollArea2D
  , scrollAreaIdConfigured
  , scrollConfigured
  , sep
  , separator
  , spacer
  )
import NanoUI.Widgets.Node
  ( Clickable (..)
  , Responding (..)
  , Response (..)
  , addWidget
  , addWidgetResp
  , addWidgetStyled
  , addWidgetWithOptions
  , setChanged
  , setClicked
  , setHovered
  )
import NanoUI.Widgets.Overlay (modal, window)
import NanoUI.Widgets.Radio
  ( boundedRadioFieldset
  , enumRadio
  , radioFieldset
  , useRadio
  )
import NanoUI.Widgets.TextInput
  ( TextInputState (..)
  , applyTextInputMenuAction
  , processTextInput
  , searchFieldLayout
  , textInputLayout
  )
import NanoUI.Widgets.TextArea
  ( loadTextAreaState
  , processTextArea
  , saveTextAreaState
  , textAreaLayout
  )
import qualified NanoUI.Widgets.TextArea as TA
import NanoUI.Widgets.TextBuffer as TB
import NanoUI.Widgets.Tree
  ( TreeItem (..)
  , tree
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

{-# INLINE onClick #-}
onClick :: Clickable r => r -> Eff es () -> Eff es ()
onClick resp act = when (respIsClicked resp) act

{-# INLINE clickButton #-}
clickButton :: Ui :> es => Text -> Eff es () -> Eff es ()
clickButton txt act = button txt >>= \resp -> onClick resp act

{-# INLINE label_ #-}
label_ :: Ui :> es => Text -> Eff es ()
label_ txt = void (label txt)

{-# INLINE image_ #-}
image_ :: Ui :> es => Layout -> ImageId -> Eff es ()
image_ layout iid = void (image layout iid)

box :: Ui :> es => Layout -> Color -> Eff es ()
box layout col = do
  wid <- nextId
  void
    ( addWidgetStyled
        wid
        NodeBox
        T.empty
        0
        layout
        (fromIntegral (colorToWord32 col))
        Nothing
    )

heading :: Ui :> es => Text -> Eff es ()
heading txt = void (labelWith (tight . fontHeading) txt)

muted :: Ui :> es => Text -> Eff es ()
muted txt = void (labelWith (fillW . fontMuted) txt)

mono :: Ui :> es => Text -> Eff es ()
mono txt = void (labelWith fontMono txt)

danger :: Ui :> es => Text -> Eff es ()
danger txt = void (labelWith (fillW . fontDanger) txt)

bold :: Ui :> es => Text -> Eff es ()
bold txt = void (labelWith fontBold txt)

italic :: Ui :> es => Text -> Eff es ()
italic txt = void (labelWith fontItalic txt)

underline :: Ui :> es => Text -> Eff es ()
underline txt = void (labelWith fontUnderline txt)

kv :: Ui :> es => Text -> Text -> Eff es ()
kv k v = do
  ctx <- askContext
  let
    host = ctxHostProfile ctx
    terminal = isCellHost host
    rowLayout =
      tight . gap (if terminal then 1 else 12) . alignMid . fillW $ defaultLayout
    keyLayout =
      if terminal then tight else tight . minW 88
  void $
    row' rowLayout $ do
      void (labelEx (keyLayout defaultLayout) k)
      void (labelEx (tight . fillW . alignEnd $ defaultLayout) (T.stripEnd v))

kvMonoRowLayout, kvMonoRowTerminalLayout :: Layout
kvMonoRowLayout = tight . gap 12 . alignMid . fillW $ defaultLayout
kvMonoRowTerminalLayout = tight . gap 1 . alignMid . fillW $ defaultLayout

kvMonoKeyLayout, kvMonoKeyTerminalLayout :: Layout
kvMonoKeyLayout = tight . minW 88 $ defaultLayout
kvMonoKeyTerminalLayout = tight defaultLayout

kvMonoValLayout :: Layout
kvMonoValLayout = tight . fillW . alignEnd . fontMono $ defaultLayout

kvMono :: Ui :> es => Text -> Text -> Eff es ()
kvMono k v = do
  ctx <- askContext
  let
    terminal = isCellHost (ctxHostProfile ctx)
    rLayout = if terminal then kvMonoRowTerminalLayout else kvMonoRowLayout
    kLayout = if terminal then kvMonoKeyTerminalLayout else kvMonoKeyLayout
    val = if T.isSuffixOf " " v then T.stripEnd v else v
  void $
    row' rLayout $ do
      void (labelEx kLayout k)
      void (labelEx kvMonoValLayout val)

kvBlock :: Ui :> es => [(Text, Text)] -> Eff es ()
kvBlock rows =
  let maxK = foldl' (\acc (k, _) -> max acc (T.length k)) 0 rows
      padK k = T.justifyLeft maxK ' ' k
   in void $
        labelEx
          (tight . gap 0 . fontMono $ defaultLayout)
          (T.unlines [padK k <> "  " <> v | (k, v) <- rows])

card :: Ui :> es => Eff es a -> Eff es a
card = panelWith (minW 300 . padXY 12 10 . gap 8 . fillW)

toolbar :: Ui :> es => Eff es a -> Eff es a
toolbar = rowWith (tight . gap 8 . alignMid . fillW)

image :: Ui :> es => Layout -> ImageId -> Eff es Response
image layout (ImageId tid) = do
  wid <- nextId
  let
    stored = if tid <= 0 then T.empty else T.pack (show tid)
  addWidget wid NodeImage stored 0 layout

button :: Ui :> es => Text -> Eff es Response
button = buttonEx True

buttonEx :: (Ui :> es) => Bool -> Text -> Eff es Response
buttonEx enabled txt = do
  wid <- nextId
  ctx <- askContext
  uiIO $ registerFocusable ctx wid
  let stored =
        if isCellHost (ctxHostProfile ctx)
          then "[ " <> txt <> " ]"
          else txt
  resp <- addWidget wid NodeButton stored 0 defaultLayout
  disabled <- uiIO (isDisabled ctx wid)
  let
    active = enabled && not disabled
  pure
    $ setClicked (active && respClicked resp)
    $ setHovered (active && respHovered resp) resp

checkbox :: Ui :> es => Text -> Bool -> Eff es (Response, Bool)
checkbox txt initial = do
  wid <- nextId
  ctx <- askContext
  store <- uiIO (getStore ctx)
  let
    key = intKey wid
    current = intBool (IM.findWithDefault (boolInt initial) key (storeInt store))
    host = ctxHostProfile ctx
    nodeText =
      if isCellHost host
        then checkboxMark (ctxIcons ctx) current <> txt
        else txt
  resp <-
    addWidgetResp
      wid
      NodeCheckbox
      nodeText
      (if current then 1 else 0)
      defaultLayout
      Nothing
  let
    clicked = respClicked resp
    display = if clicked then not current else current
  when clicked $
    uiIO $ do
      st <- getStore ctx
      setStore ctx (st {storeInt = IM.insert key (boolInt display) (storeInt st)})
      markDirty ctx
  pure (setChanged clicked resp, display)

slider ::
  Ui :> es => Text -> Float -> Float -> Float -> Eff es (Response, Float)
slider = sliderEx (fillW defaultLayout)

sliderEx ::
  Ui :> es =>
  Layout -> Text -> Float -> Float -> Float -> Eff es (Response, Float)
sliderEx layout lbl minV maxV initial = do
  wid <- nextId
  ctx <- askContext
  inp <- askInput
  store <- uiIO (getStore ctx)
  let
    key = intKey wid
    current = IM.findWithDefault initial key (storeFloat store)
    frac = if maxV > minV then (current - minV) / (maxV - minV) else 0
    host = ctxHostProfile ctx
    fm = ctxFontMetrics ctx
    nodeText = lbl
  resp <- addWidget wid NodeSlider nodeText frac layout
  active <- uiIO (readIORef (ctxActiveId ctx))
  blocked <- uiIO (getLastPointerBlocked ctx)
  mrect <- uiIO (scrollHitRect ctx wid)
  let
    isActive = active == wid
    heldByOther =
      inputMouseDown inp
        && not (inputMousePressed inp)
        && hashWidgetId active /= 0
        && not isActive
    track0 =
      case mrect of
        Just (Rect x y w h) ->
          let tr = sliderTrackBounds host fm lbl x y w h
           in if isCellHost host
                then tr
                else Rect (rectX tr) (rectY tr - sliderHandleSlack) (rectW tr) (rectH tr + 2 * sliderHandleSlack)
        Nothing -> Rect 0 0 0 0
    track = if blocked || heldByOther then Rect 0 0 0 0 else track0
  (dragged, dragging) <- withKey ("drag" :: Text) (useDrag1D DragAxisX minV maxV current track)
  when (dragging && not isActive) $ uiIO $ writeIORef (ctxActiveId ctx) wid
  when ((not dragging || blocked) && isActive) $
    uiIO $ writeIORef (ctxActiveId ctx) (WidgetId 0)
  let finalVal = if dragging then dragged else current
  when (finalVal /= current) $
    uiIO $ setStore ctx (store {storeFloat = IM.insert key finalVal (storeFloat store)})
  pure (setChanged (finalVal /= current) resp, finalVal)

data TextInputConfig = TextInputConfig
  { ticPlaceholder :: !Text
  , ticPassword :: !Bool
  , ticLayout :: !Layout
  }
  deriving (Eq, Show)

defaultTextInputConfig :: TextInputConfig
defaultTextInputConfig =
  TextInputConfig
    { ticPlaceholder = ""
    , ticPassword = False
    , ticLayout = textInputLayout
    }

textInput :: Ui :> es => Text -> Text -> Eff es (Response, Text)
textInput = textInputConfigured defaultTextInputConfig

textInputWithPlaceholder :: Ui :> es => Text -> Text -> Text -> Eff es (Response, Text)
textInputWithPlaceholder placeholder lbl initial =
  textInputConfigured (defaultTextInputConfig {ticPlaceholder = placeholder}) lbl initial

textInputPassword :: Ui :> es => Text -> Text -> Eff es (Response, Text)
textInputPassword lbl initial =
  textInputConfigured (defaultTextInputConfig {ticPassword = True}) lbl initial

textInputConfigured :: Ui :> es => TextInputConfig -> Text -> Text -> Eff es (Response, Text)
textInputConfigured cfg lbl initial =
  buildTextInput
    0
    (ticLayout cfg)
    lbl
    initial
    Nothing

-- | Shared single-line field builder. @styleIdx@ may carry the search flag on a
-- @NodeTextInput@; when @mDebounceMs@ is present the returned change pulse is
-- delayed until the text has been idle for that long (immediate for clear clicks).
buildTextInput ::
  Ui :> es =>
  Int ->
  Layout ->
  Text ->
  Text ->
  Maybe Float ->
  Eff es (Response, Text)
buildTextInput styleIdx layout lbl initial mDebounceMs = do
  wid <- nextId
  ctx <- askContext
  uiIO $ registerFocusable ctx wid
  inp <- askInput
  store <- uiIO (getStore ctx)
  let key = intKey wid
  current <- case IM.lookup key (storeText store) of
    Nothing -> do
      uiIO $ setStore ctx (store {storeText = IM.insert key initial (storeText store)})
      pure initial
    Just t -> pure t
  let
    cursor = fromMaybe (T.length current) (IM.lookup (slotKey slotCursor key) (storeInt store))
    anchor = fromMaybe cursor (IM.lookup (slotKey slotAnchor key) (storeInt store))
  focus <- uiIO (readIORef (ctxFocusId ctx))
  blocked <- uiIO (pointerBlockedByModal ctx)
  let
    isFocus = focus == wid && not blocked
  newState <-
    if isFocus
      then uiIO (processTextInput ctx inp (TextInputState current cursor anchor))
      else pure (TextInputState current cursor anchor)
  let
    newText = tisText newState
    newCursor = tisCursor newState
    newAnchor = tisAnchor newState
  when (newText /= current || newCursor /= cursor || newAnchor /= anchor)
    $ uiIO
    $ setStore
      ctx
      ( store
          { storeText = IM.insert key newText (storeText store)
          , storeInt =
              IM.insert (slotKey slotCursor key) newCursor $
                IM.insert (slotKey slotAnchor key) newAnchor (storeInt store)
          }
      )
  let submitted = isFocus && KeyEnter `elem` inputKeys inp
  changed <- case mDebounceMs of
    Nothing -> pure (newText /= current)
    Just ms -> uiIO (debounceSearchChanged ctx key isFocus (newText /= current) ms)
  resp <- addWidgetStyled wid NodeTextInput lbl 0 layout styleIdx Nothing
  pure (setSubmitted submitted (setChanged changed resp), newText)

-- | Debounced change pulse for a search field. Fires when the text differs from
-- the last committed query and either the field is empty, lost focus, or has
-- been idle for @ms@ (trailing edge). Field text lives under @key@; the last
-- committed query under 'slotSearchCommitted'.
debounceSearchChanged :: Context -> Int -> Bool -> Bool -> Float -> IO Bool
debounceSearchChanged ctx key focused rawChanged ms = do
  store <- getStore ctx
  let
    committedKey = slotKey slotSearchCommitted key
    ageKey = slotKey slotSearchAge key
    fieldText = IM.findWithDefault "" key (storeText store)
    committedMissing = not (IM.member committedKey (storeText store))
    committed = IM.findWithDefault fieldText committedKey (storeText store)
    dirty = fieldText /= committed
    needClock = rawChanged || dirty
  now <- if needClock then getMonotonicTime else pure 0
  let
    -- Debounce timing stays in Double: wall-clock seconds as Float lose
    -- resolution at long uptimes (~125 ms at 12 days), which would shift
    -- (or skip) the trailing-edge window.
    lastEdit = IM.findWithDefault now ageKey (storeDouble store)
    deadline = realToFrac ms :: Double
    idleMs = (now - lastEdit) * 1000
    commit =
      not rawChanged
        && dirty
        && (T.null fieldText || not focused || idleMs >= deadline)
  when (rawChanged || commit || committedMissing) $ do
    st <- getStore ctx
    let
      texts =
        if commit || committedMissing
          then IM.insert committedKey fieldText (storeText st)
          else storeText st
      doubles =
        if rawChanged || commit
          then IM.insert ageKey now (storeDouble st)
          else storeDouble st
    setStore ctx (st {storeText = texts, storeDouble = doubles})
  pure commit

-- | Search field: a caption-less 'NodeTextInput' with an embedded magnifier and
-- clear button. The label acts as the placeholder. Change pulses are debounced
-- (trailing edge); clearing with the embedded button fires immediately.
data SearchFieldConfig = SearchFieldConfig
  { sfcPlaceholder :: !Text
  , sfcDebounceMs :: !Float
  , sfcLayout :: !Layout
  }
  deriving (Eq, Show)

defaultSearchFieldConfig :: SearchFieldConfig
defaultSearchFieldConfig =
  SearchFieldConfig
    { sfcPlaceholder = "Search…"
    , sfcDebounceMs = 300
    , sfcLayout = searchFieldLayout
    }

searchField :: Ui :> es => Text -> Text -> Eff es (Response, Text)
searchField placeholder initial =
  searchFieldConfigured (defaultSearchFieldConfig {sfcPlaceholder = placeholder}) initial

searchFieldConfigured ::
  Ui :> es => SearchFieldConfig -> Text -> Eff es (Response, Text)
searchFieldConfigured cfg initial =
  buildTextInput
    textInputFlagSearch
    (sfcLayout cfg)
    (sfcPlaceholder cfg)
    initial
    (Just (sfcDebounceMs cfg))

-- | Maximum suggestion rows the combo dropdown shows at once; Up/Down walk
-- the highlight and the wheel scrolls the list through a sliding window.
comboBoxMaxVisible :: Int
comboBoxMaxVisible = 8

-- | Rows scrolled per wheel notch.
comboBoxRowsPerNotch :: Float
comboBoxRowsPerNotch = 3

-- | Case-insensitive substring filter behind the combo's suggestion list.
comboFiltered :: [Text] -> Text -> [Text]
comboFiltered opts q
  | T.null q = opts
  | otherwise =
      let needle = T.toLower q
       in filter (T.isInfixOf needle . T.toLower) opts

-- | Combo box: the 'searchField' with a select-style dropdown of options.
-- While the field holds focus, the shared select dropdown overlay lists the
-- options filtered by the field text (all of them while it is empty).
-- Typing edits the live field text but never commits it: the committed value
-- (and the 'respChanged' pulse) only changes on Enter, on clicking a row, or
-- when the field loses focus; Escape reverts the live text to the last
-- committed value. Hovering a row highlights it (and makes it the Enter
-- target); Enter commits the highlighted row only. Up/Down move the
-- highlight, the wheel scrolls the list (vertically over the rows,
-- horizontally over the widest rows; the scrollbar thumbs drag too). The
-- value is free text: options are suggestions, not a closed set.
comboBox :: Ui :> es => Text -> [Text] -> Text -> Eff es (Response, Text)
comboBox placeholder options initial = do
  (resp, text) <-
    buildTextInput textInputFlagSearch searchFieldLayout placeholder initial Nothing
  ctx <- askContext
  inp <- askInput
  let wid = rawRespId resp
      key = intKey wid
      hiKey = slotKey slotComboHighlight key
      winKey = slotKey slotComboScroll key
      xKey = slotKey slotComboScrollX key
      cntKey = slotKey slotComboCount key
      cwKey = slotKey slotComboContentW key
      dragKey = slotKey slotComboDrag key
      offKey = slotKey slotComboDragOff key
      committedKey = slotKey slotComboCommitted key
      focusKey = slotKey slotComboFocus key
      liveKey = slotKey slotComboLive key
      keys = inputKeys inp
  focus <- uiIO (readIORef (ctxFocusId ctx))
  blocked <- uiIO (pointerBlockedByModal ctx)
  store <- uiIO (getStore ctx)
  let
    isFocus = focus == wid && not blocked
    displayed = comboFiltered options text
    n = length displayed
    vis = max 1 comboBoxMaxVisible
    storedHi = IM.findWithDefault (-1) hiKey (storeInt store)
    -- Typing clears the highlight (-1): it never pre-selects a row.
    hi0 = if respChanged resp then -1 else storedHi
    storedWin = IM.findWithDefault 0 winKey (storeInt store)
    win0 = if respChanged resp then 0 else storedWin
    storedX = IM.findWithDefault 0 xKey (storeFloat store)
    storedContentW = IM.findWithDefault 0 cwKey (storeFloat store)
    drag0 = IM.findWithDefault 0 dragKey (storeInt store)
    dragOff0 = IM.findWithDefault 0 offKey (storeFloat store)
    committed0 = IM.findWithDefault initial committedKey (storeText store)
    live0 = IM.findWithDefault text liveKey (storeText store)
    hadFocus = IM.findWithDefault 0 focusKey (storeInt store) /= 0
    nav
      | not isFocus || n <= 0 = 0 :: Int
      | KeyDown `elem` keys = 1
      | KeyUp `elem` keys = -1
      | otherwise = 0
    hi
      | nav == 0 = hi0
      | hi0 < 0 = if nav > 0 then 0 else n - 1
      | otherwise = max 0 (min (n - 1) (hi0 + nav))
    clampWin v = max 0 (min v (max 0 (n - vis)))
    -- Keep the highlighted row inside the window after keyboard navigation.
    alignWin v
      | n <= vis = 0
      | hi < v = hi
      | hi >= v + vis = hi - vis + 1
      | otherwise = clampWin v
  contentW <- uiIO $
    if isFocus && not (null displayed)
      then maximum . (0 :) <$> mapM (fmap fst . ctxMeasureText ctx) displayed
      else pure storedContentW
  let
    Rect rx ry rw rh = respRect resp
    mouse = inputMousePos inp
    dropRect = comboDropRect (ctxHostProfile ctx) (ctxFontMetrics ctx) rx ry rw rh (min vis n) n contentW
    overDrop = isFocus && rw > 0 && rh > 0 && rectContains dropRect mouse
    itemH = selectItemH (ctxHostProfile ctx) rh
    -- Hover highlights the row under the pointer (and makes it the Enter
    -- target); it never commits by itself. Rows on screen belong to the
    -- previous frame's window, so the hit test maps through storedWin.
    hoverIdx
      | overDrop = (storedWin +) <$> comboDropPickIndex dropRect itemH (min vis n) (v2Y mouse)
      | otherwise = Nothing
    hiRaw = fromMaybe hi hoverIdx
    -- A hover mapped through a stale window can point past a shrunken list:
    -- highlight nothing then (the raw index must never reach `!!`).
    hi' = if hiRaw < n then hiRaw else -1
    -- Scrollbar geometry from the pre-frame scroll state (the thumb the user
    -- is looking at when a drag starts).
    (_, vSb, hSb, usableW) = comboScrollGeom dropRect n vis storedWin storedX contentW
    maxOffX = max 0 (contentW - usableW)
    onVThumb = maybe False (\(_, th) -> rectContains th mouse) vSb
    onVTrack = maybe False (\(t, _) -> rectContains t mouse) vSb
    onHThumb = maybe False (\(_, th) -> rectContains th mouse) hSb
    onHTrack = maybe False (\(t, _) -> rectContains t mouse) hSb
    pressed = not blocked && inputMousePressed inp
    down = not blocked && inputMouseDown inp
    startV = pressed && overDrop && onVTrack
    startH = pressed && overDrop && not startV && onHTrack
    vThumbR = maybe (Rect 0 0 0 0) snd vSb
    vTrackR = maybe (Rect 0 0 0 0) fst vSb
    hThumbR = maybe (Rect 0 0 0 0) snd hSb
    hTrackR = maybe (Rect 0 0 0 0) fst hSb
    vGrab = if onVThumb then v2Y mouse - rectY vThumbR else rectH vThumbR / 2
    hGrab = if onHThumb then v2X mouse - rectX hThumbR else rectW hThumbR / 2
    drag1
      | startV = 1
      | startH = 2
      | down && drag0 /= 0 = drag0
      | otherwise = 0
    -- Thumb-anchored drags move from the next frame on; track presses jump
    -- the window to the click immediately.
    draggingV = down && drag1 == 1 && ((drag0 == 1 && not startV) || (startV && not onVThumb))
    draggingH = down && drag1 == 2 && ((drag0 == 2 && not startH) || (startH && not onHThumb))
    dragWin = clampWin (round ((v2Y mouse - rectY vTrackR - dragOff0) / max 1 (rectH vTrackR - rectH vThumbR) * fromIntegral (n - vis)))
    dragX = max 0 (min maxOffX ((v2X mouse - rectX hTrackR - dragOff0) / max 1 (rectW hTrackR - rectW hThumbR) * maxOffX))
    wheelRows = round (v2Y (inputScroll inp) * comboBoxRowsPerNotch) :: Int
    wheelDelta = if overDrop then wheelRows else 0
    xWheel = if overDrop then v2X (inputScroll inp) * 20 else 0
    win
      | draggingV = dragWin
      | nav /= 0 = alignWin (win0 + wheelDelta)
      | otherwise = clampWin (win0 + wheelDelta)
    xOff
      | draggingH = dragX
      | otherwise = max 0 (min maxOffX (storedX + xWheel))
    dragKind' = if down then drag1 else 0
    dragOff' | startV = vGrab | startH = hGrab | otherwise = dragOff0
    -- Enter commits only an explicitly highlighted row (hover or Up/Down).
    picked = isFocus && n > 0 && hi' >= 0 && KeyEnter `elem` keys
    pickedText = displayed !! hi'
    escDismiss = isFocus && KeyEscape `elem` keys
    -- Commit points: Enter, a row click (the frame-side pick lands as a
    -- frame-start text the widget did not produce), and losing focus (which
    -- the blur frame after the focus clear detects). Escape is a cancel: it
    -- reverts the live text to the last committed value without committing.
    externalText = not (respChanged resp) && text /= live0
    commitText
      | picked = Just pickedText
      | externalText = Just text
      | hadFocus && not isFocus = Just text
      | otherwise = Nothing
    commitPulse = maybe False (/= committed0) commitText
    finalText
      | picked = pickedText
      | escDismiss = committed0
      | otherwise = text
    stateChanged =
      picked || nav /= 0 || escDismiss || wheelDelta /= 0 || xWheel /= 0
        || win /= storedWin || xOff /= storedX || hi' /= storedHi
        || dragKind' /= drag0 || commitPulse || hadFocus /= isFocus
  when (isFocus || stateChanged) $
    uiIO $ do
      st <- getStore ctx
      let len = T.length finalText
          int0 =
            IM.insert hiKey hi' $
              IM.insert winKey win $
                IM.insert cntKey n $
                  IM.insert focusKey (boolInt isFocus) $
                    IM.insert dragKey dragKind' (storeInt st)
          flt0 =
            IM.insert xKey xOff $
              IM.insert cwKey contentW $
                IM.insert offKey dragOff' (storeFloat st)
          intMap =
            if picked
              then
                IM.insert (slotKey slotCursor key) len $
                  IM.insert (slotKey slotAnchor key) len int0
              else int0
          texts =
            IM.insert liveKey finalText $
              IM.insert committedKey (fromMaybe committed0 commitText) $
                IM.insert key finalText (storeText st)
      setStore ctx st {storeText = texts, storeInt = intMap, storeFloat = flt0}
      when escDismiss $ do
        writeIORef (ctxFocusId ctx) (WidgetId 0)
        markEscapeConsumed ctx
      when stateChanged $ markDirty ctx
  -- The dropdown overlay reads its rows from the node's option list: the
  -- visible window of the filtered list.
  uiIO $
    findNodeByWidgetId ctx wid >>= \case
      Nothing -> pure ()
      Just idx -> setOptions (ctxNodeArena ctx) idx (take vis (drop win displayed))
  pure (setChanged commitPulse resp, finalText)


textArea :: Ui :> es => Text -> Text -> Eff es (Response, Text)
textArea lbl initial = do
  wid <- nextId
  ctx <- askContext
  uiIO $ registerFocusable ctx wid
  inp <- askInput
  store <- uiIO (getStore ctx)
  let key = intKey wid
  when (not (IM.member key (storeText store)))
    $ uiIO
    $ setStore ctx (store {storeText = IM.insert key initial (storeText store)})
  let current = IM.findWithDefault initial key (storeText store)
  focus <- uiIO (readIORef (ctxFocusId ctx))
  blocked <- uiIO (pointerBlockedByModal ctx)
  let isFocus = focus == wid && not blocked
  (newText, stateChanged) <-
    if isFocus
      then do
        let oldState = loadTextAreaState store key initial
            TB.Cursor oldRow oldCol = TB.getCursor (TA.buffer oldState)
            TB.Cursor oldAnchorRow oldAnchorCol = TA.selectionAnchor oldState
            (vw, vh) = IM.findWithDefault (200, 96) (slotKey slotTextAreaViewport key) (storePoint store)
            vpW = realToFrac vw
            vpH = realToFrac vh
            lineH = realToFrac (fmLineHeight (ctxFontMetrics ctx))
        newState <- uiIO (processTextArea ctx inp vpW vpH lineH oldState)
        let newText = TB.toText (TA.buffer newState)
            TB.Cursor newRow newCol = TB.getCursor (TA.buffer newState)
            TB.Cursor newAnchorRow newAnchorCol = TA.selectionAnchor newState
            changed =
              newText /= current
                || newRow /= oldRow
                || newCol /= oldCol
                || newAnchorRow /= oldAnchorRow
                || newAnchorCol /= oldAnchorCol
                || TA.scrollOffset newState /= TA.scrollOffset oldState
        when changed $ do
          curStore <- uiIO (getStore ctx)
          uiIO $ setStore ctx (saveTextAreaState key newState curStore)
        pure (newText, changed)
      else pure (current, False)
  resp <- addWidget wid NodeTextArea lbl 0 textAreaLayout
  pure (setChanged stateChanged resp, newText)

select :: Ui :> es => Text -> [Text] -> Int -> Eff es (Response, Int)
select = selectWith id

selectWith ::
  Ui :> es =>
  (Layout -> Layout) ->
  Text ->
  [Text] ->
  Int ->
  Eff es (Response, Int)
selectWith modLayout lbl options initial = do
  wid <- nextId
  ctx <- askContext
  uiIO $ registerFocusable ctx wid
  let
    opts = if null options then [""] else options
    key = intKey wid
  store0 <- uiIO (getStore ctx)
  let
    current = IM.findWithDefault initial key (storeInt store0)
    clamped = max 0 (min (length opts - 1) current)
  when (not (IM.member key (storeInt store0)))
    $ uiIO
    $ setStore ctx (store0 {storeInt = IM.insert key clamped (storeInt store0)})
  resp <- addWidgetWithOptions wid NodeSelect lbl opts 0 (modLayout defaultLayout)
  inp <- askInput
  open <- uiIO $ do
    st <- getStore ctx
    pure (isSelectOpen st key)
  let
    Rect rx ry rw rh = respRect resp
    mouse = inputMousePos inp
    onButton = rw > 0 && rh > 0 && rectContains (Rect rx ry rw rh) mouse
    dropRect = selectDropRect (ctxHostProfile ctx) (ctxFontMetrics ctx) rx ry rw rh (length opts)
    itemH = selectItemH (ctxHostProfile ctx) rh
    onDrop = rw > 0 && rh > 0 && rectContains dropRect mouse
  when (onButton && inputMousePressed inp) $
    uiIO $ do
      st <- getStore ctx
      setStore ctx (setSelectOpen st key (not open))
      writeIORef (ctxFocusId ctx) wid
      markDirty ctx
  when (open && onDrop && inputMouseReleased inp) $
    uiIO $ do
      case selectDropPickIndex dropRect itemH (length opts) (v2Y mouse) of
        Nothing -> pure ()
        Just picked -> do
          st <- getStore ctx
          setStore
            ctx
            ( setSelectOpen
                (st {storeInt = IM.insert key picked (storeInt st)})
                key
                False
            )
          writeIORef (ctxFocusId ctx) wid
          markDirty ctx
  store1 <- uiIO (getStore ctx)
  let
    finalIdx = IM.findWithDefault clamped key (storeInt store1)
  pure (setChanged (finalIdx /= initial) resp, finalIdx)

boundedSelect :: (Bounded a, Enum a, Ui :> es) => Text -> a -> (a -> Text) -> Eff es (Response, a)
boundedSelect lbl initial encode =
  let vs = [minBound .. maxBound]
      opts = map encode vs
   in fmap (\(r, i) -> (r, toEnum (max 0 (min (length vs - 1) i)))) (select lbl opts (fromEnum initial))

enumSelect :: (Bounded a, Enum a, Show a, Ui :> es) => Text -> a -> Eff es (Response, a)
enumSelect lbl initial = boundedSelect lbl initial (T.pack . show)

useEnumSelect :: (Bounded a, Enum a, Show a, Ui :> es) => Text -> a -> Eff es a
useEnumSelect lbl initial = do
  (val, setVal) <- useEnum initial
  (resp, next) <- enumSelect lbl val
  when (respChanged resp) (setVal next)
  pure next

useEnumRadio :: (Bounded a, Enum a, Show a, Ui :> es) => Text -> a -> Eff es a
useEnumRadio legend initial = do
  (val, setVal) <- useEnum initial
  (resp, next) <- enumRadio legend val
  when (respChanged resp) (setVal next)
  pure next
