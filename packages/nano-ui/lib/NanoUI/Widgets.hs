module NanoUI.Widgets
  ( Response (..)
  , Responding (..)
  , Clickable (..)
  , RightClickable (..)
  , onRightClick
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
  , gridPanel
  , gridPanel_
  , gridPanelWith
  , gridPanel'
  , label
  , labelWith
  , labelEx
  , label'
  , button
  , checkbox
  , slider
  , sliderEx
  , textInput
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
  , radioFieldset
  , boundedRadioFieldset
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
  , useToggle
  , heading
  , muted
  , mono
  , styledLabel
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
  , drawingCached
  , DrawOp (..)
  , DrawingBuild
  , animate
  , animateEase
  , animateEaseDelay
  , animateTo
  , animateToEase
  , animateToEaseDelay
  , animateToSpring
  , animateToA
  , animateToSpringA
  , sliderValueText
  , textInputText
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
  )
where

import Control.Monad (void, when)
import Data.IORef (readIORef, writeIORef)
import Data.IntMap.Strict qualified as IM
import Data.Text (Text)
import Data.Text qualified as T
import Effectful (Eff, type (:>))
import NanoUI.Context
  ( Context (..)
  , getLastPointerBlocked
  , getStore
  , intKey
  , isDisabled
  , markDirty
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
import NanoUI.Font
  ( fmLineHeight
  , sliderTrackBounds
  )
import NanoUI.Frame.Hit (scrollHitRect)
import NanoUI.Types (isCellHost)
import NanoUI.Icons (checkboxMark)
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Input (Key (..), inputKeys, inputMouseDown, inputMousePos, inputMousePressed)
import NanoUI.Layout.Arena (NodeType (..))
import NanoUI.Monad (Ui, askContext, askInput, nextId, uiIO, withKey)
import NanoUI.Store
  ( WidgetStore (..)
  , boolInt
  , intBool
  , isSelectOpen
  , setSelectOpen
  , slotAnchor
  , slotCursor
  , slotKey
  , slotTextAreaViewport
  )
import NanoUI.Style
  ( FontVariant (..)
  , Layout (..)
  , alignEnd
  , alignMid
  , defaultLayout
  , fillW
  , fontHeading
  , fontMono
  , fontMuted
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
  , useState
  , useFlag
  , useText
  , useToggle
  )
import NanoUI.Widgets.ColorPicker (colorPicker)
import NanoUI.Widgets.Drawing (DrawOp (..), DrawingBuild, drawing, drawingCached)
import NanoUI.Widgets.Layout
  ( column
  , column_
  , columnWith
  , column'
  , flex
  , grid
  , grid_
  , gridWith
  , grid'
  , gridPanel
  , gridPanel_
  , gridPanelWith
  , gridPanel'
  , label
  , labelWith
  , labelEx
  , label'
  , panel
  , panel_
  , panelWith
  , panel'
  , row
  , row_
  , rowWith
  , row'
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
  , radioFieldset
  , useRadio
  )
import NanoUI.Widgets.TextInput
  ( TextInputState (..)
  , applyTextInputMenuAction
  , processTextInput
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
heading txt = void (labelWith (tight . padXY 0 3 . fontHeading) txt)

muted :: Ui :> es => Text -> Eff es ()
muted txt = void (labelWith (fillW . fontMuted) txt)

mono :: Ui :> es => Text -> Eff es ()
mono txt = void (labelWith fontMono txt)

styledLabel :: Ui :> es => FontVariant -> Layout -> Text -> Eff es Response
styledLabel fvar l txt = labelEx (l {layoutFontVariant = fvar}) txt

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
        Just (Rect x y w h) -> sliderTrackBounds host fm lbl x y w h
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
textInputConfigured cfg lbl initial = do
  wid <- nextId
  ctx <- askContext
  uiIO $ registerFocusable ctx wid
  inp <- askInput
  store <- uiIO (getStore ctx)
  let
    key = intKey wid
  when (not (IM.member key (storeText store)))
    $ uiIO
    $ setStore ctx (store {storeText = IM.insert key initial (storeText store)})
  let
    current = IM.findWithDefault initial key (storeText store)
    cursor = IM.findWithDefault (T.length current) (slotKey slotCursor key) (storeInt store)
    anchor = IM.findWithDefault cursor (slotKey slotAnchor key) (storeInt store)
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
  resp <-
    addWidget wid NodeTextInput lbl 0 (ticLayout cfg)
  pure (setSubmitted submitted (setChanged (newText /= current) resp), newText)

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
      oldState = loadTextAreaState store key initial
      TB.Cursor oldRow oldCol = TB.getCursor (TA.buffer oldState)
      TB.Cursor oldAnchorRow oldAnchorCol = TA.selectionAnchor oldState
  focus <- uiIO (readIORef (ctxFocusId ctx))
  blocked <- uiIO (pointerBlockedByModal ctx)
  let isFocus = focus == wid && not blocked
      (vpW, vpH) =
        let (vw, vh) =
              IM.findWithDefault (200, 96) (slotKey slotTextAreaViewport key) (storePoint store)
         in (realToFrac vw, realToFrac vh)
      lineH = realToFrac (fmLineHeight (ctxFontMetrics ctx))
  newState <-
    if isFocus
      then uiIO (processTextArea ctx inp vpW vpH lineH oldState)
      else pure oldState
  let newText = TB.toText (TA.buffer newState)
      TB.Cursor newRow newCol = TB.getCursor (TA.buffer newState)
      TB.Cursor newAnchorRow newAnchorCol = TA.selectionAnchor newState
      stateChanged =
        newText /= current
          || newRow /= oldRow
          || newCol /= oldCol
          || newAnchorRow /= oldAnchorRow
          || newAnchorCol /= oldAnchorCol
  when stateChanged $
    uiIO $ setStore ctx (saveTextAreaState key newState store)
  resp <- addWidget wid NodeTextArea lbl 0 textAreaLayout
  pure (setChanged (newText /= current) resp, newText)

select :: Ui :> es => Text -> [Text] -> Int -> Eff es (Response, Int)
select lbl options initial = do
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
  resp <- addWidgetWithOptions wid NodeSelect lbl opts 0 defaultLayout
  inp <- askInput
  open <- uiIO $ do
    st <- getStore ctx
    pure (isSelectOpen st key)
  when (respClicked resp) $
    uiIO $ do
      st <- getStore ctx
      let
        Rect rx ry rw rh = respRect resp
        onButton = rw > 0 && rh > 0 && rectContains (Rect rx ry rw rh) (inputMousePos inp)
      when onButton $
        setStore ctx (setSelectOpen st key (not open))
  store1 <- uiIO (getStore ctx)
  let
    finalIdx = IM.findWithDefault clamped key (storeInt store1)
  pure (setChanged (finalIdx /= initial) resp, finalIdx)

textInputText :: Text -> Text -> Int -> Bool -> Text
textInputText = textInputTerminalText
