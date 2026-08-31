module NanoUI.Widgets
  ( Response (..)
  , Responding (..)
  , Clickable (..)
  , panel
  , row
  , column
  , label
  , labelEx
  , button
  , checkbox
  , slider
  , sliderEx
  , textInput
  , applyTextInputMenuAction
  , separator
  , spacer
  , tooltip
  , scroll
  , scrollArea
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
  , useFlag
  , useText
  , useToggle
  , heading
  , muted
  , kv
  , kvBlock
  , card
  , toolbar
  , sep
  , flex
  , image_
  , box
  , animate
  , animateEase
  , animateEaseDelay
  , animateTo
  , animateToEase
  , animateToEaseDelay
  , animateToSpring
  , animateToA
  , animateToSpringA
  , sliderText
  , sliderDisplayText
  , sliderLabelText
  , sliderValueText
  , checkboxLabelText
  , textInputText
  , textInputDisplayText
  , textInputTerminalText
  , selectPackOptions
  , selectLabelText
  , selectParseOptions
  , radioPackOption
  , radioParseOption
  , radioLabelText
  , colorPickerLabelText
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
  , getStore
  , intKey
  , isDisabled
  , pointerBlockedByModal
  , pushTooltip
  , registerFocusable
  , setStore
  )
import NanoUI.Font
  ( headingFontMarker
  , monoFontMarker
  , mutedFontMarker
  , sliderTrackBounds
  )
import NanoUI.Frame.Hit (scrollHitRect)
import NanoUI.Host (isCellHost)
import NanoUI.Icons (checkboxMark)
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Input (inputMouseDown, inputMousePos, inputMousePressed)
import NanoUI.Layout.Arena (NodeType (..))
import NanoUI.Monad (Ui, askContext, askInput, nextId, uiIO)
import NanoUI.Store
  ( WidgetStore (..)
  , boolInt
  , intBool
  , isSelectOpen
  , setSelectOpen
  , slotAnchor
  , slotCursor
  , slotKey
  )
import NanoUI.Style
  ( Layout (..)
  , alignEnd
  , alignMid
  , defaultLayout
  , fillW
  , gap
  , minW
  , padXY
  , tight
  )
import NanoUI.Types
  ( Color (..)
  , ImageId (..)
  , Rect (..)
  , V2 (..)
  , colorToWord32
  , rectContains
  , rectW
  , rectX
  , v2X
  )
import NanoUI.WidgetText
  ( checkboxLabelText
  , colorPickerDisplayText
  , colorPickerFromHex
  , colorPickerLabelText
  , colorPickerToHex
  , radioLabelText
  , radioPackOption
  , radioParseOption
  , selectLabelText
  , selectPackOptions
  , selectParseOptions
  , sliderDisplayText
  , sliderLabelText
  , sliderPackRange
  , sliderPackTerminal
  , sliderText
  , sliderValueText
  , textInputDisplayText
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
  , useFlag
  , useText
  , useToggle
  )
import NanoUI.Widgets.ColorPicker (colorPicker)
import NanoUI.Widgets.Layout
  ( column
  , flex
  , label
  , labelEx
  , panel
  , row
  , scroll
  , scrollArea
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

box :: Ui :> es => Layout -> Color -> Eff es Response
box layout col = do
  wid <- nextId
  addWidgetStyled
    wid
    NodeBox
    T.empty
    0
    layout
    (fromIntegral (colorToWord32 col))
    Nothing

heading :: Ui :> es => Text -> Eff es ()
heading txt = void (labelEx (tight . padXY 0 3 $ defaultLayout) (headingFontMarker <> txt))

muted :: Ui :> es => Text -> Eff es ()
muted txt = void (labelEx (fillW defaultLayout) (mutedFontMarker <> txt))

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
    row rowLayout $ do
      void (labelEx (keyLayout defaultLayout) k)
      void (labelEx (tight . fillW . alignEnd $ defaultLayout) (T.stripEnd v))

kvBlock :: Ui :> es => [(Text, Text)] -> Eff es ()
kvBlock rows =
  void $
    labelEx
      (tight . gap 0 $ defaultLayout)
      (monoFontMarker <> T.unlines [k <> ": " <> v | (k, v) <- rows])

card :: Ui :> es => Eff es a -> Eff es a
card = panel (minW 300 . padXY 12 10 . gap 8 . fillW $ defaultLayout)

toolbar :: Ui :> es => Eff es a -> Eff es a
toolbar = row (tight . gap 8 . alignMid . fillW $ defaultLayout)

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
  resp <- addWidget wid NodeButton ("[ " <> txt <> " ]") 0 defaultLayout
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
    display = if respClicked resp then not current else current
  pure (resp, display)

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
    nodeText =
      if isCellHost host
        then sliderPackTerminal lbl frac current minV maxV
        else sliderPackRange lbl minV maxV
  resp <- addWidget wid NodeSlider nodeText frac layout
  active <- uiIO (readIORef (ctxActiveId ctx))
  blocked <- uiIO (readIORef (ctxLastPointerBlocked ctx))
  trackHover <- uiIO $ do
    if blocked
      then pure False
      else do
        mrect <- scrollHitRect ctx wid
        pure $
          case mrect of
            Nothing -> False
            Just (Rect x y w h) ->
              rectContains (sliderTrackBounds host fm lbl x y w h) (inputMousePos inp)
  let
    isActive = active == wid
    heldByOther =
      inputMouseDown inp
        && not (inputMousePressed inp)
        && hashWidgetId active /= 0
        && not isActive
    pressed =
      inputMouseDown inp
        && not blocked
        && (isActive || (trackHover && not heldByOther))
  when (blocked && isActive)
    $ uiIO
    $ writeIORef (ctxActiveId ctx) (WidgetId 0)
  val <-
    uiIO $ do
      mrect <- scrollHitRect ctx wid
      let
        dragFrac =
          case mrect of
            Nothing -> frac
            Just (Rect x y w h) ->
              let
                track = sliderTrackBounds host fm lbl x y w h
                tx = rectX track
                tw = rectW track
                px = v2X (inputMousePos inp)
                f = (px - tx) / max tw 1
               in
                max 0 (min 1 f)
        computed = minV + dragFrac * (maxV - minV)
      if pressed then pure computed else pure current
  when (pressed && trackHover && not isActive)
    $ uiIO
    $ writeIORef (ctxActiveId ctx) wid
  when (not (inputMouseDown inp) && isActive)
    $ uiIO
    $ writeIORef (ctxActiveId ctx) (WidgetId 0)
  let
    finalVal = if pressed then val else current
  when (finalVal /= current)
    $ uiIO
    $ setStore ctx (store {storeFloat = IM.insert key finalVal (storeFloat store)})
  pure (setChanged (finalVal /= current) resp, finalVal)

textInput :: Ui :> es => Text -> Text -> Eff es (Response, Text)
textInput lbl initial = do
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
  resp <-
    addWidget wid NodeTextInput lbl 0 textInputLayout
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
    nodeText = selectPackOptions lbl opts
  when (not (IM.member key (storeInt store0)))
    $ uiIO
    $ setStore ctx (store0 {storeInt = IM.insert key clamped (storeInt store0)})
  resp <- addWidget wid NodeSelect nodeText 0 defaultLayout
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
      setStore
        ctx
        ( if open
            then
              if onButton
                then setSelectOpen st key False
                else st
            else
              if onButton
                then setSelectOpen st key True
                else st
        )
  store1 <- uiIO (getStore ctx)
  let
    finalIdx = IM.findWithDefault clamped key (storeInt store1)
  pure (setChanged (finalIdx /= initial) resp, finalIdx)

tooltip :: Ui :> es => Text -> Response -> Eff es Response
tooltip tipTxt resp = do
  when (respHovered resp) $ do
    ctx <- askContext
    uiIO $ do
      let
        (Rect rx ry rw rh) = respRect resp
      pushTooltip ctx (respId resp) (Rect (rx + rw + 4) ry 100 (max rh 20)) tipTxt
  pure resp

textInputText :: Text -> Text -> Int -> Bool -> Text
textInputText = textInputTerminalText
