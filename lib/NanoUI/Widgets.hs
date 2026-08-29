module NanoUI.Widgets
  ( Response (..)
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
  ) where

import Control.Monad (void, when)
import Data.IORef (readIORef, writeIORef)
import Effectful (Eff, type (:>))
import Data.Text (Text)
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import GHC.Stack (HasCallStack)
import NanoUI.Font
  ( headingFontMarker
  , monoFontMarker
  , mutedFontMarker
  , sliderTrackBounds
  )
import NanoUI.Host (isCellHost)
import NanoUI.WidgetText
  ( checkboxLabelText
  , sliderDisplayText
  , sliderLabelText
  , sliderPackRange
  , sliderPackTerminal
  , sliderText
  , sliderValueText
  , textInputDisplayText
  , textInputTerminalText
  , selectPackOptions
  , selectParseOptions
  , selectLabelText
  )
import NanoUI.Context
  ( Context (..)
  , getPrevRect
  , getStore
  , intKey
  , isDisabled
  , pointerBlockedByModal
  , registerFocusable
  , pushTooltip
  , setStore
  )
import NanoUI.Icons (checkboxMark)
import NanoUI.Id (WidgetId (..))
import NanoUI.Input (inputMouseDown, inputMousePos)
import NanoUI.Layout.Arena (NodeType (..))
import NanoUI.Monad (Ui, askContext, askInput, currentId, uiIO)
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
import NanoUI.Store (WidgetStore (..))
import NanoUI.Types (Color (..), ImageId (..), Rect (..), V2 (..), colorToWord32, rectContains, rectX, rectW, v2X)
import NanoUI.Widgets.Node
  ( Response (..)
  , addWidget
  , addWidgetResp
  , addWidgetStyled
  )
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
import NanoUI.Widgets.Overlay (modal, window)
import NanoUI.Widgets.TextInput
  ( TextInputState (..)
  , applyTextInputMenuAction
  , processTextInput
  , textInputLayout
  )

{-# INLINE onClick #-}
onClick :: Response -> Eff es () -> Eff es ()
onClick resp act = when (respClicked resp) act

{-# INLINE clickButton #-}
clickButton :: (HasCallStack, Ui :> es) => Text -> Eff es () -> Eff es ()
clickButton txt act = button txt >>= \resp -> onClick resp act

{-# INLINE label_ #-}
label_ :: (HasCallStack, Ui :> es) => Text -> Eff es ()
label_ txt = void (label txt)

{-# INLINE image_ #-}
image_ :: (HasCallStack, Ui :> es) => Layout -> ImageId -> Eff es ()
image_ layout iid = void (image layout iid)

box :: (HasCallStack, Ui :> es) => Layout -> Color -> Eff es Response
box layout col = do
  wid <- currentId
  addWidgetStyled
    wid
    NodeBox
    T.empty
    0
    layout
    (fromIntegral (colorToWord32 col))
    Nothing

heading :: (HasCallStack, Ui :> es) => Text -> Eff es ()
heading txt = void (labelEx (tight . padXY 0 3 $ defaultLayout) (headingFontMarker <> txt))

muted :: (HasCallStack, Ui :> es) => Text -> Eff es ()
muted txt = void (labelEx (fillW defaultLayout) (mutedFontMarker <> txt))

kv :: (HasCallStack, Ui :> es) => Text -> Text -> Eff es ()
kv k v = do
  ctx <- askContext
  let host = ctxHostProfile ctx
      terminal = isCellHost host
      rowLayout =
        tight . gap (if terminal then 1 else 12) . alignMid . fillW $ defaultLayout
      keyLayout =
        if terminal then tight else tight . minW 88
  void $
    row rowLayout $ do
      void (labelEx (keyLayout defaultLayout) k)
      void (labelEx (tight . fillW . alignEnd $ defaultLayout) (T.stripEnd v))

kvBlock :: (HasCallStack, Ui :> es) => [(Text, Text)] -> Eff es ()
kvBlock rows =
  void $
    labelEx
      (tight . gap 0 $ defaultLayout)
      (monoFontMarker <> T.unlines [k <> ": " <> v | (k, v) <- rows])

card :: Ui :> es => Eff es a -> Eff es a
card = panel (minW 300 . padXY 12 10 . gap 8 . fillW $ defaultLayout)

toolbar :: Ui :> es => Eff es a -> Eff es a
toolbar = row (tight . gap 8 . alignMid . fillW $ defaultLayout)

image :: (HasCallStack, Ui :> es) => Layout -> ImageId -> Eff es Response
image layout (ImageId tid) = do
  wid <- currentId
  let stored = if tid <= 0 then T.empty else T.pack (show tid)
  addWidget wid NodeImage stored 0 layout

button :: (HasCallStack, Ui :> es) => Text -> Eff es Response
button txt = buttonEx True txt

buttonEx :: (HasCallStack, Ui :> es) => Bool -> Text -> Eff es Response
buttonEx enabled txt = do
  wid <- currentId
  ctx <- askContext
  uiIO $ registerFocusable ctx wid
  resp <- addWidget wid NodeButton ("[ " <> txt <> " ]") 0 defaultLayout
  disabled <- uiIO (isDisabled ctx wid)
  let active = enabled && not disabled
  pure resp {respClicked = active && respClicked resp, respHovered = active && respHovered resp}

checkbox :: (HasCallStack, Ui :> es) => Text -> Bool -> Eff es (Response, Bool)
checkbox txt initial = do
  wid <- currentId
  ctx <- askContext
  store <- uiIO (getStore ctx)
  let key = intKey wid
      current = IM.findWithDefault initial key (storeCheckbox store)
      host = ctxHostProfile ctx
      nodeText =
        if isCellHost host
          then checkboxMark (ctxIcons ctx) current <> txt
          else txt
  resp <- addWidgetResp wid NodeCheckbox nodeText (if current then 1 else 0) defaultLayout Nothing
  let display = if respClicked resp then not current else current
  pure (resp, display)

slider :: (HasCallStack, Ui :> es) => Text -> Float -> Float -> Float -> Eff es (Response, Float)
slider = sliderEx (fillW defaultLayout)

sliderEx :: (HasCallStack, Ui :> es) => Layout -> Text -> Float -> Float -> Float -> Eff es (Response, Float)
sliderEx layout lbl minV maxV initial = do
  wid <- currentId
  ctx <- askContext
  inp <- askInput
  store <- uiIO (getStore ctx)
  let key = intKey wid
      current = IM.findWithDefault initial key (storeSlider store)
      frac = if maxV > minV then (current - minV) / (maxV - minV) else 0
      host = ctxHostProfile ctx
      fm = ctxFontMetrics ctx
      nodeText =
        if isCellHost host
          then sliderPackTerminal lbl frac current minV maxV
          else sliderPackRange lbl minV maxV
  resp <- addWidget wid NodeSlider nodeText frac layout
  active <- uiIO (readIORef (ctxActiveId ctx))
  trackHover <- uiIO $ do
    mrect <- getPrevRect ctx wid
    pure $
      case mrect of
        Nothing -> False
        Just (Rect x y w h) ->
          rectContains (sliderTrackBounds host fm lbl x y w h) (inputMousePos inp)
  let isActive = active == wid
      pressed = inputMouseDown inp && (trackHover || isActive)
  val <-
    uiIO $ do
      mrect <- getPrevRect ctx wid
      let dragFrac =
            case mrect of
              Nothing -> frac
              Just (Rect x y w h) ->
                let track = sliderTrackBounds host fm lbl x y w h
                    tx = rectX track
                    tw = rectW track
                    px = v2X (inputMousePos inp)
                    f = (px - tx) / max tw 1
                 in max 0 (min 1 f)
          computed = minV + dragFrac * (maxV - minV)
      if pressed then pure computed else pure current
  when (pressed && trackHover && not isActive) $
    uiIO $ writeIORef (ctxActiveId ctx) wid
  when (not (inputMouseDown inp) && isActive) $
    uiIO $ writeIORef (ctxActiveId ctx) (WidgetId 0)
  let finalVal = if pressed then val else current
  when (finalVal /= current) $
    uiIO $ setStore ctx (store {storeSlider = IM.insert key finalVal (storeSlider store)})
  pure (resp {respChanged = finalVal /= current}, finalVal)

textInput :: (HasCallStack, Ui :> es) => Text -> Text -> Eff es (Response, Text)
textInput lbl initial = do
  wid <- currentId
  ctx <- askContext
  uiIO $ registerFocusable ctx wid
  inp <- askInput
  store <- uiIO (getStore ctx)
  let key = intKey wid
  when (not (IM.member key (storeText store))) $
    uiIO $ setStore ctx (store {storeText = IM.insert key initial (storeText store)})
  let current = IM.findWithDefault initial key (storeText store)
      cursor = IM.findWithDefault (T.length current) key (storeCursor store)
      anchor = IM.findWithDefault cursor key (storeSelAnchor store)
  focus <- uiIO (readIORef (ctxFocusId ctx))
  blocked <- uiIO (pointerBlockedByModal ctx)
  let isFocus = focus == wid && not blocked
  newState <-
    if isFocus
      then uiIO (processTextInput ctx inp (TextInputState current cursor anchor))
      else pure (TextInputState current cursor anchor)
  let newText = tisText newState
      newCursor = tisCursor newState
      newAnchor = tisAnchor newState
  when (newText /= current || newCursor /= cursor || newAnchor /= anchor) $
    uiIO $
      setStore
        ctx
        ( store
            { storeText = IM.insert key newText (storeText store)
            , storeCursor = IM.insert key newCursor (storeCursor store)
            , storeSelAnchor = IM.insert key newAnchor (storeSelAnchor store)
            }
        )
  resp <-
    addWidget wid NodeTextInput lbl 0 textInputLayout
  pure (resp {respChanged = newText /= current}, newText)

select :: (HasCallStack, Ui :> es) => Text -> [Text] -> Int -> Eff es (Response, Int)
select lbl options initial = do
  wid <- currentId
  ctx <- askContext
  uiIO $ registerFocusable ctx wid
  let opts = if null options then [""] else options
      key = intKey wid
  store0 <- uiIO (getStore ctx)
  let current = IM.findWithDefault initial key (storeSelect store0)
      clamped = max 0 (min (length opts - 1) current)
      nodeText = selectPackOptions lbl opts
  when (not (IM.member key (storeSelect store0))) $
    uiIO $
      setStore ctx (store0 {storeSelect = IM.insert key clamped (storeSelect store0)})
  resp <- addWidget wid NodeSelect nodeText 0 defaultLayout
  inp <- askInput
  open <- uiIO $ do
    st <- getStore ctx
    pure (IM.findWithDefault False key (storeSelectOpen st))
  when (respClicked resp) $
    uiIO $ do
      st <- getStore ctx
      let Rect rx ry rw rh = respRect resp
          onButton = rw > 0 && rh > 0 && rectContains (Rect rx ry rw rh) (inputMousePos inp)
      setStore
        ctx
        ( if open
            then
              if onButton
                then st {storeSelectOpen = IM.insert key False (storeSelectOpen st)}
                else st
            else
              if onButton
                then st {storeSelectOpen = IM.singleton key True}
                else st
        )
  store1 <- uiIO (getStore ctx)
  let finalIdx = IM.findWithDefault clamped key (storeSelect store1)
  pure (resp {respChanged = finalIdx /= initial}, finalIdx)

tooltip :: Ui :> es => Text -> Response -> Eff es Response
tooltip tipTxt resp = do
  when (respHovered resp) $ do
    ctx <- askContext
    uiIO $ do
      let (Rect rx ry rw rh) = respRect resp
      pushTooltip ctx (respId resp) (Rect (rx + rw + 4) ry 100 (max rh 20)) tipTxt
  pure resp

textInputText :: Text -> Text -> Int -> Bool -> Text
textInputText = textInputTerminalText
