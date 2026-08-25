module NanoUI.Widgets
  ( Response (..)
  , panel
  , row
  , column
  , label
  , button
  , checkbox
  , slider
  , textInput
  , separator
  , spacer
  , tooltip
  , scrollArea
  , select
  , sliderText
  , sliderDisplayText
  , sliderLabelText
  , sliderValueText
  , checkboxLabelText
  , textInputText
  , textInputDisplayText
  , selectPackOptions
  , selectLabelText
  , selectParseOptions
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import GHC.Stack (HasCallStack)
import NanoUI.Font (isTerminalFont)
import NanoUI.WidgetText
  ( checkboxLabelText
  , sliderDisplayText
  , sliderLabelText
  , sliderPackRange
  , sliderPackTerminal
  , sliderText
  , sliderValueText
  , textInputDisplayText
  , selectPackOptions
  , selectParseOptions
  , selectLabelText
  )
import NanoUI.Context
  ( Context (..)
  , WidgetStore (..)
  , getPrevRect
  , getStore
  , intKey
  , isDisabled
  , registerFocusable
  , pushTooltip
  , setStore
  )
import NanoUI.Id (WidgetId (..))
import NanoUI.Input (Input (..), Key (..))
import NanoUI.Layout.Arena
  ( NodeType (..)
  , addNode
  , setNodeText
  , setNodeValue
  , setWidgetId
  )
import NanoUI.Monad (UI (..), askContext, askInput, currentId, emit)
import NanoUI.Style
  ( AlignX (..)
  , AlignY (..)
  , Direction (..)
  , Layout (..)
  , Padding (..)
  , Sizing (..)
  , defaultLayout
  )
import NanoUI.Types (Rect (..), V2 (..), rectContains)

parentIdx :: [Int] -> Int
parentIdx = \case
  [] -> -1
  (p : _) -> p

data Response = Response
  { respId :: WidgetId
  , respRect :: Rect
  , respHovered :: Bool
  , respPressed :: Bool
  , respClicked :: Bool
  , respChanged :: Bool
  }
  deriving (Eq, Show)

{-# INLINE panel #-}
panel :: Layout -> UI a -> UI a
panel = container

{-# INLINE row #-}
row :: Layout -> UI a -> UI a
row layout child = container (layout {layoutDirection = Row}) child

{-# INLINE column #-}
column :: Layout -> UI a -> UI a
column layout child = container (layout {layoutDirection = Column}) child

container :: Layout -> UI a -> UI a
container layout child = UI $ \ctx inp -> do
  stack <- readIORef (ctxContainerStack ctx)
  let parent = parentIdx stack
  idx <-
    addNode
      (ctxNodeArena ctx)
      NodeContainer
      parent
      (layoutDirection layout)
      (layoutWidth layout)
      (layoutHeight layout)
      (layoutPadding layout)
      (layoutGap layout)
      (layoutMinW layout)
      (layoutMinH layout)
      (layoutMaxW layout)
      (layoutMaxH layout)
      0
      (layoutAlignX layout)
      (layoutAlignY layout)
  writeIORef (ctxContainerStack ctx) (idx : stack)
  r <- unUI child ctx inp
  writeIORef (ctxContainerStack ctx) stack
  pure r

{-# INLINE label #-}
label :: HasCallStack => Text -> UI Response
label txt = do
  wid <- currentId
  addWidget wid NodeText txt 0 defaultLayout

{-# INLINE button #-}
button :: HasCallStack => Text -> UI Response
button txt = buttonEx True txt

buttonEx :: HasCallStack => Bool -> Text -> UI Response
buttonEx enabled txt = do
  wid <- currentId
  ctx <- askContext
  liftIO $ registerFocusable ctx wid
  resp <- addWidget wid NodeButton ("[ " <> txt <> " ]") 0 defaultLayout
  disabled <- liftIO (isDisabled ctx wid)
  let active = enabled && not disabled
  when (active && respClicked resp) $ emit ("button:" <> T.unpack txt)
  pure resp {respClicked = active && respClicked resp, respHovered = active && respHovered resp}

{-# INLINE checkbox #-}
checkbox :: HasCallStack => Text -> Bool -> UI (Response, Bool)
checkbox txt initial = do
  wid <- currentId
  ctx <- askContext
  store <- liftIO (getStore ctx)
  let key = intKey wid
      current = IM.findWithDefault initial key (storeCheckbox store)
      fm = ctxFontMetrics ctx
      nodeText =
        if isTerminalFont fm
          then (if current then "[x] " else "[ ] ") <> txt
          else txt
  resp <- addWidgetResp wid NodeCheckbox nodeText (if current then 1 else 0) defaultLayout Nothing
  let display = if respClicked resp then not current else current
  pure (resp, display)

{-# INLINE slider #-}
slider :: HasCallStack => Layout -> Text -> Float -> Float -> Float -> UI (Response, Float)
slider layout lbl minV maxV initial = do
  wid <- currentId
  ctx <- askContext
  inp <- askInput
  store <- liftIO (getStore ctx)
  let key = intKey wid
      current = IM.findWithDefault initial key (storeSlider store)
      frac = if maxV > minV then (current - minV) / (maxV - minV) else 0
      fm = ctxFontMetrics ctx
      nodeText =
        if isTerminalFont fm
          then sliderPackTerminal lbl frac current minV maxV
          else sliderPackRange lbl minV maxV
  resp <- addWidget wid NodeSlider nodeText frac layout
  active <- liftIO (readIORef (ctxActiveId ctx))
  let isActive = active == wid
      hovered = respHovered resp
      pressed = inputMouseDown inp && (hovered || isActive)
  val <-
    liftIO $ do
      mrect <- getPrevRect ctx wid
      let dragFrac =
            case mrect of
              Nothing -> frac
              Just (Rect x _ w _) ->
                let px = v2X (inputMousePos inp)
                    f = (px - x) / max w 1
                 in max 0 (min 1 f)
          computed = minV + dragFrac * (maxV - minV)
      if pressed then pure computed else pure current
  when (pressed && hovered && not isActive) $
    liftIO $ writeIORef (ctxActiveId ctx) wid
  when (not (inputMouseDown inp) && isActive) $
    liftIO $ writeIORef (ctxActiveId ctx) (WidgetId 0)
  let finalVal = if pressed then val else current
  when (finalVal /= current) $
    liftIO $ setStore ctx (store {storeSlider = IM.insert key finalVal (storeSlider store)})
  pure (resp {respChanged = finalVal /= current}, finalVal)

{-# INLINE textInput #-}
textInput :: HasCallStack => Text -> String -> UI (Response, String)
textInput lbl initial = do
  wid <- currentId
  ctx <- askContext
  liftIO $ registerFocusable ctx wid
  inp <- askInput
  store <- liftIO (getStore ctx)
  let key = intKey wid
      current = IM.findWithDefault initial key (storeText store)
      cursor = IM.findWithDefault (length current) key (storeCursor store)
  focus <- liftIO (readIORef (ctxFocusId ctx))
  let isFocus = focus == wid
  (newText, newCursor) <-
    if isFocus
      then liftIO (processTextInput inp current cursor)
      else pure (current, cursor)
  when (newText /= current || newCursor /= cursor) $
    liftIO $
      setStore
        ctx
        ( store
            { storeText = IM.insert key newText (storeText store)
            , storeCursor = IM.insert key newCursor (storeCursor store)
            }
        )
  resp <-
    addWidget wid NodeTextInput (textInputText lbl newText newCursor isFocus) 0 defaultLayout
  pure (resp {respChanged = newText /= current}, newText)

{-# INLINE separator #-}
separator :: HasCallStack => UI Response
separator = do
  wid <- currentId
  ctx <- askContext
  inp <- askInput
  liftIO $ do
    stack <- readIORef (ctxContainerStack ctx)
    let parent = parentIdx stack
    idx <-
      addNode
        (ctxNodeArena ctx)
        NodeSeparator
        parent
        Row
        (Fixed 1)
        Fit
        (Padding 0 0 0 0)
        0
        0
        0
        1e9
        1e9
        0
        AlignStart
        AlignTop
    setWidgetId (ctxNodeArena ctx) idx wid
    resolveInteraction ctx inp wid

{-# INLINE spacer #-}
spacer :: HasCallStack => Sizing -> Sizing -> UI Response
spacer w h = do
  wid <- currentId
  ctx <- askContext
  inp <- askInput
  liftIO $ do
    stack <- readIORef (ctxContainerStack ctx)
    let parent = parentIdx stack
    idx <-
      addNode
        (ctxNodeArena ctx)
        NodeSpacer
        parent
        Row
        w
        h
        (Padding 0 0 0 0)
        0
        0
        0
        1e9
        1e9
        0
        AlignStart
        AlignTop
    setWidgetId (ctxNodeArena ctx) idx wid
    resolveInteraction ctx inp wid

{-# INLINE scrollArea #-}
scrollArea :: HasCallStack => Layout -> UI a -> UI (WidgetId, a)
scrollArea layout child = do
  wid <- currentId
  r <-
    UI $ \ctx inp -> do
      stack <- readIORef (ctxContainerStack ctx)
      let parent = parentIdx stack
      idx <-
        addNode
          (ctxNodeArena ctx)
          NodeScrollContainer
          parent
          (layoutDirection layout)
          (layoutWidth layout)
          (layoutHeight layout)
          (layoutPadding layout)
          (layoutGap layout)
          (layoutMinW layout)
          (layoutMinH layout)
          (layoutMaxW layout)
          (layoutMaxH layout)
          0
          (layoutAlignX layout)
          (layoutAlignY layout)
      setWidgetId (ctxNodeArena ctx) idx wid
      writeIORef (ctxContainerStack ctx) (idx : stack)
      childR <- unUI child ctx inp
      writeIORef (ctxContainerStack ctx) stack
      pure childR
  pure (wid, r)

{-# INLINE select #-}
select :: HasCallStack => Text -> [Text] -> Int -> UI (Response, Int)
select lbl options initial = do
  wid <- currentId
  ctx <- askContext
  liftIO $ registerFocusable ctx wid
  let opts = if null options then [""] else options
      key = intKey wid
  store0 <- liftIO (getStore ctx)
  let current = IM.findWithDefault initial key (storeSelect store0)
      clamped = max 0 (min (length opts - 1) current)
      nodeText = selectPackOptions lbl opts
  when (not (IM.member key (storeSelect store0))) $
    liftIO $
      setStore ctx (store0 {storeSelect = IM.insert key clamped (storeSelect store0)})
  resp <- addWidget wid NodeSelect nodeText 0 defaultLayout
  open <- liftIO $ do
    st <- getStore ctx
    pure (IM.findWithDefault False key (storeSelectOpen st))
  when (respClicked resp) $
    liftIO $ do
      st <- getStore ctx
      setStore
        ctx
        ( if open
            then st {storeSelectOpen = IM.insert key False (storeSelectOpen st)}
            else st {storeSelectOpen = IM.singleton key True}
        )
  store1 <- liftIO (getStore ctx)
  let finalIdx = IM.findWithDefault clamped key (storeSelect store1)
  pure (resp {respChanged = finalIdx /= initial}, finalIdx)

{-# INLINE tooltip #-}
tooltip :: Text -> Response -> UI Response
tooltip tipTxt resp = do
  when (respHovered resp) $ do
    ctx <- askContext
    liftIO $ do
      let (Rect rx ry rw rh) = respRect resp
      pushTooltip ctx (Rect (rx + rw + 4) ry 100 (max rh 20)) tipTxt
  pure resp

textInputText :: Text -> String -> Int -> Bool -> Text
textInputText lbl value cursor focused =
  let body = T.pack value
      shown =
        if focused
          then
            let c = max 0 (min (T.length body) cursor)
             in T.take c body <> "\x2502" <> T.drop c body
          else body
   in lbl <> ": " <> shown

addWidget ::
  WidgetId ->
  NodeType ->
  Text ->
  Float ->
  Layout ->
  UI Response
addWidget wid nt txt value layout = addWidgetResp wid nt txt value layout Nothing

addWidgetResp ::
  WidgetId ->
  NodeType ->
  Text ->
  Float ->
  Layout ->
  Maybe Response ->
  UI Response
addWidgetResp wid nt txt value layout mResp = do
  ctx <- askContext
  inp <- askInput
  liftIO $ do
    stack <- readIORef (ctxContainerStack ctx)
    let parent = parentIdx stack
    idx <-
      addNode
        (ctxNodeArena ctx)
        nt
        parent
        (layoutDirection layout)
        (layoutWidth layout)
        (layoutHeight layout)
        (layoutPadding layout)
        (layoutGap layout)
        (layoutMinW layout)
        (layoutMinH layout)
        (layoutMaxW layout)
        (layoutMaxH layout)
        0
        (layoutAlignX layout)
        (layoutAlignY layout)
    setNodeText (ctxNodeArena ctx) idx txt
    setNodeValue (ctxNodeArena ctx) idx value
    setWidgetId (ctxNodeArena ctx) idx wid
    case mResp of
      Just resp -> pure resp
      Nothing -> resolveInteraction ctx inp wid

resolveInteraction :: Context -> Input -> WidgetId -> IO Response
resolveInteraction ctx inp wid = do
  disabled <- isDisabled ctx wid
  if disabled
    then do
      mrect <- getPrevRect ctx wid
      let rect = maybe (Rect 0 0 0 0) id mrect
      pure
        Response
          { respId = wid
          , respRect = rect
          , respHovered = False
          , respPressed = False
          , respClicked = False
          , respChanged = False
          }
    else do
      mrect <- getPrevRect ctx wid
      let rect = maybe (Rect 0 0 0 0) id mrect
          mouse = inputMousePos inp
          hovered =
            case rect of
              Rect _ _ rw rh -> rw > 0 && rh > 0 && rectContains rect mouse
      active <- readIORef (ctxActiveId ctx)
      let activating = inputMousePressed inp && hovered
          isActive = active == wid || activating
      when hovered $ writeIORef (ctxHotId ctx) wid
      when activating $ writeIORef (ctxActiveId ctx) wid
      let pressed = inputMouseDown inp && (hovered || active == wid)
          clicked = inputMouseReleased inp && isActive
      pure
        Response
          { respId = wid
          , respRect = rect
          , respHovered = hovered
          , respPressed = pressed
          , respClicked = clicked
          , respChanged = False
          }

processTextInput :: Input -> String -> Int -> IO (String, Int)
processTextInput inp txt cur =
  let chars = inputChars inp
      keys = inputKeys inp
      (t1, c1) = foldl insertChar (txt, cur) chars
      (t2, c2) = foldl applyKey (t1, c1) keys
   in pure (t2, c2)

insertChar :: (String, Int) -> Char -> (String, Int)
insertChar (t, c) ch = (take c t ++ [ch] ++ drop c t, c + 1)

applyKey :: (String, Int) -> Key -> (String, Int)
applyKey (t, c) key =
  case key of
    KeyBackspace ->
      if c > 0 then (take (c - 1) t ++ drop c t, c - 1) else (t, c)
    KeyDelete ->
      if c < length t then (take c t ++ drop (c + 1) t, c) else (t, c)
    KeyLeft -> (t, max 0 (c - 1))
    KeyRight -> (t, min (length t) (c + 1))
    KeyHome -> (t, 0)
    KeyEnd -> (t, length t)
    _ -> (t, c)
