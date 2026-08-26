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
  , textInput
  , applyTextInputMenuAction
  , separator
  , spacer
  , tooltip
  , scrollArea
  , select
  , modal
  , image
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

import Control.Monad (foldM, void, when)
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
  , textInputTerminalText
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
  , markDirty
  , beginModal
  , endModal
  , markEscapeConsumed
  , pointerBlockedByModal
  )
import NanoUI.Id (WidgetId (..))
import NanoUI.Input (Input (..), Key (..), Modifiers (..), inputChars, inputKeys, inputModifiers)
import NanoUI.Layout.Arena
  ( NodeType (..)
  , addNode
  , setNodeText
  , setNodeValue
  , setWidgetId
  )
import NanoUI.Monad (UI (..), askContext, askInput, currentId, emit, withKey)
import NanoUI.Style
  ( AlignX (..)
  , AlignY (..)
  , Direction (..)
  , Layout (..)
  , Padding (..)
  , Sizing (..)
  , defaultLayout
  )
import NanoUI.Types (ImageId (..), Rect (..), Size (..), V2 (..), rectContains, rectH, rectW, sliderTrackRect)

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

modal :: HasCallStack => Bool -> Text -> UI a -> UI (Response, Maybe a)
modal open title child
  | not open = do
      wid <- currentId
      pure (emptyModalResp wid, Nothing)
  | otherwise = do
      wid <- currentId
      ctx <- askContext
      inp <- askInput
      body <-
        UI $ \c i -> do
          stack <- readIORef (ctxContainerStack c)
          let parent = parentIdx stack
              Size winW winH = inputWindowSize i
              margin = 16
              maxW = max 220 (winW - 2 * margin)
              maxH = max 40 (winH - 2 * margin)
          idx <-
            addNode
              (ctxNodeArena c)
              NodeModal
              parent
              Column
              Fit
              Fit
              (Padding 16 16 16 16)
              8
              220
              0
              maxW
              maxH
              0
              AlignStart
              AlignTop
              False
          setWidgetId (ctxNodeArena c) idx wid
          writeIORef (ctxContainerStack c) (idx : stack)
          beginModal c
          r <-
            unUI
              ( do
                  when (not (T.null title)) $ do
                    _ <- withKey title (label title)
                    pure ()
                  child
              )
              c
              i
          endModal c
          writeIORef (ctxContainerStack c) stack
          pure r
      mrect <- liftIO (getPrevRect ctx wid)
      let mouse = inputMousePos inp
          inPanel = maybe False (\r -> rectW r > 0 && rectH r > 0 && rectContains r mouse) mrect
          backdrop =
            case mrect of
              Just r | rectW r > 0 && rectH r > 0 ->
                inputMousePressed inp && not (rectContains r mouse)
              _ -> False
          esc = KeyEscape `elem` inputKeys inp
          dismiss = backdrop || esc
      when esc $ liftIO (markEscapeConsumed ctx)
      pure
        ( Response
            { respId = wid
            , respRect = maybe (Rect 0 0 0 0) id mrect
            , respHovered = inPanel
            , respPressed = False
            , respClicked = dismiss
            , respChanged = dismiss
            }
        , Just body
        )

image :: HasCallStack => Layout -> ImageId -> UI Response
image layout (ImageId tid) = do
  wid <- currentId
  let stored = if tid <= 0 then T.empty else T.pack (show tid)
  addWidget wid NodeImage stored 0 layout

emptyModalResp :: WidgetId -> Response
emptyModalResp wid =
  Response
    { respId = wid
    , respRect = Rect 0 0 0 0
    , respHovered = False
    , respPressed = False
    , respClicked = False
    , respChanged = False
    }

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
      (layoutWrap layout)
  writeIORef (ctxContainerStack ctx) (idx : stack)
  r <- unUI child ctx inp
  writeIORef (ctxContainerStack ctx) stack
  pure r

{-# INLINE label #-}
label :: HasCallStack => Text -> UI Response
label = labelEx defaultLayout

{-# INLINE labelEx #-}
labelEx :: HasCallStack => Layout -> Text -> UI Response
labelEx layout txt = do
  wid <- currentId
  addWidget wid NodeText txt 0 layout

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
  trackHover <- liftIO $ do
    mrect <- getPrevRect ctx wid
    pure $
      case mrect of
        Nothing -> False
        Just (Rect x y w h) -> rectContains (sliderTrackRect x y w h) (inputMousePos inp)
  let isActive = active == wid
      pressed = inputMouseDown inp && (trackHover || isActive)
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
  when (pressed && trackHover && not isActive) $
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
  when (not (IM.member key (storeText store))) $
    liftIO $ setStore ctx (store {storeText = IM.insert key initial (storeText store)})
  let current = IM.findWithDefault initial key (storeText store)
      cursor = IM.findWithDefault (length current) key (storeCursor store)
      anchor = IM.findWithDefault cursor key (storeSelAnchor store)
  focus <- liftIO (readIORef (ctxFocusId ctx))
  blocked <- liftIO (pointerBlockedByModal ctx)
  let isFocus = focus == wid && not blocked
  newState <-
    if isFocus
      then liftIO (processTextInput ctx inp (TextInputState current cursor anchor))
      else pure (TextInputState current cursor anchor)
  let newText = tisText newState
      newCursor = tisCursor newState
      newAnchor = tisAnchor newState
  when (newText /= current || newCursor /= cursor || newAnchor /= anchor) $
    liftIO $
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

textInputLayout :: Layout
textInputLayout =
  defaultLayout
    { layoutWidth = Grow 1
    , layoutMinW = 160
    }

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
        False
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
        False
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
          (layoutWrap layout)
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
  inp <- askInput
  open <- liftIO $ do
    st <- getStore ctx
    pure (IM.findWithDefault False key (storeSelectOpen st))
  when (respClicked resp) $
    liftIO $ do
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
      pushTooltip ctx (respId resp) (Rect (rx + rw + 4) ry 100 (max rh 20)) tipTxt
  pure resp

textInputText :: Text -> String -> Int -> Bool -> Text
textInputText = textInputTerminalText

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
        (layoutWrap layout)
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
      blocked <- pointerBlockedByModal ctx
      let rect = maybe (Rect 0 0 0 0) id mrect
          mouse = inputMousePos inp
          hovered =
            case rect of
              Rect _ _ rw rh ->
                not blocked && rw > 0 && rh > 0 && rectContains rect mouse
      active <- readIORef (ctxActiveId ctx)
      let activating = inputMousePressed inp && hovered
          isActive = active == wid || activating
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

data TextInputState = TextInputState
  { tisText :: String
  , tisCursor :: Int
  , tisAnchor :: Int
  }
  deriving (Eq, Show)

textInputSelRange :: TextInputState -> Maybe (Int, Int)
textInputSelRange s
  | tisAnchor s == tisCursor s = Nothing
  | otherwise = Just (min (tisAnchor s) (tisCursor s), max (tisAnchor s) (tisCursor s))

selectionText :: TextInputState -> Maybe String
selectionText s =
  case textInputSelRange s of
    Nothing -> Nothing
    Just (lo, hi) -> Just (take (hi - lo) (drop lo (tisText s)))

selectAllTextInput :: TextInputState -> TextInputState
selectAllTextInput s =
  s {tisAnchor = 0, tisCursor = length (tisText s)}

textInputCopy :: Context -> TextInputState -> IO ()
textInputCopy ctx s = do
  let txt =
        case selectionText s of
          Just slice -> slice
          Nothing -> tisText s
  when (not (null txt)) $
    void (ctxClipboardSet ctx txt)

textInputCut :: Context -> TextInputState -> IO TextInputState
textInputCut ctx s = do
  case selectionText s of
    Nothing -> pure s
    Just slice -> do
      void (ctxClipboardSet ctx slice)
      pure (deleteBackward s)

textInputPaste :: Context -> TextInputState -> IO TextInputState
textInputPaste ctx s = do
  mtxt <- ctxClipboardGet ctx
  case mtxt of
    Nothing -> pure s
    Just paste ->
      let p = paste
          pos = case textInputSelRange s of
            Nothing -> tisCursor s
            Just (lo, _) -> lo
          t = tisText s
          t' =
            case textInputSelRange s of
              Nothing -> take pos t ++ p ++ drop pos t
              Just (lo, hi) -> take lo t ++ p ++ drop hi t
          end = pos + length p
       in pure s {tisText = t', tisCursor = end, tisAnchor = end}

applyTextInputMenuAction :: Context -> WidgetId -> Int -> IO ()
applyTextInputMenuAction ctx wid item = do
  store <- getStore ctx
  let key = intKey wid
      text = IM.findWithDefault "" key (storeText store)
      cursor = IM.findWithDefault (length text) key (storeCursor store)
      anchor = IM.findWithDefault cursor key (storeSelAnchor store)
      s0 = TextInputState text cursor anchor
  s1 <-
    case item of
      0 -> textInputCut ctx s0
      1 -> textInputCopy ctx s0 >> pure s0
      2 -> textInputPaste ctx s0
      3 -> pure (selectAllTextInput s0)
      _ -> pure s0
  setStore
    ctx
    ( store
        { storeText = IM.insert key (tisText s1) (storeText store)
        , storeCursor = IM.insert key (tisCursor s1) (storeCursor store)
        , storeSelAnchor = IM.insert key (tisAnchor s1) (storeSelAnchor store)
        }
    )
  writeIORef (ctxTextInputMenu ctx) Nothing
  markDirty ctx

processTextInput :: Context -> Input -> TextInputState -> IO TextInputState
processTextInput ctx inp s0 = do
  let mods = inputModifiers inp
      ctrl = modCtrl mods
      shift = modShift mods
      keys = inputKeys inp
      chars = inputChars inp
  s1 <-
    if ctrl
      then foldM (handleCtrlChar ctx) s0 chars
      else pure s0
  let filtered = filter (not . isCtrlCombo ctrl) chars
      s2 = foldl insertChar s1 filtered
  pure (foldl (applyKey shift) s2 keys)
  where
    isCtrlCombo c ch = c && ch `elem` ("aAcCxXvV\x01" :: String)

handleCtrlChar :: Context -> TextInputState -> Char -> IO TextInputState
handleCtrlChar ctx s ch
  | ch `elem` ('a' : 'A' : '\x01' : []) = pure (selectAllTextInput s)
  | ch `elem` ('c' : 'C' : '\ETX' : []) = textInputCopy ctx s >> pure s
  | ch `elem` ('x' : 'X' : []) = textInputCut ctx s
  | ch `elem` ('v' : 'V' : []) = textInputPaste ctx s
  | otherwise = pure s

insertChar :: TextInputState -> Char -> TextInputState
insertChar s ch =
  case textInputSelRange s of
    Nothing ->
      let t = tisText s
          c = tisCursor s
          pos = c + 1
       in s {tisText = take c t ++ [ch] ++ drop c t, tisCursor = pos, tisAnchor = pos}
    Just (lo, hi) ->
      let t = tisText s
          pos = lo + 1
       in s {tisText = take lo t ++ [ch] ++ drop hi t, tisCursor = pos, tisAnchor = pos}

applyKey :: Bool -> TextInputState -> Key -> TextInputState
applyKey shift s key =
  case key of
    KeyBackspace -> deleteBackward s
    KeyDelete -> deleteForward s
    KeyLeft -> moveCursor s (max 0 (tisCursor s - 1)) shift
    KeyRight -> moveCursor s (min (length (tisText s)) (tisCursor s + 1)) shift
    KeyHome -> moveCursor s 0 shift
    KeyEnd -> moveCursor s (length (tisText s)) shift
    _ -> s

deleteBackward :: TextInputState -> TextInputState
deleteBackward s =
  case textInputSelRange s of
    Just (lo, hi) -> s {tisText = take lo (tisText s) ++ drop hi (tisText s), tisCursor = lo, tisAnchor = lo}
    Nothing ->
      let c = tisCursor s
       in if c > 0
            then
              let t = tisText s
                  pos = c - 1
               in s {tisText = take pos t ++ drop c t, tisCursor = pos, tisAnchor = pos}
            else s

deleteForward :: TextInputState -> TextInputState
deleteForward s =
  case textInputSelRange s of
    Just (lo, hi) -> s {tisText = take lo (tisText s) ++ drop hi (tisText s), tisCursor = lo, tisAnchor = lo}
    Nothing ->
      let c = tisCursor s
          t = tisText s
       in if c < length t
            then s {tisText = take c t ++ drop (c + 1) t}
            else s

moveCursor :: TextInputState -> Int -> Bool -> TextInputState
moveCursor s pos shift =
  if shift
    then s {tisCursor = pos}
    else s {tisCursor = pos, tisAnchor = pos}
