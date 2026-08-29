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

import Control.Monad (foldM, void, when)
import Data.IORef (readIORef, writeIORef)
import Effectful (Eff, type (:>))
import Data.Text (Text)
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import GHC.Stack (HasCallStack)
import NanoUI.Font
  ( headingFontMarker
  , monoFontMarker
  , layoutUnitScale
  , mutedFontMarker
  , resolveLayoutGap
  , resolveLayoutPadding
  , sliderTrackBounds
  )
import NanoUI.Host (HostProfile, isCellHost)
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
import NanoUI.Animatable (Animatable (..))
import NanoUI.Context
  ( Context (..)
  , Ease (..)
  , WidgetStore (..)
  , approxEq
  , easeSameSpec
  , getAnimationValue
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
  , startAnimationEaseDelay
  , startSpring
  )
import NanoUI.Spring (SpringParams)
import NanoUI.Icons (Icons (..), checkboxMark)
import NanoUI.Id (WidgetId (..))
import NanoUI.Input (Input (..), Key (..), Modifiers (..), inputChars, inputKeys, inputModifiers)
import NanoUI.Layout.Arena
  ( DirTag (..)
  , NodeType (..)
  , addNode
  , getDirection
  , setAspect
  , setNodeText
  , setNodeValue
  , setStyleIdx
  , setWidgetId
  )
import NanoUI.Monad (Ui, askContext, askInput, currentId, uiFinally, uiIO, withKey)
import NanoUI.Style
  ( AlignX (..)
  , AlignY (..)
  , Direction (..)
  , Layout (..)
  , Padding (..)
  , Sizing (..)
  , alignEnd
  , alignMid
  , defaultLayout
  , fillW
  , gap
  , minW
  , padXY
  , tight
  , fixedH
  , fixedW
  , fixedWH
  , grow
  , windowPad
  , windowMargin
  )
import NanoUI.Types (Color (..), ImageId (..), Rect (..), Size (..), V2 (..), colorToWord32, rectContains, rectH, rectW, rectX, v2X)

parentIdx :: [Int] -> Int
parentIdx = \case
  [] -> -1
  (p : _) -> p

titleBarH :: Float
titleBarH = 28

titleBarHFor :: HostProfile -> Float
titleBarHFor host
  | isCellHost host = 1
  | otherwise = titleBarH

titleBarLayoutFor :: HostProfile -> Layout
titleBarLayoutFor host =
  tight . gap (if isCellHost host then 1 else 6) . alignMid . fixedH (titleBarHFor host) . fillW $ defaultLayout

titleLabelLayoutFor :: HostProfile -> Layout
titleLabelLayoutFor host =
  tight . alignMid . fixedH (titleBarHFor host) $ defaultLayout

-- Pixel-authored chrome. Cell hosts map one cell per defaultLayout gap step.
floatPadFor :: HostProfile -> Padding -> Padding
floatPadFor host pad
  | isCellHost host = Padding 4 4 4 4
  | otherwise = pad

floatGapFor :: HostProfile -> Float -> Float
floatGapFor host g
  | isCellHost host = 4
  | otherwise = g

floatMinFor :: HostProfile -> Float -> Float -> Float
floatMinFor host authored avail =
  let raw =
        if isCellHost host
          then authored * layoutUnitScale host
          else authored
   in max 1 (min raw avail)

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
panel :: Ui :> es => Layout -> Eff es a -> Eff es a
panel = container NodePanel

{-# INLINE row #-}
row :: Ui :> es => Layout -> Eff es a -> Eff es a
row layout child = container NodeContainer (layout {layoutDirection = Row}) child

{-# INLINE column #-}
column :: Ui :> es => Layout -> Eff es a -> Eff es a
column layout child = container NodeContainer (layout {layoutDirection = Column}) child

{-# INLINE onClick #-}
onClick :: Response -> Eff es () -> Eff es ()
onClick resp act = when (respClicked resp) act

{-# INLINE clickButton #-}
clickButton :: (HasCallStack, Ui :> es) => Text -> Eff es () -> Eff es ()
clickButton txt act = button txt >>= \resp -> onClick resp act

{-# INLINE label_ #-}
label_ :: (HasCallStack, Ui :> es) => Text -> Eff es ()
label_ txt = void (label txt)

{-# INLINE sep #-}
sep :: (HasCallStack, Ui :> es) => Eff es ()
sep = void separator

{-# INLINE flex #-}
flex :: (HasCallStack, Ui :> es) => Eff es ()
flex = void (spacer (Grow 1) Fit)

{-# INLINE image_ #-}
image_ :: (HasCallStack, Ui :> es) => Layout -> ImageId -> Eff es ()
image_ layout iid = void (image layout iid)

-- Solid colored rect. Not a hover target. RGBA is stored as Word32 bits in
-- styleIdx; GHC `fromIntegral` roundtrips Int on all supported targets.
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

-- Call every frame. After each settle, the next call restarts from `from`.
animate :: (HasCallStack, Ui :> es) => Float -> Float -> Float -> Eff es Float
animate = animateEase EaseLinear

animateEase :: (HasCallStack, Ui :> es) => Ease -> Float -> Float -> Float -> Eff es Float
animateEase ease from to dur = animateEaseDelay ease from to dur 0

animateEaseDelay :: (HasCallStack, Ui :> es) => Ease -> Float -> Float -> Float -> Float -> Eff es Float
animateEaseDelay ease from to dur delay = do
  wid <- currentId
  ctx <- askContext
  uiIO $ do
    startAnimationEaseDelay ctx wid from to dur ease delay
    getAnimationValue ctx wid

-- Tween toward `to` and hold. Reverses from the current value if the target changes.
animateTo :: (HasCallStack, Ui :> es) => Float -> Float -> Eff es Float
animateTo = animateToEase EaseLinear

animateToEase :: (HasCallStack, Ui :> es) => Ease -> Float -> Float -> Eff es Float
animateToEase ease target dur = animateToEaseDelay ease target dur 0

animateToEaseDelay :: (HasCallStack, Ui :> es) => Ease -> Float -> Float -> Float -> Eff es Float
animateToEaseDelay ease target dur delay = do
  wid <- currentId
  ctx <- askContext
  uiIO $ do
    cur <- getAnimationValue ctx wid
    anims <- readIORef (ctxAnimations ctx)
    let manim = IM.lookup (intKey wid) anims
    case manim of
      Just a
        | easeSameSpec a ease dur delay target -> pure cur
        | otherwise -> do
            startAnimationEaseDelay ctx wid cur target dur ease delay
            getAnimationValue ctx wid
      Nothing
        | approxEq cur target -> pure cur
        | otherwise -> do
            startAnimationEaseDelay ctx wid cur target dur ease delay
            getAnimationValue ctx wid

-- Spring toward `to` and hold. Retarget keeps velocity.
animateToSpring :: (HasCallStack, Ui :> es) => SpringParams -> Float -> Eff es Float
animateToSpring params target = do
  wid <- currentId
  ctx <- askContext
  uiIO $ do
    startSpring ctx wid params target
    getAnimationValue ctx wid

-- Ease each component of an Animatable under a derived key.
animateToA :: (HasCallStack, Animatable a, Ui :> es) => Ease -> Float -> a -> Eff es a
animateToA ease dur target = do
  comps <-
    mapM
      (\(i, c) -> withKey (i :: Int) (animateToEase ease c dur))
      (zip [0 ..] (toComponents target))
  pure (fromComponents comps)

-- Spring each component of an Animatable under a derived key.
animateToSpringA :: (HasCallStack, Animatable a, Ui :> es) => SpringParams -> a -> Eff es a
animateToSpringA params target = do
  comps <-
    mapM
      (\(i, c) -> withKey (i :: Int) (animateToSpring params c))
      (zip [0 ..] (toComponents target))
  pure (fromComponents comps)

useFlag :: (HasCallStack, Ui :> es) => Bool -> Eff es (Bool, Bool -> Eff es ())
useFlag initial = do
  wid <- currentId
  ctx <- askContext
  store <- uiIO (getStore ctx)
  let key = intKey wid
      cur = IM.findWithDefault initial key (storeFlag store)
      set v = uiIO $ do
        st <- getStore ctx
        let prev = IM.findWithDefault initial key (storeFlag st)
        when (prev /= v) $ do
          setStore ctx (st {storeFlag = IM.insert key v (storeFlag st)})
          markDirty ctx
  pure (cur, set)

useText :: (HasCallStack, Ui :> es) => String -> Eff es (String, String -> Eff es ())
useText initial = do
  wid <- currentId
  ctx <- askContext
  store <- uiIO (getStore ctx)
  let key = intKey wid
      cur = IM.findWithDefault initial key (storeNote store)
      set v = uiIO $ do
        st <- getStore ctx
        let prev = IM.findWithDefault initial key (storeNote st)
        when (prev /= v) $ do
          setStore ctx (st {storeNote = IM.insert key v (storeNote st)})
          markDirty ctx
  pure (cur, set)

useToggle :: (HasCallStack, Ui :> es) => Bool -> Eff es (Bool, Eff es ())
useToggle initial = do
  (cur, set) <- useFlag initial
  pure (cur, set (not cur))

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

kvBlock :: (HasCallStack, Ui :> es) => [(String, String)] -> Eff es ()
kvBlock rows =
  void $
    labelEx
      (tight . gap 0 $ defaultLayout)
      (monoFontMarker <> T.unlines [T.pack (k <> ": " <> v) | (k, v) <- rows])

card :: Ui :> es => Eff es a -> Eff es a
card = panel (minW 300 . padXY 12 10 . gap 8 . fillW $ defaultLayout)

toolbar :: Ui :> es => Eff es a -> Eff es a
toolbar = row (tight . gap 8 . alignMid . fillW $ defaultLayout)

modal :: (HasCallStack, Ui :> es) => Bool -> Text -> Eff es a -> Eff es (Response, Maybe a)
modal open title child
  | not open = do
      wid <- currentId
      pure (emptyModalResp wid, Nothing)
  | otherwise = do
      wid <- currentId
      ctx <- askContext
      inp <- askInput
      (closeResp, body) <- do
          stack <- uiIO (readIORef (ctxContainerStack ctx))
          let fm = ctxFontMetrics ctx
              host = ctxHostProfile ctx
              parent = parentIdx stack
              Size winW winH = inputWindowSize inp
              margin = resolveLayoutGap host fm windowMargin
              availW = max 1 (winW - 2 * margin)
              availH = max 1 (winH - 2 * margin)
              minWidth = floatMinFor host 260 availW
              maxW = availW
              maxH = availH
          uiIO $ do
            idx <-
              addNode
                (ctxNodeArena ctx)
                NodeModal
                parent
                Column
                Fit
                Fit
                (floatPadFor host (Padding 14 14 12 12))
                (floatGapFor host 8)
                minWidth
                0
                maxW
                maxH
                0
                AlignStart
                AlignTop
                False
            setWidgetId (ctxNodeArena ctx) idx wid
            writeIORef (ctxContainerStack ctx) (idx : stack)
            beginModal ctx
          (closeResp, r) <-
            ( do
                close <-
                  row (titleBarLayoutFor host) $ do
                    when (not (T.null title)) $
                      void (labelEx (titleLabelLayoutFor host) (titleMark host (iconModalTitle (ctxIcons ctx)) <> title))
                    flex
                    withKey ("close" :: Text) closeButton
                when (not (T.null title)) sep
                r <-
                  if isCellHost host
                    then scroll (tight . grow $ defaultLayout) child
                    else child
                pure (close, r)
            )
              `uiFinally` do
                endModal ctx
                writeIORef (ctxContainerStack ctx) stack
          pure (closeResp, r)
      mrect <- uiIO (getPrevRect ctx wid)
      let mouse = inputMousePos inp
          inPanel = maybe False (\r -> rectW r > 0 && rectH r > 0 && rectContains r mouse) mrect
          backdrop =
            case mrect of
              Just r | rectW r > 0 && rectH r > 0 ->
                inputMousePressed inp && not (rectContains r mouse)
              _ -> False
          esc = KeyEscape `elem` inputKeys inp
          dismiss = backdrop || esc || respClicked closeResp
      when esc $ uiIO (markEscapeConsumed ctx)
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

image :: (HasCallStack, Ui :> es) => Layout -> ImageId -> Eff es Response
image layout (ImageId tid) = do
  wid <- currentId
  let stored = if tid <= 0 then T.empty else T.pack (show tid)
  addWidget wid NodeImage stored 0 layout

window :: (HasCallStack, Ui :> es) => Bool -> Text -> Eff es a -> Eff es (Response, Maybe a)
window open title child
  | not open = do
      wid <- currentId
      pure (emptyModalResp wid, Nothing)
  | otherwise = do
      wid <- currentId
      ctx <- askContext
      inp <- askInput
      (closeResp, body) <- do
          stack <- uiIO (readIORef (ctxContainerStack ctx))
          let fm = ctxFontMetrics ctx
              host = ctxHostProfile ctx
              parent = parentIdx stack
              Size winW winH = inputWindowSize inp
              margin = resolveLayoutGap host fm windowMargin
              availW = max 1 (winW - 2 * margin)
              availH = max 1 (winH - 2 * margin)
              pad = resolveLayoutPadding host fm (floatPadFor host windowPad)
              authoredMin = if isCellHost host then 160 else 280
              minWidth = floatMinFor host authoredMin availW
              minHeight =
                min availH (padT pad + titleBarHFor host + padB pad)
              maxW = availW
              maxH = availH
          uiIO $ do
            idx <-
              addNode
                (ctxNodeArena ctx)
                NodeWindow
                parent
                Column
                Fit
                Fit
                (floatPadFor host windowPad)
                (floatGapFor host 10)
                minWidth
                minHeight
                maxW
                maxH
                0
                AlignStart
                AlignTop
                False
            setWidgetId (ctxNodeArena ctx) idx wid
            writeIORef (ctxContainerStack ctx) (idx : stack)
          (closeResp, body) <-
            ( do
                close <-
                  row (titleBarLayoutFor host) $ do
                    when (not (T.null title)) $
                      withKey title (void (labelEx (titleLabelLayoutFor host) (titleMark host (iconWindowTitle (ctxIcons ctx)) <> title)))
                    flex
                    withKey ("close" :: Text) closeButton
                sep
                body <- scroll (tight . grow $ defaultLayout) child
                pure (close, body)
            )
              `uiFinally` writeIORef (ctxContainerStack ctx) stack
          pure (closeResp, body)
      mrect <- uiIO (getPrevRect ctx wid)
      let mouse = inputMousePos inp
          inPanel = maybe False (\r -> rectW r > 0 && rectH r > 0 && rectContains r mouse) mrect
      pure
        ( Response
            { respId = wid
            , respRect = maybe (Rect 0 0 0 0) id mrect
            , respHovered = inPanel
            , respPressed = False
            , respClicked = respClicked closeResp
            , respChanged = respClicked closeResp
            }
        , Just body
        )

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

container :: Ui :> es => NodeType -> Layout -> Eff es a -> Eff es a
container nt layout child = do
  ctx <- askContext
  stack <- uiIO $ do
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
    setAspect (ctxNodeArena ctx) idx (layoutAspect layout)
    writeIORef (ctxContainerStack ctx) (idx : stack)
    pure stack
  r <- uiFinally child (writeIORef (ctxContainerStack ctx) stack)
  pure r

{-# INLINE label #-}
label :: (HasCallStack, Ui :> es) => Text -> Eff es Response
label = labelEx defaultLayout

{-# INLINE labelEx #-}
labelEx :: (HasCallStack, Ui :> es) => Layout -> Text -> Eff es Response
labelEx layout txt = do
  wid <- currentId
  addWidget wid NodeText txt 0 layout

closeButtonMarker :: Text
closeButtonMarker = T.singleton '\x01'

-- Title bar marks are a cell-host affordance; pixel hosts draw their own chrome.
titleMark :: HostProfile -> Text -> Text
titleMark host mark = if isCellHost host then mark else ""

{-# INLINE closeButton #-}
closeButton :: (HasCallStack, Ui :> es) => Eff es Response
closeButton = do
  wid <- currentId
  ctx <- askContext
  uiIO $ registerFocusable ctx wid
  let host = ctxHostProfile ctx
      stored = "[ " <> closeButtonMarker <> iconClose (ctxIcons ctx) <> " ]"
      h = titleBarHFor host
      layout =
        if isCellHost host
          then
            -- Same 3-cell slot as Win32 / ASCII so the glyph column matches.
            let slotW = 3
             in tight . fixedW slotW . alignMid $ defaultLayout
          else tight . fixedWH h h . alignMid $ defaultLayout
  resp <- addWidget wid NodeButton stored 0 layout
  disabled <- uiIO (isDisabled ctx wid)
  pure
    resp
      { respClicked = not disabled && respClicked resp
      , respHovered = not disabled && respHovered resp
      }

{-# INLINE button #-}
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

{-# INLINE checkbox #-}
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

{-# INLINE slider #-}
slider :: (HasCallStack, Ui :> es) => Text -> Float -> Float -> Float -> Eff es (Response, Float)
slider = sliderEx (fillW defaultLayout)

{-# INLINE sliderEx #-}
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

{-# INLINE textInput #-}
textInput :: (HasCallStack, Ui :> es) => Text -> String -> Eff es (Response, String)
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
      cursor = IM.findWithDefault (length current) key (storeCursor store)
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

textInputLayout :: Layout
textInputLayout =
  defaultLayout
    { layoutWidth = Grow 1
    , layoutMinW = 160
    }

{-# INLINE separator #-}
separator :: (HasCallStack, Ui :> es) => Eff es Response
separator = do
  wid <- currentId
  ctx <- askContext
  inp <- askInput
  uiIO $ do
    stack <- readIORef (ctxContainerStack ctx)
    let parent = parentIdx stack
    parentDir <-
      if parent < 0
        then pure DirColumn
        else getDirection (ctxNodeArena ctx) parent
    let (dir, wSiz, hSiz) =
          case parentDir of
            DirColumn -> (Column, Grow 1, Fixed 1)
            DirRow -> (Row, Fixed 1, Grow 1)
    idx <-
      addNode
        (ctxNodeArena ctx)
        NodeSeparator
        parent
        dir
        wSiz
        hSiz
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
spacer :: (HasCallStack, Ui :> es) => Sizing -> Sizing -> Eff es Response
spacer w h = do
  wid <- currentId
  ctx <- askContext
  inp <- askInput
  uiIO $ do
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

{-# INLINE scroll #-}
scroll :: (HasCallStack, Ui :> es) => Layout -> Eff es a -> Eff es a
scroll layout child = do
  (_, r) <- scrollArea layout child
  pure r

{-# INLINE scrollArea #-}
scrollArea :: (HasCallStack, Ui :> es) => Layout -> Eff es a -> Eff es (WidgetId, a)
scrollArea layout child = do
  wid <- currentId
  ctx <- askContext
  stack <- uiIO $ do
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
    pure stack
  childR <- uiFinally child (writeIORef (ctxContainerStack ctx) stack)
  pure (wid, childR)

{-# INLINE select #-}
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

{-# INLINE tooltip #-}
tooltip :: Ui :> es => Text -> Response -> Eff es Response
tooltip tipTxt resp = do
  when (respHovered resp) $ do
    ctx <- askContext
    uiIO $ do
      let (Rect rx ry rw rh) = respRect resp
      pushTooltip ctx (respId resp) (Rect (rx + rw + 4) ry 100 (max rh 20)) tipTxt
  pure resp

textInputText :: Text -> String -> Int -> Bool -> Text
textInputText = textInputTerminalText

addWidget ::
  Ui :> es =>
  WidgetId ->
  NodeType ->
  Text ->
  Float ->
  Layout ->
  Eff es Response
addWidget wid nt txt value layout = addWidgetResp wid nt txt value layout Nothing

addWidgetResp ::
  Ui :> es =>
  WidgetId ->
  NodeType ->
  Text ->
  Float ->
  Layout ->
  Maybe Response ->
  Eff es Response
addWidgetResp wid nt txt value layout mResp =
  addWidgetStyled wid nt txt value layout 0 mResp

addWidgetStyled ::
  Ui :> es =>
  WidgetId ->
  NodeType ->
  Text ->
  Float ->
  Layout ->
  Int ->
  Maybe Response ->
  Eff es Response
addWidgetStyled wid nt txt value layout styleIdx mResp = do
  ctx <- askContext
  inp <- askInput
  uiIO $ do
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
    setAspect (ctxNodeArena ctx) idx (layoutAspect layout)
    setNodeText (ctxNodeArena ctx) idx txt
    setNodeValue (ctxNodeArena ctx) idx value
    setStyleIdx (ctxNodeArena ctx) idx styleIdx
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
