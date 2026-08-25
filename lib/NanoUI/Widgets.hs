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
  ) where

import Control.Monad (when)
import Data.IORef (readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import GHC.Stack (HasCallStack)
import NanoUI.Context
  ( Context (..)
  , WidgetStore (..)
  , getPrevRect
  , getStore
  , intKey
  , setStore
  )
import NanoUI.Draw (beginLayer, Layer (..), pushRect, pushText)
import NanoUI.Id (WidgetId (..))
import NanoUI.Input (Input (..), Key (..))
import NanoUI.Layout.Arena
  ( NodeType (..)
  , addNode
  , setNodeText
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
  , Style (..)
  , Theme (..)
  , defaultLayout
  , themeButton
  , themeInput
  , themePanel
  )
import NanoUI.Types (Rect (..), V2 (..), rectContains)

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
panel :: HasCallStack => Layout -> UI a -> UI a
panel = container

{-# INLINE row #-}
row :: HasCallStack => Layout -> UI a -> UI a
row layout child = container (layout {layoutDirection = Row}) child

{-# INLINE column #-}
column :: HasCallStack => Layout -> UI a -> UI a
column layout child = container (layout {layoutDirection = Column}) child

container :: HasCallStack => Layout -> UI a -> UI a
container layout child = UI $ \ctx inp -> do
  stack <- readIORef (ctxContainerStack ctx)
  let parent = if null stack then -1 else head stack
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
label txt = addWidget NodeText txt defaultLayout themePanel

{-# INLINE button #-}
button :: HasCallStack => Text -> UI Response
button txt = do
  resp <- addWidget NodeWidget txt defaultLayout themeButton
  when (respClicked resp) $ emit ("button:" <> T.unpack txt)
  pure resp

{-# INLINE checkbox #-}
checkbox :: HasCallStack => Text -> Bool -> UI (Response, Bool)
checkbox txt initial = do
  wid <- currentId
  ctx <- askContext
  store <- liftIO (getStore ctx)
  let key = intKey wid
      current = IM.findWithDefault initial key (storeCheckbox store)
  resp <- addWidget NodeWidget txt defaultLayout themeButton
  let newVal = if respClicked resp then not current else current
  when (respClicked resp) $ do
    liftIO $ setStore ctx (store {storeCheckbox = IM.insert key newVal (storeCheckbox store)})
    emit ("checkbox:" <> T.unpack txt)
  pure (resp, newVal)

{-# INLINE slider #-}
slider :: HasCallStack => Text -> Float -> Float -> Float -> UI (Response, Float)
slider lbl minV maxV initial = do
  wid <- currentId
  ctx <- askContext
  inp <- askInput
  store <- liftIO (getStore ctx)
  let key = intKey wid
      current = IM.findWithDefault initial key (storeSlider store)
  resp <- addWidget NodeWidget lbl defaultLayout themeInput
  active <- liftIO (readIORef (ctxActiveId ctx))
  let isActive = active == wid
      hovered = respHovered resp
      pressed = inputMouseDown inp && (hovered || isActive)
  val <-
    liftIO $ do
      mrect <- getPrevRect ctx wid
      let frac =
            case mrect of
              Nothing -> 0
              Just (Rect x _ w _) ->
                let px = v2X (inputMousePos inp)
                    f = (px - x) / max w 1
                 in max 0 (min 1 f)
          computed = minV + frac * (maxV - minV)
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
  inp <- askInput
  store <- liftIO (getStore ctx)
  let key = intKey wid
      current = IM.findWithDefault initial key (storeText store)
      cursor = IM.findWithDefault (length current) key (storeCursor store)
  resp <- addWidget NodeWidget lbl defaultLayout themeInput
  focus <- liftIO (readIORef (ctxFocusId ctx))
  let isFocus = focus == wid
  when (respClicked resp) $ liftIO $ writeIORef (ctxFocusId ctx) wid
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
  pure (resp {respChanged = newText /= current}, newText)

{-# INLINE separator #-}
separator :: HasCallStack => UI Response
separator = do
  wid <- currentId
  ctx <- askContext
  inp <- askInput
  liftIO $ do
    stack <- readIORef (ctxContainerStack ctx)
    let parent = if null stack then -1 else head stack
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
    let parent = if null stack then -1 else head stack
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

{-# INLINE tooltip #-}
tooltip :: HasCallStack => Text -> Response -> UI Response
tooltip tipTxt resp = do
  when (respHovered resp) $ do
    ctx <- askContext
    liftIO $ do
      let (Rect rx ry rw rh) = respRect resp
      beginLayer (ctxDrawArena ctx) LayerOverlay
      let fm = ctxFontMetrics ctx
          style = themePanel (ctxTheme ctx)
      pushRect (ctxDrawArena ctx) (Rect (rx + rw + 4) ry 100 20) (styleBg style)
      pushText (ctxDrawArena ctx) fm (rx + rw + 8) (ry + 4) tipTxt (styleFg style)
  pure resp

addWidget ::
  HasCallStack =>
  NodeType ->
  Text ->
  Layout ->
  (Theme -> Style) ->
  UI Response
addWidget nt txt layout styleFn = do
  wid <- currentId
  ctx <- askContext
  inp <- askInput
  liftIO $ do
    stack <- readIORef (ctxContainerStack ctx)
    let parent = if null stack then -1 else head stack
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
    setWidgetId (ctxNodeArena ctx) idx wid
    resolveInteraction ctx inp wid

resolveInteraction :: Context -> Input -> WidgetId -> IO Response
resolveInteraction ctx inp wid = do
  mrect <- getPrevRect ctx wid
  let rect = maybe (Rect 0 0 0 0) id mrect
      mouse = inputMousePos inp
      hovered = rectContains rect mouse
  active <- readIORef (ctxActiveId ctx)
  let isActive = active == wid
  when hovered $ writeIORef (ctxHotId ctx) wid
  when (inputMousePressed inp && hovered) $
    writeIORef (ctxActiveId ctx) wid
  let pressed = inputMouseDown inp && (hovered || isActive)
      clicked = inputMouseReleased inp && isActive
  when clicked $ writeIORef (ctxActiveId ctx) (WidgetId 0)
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

liftIO :: IO a -> UI a
liftIO act = UI (\_ _ -> act)
