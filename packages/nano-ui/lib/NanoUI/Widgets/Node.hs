{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Widget node construction and interaction responses.
module NanoUI.Widgets.Node
  ( Response (..)
  , Responding (..)
  , Clickable (..)
  , RightClickable (..)
  , onRightClick
  , mkResponse
  , emptyModalResp
  , setClicked
  , setChanged
  , setHovered
  , setPressed
  , setSubmitted
  , setRightClicked
  , setRightPressed
  , parentIdx
  , container
  , containerStyled
  , containerResponse
  , containerResponseStyled
  , addWidget
  , addWidgetResp
  , addWidgetStyled
  , addWidgetWithOptions
  , addSizingLeafNode
  , resolveInteraction
  , tagContainer
  )
where

import Control.Monad (when)
import Data.IORef (readIORef, writeIORef)
import Data.Text (Text)
import Effectful (Eff, type (:>))
import NanoUI.Context
  ( Context (..)
  , isDisabled
  , pointerBlockedByOverlay
  )
import NanoUI.Id (WidgetId (..), enterScope, hashWidgetId, scopeTag)
import NanoUI.Input
  ( Input (..)
  , inputMouseDown
  , inputMousePos
  , inputMouseReleased
  , inputMouseRightDown
  , inputMouseRightReleased
  )
import NanoUI.Layout.Arena
  ( NodeType (..)
  , addNode
  , addNodeFromLayout
  , setNodeText
  , setOptions
  , setNodeValue
  , setStyleIdx
  , setWidgetId
  )
import NanoUI.Monad (Ui, askContext, askInput, nextId, uiIO)
import NanoUI.WidgetText (packTextNodeStyle)
import NanoUI.Style
  ( AlignX (..)
  , AlignY (..)
  , Direction (..)
  , Layout (..)
  , Padding (..)
  , Sizing (..)
  )
import NanoUI.Types (Rect (..), rectContains, rectH, rectUnion, rectW)
import NanoUI.Frame.Hit (findNodeByWidgetId, nodeInteractionHit, scrollHitRect)

parentIdx :: [Int] -> Int
parentIdx = \case
  [] -> -1
  (p : _) -> p

class Responding r where
  respId :: r -> WidgetId
  respRect :: r -> Rect
  respHovered :: r -> Bool
  respPressed :: r -> Bool
  respClicked :: r -> Bool
  respChanged :: r -> Bool
  respSubmitted :: r -> Bool
  respSubmitted _ = False
  respRightPressed :: r -> Bool
  respRightPressed _ = False
  respRightClicked :: r -> Bool
  respRightClicked _ = False

class Clickable r where
  respIsClicked :: r -> Bool

class RightClickable r where
  respIsRightClicked :: r -> Bool

onRightClick :: RightClickable r => r -> Eff es () -> Eff es ()
onRightClick resp act = when (respIsRightClicked resp) act

data Response = Response
  { rawRespId :: !WidgetId
  , rawRespRect :: !Rect
  , rawRespHovered :: !Bool
  , rawRespPressed :: !Bool
  , rawRespClicked :: !Bool
  , rawRespChanged :: !Bool
  , rawRespSubmitted :: !Bool
  , rawRespRightPressed :: !Bool
  , rawRespRightClicked :: !Bool
  }
  deriving (Eq, Show)

instance Responding Response where
  respId = rawRespId
  respRect = rawRespRect
  respHovered = rawRespHovered
  respPressed = rawRespPressed
  respClicked = rawRespClicked
  respChanged = rawRespChanged
  respSubmitted = rawRespSubmitted
  respRightPressed = rawRespRightPressed
  respRightClicked = rawRespRightClicked

instance Clickable Response where
  respIsClicked = rawRespClicked

instance RightClickable Response where
  respIsRightClicked = rawRespRightClicked

instance Semigroup Response where
  a <> b =
    Response
      { rawRespId = rawRespId b
      , rawRespRect = unionRespRect (rawRespRect a) (rawRespRect b)
      , rawRespHovered = rawRespHovered a || rawRespHovered b
      , rawRespPressed = rawRespPressed a || rawRespPressed b
      , rawRespClicked = rawRespClicked a || rawRespClicked b
      , rawRespChanged = rawRespChanged a || rawRespChanged b
      , rawRespSubmitted = rawRespSubmitted a || rawRespSubmitted b
      , rawRespRightPressed = rawRespRightPressed a || rawRespRightPressed b
      , rawRespRightClicked = rawRespRightClicked a || rawRespRightClicked b
      }

instance Monoid Response where
  mempty = mkResponse (WidgetId 0) (Rect 0 0 0 0) False False False False

unionRespRect :: Rect -> Rect -> Rect
unionRespRect a b
  | rectW a <= 0 || rectH a <= 0 = b
  | rectW b <= 0 || rectH b <= 0 = a
  | otherwise = rectUnion a b

setClicked :: Bool -> Response -> Response
setClicked c r = r {rawRespClicked = c}

setChanged :: Bool -> Response -> Response
setChanged c r = r {rawRespChanged = c}

setSubmitted :: Bool -> Response -> Response
setSubmitted s r = r {rawRespSubmitted = s}

setHovered :: Bool -> Response -> Response
setHovered h r = r {rawRespHovered = h}

setPressed :: Bool -> Response -> Response
setPressed p r = r {rawRespPressed = p}

setRightClicked :: Bool -> Response -> Response
setRightClicked c r = r {rawRespRightClicked = c}

setRightPressed :: Bool -> Response -> Response
setRightPressed p r = r {rawRespRightPressed = p}

mkResponse :: WidgetId -> Rect -> Bool -> Bool -> Bool -> Bool -> Response
mkResponse wid rect hovered pressed clicked changed =
  Response
    { rawRespId = wid
    , rawRespRect = rect
    , rawRespHovered = hovered
    , rawRespPressed = pressed
    , rawRespClicked = clicked
    , rawRespChanged = changed
    , rawRespSubmitted = False
    , rawRespRightPressed = False
    , rawRespRightClicked = False
    }

emptyModalResp :: WidgetId -> Response
emptyModalResp wid = mkResponse wid (Rect 0 0 0 0) False False False False

container :: Ui :> es => NodeType -> Layout -> Eff es a -> Eff es a
container nt layout child = runContainer nt layout Nothing child

containerStyled :: Ui :> es => NodeType -> Layout -> Int -> Eff es a -> Eff es a
containerStyled nt layout si child = runContainerStyled nt layout Nothing si child

containerResponse :: Ui :> es => NodeType -> Layout -> Eff es a -> Eff es (a, Response)
containerResponse nt layout child = containerResponseStyled nt layout 0 child

containerResponseStyled :: Ui :> es => NodeType -> Layout -> Int -> Eff es a -> Eff es (a, Response)
containerResponseStyled nt layout si child = do
  wid <- nextId
  ctx <- askContext
  inp <- askInput
  r <- runContainerStyled nt layout (Just wid) si child
  resp <- uiIO (resolveInteraction ctx inp wid)
  pure (r, resp)

runContainer :: Ui :> es => NodeType -> Layout -> Maybe WidgetId -> Eff es a -> Eff es a
runContainer nt layout mWid child = runContainerStyled nt layout mWid 0 child

runContainerStyled :: Ui :> es => NodeType -> Layout -> Maybe WidgetId -> Int -> Eff es a -> Eff es a
runContainerStyled nt layout mWid si child = do
  ctx <- askContext
  (stack, parent') <- uiIO $ do
    stack0 <- readIORef (ctxContainerStack ctx)
    let
      parent = parentIdx stack0
    idx <- addNodeFromLayout (ctxNodeArena ctx) nt parent layout
    when (si /= 0) $
      setStyleIdx (ctxNodeArena ctx) idx si
    case mWid of
      Just wid -> setWidgetId (ctxNodeArena ctx) idx wid
      Nothing -> pure ()
    writeIORef (ctxContainerStack ctx) (idx : stack0)
    oldCtx <- readIORef (ctxIdContext ctx)
    let
      (parent', childCtx) = enterScope scopeTag oldCtx
    writeIORef (ctxIdContext ctx) childCtx
    pure (stack0, parent')
  r <- child
  uiIO $ do
    writeIORef (ctxContainerStack ctx) stack
    writeIORef (ctxIdContext ctx) parent'
  pure r

addSizingLeafNode ::
  Context
  -> Input
  -> WidgetId
  -> NodeType
  -> Direction
  -> Sizing
  -> Sizing
  -> IO Response
addSizingLeafNode ctx inp wid nt dir wSiz hSiz = do
  stack <- readIORef (ctxContainerStack ctx)
  let
    parent = parentIdx stack
  idx <-
    addNode
      (ctxNodeArena ctx)
      nt
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
  setWidgetId (ctxNodeArena ctx) idx wid
  resolveInteraction ctx inp wid

addWidget ::
  Ui :> es =>
  WidgetId
  -> NodeType
  -> Text
  -> Float
  -> Layout
  -> Eff es Response
addWidget wid nt txt value layout = addWidgetResp wid nt txt value layout Nothing

addWidgetResp ::
  Ui :> es =>
  WidgetId
  -> NodeType
  -> Text
  -> Float
  -> Layout
  -> Maybe Response
  -> Eff es Response
addWidgetResp wid nt txt value layout mResp =
  addWidgetStyled wid nt txt value layout 0 mResp

addWidgetStyled ::
  Ui :> es =>
  WidgetId
  -> NodeType
  -> Text
  -> Float
  -> Layout
  -> Int
  -> Maybe Response
  -> Eff es Response
addWidgetStyled wid nt txt value layout styleIdx mResp = do
  ctx <- askContext
  inp <- askInput
  uiIO $ do
    stack <- readIORef (ctxContainerStack ctx)
    let
      parent = parentIdx stack
    idx <- addNodeFromLayout (ctxNodeArena ctx) nt parent layout
    setNodeText (ctxNodeArena ctx) idx txt
    setNodeValue (ctxNodeArena ctx) idx value
    let effectiveStyle
          | nt == NodeText = packTextNodeStyle (layoutFontVariant layout) styleIdx
          | otherwise = styleIdx
    setStyleIdx (ctxNodeArena ctx) idx effectiveStyle
    setWidgetId (ctxNodeArena ctx) idx wid
    case mResp of
      Just resp -> pure resp
      Nothing -> resolveInteraction ctx inp wid

addWidgetWithOptions ::
  Ui :> es =>
  WidgetId
  -> NodeType
  -> Text
  -> [Text]
  -> Float
  -> Layout
  -> Eff es Response
addWidgetWithOptions wid nt txt opts value layout = do
  ctx <- askContext
  inp <- askInput
  uiIO $ do
    stack <- readIORef (ctxContainerStack ctx)
    let parent = parentIdx stack
    idx <- addNodeFromLayout (ctxNodeArena ctx) nt parent layout
    setNodeText (ctxNodeArena ctx) idx txt
    setOptions (ctxNodeArena ctx) idx opts
    setNodeValue (ctxNodeArena ctx) idx value
    setStyleIdx (ctxNodeArena ctx) idx 0
    setWidgetId (ctxNodeArena ctx) idx wid
    resolveInteraction ctx inp wid

resolveInteraction :: Context -> Input -> WidgetId -> IO Response
resolveInteraction ctx inp wid = do
  disabled <- isDisabled ctx wid
  mrect <- scrollHitRect ctx wid
  blocked <- pointerBlockedByOverlay ctx (inputMousePos inp)
  active <- readIORef (ctxActiveId ctx)
  let mouse = inputMousePos inp
      captured =
        hashWidgetId active /= 0 && active /= wid && inputMouseDown inp
  hovered <-
    if disabled || blocked || captured
      then pure False
      else
        case mrect of
          Nothing -> pure False
          Just r ->
            findNodeByWidgetId ctx wid >>= \case
              Nothing ->
                pure (rectContains r mouse)
              Just idx -> nodeInteractionHit ctx idx r mouse
  let
    rect = case mrect of
      Just r -> r
      Nothing -> Rect 0 0 0 0
    pressed = hovered && inputMouseDown inp
    rightPressed = hovered && inputMouseRightDown inp
  pending <- readIORef (ctxClickedId ctx)
  when (hovered && inputMouseReleased inp && wid == active) $
    writeIORef (ctxReleaseClickedId ctx) wid
  let
    clicked = (hovered && inputMouseReleased inp) || pending == wid
    rightClicked = hovered && inputMouseRightReleased inp
  pure $
    Response
      { rawRespId = wid
      , rawRespRect = rect
      , rawRespHovered = hovered
      , rawRespPressed = pressed
      , rawRespClicked = clicked
      , rawRespChanged = False
      , rawRespSubmitted = False
      , rawRespRightPressed = rightPressed
      , rawRespRightClicked = rightClicked
      }

-- | Stamp the current container with a widget id (radio/tree group key).
tagContainer :: Ui :> es => WidgetId -> Eff es ()
tagContainer wid = do
  ctx <- askContext
  uiIO $ do
    stack <- readIORef (ctxContainerStack ctx)
    case stack of
      (idx : _) -> setWidgetId (ctxNodeArena ctx) idx wid
      [] -> pure ()
