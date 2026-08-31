{-# LANGUAGE OverloadedStrings #-}

-- | Widget node construction and interaction responses.
module NanoUI.Widgets.Node
  ( Response (..)
  , Responding (..)
  , Clickable (..)
  , mkResponse
  , emptyModalResp
  , setClicked
  , setChanged
  , setHovered
  , setPressed
  , parentIdx
  , container
  , addWidget
  , addWidgetResp
  , addWidgetStyled
  , addSizingLeafNode
  , resolveInteraction
  , tagContainer
  )
where

import Data.IORef (readIORef, writeIORef)
import Data.Text (Text)
import Effectful (Eff, type (:>))
import NanoUI.Context
  ( Context (..)
  , isDisabled
  , pointerBlockedByOverlay
  )
import NanoUI.Id (WidgetId (..), enterScope, scopeTag)
import NanoUI.Input
  ( Input (..)
  , inputMouseDown
  , inputMousePos
  , inputMouseReleased
  )
import NanoUI.Layout.Arena
  ( NodeType (..)
  , addNode
  , addNodeFromLayout
  , setNodeText
  , setNodeValue
  , setStyleIdx
  , setWidgetId
  )
import NanoUI.Monad (Ui, askContext, askInput, uiFinally, uiIO)
import NanoUI.Style
  ( AlignX (..)
  , AlignY (..)
  , Direction (..)
  , Layout (..)
  , Padding (..)
  , Sizing (..)
  )
import NanoUI.Types (Rect (..), rectContains, rectH, rectUnion, rectW)
import NanoUI.Frame.Hit (scrollHitRect)

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

class Clickable r where
  respIsClicked :: r -> Bool

data Response = Response
  { rawRespId :: !WidgetId
  , rawRespRect :: !Rect
  , rawRespHovered :: !Bool
  , rawRespPressed :: !Bool
  , rawRespClicked :: !Bool
  , rawRespChanged :: !Bool
  }
  deriving (Eq, Show)

instance Responding Response where
  respId = rawRespId
  respRect = rawRespRect
  respHovered = rawRespHovered
  respPressed = rawRespPressed
  respClicked = rawRespClicked
  respChanged = rawRespChanged

instance Clickable Response where
  respIsClicked = rawRespClicked

instance Semigroup Response where
  a <> b =
    Response
      { rawRespId = rawRespId b
      , rawRespRect = unionRespRect (rawRespRect a) (rawRespRect b)
      , rawRespHovered = rawRespHovered a || rawRespHovered b
      , rawRespPressed = rawRespPressed a || rawRespPressed b
      , rawRespClicked = rawRespClicked a || rawRespClicked b
      , rawRespChanged = rawRespChanged a || rawRespChanged b
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

setHovered :: Bool -> Response -> Response
setHovered h r = r {rawRespHovered = h}

setPressed :: Bool -> Response -> Response
setPressed p r = r {rawRespPressed = p}

mkResponse :: WidgetId -> Rect -> Bool -> Bool -> Bool -> Bool -> Response
mkResponse wid rect hovered pressed clicked changed =
  Response
    { rawRespId = wid
    , rawRespRect = rect
    , rawRespHovered = hovered
    , rawRespPressed = pressed
    , rawRespClicked = clicked
    , rawRespChanged = changed
    }

emptyModalResp :: WidgetId -> Response
emptyModalResp wid = mkResponse wid (Rect 0 0 0 0) False False False False

container :: Ui :> es => NodeType -> Layout -> Eff es a -> Eff es a
container nt layout child = do
  ctx <- askContext
  (stack, parent') <- uiIO $ do
    stack0 <- readIORef (ctxContainerStack ctx)
    let
      parent = parentIdx stack0
    idx <- addNodeFromLayout (ctxNodeArena ctx) nt parent layout
    writeIORef (ctxContainerStack ctx) (idx : stack0)
    oldCtx <- readIORef (ctxIdContext ctx)
    let
      (parent', childCtx) = enterScope scopeTag oldCtx
    writeIORef (ctxIdContext ctx) childCtx
    pure (stack0, parent')
  r <-
    uiFinally child $ do
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
      False
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
    setStyleIdx (ctxNodeArena ctx) idx styleIdx
    setWidgetId (ctxNodeArena ctx) idx wid
    case mResp of
      Just resp -> pure resp
      Nothing -> resolveInteraction ctx inp wid

resolveInteraction :: Context -> Input -> WidgetId -> IO Response
resolveInteraction ctx inp wid = do
  disabled <- isDisabled ctx wid
  mrect <- scrollHitRect ctx wid
  blocked <- pointerBlockedByOverlay ctx (inputMousePos inp)
  let
    rect = case mrect of
      Just r -> r
      Nothing -> Rect 0 0 0 0
    mouse = inputMousePos inp
    hovered = not disabled && not blocked && maybe False (`rectContains` mouse) mrect
    pressed = hovered && inputMouseDown inp
    clicked = hovered && inputMouseReleased inp
  pure $
    Response
      { rawRespId = wid
      , rawRespRect = rect
      , rawRespHovered = hovered
      , rawRespPressed = pressed
      , rawRespClicked = clicked
      , rawRespChanged = False
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
