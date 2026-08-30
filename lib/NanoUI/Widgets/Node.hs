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
  ) where

import Data.IORef (readIORef, writeIORef)
import Effectful (Eff, type (:>))
import Data.Text (Text)
import NanoUI.Context
  ( Context (..)
  , getPrevRect
  , isDisabled
  , pointerBlockedByOverlay
  )
import NanoUI.Id (WidgetId (..))
import NanoUI.Input (Input (..), inputMouseDown, inputMousePos, inputMouseReleased)
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
import NanoUI.Style (AlignX (..), AlignY (..), Direction (..), Layout (..), Padding (..), Sizing (..))
import NanoUI.Types (Rect (..), rectContains)

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
  stack <- uiIO $ do
    stack <- readIORef (ctxContainerStack ctx)
    let parent = parentIdx stack
    idx <- addNodeFromLayout (ctxNodeArena ctx) nt parent layout
    writeIORef (ctxContainerStack ctx) (idx : stack)
    pure stack
  r <- uiFinally child (writeIORef (ctxContainerStack ctx) stack)
  pure r

addSizingLeafNode ::
  Context ->
  Input ->
  WidgetId ->
  NodeType ->
  Direction ->
  Sizing ->
  Sizing ->
  IO Response
addSizingLeafNode ctx inp wid nt dir wSiz hSiz = do
  stack <- readIORef (ctxContainerStack ctx)
  let parent = parentIdx stack
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
  mrect <- getPrevRect ctx wid
  blocked <- pointerBlockedByOverlay ctx (inputMousePos inp)
  let rect = case mrect of
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
