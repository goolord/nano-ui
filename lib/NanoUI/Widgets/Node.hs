{-# LANGUAGE OverloadedStrings #-}

-- | Widget node construction and interaction responses.
module NanoUI.Widgets.Node
  ( Response (..)
  , mkResponse
  , emptyModalResp
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
  , pointerBlockedByModal
  )
import NanoUI.Id (WidgetId (..))
import NanoUI.Input (Input (..), inputMouseDown, inputMousePos, inputMousePressed, inputMouseReleased)
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

data Response = Response
  { respId :: WidgetId
  , respRect :: Rect
  , respHovered :: Bool
  , respPressed :: Bool
  , respClicked :: Bool
  , respChanged :: Bool
  }
  deriving (Eq, Show)

mkResponse :: WidgetId -> Rect -> Bool -> Bool -> Bool -> Bool -> Response
mkResponse wid rect hovered pressed clicked changed =
  Response
    { respId = wid
    , respRect = rect
    , respHovered = hovered
    , respPressed = pressed
    , respClicked = clicked
    , respChanged = changed
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
  if disabled
    then do
      mrect <- getPrevRect ctx wid
      let rect = maybe (Rect 0 0 0 0) id mrect
      pure (mkResponse wid rect False False False False)
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
      pure (mkResponse wid rect hovered pressed clicked False)
