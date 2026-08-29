{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Widgets.Internal
  ( Response (..)
  , parentIdx
  , titleBarHFor
  , titleBarLayoutFor
  , titleLabelLayoutFor
  , floatPadFor
  , floatGapFor
  , floatMinFor
  , mkResponse
  , emptyModalResp
  , container
  , titleMark
  , closeButton
  , addWidget
  , addWidgetResp
  , addWidgetStyled
  , resolveInteraction
  ) where

import Data.IORef (readIORef, writeIORef)
import Effectful (Eff, type (:>))
import Data.Text (Text)
import GHC.Stack (HasCallStack)
import NanoUI.Font (layoutUnitScale)
import NanoUI.WidgetMarkers (closeButtonMarker)
import NanoUI.Host (HostProfile, isCellHost)
import NanoUI.Context
  ( Context (..)
  , getPrevRect
  , isDisabled
  , pointerBlockedByModal
  , registerFocusable
  )
import NanoUI.Icons (Icons (..))
import NanoUI.Id (WidgetId (..))
import NanoUI.Input (Input (..), inputMouseDown, inputMousePos, inputMousePressed, inputMouseReleased)
import NanoUI.Layout.Arena
  ( NodeType (..)
  , addNodeFromLayout
  , setNodeText
  , setNodeValue
  , setStyleIdx
  , setWidgetId
  )
import NanoUI.Monad (Ui, askContext, askInput, currentId, uiFinally, uiIO)
import NanoUI.Style
  ( Layout (..)
  , Padding (..)
  , alignMid
  , defaultLayout
  , fillW
  , fixedH
  , fixedW
  , fixedWH
  , gap
  , tight
  )
import NanoUI.Types (Rect (..), rectContains)

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
