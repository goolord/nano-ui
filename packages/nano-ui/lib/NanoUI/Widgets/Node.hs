{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Widget node construction and interaction responses.
module NanoUI.Widgets.Node
  ( Response (..)
  , HasResponse (..)
  , respId
  , respRect
  , respHovered
  , respPressed
  , respClicked
  , respChanged
  , respSubmitted
  , respRightPressed
  , respRightClicked
  , onClick
  , onRightClick
  , mkResponse
  , emptyModalResp
  , setClicked
  , setChanged
  , setSubmitted
  , parentIdx
  , container
  , containerStyled
  , containerResponse
  , containerResponseStyled
  , withContainerNode
  , floatingPanel
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
  , getCurrentFloatingId
  , isDisabled
  , pointerBlockedByOverlay
  , setCurrentFloatingId
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
  ( NodeIdx
  , NodeType (..)
  , addNode
  , addNodeFromLayout
  , rootAttachParent
  , setNodeText
  , setOptions
  , setNodeValue
  , setStyleIdx
  , setWidgetId
  )
import NanoUI.Monad (Ui, askContext, askInput, nextId, uiIO)
import NanoUI.WidgetText (packTextNodeStyleFull)
import NanoUI.Style
  ( AlignX (..)
  , AlignY (..)
  , Direction (..)
  , Layout (..)
  , Padding (..)
  , Sizing (..)
  )
import NanoUI.Types (Rect (..), rectContains, rectH, rectHit, rectUnion, rectW)
import NanoUI.Frame.Hit (findNodeByWidgetId, nodeInteractionHit, scrollHitRect)

parentIdx :: [Int] -> Int
parentIdx = \case
  [] -> -1
  (p : _) -> p

-- | Anything that carries a widget 'Response' (composite widget results such
-- as 'NanoUI.Widgets.Tabs.TabResponse'). The @resp*@ accessors work on all of them.
class HasResponse r where
  toResponse :: r -> Response

instance HasResponse Response where
  {-# INLINE toResponse #-}
  toResponse = id

{-# INLINE respId #-}
respId :: HasResponse r => r -> WidgetId
respId = rawRespId . toResponse

{-# INLINE respRect #-}
respRect :: HasResponse r => r -> Rect
respRect = rawRespRect . toResponse

{-# INLINE respHovered #-}
respHovered :: HasResponse r => r -> Bool
respHovered = rawRespHovered . toResponse

{-# INLINE respPressed #-}
respPressed :: HasResponse r => r -> Bool
respPressed = rawRespPressed . toResponse

{-# INLINE respClicked #-}
respClicked :: HasResponse r => r -> Bool
respClicked = rawRespClicked . toResponse

{-# INLINE respChanged #-}
respChanged :: HasResponse r => r -> Bool
respChanged = rawRespChanged . toResponse

{-# INLINE respSubmitted #-}
respSubmitted :: HasResponse r => r -> Bool
respSubmitted = rawRespSubmitted . toResponse

{-# INLINE respRightPressed #-}
respRightPressed :: HasResponse r => r -> Bool
respRightPressed = rawRespRightPressed . toResponse

{-# INLINE respRightClicked #-}
respRightClicked :: HasResponse r => r -> Bool
respRightClicked = rawRespRightClicked . toResponse

{-# INLINE onClick #-}
onClick :: HasResponse r => r -> Eff es () -> Eff es ()
onClick resp = when (respClicked resp)

{-# INLINE onRightClick #-}
onRightClick :: HasResponse r => r -> Eff es () -> Eff es ()
onRightClick resp = when (respRightClicked resp)

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

instance Semigroup Response where
  a <> b =
    Response
      { rawRespId = if rawRespId b == WidgetId 0 then rawRespId a else rawRespId b
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
emptyModalResp wid = mempty {rawRespId = wid}

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
  idx <- uiIO $ do
    stack <- readIORef (ctxContainerStack ctx)
    idx <- addNodeFromLayout (ctxNodeArena ctx) nt (parentIdx stack) layout
    when (si /= 0) $
      setStyleIdx (ctxNodeArena ctx) idx si
    mapM_ (setWidgetId (ctxNodeArena ctx) idx) mWid
    pure idx
  withContainerNode True idx child

-- | Push container node @idx@ (already added under the current parent), run
-- @child@ inside it, then pop. @scoped@ also runs the children in a fresh id
-- scope; it changes the children's widget ids (and so their store keys), so
-- callers pick it explicitly: plain containers scope, scroll containers do not.
withContainerNode :: Ui :> es => Bool -> NodeIdx -> Eff es a -> Eff es a
withContainerNode scoped idx child = do
  ctx <- askContext
  (stack, parentIds) <- uiIO $ do
    stack0 <- readIORef (ctxContainerStack ctx)
    writeIORef (ctxContainerStack ctx) (idx : stack0)
    ids0 <- readIORef (ctxIdContext ctx)
    if scoped
      then do
        let (parentIds, childIds) = enterScope scopeTag ids0
        writeIORef (ctxIdContext ctx) childIds
        pure (stack0, parentIds)
      else pure (stack0, ids0)
  r <- child
  uiIO $ do
    writeIORef (ctxContainerStack ctx) stack
    when scoped $ writeIORef (ctxIdContext ctx) parentIds
  pure r

-- | A floating panel (popup, modal, window): its node attaches to the root
-- layer and it is the current floating panel while @body@ runs. @addPanel@
-- adds the node under the given parent; @enter@ runs once the node is pushed
-- (seeding its rect, opening a modal).
floatingPanel ::
  Ui :> es => Bool -> WidgetId -> (Int -> IO NodeIdx) -> IO () -> Eff es a -> Eff es a
floatingPanel scoped wid addPanel enter body = do
  ctx <- askContext
  let arena = ctxNodeArena ctx
  prevFloat <- uiIO (getCurrentFloatingId ctx)
  idx <- uiIO $ do
    stack <- readIORef (ctxContainerStack ctx)
    idx <- addPanel =<< rootAttachParent arena (parentIdx stack)
    setWidgetId arena idx wid
    pure idx
  r <- withContainerNode scoped idx (uiIO (enter >> setCurrentFloatingId ctx (Just wid)) >> body)
  uiIO (setCurrentFloatingId ctx prevFloat)
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

{-# INLINE addWidget #-}
addWidget ::
  Ui :> es =>
  WidgetId
  -> NodeType
  -> Text
  -> Float
  -> Layout
  -> Eff es Response
addWidget wid nt txt value layout = addWidgetResp wid nt txt value layout Nothing

{-# INLINE addWidgetResp #-}
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

{-# INLINE addWidgetStyled #-}
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
          | nt == NodeText = packTextNodeStyleFull (layoutFontVariant layout) (layoutFontWeight layout) (layoutFontStyle layout) (layoutTextDecoration layout) styleIdx
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
  mrect <- scrollHitRect ctx wid
  active <- readIORef (ctxActiveId ctx)
  pending <- readIORef (ctxClickedId ctx)
  let
    mouse = inputMousePos inp
    rect = case mrect of
      Just r -> r
      Nothing -> Rect 0 0 0 0
    canHit = rectHit rect mouse || pending == wid
  if not canHit
    then pure (mkResponse wid rect False False False False)
    else do
      disabled <- isDisabled ctx wid
      blocked <- pointerBlockedByOverlay ctx mouse
      let
        captured =
          hashWidgetId active /= 0 && active /= wid && inputMouseDown inp
      hovered <-
        if disabled || blocked || captured
          then pure False
          else
            findNodeByWidgetId ctx wid >>= \case
              Nothing ->
                pure (rectContains rect mouse)
              Just idx -> nodeInteractionHit ctx idx rect mouse
      let
        pressed = hovered && inputMouseDown inp
        rightPressed = hovered && inputMouseRightDown inp
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
