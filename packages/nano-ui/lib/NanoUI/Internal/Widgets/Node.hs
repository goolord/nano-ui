-- | Widget node construction and interaction responses.
module NanoUI.Internal.Widgets.Node
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
  , mkResponse
  , setClicked
  , setChanged
  , setSubmitted
  , inertResponse
  , currentParent
  , container
  , containerResponse
  , withContainerNode
  , floatingPanel
  , dropdownInput
  , addWidget
  , addWidgetStyled
  , addWidgetWithOptions
  , tagContainer
  )
where

import Control.Monad (when)
import Data.IORef (readIORef, writeIORef)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Effectful (Eff, type (:>))
import NanoUI.Internal.Context
  ( Context (..)
  , isDisabled
  , intKey
  , routedInput
  , getsInteraction
  , PointerRoute (..)
  , InteractionState (..)
  )
import NanoUI.Internal.Id (WidgetId (..), enterScope, hashWidgetId, scopeTag)
import NanoUI.Internal.Input
  ( Input (..)
  , inputMouseDown
  , inputMousePos
  , inputMouseReleased
  , inputMouseRightDown
  , inputMouseRightReleased
  )
import NanoUI.Internal.Layout.Arena
  ( NodeArena
  , NodeIdx
  , NodeType (..)
  , addNodeFromLayout
  , rootAttachParent
  , setNodeText
  , setNodeValue
  , setOptions
  , setStyleIdx
  , setWidgetId
  )
import NanoUI.Internal.Monad (Ui, (<&&>), askContext, askFrameInput, askInput, localInput, nextId, uiIO, withContext)
import NanoUI.Internal.WidgetText (packTextNodeStyle)
import NanoUI.Internal.Style (Layout (..))
import NanoUI.Internal.Types (Rect (..), rectContains, rectH, rectHit, rectUnion, rectW)
import NanoUI.Internal.Frame.Hit (findNodeByWidgetId, nodeInteractionHit, scrollHitRect)

-- | The innermost open container, or @-1@ at the root.
currentParent :: Context -> IO Int
currentParent ctx =
  readIORef (ctxContainerStack ctx) <&> \case
    [] -> -1
    (p : _) -> p

-- | Anything that carries a widget 'Response' (composite widget results such
-- as 'NanoUI.Internal.Widgets.Tabs.TabResponse'). The @resp*@ accessors work on all of them.
class HasResponse r where
  toResponse :: r -> Response

instance HasResponse Response where
  {-# INLINE toResponse #-}
  toResponse = id

-- | Widget identity used for state, tooltips, and commands.
{-# INLINE respId #-}
respId :: HasResponse r => r -> WidgetId
respId = rawRespId . toResponse

-- | Widget bounds in logical window coordinates. During view construction,
-- these come from the previous frame and can be empty on first appearance.
{-# INLINE respRect #-}
respRect :: HasResponse r => r -> Rect
respRect = rawRespRect . toResponse

-- | Whether the routed pointer is over the widget's visible hit area.
{-# INLINE respHovered #-}
respHovered :: HasResponse r => r -> Bool
respHovered = rawRespHovered . toResponse

-- | Whether the left button is held over the widget. This is a held state,
-- not a one-frame button-down event.
{-# INLINE respPressed #-}
respPressed :: HasResponse r => r -> Bool
respPressed = rawRespPressed . toResponse

-- | Whether the widget reports activation on this frame.
{-# INLINE respClicked #-}
respClicked :: HasResponse r => r -> Bool
respClicked = rawRespClicked . toResponse

-- | Whether the widget reports a value change. Search fields debounce this flag.
{-# INLINE respChanged #-}
respChanged :: HasResponse r => r -> Bool
respChanged = rawRespChanged . toResponse

-- | Whether the widget reports a commit, such as Enter in a text field.
{-# INLINE respSubmitted #-}
respSubmitted :: HasResponse r => r -> Bool
respSubmitted = rawRespSubmitted . toResponse

-- | Whether the right button is held over the widget.
{-# INLINE respRightPressed #-}
respRightPressed :: HasResponse r => r -> Bool
respRightPressed = rawRespRightPressed . toResponse

-- | Whether a right-button click completed on the widget this frame.
{-# INLINE respRightClicked #-}
respRightClicked :: HasResponse r => r -> Bool
respRightClicked = rawRespRightClicked . toResponse

-- | Per-frame widget identity, bounds, and interaction flags. Primed widget
-- variants expose this alongside their value. Combining responses unions
-- bounds, ORs flags, and keeps the last nonzero id.
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
  Response i1 r1 h1 p1 c1 ch1 s1 rp1 rc1 <> Response i2 r2 h2 p2 c2 ch2 s2 rp2 rc2 =
    Response
      (if i2 == WidgetId 0 then i1 else i2)
      (unionRespRect r1 r2)
      (h1 || h2) (p1 || p2) (c1 || c2) (ch1 || ch2) (s1 || s2) (rp1 || rp2) (rc1 || rc2)

instance Monoid Response where
  mempty = mkResponse (WidgetId 0) (Rect 0 0 0 0) False False False

unionRespRect :: Rect -> Rect -> Rect
unionRespRect a b
  | rectW a <= 0 || rectH a <= 0 = b
  | rectW b <= 0 || rectH b <= 0 = a
  | otherwise = rectUnion a b

-- | Replace only the activation flag, for composite widgets.
setClicked :: Bool -> Response -> Response
setClicked c r = r {rawRespClicked = c}

-- | Replace only the value-change flag.
setChanged :: Bool -> Response -> Response
setChanged c r = r {rawRespChanged = c}

-- | Replace only the commit flag.
setSubmitted :: Bool -> Response -> Response
setSubmitted s r = r {rawRespSubmitted = s}

-- | The response of a control that takes no input: its id and rect, and no
-- interaction.
inertResponse :: Response -> Response
inertResponse r =
  r {rawRespHovered = False, rawRespPressed = False, rawRespClicked = False, rawRespRightPressed = False, rawRespRightClicked = False}

-- | An unpressed response with the given hover, click, and change flags.
mkResponse :: WidgetId -> Rect -> Bool -> Bool -> Bool -> Response
mkResponse wid rect hovered clicked changed =
  Response wid rect hovered False clicked changed False False False

container :: Ui :> es => NodeType -> Layout -> Eff es a -> Eff es a
container nt layout child = do
  ctx <- askContext
  idx <- uiIO $ do
    parent <- currentParent ctx
    addNodeFromLayout (ctxNodeArena ctx) nt parent layout
  withContainerNode True idx child

-- | A 'container' tagged with a fresh id, and its interaction under that id.
containerResponse :: Ui :> es => NodeType -> Layout -> Eff es a -> Eff es (a, Response)
containerResponse nt layout child = do
  wid <- nextId
  inp <- askInput
  r <- container nt layout (tagContainer wid >> child)
  resp <- withContext (\ctx -> resolveInteraction ctx inp wid)
  pure (r, resp)

-- | Push container node @idx@ (already added under the current parent), run
-- @child@ inside it, then pop. @scoped@ also runs the children in a fresh id
-- scope; it changes the children's widget ids (and so their store keys), so
-- callers pick it explicitly: plain containers scope, scroll containers do not.
withContainerNode :: Ui :> es => Bool -> NodeIdx -> Eff es a -> Eff es a
withContainerNode scoped idx child = do
  ctx <- askContext
  (stack, parentIds) <- uiIO $ do
    stack <- readIORef (ctxContainerStack ctx)
    writeIORef (ctxContainerStack ctx) (idx : stack)
    ids <- readIORef (ctxIdContext ctx)
    let !(!parentIds, !childIds) = if scoped then enterScope scopeTag ids else (ids, ids)
    writeIORef (ctxIdContext ctx) childIds
    pure (stack, parentIds)
  r <- child
  uiIO $ do
    writeIORef (ctxContainerStack ctx) stack
    when scoped $ writeIORef (ctxIdContext ctx) parentIds
  pure r

-- | A floating panel (popup, modal, window): its node attaches to the root
-- layer, and @body@ runs as a layer of its own, with the pointer when the
-- panel is what the frame routed it to. @addPanel@ adds the node under the
-- given parent; @enter@ runs once the node is pushed (seeding its rect,
-- opening a modal). The body runs in a fresh id scope.
floatingPanel ::
  Ui :> es => WidgetId -> (Int -> IO NodeIdx) -> IO () -> Eff es a -> Eff es a
floatingPanel wid addPanel enter body = do
  ctx <- askContext
  let arena = ctxNodeArena ctx
  idx <- uiIO $ do
    idx <- addPanel =<< rootAttachParent arena =<< currentParent ctx
    setWidgetId arena idx wid
    pure idx
  withContainerNode True idx $ do
    -- A modal has to be entered first: that is what lets its own body through.
    uiIO enter
    frame <- askFrameInput
    inp <- uiIO (routedInput ctx (intKey wid) frame)
    localInput inp body

-- | The input for widget @wid@'s own dropdown, which the frame draws over
-- every layer: the frame's while the pointer is routed to that dropdown, and
-- the view's otherwise.
dropdownInput :: Ui :> es => WidgetId -> Eff es Input
dropdownInput wid =
  withContext (\ctx -> getsInteraction ctx isPointerRoute) >>= \case
    RouteDropdown owner | owner == wid -> askFrameInput
    _ -> askInput

{-# INLINE addWidget #-}
addWidget ::
  Ui :> es =>
  WidgetId
  -> NodeType
  -> Text
  -> Float
  -> Layout
  -> Eff es Response
addWidget wid nt txt value layout = addWidgetStyled wid nt txt value layout 0

{-# INLINE addWidgetStyled #-}
addWidgetStyled ::
  Ui :> es =>
  WidgetId
  -> NodeType
  -> Text
  -> Float
  -> Layout
  -> Int
  -> Eff es Response
addWidgetStyled wid nt txt value layout styleIdx =
  addWidgetNode wid nt txt value layout $ \arena idx ->
    setStyleIdx arena idx (if nt == NodeText then packTextNodeStyle layout styleIdx else styleIdx)

-- The initializer specializes at each call site; the node allocation, identity
-- and interaction path are shared by styled leaves and option controls.
{-# INLINE addWidgetNode #-}
addWidgetNode ::
  Ui :> es =>
  WidgetId
  -> NodeType
  -> Text
  -> Float
  -> Layout
  -> (NodeArena -> NodeIdx -> IO ())
  -> Eff es Response
addWidgetNode wid nt txt value layout initialize = do
  ctx <- askContext
  inp <- askInput
  uiIO $ do
    parent <- currentParent ctx
    idx <- addNodeFromLayout (ctxNodeArena ctx) nt parent layout
    setNodeText (ctxNodeArena ctx) idx txt
    setNodeValue (ctxNodeArena ctx) idx value
    initialize (ctxNodeArena ctx) idx
    setWidgetId (ctxNodeArena ctx) idx wid
    resolveInteraction ctx inp wid

addWidgetWithOptions ::
  Ui :> es =>
  WidgetId
  -> NodeType
  -> Text
  -> [Text]
  -> Float
  -> Layout
  -> Eff es Response
addWidgetWithOptions wid nt txt opts value layout =
  addWidgetNode wid nt txt value layout $ \arena idx -> do
    setOptions arena idx opts
    setStyleIdx arena idx 0

resolveInteraction :: Context -> Input -> WidgetId -> IO Response
resolveInteraction ctx inp wid = do
  mrect <- scrollHitRect ctx wid
  active <- readIORef (ctxActiveId ctx)
  pending <- readIORef (ctxClickedId ctx)
  let
    mouse = inputMousePos inp
    rect = fromMaybe (Rect 0 0 0 0) mrect
  if not (rectHit rect mouse || pending == wid)
    then pure $! mkResponse wid rect False False False
    else do
      disabled <- isDisabled ctx wid
      mIdx <- findNodeByWidgetId ctx wid
      let
        hitAt p = case mIdx of
          Nothing -> pure (rectContains rect p)
          Just idx -> nodeInteractionHit ctx idx rect p
        -- Whether the button held in @ref@ went down on this widget. A press
        -- the frame never saw (synthesized input, or one swallowed before it
        -- arrived) leaves the gesture unowned, so nobody is ruled out.
        startedHere ref = readIORef ref >>= maybe (pure True) hitAt
      -- A held button belongs to whatever it went down on. Another widget the
      -- drag passes over is not hovered, so it neither lights up nor reports a
      -- press of its own.
      captured <-
        pure (inputMouseDown inp)
          <&&> if hashWidgetId active /= 0 && active /= wid
            then pure True
            else not <$> startedHere (ctxPressPos ctx)
      hovered <- pure (not (disabled || captured)) <&&> hitAt mouse
      let
        pressed = hovered && inputMouseDown inp
        rightPressed = hovered && inputMouseRightDown inp
      -- The click belongs to whatever the press went down on: a release that
      -- drifted here from a neighbouring widget is not this widget's click.
      released <- pure (hovered && inputMouseReleased inp) <&&> startedHere (ctxPressPos ctx)
      rightClicked <-
        pure (hovered && inputMouseRightReleased inp) <&&> startedHere (ctxRightPressPos ctx)
      when (released && wid == active) $
        writeIORef (ctxReleaseClickedId ctx) wid
      pure $! Response wid rect hovered pressed (released || pending == wid) False False rightPressed rightClicked

-- | Stamp the current container with a widget id (a radio or tree group key),
-- so store keys and damage requests under that id resolve to the container.
-- Containers are never hot, so the id does not make it hoverable.
tagContainer :: Ui :> es => WidgetId -> Eff es ()
tagContainer wid = do
  ctx <- askContext
  uiIO $ do
    parent <- currentParent ctx
    when (parent >= 0) $ setWidgetId (ctxNodeArena ctx) parent wid
