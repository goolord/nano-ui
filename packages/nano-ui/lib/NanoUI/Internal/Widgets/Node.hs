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
  , respHeldWith
  , respClickedWith
  , respRightPressed
  , respRightClicked
  , pointerOnWidget
  , mkResponse
  , setClicked
  , setChanged
  , setSubmitted
  , inertResponse
  , currentParent
  , container
  , containerResponse
  , mouseArea
  , inertContainer
  , withContainerNode
  , withWidgetChildren
  , floatingPanel
  , dropdownInput
  , addWidget
  , addWidgetStyled
  , addWidgetNode
  , addWidgetWithOptions
  , tagContainer
  , setWidgetValue
  , moveSelection
  )
where

import Control.Monad (forM, forM_, unless, when)
import Data.IORef (readIORef, writeIORef)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Effectful (Eff, type (:>))
import NanoUI.Internal.Context
import NanoUI.Internal.Id (IdContext (..), WidgetId (..), enterScope, hashWidgetId, mix64, scopeTag)
import NanoUI.Internal.Input
import NanoUI.Internal.Layout.Arena
import NanoUI.Internal.Monad (Ui, (<&&>), askContext, askDefaultLayout, askFrameInput, askInput, localInput, nextId, uiIO, withContext, withIdFrame)
import NanoUI.Internal.WidgetText (containerFlagInert, packTextNodeStyle)
import NanoUI.Internal.Style (Layout (..), tight)
import NanoUI.Internal.Types (Rect (..), V2, rectContains, rectH, rectHit, rectUnion, rectW)
import NanoUI.Internal.Frame.Hit (findNodeByWidgetId, nodeInteractionHit, passesPointer, withWidgetNode)

-- | The innermost open container, or @-1@ at the root.
currentParent :: Context -> IO Int
currentParent ctx = fromMaybe (-1) . listToMaybe <$> readIORef (ctxContainerStack ctx)

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

-- | Whether the left button went down on the widget and is held over it
-- ('respHeldWith' 'MouseLeft'). This is a held state, not a one-frame
-- button-down event.
{-# INLINE respPressed #-}
respPressed :: HasResponse r => r -> Bool
respPressed = respHeldWith MouseLeft

-- | Whether the widget reports activation on this frame: a left click on
-- it, or Enter or Space while it has the keyboard, whatever activates it.
-- 'respClickedWith' is a click of one button alone.
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

-- | Whether the button went down on the widget and is held over it. A
-- button held from elsewhere and dragged over the widget does not count.
--
-- > (_, area) <- mouseArea id (label "Pan me")
-- > when (respHeldWith MouseMiddle area) (pan =<< uiMousePos)
{-# INLINE respHeldWith #-}
respHeldWith :: HasResponse r => MouseButton -> r -> Bool
respHeldWith b = buttonsMember b . rawRespHeld . toResponse

-- | Whether a click of the button completed on the widget this frame: the
-- button went down on the widget and came up on it. A click of any button
-- but the left is only this; what it does is up to the view, such as a
-- middle click closing a tab. A left click is also 'respClicked'.
{-# INLINE respClickedWith #-}
respClickedWith :: HasResponse r => MouseButton -> r -> Bool
respClickedWith b = buttonsMember b . rawRespClickedWith . toResponse

-- | 'respHeldWith' 'MouseRight'.
{-# INLINE respRightPressed #-}
respRightPressed :: HasResponse r => r -> Bool
respRightPressed = respHeldWith MouseRight

-- | 'respClickedWith' 'MouseRight'.
{-# INLINE respRightClicked #-}
respRightClicked :: HasResponse r => r -> Bool
respRightClicked = respClickedWith MouseRight

-- | Per-frame widget identity, bounds, and interaction flags. Primed widget
-- variants expose this alongside their value. Combining responses unions
-- bounds, flags and buttons, and keeps the last nonzero id.
data Response = Response
  { rawRespId :: !WidgetId
  , rawRespRect :: !Rect
  , rawRespHovered :: !Bool
  , rawRespClicked :: !Bool
  , rawRespChanged :: !Bool
  , rawRespSubmitted :: !Bool
  , rawRespHeld :: {-# UNPACK #-} !MouseButtons
  -- ^ The buttons that went down on the widget and are held over it
  -- ('respHeldWith').
  , rawRespClickedWith :: {-# UNPACK #-} !MouseButtons
  -- ^ The buttons whose click completed on the widget this frame
  -- ('respClickedWith').
  }
  deriving (Eq, Show)

instance Semigroup Response where
  Response i1 r1 h1 c1 ch1 s1 held1 cw1 <> Response i2 r2 h2 c2 ch2 s2 held2 cw2 =
    Response
      (if i2 == WidgetId 0 then i1 else i2)
      (unionRespRect r1 r2)
      (h1 || h2) (c1 || c2) (ch1 || ch2) (s1 || s2) (held1 <> held2) (cw1 <> cw2)

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
  r {rawRespHovered = False, rawRespClicked = False, rawRespHeld = noButtons, rawRespClickedWith = noButtons}

-- | An unpressed response with the given hover, click, and change flags.
mkResponse :: WidgetId -> Rect -> Bool -> Bool -> Bool -> Response
mkResponse wid rect hovered clicked changed =
  Response wid rect hovered clicked changed False noButtons noButtons

container :: Ui :> es => NodeType -> Layout -> Eff es a -> Eff es a
container nt layout child = do
  ctx <- askContext
  idx <- uiIO $ do
    parent <- currentParent ctx
    addNodeFromLayout (ctxNodeArena ctx) nt parent layout
  withContainerNode True idx child

-- | A container whose widgets are for display: they see no pointer, and a
-- press passes through them to the widget they are drawn in
-- ('NanoUI.Internal.Frame.Hit.innermostHit').
inertContainer :: Ui :> es => Layout -> Eff es a -> Eff es a
inertContainer layout child = do
  ctx <- askContext
  inp <- askInput
  idx <- uiIO $ do
    parent <- currentParent ctx
    idx <- addNodeFromLayout (ctxNodeArena ctx) NodeContainer parent layout
    idx <$ setStyleIdx (ctxNodeArena ctx) idx containerFlagInert
  withContainerNode True idx (localInput (withoutPointer inp) child)

-- | A 'container' tagged with a fresh id, and its interaction under that id.
containerResponse :: Ui :> es => NodeType -> Layout -> Eff es a -> Eff es (a, Response)
containerResponse nt layout child = do
  wid <- nextId
  inp <- askInput
  r <- container nt layout (tagContainer wid >> child)
  resp <- withContext (\ctx -> resolveInteraction ctx inp wid)
  pure (r, resp)

-- | A column without padding, laid out by the modifier, that reports the
-- pointer over it and everything in it, as iced's @mouse_area@ does: whether
-- it is hovered ('respHovered'), which buttons went down on it and are held
-- ('respHeldWith') and which clicked it ('respClickedWith', and
-- 'respClicked' for the left one). A widget inside keeps what it takes for
-- itself, so a click on a button inside is the button's; any other button,
-- and a click beside the widgets, is the area's. Nothing inside covers the
-- area, so what the area shows while hovered stays shown while the pointer
-- is on it:
--
-- > (hovered, setHovered) <- useFlag False
-- > (_, item) <- mouseArea (fillW . gap 6) $ do
-- >   label name
-- >   when hovered (void (button "Delete"))
-- > setHovered (respHovered item)
-- > when (respClickedWith MouseMiddle item) (openInNewTab name)
mouseArea :: Ui :> es => (Layout -> Layout) -> Eff es a -> Eff es (a, Response)
mouseArea f body = do
  base <- askDefaultLayout
  containerResponse NodeContainer (f (tight base)) body

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

-- | Run @child@ inside the node of widget @wid@, just added, in the id scope
-- the widget's id opens, so its siblings' ids do not move; 'Nothing' when it
-- has no node.
withWidgetChildren :: Ui :> es => WidgetId -> Eff es a -> Eff es (Maybe a)
withWidgetChildren wid@(WidgetId w) child = do
  ctx <- askContext
  mIdx <- uiIO (lookupNodeByWidgetId (ctxNodeArena ctx) wid)
  forM mIdx $ \idx ->
    withContainerNode False idx (withIdFrame (\ids -> (ids, IdContext (mix64 w scopeTag) 0)) child)

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

-- | Whether the pointer at @mouse@ is on widget @wid@ laid out at @rect@
-- last frame, with @mIdx@ its node: on the part of it scrollers leave in
-- view ('nodeInteractionHit'), with nothing that takes the pointer drawn over
-- it there ('pointerCovered'), and the widget not letting the pointer
-- through ('passesPointer'). Whether the widget takes the pointer, being
-- enabled and not dragged over, is its hover's business.
pointerOnWidget :: Context -> Maybe NodeIdx -> WidgetId -> Rect -> V2 -> IO Bool
pointerOnWidget ctx mIdx wid rect mouse =
  (not <$> pointerCovered ctx wid)
    <&&> (not <$> maybe (pure False) (passesPointer (ctxNodeArena ctx)) mIdx)
    <&&> widgetHit ctx mIdx rect mouse

-- | Whether point @p@ is on the part of a widget laid out at @rect@ last
-- frame that the scrollers around its node @mIdx@ leave in view
-- ('nodeInteractionHit'); on @rect@ for a widget without a node.
widgetHit :: Context -> Maybe NodeIdx -> Rect -> V2 -> IO Bool
widgetHit ctx mIdx rect p = maybe (pure (rectContains rect p)) (\idx -> nodeInteractionHit ctx idx rect p) mIdx

resolveInteraction :: Context -> Input -> WidgetId -> IO Response
resolveInteraction ctx inp wid = do
  mrect <- getPrevRect ctx wid
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
      presses <- readIORef (ctxPressPos ctx)
      let
        -- Whether the button went down on this widget. A press the frame
        -- never saw (synthesized input, or one swallowed before it arrived)
        -- leaves the gesture unowned, so nobody is ruled out.
        startedHere b = maybe (pure True) (widgetHit ctx mIdx rect) (M.lookup b presses)
      -- A held left button belongs to whatever it went down on. Another
      -- widget the drag passes over is not hovered, so it neither lights up
      -- nor reports a press of its own.
      captured <-
        pure (buttonHeld MouseLeft inp)
          <&&> if hashWidgetId active /= 0 && active /= wid
            then pure True
            else not <$> startedHere MouseLeft
      -- Where layers or a pinned node draw something that takes the
      -- pointer over this one, the pointer is that one's; and a node that
      -- lets the pointer through ('PointerPass') takes none of it.
      hovered <- pure (not (disabled || captured)) <&&> pointerOnWidget ctx mIdx wid rect mouse
      -- Every other button, held or released, is the widget's only when it
      -- went down on it; a hovered widget owns a held left button already.
      let ownedHere bs = if hovered then buttonsFilterM startedHere bs else pure noButtons
          leftHeld = if hovered && buttonHeld MouseLeft inp then buttonsInsert MouseLeft noButtons else noButtons
      held <- (leftHeld <>) <$> ownedHere (buttonsDelete MouseLeft (inputButtonsHeld inp))
      released <- ownedHere (inputButtonsReleased inp)
      -- The click belongs to whatever the press went down on: a release that
      -- drifted here from a neighbouring widget is not this widget's click,
      -- nor is one on a widget that moved under the pointer after another
      -- widget took the press. A press and release in one frame have set no
      -- active widget yet.
      let ownsRelease = hashWidgetId active == 0 || active == wid || buttonPressed MouseLeft inp
          clickedWith = if ownsRelease then released else buttonsDelete MouseLeft released
          leftClicked = buttonsMember MouseLeft clickedWith
      when (leftClicked && wid == active) $
        writeIORef (ctxReleaseClickedId ctx) wid
      let clicked = leftClicked || pending == wid
      pure $! Response wid rect hovered clicked False False held clickedWith

-- | Set the value of widget @wid@'s node, added this frame: a checkbox
-- showing the click read after it was added.
setWidgetValue :: Context -> WidgetId -> Float -> IO ()
setWidgetValue ctx wid v = withWidgetNode ctx wid () (\i -> setNodeValue (ctxNodeArena ctx) i v)

-- | Move a group's selection, which its members' values mark, from member
-- @old@ to member @new@. The members were added with @old@ selected, since the
-- click or key that moves it is read after them.
moveSelection :: Eq k => Context -> k -> k -> [(k, Response)] -> IO ()
moveSelection ctx old new members =
  unless (old == new) $ forM_ members $ \(k, r) ->
    when (k == old || k == new) $ setWidgetValue ctx (rawRespId r) (if k == new then 1 else 0)

-- | Stamp the current container with a widget id (a radio or tree group key),
-- so store keys and damage requests under that id resolve to the container.
-- Containers are never hot, so the id does not make it hoverable.
tagContainer :: Ui :> es => WidgetId -> Eff es ()
tagContainer wid = do
  ctx <- askContext
  uiIO $ do
    parent <- currentParent ctx
    when (parent >= 0) $ setWidgetId (ctxNodeArena ctx) parent wid
