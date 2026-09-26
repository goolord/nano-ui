-- | Anchored floating panels and tooltips, with caller-owned visibility state.
module NanoUI.Internal.Widgets.Popup
  ( PopupAnchor (..)
  , PopupPlacement (..)
  , PopupConfig (..)
  , defaultPopupConfig
  , popup
  , popupWith
  , floatingOverlay
  , tooltipWidget
  , tooltipAt
  , tooltip
  , withTooltip
  , TooltipConfig (..)
  , defaultTooltipConfig
  , tooltipConfigured
  , tooltipWidgetConfigured
  )
where

import Control.Monad (void, when)
import Data.IORef (modifyIORef')
import Data.IntMap.Strict qualified as IM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Effectful (Eff, type (:>))
import GHC.Clock (getMonotonicTime)
import NanoUI.Internal.Context
import NanoUI.Internal.Id (WidgetId, enterScope, scopeTag)
import NanoUI.Internal.Input
import NanoUI.Internal.Frame.Hit (findNodeByWidgetId)
import NanoUI.Internal.Layout.Arena (NodeIdx, NodeType (..), addNodeFromLayout)
import NanoUI.Internal.Monad
import NanoUI.Internal.Store (deleteSlot, fieldQuiet, findSlot, insertSlot)
import NanoUI.Internal.Style
import NanoUI.Internal.Types
import NanoUI.Internal.Widgets.Behavior (useDismissable)
import NanoUI.Internal.Widgets.Layout (label)
import NanoUI.Internal.Widgets.Node

-- | Anchor, preferred side, outside-click dismissal, and gap in logical pixels.
-- Anchors use logical window coordinates.
data PopupConfig = PopupConfig
  { cfgAnchor :: !PopupAnchor
  , cfgPlacement :: !PopupPlacement
  , cfgDismissable :: !Bool
  , cfgOffset :: !Float
  }
  deriving (Eq, Show)

-- | Automatic placement with a 4-pixel gap and outside-click dismissal enabled.
defaultPopupConfig :: PopupAnchor -> PopupConfig
defaultPopupConfig anchor =
  PopupConfig
    { cfgAnchor = anchor
    , cfgPlacement = PlacementAuto
    , cfgDismissable = True
    , cfgOffset = 4
    }

-- | A floating panel placed by the config, shown while @open@. Returns the
-- body's result while open. The 'Response' reports a dismissal (Escape, or a
-- click outside when 'cfgDismissable') as a click.
popup ::
  Ui :> es =>
  Bool ->
  PopupConfig ->
  Eff es a ->
  Eff es (Response, Maybe a)
popup open cfg child = popupWith open cfg id child

-- | 'popup' with a modifier applied to its tight default layout.
popupWith ::
  Ui :> es =>
  Bool ->
  PopupConfig ->
  (Layout -> Layout) ->
  Eff es a ->
  Eff es (Response, Maybe a)
popupWith open cfg f child = do
  ctx <- askContext
  let
    layout = f (tight defaultLayout)
    addPopupNode wid parent = do
      registerPopupConfig ctx wid (cfgAnchor cfg) (cfgPlacement cfg) (cfgOffset cfg)
      addNodeFromLayout
        (ctxNodeArena ctx)
        NodePopup
        parent
        layout {layoutPadding = Padding 6 6 6 6, layoutGap = 4}
    seedFromPrev wid = getPrevRect ctx wid >>= mapM_ (seedFloatingPanel ctx wid)
  floatingOverlay open (cfgDismissable cfg) addPopupNode seedFromPrev ((,) False <$> child)

-- | The floating panel behind popups, modals and windows, shown while @open@
-- with its body in its own id scope. @addPanel@ and @enter@ are those of
-- 'floatingPanel', given the panel's id. The body returns whether it closed
-- the panel along with its result. The 'Response' reports a dismissal (the
-- body's close, Escape, or a click outside when @dismissable@) as a click. A
-- closed panel still consumes its id scope, so the ids of later siblings do
-- not shift when it opens.
floatingOverlay ::
  Ui :> es =>
  Bool ->
  Bool ->
  (WidgetId -> Int -> IO NodeIdx) ->
  (WidgetId -> IO ()) ->
  Eff es (Bool, a) ->
  Eff es (Response, Maybe a)
floatingOverlay open dismissable addPanel enter body = do
  (wid, ctx) <- freshWidget
  if not open
    then do
      uiIO (modifyIORef' (ctxIdContext ctx) (fst . enterScope scopeTag))
      pure (mempty {rawRespId = wid}, Nothing)
    else do
      -- Hovered is the pointer on the panel as the panel's own layer sees it,
      -- so whatever is in front of the panel takes the hover with it.
      (mouse, (closed, r)) <-
        floatingPanel wid (addPanel wid) (enter wid) ((,) <$> uiMousePos <*> body)
      panel <- fromMaybe (Rect 0 0 0 0) <$> lastRect wid
      outside <- pure (dismissable && rectNonEmpty panel) <&&> useDismissable panel
      let dismissed = closed || outside
      pure
        ( mkResponse wid panel (rectHit panel mouse) dismissed dismissed
        , Just r
        )

-- | When a tooltip opens, and where.
data TooltipConfig = TooltipConfig
  { tooltipDelay :: !Double
  -- ^ Seconds the pointer rests on the target before the tooltip opens; 0
  -- opens it at once. Moving over the target does not restart the wait.
  , tooltipGrace :: !Double
  -- ^ Seconds after a tooltip was last up in which this one opens without
  -- its delay, so the pointer can run along a toolbar reading each tooltip.
  , tooltipPlacement :: !PopupPlacement
  -- ^ The side of the target the tooltip opens on. 'PlacementAtCursor' puts
  -- it below the pointer instead, above it when there is no room, and moves
  -- it with the pointer.
  , tooltipGap :: !Float
  -- ^ Logical pixels between the tooltip and its target, or the pointer it
  -- follows.
  }
  deriving (Eq, Show)

-- | Below the target, 4 pixels from it, after the pointer has rested on it
-- for half a second, or at once within 0.3 seconds of another tooltip.
defaultTooltipConfig :: TooltipConfig
defaultTooltipConfig =
  TooltipConfig
    { tooltipDelay = 0.5
    , tooltipGrace = 0.3
    , tooltipPlacement = PlacementBelow
    , tooltipGap = 4
    }

-- | Attach a rich tooltip widget to any target response, displayed once the
-- pointer has rested on it ('defaultTooltipConfig').
tooltipWidget ::
  (Ui :> es, HasResponse r) =>
  r ->
  Eff es a ->
  Eff es (Maybe a)
tooltipWidget = tooltipWidgetConfigured defaultTooltipConfig

-- | 'tooltipWidget' with its delay, placement and gap. The pointer is on the
-- target where the view's routed pointer is on its visible part with nothing
-- drawn over it ('pointerOnWidget'), whether or not the target takes input:
-- a disabled button has its tooltip, to say why it is off.
tooltipWidgetConfigured ::
  (Ui :> es, HasResponse r) =>
  TooltipConfig ->
  r ->
  Eff es a ->
  Eff es (Maybe a)
tooltipWidgetConfigured cfg target child = do
  -- The id 'popup' takes next. The timer is kept under it, so it costs no
  -- sibling id of its own.
  wid <- currentId
  ctx <- askContext
  frame <- askFrameInput
  routed <- inputMousePos <$> askInput
  let mouse@(V2 mx my) = inputMousePos frame
      follow = tooltipPlacement cfg == PlacementAtCursor
      rect = respRect target
      tid = respId target
  onTarget <-
    if rectHit rect routed
      then uiIO (findNodeByWidgetId ctx tid >>= \mIdx -> pointerOnWidget ctx mIdx tid rect routed)
      else pure False
  -- The pointer is still on the target while the tooltip itself is in front
  -- of it there, as one following the pointer is for the frame before it
  -- moves out from under it.
  onTip <- uiIO ((== RouteLayer (intKey wid)) <$> getsInteraction ctx isPointerRoute)
  let hovered = onTarget || (onTip && rectHit rect mouse)
  open <- uiIO (tooltipTimer ctx cfg (intKey wid) hovered frame)
  -- The pointer coming onto the target starts the wait, and leaving it shuts
  -- the tooltip, even where the target is a label or a container; while one
  -- that follows the pointer is up, every move over the target moves it.
  uiIO (registerHoverZone ctx (open && follow) rect)
  let (anchor, placement)
        | follow = (AnchorRect (Rect mx my 0 pointerClearance), PlacementBelow)
        | otherwise = (AnchorRect rect, tooltipPlacement cfg)
  snd <$> popup open ((defaultPopupConfig anchor) {cfgPlacement = placement, cfgDismissable = False, cfgOffset = tooltipGap cfg}) child

-- | How far below the pointer's hot spot a tooltip following the pointer
-- starts, before the popup gap: an arrow pointer's height, so the pointer
-- does not cover the tooltip.
pointerClearance :: Float
pointerClearance = 16

-- | Whether the tooltip with store key @k@ is open this frame, given whether
-- its target is @hovered@. The wait starts when the pointer comes onto the
-- target and runs out 'tooltipDelay' later, or at once within 'tooltipGrace'
-- of the last tooltip going away. A button held down keeps the tooltip shut,
-- and letting it go starts the wait again; a wheel turn restarts it. Either
-- one also ends the grace period. The times live in the bookkeeping slots,
-- which neither damage nor wake: a tooltip opening or closing repaints as a
-- floating panel, and the frame it opens on is asked for with
-- 'requestWakeAt', so an app waiting on one draws nothing until then.
tooltipTimer :: Context -> TooltipConfig -> Int -> Bool -> Input -> IO Bool
tooltipTimer ctx cfg k hovered inp = do
  store <- getStore ctx
  let showK = slotKey SlotTooltipShow k
      lastK = slotKey SlotTooltipShow 0
      showAt0 = findSlot fieldQuiet 0 showK store
      lastUp0 = findSlot fieldQuiet 0 lastK store
      held = inputPointerHeld inp
      interrupted = anyButtonPressed inp || inputScroll inp /= V2 0 0
  -- Most tooltips belong to targets the pointer is nowhere near.
  if not hovered && showAt0 == 0 && (lastUp0 == 0 || not interrupted)
    then pure False
    else do
      now <- getMonotonicTime
      let micros t = round (t * 1e6) :: Int
          nowUs = micros now
          delayUs = micros (max 0 (tooltipDelay cfg))
          graceUs = micros (tooltipGrace cfg)
          -- Nothing draws while a tooltip rests open, so the frame it goes
          -- away on is the last it was up.
          wasUp = showAt0 > 0 && nowUs >= showAt0
          starting = hovered && not held && not interrupted && showAt0 == 0
          -- One up in the frame before whose call comes after this one has
          -- not recorded going away yet.
          otherUp = any (\k' -> k' /= k && findSlot fieldQuiet 0 (slotKey SlotTooltipShow k') store > 0) . IM.keys
      warm <-
        if not starting || graceUs <= 0 || lastUp0 == 0
          then pure False
          else (nowUs - lastUp0 < graceUs ||) . otherUp <$> getsOverlay ctx osPrevFloatingRects
      let showAt
            | not hovered || held = 0
            | interrupted = nowUs + delayUs
            | showAt0 > 0 = showAt0
            | warm = nowUs
            | otherwise = nowUs + delayUs
          open = showAt > 0 && nowUs >= showAt
          lastUp
            | interrupted = 0
            | open || wasUp = nowUs
            | otherwise = lastUp0
          set key v = if v > 0 then insertSlot fieldQuiet key v else deleteSlot fieldQuiet key
      when (showAt /= showAt0 || lastUp /= lastUp0) $
        modifyStore ctx (set showK showAt . set lastK lastUp)
      when (showAt > 0 && not open) $
        requestWakeAt ctx (fromIntegral showAt / 1e6)
      pure open

-- | Attach a rich tooltip widget to an inner UI computation.
withTooltip ::
  Ui :> es =>
  Eff es a ->
  Eff es b ->
  Eff es (a, Maybe b)
withTooltip mainChild tipChild = do
  base <- askDefaultLayout
  (res, contResp) <- containerResponse NodeContainer (tight base) mainChild
  mTip <- tooltipWidget contResp tipChild
  pure (res, mTip)

-- | 'tooltip' with a placement. 'PlacementAtCursor' follows the pointer.
tooltipAt ::
  (Ui :> es, HasResponse r) =>
  PopupPlacement ->
  r ->
  Text ->
  Eff es ()
tooltipAt placement = tooltipConfigured defaultTooltipConfig {tooltipPlacement = placement}

-- | 'tooltip' with its delay and placement.
--
-- > tooltipConfigured defaultTooltipConfig {tooltipPlacement = PlacementAtCursor} swatch name
tooltipConfigured ::
  (Ui :> es, HasResponse r) =>
  TooltipConfig ->
  r ->
  Text ->
  Eff es ()
tooltipConfigured cfg target txt = void (tooltipWidgetConfigured cfg target (label txt))

-- | Text shown below a widget once the pointer has rested on it for half a
-- second, until it leaves or a button is pressed ('defaultTooltipConfig').
--
-- > save <- button' "Save"
-- > tooltip save "Write the file to disk"
tooltip ::
  (Ui :> es, HasResponse r) =>
  r ->
  Text ->
  Eff es ()
tooltip = tooltipConfigured defaultTooltipConfig
