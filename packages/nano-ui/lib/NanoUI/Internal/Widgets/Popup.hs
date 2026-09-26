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
  -- ^ Seconds the pointer must rest on the target before the tooltip
  -- opens; 0 opens it at once. Moving within the target does not restart it.
  , tooltipGrace :: !Double
  -- ^ Seconds after the last tooltip closed during which this one skips its
  -- delay, so the pointer can sweep along a toolbar reading each tooltip.
  , tooltipPlacement :: !PopupPlacement
  -- ^ The side of the target to open on. 'PlacementAtCursor' instead puts
  -- it below the pointer (above if there is no room) and follows it.
  , tooltipGap :: !Float
  -- ^ Logical pixels between the tooltip and its target or the pointer.
  }
  deriving (Eq, Show)

-- | 4 pixels below the target, after a 0.5 s rest, or at once within 0.3 s
-- of another tooltip.
defaultTooltipConfig :: TooltipConfig
defaultTooltipConfig =
  TooltipConfig
    { tooltipDelay = 0.5
    , tooltipGrace = 0.3
    , tooltipPlacement = PlacementBelow
    , tooltipGap = 4
    }

-- | Attach a rich tooltip widget to any target response, shown once the
-- pointer rests on it ('defaultTooltipConfig').
tooltipWidget ::
  (Ui :> es, HasResponse r) =>
  r ->
  Eff es a ->
  Eff es (Maybe a)
tooltipWidget = tooltipWidgetConfigured defaultTooltipConfig

-- | 'tooltipWidget' with a configured delay, placement and gap. The target
-- counts as hovered when the routed pointer is on its visible, uncovered
-- part ('pointerOnWidget'), even if it takes no input, so a disabled button
-- can still explain why it is off.
tooltipWidgetConfigured ::
  (Ui :> es, HasResponse r) =>
  TooltipConfig ->
  r ->
  Eff es a ->
  Eff es (Maybe a)
tooltipWidgetConfigured cfg target child = do
  -- The id 'popup' takes next; the timer is keyed on it to avoid using a
  -- sibling id.
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
  -- The tooltip covering the target still counts as hovering it, as when a
  -- pointer-following tooltip lags one frame behind the pointer.
  onTip <- uiIO ((== RouteLayer (intKey wid)) <$> getsInteraction ctx isPointerRoute)
  let hovered = onTarget || (onTip && rectHit rect mouse)
  open <- uiIO (tooltipTimer ctx cfg (intKey wid) hovered frame)
  -- Request a frame when the pointer enters or leaves the target, even a
  -- label or container, and on every move while a following tooltip is open.
  uiIO (registerHoverZone ctx (open && follow) rect)
  let (anchor, placement)
        | follow = (AnchorRect (Rect mx my 0 pointerClearance), PlacementBelow)
        | otherwise = (AnchorRect rect, tooltipPlacement cfg)
  snd <$> popup open ((defaultPopupConfig anchor) {cfgPlacement = placement, cfgDismissable = False, cfgOffset = tooltipGap cfg}) child

-- | Offset below the pointer's hot spot for a following tooltip, before the
-- gap: roughly an arrow cursor's height, so the cursor does not cover it.
pointerClearance :: Float
pointerClearance = 16

-- | Whether the tooltip with store key @k@ is open this frame. The delay
-- starts when the target becomes @hovered@ and ends after 'tooltipDelay', or
-- at once within 'tooltipGrace' of the last tooltip closing. A held button
-- keeps the tooltip shut and restarts the delay on release; a wheel turn
-- restarts it. Both also end the grace period.
--
-- The times live in quiet store slots, which neither damage nor wake.
-- Opening and closing repaint as a floating panel, and the opening frame is
-- scheduled with 'requestWakeAt', so an idle app draws nothing while waiting.
tooltipTimer :: Context -> TooltipConfig -> Int -> Bool -> Input -> IO Bool
tooltipTimer ctx cfg k hovered inp = do
  store <- getStore ctx
  let showK = slotKey SlotTooltipShow k
      lastK = slotKey SlotTooltipShow 0
      showAt0 = findSlot fieldQuiet 0 showK store
      lastUp0 = findSlot fieldQuiet 0 lastK store
      held = inputPointerHeld inp
      interrupted = anyButtonPressed inp || inputScroll inp /= V2 0 0
  -- Fast path: most targets are not near the pointer.
  if not hovered && showAt0 == 0 && (lastUp0 == 0 || not interrupted)
    then pure False
    else do
      now <- getMonotonicTime
      let micros t = round (t * 1e6) :: Int
          nowUs = micros now
          delayUs = micros (max 0 (tooltipDelay cfg))
          graceUs = micros (tooltipGrace cfg)
          -- No frames run while a tooltip sits open, so the closing frame
          -- counts as the last one it was up.
          wasUp = showAt0 > 0 && nowUs >= showAt0
          starting = hovered && not held && not interrupted && showAt0 == 0
          -- A tooltip open last frame whose call comes after this one has
          -- not yet recorded closing.
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

-- | 'tooltip' with a configured delay and placement.
--
-- > tooltipConfigured defaultTooltipConfig {tooltipPlacement = PlacementAtCursor} swatch name
tooltipConfigured ::
  (Ui :> es, HasResponse r) =>
  TooltipConfig ->
  r ->
  Text ->
  Eff es ()
tooltipConfigured cfg target txt = void (tooltipWidgetConfigured cfg target (label txt))

-- | Text shown below a widget after the pointer rests on it for half a
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
