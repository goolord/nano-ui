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
  )
where

import Control.Monad (void)
import Data.IORef (modifyIORef')
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Effectful (Eff, type (:>))
import NanoUI.Internal.Context
  ( Context (..)
  , getPrevRect
  , registerPopupConfig
  , seedFloatingPanel
  )
import NanoUI.Internal.Id (WidgetId, enterScope, scopeTag)
import NanoUI.Internal.Layout.Arena (NodeIdx, NodeType (..), addNode)
import NanoUI.Internal.Monad
  ( Ui
  , askContext
  , askDefaultLayout
  , lastRect
  , nextId
  , uiIO
  , uiMousePos
  )
import NanoUI.Internal.Style
  ( AlignX (..)
  , AlignY (..)
  , Layout (..)
  , Padding (..)
  , defaultLayout
  , tight
  )
import NanoUI.Internal.Types
  ( PopupAnchor (..)
  , PopupPlacement (..)
  , Rect (..)
  , rectHit
  , rectNonEmpty
  )
import NanoUI.Internal.Widgets.Behavior (useDismissable)
import NanoUI.Internal.Widgets.Layout (label)
import NanoUI.Internal.Widgets.Node
  ( HasResponse
  , Response (..)
  , containerResponse
  , emptyModalResp
  , floatingPanel
  , mkResponse
  , respHovered
  , respRect
  )

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
      addNode
        (ctxNodeArena ctx)
        NodePopup
        parent
        (layoutDirection layout)
        (layoutWidth layout)
        (layoutHeight layout)
        (Padding 6 6 6 6)
        4
        0
        0
        1e9
        1e9
        0
        AlignStart
        AlignTop
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
  wid <- nextId
  ctx <- askContext
  if not open
    then do
      uiIO (modifyIORef' (ctxIdContext ctx) (fst . enterScope scopeTag))
      pure (emptyModalResp wid, Nothing)
    else do
      -- Hovered is the pointer on the panel as the panel's own layer sees it,
      -- so whatever is in front of the panel takes the hover with it.
      (mouse, (closed, r)) <-
        floatingPanel True wid (addPanel wid) (enter wid) ((,) <$> uiMousePos <*> body)
      panel <- fromMaybe (Rect 0 0 0 0) <$> lastRect wid
      outside <-
        if dismissable && rectNonEmpty panel
          then useDismissable panel
          else pure False
      let dismissed = closed || outside
      pure
        ( mkResponse wid panel (rectHit panel mouse) False dismissed dismissed
        , Just r
        )

-- | Attach a rich tooltip widget to any target response, displayed on hover.
tooltipWidget ::
  (Ui :> es, HasResponse r) =>
  r ->
  Eff es a ->
  Eff es (Maybe a)
tooltipWidget target child =
  snd <$> popup (respHovered target) cfg child
  where
    cfg = (defaultPopupConfig (AnchorRect (respRect target))) {cfgPlacement = PlacementBelow, cfgDismissable = False}

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

-- | 'tooltip' with a placement.
tooltipAt ::
  (Ui :> es, HasResponse r) =>
  PopupPlacement ->
  r ->
  Text ->
  Eff es ()
tooltipAt placement target txt =
  void (popup (respHovered target) cfg (label txt))
  where
    cfg = (defaultPopupConfig (AnchorRect (respRect target))) {cfgPlacement = placement, cfgDismissable = False}

-- | Text shown below a widget while the pointer is over it.
--
-- > save <- button' "Save"
-- > tooltip save "Write the file to disk"
tooltip ::
  (Ui :> es, HasResponse r) =>
  r ->
  Text ->
  Eff es ()
tooltip = tooltipAt PlacementBelow
