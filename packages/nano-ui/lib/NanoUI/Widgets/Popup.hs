{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Widgets.Popup
  ( PopupAnchor (..)
  , PopupPlacement (..)
  , PopupConfig (..)
  , defaultPopupConfig
  , popup
  , popupEx
  , tooltipWidget
  , tooltipWith
  , tooltip
  , withTooltip
  )
where

import Control.Monad (void)
import Data.IORef (modifyIORef')
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Effectful (Eff, type (:>))
import NanoUI.Context
  ( Context (..)
  , getPrevRect
  , registerPopupConfig
  , seedFloatingPanel
  )
import NanoUI.Font (resolveLayoutGap, resolveLayoutPadding)
import NanoUI.Id (enterScope, scopeTag)
import NanoUI.Input (inputMousePos)
import NanoUI.Layout.Arena (NodeType (..), addNode)
import NanoUI.Monad
  ( Ui
  , askContext
  , askDefaultLayout
  , askInput
  , nextId
  , uiIO
  )
import NanoUI.Style
  ( AlignX (..)
  , AlignY (..)
  , Layout (..)
  , Padding (..)
  , defaultLayout
  , tight
  )
import NanoUI.Types
  ( PopupAnchor (..)
  , PopupPlacement (..)
  , Rect (..)
  , rectHit
  , rectNonEmpty
  )
import NanoUI.Widgets.Behavior (useDismissable)
import NanoUI.Widgets.Layout (label)
import NanoUI.Widgets.Node
  ( HasResponse
  , Response (..)
  , containerResponse
  , emptyModalResp
  , floatingPanel
  , mkResponse
  , respHovered
  , respRect
  )

data PopupConfig = PopupConfig
  { cfgAnchor :: !PopupAnchor
  , cfgPlacement :: !PopupPlacement
  , cfgDismissable :: !Bool
  , cfgOffset :: !Float
  }
  deriving (Eq, Show)

defaultPopupConfig :: PopupAnchor -> PopupConfig
defaultPopupConfig anchor =
  PopupConfig
    { cfgAnchor = anchor
    , cfgPlacement = PlacementAuto
    , cfgDismissable = True
    , cfgOffset = 4
    }

popup ::
  Ui :> es =>
  Bool ->
  PopupConfig ->
  Eff es a ->
  Eff es (Response, Maybe a)
popup open cfg child = popupEx open cfg (tight defaultLayout) child

popupEx ::
  Ui :> es =>
  Bool ->
  PopupConfig ->
  Layout ->
  Eff es a ->
  Eff es (Response, Maybe a)
popupEx open cfg layout child = do
  wid <- nextId
  ctx <- askContext
  if not open
    then do
      -- A closed popup still consumes its id scope, so the ids of later
      -- siblings do not shift when it opens.
      uiIO (modifyIORef' (ctxIdContext ctx) (fst . enterScope scopeTag))
      pure (emptyModalResp wid, Nothing)
    else do
      inp <- askInput
      let
        fm = ctxFontMetrics ctx
        addPopupNode parent = do
          registerPopupConfig ctx wid (cfgAnchor cfg) (cfgPlacement cfg) (cfgOffset cfg)
          addNode
            (ctxNodeArena ctx)
            NodePopup
            parent
            (layoutDirection layout)
            (layoutWidth layout)
            (layoutHeight layout)
            (resolveLayoutPadding fm (Padding 6 6 6 6))
            (resolveLayoutGap fm 4)
            0
            0
            1e9
            1e9
            0
            AlignStart
            AlignTop
        seedFromPrev = getPrevRect ctx wid >>= mapM_ (seedFloatingPanel ctx wid)
      body <- floatingPanel True wid addPopupNode seedFromPrev child
      mrect <- uiIO (getPrevRect ctx wid)
      let
        panel = fromMaybe (Rect 0 0 0 0) mrect
        inPanel = rectHit panel (inputMousePos inp)
      dismissed <-
        if cfgDismissable cfg && rectNonEmpty panel
          then useDismissable panel
          else pure False
      pure
        ( mkResponse wid panel inPanel False dismissed dismissed
        , Just body
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

-- | Concise tooltip with specified placement.
tooltipWith ::
  (Ui :> es, HasResponse r) =>
  PopupPlacement ->
  r ->
  Text ->
  Eff es ()
tooltipWith placement target txt =
  void (popup (respHovered target) cfg (label txt))
  where
    cfg = (defaultPopupConfig (AnchorRect (respRect target))) {cfgPlacement = placement, cfgDismissable = False}

-- | Standard text tooltip widget on hover.
tooltip ::
  (Ui :> es, HasResponse r) =>
  r ->
  Text ->
  Eff es ()
tooltip = tooltipWith PlacementBelow
