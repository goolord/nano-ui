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

import Control.Monad (void, when)
import Data.IORef (readIORef, writeIORef)
import Data.Text (Text)
import Effectful (Eff, type (:>))
import NanoUI.Context
  ( Context (..)
  , getPrevRect
  , registerPopupConfig
  , seedFloatingPanel
  )
import NanoUI.Font (resolveLayoutGap, resolveLayoutPadding)
import NanoUI.Host (isCellHost)
import NanoUI.Input (inputMousePos)
import NanoUI.Layout.Arena (NodeType (..), addNode, setWidgetId)
import NanoUI.Id (enterScope, scopeTag)
import NanoUI.Monad
  ( Ui
  , askContext
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
  , rectContains
  , rectH
  , rectW
  )
import NanoUI.Widgets.Behavior (useDismissable)
import NanoUI.Widgets.Layout (columnResponse, label)
import NanoUI.Widgets.Node
  ( Responding (..)
  , Response (..)
  , emptyModalResp
  , mkResponse
  , parentIdx
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
  inp <- askInput
  parent' <- uiIO $ do
    oldCtx <- readIORef (ctxIdContext ctx)
    let
      (parent', childCtx) = enterScope scopeTag oldCtx
    writeIORef (ctxIdContext ctx) childCtx
    pure parent'
  result <-
    if not open
      then pure (emptyModalResp wid, Nothing)
      else do
        body <- do
          stack <- uiIO (readIORef (ctxContainerStack ctx))
          let
            fm = ctxFontMetrics ctx
            host = ctxHostProfile ctx
            parent = parentIdx stack
            terminal = isCellHost host
            pad = if terminal then Padding 0 0 0 0 else Padding 6 6 6 6
            gap = if terminal then 0 else 4
            resolvedPad = resolveLayoutPadding host fm pad
            resolvedGap = resolveLayoutGap host fm gap
            dir = layoutDirection layout
            wSiz = layoutWidth layout
            hSiz = layoutHeight layout
          prevFloat <- uiIO $ do
            registerPopupConfig ctx wid (cfgAnchor cfg) (cfgPlacement cfg) (cfgOffset cfg)
            idx <-
              addNode
                (ctxNodeArena ctx)
                NodePopup
                parent
                dir
                wSiz
                hSiz
                resolvedPad
                resolvedGap
                0
                0
                1e9
                1e9
                0
                AlignStart
                AlignTop
                False
            setWidgetId (ctxNodeArena ctx) idx wid
            writeIORef (ctxContainerStack ctx) (idx : stack)
            mPrev <- getPrevRect ctx wid
            let seedRect = maybe (Rect 0 0 0 0) id mPrev
            when (rectW seedRect > 0 && rectH seedRect > 0) $
              seedFloatingPanel ctx wid seedRect
            prev <- readIORef (ctxCurrentFloatingId ctx)
            writeIORef (ctxCurrentFloatingId ctx) (Just wid)
            pure prev
          r <- child
          uiIO $ do
            writeIORef (ctxContainerStack ctx) stack
            writeIORef (ctxCurrentFloatingId ctx) prevFloat
          pure r
        mrect <- uiIO (getPrevRect ctx wid)
        let
          mouse = inputMousePos inp
          panel = maybe (Rect 0 0 0 0) id mrect
          inPanel = rectW panel > 0 && rectH panel > 0 && rectContains panel mouse
        dismissed <-
          if cfgDismissable cfg && rectW panel > 0 && rectH panel > 0
            then useDismissable panel
            else pure False
        pure
          ( mkResponse wid panel inPanel False dismissed dismissed
          , Just body
          )
  uiIO $ writeIORef (ctxIdContext ctx) parent'
  pure result

-- | Attach a rich tooltip widget to any target response, displayed on hover.
tooltipWidget ::
  (Ui :> es, Responding r) =>
  r ->
  Eff es a ->
  Eff es (Maybe a)
tooltipWidget target child = do
  let hovered = respHovered target
      rect = respRect target
      cfg = PopupConfig
        { cfgAnchor = AnchorRect rect
        , cfgPlacement = PlacementBelow
        , cfgDismissable = False
        , cfgOffset = 4
        }
  fmap snd (popup hovered cfg child)

-- | Attach a rich tooltip widget to an inner UI computation.
withTooltip ::
  Ui :> es =>
  Eff es a ->
  Eff es b ->
  Eff es (a, Maybe b)
withTooltip mainChild tipChild = do
  (res, contResp) <- columnResponse (tight defaultLayout) mainChild
  mTip <- tooltipWidget contResp tipChild
  pure (res, mTip)

-- | Concise tooltip with specified placement.
tooltipWith ::
  (Ui :> es, Responding r) =>
  PopupPlacement ->
  Text ->
  r ->
  Eff es ()
tooltipWith placement txt target = do
  let hovered = respHovered target
      rect = respRect target
      cfg = PopupConfig
        { cfgAnchor = AnchorRect rect
        , cfgPlacement = placement
        , cfgDismissable = False
        , cfgOffset = 4
        }
  void (popup hovered cfg (label txt))

-- | Standard text tooltip widget on hover.
tooltip ::
  (Ui :> es, Responding r) =>
  Text ->
  r ->
  Eff es ()
tooltip = tooltipWith PlacementBelow
