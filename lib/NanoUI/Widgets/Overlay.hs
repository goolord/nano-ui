{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Widgets.Overlay
  ( modal
  , window
  )
where

import Control.Monad (void, when)
import Data.IORef (readIORef, writeIORef)
import Data.IntMap.Strict qualified as IM
import Data.Text (Text)
import Data.Text qualified as T
import Effectful (Eff, type (:>))
import NanoUI.Context
  ( Context (..)
  , beginModal
  , endModal
  , getPrevRect
  , getStore
  , intKey
  , seedFloatingPanel
  )
import NanoUI.Font (resolveLayoutGap, resolveLayoutPadding)
import NanoUI.Host (isCellHost)
import NanoUI.Icons (Icons (..))
import NanoUI.Id (WidgetId)
import NanoUI.Input
  ( inputMousePos
  , inputWindowSize
  )
import NanoUI.Layout.Arena (NodeType (..), addNode, setWidgetId)
import NanoUI.Monad
  ( Ui
  , askContext
  , askInput
  , nextId
  , uiFinally
  , uiIO
  , withKey
  )
import NanoUI.Store (WidgetStore (..), listPair, slotKey, slotWinSize)
import NanoUI.Style
  ( AlignX (..)
  , AlignY (..)
  , Direction (..)
  , Padding (..)
  , Sizing (..)
  , defaultLayout
  , grow
  , padB
  , padT
  , tight
  , windowMargin
  , windowPad
  )
import NanoUI.Types (Rect (..), Size (..), rectContains, rectH, rectW)
import NanoUI.Widgets.Chrome
  ( closeButton
  , floatGapFor
  , floatMinFor
  , floatPadFor
  , titleBarHFor
  , titleBarLayoutFor
  , titleLabelLayoutFor
  , titleMark
  )
import NanoUI.Widgets.Behavior (useDismissable)
import NanoUI.Widgets.Layout
  ( flex
  , labelEx
  , row
  , scroll
  , sep
  )
import NanoUI.Widgets.Node
  ( Responding (..)
  , Response (..)
  , emptyModalResp
  , mkResponse
  , parentIdx
  )

data OverlayKind
  = ModalOverlay
  | WindowOverlay
  deriving Eq

modal :: Ui :> es => Bool -> Text -> Eff es a -> Eff es (Response, Maybe a)
modal = overlay ModalOverlay

window :: Ui :> es => Bool -> Text -> Eff es a -> Eff es (Response, Maybe a)
window = overlay WindowOverlay

overlay ::
  Ui :> es =>
  OverlayKind -> Bool -> Text -> Eff es a -> Eff es (Response, Maybe a)
overlay kind open title child
  | not open = do
      wid <- nextId
      pure (emptyModalResp wid, Nothing)
  | otherwise = do
      wid <- nextId
      ctx <- askContext
      inp <- askInput
      (closeResp, body) <- do
        stack <- uiIO (readIORef (ctxContainerStack ctx))
        let
          fm = ctxFontMetrics ctx
          host = ctxHostProfile ctx
          parent = parentIdx stack
          Size winW winH = inputWindowSize inp
          margin = resolveLayoutGap host fm windowMargin
          availW = max 1 (winW - 2 * margin)
          availH = max 1 (winH - 2 * margin)
          isModal = kind == ModalOverlay
          padding = floatPadFor host (if isModal then Padding 14 14 12 12 else windowPad)
          minWidth =
            floatMinFor
              host
              (if isModal then 260 else if isCellHost host then 160 else 280)
              availW
          minHeight =
            if isModal
              then 0
              else
                let
                  pad = resolveLayoutPadding host fm padding
                 in
                  min availH (padT pad + titleBarHFor host + padB pad)
          maxW = availW
          maxH = availH
        prevFloat <- uiIO $ do
          idx <-
            addNode
              (ctxNodeArena ctx)
              (if isModal then NodeModal else NodeWindow)
              parent
              Column
              Fit
              Fit
              padding
              (floatGapFor host (if isModal then 8 else 10))
              minWidth
              minHeight
              maxW
              maxH
              0
              AlignStart
              AlignTop
              False
          setWidgetId (ctxNodeArena ctx) idx wid
          writeIORef (ctxContainerStack ctx) (idx : stack)
          when isModal (beginModal ctx)
          seedRect <- floatingSeedRect ctx wid isModal minWidth minHeight margin winW winH
          seedFloatingPanel ctx wid seedRect
          prev <- readIORef (ctxCurrentFloatingId ctx)
          writeIORef (ctxCurrentFloatingId ctx) (Just wid)
          pure prev
        (closeResp, r) <-
          ( do
              close <-
                row (titleBarLayoutFor host) $ do
                  when (not (T.null title)) $
                    case kind of
                      ModalOverlay ->
                        void
                          ( labelEx
                              (titleLabelLayoutFor host)
                              (titleMark host (iconModalTitle (ctxIcons ctx)) <> title)
                          )
                      WindowOverlay ->
                        withKey
                          title
                          ( void
                              ( labelEx
                                  (titleLabelLayoutFor host)
                                  (titleMark host (iconWindowTitle (ctxIcons ctx)) <> title)
                              )
                          )
                  flex
                  withKey ("close" :: Text) closeButton
              when (isModal && not (T.null title) || not isModal) sep
              r <-
                if isModal && not (isCellHost host)
                  then child
                  else scroll (tight . grow $ defaultLayout) child
              pure (close, r)
          )
            `uiFinally` do
              when isModal (endModal ctx)
              writeIORef (ctxContainerStack ctx) stack
              writeIORef (ctxCurrentFloatingId ctx) prevFloat
        pure (closeResp, r)
      mrect <- uiIO (getPrevRect ctx wid)
      let
        mouse = inputMousePos inp
        panel = maybe (Rect 0 0 0 0) id mrect
        inPanel = rectW panel > 0 && rectH panel > 0 && rectContains panel mouse
      outside <-
        if kind == ModalOverlay && rectW panel > 0 && rectH panel > 0
          then useDismissable panel
          else pure False
      let dismissed = outside || respClicked closeResp
      pure
        ( mkResponse wid panel inPanel False dismissed dismissed
        , Just body
        )

floatingSeedRect ::
  Context
  -> WidgetId
  -> Bool
  -> Float
  -> Float
  -> Float
  -> Float
  -> Float
  -> IO Rect
floatingSeedRect ctx wid isModal minWidth minHeight margin winW winH = do
  mPrev <- getPrevRect ctx wid
  case mPrev of
    Just r | rectW r > 0 && rectH r > 0 -> pure r
    _ -> do
      store <- getStore ctx
      let
        k = intKey wid
        pos = IM.lookup k (storeFloatList store) >>= listPair
        sz = IM.lookup (slotKey slotWinSize k) (storeFloatList store) >>= listPair
      pure $
        case (pos, sz) of
          (Just (x, y), Just (w, h)) | w > 0 && h > 0 -> Rect x y w h
          (Just (x, y), _) -> Rect x y minWidth (max minHeight 1)
          _ ->
            let
              w = minWidth
              h = max minHeight 1
             in
              if isModal
                then Rect ((winW - w) / 2) ((winH - h) / 2) w h
                else Rect (max 0 (winW - w - margin)) margin w h
