{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Widgets.Overlay
  ( modal
  , window
  )
where

import Control.Monad (void, when)
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
import NanoUI.Id (WidgetId)
import NanoUI.Input
  ( inputWindowSize
  )
import NanoUI.Layout.Arena (NodeType (..), addNode)
import NanoUI.Monad
  ( Ui
  , askContext
  , askInput
  , uiIO
  , withKey
  )
import NanoUI.Store (Slot (..), fieldPoint, lookupSlot, slotKey)
import NanoUI.Style
  ( AlignX (..)
  , AlignY (..)
  , Direction (..)
  , Padding (..)
  , Sizing (..)
  , grow
  , padB
  , padT
  , tight
  , windowMargin
  , windowPad
  )
import NanoUI.Types (Rect (..), Size (..), rectNonEmpty)
import NanoUI.Widgets.Chrome
  ( closeButton
  , floatMinFor
  , modalTitleBarH
  , titleBarChromeHFor
  , titleBarLayoutFor
  , titleLabelLayoutFor
  )
import NanoUI.Widgets.Popup (floatingOverlay)
import NanoUI.Widgets.Layout
  ( flex
  , labelEx
  , row'
  , scrollWith
  , separator
  )
import NanoUI.Widgets.Node
  ( Response (..)
  , respClicked
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
overlay kind open title child = do
  ctx <- askContext
  inp <- askInput
  let
    Size winW winH = inputWindowSize inp
    margin = windowMargin
    availW = max 1 (winW - 2 * margin)
    availH = max 1 (winH - 2 * margin)
    isModal = kind == ModalOverlay
    -- Modals share the window's side padding. The body's scrollbar sits
    -- out in it just inside the panel's edge, that padding from the
    -- content.
    padding = if isModal then windowPad {padB = 12} else windowPad
    barH = if isModal then modalTitleBarH else titleBarChromeHFor
    -- Window body breathing room: one side-pad between the chrome and
    -- the body, matching the window's left/right padding. Modals keep
    -- their own larger gap.
    bodyGap = if isModal then 8 else 10
    minWidth =
      floatMinFor
        (if isModal then 260 else 280)
        availW
    minHeight =
      if isModal
        then 0
        else
          min availH (padT padding + titleBarChromeHFor + bodyGap + padB padding)
    addOverlayNode _ parent =
      addNode
        (ctxNodeArena ctx)
        (if isModal then NodeModal else NodeWindow)
        parent
        Column
        Fit
        Fit
        padding
        bodyGap
        minWidth
        minHeight
        availW
        availH
        0
        AlignStart
        AlignTop
    enter wid = do
      when isModal (beginModal ctx)
      seedFloatingPanel ctx wid
        =<< floatingSeedRect ctx wid isModal minWidth minHeight margin winW winH
    titleLabel = void (labelEx (titleLabelLayoutFor barH) title)
  floatingOverlay open isModal addOverlayNode enter $ do
    close <-
      row' (titleBarLayoutFor barH) $ do
        when (not (T.null title)) $
          case kind of
            ModalOverlay -> titleLabel
            WindowOverlay -> withKey title titleLabel
        flex
        withKey ("close" :: Text) closeButton
    when (isModal && not (T.null title)) separator
    r <- scrollWith (tight . grow) child
    when isModal (uiIO (endModal ctx))
    pure (respClicked close, r)

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
    Just r | rectNonEmpty r -> pure r
    _ -> do
      store <- getStore ctx
      let
        k = intKey wid
        pos = lookupSlot fieldPoint k store
        sz = lookupSlot fieldPoint (slotKey SlotWinSize k) store
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
