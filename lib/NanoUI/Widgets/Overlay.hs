{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Widgets.Overlay
  ( modal
  , window
  ) where

import Control.Monad (void, when)
import Data.IORef (readIORef, writeIORef)
import Effectful (Eff, type (:>))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Stack (HasCallStack)
import NanoUI.Font (resolveLayoutGap, resolveLayoutPadding)
import NanoUI.Host (isCellHost)
import NanoUI.Context
  ( Context (..)
  , beginModal
  , endModal
  , getPrevRect
  , markEscapeConsumed
  )
import NanoUI.Icons (Icons (..))
import NanoUI.Input (Key (..), inputKeys, inputKeysElem, inputMousePos, inputMousePressed, inputWindowSize)
import NanoUI.Layout.Arena (NodeType (..), addNode, setWidgetId)
import NanoUI.Monad (Ui, askContext, askInput, currentId, uiFinally, uiIO, withKey)
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
  , windowPad
  , windowMargin
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
import NanoUI.Widgets.Node (Responding (..), Response (..), emptyModalResp, mkResponse, parentIdx)
import NanoUI.Widgets.Layout
  ( flex
  , labelEx
  , row
  , scroll
  , sep
  )

data OverlayKind
  = ModalOverlay
  | WindowOverlay
  deriving (Eq)

modal :: (HasCallStack, Ui :> es) => Bool -> Text -> Eff es a -> Eff es (Response, Maybe a)
modal = overlay ModalOverlay

window :: (HasCallStack, Ui :> es) => Bool -> Text -> Eff es a -> Eff es (Response, Maybe a)
window = overlay WindowOverlay

overlay :: (HasCallStack, Ui :> es) => OverlayKind -> Bool -> Text -> Eff es a -> Eff es (Response, Maybe a)
overlay kind open title child
  | not open = do
      wid <- currentId
      pure (emptyModalResp wid, Nothing)
  | otherwise = do
      wid <- currentId
      ctx <- askContext
      inp <- askInput
      (closeResp, body) <- do
          stack <- uiIO (readIORef (ctxContainerStack ctx))
          let fm = ctxFontMetrics ctx
              host = ctxHostProfile ctx
              parent = parentIdx stack
              Size winW winH = inputWindowSize inp
              margin = resolveLayoutGap host fm windowMargin
              availW = max 1 (winW - 2 * margin)
              availH = max 1 (winH - 2 * margin)
              isModal = kind == ModalOverlay
              padding = floatPadFor host (if isModal then Padding 14 14 12 12 else windowPad)
              minWidth = floatMinFor host (if isModal then 260 else if isCellHost host then 160 else 280) availW
              minHeight =
                if isModal
                  then 0
                  else
                    let pad = resolveLayoutPadding host fm padding
                     in min availH (padT pad + titleBarHFor host + padB pad)
              maxW = availW
              maxH = availH
          uiIO $ do
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
          (closeResp, r) <-
            ( do
                close <-
                  row (titleBarLayoutFor host) $ do
                    when (not (T.null title)) $
                      case kind of
                        ModalOverlay -> void (labelEx (titleLabelLayoutFor host) (titleMark host (iconModalTitle (ctxIcons ctx)) <> title))
                        WindowOverlay -> withKey title (void (labelEx (titleLabelLayoutFor host) (titleMark host (iconWindowTitle (ctxIcons ctx)) <> title)))
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
          pure (closeResp, r)
      mrect <- uiIO (getPrevRect ctx wid)
      let mouse = inputMousePos inp
          inPanel = maybe False (\r -> rectW r > 0 && rectH r > 0 && rectContains r mouse) mrect
          backdrop =
            kind == ModalOverlay
              && case mrect of
                Just r | rectW r > 0 && rectH r > 0 ->
                  inputMousePressed inp && not (rectContains r mouse)
                _ -> False
          esc = kind == ModalOverlay && inputKeysElem KeyEscape (inputKeys inp)
          dismissed = backdrop || esc || respClicked closeResp
      when esc $ uiIO (markEscapeConsumed ctx)
      pure
        ( mkResponse wid (maybe (Rect 0 0 0 0) id mrect) inPanel False dismissed dismissed
        , Just body
        )
