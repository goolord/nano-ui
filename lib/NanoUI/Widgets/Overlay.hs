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
import NanoUI.Input (Key (..), inputKeys, inputMousePos, inputMousePressed, inputWindowSize)
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
import NanoUI.Widgets.Internal
  ( Response (..)
  , closeButton
  , emptyModalResp
  , floatGapFor
  , floatMinFor
  , floatPadFor
  , mkResponse
  , parentIdx
  , titleBarLayoutFor
  , titleLabelLayoutFor
  , titleMark
  , titleBarHFor
  )
import NanoUI.Widgets.Layout
  ( flex
  , labelEx
  , row
  , scroll
  , sep
  )

modal :: (HasCallStack, Ui :> es) => Bool -> Text -> Eff es a -> Eff es (Response, Maybe a)
modal open title child
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
              minWidth = floatMinFor host 260 availW
              maxW = availW
              maxH = availH
          uiIO $ do
            idx <-
              addNode
                (ctxNodeArena ctx)
                NodeModal
                parent
                Column
                Fit
                Fit
                (floatPadFor host (Padding 14 14 12 12))
                (floatGapFor host 8)
                minWidth
                0
                maxW
                maxH
                0
                AlignStart
                AlignTop
                False
            setWidgetId (ctxNodeArena ctx) idx wid
            writeIORef (ctxContainerStack ctx) (idx : stack)
            beginModal ctx
          (closeResp, r) <-
            ( do
                close <-
                  row (titleBarLayoutFor host) $ do
                    when (not (T.null title)) $
                      void (labelEx (titleLabelLayoutFor host) (titleMark host (iconModalTitle (ctxIcons ctx)) <> title))
                    flex
                    withKey ("close" :: Text) closeButton
                when (not (T.null title)) sep
                r <-
                  if isCellHost host
                    then scroll (tight . grow $ defaultLayout) child
                    else child
                pure (close, r)
            )
              `uiFinally` do
                endModal ctx
                writeIORef (ctxContainerStack ctx) stack
          pure (closeResp, r)
      mrect <- uiIO (getPrevRect ctx wid)
      let mouse = inputMousePos inp
          inPanel = maybe False (\r -> rectW r > 0 && rectH r > 0 && rectContains r mouse) mrect
          backdrop =
            case mrect of
              Just r | rectW r > 0 && rectH r > 0 ->
                inputMousePressed inp && not (rectContains r mouse)
              _ -> False
          esc = KeyEscape `elem` inputKeys inp
          dismiss = backdrop || esc || respClicked closeResp
      when esc $ uiIO (markEscapeConsumed ctx)
      pure
        ( mkResponse wid (maybe (Rect 0 0 0 0) id mrect) inPanel False dismiss dismiss
        , Just body
        )

window :: (HasCallStack, Ui :> es) => Bool -> Text -> Eff es a -> Eff es (Response, Maybe a)
window open title child
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
              pad = resolveLayoutPadding host fm (floatPadFor host windowPad)
              authoredMin = if isCellHost host then 160 else 280
              minWidth = floatMinFor host authoredMin availW
              minHeight =
                min availH (padT pad + titleBarHFor host + padB pad)
              maxW = availW
              maxH = availH
          uiIO $ do
            idx <-
              addNode
                (ctxNodeArena ctx)
                NodeWindow
                parent
                Column
                Fit
                Fit
                (floatPadFor host windowPad)
                (floatGapFor host 10)
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
          (closeResp, body) <-
            ( do
                close <-
                  row (titleBarLayoutFor host) $ do
                    when (not (T.null title)) $
                      withKey title (void (labelEx (titleLabelLayoutFor host) (titleMark host (iconWindowTitle (ctxIcons ctx)) <> title)))
                    flex
                    withKey ("close" :: Text) closeButton
                sep
                body <- scroll (tight . grow $ defaultLayout) child
                pure (close, body)
            )
              `uiFinally` writeIORef (ctxContainerStack ctx) stack
          pure (closeResp, body)
      mrect <- uiIO (getPrevRect ctx wid)
      let mouse = inputMousePos inp
          inPanel = maybe False (\r -> rectW r > 0 && rectH r > 0 && rectContains r mouse) mrect
      pure
        ( mkResponse wid (maybe (Rect 0 0 0 0) id mrect) inPanel False (respClicked closeResp) (respClicked closeResp)
        , Just body
        )
