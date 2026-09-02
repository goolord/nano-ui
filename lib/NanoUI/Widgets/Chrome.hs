{-# LANGUAGE OverloadedStrings #-}

-- | Floating overlay chrome: title bars, close buttons, cell-host scaling.
module NanoUI.Widgets.Chrome
  ( titleBarHFor
  , titleBarLayoutFor
  , titleLabelLayoutFor
  , floatPadFor
  , floatGapFor
  , floatMinFor
  , titleMark
  , closeButton
  ) where

import Effectful (Eff, type (:>))
import Data.Text (Text)
import NanoUI.Font (layoutUnitScale)
import NanoUI.WidgetText (closeButtonMarker)
import NanoUI.Types (HostProfile, isCellHost)
import NanoUI.Context (Context (..), isDisabled, registerFocusable)
import NanoUI.Icons (Icons (..))
import NanoUI.Layout.Arena (NodeType (NodeButton))
import NanoUI.Monad (Ui, askContext, nextId, uiIO)
import NanoUI.Style
  ( Layout (..)
  , Padding (..)
  , alignMid
  , defaultLayout
  , fillW
  , fixedH
  , fixedW
  , fixedWH
  , gap
  , tight
  )
import NanoUI.Widgets.Node (Responding (..), Response, addWidget, setClicked, setHovered)

titleBarH :: Float
titleBarH = 28

titleBarHFor :: HostProfile -> Float
titleBarHFor host
  | isCellHost host = 1
  | otherwise = titleBarH

titleBarLayoutFor :: HostProfile -> Layout
titleBarLayoutFor host =
  tight . gap (if isCellHost host then 1 else 6) . alignMid . fixedH (titleBarHFor host) . fillW $ defaultLayout

titleLabelLayoutFor :: HostProfile -> Layout
titleLabelLayoutFor host =
  tight . alignMid . fixedH (titleBarHFor host) $ defaultLayout

floatPadFor :: HostProfile -> Padding -> Padding
floatPadFor host pad
  | isCellHost host = Padding 4 4 4 4
  | otherwise = pad

floatGapFor :: HostProfile -> Float -> Float
floatGapFor host g
  | isCellHost host = 4
  | otherwise = g

floatMinFor :: HostProfile -> Float -> Float -> Float
floatMinFor host authored avail =
  let raw =
        if isCellHost host
          then authored * layoutUnitScale host
          else authored
   in max 1 (min raw avail)

titleMark :: HostProfile -> Text -> Text
titleMark host mark = if isCellHost host then mark else ""

{-# INLINE closeButton #-}
closeButton :: (Ui :> es) => Eff es Response
closeButton = do
  wid <- nextId
  ctx <- askContext
  uiIO $ registerFocusable ctx wid
  let host = ctxHostProfile ctx
      stored = closeButtonMarker <> iconClose (ctxIcons ctx)
      h = titleBarHFor host
      layout =
        if isCellHost host
          then
            let slotW = 3
             in tight . fixedW slotW . alignMid $ defaultLayout
          else tight . fixedWH h h . alignMid $ defaultLayout
  resp <- addWidget wid NodeButton stored 0 layout
  disabled <- uiIO (isDisabled ctx wid)
  pure $
    setClicked (not disabled && respClicked resp) $
      setHovered (not disabled && respHovered resp) resp
