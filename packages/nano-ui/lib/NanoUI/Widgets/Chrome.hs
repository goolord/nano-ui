{-# LANGUAGE OverloadedStrings #-}

-- | Floating overlay chrome: title bars, close buttons, cell-host scaling.
module NanoUI.Widgets.Chrome
  ( titleBarHFor
  , titleBarChromeHFor
  , titleBarLayoutFor
  , titleLabelLayoutFor
  , floatPadFor
  , floatGapFor
  , floatMinFor
  , titleMark
  , closeButton
  , windowChromeTop
  , windowChromeSepH
  ) where

import Effectful (Eff, type (:>))
import Data.Text (Text)
import NanoUI.Font (layoutUnitScale)
import NanoUI.WidgetText (buttonFlagClose)
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
import NanoUI.Widgets.Node (Responding (..), Response, addWidgetStyled, setClicked, setHovered)

titleBarH :: Float
titleBarH = 28

closeButtonSize :: Float
closeButtonSize = 24

windowChromeTop :: Float
windowChromeTop = 10

titleBarHFor :: HostProfile -> Float
titleBarHFor host
  | isCellHost host = 1
  | otherwise = titleBarH

windowChromeSepH :: Float
windowChromeSepH = 1

titleBarChromeHFor :: HostProfile -> Float
titleBarChromeHFor host
  | isCellHost host = titleBarHFor host
  | otherwise = titleBarH + windowChromeTop + windowChromeSepH

titleBarLayoutFor :: HostProfile -> Float -> Layout
titleBarLayoutFor host barH =
  tight . gap (if isCellHost host then 1 else 6) . alignMid . fixedH barH . fillW $ defaultLayout

titleLabelLayoutFor :: HostProfile -> Float -> Layout
titleLabelLayoutFor _host barH =
  (fixedH barH . alignMid . tight) $
    defaultLayout {layoutMinH = barH, layoutMaxH = barH}

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
      stored = iconClose (ctxIcons ctx)
      layout =
        if isCellHost host
          then
            let slotW = 3
             in tight . fixedW slotW . alignMid $ defaultLayout
          else tight . fixedWH closeButtonSize closeButtonSize . alignMid $ defaultLayout
  resp <- addWidgetStyled wid NodeButton stored 0 layout buttonFlagClose Nothing
  disabled <- uiIO (isDisabled ctx wid)
  pure $
    setClicked (not disabled && respClicked resp) $
      setHovered (not disabled && respHovered resp) resp
