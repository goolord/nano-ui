-- | Floating overlay chrome: title bars, close buttons.
module NanoUI.Internal.Widgets.Chrome
  ( modalTitleBarH
  , titleBarChromeHFor
  , titleBarLayoutFor
  , titleLabelLayoutFor
  , closeButton
  , windowChromeSepH
  ) where

import Data.Bits ((.|.))
import Effectful (Eff, type (:>))
import NanoUI.Internal.WidgetText (buttonCloseTrailing, buttonFlagClose)
import NanoUI.Internal.Monad (Ui)
import NanoUI.Internal.Style
  ( Layout (..)
  , alignMid
  , defaultLayout
  , fillW
  , fixedH
  , fixedWH
  , gap
  , tight
  )
import NanoUI.Internal.Widgets.Combinators (buttonStyled)
import NanoUI.Internal.Widgets.Node (Response)

titleBarH :: Float
titleBarH = 28

closeButtonSize :: Float
closeButtonSize = 24

windowChromeTop :: Float
windowChromeTop = 10

modalTitleBarH :: Float
modalTitleBarH = 40

windowChromeSepH :: Float
windowChromeSepH = 1

titleBarChromeHFor :: Float
titleBarChromeHFor = titleBarH + windowChromeTop + windowChromeSepH

titleBarLayoutFor :: Float -> Layout
titleBarLayoutFor barH =
  tight . gap 6 . alignMid . fixedH barH . fillW $ defaultLayout

titleLabelLayoutFor :: Float -> Layout
titleLabelLayoutFor barH =
  (fixedH barH . alignMid . tight) $
    defaultLayout {layoutMinH = barH, layoutMaxH = barH}

{-# INLINE closeButton #-}
closeButton :: (Ui :> es) => Eff es Response
closeButton = buttonStyled "" 0 layout (buttonFlagClose .|. buttonCloseTrailing)
  where
    layout = tight . fixedWH closeButtonSize closeButtonSize . alignMid $ defaultLayout
