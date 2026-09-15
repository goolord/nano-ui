{-# LANGUAGE OverloadedStrings #-}

-- | Floating overlay chrome: title bars, close buttons.
module NanoUI.Widgets.Chrome
  ( modalTitleBarH
  , titleBarChromeHFor
  , titleBarLayoutFor
  , titleLabelLayoutFor
  , floatMinFor
  , closeButton
  , windowChromeTop
  , windowChromeSepH
  ) where

import Effectful (Eff, type (:>))
import NanoUI.WidgetText (buttonFlagClose)
import NanoUI.Monad (Ui)
import NanoUI.Style
  ( Layout (..)
  , alignMid
  , defaultLayout
  , fillW
  , fixedH
  , fixedWH
  , gap
  , tight
  )
import NanoUI.Types (clamp)
import NanoUI.Widgets.Combinators (buttonStyled)
import NanoUI.Widgets.Node (Response)

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

floatMinFor :: Float -> Float -> Float
floatMinFor authored avail = clamp 1 avail authored

{-# INLINE closeButton #-}
closeButton :: (Ui :> es) => Eff es Response
closeButton = buttonStyled "" 0 layout buttonFlagClose
  where
    layout = tight . fixedWH closeButtonSize closeButtonSize . alignMid $ defaultLayout
