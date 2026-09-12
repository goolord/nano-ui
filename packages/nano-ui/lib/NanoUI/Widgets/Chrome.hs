{-# LANGUAGE OverloadedStrings #-}

-- | Floating overlay chrome: title bars, close buttons.
module NanoUI.Widgets.Chrome
  ( titleBarHFor
  , modalTitleBarHFor
  , modalTitleBarH
  , titleBarChromeHFor
  , titleBarLayoutFor
  , titleLabelLayoutFor
  , floatPadFor
  , floatGapFor
  , floatMinFor
  , closeButton
  , windowChromeTop
  , windowChromeSepH
  ) where

import Effectful (Eff, type (:>))
import NanoUI.WidgetText (buttonFlagClose)
import NanoUI.Context (isDisabled, registerFocusable)
import NanoUI.Layout.Arena (NodeType (NodeButton))
import NanoUI.Monad (Ui, askContext, nextId, uiIO)
import NanoUI.Style
  ( Layout (..)
  , Padding (..)
  , alignMid
  , defaultLayout
  , fillW
  , fixedH
  , fixedWH
  , gap
  , tight
  )
import NanoUI.Widgets.Behavior (keyActivated)
import NanoUI.Widgets.Node (Responding (..), Response, addWidgetStyled, setClicked, setHovered)

titleBarH :: Float
titleBarH = 28

closeButtonSize :: Float
closeButtonSize = 24

windowChromeTop :: Float
windowChromeTop = 10

titleBarHFor :: Float
titleBarHFor = titleBarH

modalTitleBarH :: Float
modalTitleBarH = 40

modalTitleBarHFor :: Float
modalTitleBarHFor = modalTitleBarH

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

floatPadFor :: Padding -> Padding
floatPadFor pad = pad

floatGapFor :: Float -> Float
floatGapFor g = g

floatMinFor :: Float -> Float -> Float
floatMinFor authored avail = max 1 (min authored avail)

{-# INLINE closeButton #-}
closeButton :: (Ui :> es) => Eff es Response
closeButton = do
  wid <- nextId
  ctx <- askContext
  uiIO $ registerFocusable ctx wid
  let stored = ""
      layout = tight . fixedWH closeButtonSize closeButtonSize . alignMid $ defaultLayout
  resp <- addWidgetStyled wid NodeButton stored 0 layout buttonFlagClose Nothing
  disabled <- uiIO (isDisabled ctx wid)
  keyClick <- keyActivated wid
  pure $
    setClicked (not disabled && (respClicked resp || keyClick)) $
      setHovered (not disabled && respHovered resp) resp
