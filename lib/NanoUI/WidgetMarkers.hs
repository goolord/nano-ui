module NanoUI.WidgetMarkers
  ( closeButtonMarker
  , tabButtonMarker
  , stripButtonBrackets
  , isCloseButtonText
  , isTabButtonText
  , closeButtonDisplayText
  , tabButtonDisplayText
  , buttonDisplayText
  , buttonFlags
  , buttonDisplayTextFromFlags
  ) where

import qualified Data.Text as T

closeButtonMarker :: T.Text
closeButtonMarker = T.singleton '\x01'

tabButtonMarker :: T.Text
tabButtonMarker = T.singleton '\x02'

{-# INLINE stripButtonBrackets #-}
stripButtonBrackets :: T.Text -> T.Text
stripButtonBrackets txt =
  let t = T.strip txt
   in if T.isPrefixOf "[ " t && T.isSuffixOf " ]" t
        then T.strip $ T.dropEnd 2 $ T.drop 2 t
        else txt

{-# INLINE buttonFlags #-}
buttonFlags :: T.Text -> (Bool, Bool)
buttonFlags txt =
  let lbl = stripButtonBrackets txt
   in ( closeButtonMarker `T.isPrefixOf` lbl
      , tabButtonMarker `T.isPrefixOf` lbl
      )

{-# INLINE isCloseButtonText #-}
isCloseButtonText :: T.Text -> Bool
isCloseButtonText txt = fst (buttonFlags txt)

{-# INLINE isTabButtonText #-}
isTabButtonText :: T.Text -> Bool
isTabButtonText txt = snd (buttonFlags txt)

{-# INLINE buttonDisplayTextFromFlags #-}
buttonDisplayTextFromFlags :: (Bool, Bool) -> T.Text -> T.Text
buttonDisplayTextFromFlags (isClose, isTab) txt
  | isClose || isTab = T.drop 1 (stripButtonBrackets txt)
  | otherwise = txt

{-# INLINE closeButtonDisplayText #-}
closeButtonDisplayText :: T.Text -> T.Text
closeButtonDisplayText txt = buttonDisplayTextFromFlags (True, False) txt

{-# INLINE tabButtonDisplayText #-}
tabButtonDisplayText :: T.Text -> T.Text
tabButtonDisplayText txt = buttonDisplayTextFromFlags (False, True) txt

{-# INLINE buttonDisplayText #-}
buttonDisplayText :: T.Text -> T.Text
buttonDisplayText txt = buttonDisplayTextFromFlags (buttonFlags txt) txt
