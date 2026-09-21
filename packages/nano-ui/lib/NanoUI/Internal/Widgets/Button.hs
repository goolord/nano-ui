-- | Push buttons.
module NanoUI.Internal.Widgets.Button
  ( button
  , button'
  , buttonWith
  , buttonWith'
  )
where

import Data.Text (Text)
import Effectful (Eff, type (:>))
import NanoUI.Internal.Monad (Ui)
import NanoUI.Internal.Style (Layout, defaultLayout)
import NanoUI.Internal.Widgets.Combinators (buttonStyledEx)
import NanoUI.Internal.Widgets.Node (Response, respClicked)

-- | Button with a text label. 'True' on the frame it is clicked, by pointer
-- or by Enter or Space while focused.
--
-- > whenM (button "Save") saveDocument
{-# INLINE button #-}
button :: Ui :> es => Text -> Eff es Bool
button txt = respClicked <$> button' txt

-- | 'button' returning its 'Response', for tooltips, anchored popups, or
-- hover state.
--
-- > help <- button' "Help"
-- > tooltip help "Open the manual"
-- > when (respClicked help) openManual
{-# INLINE button' #-}
button' :: Ui :> es => Text -> Eff es Response
button' = buttonWith' id

-- | 'button' with a layout modifier.
--
-- > whenM (buttonWith (fixedW 120) "Submit") submitForm
{-# INLINE buttonWith #-}
buttonWith :: Ui :> es => (Layout -> Layout) -> Text -> Eff es Bool
buttonWith f txt = respClicked <$> buttonWith' f txt

{-# INLINE buttonWith' #-}
-- | 'buttonWith' returning the full response; activation is in @respClicked@.
buttonWith' :: Ui :> es => (Layout -> Layout) -> Text -> Eff es Response
buttonWith' f txt = buttonStyledEx True txt 0 (f defaultLayout) 0
