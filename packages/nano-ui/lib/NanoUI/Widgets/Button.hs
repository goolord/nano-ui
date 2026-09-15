-- | Push buttons.
module NanoUI.Widgets.Button
  ( button
  , button'
  , buttonWith
  , buttonWith'
  , button_
  , buttonEx
  )
where

import Control.Monad (void)
import Data.Text (Text)
import Effectful (Eff, type (:>))
import NanoUI.Monad (Ui)
import NanoUI.Style (Layout, defaultLayout)
import NanoUI.Widgets.Combinators (buttonStyledEx)
import NanoUI.Widgets.Node (Response, respClicked)

-- | Immediate-mode button with default layout. Returns 'True' if clicked this frame.
--
-- Example:
--
-- @
-- whenM (button "Save") saveDocument
-- @
{-# INLINE button #-}
button :: Ui :> es => Text -> Eff es Bool
button txt = respClicked <$> button' txt

-- | Button returning the full 'Response' record (for tooltips, context menus,
-- hover tracking, or testing assertions).
--
-- Example:
--
-- @
-- btn <- button' "Help"
-- tooltip btn "Click for documentation"
-- when (respClicked btn) openDocs
-- @
{-# INLINE button' #-}
button' :: Ui :> es => Text -> Eff es Response
button' = buttonEx True

-- | Immediate-mode button with a layout modifier. Returns 'True' if clicked this frame.
--
-- Example:
--
-- @
-- whenM (buttonWith (fixedW 120 . fontBold) "Submit") submitForm
-- @
{-# INLINE buttonWith #-}
buttonWith :: Ui :> es => (Layout -> Layout) -> Text -> Eff es Bool
buttonWith f txt = respClicked <$> buttonWith' f txt

-- | Button with a layout modifier returning the full 'Response'.
{-# INLINE buttonWith' #-}
buttonWith' :: Ui :> es => (Layout -> Layout) -> Text -> Eff es Response
buttonWith' f txt = buttonLayoutEx (f defaultLayout) True txt

-- | Button that ignores the click result (useful for static or decorative buttons).
{-# INLINE button_ #-}
button_ :: Ui :> es => Text -> Eff es ()
button_ txt = void (button' txt)

-- | Button with custom layout and enabled state.
{-# INLINE buttonLayoutEx #-}
buttonLayoutEx :: (Ui :> es) => Layout -> Bool -> Text -> Eff es Response
buttonLayoutEx layout enabled txt = buttonStyledEx enabled txt 0 layout 0

-- | Button with enabled/disabled flag and default layout.
{-# INLINE buttonEx #-}
buttonEx :: (Ui :> es) => Bool -> Text -> Eff es Response
buttonEx = buttonLayoutEx defaultLayout
