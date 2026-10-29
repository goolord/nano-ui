-- | Signature listings for the book. Each anchored block is a run of type
-- signatures, defined below the anchor as the library function itself, so a
-- signature that drifts from the library fails to compile.
module Book.Signatures where

import Data.Text (Text)
import NanoUI hiding (button, button', buttonContent, buttonWith, iconButton, modal, popup, window)
import NanoUI qualified as N

-- ANCHOR: buttons
button   :: Text -> NanoUI Bool
button'  :: Text -> NanoUI Response
buttonWith :: (Layout -> Layout) -> Text -> NanoUI Bool
buttonContent :: NanoUI () -> NanoUI Bool
iconButton :: Svg -> Text -> NanoUI Bool
-- ANCHOR_END: buttons

button = N.button
button' = N.button'
buttonWith = N.buttonWith
buttonContent = N.buttonContent
iconButton = N.iconButton

-- ANCHOR: overlays
modal  :: Var Bool -> Text -> NanoUI () -> NanoUI ()
window :: Var Bool -> Text -> NanoUI () -> NanoUI ()
popup  :: Var Bool -> PopupConfig -> NanoUI () -> NanoUI ()
-- ANCHOR_END: overlays

modal = N.modal
window = N.window
popup = N.popup
