-- | Push buttons.
module NanoUI.Internal.Widgets.Button
  ( button
  , button'
  , buttonWith
  , buttonWith'
  , ButtonConfig (..)
  , defaultButtonConfig
  , buttonConfigured
  , buttonConfigured'
  , iconButton
  , iconButton'
  , buttonContent
  , buttonContent'
  , buttonContentWith
  , buttonContentWith'
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import NanoUI.Internal.Monad (NanoUI, uiTheme)
import NanoUI.Internal.Style (Layout (..), Style (..), Theme (..), defaultLayout)
import NanoUI.Internal.Types (Color)
import NanoUI.Internal.WidgetText (buttonFlagContent)
import NanoUI.Internal.Widgets.Adornment (Adornments, adornWidget, adornmentRow, icon, leading)
import NanoUI.Internal.Widgets.Behavior (keyActivated)
import NanoUI.Internal.Widgets.Combinators (buttonStyledEx)
import NanoUI.Internal.Widgets.Node (Response, inertResponse, respClicked, respId, setClicked, withWidgetChildren)
import NanoUI.Svg (Svg)

-- | Button with a text label. 'True' on the frame it is clicked, by pointer
-- or by Enter or Space while focused.
--
-- > whenM (button "Save") saveDocument
{-# INLINE button #-}
button :: Text -> NanoUI Bool
button txt = respClicked <$> button' txt

-- | 'button' returning its 'Response', for tooltips, anchored popups, or
-- hover state.
--
-- > help <- button' "Help"
-- > tooltip help "Open the manual"
-- > when (respClicked help) openManual
{-# INLINE button' #-}
button' :: Text -> NanoUI Response
button' = buttonWith' id

-- | 'button' with a layout modifier.
--
-- > whenM (buttonWith (fixedW 120) "Submit") submitForm
{-# INLINE buttonWith #-}
buttonWith :: (Layout -> Layout) -> Text -> NanoUI Bool
buttonWith f txt = respClicked <$> buttonWith' f txt

{-# INLINE buttonWith' #-}
-- | 'buttonWith' returning the full response; activation is in @respClicked@.
buttonWith' :: (Layout -> Layout) -> Text -> NanoUI Response
buttonWith' f txt = buttonStyledEx True txt 0 (f defaultLayout) 0

-- | A button's layout, and the adornments ("NanoUI.Adornment") it draws
-- beside its label. The label and its adornments sit together in the middle
-- of the button, the layout's gap apart.
data ButtonConfig = ButtonConfig
  { bcLayout :: !Layout
  , bcAdornments :: !Adornments
  }
  deriving (Show)

-- | The default layout and no adornments: a plain 'button'.
defaultButtonConfig :: ButtonConfig
defaultButtonConfig = ButtonConfig {bcLayout = defaultLayout, bcAdornments = mempty}

-- | 'button' with its own layout or adornments, drawn in the label's colour.
--
-- > whenM (buttonConfigured defaultButtonConfig {bcAdornments = A.trailing (A.icon chevron)} "Next") next
{-# INLINE buttonConfigured #-}
buttonConfigured :: ButtonConfig -> Text -> NanoUI Bool
buttonConfigured cfg txt = respClicked <$> buttonConfigured' cfg txt

-- | 'buttonConfigured' returning its 'Response'.
buttonConfigured' :: ButtonConfig -> Text -> NanoUI Response
buttonConfigured' (ButtonConfig lay adorns) txt = do
  resp <- buttonStyledEx True txt 0 lay 0
  taken <- adornWidget (respId resp) lay (labelColor lay) adorns
  -- A control among the adornments holds the pointer: its press is its own,
  -- and only the keyboard can click the button.
  if taken then (`setClicked` inertResponse resp) <$> keyActivated (respId resp) else pure resp

-- | The colour of a button's label: its layout's font colour, or the theme's.
labelColor :: Layout -> Theme -> Color
labelColor lay theme = fromMaybe (styleFg (themeButton theme)) (layoutFontColor lay)

-- | A button with an icon before its label, or with an empty label an icon
-- alone in a square button.
--
-- > whenM (iconButton saveIcon "Save") saveDocument
-- > whenM (iconButton gearIcon "") openSettings
{-# INLINE iconButton #-}
iconButton :: Svg -> Text -> NanoUI Bool
iconButton doc txt = respClicked <$> iconButton' doc txt

-- | 'iconButton' returning its 'Response', for a tooltip on an icon alone.
{-# INLINE iconButton' #-}
iconButton' :: Svg -> Text -> NanoUI Response
iconButton' doc = buttonConfigured' defaultButtonConfig {bcAdornments = leading (icon doc)}

-- | A button whose content is any view, as iced's buttons are: laid out in a
-- row, the layout's gap apart, and centred as a label is, with a label's
-- padding around it. Labels in it take the button's text colour. The content
-- is for display, so a press anywhere on the button is the button's.
--
-- > whenM (buttonContent (svgIcon 16 saveIcon >> label "Save")) saveDocument
{-# INLINE buttonContent #-}
buttonContent :: NanoUI () -> NanoUI Bool
buttonContent = buttonContentWith id

-- | 'buttonContent' returning its 'Response'.
{-# INLINE buttonContent' #-}
buttonContent' :: NanoUI () -> NanoUI Response
buttonContent' = buttonContentWith' id

-- | 'buttonContent' with a layout modifier.
--
-- > whenM (buttonContentWith (gap 4 . fixedW 160) (spinner >> label "Saving")) cancel
{-# INLINE buttonContentWith #-}
buttonContentWith :: (Layout -> Layout) -> NanoUI () -> NanoUI Bool
buttonContentWith f content = respClicked <$> buttonContentWith' f content

-- | 'buttonContentWith' returning its 'Response'.
buttonContentWith' :: (Layout -> Layout) -> NanoUI () -> NanoUI Response
buttonContentWith' f content = do
  let lay = f defaultLayout
  resp <- buttonStyledEx True "" 0 lay buttonFlagContent
  theme <- uiTheme
  -- The content is the button's one child, which lays out and paints as an
  -- adornment beside an empty label does.
  _ <- withWidgetChildren (respId resp) (adornmentRow lay (labelColor lay theme) content)
  pure resp
