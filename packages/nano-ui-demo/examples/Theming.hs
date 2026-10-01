-- | Theming: pick a theme, restyle part of a view, and colour your own drawing from the theme.
--
-- The context holds one base theme for the whole app. This example sets it
-- with 'setUiTheme' on every frame, from the radio choice. That is free: an
-- unchanged theme is a no-op, and only a real change repaints the window.
-- "Follow system" reads the desktop's light/dark preference with
-- 'systemAppearance' and maps it to a theme with 'lightDark'.
--
-- Everything else is a theme modifier, a plain @Theme -> Theme@ function.
-- 'styled' applies one to a part of the view, scopes nest, and modifiers
-- compose with @(.)@ like layout modifiers do. Because a modifier changes
-- whatever theme is around it rather than naming colours of its own, the
-- same 'primary' or 'pill' looks right in every theme in the list. Flip
-- through them and watch the buttons and the "Brand" panel follow along.
--
-- Your own drawing ('box', a 'fontColor' label) has no theme of its own, so
-- read the theme with 'uiTheme' and take colours from it ('themeAccent',
-- 'themeSeries'). See Animation.hs for animating a colour.
--
-- Run it with @cabal run nano-ui-example-theming@.
module Main (main) where

import Control.Monad (forM_)
import NanoUI
import NanoUI.Backend.Sdl (SdlOptions (..), defaultSdlOptions, runSdlApp)
import qualified Data.Text as T

main :: IO ()
main =
  runSdlApp
    defaultSdlOptions
      { sdlAppShouldQuit = pressedOnceIn KeyEscape
      , sdlWindowSettings = defaultWindowSettings {wsTitle = "Theming", wsSize = Size 760 640}
      }
    view

data ThemeChoice
  = FollowSystem
  | Dark
  | Light
  | TomorrowNight
  | TomorrowLight
  | TomorrowMidnight
  | Gruvbox
  deriving (Eq, Show, Enum, Bounded)

choiceName :: ThemeChoice -> T.Text
choiceName = \case
  FollowSystem -> "Follow system"
  Dark -> "Default dark"
  Light -> "Default light"
  TomorrowNight -> "Tomorrow Night Min"
  TomorrowLight -> "Tomorrow Min Light"
  TomorrowMidnight -> "Tomorrow Midnight Min"
  Gruvbox -> "Gruvbox (from Base16)"

-- | The theme for a choice. Only "Follow system" needs the view, to ask the
-- backend for the desktop's appearance (Nothing when it cannot tell, which
-- 'lightDark' treats as dark).
choiceTheme :: ThemeChoice -> NanoUI Theme
choiceTheme = \case
  FollowSystem -> lightDark defaultLightTheme defaultTheme <$> systemAppearance
  Dark -> pure defaultTheme
  Light -> pure defaultLightTheme
  TomorrowNight -> pure tomorrowNightMinDarkTheme
  TomorrowLight -> pure tomorrowMinLightTheme
  TomorrowMidnight -> pure tomorrowMidnightMinDarkTheme
  Gruvbox -> pure gruvboxTheme

-- | A whole theme from 16 colours. 'themeFromBase16' picks the dark or light
-- mapping by comparing the background with the foreground. A top-level
-- value, so it is built once rather than every frame.
gruvboxTheme :: Theme
gruvboxTheme =
  themeFromBase16
    Base16
      { base00 = colorRGB 40 40 40
      , base01 = colorRGB 60 56 54
      , base02 = colorRGB 80 73 69
      , base03 = colorRGB 102 92 84
      , base04 = colorRGB 189 174 147
      , base05 = colorRGB 213 196 161
      , base06 = colorRGB 235 219 178
      , base07 = colorRGB 251 241 199
      , base08 = colorRGB 251 73 52
      , base09 = colorRGB 254 128 25
      , base0A = colorRGB 250 189 47
      , base0B = colorRGB 184 187 38
      , base0C = colorRGB 142 192 124
      , base0D = colorRGB 131 165 152
      , base0E = colorRGB 211 134 155
      , base0F = colorRGB 214 93 14
      }

-- | A reusable modifier of your own: a rounded main-action button. It is
-- built from other modifiers, so it inherits each theme's accent.
pill :: Theme -> Theme
pill = buttonStyle (cornerRadius 14) . primary

-- | A section restyled as a unit: a fixed brand accent (checkboxes, sliders
-- and 'primary' buttons pick it up) and a panel with a bar down its left.
brand :: Theme -> Theme
brand = accentColor magenta . panelStyle (borderLeft 4 magenta . cornerRadius 2)
  where
    magenta = colorRGB 214 51 132

view :: NanoUI ()
view = do
  (choice, setChoice) <- useEnum FollowSystem
  (agree, setAgree) <- useFlag True
  (level, setLevel) <- useFloat 40
  scrollWith (padAll 20 . grow) $
    columnWith (tight . gap 16 . fillW) $ do
      rowWith (tight . gap 24 . fillW) $ do
        columnWith (tight . gap 8) $ do
          heading "Theme"
          picked <- boundedRadio choiceName choice
          setChoice picked
          -- Every frame; a no-op unless the choice (or the system) changed.
          setUiTheme =<< choiceTheme picked

        columnWith (tight . gap 10 . fillW) $ do
          heading "Button modifiers"
          rowWith (wrap . tight . gap 8 . fillW) $ do
            _ <- button "Plain"
            _ <- styled primary (button "Primary")
            _ <- styled destructive (button "Delete")
            _ <- styled success (button "Approve")
            _ <- styled warning (button "Override")
            _ <- styled subtle (button "Subtle")
            pure ()
          muted "Your own modifier, composed with (.):"
          rowWith (tight . gap 8) $ do
            _ <- styled pill (button "Pill")
            -- Modifiers stack: a pill that is also destructive.
            _ <- styled (destructive . pill) (button "Pill, destructive")
            pure ()

      -- 'styled' applies to everything inside, including nested panels and
      -- widgets, and to nothing outside.
      styled brand $
        panelWith (padAll 14 . gap 8 . fillW) $ do
          heading "Brand section"
          setAgree =<< checkbox "Uses the brand accent" agree
          setLevel =<< slider 0 100 level
          _ <- styled pill (button "Pill, in brand colours")
          pure ()

      -- Colours read from the theme, for things the theme does not style
      -- itself. They change with the theme like the widgets do.
      theme <- uiTheme
      card $ do
        labelWith (tight . fontColor (themeAccent theme) . fontMedium) "Drawn in themeAccent"
        rowWith (tight . gap 6) $
          forM_ (themeSeries theme) $ \c -> box (fixedWH 32 32) c
        muted "themeSeries: red, orange, yellow, green, accent, purple"
