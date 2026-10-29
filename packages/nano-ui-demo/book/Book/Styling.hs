-- | Code for the book's Styling & Theming chapter
-- (@book/src/styling/index.md@).
module Book.Styling where

import NanoUI
import NanoUI.Backend.Sdl (NanoUIFont (..), SdlOptions (..), defaultSdlOptions)

themedViews, quietSkip, savingButtons, warningLabel :: NanoUI ()

-- ANCHOR: panel_page
-- An app with no panel of its own: the panel surface is the page.
panelPageOptions :: SdlOptions
panelPageOptions = defaultSdlOptions {sdlAppTheme = Just (panelPage defaultTheme)}

-- Or scope it to a page with no panels.
body :: NanoUI ()
body = styled panelPage (columnWith (padAll 12) (label "Body"))
-- ANCHOR_END: panel_page

-- ANCHOR: scoped
themedViews = do
  -- Replace the base theme for a subtree.
  themed defaultLightTheme mainContent

  -- Modify the active theme for a subtree.
  whenM (styled (primary . rounded 999) (button "Continue")) continue
-- ANCHOR_END: scoped

-- ANCHOR: quiet
quiet :: Theme -> Theme
quiet t = (subtle . buttonStyle (foreground (themeMuted t))) t

quietSkip = whenM (styled quiet (button "Skip")) skip
-- ANCHOR_END: quiet

-- ANCHOR: disabled
savingButtons = do
  isSaving <- useVar False
  saving <- readVar isSaving
  disabledWhen saving $ row $ do
    whenM (button "Cancel") cancel
    whenM (button "Confirm") confirm
-- ANCHOR_END: disabled

-- ANCHOR: font_modifiers
warningLabel = labelWith (fontMono . fontSize 15 . fontTone Warning) "warning: low disk"
-- ANCHOR_END: font_modifiers

-- ANCHOR: fonts
fontOptions :: SdlOptions
fontOptions =
  defaultSdlOptions
    { sdlAppFont = FontSearch ["Segoe UI", "SF Pro Text", "Helvetica Neue"]
    , sdlAppMonoFont = FontSearch ["Cascadia Mono", "JetBrains Mono", "Consolas"]
    , sdlAppFontSize = 14
    }
-- ANCHOR_END: fonts

-- Stand-ins for application code the snippets refer to.

mainContent, continue, skip, cancel, confirm :: NanoUI ()
mainContent = pure ()
continue = pure ()
skip = pure ()
cancel = pure ()
confirm = pure ()
