-- | The draw context a custom widget sees, built by the widget itself
-- ("NanoUI.Widgets.Custom") and by the paint, damage and cursor passes.
module NanoUI.Internal.Widgets.Custom
  ( mkCustomDrawContext
  , customDrawContext
  ) where

import Data.IORef (readIORef)
import NanoUI.Internal.Context (Context (..), CustomDrawContext (..), getFocusId, getHotId, isDisabled, widgetTheme)
import NanoUI.Internal.Font (FontMetrics)
import NanoUI.Internal.Id (WidgetId)

-- | Build the draw context a custom widget sees, resolving hover/press/focus
-- state for @wid@ from the ambient context. One policy for state masking.
mkCustomDrawContext ::Context -> FontMetrics -> WidgetId -> IO CustomDrawContext
mkCustomDrawContext ctx fm wid = do
  hot <- getHotId ctx
  active <- readIORef (ctxActiveId ctx)
  customDrawContext ctx fm wid (hot == wid) (active == wid)

-- | Draw context for @wid@ with the given hover and press state; a disabled
-- widget is never hovered or pressed.
customDrawContext :: Context -> FontMetrics -> WidgetId -> Bool -> Bool -> IO CustomDrawContext
customDrawContext ctx fm wid hovered pressed = do
  disabled <- isDisabled ctx wid
  focused <- (== wid) <$> getFocusId ctx
  active <- readIORef (ctxActiveId ctx)
  theme <- widgetTheme ctx wid
  pure
    CustomDrawContext
      { cdcHovered = hovered && not disabled
      , cdcPressed = pressed && not disabled
      , cdcFocused = focused
      , cdcActive = active == wid
      , cdcDisabled = disabled
      , cdcTheme = theme
      , cdcFont = fm
      }
