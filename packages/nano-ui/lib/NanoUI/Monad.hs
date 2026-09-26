-- | The 'Ui' effect and the 'NanoUI' view type: widget id scopes and keys,
-- the frame's input and time, theme scopes, focus, clipboard, scrolling,
-- and damage requests from inside a view. "NanoUI" re-exports the names a
-- view usually needs; this module has the rest. Running a view against a
-- context is in "NanoUI.Backend".
module NanoUI.Monad
  ( -- * The effect
    NanoUI
  , NanoUIEs
  , Ui
  , uiIO
  , withUiResource
  , emit

    -- * Widget ids
  , withKey
  , keyedTag
  , scope
  , nextId
  , currentId

    -- * Input and time
  , askInput
  , askFrameInput
  , localInput
  , uiMousePos
  , uiTime
  , windowSize
  , windowWidth
  , windowHeight
  , lastRect
  , takeEscape

    -- * Layout, fonts and themes
  , askDefaultLayout
  , withDefaultLayout
  , uiFontMetrics
  , resolveFontUi
  , lineWidthUi
  , uiTheme
  , setUiTheme
  , styled
  , themed
  , disabledWhen
  , followSystemThemeUi
  , systemAppearance

    -- * Focus and clipboard
  , holdFocus
  , releaseFocus
  , focusedWidget
  , requestFocus
  , getClipboard
  , setClipboard

    -- * Host
  , askHost

    -- * Scrolling
  , getScrollMetricsUi
  , setScrollOffsetUi
  , scrollToUi
  , scrollByUi
  , scrollPagesUi
  , scrollRectIntoViewUi
  , setScrollStepUi

    -- * Redraw and damage
  , requestFrame
  , damageWidgetNow
  , damageKeyNow
  , damageRectNow
  , damageGroupNow
  , damageFullNow

    -- * Monadic helpers
  , whenM
  , unlessM
  , ifM
  , (<&&>)
  )
where

import NanoUI.Internal.Monad
