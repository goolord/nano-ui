
-- | Implementation of "NanoUI.Monad", plus running a view against a context,
-- reading and swapping the context, and id-frame plumbing. The definitions
-- live in "NanoUI.Internal.Monad.Core" (the monad, scopes, themes, fonts,
-- damage), "NanoUI.Internal.Monad.Focus" (focus, clipboard, Escape),
-- "NanoUI.Internal.Monad.Scroll" and "NanoUI.Internal.Monad.Explain"; this
-- module is their export list.
module NanoUI.Internal.Monad
  ( NanoUI (..)
  , NanoUIEs
  , Ui
  , runNanoUI
  , runUi
  , embedNanoUI
  , withRunInNanoUI
  , liftIO
  , withContext
  , withUiResource
  , withBuildScope
  , catchView
  , withKey
  , keyedTag
  , scope
  , withIdFrame
  , nextId
  , freshWidget
  , burstNextIds
  , currentId
  , askContext
  , askInput
  , askFrameInput
  , localInput
  , inertView
  , askDefaultLayout
  , withDefaultLayout
  , askHost
  , askBackendSession
  , uiFontMetrics
  , uiFontSize
  , resolveFontUi
  , lineWidthUi
  , truncateTextUi
  , truncateTextWithUi
  , uiTime
  , uiTheme
  , setTheme
  , followSystemTheme
  , systemAppearance
  , styled
  , themed
  , disabledWhen
  , uiMousePos
  , mousePressed
  , mouseReleased
  , mouseHeld
  , windowSize
  , windowWidth
  , windowHeight
  , lastRect
  , holdFocus
  , releaseFocus
  , focusedWidget
  , requestFocus
  , focusNext
  , focusPrevious
  , clearFocus
  , isFocused
  , getClipboard
  , setClipboard
  , requestFrame
  , takeEscape
  , explainLayout
  , explainingLayout
  , explainedNode
  , explainScope
  , withArenaRange
  , inArenaRange
  , getScrollTuning
  , setScrollTuning
  , getScrollMetrics
  , setScrollOffset
  , scrollTo
  , scrollBy
  , scrollPages
  , scrollToStart
  , scrollToEnd
  , scrollIntoView
  , scrollRectIntoView
  , setScrollStep
  , damageWidgetNow
  , damageKeyNow
  , damageRectNow
  , damageGroupNow
  , damageFullNow
  , whenM
  , unlessM
  , ifM
  , (<&&>)
  )
where

import Control.Monad.IO.Class (liftIO)
import NanoUI.Internal.Monad.Core
import NanoUI.Internal.Monad.Explain
import NanoUI.Internal.Monad.Focus
import NanoUI.Internal.Monad.Scroll
