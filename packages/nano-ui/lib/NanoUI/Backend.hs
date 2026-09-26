-- |
-- Module      : NanoUI.Backend
-- Description : What a window backend is built from
-- Copyright   : (c) 2026 Zachary Churchill
-- License     : MIT
-- Maintainer  : zacharyachurchill@gmail.com
--
-- The parts of nano-ui a backend needs and a view does not: filling in a
-- frame's 'Input', measuring and shaping text for the widgets, reading back
-- the rectangles a frame changed, and running a view against a context.
-- @NanoUI.Backend.Sdl@ and @NanoUI.Backend.Rgfw@ are written against this
-- module; so is anything else that drives frames itself, such as a recorder,
-- a benchmark, or a test harness.
--
-- Writing a GUI needs none of it: "NanoUI" has the widgets, layout, styling
-- and state, and a backend's own runner (@runSdlApp@, @runRgfwApp@) starts
-- the loop. Nothing here shadows a name in "NanoUI": the names the two
-- share (the input types, 'FontMetrics', 'lineWidth', 'Appearance' and
-- 'WindowState') are the same entities in both, so importing both is safe.
--
-- A backend's frame is: collect events into an 'Input', run the view with
-- "NanoUI.Runner" or "NanoUI.Testing", take the 'Damage' and present the
-- rectangles it names. The @Context@ that "NanoUI.Testing" creates holds the
-- state that outlives a frame, and the font callbacks below are what it
-- measures text with.
module NanoUI.Backend
  ( -- * Running a view

    -- | These run one view against a @Context@ from "NanoUI.Testing" and an
    -- 'Input'. Most backends want a whole frame instead: @runFrame@ in
    -- "NanoUI.Testing", or the runners in "NanoUI.Runner", which lay out,
    -- paint and collect damage around a call to these.
    runUi
  , runNanoUI

    -- * Input

    -- | Start from 'emptyInput' once. Each later frame carries the last
    -- frame's input forward through 'clearEphemeral', which drops the
    -- one-shot events (keys, typed text, clicks, scroll, drops) and keeps
    -- what is held (buttons, pointer, modifiers, window size), then folds the
    -- window's new events into it. Starting from 'emptyInput' every frame
    -- instead forgets a held button and the pointer between events. Keys
    -- arrive through 'applyKey', which records presses, releases and held
    -- keys, mouse buttons through 'applyMouseButton' and dropped files
    -- through 'appendDropEvent'. Every key goes in as a 'Key' whatever the
    -- modifiers, a key that types a character as the 'KeyChar' it types
    -- with no modifier held; the text typed goes in 'inputChars' as well, and
    -- a chord such as Ctrl+C types none. A held key's auto-repeats go in
    -- only for the keys 'keyRepeats' says repeat, and 'keypadKey' says what
    -- a keypad key is.
  , Input (..)
  , Key (..)
  , Modifiers (..)
  , MouseButton (..)
  , DropEvent (..)
  , DropType (..)
  , emptyInput
  , applyMouseButton
  , clearEphemeral
  , appendInputKey
  , appendDropEvent
  , inputInteracted
  , inputPointerHeld
  , inputKeysFromList
  , inputKeysNull
  , foldInputKeys
  , applyKey
  , keyRepeats
  , keypadKey
  , noModifiers
  , modifiersFromBits

    -- * Input methods

    -- | An input method (IME) composes text before it commits it, such as
    -- the reading of Japanese before it is converted to kanji. Fold each
    -- update of that composition into the input with 'applyComposition': it
    -- is held, like a button, until the next update ends or replaces it, and
    -- the focused text field draws it at its caret. The text the input
    -- method commits arrives as typed text in 'inputChars'. After a frame,
    -- @textInputArea@ in "NanoUI.Testing" says where the input method should
    -- put its candidate window.
  , Composition (..)
  , applyComposition

    -- * Cursors
  , cursorFallback

    -- * Fonts

    -- | A backend hands the context 'FontMetrics' whose 'fmBackend' is a
    -- 'FontBackend', and nano-ui calls back into it to measure and shape.
    -- 'prepareFontMetrics' asks it for a snapshot prepared for one text, so
    -- that text is measured with shaping; without a backend the metrics come
    -- back as they were. 'drawShaped' shapes a run into 'ShapedGlyphs', and
    -- 'drawGlyph' gives one glyph's 'GlyphQuad' from the snapshot.
  , FontMetrics (..)
  , FontBackend (..)
  , prepareFontMetrics
  , prepareFontMetricsMany
  , scaleFontMetrics
  , monospaceMetrics
  , uiFontMetrics
  , measureTextIO
  , lineWidthIO
  , lineWidth
  , drawShaped
  , drawGlyph
  , drawTextBox
  , GlyphQuad (..)
  , ShapedText (..)
  , ShapedGlyphs (..)

    -- * Damage

    -- | What changed since the last frame, so a backend can present part of
    -- the window instead of all of it. @takeDamage@ in "NanoUI.Testing"
    -- reads the frame's damage; the @Now@ functions mark a rectangle from
    -- inside a view, which a custom widget that paints outside its own node
    -- needs.
  , Damage (..)
  , DamageBounds (..)
  , defaultDamageSlop
  , sliderDamageSlop
  , haloDamageSlop
  , resolveDamageRect
  , damageWidgetNow
  , damageKeyNow
  , damageRectNow
  , damageGroupNow
  , damageFullNow

    -- * Frame metrics

    -- | The paddings and widths the widgets lay themselves out by. A backend
    -- that draws its own chrome around a view, or a test that asserts where
    -- something landed, reads them rather than repeating the numbers.
  , widgetContentInset
  , widgetPadding
  , treeItemPadding
  , ScrollBarSlot (..)
  , scrollBarGutter
  , scrollBarWidth
  , windowPad
  , windowMargin

    -- * Widget ids

    -- | How a @WidgetId@ is derived. A view uses @scope@, @withKey@ and
    -- @nextId@ from "NanoUI" instead; this is for code that keeps its own id
    -- context, or hashes one the same way nano-ui does.
  , IdContext
  , initialIdContext
  , widgetId
  , hashWidgetId
  , mix64
  , mixFnv
  , burstNextIds

    -- * Compact allocation

    -- | The compact region long-lived host state is kept in, so the
    -- collector does not walk it every frame.
  , Compact
  , compactHost
  , askCompact

    -- * Debugging

    -- | The layout overlay a view turns on with @explainLayout@, for a
    -- backend option or a harness that turns it on from outside the view.
  , setExplainLayout
  , getExplainLayout
  , getExplainedNode

    -- * System appearance

    -- | A backend that can ask the platform whether the desktop is set to
    -- light or dark colours reports it with 'setSystemAppearance' before the
    -- first frame and again when it changes. A view reads it with
    -- @systemAppearance@, and a context following it (@followSystemTheme@)
    -- switches to the theme its function gives for it.
  , Appearance (..)
  , setSystemAppearance
  , getSystemAppearance

    -- * The native window

    -- | A backend opens its window from a 'NanoUI.WindowSettings', with its
    -- title, size, mode, resizability and transparency, then installs a
    -- 'WindowHost' ('installWindowHost'), which says what it does with what
    -- a view asks of the window ('NanoUI.setWindowTitleUi',
    -- 'NanoUI.moveWindowUi' and the rest) and applies the rest of the
    -- settings through it. Build the host from 'defaultWindowHost' with a
    -- record update, so a field added later does nothing rather than break
    -- the backend. Once a frame, before the view runs, report the window's
    -- scale, position, focus and mode with 'reportWindowState', which views
    -- read with 'NanoUI.askWindow'; once a frame is on screen, call
    -- 'answerScreenshots' with a capture of it.
    --
    -- @runSessionLoop@ in "NanoUI.Runner" handles closing: a close request
    -- ends the session or, if the settings say, is shown to the view, and
    -- 'NanoUI.quitUi' ends it. A loop of the backend's own does the same with
    -- 'requestWindowClose', 'clearWindowClose' and 'quitRequested'.
  , WindowHost (..)
  , defaultWindowHost
  , installWindowHost
  , WindowState (..)
  , defaultWindowState
  , reportWindowState
  , answerScreenshots
  , requestWindowClose
  , clearWindowClose
  , quitRequested

    -- * Background work

    -- | A view's @useTaskStatus@, @useTask@ and @useStream@ jobs, and any
    -- thread that calls the action @askWake@ returns, wake the loop through
    -- the wake action the backend installs with 'setWakeLoop' before the
    -- first frame: an action any thread may call that ends the loop's wait
    -- for events, such as pushing an event of the backend's own onto the
    -- platform's queue. A loop that blocks with no wake action installed
    -- shows a job's result only once other input comes along. The jobs run
    -- until their hooks stop being called. @runSessionLoop@ in
    -- "NanoUI.Runner" ends the rest as its loop returns; a host that runs
    -- frames itself ends them with 'cancelTasks' when it closes the
    -- context's session.
  , setWakeLoop
  , cancelTasks
  )
where

import NanoUI.Internal.Compact (Compact, askCompact, compactHost)
import NanoUI.Internal.Context (getExplainLayout, getExplainedNode, getSystemAppearance, setExplainLayout, setSystemAppearance, setWakeLoop)
import NanoUI.Internal.Draw (drawTextBox)
import NanoUI.Internal.Font
import NanoUI.Internal.Id
import NanoUI.Internal.Input
import NanoUI.Internal.Monad
import NanoUI.Internal.NativeWindow
  ( WindowHost (..)
  , WindowState (..)
  , answerScreenshots
  , clearWindowClose
  , defaultWindowHost
  , defaultWindowState
  , installWindowHost
  , quitRequested
  , reportWindowState
  , requestWindowClose
  )
import NanoUI.Internal.Style (Appearance (..), windowMargin, windowPad)
import NanoUI.Internal.Tasks (cancelTasks)
import NanoUI.Internal.Types
