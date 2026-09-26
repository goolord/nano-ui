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
    -- instead forgets a held button and the pointer between events.
    --
    -- Keys go through 'applyKey', which tracks presses, releases and held
    -- keys; mouse buttons through 'applyMouseButton' (numbered with
    -- 'mouseButtonNumber'); dropped files through 'appendDropEvent'. When the
    -- pointer leaves the window, 'applyPointerLeave' moves it off every
    -- widget. When the window loses keyboard focus, 'releaseAllKeys' releases
    -- the held keys, whose release events go elsewhere.
    --
    -- Every key goes in as a 'Key' whatever the modifiers; a key that types a
    -- character is the 'KeyChar' it types with no modifier held. The typed
    -- text also goes in 'inputChars'; a chord such as Ctrl+C types none.
    -- Auto-repeats go in as presses, which 'applyKey' keeps out of
    -- 'inputKeysNew'. 'keypadKey' maps keypad keys.
    --
    -- A frame does not keep the order of its text, keys and modifiers, so
    -- 'NanoUI.Runner.runSessionLoop' ends a frame after a command key when
    -- text, another key or a modifier change follows. A frame's text thus
    -- comes before its one command key ('inputKeys'). A backend's own loop
    -- should batch events the same way.
  , Input (..)
  , Key (..)
  , Modifiers (..)
  , MouseButton (..)
  , mouseButtonNumber
  , MouseButtons
  , DropEvent (..)
  , DropType (..)
  , emptyInput
  , applyMouseButton
  , applyPointerLeave
  , noButtons
  , buttonsMember
  , buttonsNull
  , buttonsInsert
  , buttonsDelete
  , buttonsToList
  , buttonsFromList
  , clearEphemeral
  , appendInputKey
  , appendDropEvent
  , inputInteracted
  , inputPointerHeld
  , inputKeysFromList
  , inputKeysNull
  , foldInputKeys
  , applyKey
  , releaseAllKeys
  , keypadKey
  , noModifiers
  , modifiersFromBits

    -- * Input methods

    -- | An input method (IME) composes text before committing it, such as
    -- Japanese kana before conversion to kanji. Fold each composition update
    -- into the input with 'applyComposition'. The composition is held, like
    -- a button, until the next update ends or replaces it, and the focused
    -- text field draws it at its caret. Committed text arrives as typed text
    -- in 'inputChars'.
    --
    -- After a frame, 'textInputArea' says whether a widget takes text, where
    -- to put the candidate window, and what kind of text it takes
    -- ('InputPurpose'). Enable the platform's text input only while it is
    -- 'Just', so no IME composes where nothing shows it and no on-screen
    -- keyboard stays up. 'getFocusId' gives the focused widget; drop any
    -- composition in progress when focus moves.
  , Composition (..)
  , applyComposition
  , InputPurpose (..)
  , TextInputArea (..)
  , textInputArea
  , getFocusId

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

    -- | Control the @explainLayout@ overlay from outside the view, for a
    -- backend option or a test harness.
  , setExplainLayout
  , getExplainLayout
  , getExplainedNode

    -- * System appearance

    -- | A backend that can read the desktop's light or dark setting reports
    -- it with 'setSystemAppearance' before the first frame and whenever it
    -- changes. Views read it with @systemAppearance@; a context set up with
    -- @followSystemTheme@ switches theme to match.
  , Appearance (..)
  , setSystemAppearance
  , getSystemAppearance

    -- * The native window

    -- | A backend opens its window from a 'NanoUI.WindowSettings' with its
    -- title, size, mode, resizability and transparency, then calls
    -- 'installWindowHost', which applies the remaining settings through the
    -- 'WindowHost'. The host carries out view requests such as
    -- 'NanoUI.setWindowTitleUi' and 'NanoUI.moveWindowUi'. Build it by
    -- updating 'defaultWindowHost', so fields added later are no-ops.
    --
    -- Each frame, before the view runs, report the window's scale, position,
    -- focus and mode with 'reportWindowState' (views read it with
    -- 'NanoUI.askWindow'). Once the frame is on screen, call
    -- 'answerScreenshots' with a capture of it.
    --
    -- @runSessionLoop@ in "NanoUI.Runner" handles closing: a close request
    -- either ends the session or, per the settings, is passed to the view,
    -- and 'NanoUI.quitUi' ends it. A backend's own loop does the same with
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

    -- | Background jobs (@useTaskStatus@, @useTask@, @useStream@) and
    -- threads calling the action from @askWake@ wake the loop through the
    -- action installed with 'setWakeLoop' before the first frame. It must be
    -- callable from any thread and end the loop's wait for events, for
    -- example by pushing a custom event onto the platform queue. Without it,
    -- a blocking loop shows a job's result only when other input arrives.
    --
    -- Jobs run until their hooks stop being called. @runSessionLoop@ in
    -- "NanoUI.Runner" cancels the rest when it returns; a host that runs
    -- frames itself calls 'cancelTasks' when it closes the session.
  , setWakeLoop
  , cancelTasks
  )
where

import NanoUI.Internal.Compact (Compact, askCompact, compactHost)
import NanoUI.Internal.Context (getExplainLayout, getExplainedNode, getFocusId, getSystemAppearance, setExplainLayout, setSystemAppearance, setWakeLoop)
import NanoUI.Internal.Frame.TextArea (TextInputArea (..), textInputArea)
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
