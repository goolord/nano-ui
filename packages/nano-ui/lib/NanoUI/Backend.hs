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
-- share (the input types, 'FontMetrics' and 'lineWidth') are the same
-- entities in both, so importing both is safe.
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
    -- arrive through 'appendInputKey', mouse buttons through
    -- 'applyMouseButton' and dropped files through 'appendDropEvent'; typed
    -- characters belong in 'inputChars' rather than as keys.
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
  , modifiersFromBits

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
  )
where

import NanoUI.Internal.Compact (Compact, askCompact, compactHost)
import NanoUI.Internal.Draw (drawTextBox)
import NanoUI.Internal.Font
import NanoUI.Internal.Id
import NanoUI.Internal.Input
import NanoUI.Internal.Monad
import NanoUI.Internal.Style (windowMargin, windowPad)
import NanoUI.Internal.Types
