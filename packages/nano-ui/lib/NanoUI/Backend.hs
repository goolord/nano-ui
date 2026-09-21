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
-- the loop. Nothing here shadows a name in "NanoUI": the input types it
-- re-exports are the same ones "NanoUI" has, so importing both is safe.
--
-- A backend's frame is: collect events into an 'Input', run the view with
-- "NanoUI.Runner" or "NanoUI.Testing", take the 'Damage' and present the
-- rectangles it names. "NanoUI.Context" holds the state that outlives a
-- frame, and the font callbacks below are what it measures text with.
module NanoUI.Backend
  ( -- * Running a view

    -- | These run one view against a @Context@ from "NanoUI.Context" and an
    -- 'Input'. Most backends want a whole frame instead: @runFrame@ in
    -- "NanoUI.Testing", or the runners in "NanoUI.Runner", which lay out,
    -- paint and collect damage around a call to these.
    runUi
  , runNanoUI

    -- * Input

    -- | Start from 'emptyInput' each frame, fold the window's events into
    -- it, and hand it to the frame runner. Keys arrive through
    -- 'appendInputKey', mouse buttons through 'applyMouseButton' and dropped
    -- files through 'appendDropEvent'; typed characters belong in
    -- 'inputChars' rather than as keys. 'clearEphemeral' drops what only the
    -- frame it arrived in should see, for a frame redrawn without new events.
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
  , emptyDropEvents
  , inputInteracted
  , inputPointerHeld
  , emptyInputKeys
  , inputKeysFromList
  , inputKeysNull
  , foldInputKeys

    -- * Fonts

    -- | A backend installs a 'FontBackend' on the context and nano-ui calls
    -- back into it to measure and shape. 'prepareFontMetrics' builds the
    -- 'FontMetrics' a size and family is measured through, 'drawShaped'
    -- shapes a run into 'ShapedGlyphs', and 'drawGlyph' gives one glyph's
    -- 'GlyphQuad' for the atlas.
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

import NanoUI.Compact (Compact, askCompact, compactHost)
import NanoUI.Draw.Text (drawTextBox)
import NanoUI.Font
  ( FontBackend (..)
  , FontMetrics (..)
  , GlyphQuad (..)
  , ScrollBarSlot (..)
  , ShapedGlyphs (..)
  , ShapedText (..)
  , drawGlyph
  , drawShaped
  , lineWidth
  , lineWidthIO
  , measureTextIO
  , monospaceMetrics
  , prepareFontMetrics
  , prepareFontMetricsMany
  , scaleFontMetrics
  , scrollBarGutter
  , scrollBarWidth
  , treeItemPadding
  , widgetContentInset
  , widgetPadding
  )
import NanoUI.Id
  ( IdContext
  , hashWidgetId
  , initialIdContext
  , mix64
  , mixFnv
  , widgetId
  )
import NanoUI.Input
  ( DropEvent (..)
  , DropType (..)
  , Input (..)
  , Key (..)
  , Modifiers (..)
  , MouseButton (..)
  , appendDropEvent
  , appendInputKey
  , applyMouseButton
  , clearEphemeral
  , emptyDropEvents
  , emptyInput
  , emptyInputKeys
  , foldInputKeys
  , inputInteracted
  , inputKeysFromList
  , inputKeysNull
  , inputPointerHeld
  )
import NanoUI.Monad
  ( burstNextIds
  , damageFullNow
  , damageGroupNow
  , damageKeyNow
  , damageRectNow
  , damageWidgetNow
  , runNanoUI
  , runUi
  , uiFontMetrics
  )
import NanoUI.Style (windowMargin, windowPad)
import NanoUI.Types
  ( Damage (..)
  , DamageBounds (..)
  , defaultDamageSlop
  , haloDamageSlop
  , resolveDamageRect
  , sliderDamageSlop
  )
