-- | Deterministic frame execution and render inspection for tests and tools.
-- Application code should use a backend's runner, such as
-- @runSdlApp@ in @NanoUI.Backend.Sdl@, instead of this module.
module NanoUI.Testing
  ( -- * Frame
    runFrame
  , runFrameEff
  , runFrameReduce
  , runFrameReduceEff
  , needsRedraw
  , pointerDragActive
  , textFieldActive
  , textInputArea
  , TextInputArea (..)
  , floatingPanelActive
  , floatingPanelRects
  , debugPanelOpen
  , widgetNodeCount
  , pointerCursorWanted
  , cursorKindIs
  , uiCursorKind
  , UiCursorKind (..)
  , computePopupPosition
  , scrollBarLayout
  , ScrollBarLayout (..)
  , sliderTrackBounds
  , colorPickerSvSquare
  , widgetStoreBaseColor
  , widgetStoreColor
  , collectTextSpans
  , collectRasterSpans
  , collectOverlayTextSpans
  , ctxSpanBase
  , ctxSpanOverlay
  , SpanArena
  , spanArenaCount
  , foldSpanArena
    -- * Context
  , Context
  , newContext
  , newPixelContext
  , ctxTheme
  , ctxPaintFull
  , ctxFontMetrics
  , setHost
  , askHost
  , withFontMetrics
  , withMonoFontMetrics
  , withMeasureText
  , withFontResolver
  , withFontSize
  , wrapMeasureCache
  , enableMeasureCache
  , withTheme
  , setTheme
  , getTheme
  , markDirty
  , clearDirty
  , clearMeasureCache
  , isDirty
  , setWakeLoop
  , requestWakeAt
  , requestWakeAfter
  , getWakeAt
  , DamageRequest (..)
  , requestDamage
  , damageWidget
  , damageKey
  , damageRect
  , damagePeers
  , damageFull
  , getHotId
  , getFocusId
  , getPrevRect
  , getPrevClipRect
  , getStore
  , getScrollOffset
  , setScrollOffset
  , textInputEditActive
  , modalActive
  , overlayConsumesQuit
  , withClipboard
  , getAnimationValue
  , setAnimationValue
  , startAnimation
  , startAnimationEase
  , startAnimationEaseDelay
  , startSpring
  , anyAnimating
    -- * Images
  , registerImage
  , registerImages
  , atlasTextureId
  , atlasSnapshot
  , atlasChanges
  , AtlasUpload (..)
    -- * Messages
  , FrameMsg (..)
  , decodeMessages
  , reduceMessages
  , reduceUpdates
    -- * Draw
  , DrawData (..)
  , DrawCmd (..)
  , DrawOp (..)
  , Shade (..)
  , drawTextBox
  , Layer (..)
  , drawCmdNull
  , drawCmdElems
  , forDrawCmdsInLayer_
  , drawCmdCount
  , vertexSize
  , indexSize
  , backdropDimTextureId
  , glyphAtlasTextureId
  , glyphAtlasPages
  , glyphPageTextureId
  , textureGlyphPage
  , Damage (..)
  , takeDamage
  , takeDamagePieces
  , damagePieces
  , damageIsEmpty
    -- * Effectful
  , Eff
  , runEff
  , IOE
  , type (:>)
  , askContext
  , askInput
  , Ui
  , uiIO
    -- * Text measurement
  , lineWidth
  , textIndexAtX
  , WrapResult (..)
  , wrapTextIO
  , wrapTextLinesIO
  , caretX
  , selectionSpans
  , textNodeFontVariant
  , textNodeFontTone
  , textNodeFontWeight
  , textNodeFontStyle
  , textNodeTextDecoration
  ) where

import NanoUI.Internal.Context
import NanoUI.Internal.Frame.SpanArena (SpanArena, foldSpanArena, spanArenaCount)
import NanoUI.Internal.Draw
import NanoUI.Internal.Damage (damagePieces, floatingPanelRects)
import NanoUI.Internal.Font (WrapResult (..), caretX, lineWidth, selectionSpans, sliderTrackBounds, textIndexAtX, wrapTextIO, wrapTextLinesIO)
import NanoUI.Internal.Widgets.ColorPicker
import NanoUI.Internal.Frame (runFrame, runFrameEff, runFrameReduce, runFrameReduceEff)
import NanoUI.Internal.Frame.Cursor (UiCursorKind (..), cursorKindIs, pointerCursorWanted, uiCursorKind)
import NanoUI.Internal.Frame.Input
import NanoUI.Internal.Frame.Spans (collectOverlayTextSpans, collectRasterSpans, collectTextSpans, widgetNodeCount)
import NanoUI.Internal.Frame.Scroll (ScrollBarLayout (..), scrollBarLayout)
import NanoUI.Internal.Frame.TextArea (TextInputArea (..), textInputArea)
import NanoUI.Internal.Layout.Solve (computePopupPosition)
import NanoUI.Internal.Monad (Ui, askContext, askHost, askInput, uiIO)
import NanoUI.Internal.WidgetText (textNodeFontStyle, textNodeFontTone, textNodeFontVariant, textNodeFontWeight, textNodeTextDecoration)
import NanoUI.Internal.Types (Damage (..), damageIsEmpty)
import Effectful (Eff, IOE, runEff, type (:>))
