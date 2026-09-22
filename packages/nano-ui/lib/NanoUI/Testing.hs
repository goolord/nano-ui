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
  , wrapMeasureCache
  , withExternalText
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
    -- * Compact
  , Compact
  , compactHost
  , askCompact
    -- * Text measurement
  , lineWidth
  , textIndexAtX
  , WrapResult (..)
  , wrapTextIO
  , wrapTextLinesIO
  , caretX
  , selectionSpans
  , textNodeFontWeight
  , textNodeFontStyle
  , textNodeTextDecoration
  ) where

import NanoUI.Internal.Compact (Compact, askCompact, compactHost)
import NanoUI.Internal.Context
  ( Context (..)
  , FrameMsg (..)
  , anyAnimating
  , AtlasUpload (..)
  , atlasChanges
  , atlasSnapshot
  , atlasTextureId
  , clearDirty
  , clearMeasureCache
  , ctxTheme
  , DamageRequest (..)
  , damageFull
  , damageKey
  , damagePeers
  , damageRect
  , damageWidget
  , decodeMessages
  , enableMeasureCache
  , getAnimationValue
  , getFocusId
  , getHotId
  , getPrevRect
  , getPrevClipRect
  , getScrollOffset
  , setScrollOffset
  , getStore
  , isDirty
  , markDirty
  , modalActive
  , overlayConsumesQuit
  , reduceMessages
  , reduceUpdates
  , registerImage
  , registerImages
  , requestDamage
  , setAnimationValue
  , setHost
  , setWakeLoop
  , requestWakeAt
  , requestWakeAfter
  , getWakeAt
  , startAnimation
  , startAnimationEase
  , startAnimationEaseDelay
  , startSpring
  , takeDamage
  , takeDamagePieces
  , textInputEditActive
  , withClipboard
  , withExternalText
  , withFontMetrics
  , withMeasureText
  , withFontResolver
  , withMonoFontMetrics
  , withTheme
  , setTheme
  , getTheme
  , wrapMeasureCache
  )
import NanoUI.Internal.Context (newContext, newPixelHostContext)
import NanoUI.Internal.Frame.SpanArena (SpanArena, foldSpanArena, spanArenaCount)
import NanoUI.Internal.Draw
  ( DrawCmd (..)
  , DrawData (..)
  , DrawOp (..)
  , Layer (..)
  , backdropDimTextureId
  , glyphAtlasTextureId
  , glyphAtlasPages
  , glyphPageTextureId
  , textureGlyphPage
  , drawCmdElems
  , forDrawCmdsInLayer_
  , drawCmdNull
  , drawCmdCount
  , drawTextBox
  , indexSize
  , vertexSize
  )
import NanoUI.Internal.Damage (damagePieces, floatingPanelRects)
import NanoUI.Internal.Font (WrapResult (..), caretX, lineWidth, selectionSpans, sliderTrackBounds, textIndexAtX, wrapTextIO, wrapTextLinesIO)
import NanoUI.Internal.Widgets.ColorPicker
  ( colorPickerSvSquare
  , widgetStoreBaseColor
  , widgetStoreColor
  )
import NanoUI.Internal.Frame
  ( UiCursorKind (..)
  , collectOverlayTextSpans
  , collectRasterSpans
  , collectTextSpans
  , cursorKindIs
  , debugPanelOpen
  , floatingPanelActive
  , needsRedraw
  , pointerCursorWanted
  , pointerDragActive
  , runFrame
  , runFrameEff
  , runFrameReduce
  , runFrameReduceEff
  , textFieldActive
  , uiCursorKind
  , widgetNodeCount
  )
import NanoUI.Internal.Frame.Scroll (ScrollBarLayout (..), scrollBarLayout)
import NanoUI.Internal.Layout.Solve (computePopupPosition)
import NanoUI.Internal.Monad (Ui, askContext, askHost, askInput, uiIO)
import NanoUI.Internal.WidgetText (textNodeFontStyle, textNodeFontWeight, textNodeTextDecoration)
import NanoUI.Internal.Types (Damage (..), damageIsEmpty)
import Effectful (Eff, IOE, runEff, type (:>))

-- | A headless context for tests: 16px monospace metrics, the measure cache
-- on, text kept out of the vertex buffer, and the default theme.
newPixelContext :: IO Context
newPixelContext = newPixelHostContext
