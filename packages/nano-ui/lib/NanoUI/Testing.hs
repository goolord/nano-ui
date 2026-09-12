-- | Deterministic frame execution and render inspection for tests and tools.
-- Application code should use backend runners ('NanoUI.Backend.Term',
-- 'NanoUI.Backend.Sdl') instead of this module.
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
  , colorPickerGeom
  , ColorPickerGeom (..)
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
  , stopAnimation
  , startSpring
  , anyAnimating
    -- * Images
  , registerImage
  , registerImages
  , atlasTextureId
  , atlasSnapshot
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
  , LayerSlice (..)
  , drawCmdNull
  , drawCmdElems
  , forDrawCmdsInLayer_
  , drawCmdCount
  , vertexSize
  , indexSize
  , backdropDimTextureId
  , glyphAtlasTextureId
  , Damage (..)
  , takeDamage
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
  , textDisplayWidth
  , lineWidth
  , textIndexAtX
  , textNodeFontWeight
  , textNodeFontStyle
  , textNodeTextDecoration
  ) where

import NanoUI.Compact (Compact, askCompact, compactHost)
import NanoUI.Context
  ( Context (..)
  , FrameMsg (..)
  , anyAnimating
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
  , startAnimation
  , startAnimationEase
  , startAnimationEaseDelay
  , stopAnimation
  , startSpring
  , takeDamage
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
import NanoUI.Context (newContext, newPixelHostContext)
import NanoUI.Frame.SpanArena (SpanArena, foldSpanArena, spanArenaCount)
import NanoUI.Draw
  ( DrawCmd (..)
  , DrawData (..)
  , DrawOp (..)
  , Layer (..)
  , LayerSlice (..)
  , backdropDimTextureId
  , glyphAtlasTextureId
  , drawCmdElems
  , forDrawCmdsInLayer_
  , drawCmdNull
  , drawCmdCount
  , drawTextBox
  , drawVertices
  , indexSize
  , vertexSize
  )
import NanoUI.Damage (floatingPanelRects)
import NanoUI.Font (lineWidth, sliderTrackBounds, textDisplayWidth, textIndexAtX)
import NanoUI.Widgets.ColorPicker
  ( ColorPickerGeom (..)
  , colorPickerGeom
  , widgetStoreBaseColor
  , widgetStoreColor
  )
import NanoUI.Frame
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
import NanoUI.Frame.Scroll (ScrollBarLayout (..), scrollBarLayout)
import NanoUI.Layout.Solve (computePopupPosition)
import NanoUI.Monad (Ui, askContext, askHost, askInput, uiIO)
import NanoUI.WidgetText (textNodeFontStyle, textNodeFontWeight, textNodeTextDecoration)
import NanoUI.Types (Damage (..), damageIsEmpty)
import Effectful (Eff, IOE, runEff, type (:>))

-- | Pixel-host context with SDL-like defaults for headless tests.
newPixelContext :: IO Context
newPixelContext = newPixelHostContext
