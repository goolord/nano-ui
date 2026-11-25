-- | Deterministic frame execution and render inspection for tests and tools.
-- Application code should use a backend's runner, such as
-- @runSdlApp@ in @NanoUI.Backend.Sdl@, instead of this module.
module NanoUI.Testing
  ( -- * Frame
    runFrame
  , needsRedraw
  , pointerDragActive
  , textFieldActive
  , textInputArea
  , TextInputArea (..)
  , floatingPanelActive
  , floatingPanelRects
  , floatingWindowOpen
  , widgetNodeCount
  , NodeType (..)
  , pointerCursorWanted
  , cursorKindIs
  , uiCursorKind
  , UiCursorKind (..)
  , computePopupPosition
  , scrollBarLayout
  , ScrollBarLayout (..)
  , ScrollBarSlot (..)
  , DirTag (..)
  , sliderTrackBounds
  , colorPickerSvSquare
  , WidgetStore
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
  , setDrawReuse
  , ctxFontMetrics
  , setHost
  , Host
  , newHost
  , askHost
  , withFontMetrics
  , withMonoFontMetrics
  , withMeasureText
  , withFontResolver
  , withFontSize
  , wrapMeasureCache
  , enableMeasureCache
  , withTheme
  , setThemeIO
  , getThemeIO
  , followSystemThemeIO
  , markDirty
  , clearDirty
  , clearMeasureCache
  , isDirty
  , lastFramePasses
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
  , getScrollOffsetIO
  , setScrollOffsetIO
  , getScrollOffset2DIO
  , setScrollOffset2DIO
  , getScrollMetricsIO
  , getScrollTuningIO
  , setScrollTuningIO
  , getScrollStepIO
  , setScrollStepIO
  , scrollToIO
  , scrollByIO
  , scrollPagesIO
  , scrollToStartIO
  , scrollToEndIO
  , scrollIntoViewIO
  , scrollRectIntoViewIO
  , scrollGlidingIO
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
  , clipPixelEdges
    -- * View
  , askContext
  , askInput
  , liftIO
    -- * Text measurement
  , lineWidth
  , textIndexAtX
  , WrapResult (..)
  , wrapTextIO
  , wrapTextLinesIO
  , caretX
  , selectionSpans
  , alignedTextPen
  , textInkEnd
  , textNodeFontVariant
  , textNodeFontTone
  , textNodeFontWeight
  , textNodeFontStyle
  , textNodeTextDecoration
  ) where

import NanoUI.Backend (followSystemThemeIO, getThemeIO, setThemeIO)
import NanoUI.Internal.Context qualified as Ctx
import NanoUI.Internal.Context hiding (followSystemTheme, getScrollMetrics, getScrollOffset, getScrollOffset2D, getScrollStep, getScrollTuning, getTheme, scrollBy, scrollGliding, scrollIntoView, scrollPages, scrollRectIntoView, scrollTo, scrollToEnd, scrollToStart, setScrollOffset, setScrollOffset2D, setScrollStep, setScrollTuning, setTheme)
import NanoUI.Internal.Frame.SpanArena (SpanArena, foldSpanArena, spanArenaCount)
import NanoUI.Internal.Draw
import NanoUI.Internal.Damage (clipPixelEdges, damagePieces, floatingPanelRects)
import NanoUI.Internal.Font (ScrollBarSlot (..), WrapResult (..), alignedTextPen, caretX, lineWidth, selectionSpans, sliderTrackBounds, textIndexAtX, textInkEnd, wrapTextIO, wrapTextLinesIO)
import NanoUI.Internal.Widgets.ColorPicker
import NanoUI.Internal.Frame (runFrame)
import NanoUI.Internal.Frame.Cursor (UiCursorKind (..), cursorKindIs, pointerCursorWanted, uiCursorKind)
import NanoUI.Internal.Frame.Input
import NanoUI.Internal.Frame.Spans (collectOverlayTextSpans, collectRasterSpans, collectTextSpans, widgetNodeCount)
import NanoUI.Internal.Frame.Scroll (ScrollBarLayout (..), scrollBarLayout)
import NanoUI.Internal.Frame.TextArea (TextInputArea (..), textInputArea)
import NanoUI.Internal.Layout.Arena (DirTag (..), NodeType (..))
import NanoUI.Internal.Layout.Solve (computePopupPosition)
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Monad (askContext, askHost, askInput, liftIO)
import NanoUI.Internal.WidgetText (textNodeFontStyle, textNodeFontTone, textNodeFontVariant, textNodeFontWeight, textNodeTextDecoration)
import NanoUI.Internal.Types (Damage (..), Rect, V2, damageIsEmpty)
import NanoUI.Internal.Var (lastFramePasses)

-- | Read the main-axis scroll offset of a scroller from outside a view.
getScrollOffsetIO :: Context -> WidgetId -> IO Float
getScrollOffsetIO = Ctx.getScrollOffset

-- | Set the main-axis scroll offset of a scroller from outside a view.
setScrollOffsetIO :: Context -> WidgetId -> Float -> IO ()
setScrollOffsetIO = Ctx.setScrollOffset

-- | Read both axes of a scroller's stored offset from outside a view.
getScrollOffset2DIO :: Context -> WidgetId -> IO V2
getScrollOffset2DIO = Ctx.getScrollOffset2D

-- | Set both axes of a scroller's stored offset from outside a view.
setScrollOffset2DIO :: Context -> WidgetId -> V2 -> IO ()
setScrollOffset2DIO = Ctx.setScrollOffset2D

-- | Read a scroller's cached metrics from outside a view.
getScrollMetricsIO :: Context -> WidgetId -> IO (Maybe ScrollMetrics)
getScrollMetricsIO = Ctx.getScrollMetrics

-- | Read the context's global scroll tuning from outside a view.
getScrollTuningIO :: Context -> IO ScrollTuning
getScrollTuningIO = Ctx.getScrollTuning

-- | Set the context's global scroll tuning from outside a view.
setScrollTuningIO :: Context -> ScrollTuning -> IO ()
setScrollTuningIO = Ctx.setScrollTuning

-- | Read a scroller's wheel step override from outside a view.
getScrollStepIO :: Context -> WidgetId -> IO Float
getScrollStepIO = Ctx.getScrollStep

-- | Set a scroller's wheel step override from outside a view.
setScrollStepIO :: Context -> WidgetId -> Float -> IO ()
setScrollStepIO = Ctx.setScrollStep

-- | Scroll to an offset from outside a view.
scrollToIO :: Context -> WidgetId -> V2 -> ScrollBehavior -> IO ()
scrollToIO = Ctx.scrollTo

-- | Scroll by a pixel delta from outside a view.
scrollByIO :: Context -> WidgetId -> V2 -> ScrollBehavior -> IO ()
scrollByIO = Ctx.scrollBy

-- | Scroll by viewport fractions from outside a view.
scrollPagesIO :: Context -> WidgetId -> V2 -> ScrollBehavior -> IO ()
scrollPagesIO = Ctx.scrollPages

-- | Scroll to the start from outside a view.
scrollToStartIO :: Context -> WidgetId -> ScrollBehavior -> IO ()
scrollToStartIO = Ctx.scrollToStart

-- | Scroll to the end from outside a view.
scrollToEndIO :: Context -> WidgetId -> ScrollBehavior -> IO ()
scrollToEndIO = Ctx.scrollToEnd

-- | Scroll a target widget into a scroller's viewport from outside a view.
scrollIntoViewIO :: Context -> WidgetId -> WidgetId -> ScrollAlign -> ScrollBehavior -> IO ()
scrollIntoViewIO = Ctx.scrollIntoView

-- | Scroll a content rectangle into a scroller's viewport from outside a view.
scrollRectIntoViewIO :: Context -> WidgetId -> Rect -> ScrollAlign -> ScrollBehavior -> IO ()
scrollRectIntoViewIO = Ctx.scrollRectIntoView

-- | Whether a scroller has a smooth-scroll glide in flight.
scrollGlidingIO :: Context -> WidgetId -> IO Bool
scrollGlidingIO = Ctx.scrollGliding
