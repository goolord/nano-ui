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
  , needsRedrawIdle
  , pointerDragActive
  , textFieldActive
  , floatingPanelActive
  , debugPanelOpen
  , widgetNodeCount
  , pointerCursorWanted
  , cursorKindIs
  , uiCursorKind
  , UiCursorKind (..)
  , sliderTrackBounds
  , collectTextSpans
  , collectRasterSpans
  , collectOverlayTextSpans
    -- * Context
  , Context
  , newContext
  , newPixelContext
  , ctxTheme
  , ctxFontMetrics
  , ctxHostProfile
  , ctxIcons
  , setHost
  , askHost
  , withFontMetrics
  , withMonoFontMetrics
  , withMeasureText
  , wrapMeasureCache
  , withExternalText
  , enableMeasureCache
  , withTheme
  , withIcons
  , withHostProfile
  , HostProfile (..)
  , markDirty
  , clearDirty
  , isDirty
  , setWakeLoop
  , getHotId
  , getFocusId
  , getPrevRect
  , getStore
  , getScrollOffset
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
    -- * Messages
  , FrameMsg (..)
  , decodeMessages
  , reduceMessages
  , reduceUpdates
    -- * Draw
  , DrawData (..)
  , DrawCmd (..)
  , Layer (..)
  , drawCmdNull
  , drawCmdElems
  , drawCmdFilter
  , drawCmdForLayer
  , drawCmdElemsForLayer
  , drawCmdPartitionByLayer
  , drawCmdCount
  , vertexSize
  , indexSize
  , backdropDimTextureId
  , glyphAtlasTextureId
  , Damage (..)
  , takeDamage
  , damageIsEmpty
    -- * ASCII
  , renderASCII
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
    -- * Icons (terminal metric helpers)
  , terminalCharColumns
  , terminalTextColumns
  , terminalPaintColumns
  , terminalTextPositions
  , wideTrailChar
  , textDisplayWidth
  ) where

import NanoUI.Compact (Compact, askCompact, compactHost)
import NanoUI.Context
  ( Context (..)
  , FrameMsg (..)
  , anyAnimating
  , atlasSnapshot
  , atlasTextureId
  , clearDirty
  , ctxTheme
  , decodeMessages
  , enableMeasureCache
  , getAnimationValue
  , getFocusId
  , getHotId
  , getPrevRect
  , getScrollOffset
  , getStore
  , isDirty
  , markDirty
  , modalActive
  , overlayConsumesQuit
  , reduceMessages
  , reduceUpdates
  , registerImage
  , registerImages
  , setAnimationValue
  , setHost
  , setWakeLoop
  , startAnimation
  , startAnimationEase
  , startAnimationEaseDelay
  , startSpring
  , takeDamage
  , textInputEditActive
  , withClipboard
  , withExternalText
  , withFontMetrics
  , withHostProfile
  , withIcons
  , withMeasureText
  , withMonoFontMetrics
  , withTheme
  , wrapMeasureCache
  )
import NanoUI.Context.New (newContext, newPixelHostContext)
import NanoUI.Draw
  ( DrawCmd (..)
  , DrawData (..)
  , Layer (..)
  , backdropDimTextureId
  , glyphAtlasTextureId
  , drawCmdElems
  , drawCmdFilter
  , drawCmdForLayer
  , drawCmdElemsForLayer
  , drawCmdPartitionByLayer
  , drawCmdNull
  , drawCmdCount
  , indexSize
  , vertexSize
  )
import NanoUI.Font (sliderTrackBounds, textDisplayWidth)
import NanoUI.Frame
  ( UiCursorKind (..)
  , collectOverlayTextSpans
  , collectRasterSpans
  , collectTextSpans
  , cursorKindIs
  , debugPanelOpen
  , floatingPanelActive
  , needsRedraw
  , needsRedrawIdle
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
import NanoUI.Host (HostProfile (..))
import NanoUI.Icons
  ( terminalCharColumns
  , terminalPaintColumns
  , terminalTextColumns
  , terminalTextPositions
  , wideTrailChar
  )
import NanoUI.Monad (Ui, askContext, askHost, askInput, uiIO)
import NanoUI.Render.ASCII (renderASCII)
import NanoUI.Types (Damage (..), damageIsEmpty)
import Effectful (Eff, IOE, runEff, type (:>))

-- | Pixel-host context with SDL-like defaults for headless tests.
newPixelContext :: IO Context
newPixelContext = newPixelHostContext
