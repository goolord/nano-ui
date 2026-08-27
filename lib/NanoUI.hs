module NanoUI
  ( -- Types
    V2 (..)
  , Rect (..)
  , Size (..)
  , Color (..)
  , colorRGBA
  , colorToWord32
  , ImageId (..)
  , registerImage
  , registerImages
  , atlasTextureId
  , atlasSnapshot
  , rectContains
  , rectIntersect
  , v2Add
  , Damage (..)
  , damageIsEmpty
  , takeDamage
  , -- Input
    Input (..)
  , Key (..)
  , Modifiers (..)
  , emptyInput
  , inputChanged
  , inputInteracted
  , inputPointerHeld
  , -- Style
    Sizing (..)
  , Direction (..)
  , AlignX (..)
  , AlignY (..)
  , Padding (..)
  , Layout (..)
  , defaultLayout
  , Style (..)
  , Theme (..)
  , defaultTheme
  , terminalTheme
  , sdlTheme
  , panelPaintPad
  , windowPad
  , windowMargin
  , padAll
  , padXY
  , gap
  , fillW
  , fillH
  , grow
  , minW
  , fixedW
  , fixedH
  , fixedWH
  , alignMid
  , alignEnd
  , wrap
  , tight
  , percent
  , percentH
  , aspect
  , -- ID
    WidgetId (..)
  , widgetId
  , hashWidgetId
  , -- Monad
    NanoUI
  , Ui
  , Eff
  , type (:>)
  , IOE
  , runEff
  , runUi
  , runNanoUI
  , uiIO
  , emit
  , withKey
  , currentId
  , -- Widgets
    Response (..)
  , panel
  , row
  , column
  , label
  , labelEx
  , label_
  , button
  , checkbox
  , slider
  , sliderEx
  , textInput
  , separator
  , spacer
  , tooltip
  , scroll
  , scrollArea
  , select
  , modal
  , window
  , image
  , onClick
  , clickButton
  , useFlag
  , useText
  , useToggle
  , heading
  , muted
  , kv
  , card
  , toolbar
  , sep
  , flex
  , image_
  , -- Frame
    runFrame
  , runFrameEff
  , needsRedraw
  , textFieldActive
  , floatingPanelActive
  , debugPanelOpen
  , collectTextSpans
  , collectOverlayTextSpans
  , pointerCursorWanted
  , cursorKindIs
  , uiCursorKind
  , UiCursorKind (..)
  , sliderTrackRect
  , sliderTrackBounds
  , -- Context
    Context
  , ctxTheme
  , ctxFontMetrics
  , setHost
  , askHost
  , Compact
  , compactHost
  , askCompact
  , newContext
  , withFontMetrics
  , withMonoFontMetrics
  , withMeasureText
  , wrapMeasureCache
  , withExternalText
  , newTerminalContext
  , newSdlContext
  , markDirty
  , isDirty
  , setWakeLoop
  , getHotId
  , getFocusId
  , withClipboard
  , textInputEditActive
  , modalActive
  , overlayConsumesQuit
  , getPrevRect
  , getStore
  , getScrollOffset
  , startAnimation
  , setAnimationValue
  , getAnimationValue
  , anyAnimating
  , FrameMsg (..)
  , -- Draw
    DrawData (..)
  , DrawCmd (..)
  , Layer (..)
  , vertexSize
  , indexSize
  , -- Font
    FontMetrics (..)
  , monospaceMetrics
  , labelContentInset
  , widgetContentInset
  , widgetPadding
  , isTerminalFont
  , resolveLayoutGap
  , resolveLayoutPadding
  , monoFontMarker
  , hasMonoFontMarker
  , stripMonoFontMarker
  , headingFontMarker
  , mutedFontMarker
  , stripWidgetMarkers
  , scrollBarGutter
  , scrollBarPageExtra
  , scrollBarListExtra
  , scrollBarWidth
  , scrollBarWindowGutter
  , -- ASCII
    renderASCII
  , renderASCIIFromRects
  ) where

import NanoUI.Compact (Compact, askCompact, compactHost)
import NanoUI.Context (Context (..), FrameMsg (..), anyAnimating, atlasSnapshot, atlasTextureId, ctxTheme, getAnimationValue, getFocusId, getHotId, getPrevRect, getScrollOffset, getStore, isDirty, markDirty, modalActive, newContext, newSdlContext, newTerminalContext, overlayConsumesQuit, registerImage, registerImages, setAnimationValue, setHost, setWakeLoop, startAnimation, takeDamage, textInputEditActive, withClipboard, withExternalText, withFontMetrics, withMeasureText, withMonoFontMetrics, wrapMeasureCache)
import NanoUI.Draw (DrawCmd (..), DrawData (..), Layer (..), indexSize, vertexSize)
import NanoUI.Font (FontMetrics (..), hasMonoFontMarker, headingFontMarker, isTerminalFont, labelContentInset, monoFontMarker, monospaceMetrics, mutedFontMarker, resolveLayoutGap, resolveLayoutPadding, scrollBarGutter, scrollBarListExtra, scrollBarPageExtra, scrollBarWidth, scrollBarWindowGutter, sliderTrackBounds, stripMonoFontMarker, stripWidgetMarkers, widgetContentInset, widgetPadding)
import Effectful (Eff, IOE, runEff, type (:>))
import NanoUI.Frame (collectOverlayTextSpans, collectTextSpans, cursorKindIs, debugPanelOpen, floatingPanelActive, needsRedraw, pointerCursorWanted, runFrame, runFrameEff, sliderTrackRect, textFieldActive, uiCursorKind, UiCursorKind (..))
import NanoUI.Id (WidgetId (..), hashWidgetId, widgetId)
import NanoUI.Input
  ( Input (..)
  , Key (..)
  , Modifiers (..)
  , emptyInput
  , inputChanged
  , inputInteracted
  , inputPointerHeld
  )
import NanoUI.Monad (NanoUI, Ui, askHost, currentId, emit, runNanoUI, runUi, uiIO, withKey)
import NanoUI.Render.ASCII (renderASCII, renderASCIIFromRects)
import NanoUI.Style
  ( AlignX (..)
  , AlignY (..)
  , Direction (..)
  , Layout (..)
  , Padding (..)
  , Sizing (..)
  , Style (..)
  , Theme (..)
  , defaultLayout
  , defaultTheme
  , terminalTheme
  , sdlTheme
  , panelPaintPad
  , windowPad
  , windowMargin
  , padAll
  , padXY
  , gap
  , fillW
  , fillH
  , grow
  , minW
  , fixedW
  , fixedH
  , fixedWH
  , alignMid
  , alignEnd
  , wrap
  , tight
  , percent
  , percentH
  , aspect
  )
import NanoUI.Types (Color (..), Damage (..), ImageId (..), Rect (..), Size (..), V2 (..), colorRGBA, colorToWord32, damageIsEmpty, rectContains, rectIntersect, v2Add)
import NanoUI.Widgets
  ( Response (..)
  , button
  , checkbox
  , clickButton
  , column
  , label
  , labelEx
  , label_
  , onClick
  , panel
  , row
  , separator
  , slider
  , sliderEx
  , spacer
  , textInput
  , tooltip
  , scroll
  , scrollArea
  , select
  , modal
  , window
  , image
  , useFlag
  , useText
  , useToggle
  , heading
  , muted
  , kv
  , card
  , toolbar
  , sep
  , flex
  , image_
  )
