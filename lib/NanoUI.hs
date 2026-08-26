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
  , atlasTextureId
  , atlasSnapshot
  , rectContains
  , v2Add
  , -- Input
    Input (..)
  , Key (..)
  , Modifiers (..)
  , emptyInput
  , inputChanged
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
  , -- ID
    WidgetId (..)
  , widgetId
  , hashWidgetId
  , -- Monad
    UI
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
  , button
  , checkbox
  , slider
  , textInput
  , separator
  , spacer
  , tooltip
  , scrollArea
  , select
  , modal
  , image
  , -- Frame
    runFrame
  , needsRedraw
  , collectTextSpans
  , collectOverlayTextSpans
  , pointerCursorWanted
  , cursorKindIs
  , uiCursorKind
  , UiCursorKind (..)
  , sliderTrackRect
  , -- Context
    Context
  , ctxTheme
  , ctxFontMetrics
  , newContext
  , withFontMetrics
  , withMeasureText
  , withExternalText
  , newTerminalContext
  , newSdlContext
  , markDirty
  , isDirty
  , getHotId
  , getFocusId
  , withClipboard
  , textInputEditActive
  , modalActive
  , overlayConsumesQuit
  , getPrevRect
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
  , -- ASCII
    renderASCII
  , renderASCIIFromRects
  ) where

import NanoUI.Context (Context (..), FrameMsg (..), anyAnimating, atlasSnapshot, atlasTextureId, ctxTheme, getAnimationValue, getFocusId, getHotId, getPrevRect, getScrollOffset, isDirty, markDirty, modalActive, newContext, newSdlContext, newTerminalContext, overlayConsumesQuit, registerImage, setAnimationValue, startAnimation, textInputEditActive, withClipboard, withExternalText, withFontMetrics, withMeasureText)
import NanoUI.Draw (DrawCmd (..), DrawData (..), Layer (..), indexSize, vertexSize)
import NanoUI.Font (FontMetrics (..), isTerminalFont, labelContentInset, monospaceMetrics, widgetContentInset, widgetPadding)
import NanoUI.Frame (collectOverlayTextSpans, collectTextSpans, cursorKindIs, needsRedraw, pointerCursorWanted, runFrame, sliderTrackRect, uiCursorKind, UiCursorKind (..))
import NanoUI.Id (WidgetId (..), hashWidgetId, widgetId)
import NanoUI.Input
  ( Input (..)
  , Key (..)
  , Modifiers (..)
  , emptyInput
  , inputChanged
  )
import NanoUI.Monad (UI, currentId, emit, withKey)
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
  )
import NanoUI.Types (Color (..), ImageId (..), Rect (..), Size (..), V2 (..), colorRGBA, colorToWord32, rectContains, v2Add)
import NanoUI.Widgets
  ( Response (..)
  , button
  , checkbox
  , column
  , label
  , labelEx
  , panel
  , row
  , separator
  , slider
  , spacer
  , textInput
  , tooltip
  , scrollArea
  , select
  , modal
  , image
  )
