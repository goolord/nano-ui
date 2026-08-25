module NanoUI
  ( -- Types
    V2 (..)
  , Rect (..)
  , Size (..)
  , Color (..)
  , colorRGBA
  , colorToWord32
  , rectContains
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
  , -- Frame
    runFrame
  , needsRedraw
  , collectTextSpans
  , collectOverlayTextSpans
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

import NanoUI.Context (Context (..), FrameMsg (..), anyAnimating, ctxTheme, getAnimationValue, getFocusId, getHotId, getScrollOffset, isDirty, markDirty, newContext, newSdlContext, newTerminalContext, setAnimationValue, startAnimation, withExternalText, withFontMetrics, withMeasureText)
import NanoUI.Draw (DrawCmd (..), DrawData (..), Layer (..), indexSize, vertexSize)
import NanoUI.Font (FontMetrics (..), isTerminalFont, labelContentInset, monospaceMetrics, widgetContentInset, widgetPadding)
import NanoUI.Frame (collectOverlayTextSpans, collectTextSpans, needsRedraw, runFrame)
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
  )
import NanoUI.Types (Color (..), Rect (..), Size (..), V2 (..), colorRGBA, colorToWord32, rectContains)
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
  )
