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
  , padAll
  , padXY
  , gap
  , fillW
  , fillH
  , grow
  , minW
  , fixedH
  , fixedWH
  , alignMid
  , wrap
  , tight
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
  , sep
  , flex
  , image_
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

import NanoUI.Context (Context (..), FrameMsg (..), anyAnimating, atlasSnapshot, atlasTextureId, ctxTheme, getAnimationValue, getFocusId, getHotId, getPrevRect, getScrollOffset, isDirty, markDirty, modalActive, newContext, newSdlContext, newTerminalContext, overlayConsumesQuit, registerImage, registerImages, setAnimationValue, startAnimation, takeDamage, textInputEditActive, withClipboard, withExternalText, withFontMetrics, withMeasureText)
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
  , inputInteracted
  , inputPointerHeld
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
  , padAll
  , padXY
  , gap
  , fillW
  , fillH
  , grow
  , minW
  , fixedH
  , fixedWH
  , alignMid
  , wrap
  , tight
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
  , sep
  , flex
  , image_
  )
