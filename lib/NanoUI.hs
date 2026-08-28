module NanoUI
  ( -- Types
    V2 (..)
  , Rect (..)
  , Size (..)
  , Color (..)
  , colorRGBA
  , colorToWord32
  , lerpColor
  , contrastRatio
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
  , terminalThemeFromColors
  , terminalDefaultFg
  , terminalDefaultBg
  , scrollBarTrackColor
  , scrollBarThumbColor
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
  , askContext
  , askInput
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
  , kvBlock
  , card
  , toolbar
  , sep
  , flex
  , image_
  , -- Frame
    runFrame
  , runFrameEff
  , needsRedraw
  , needsRedrawIdle
  , pointerDragActive
  , collectRasterSpans
  , textFieldActive
  , floatingPanelActive
  , debugPanelOpen
  , collectTextSpans
  , collectOverlayTextSpans
  , widgetNodeCount
  , pointerCursorWanted
  , cursorKindIs
  , uiCursorKind
  , UiCursorKind (..)
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
  , withTheme
  , withIcons
  , ctxIcons
  , IconSet (..)
  , Icons (..)
  , asciiIcons
  , glyphIcons
  , iconsFor
  , iconSetName
  , parseIconSet
  , checkboxMark
  , fontAwesomeIcon
  , terminalCharColumns
  , terminalTextColumns
  , terminalTextPositions
  , wideTrailChar
  , textDisplayWidth
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
  , backdropDimTextureId
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
  ) where

import NanoUI.Compact (Compact, askCompact, compactHost)
import NanoUI.Context (Context (..), FrameMsg (..), anyAnimating, atlasSnapshot, atlasTextureId, ctxTheme, getAnimationValue, getFocusId, getHotId, getPrevRect, getScrollOffset, getStore, isDirty, markDirty, modalActive, newContext, newSdlContext, newTerminalContext, overlayConsumesQuit, registerImage, registerImages, setAnimationValue, setHost, setWakeLoop, startAnimation, takeDamage, textInputEditActive, withClipboard, withExternalText, withFontMetrics, withIcons, withMeasureText, withMonoFontMetrics, withTheme, wrapMeasureCache)
import NanoUI.Icons (IconSet (..), Icons (..), asciiIcons, checkboxMark, fontAwesomeIcon, glyphIcons, iconSetName, iconsFor, parseIconSet, terminalCharColumns, terminalTextColumns, terminalTextPositions, wideTrailChar)
import NanoUI.Draw (DrawCmd (..), DrawData (..), Layer (..), backdropDimTextureId, indexSize, vertexSize)
import NanoUI.Font (FontMetrics (..), hasMonoFontMarker, headingFontMarker, isTerminalFont, labelContentInset, monoFontMarker, monospaceMetrics, mutedFontMarker, resolveLayoutGap, resolveLayoutPadding, scrollBarGutter, scrollBarListExtra, scrollBarPageExtra, scrollBarWidth, scrollBarWindowGutter, sliderTrackBounds, stripMonoFontMarker, stripWidgetMarkers, textDisplayWidth, widgetContentInset, widgetPadding)
import Effectful (Eff, IOE, runEff, type (:>))
import NanoUI.Frame (collectOverlayTextSpans, collectRasterSpans, collectTextSpans, cursorKindIs, debugPanelOpen, floatingPanelActive, needsRedraw, needsRedrawIdle, pointerDragActive, pointerCursorWanted, runFrame, runFrameEff, textFieldActive, uiCursorKind, widgetNodeCount, UiCursorKind (..))
import NanoUI.Id (WidgetId (..), hashWidgetId, widgetId)
import NanoUI.Input
  ( Input (..)
  , Key (..)
  , Modifiers (..)
  , emptyInput
  , inputInteracted
  , inputPointerHeld
  )
import NanoUI.Monad (NanoUI, Ui, askContext, askInput, askHost, currentId, emit, runNanoUI, runUi, uiIO, withKey)
import NanoUI.Render.ASCII (renderASCII)
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
  , terminalThemeFromColors
  , terminalDefaultFg
  , terminalDefaultBg
  , scrollBarTrackColor
  , scrollBarThumbColor
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
  , aspect
  )
import NanoUI.Types (Color (..), Damage (..), ImageId (..), Rect (..), Size (..), V2 (..), colorRGBA, colorToWord32, contrastRatio, damageIsEmpty, lerpColor, rectContains, rectIntersect, v2Add)
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
  , kvBlock
  , card
  , toolbar
  , sep
  , flex
  , image_
  )
