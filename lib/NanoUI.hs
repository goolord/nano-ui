module NanoUI
  ( -- Types
    V2 (..)
  , Rect (..)
  , Size (..)
  , Color (..)
  , colorRGBA
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
  , button
  , checkbox
  , slider
  , textInput
  , separator
  , spacer
  , tooltip
  , -- Frame
    runFrame
  , needsRedraw
  , -- Context
    Context
  , newContext
  , markDirty
  , startAnimation
  , FrameMsg (..)
  , -- Draw
    DrawData (..)
  , DrawCmd (..)
  , Layer (..)
  , -- Font
    FontMetrics (..)
  , monospaceMetrics
  , -- ASCII
    renderASCII
  , renderASCIIFromRects
  ) where

import NanoUI.Context (Context (..), FrameMsg (..), markDirty, newContext, startAnimation)
import NanoUI.Draw (DrawCmd (..), DrawData (..), Layer (..))
import NanoUI.Font (FontMetrics (..), monospaceMetrics)
import NanoUI.Frame (needsRedraw, runFrame)
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
  )
import NanoUI.Types (Color (..), Rect (..), Size (..), V2 (..), colorRGBA)
import NanoUI.Widgets
  ( Response (..)
  , button
  , checkbox
  , column
  , label
  , panel
  , row
  , separator
  , slider
  , spacer
  , textInput
  , tooltip
  )
