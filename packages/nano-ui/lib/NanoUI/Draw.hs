-- | Draw layer facade: data types, arena, geometry and text emitters.
module NanoUI.Draw
  ( Layer (..)
  , DrawCmd (..)
  , LayerSlice (..)
  , DrawData (..)
  , DrawArena (..)
  , DrawOp (..)
  , TextFont (..)
  , defaultTextFont
  , DrawingBuild
  , shiftDrawOp
  , newDrawArena
  , resetDrawArena
  , setDrawSnapScale
  , getDrawSnapScale
  , setDrawSquareGeometry
  , setDrawExternalText
  , beginLayer
  , currentLayer
  , setClip
  , withClip
  , finishDraw
  , drawCmdCount
  , drawCmdNull
  , forDrawCmdsInLayer_
  , drawCmdElems
  , vertexSize
  , indexSize
  , backdropDimTextureId
  , glyphAtlasTextureId
  , pushRect
  , pushQuadGradient
  , pushImage
  , pushRoundedRect
  , pushRoundedRectRaw
  , pushRoundedStroke
  , pushLine
  , pushStrokeAA
  , pushStroke
  , pushFilledTriangle
  , drawTextBox
  , pushText
  , pushTextStyled
  , emitDrawOps
  ) where

import NanoUI.Draw.Arena
import NanoUI.Draw.Shapes
import NanoUI.Draw.Text
import NanoUI.Draw.Types
