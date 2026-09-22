-- | Draw layer facade: data types, arena, geometry and text emitters.
module NanoUI.Internal.Draw
  ( Layer (..)
  , DrawCmd (..)
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
  , currentClip
  , setClip
  , setClipPieces
  , getClipPieces
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
  , glyphAtlasPages
  , glyphPageTextureId
  , textureGlyphPage
  , pushRect
  , pushQuadGradient
  , pushImage
  , pushRoundedRect
  , pushRoundedRectRaw
  , pushRoundedStroke
  , pushCircle
  , pushCircleStroke
  , pushLine
  , pushStrokeAA
  , pushStroke
  , pushFilledTriangle
  , pushPolygonAA
  , pushPolylineAA
  , points3
  , drawTextBox
  , pushText
  , pushTextStyled
  , emitDrawOps
  ) where

import NanoUI.Internal.Draw.Arena
import NanoUI.Internal.Draw.Shapes
import NanoUI.Internal.Draw.Text
import NanoUI.Internal.Draw.Types
