{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

-- | Draw-layer data: immediate vector ops, batched draw commands, the finished
-- per-frame draw data and the arena record. Free of font and emitter code so
-- the context types can name these without depending on the emitters.
module NanoUI.Internal.Draw.Types
  ( Layer (..)
  , DrawOp (..)
  , TextFont (..)
  , defaultTextFont
  , DrawingBuild
  , shiftDrawOp
  , DrawCmd (..)
  , DrawData (..)
  , drawCmdCount
  , drawCmdNull
  , forDrawCmdsInLayer_
  , drawCmdElems
  , DrawArena (..)
  , vertexSize
  , indexSize
  , backdropDimTextureId
  , glyphAtlasTextureId
  , glyphAtlasPages
  , glyphPageTextureId
  , textureGlyphPage
  )
where

import Data.IORef (IORef)
import Data.Primitive.PrimArray (MutablePrimArray, PrimArray, imapPrimArray, indexPrimArray)
import qualified Data.Text as T
import Data.Primitive.SmallArray (SmallArray)
import Data.Word (Word32, Word8)
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Unboxed qualified as U
import Foreign.ForeignPtr (ForeignPtr)
import GHC.Exts (RealWorld)
import NanoUI.Internal.Style (FontStyle (..), FontVariant (..), FontWeight (..), TextDecoration (..))
import NanoUI.Internal.Types (Color (..), Rect (..))

-- | Painting order from background through content and overlays to chrome.
data Layer = LayerBackground | LayerContent | LayerOverlay | LayerChrome
  deriving (Eq, Show, Enum, Bounded)

-- | Vector drawing operations in logical pixels. Builders receive a solved
-- window-space rectangle and should place operations within it. Circles use
-- centre/radius; strokes use endpoints and width. Image UVs are normalised.
data DrawOp
  = FillRect !Rect !Color
  -- ^ Solid rectangle.
  | FillRoundedRect !Rect {-# UNPACK #-} !Float !Color
  -- ^ Rectangle, corner radius, and fill colour.
  | FillTriangle
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      !Color
  -- ^ Three x/y pairs followed by the fill colour. The edges are
  -- anti-aliased, so a shape tessellated into triangles shows faint seams
  -- along the shared edges; fill it with 'FillPolygon' instead.
  | FillCircle
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      !Color
  -- ^ Centre x/y, radius, and fill colour.
  | Stroke
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      !Color
  -- ^ Endpoint x0/y0/x1/y1, width, and colour.
  | StrokeRoundedRect !Rect {-# UNPACK #-} !Float {-# UNPACK #-} !Float !Color
  -- ^ Rectangle, corner radius, border width, and colour.
  | StrokeCircle
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      !Color
  -- ^ Centre x/y, radius, border width, and colour.
  | StrokeLineAA
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      !Color
  -- ^ Anti-aliased line: endpoint x0/y0/x1/y1, width, and colour.
  | FillPolygon !(PrimArray Float) !(PrimArray Int) !Color
  -- ^ Simple polygon with anti-aliased edges: its outline as x/y pairs,
  -- without repeating the first point; index triples into the outline's
  -- points that cover it; and the fill colour.
  | StrokePolyline !(PrimArray Float) {-# UNPACK #-} !Float !Bool !Color
  -- ^ Anti-aliased polyline with mitered joins: its points as x/y pairs,
  -- width, whether the last point joins back to the first (which is then not
  -- repeated), and colour.
  | FillQuadGradient !Rect !Color !Color !Color !Color
  -- ^ Rectangle with colours at top-left, top-right, bottom-right, bottom-left.
  | DrawImageRect
      !Rect
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      !Color
  -- ^ Destination rectangle, texture id, u0/v0/u1/v1, and tint colour. In a
  -- drawing the texture id may be an 'NanoUI.ImageId' registered with the
  -- context, whose own UVs run from 0 to 1.
  | DrawImageRotated
      !Rect
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      !Color
  -- ^ 'DrawImageRect' turned about the rectangle's centre: the rectangle,
  -- the angle in radians (clockwise on screen), texture id, u0/v0/u1/v1, and
  -- tint colour.
  | DrawText
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      !T.Text
      !Color
  -- ^ Pen at (x, y) is the alignment point. ax 0..1 is left..right. ay 0..1 is
  -- bottom..top. ay < 0 means baseline (x is left, y is the baseline). Glyph size
  -- is the host font (`drawTextBox`).
  | DrawTextStyled
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      !TextFont
      !T.Text
      !Color
  -- ^ Text in a font of its own, its line box's top left corner at (x, y).
  deriving (Eq)

-- | The font a 'DrawTextStyled' draws with: the same choices a label's
-- layout makes.
data TextFont = TextFont
  { textFontSize :: {-# UNPACK #-} !Float
  -- ^ Requested logical font size, @0@ for the backend default.
  , textFontVariant :: !FontVariant
  , textFontWeight :: !FontWeight
  , textFontStyle :: !FontStyle
  , textFontDecoration :: !TextDecoration
  }
  deriving (Eq, Show)

-- | Regular text using the backend's default size, weight, and upright style.
defaultTextFont :: TextFont
defaultTextFont = TextFont 0 FontRegular WeightNormal FontStyleNormal DecorationNone

-- | Translate every vertex in a 'DrawOp'. Paint reuses ops when only (x, y) moved.
shiftDrawOp :: Float -> Float -> DrawOp -> DrawOp
shiftDrawOp dx dy op =
  case op of
    FillRect (Rect x y w h) c -> FillRect (Rect (x + dx) (y + dy) w h) c
    FillRoundedRect (Rect x y w h) r c -> FillRoundedRect (Rect (x + dx) (y + dy) w h) r c
    FillTriangle x0 y0 x1 y1 x2 y2 c ->
      FillTriangle (x0 + dx) (y0 + dy) (x1 + dx) (y1 + dy) (x2 + dx) (y2 + dy) c
    FillCircle cx cy r c -> FillCircle (cx + dx) (cy + dy) r c
    Stroke x0 y0 x1 y1 t c -> Stroke (x0 + dx) (y0 + dy) (x1 + dx) (y1 + dy) t c
    StrokeRoundedRect (Rect x y w h) r bw c -> StrokeRoundedRect (Rect (x + dx) (y + dy) w h) r bw c
    StrokeCircle cx cy r bw c -> StrokeCircle (cx + dx) (cy + dy) r bw c
    StrokeLineAA x0 y0 x1 y1 bw c -> StrokeLineAA (x0 + dx) (y0 + dy) (x1 + dx) (y1 + dy) bw c
    FillPolygon pts tris c -> FillPolygon (shiftPoints dx dy pts) tris c
    StrokePolyline pts w closed c -> StrokePolyline (shiftPoints dx dy pts) w closed c
    FillQuadGradient (Rect x y w h) c0 c1 c2 c3 -> FillQuadGradient (Rect (x + dx) (y + dy) w h) c0 c1 c2 c3
    DrawImageRect (Rect x y w h) tex u0 v0 u1 v1 c -> DrawImageRect (Rect (x + dx) (y + dy) w h) tex u0 v0 u1 v1 c
    DrawImageRotated (Rect x y w h) angle tex u0 v0 u1 v1 c ->
      DrawImageRotated (Rect (x + dx) (y + dy) w h) angle tex u0 v0 u1 v1 c
    DrawText x y ax ay t c -> DrawText (x + dx) (y + dy) ax ay t c
    DrawTextStyled x y font t c -> DrawTextStyled (x + dx) (y + dy) font t c

-- | Translate x/y pairs.
shiftPoints :: Float -> Float -> PrimArray Float -> PrimArray Float
shiftPoints dx dy = imapPrimArray (\i v -> v + (if even i then dx else dy))

-- | Pure painter from solved logical window bounds to draw operations.
type DrawingBuild = Rect -> SmallArray DrawOp

-- | Indexed draw batch with logical clip, texture id, and layer. Index offset
-- and count are elements, not byte offsets; multiply by 'indexSize' for FFI use.
data DrawCmd = DrawCmd
  { cmdClipX :: {-# UNPACK #-} !Float
  , cmdClipY :: {-# UNPACK #-} !Float
  , cmdClipW :: {-# UNPACK #-} !Float
  , cmdClipH :: {-# UNPACK #-} !Float
  , cmdTextureId :: {-# UNPACK #-} !Int
  , cmdIndexOffset :: {-# UNPACK #-} !Word32
  , cmdIndexCount :: {-# UNPACK #-} !Word32
  , cmdLayer :: !Layer
  }
  deriving (Eq, Show)

-- Vector's tuple representation stores each field in a primitive array. The
-- isomorphism is inlined at command reads and writes.
type DrawCmdRep = ((Float, Float, Float, Float), Int, Word32, Word32, Word8)

instance U.IsoUnbox DrawCmd DrawCmdRep where
  {-# INLINE toURepr #-}
  toURepr (DrawCmd x y w h tex off count layer) = ((x, y, w, h), tex, off, count, fromIntegral (fromEnum layer))
  {-# INLINE fromURepr #-}
  fromURepr ((x, y, w, h), tex, off, count, layer) = DrawCmd x y w h tex off count (toEnum (fromIntegral layer))

newtype instance U.MVector s DrawCmd = MVDrawCmd (U.MVector s (U.As DrawCmd DrawCmdRep))

newtype instance U.Vector DrawCmd = VDrawCmd (U.Vector (U.As DrawCmd DrawCmdRep))

deriving via (U.As DrawCmd DrawCmdRep) instance GM.MVector U.MVector DrawCmd

deriving via (U.As DrawCmd DrawCmdRep) instance G.Vector U.Vector DrawCmd

instance U.Unbox DrawCmd

-- | One frame's geometry and batches. Vertex/index pointers refer to reusable
-- arena storage: render or copy them before running another frame on the
-- context. Counts describe the used prefix, not buffer capacity.
data DrawData = DrawData
  { drawVertices :: ForeignPtr Word8
  , drawVertexCount :: {-# UNPACK #-} !Int
  , drawIndices :: ForeignPtr Word8
  , drawIndexCount :: {-# UNPACK #-} !Int
  , drawCommands :: !(U.Vector DrawCmd)
  , drawLayerOffsets :: !(PrimArray Int)
  -- ^ Cumulative command offsets: layer @i@ occupies @[offsets[i], offsets[i+1])@.
  -- Includes a final sentinel equal to 'drawCmdCount'; empty layers repeat offsets.
  }

-- | Number of batches across all layers.
{-# INLINE drawCmdCount #-}
drawCmdCount :: DrawData -> Int
drawCmdCount dd = U.length (drawCommands dd)

-- | Whether there are no draw batches.
{-# INLINE drawCmdNull #-}
drawCmdNull :: DrawData -> Bool
drawCmdNull dd = drawCmdCount dd == 0

-- | Visit one layer's commands in recorded order without constructing a list.
{-# INLINE forDrawCmdsInLayer_ #-}
forDrawCmdsInLayer_ :: Layer -> DrawData -> (DrawCmd -> IO ()) -> IO ()
forDrawCmdsInLayer_ ly dd f =
  let
    offsets = drawLayerOffsets dd
    off = indexPrimArray offsets (fromEnum ly)
    end = indexPrimArray offsets (fromEnum ly + 1)
    cmds = drawCommands dd
    go !i
      | i >= end = pure ()
      | otherwise = f (U.unsafeIndex cmds i) >> go (i + 1)
   in
    go off

-- | Copy command values into a list in recorded order.
drawCmdElems :: DrawData -> [DrawCmd]
drawCmdElems = U.toList . drawCommands

data DrawArena = DrawArena
  { daVertexFPtr :: !(IORef (ForeignPtr Word8))
  , daVertexCap :: !(IORef Int)
  , daIndexFPtr :: !(IORef (ForeignPtr Word8))
  , daIndexCap :: !(IORef Int)
  , daCounts :: !(MutablePrimArray RealWorld Int)
  -- ^ Vertex count, index count, command count and the pending command's
  -- start index, unboxed so the per-primitive writes do not allocate.
  , daCmdStore :: !(IORef (U.MVector RealWorld DrawCmd))
  , daCurrentLayer :: !(IORef Layer)
  , daCurrentClip :: !(MutablePrimArray RealWorld Float)
  -- ^ The current clip rect: x, y, width and height.
  , daCurrentTexture :: !(IORef Int)
  , daSnapScale :: !(IORef Float)
  , daSquareGeometry :: !(IORef Bool)
  , daExternalText :: !(IORef Bool)
  , daClipPieces :: !(IORef (PrimArray Float))
  -- ^ Disjoint rects as @x0, y0, x1, y1@ runs that every command is cut to,
  -- one copy per rect it meets; empty for none.
  }

-- | Packed vertex stride in bytes: 32.
vertexSize :: Int
vertexSize = 32

-- | Index stride in bytes: 4, for a 32-bit unsigned index.
indexSize :: Int
indexSize = 4

-- | Reserved texture id. These quads act as a backdrop dim, not a solid fill.
-- Mix comes from the vertex color alpha.
backdropDimTextureId :: Int
backdropDimTextureId = 0x7ffffffe

-- | Reserved texture id for the per-glyph SDL_ttf atlas. The renderer binds
-- the glyph atlas SDL_Texture when it sees this id. Glyphs are cached as
-- white-on-alpha so vertex color tints them at draw time.
glyphAtlasTextureId :: Int
glyphAtlasTextureId = 0x7ffffffd

-- | Pages a glyph atlas can have. A glyph's page is the whole part of its
-- u coordinates ('NanoUI.Internal.Font.ShapedGlyphs'), so page 0's UVs are
-- the plain ones.
glyphAtlasPages :: Int
glyphAtlasPages = 4

-- | The reserved texture id of glyph atlas page @page@: 'glyphAtlasTextureId'
-- for page 0, and the ids below it for the others.
{-# INLINE glyphPageTextureId #-}
glyphPageTextureId :: Int -> Int
glyphPageTextureId page = glyphAtlasTextureId - page

-- | The glyph atlas page a texture id names, or -1 for any other texture.
{-# INLINE textureGlyphPage #-}
textureGlyphPage :: Int -> Int
textureGlyphPage tex =
  let page = glyphAtlasTextureId - tex
   in if page >= 0 && page < glyphAtlasPages then page else -1
