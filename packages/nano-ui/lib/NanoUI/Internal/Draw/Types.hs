{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

-- | Draw-layer data: immediate vector ops, batched draw commands, the finished
-- per-frame draw data and the arena record. Free of font and emitter code so
-- the context types can name these without depending on the emitters.
module NanoUI.Internal.Draw.Types
  ( Layer (..)
  , DrawOp (.., DrawImageRect)
  , LineCap (..)
  , LineJoin (..)
  , Shade (..)
  , TextFont (..)
  , defaultTextFont
  , DrawingBuild
  , shiftDrawOp
  , quadPolygon
  , DrawCmd (..)
  , ShaderCall (..)
  , EffectSpan (..)
  , PendingEffectSpan (..)
  , emptyShaderCalls
  , emptyEffectSpans
  , emptyUniformBytes
  , DrawData (..)
  , drawCmdCount
  , drawCmdNull
  , forDrawCmdsInLayer_
  , drawCmdElems
  , DrawArena (..)
  , RecordedOp (..)
  , vertexSize
  , indexSize
  , backdropDimTextureId
  , glyphAtlasTextureId
  , glyphAtlasPages
  , glyphPageTextureId
  , textureGlyphPage
  )
where

import Data.Int (Int32)
import Data.IORef (IORef)
import Data.Primitive.ByteArray (ByteArray, MutableByteArray, emptyByteArray)
import Data.Primitive.PrimArray (MutablePrimArray, PrimArray, imapPrimArray, indexPrimArray, primArrayFromListN)
import qualified Data.Text as T
import Data.Primitive.SmallArray (SmallArray, smallArrayFromList)
import Data.Word (Word32, Word64, Word8)
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Unboxed qualified as U
import Foreign.ForeignPtr (ForeignPtr)
import GHC.Exts (RealWorld)
import NanoUI.Internal.Shader (ShaderProgram)
import NanoUI.Internal.Style (FontStyle (..), FontVariant (..), FontWeight (..), TextDecoration (..))
import NanoUI.Internal.Types (Color (..), Rect (..), V2 (..))

-- | Painting order from background through content and overlays to chrome.
data Layer = LayerBackground | LayerContent | LayerOverlay | LayerChrome
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Vector drawing operations in logical pixels. Builders receive a solved
-- window-space rectangle and should place operations within it. Circles use
-- centre/radius; strokes use endpoints and width. Image UVs are normalised.
--
-- Build ops with the canvas ("NanoUI.Widgets.Custom"), which fills and
-- strokes paths, clips, and draws through transforms. Constructors after
-- 'DrawTextStyled' are the canvas's building blocks and may change.
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
  -- along the shared edges; fill a path on a canvas instead.
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
  -- ^ Endpoint x0/y0/x1/y1, width, and colour. Not anti-aliased; see
  -- 'StrokeLineAA'.
  | StrokeRoundedRect !Rect {-# UNPACK #-} !Float {-# UNPACK #-} !Float !Color
  -- ^ Rectangle, corner radius, border width, and colour. The border lies
  -- inside the rectangle.
  | StrokeCircle
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      !Color
  -- ^ Centre x/y, radius, border width, and colour. The border lies inside
  -- the circle.
  | StrokeLineAA
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      !Color
  -- ^ Anti-aliased line: endpoint x0/y0/x1/y1, width, and colour.
  | FillQuadGradient !Rect !Color !Color !Color !Color
  -- ^ Rectangle with colours at top-left, top-right, bottom-right, bottom-left.
  | DrawImage
      !Rect
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      !Color
  -- ^ Destination rectangle, rotation about its centre in radians
  -- (clockwise on screen), texture id, u0/v0/u1/v1, and tint colour. In a
  -- drawing the texture id may be a registered 'NanoUI.ImageId', with UVs
  -- from 0 to 1. An unrotated image snaps to the pixel grid; a rotated one
  -- does not. Canvas code uses 'NanoUI.Widgets.Custom.drawImageWith'.
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
  | FillPolygon !(PrimArray Float) !(PrimArray Int) !(PrimArray Int) !Shade
  -- ^ A polygon with holes, anti-aliased along its rings (outline and
  -- holes), not along the edges between its triangles. Fields:
  -- points as x/y pairs, ring after ring, then any interior points the
  -- triangles need; each ring's start offset plus the last ring's end;
  -- index triples into the points; and the shade. The first ring is the
  -- outline; the rest are holes, wound the opposite way.
  | StrokePolyline
      !(PrimArray Float)
      {-# UNPACK #-} !Float
      !Bool
      !LineCap
      !LineJoin
      {-# UNPACK #-} !Float
      !Shade
  -- ^ A polyline anti-aliased along its sides. Fields: points as x/y
  -- pairs; width; whether it closes back to the first point (not
  -- repeated); cap for open ends; corner join; miter limit as a multiple of
  -- the width, beyond which a miter becomes a bevel; and the shade.
  | DrawTextAligned
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      !TextFont
      !T.Text
      !Color
  -- ^ Text in its own font, anchored at (x, y) by ax/ay as in 'DrawText',
  -- with the font size scaled. Fields: x, y, ax, ay, scale, font, text,
  -- colour.
  | PushClip !Rect
  -- ^ Clip ops up to the matching 'PopClip' to this rectangle, intersected
  -- with the current clip.
  | PopClip
  -- ^ End the innermost open 'PushClip'.
  deriving (Eq, Show)

-- | An unrotated 'DrawImage': destination rectangle, texture id,
-- u0/v0/u1/v1, and tint colour. As a pattern it matches only angle 0.
pattern DrawImageRect :: Rect -> Int -> Float -> Float -> Float -> Float -> Color -> DrawOp
pattern DrawImageRect r tex u0 v0 u1 v1 c = DrawImage r 0 tex u0 v0 u1 v1 c

-- | How a stroke ends an open subpath.
data LineCap
  = ButtCap
  -- ^ Flat at the end point.
  | SquareCap
  -- ^ Flat, half the width past the end point.
  | RoundCap
  -- ^ A half disc past the end point.
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | How a stroke turns a corner.
data LineJoin
  = MiterJoin
  -- ^ Sides extend until they meet in a point, or bevel if that point is
  -- beyond the miter limit.
  | RoundJoin
  -- ^ A circular arc around the corner.
  | BevelJoin
  -- ^ Corner cut straight across.
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Colour of a 'FillPolygon' or 'StrokePolyline': one for the whole shape,
-- or one per point, blended across the triangles. A stroke uses each
-- point's colour across the line's width there.
data Shade
  = Flat !Color
  | Shaded !(PrimArray Word32)
  deriving (Eq, Show)

-- | The font a 'DrawTextStyled' draws with: the same choices a label's
-- layout makes.
data TextFont = TextFont
  { textFontSize :: {-# UNPACK #-} !Float
  -- ^ Requested logical font size, @0@ for the backend default.
  , textFontVariant :: !FontVariant
  , textFontWeight :: !FontWeight
  , textFontStyle :: !FontStyle
  , textFontDecoration :: !TextDecoration
  , textFontTabular :: !Bool
  -- ^ Uniform-width ASCII digits ('NanoUI.Internal.Style.fontTabular').
  }
  deriving (Eq, Show)

-- | Regular text using the backend's default size, weight, and upright style.
defaultTextFont :: TextFont
defaultTextFont = TextFont 0 FontRegular WeightNormal FontStyleNormal DecorationNone False

-- | A quad as a 'FillPolygon': its four corners' x/y pairs in order round
-- it, cut into two triangles from the first.
quadPolygon :: PrimArray Float -> Shade -> DrawOp
quadPolygon corners = FillPolygon corners (primArrayFromListN 2 [0, 4]) (primArrayFromListN 6 [0, 1, 2, 0, 2, 3])

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
    FillPolygon pts rings tris c -> FillPolygon (shiftPoints dx dy pts) rings tris c
    StrokePolyline pts w closed cap join limit c -> StrokePolyline (shiftPoints dx dy pts) w closed cap join limit c
    FillQuadGradient (Rect x y w h) c0 c1 c2 c3 -> FillQuadGradient (Rect (x + dx) (y + dy) w h) c0 c1 c2 c3
    DrawImage (Rect x y w h) angle tex u0 v0 u1 v1 c -> DrawImage (Rect (x + dx) (y + dy) w h) angle tex u0 v0 u1 v1 c
    DrawText x y ax ay t c -> DrawText (x + dx) (y + dy) ax ay t c
    DrawTextStyled x y font t c -> DrawTextStyled (x + dx) (y + dy) font t c
    DrawTextAligned x y ax ay k font t c -> DrawTextAligned (x + dx) (y + dy) ax ay k font t c
    PushClip (Rect x y w h) -> PushClip (Rect (x + dx) (y + dy) w h)
    PopClip -> PopClip

-- | Translate x/y pairs.
shiftPoints :: Float -> Float -> PrimArray Float -> PrimArray Float
shiftPoints dx dy = imapPrimArray (\i v -> v + (if even i then dx else dy))

-- | Pure painter from solved logical window bounds to draw operations.
type DrawingBuild = Rect -> SmallArray DrawOp

-- | Indexed draw batch with logical clip, texture id, layer, and optional
-- shader call index. Index offset and count are elements, not byte offsets;
-- multiply by 'indexSize' for FFI use.
data DrawCmd = DrawCmd
  { cmdClipX :: {-# UNPACK #-} !Float
  , cmdClipY :: {-# UNPACK #-} !Float
  , cmdClipW :: {-# UNPACK #-} !Float
  , cmdClipH :: {-# UNPACK #-} !Float
  , cmdTextureId :: {-# UNPACK #-} !Int
  , cmdIndexOffset :: {-# UNPACK #-} !Word32
  , cmdIndexCount :: {-# UNPACK #-} !Word32
  , cmdLayer :: !Layer
  , cmdShader :: {-# UNPACK #-} !Int32
  -- ^ @0@ for the standard UI pipeline; 1-based index into 'drawShaderCalls'.
  }
  deriving (Eq, Show)

-- Vector's tuple representation stores each field in a primitive array. The
-- isomorphism is inlined at command reads and writes.
type DrawCmdRep = ((Float, Float, Float, Float), Int, Word32, Word32, Word8, Int32)

instance U.IsoUnbox DrawCmd DrawCmdRep where
  {-# INLINE toURepr #-}
  toURepr (DrawCmd x y w h tex off count layer sh) =
    ((x, y, w, h), tex, off, count, fromIntegral (fromEnum layer), sh)
  {-# INLINE fromURepr #-}
  fromURepr ((x, y, w, h), tex, off, count, layer, sh) =
    DrawCmd x y w h tex off count (toEnum (fromIntegral layer)) sh

newtype instance U.MVector s DrawCmd = MVDrawCmd (U.MVector s (U.As DrawCmd DrawCmdRep))

newtype instance U.Vector DrawCmd = VDrawCmd (U.Vector (U.As DrawCmd DrawCmdRep))

deriving via (U.As DrawCmd DrawCmdRep) instance GM.MVector U.MVector DrawCmd

deriving via (U.As DrawCmd DrawCmdRep) instance G.Vector U.Vector DrawCmd

instance U.Unbox DrawCmd

-- | One fragment shader invocation referenced by 'cmdShader' (1-based) or an
-- 'EffectSpan'.
data ShaderCall = ShaderCall
  { scHash :: {-# UNPACK #-} !Word64
  -- ^ Content hash of 'scProgram', used by backends to cache compiled programs.
  , scProgram :: !ShaderProgram
  -- ^ Lazy target shader payloads (GLSL 150, GLSL ES 300, SPIR-V, MSL).
  , scUniformOffset :: {-# UNPACK #-} !Int
  -- ^ Byte offset into 'drawUniformBytes' (16-byte aligned).
  , scUniformLength :: {-# UNPACK #-} !Int
  -- ^ Byte length in 'drawUniformBytes': @48 + userUniformBytes@.
  }

-- | Brackets a contiguous slice of 'drawCommands' rendered into a pooled
-- offscreen target and composited back through one or more shader passes.
data EffectSpan = EffectSpan
  { esId :: {-# UNPACK #-} !Int
  -- ^ 1-based identifier of this effect span within the frame.
  , esParent :: {-# UNPACK #-} !Int
  -- ^ Enclosing 'esId', or @0@ for the root target.
  , esLayer :: !Layer
  -- ^ Layer whose commands this span brackets.
  , esCmdStart :: {-# UNPACK #-} !Int
  -- ^ Inclusive start index into 'drawCommands'.
  , esCmdEnd :: {-# UNPACK #-} !Int
  -- ^ Exclusive end index into 'drawCommands'.
  , esBounds :: !Rect
  -- ^ Logical node rectangle before margin expansion.
  , esClip :: !Rect
  -- ^ Enclosing logical clip rectangle active when the span opened.
  , esMargin :: !V2
  -- ^ Maximum sample offset in logical pixels (X and Y).
  , esPasses :: !(SmallArray ShaderCall)
  -- ^ Shader passes applied in order to the offscreen texture.
  , esSvgOpacity :: !(Maybe Float)
  -- ^ Optional native opacity hint for SVG export.
  }

-- | Unfinalized 'EffectSpan' recorded during painting before 'finishDraw'
-- maps raw command indices through piece cutting and layer grouping.
data PendingEffectSpan = PendingEffectSpan
  { pesId :: {-# UNPACK #-} !Int
  , pesParent :: {-# UNPACK #-} !Int
  , pesLayer :: !Layer
  , pesRawStart :: {-# UNPACK #-} !Int
  , pesRawEnd :: {-# UNPACK #-} !Int
  , pesBounds :: !Rect
  , pesClip :: !Rect
  , pesMargin :: !V2
  , pesPasses :: !(SmallArray ShaderCall)
  , pesSvgOpacity :: !(Maybe Float)
  }

-- | Shared empty 'SmallArray' for frames without shader calls.
emptyShaderCalls :: SmallArray ShaderCall
emptyShaderCalls = smallArrayFromList []

-- | Shared empty 'SmallArray' for frames without offscreen effect spans.
emptyEffectSpans :: SmallArray EffectSpan
emptyEffectSpans = smallArrayFromList []

-- | Shared empty 'ByteArray' for frames without shader uniforms.
emptyUniformBytes :: ByteArray
emptyUniformBytes = emptyByteArray

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
  , drawClear :: !Color
  -- ^ The colour a full frame clears the target to before 'drawCommands':
  -- the page surface ('NanoUI.pageBackground') laid over the window colour
  -- ('NanoUI.themeWindow'). A clip frame's commands paint it as their own
  -- backdrop.
  , drawShaderCalls :: !(SmallArray ShaderCall)
  -- ^ Shader invocations referenced by 1-based 'cmdShader' indices; empty
  -- when the frame draws no shaders.
  , drawUniformBytes :: !ByteArray
  -- ^ Flat std140 uniform byte arena sliced by 'ShaderCall'; empty when the
  -- frame draws no shaders.
  , drawEffectSpans :: !(SmallArray EffectSpan)
  -- ^ Subtree effect spans in open order; empty when the frame uses no
  -- offscreen effects.
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
  , daTextSpans :: !(IORef ([(Rect, T.Text, Color, Color, Rect)], [(Rect, T.Text, Color, Color, Rect)]))
  -- ^ Under external text, the drawing text this paint recorded as host
  -- spans (bounds, text, foreground, transparent background, clip), newest
  -- first: base layers, then overlay layers. Read by
  -- 'NanoUI.Internal.Frame.Spans.collectRasterSpans' and emptied only by
  -- 'NanoUI.Internal.Draw.Arena.resetDrawArena', so a reused frame keeps
  -- the spans of the paint that drew it.
  , daClear :: !(IORef Color)
  -- ^ This paint's clear colour; see 'drawClear'.
  , daRecordOps :: !(IORef Bool)
  -- ^ Whether the shape emitters record what they draw ('daRecordedOps').
  -- Persists across 'NanoUI.Internal.Draw.Arena.resetDrawArena'.
  , daRecordedOps :: !(IORef [RecordedOp])
  -- ^ While 'daRecordOps' is on, the shapes and images this paint drew, newest
  -- first. Emptied only by 'NanoUI.Internal.Draw.Arena.resetDrawArena', so
  -- a reused frame keeps the shapes of the paint that drew it.
  , daRecordOpacity :: !(IORef Float)
  -- ^ Active SVG group opacity multiplier when 'daRecordOps' is on (@1.0@
  -- outside opacity effects).
  , daCurrentShader :: !(IORef Int32)
  -- ^ Active 1-based shader call index, or @0@ for the standard UI pipeline.
  , daNoMergeBefore :: !(IORef Int)
  -- ^ Minimum command count before which 'flushCmd' may not coalesce into the
  -- preceding command (updated at effect span boundaries).
  , daShaderCalls :: !(IORef [ShaderCall])
  -- ^ Shader calls emitted this paint, newest first.
  , daShaderCallCount :: !(IORef Int)
  -- ^ Number of entries in 'daShaderCalls'.
  , daUniformBuf :: !(IORef (MutableByteArray RealWorld))
  -- ^ Reusable flat std140 uniform staging buffer, grown lazily on shader use.
  , daUniformLen :: !(IORef Int)
  -- ^ Number of bytes written into 'daUniformBuf' this paint.
  , daEffectSpans :: !(IORef [PendingEffectSpan])
  -- ^ Closed effect spans recorded this paint, newest first.
  , daEffectStack :: !(IORef [Int])
  -- ^ Currently open effect span IDs, innermost first.
  , daEffectNextId :: !(IORef Int)
  -- ^ Next 1-based effect span ID.
  , daPixelScale :: !(IORef Float)
  -- ^ Logical-to-physical scale forwarded in shader system uniforms (@uMeta.x@).
  }

-- | A solid shape as an emitter drew it: its layer, the clip it drew under,
-- its active group opacity (@1.0@ outside opacity effects), and the shape as
-- the op that draws it. Exporters read these in place of the tessellated
-- vertices ('NanoUI.Internal.Draw.Arena.setDrawRecordOps').
data RecordedOp = RecordedOp !Layer !Rect !Float !DrawOp

-- | Packed vertex stride in bytes: 32. A vertex is eight 32-bit floats: x
-- and y in logical pixels, red, green, blue and alpha from 0 to 1 with
-- straight (not premultiplied) alpha, then the texture's u and v.
vertexSize :: Int
vertexSize = 32

-- | Index stride in bytes: 4, for a 32-bit unsigned index.
indexSize :: Int
indexSize = 4

-- | Reserved texture id. nano-ui emits no command with it: a modal's
-- backdrop dim is a plain translucent fill.
backdropDimTextureId :: Int
backdropDimTextureId = 0x7ffffffe

-- | Reserved texture id for page 0 of the host's glyph atlas
-- ('glyphPageTextureId'). The renderer binds its atlas texture when it sees
-- this id. Glyphs are cached as white-on-alpha so vertex color tints them at
-- draw time. Solid shapes (rects, rounded shapes, strokes, polygons) draw
-- with this id too and sample u = v = 1.5/1024, so a host that samples the
-- atlas for them keeps that point opaque white.
glyphAtlasTextureId :: Int
glyphAtlasTextureId = 0x7ffffffd

-- | Pages a glyph atlas can have. A glyph's page is the whole part of its
-- u coordinates ('NanoUI.Backend.ShapedGlyphs'), so page 0's UVs are
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
