{-# LANGUAGE StrictData #-}

-- | Draw-layer data: immediate vector ops, batched draw commands, the finished
-- per-frame draw data and the arena record. Free of font and emitter code so
-- the context types can name these without depending on the emitters.
module NanoUI.Draw.Types
  ( Layer (..)
  , DrawOp (..)
  , TextFont (..)
  , defaultTextFont
  , DrawingBuild
  , shiftDrawOp
  , DrawCmd (..)
  , LayerSlice (..)
  , DrawData (..)
  , drawCmdCount
  , drawCmdNull
  , forDrawCmdsInLayer_
  , drawCmdElems
  , DrawArena (..)
  , BufferPool
  , vertexSize
  , indexSize
  , backdropDimTextureId
  , glyphAtlasTextureId
  ) where

import Data.IORef (IORef)
import Data.Primitive.PrimArray (MutablePrimArray, PrimArray, indexPrimArray, sizeofPrimArray)
import Data.Primitive.Types (Prim (..), defaultSetByteArray#, defaultSetOffAddr#)
import qualified Data.Text as T
import Data.Primitive.SmallArray (SmallArray)
import Data.Word (Word32, Word8)
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr)
import GHC.Exts
  ( Float (F#)
  , Int (I#)
  , RealWorld
  , (*#)
  , (+#)
  , indexFloatOffAddr#
  , indexIntOffAddr#
  , indexWord8Array#
  , indexWord8ArrayAsFloat#
  , indexWord8ArrayAsInt#
  , indexWord8ArrayAsWord32#
  , indexWord8OffAddr#
  , indexWord32OffAddr#
  , plusAddr#
  , readFloatOffAddr#
  , readIntOffAddr#
  , readWord8Array#
  , readWord8ArrayAsFloat#
  , readWord8ArrayAsInt#
  , readWord8ArrayAsWord32#
  , readWord8OffAddr#
  , readWord32OffAddr#
  , writeFloatOffAddr#
  , writeIntOffAddr#
  , writeWord8Array#
  , writeWord8ArrayAsFloat#
  , writeWord8ArrayAsInt#
  , writeWord8ArrayAsWord32#
  , writeWord8OffAddr#
  , writeWord32OffAddr#
  )
import GHC.Word (Word8 (W8#), Word32 (W32#))
import NanoUI.Style (FontStyle (..), FontVariant (..), FontWeight (..), TextDecoration (..))
import NanoUI.Types (Color (..), Rect (..))

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
  -- ^ Three x/y pairs followed by the fill colour.
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
  -- ^ Destination rectangle, texture id, u0/v0/u1/v1, and tint colour.
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
    FillQuadGradient (Rect x y w h) c0 c1 c2 c3 -> FillQuadGradient (Rect (x + dx) (y + dy) w h) c0 c1 c2 c3
    DrawImageRect (Rect x y w h) tex u0 v0 u1 v1 c -> DrawImageRect (Rect (x + dx) (y + dy) w h) tex u0 v0 u1 v1 c
    DrawText x y ax ay t c -> DrawText (x + dx) (y + dy) ax ay t c
    DrawTextStyled x y font t c -> DrawTextStyled (x + dx) (y + dy) font t c

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

-- | Contiguous range of draw commands for one layer, as element offset/count.
data LayerSlice = LayerSlice
  { sliceOffset :: {-# UNPACK #-} !Int
  , sliceCount :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Show)

-- Two packed Ints, 16 bytes, 8-byte aligned.
instance Prim LayerSlice where
  sizeOfType# _ = 16#
  alignmentOfType# _ = 8#
  indexByteArray# arr# i# =
    let o# = i# *# 16#
     in LayerSlice
          (I# (indexWord8ArrayAsInt# arr# o#))
          (I# (indexWord8ArrayAsInt# arr# (o# +# 8#)))
  readByteArray# arr# i# s0 =
    let o# = i# *# 16#
     in case readWord8ArrayAsInt# arr# o# s0 of
          (# s1, off# #) ->
            case readWord8ArrayAsInt# arr# (o# +# 8#) s1 of
              (# s2, cnt# #) -> (# s2, LayerSlice (I# off#) (I# cnt#) #)
  writeByteArray# arr# i# (LayerSlice (I# off#) (I# cnt#)) s0 =
    let o# = i# *# 16#
     in writeWord8ArrayAsInt# arr# (o# +# 8#) cnt# (writeWord8ArrayAsInt# arr# o# off# s0)
  setByteArray# = defaultSetByteArray#
  indexOffAddr# addr# i# =
    let a# = addr# `plusAddr#` (i# *# 16#)
     in LayerSlice (I# (indexIntOffAddr# a# 0#)) (I# (indexIntOffAddr# (a# `plusAddr#` 8#) 0#))
  readOffAddr# addr# i# s0 =
    let a# = addr# `plusAddr#` (i# *# 16#)
     in case readIntOffAddr# a# 0# s0 of
          (# s1, off# #) ->
            case readIntOffAddr# (a# `plusAddr#` 8#) 0# s1 of
              (# s2, cnt# #) -> (# s2, LayerSlice (I# off#) (I# cnt#) #)
  writeOffAddr# addr# i# (LayerSlice (I# off#) (I# cnt#)) s0 =
    let a# = addr# `plusAddr#` (i# *# 16#)
     in writeIntOffAddr# (a# `plusAddr#` 8#) 0# cnt# (writeIntOffAddr# a# 0# off# s0)
  setOffAddr# = defaultSetOffAddr#

{-# INLINE layerToWord8 #-}
layerToWord8 :: Layer -> Word8
layerToWord8 ly = fromIntegral (fromEnum ly)

{-# INLINE layerFromWord8 #-}
layerFromWord8 :: Word8 -> Layer
layerFromWord8 w = toEnum (fromIntegral w)

-- Clip floats (16) + Int tex (8) + two Word32 (8) + Layer Word8 + pad = 40.
instance Prim DrawCmd where
  sizeOfType# _ = 40#
  alignmentOfType# _ = 8#
  indexByteArray# arr# i# =
    let o# = i# *# 40#
     in DrawCmd
          (F# (indexWord8ArrayAsFloat# arr# o#))
          (F# (indexWord8ArrayAsFloat# arr# (o# +# 4#)))
          (F# (indexWord8ArrayAsFloat# arr# (o# +# 8#)))
          (F# (indexWord8ArrayAsFloat# arr# (o# +# 12#)))
          (I# (indexWord8ArrayAsInt# arr# (o# +# 16#)))
          (W32# (indexWord8ArrayAsWord32# arr# (o# +# 24#)))
          (W32# (indexWord8ArrayAsWord32# arr# (o# +# 28#)))
          (layerFromWord8 (W8# (indexWord8Array# arr# (o# +# 32#))))
  readByteArray# arr# i# s0 =
    let o# = i# *# 40#
     in case readWord8ArrayAsFloat# arr# o# s0 of
          (# s1, x# #) ->
            case readWord8ArrayAsFloat# arr# (o# +# 4#) s1 of
              (# s2, y# #) ->
                case readWord8ArrayAsFloat# arr# (o# +# 8#) s2 of
                  (# s3, w# #) ->
                    case readWord8ArrayAsFloat# arr# (o# +# 12#) s3 of
                      (# s4, h# #) ->
                        case readWord8ArrayAsInt# arr# (o# +# 16#) s4 of
                          (# s5, tex# #) ->
                            case readWord8ArrayAsWord32# arr# (o# +# 24#) s5 of
                              (# s6, off# #) ->
                                case readWord8ArrayAsWord32# arr# (o# +# 28#) s6 of
                                  (# s7, cnt# #) ->
                                    case readWord8Array# arr# (o# +# 32#) s7 of
                                      (# s8, ly# #) ->
                                        (# s8
                                         , DrawCmd
                                            (F# x#)
                                            (F# y#)
                                            (F# w#)
                                            (F# h#)
                                            (I# tex#)
                                            (W32# off#)
                                            (W32# cnt#)
                                            (layerFromWord8 (W8# ly#))
                                         #)
  writeByteArray# arr# i# cmd s0 =
    case cmd of
      DrawCmd (F# x#) (F# y#) (F# w#) (F# h#) (I# tex#) (W32# off#) (W32# cnt#) ly ->
        let o# = i# *# 40#
            !(W8# ly#) = layerToWord8 ly
         in writeWord8Array# arr# (o# +# 32#) ly# $
              writeWord8ArrayAsWord32# arr# (o# +# 28#) cnt# $
                writeWord8ArrayAsWord32# arr# (o# +# 24#) off# $
                  writeWord8ArrayAsInt# arr# (o# +# 16#) tex# $
                    writeWord8ArrayAsFloat# arr# (o# +# 12#) h# $
                      writeWord8ArrayAsFloat# arr# (o# +# 8#) w# $
                        writeWord8ArrayAsFloat# arr# (o# +# 4#) y# $
                          writeWord8ArrayAsFloat# arr# o# x# s0
  setByteArray# = defaultSetByteArray#
  indexOffAddr# addr# i# =
    let a# = addr# `plusAddr#` (i# *# 40#)
     in DrawCmd
          (F# (indexFloatOffAddr# a# 0#))
          (F# (indexFloatOffAddr# (a# `plusAddr#` 4#) 0#))
          (F# (indexFloatOffAddr# (a# `plusAddr#` 8#) 0#))
          (F# (indexFloatOffAddr# (a# `plusAddr#` 12#) 0#))
          (I# (indexIntOffAddr# (a# `plusAddr#` 16#) 0#))
          (W32# (indexWord32OffAddr# (a# `plusAddr#` 24#) 0#))
          (W32# (indexWord32OffAddr# (a# `plusAddr#` 28#) 0#))
          (layerFromWord8 (W8# (indexWord8OffAddr# (a# `plusAddr#` 32#) 0#)))
  readOffAddr# addr# i# s0 =
    let a# = addr# `plusAddr#` (i# *# 40#)
     in case readFloatOffAddr# a# 0# s0 of
          (# s1, x# #) ->
            case readFloatOffAddr# (a# `plusAddr#` 4#) 0# s1 of
              (# s2, y# #) ->
                case readFloatOffAddr# (a# `plusAddr#` 8#) 0# s2 of
                  (# s3, w# #) ->
                    case readFloatOffAddr# (a# `plusAddr#` 12#) 0# s3 of
                      (# s4, h# #) ->
                        case readIntOffAddr# (a# `plusAddr#` 16#) 0# s4 of
                          (# s5, tex# #) ->
                            case readWord32OffAddr# (a# `plusAddr#` 24#) 0# s5 of
                              (# s6, off# #) ->
                                case readWord32OffAddr# (a# `plusAddr#` 28#) 0# s6 of
                                  (# s7, cnt# #) ->
                                    case readWord8OffAddr# (a# `plusAddr#` 32#) 0# s7 of
                                      (# s8, ly# #) ->
                                        (# s8
                                         , DrawCmd
                                            (F# x#)
                                            (F# y#)
                                            (F# w#)
                                            (F# h#)
                                            (I# tex#)
                                            (W32# off#)
                                            (W32# cnt#)
                                            (layerFromWord8 (W8# ly#))
                                         #)
  writeOffAddr# addr# i# cmd s0 =
    case cmd of
      DrawCmd (F# x#) (F# y#) (F# w#) (F# h#) (I# tex#) (W32# off#) (W32# cnt#) ly ->
        let a# = addr# `plusAddr#` (i# *# 40#)
            !(W8# ly#) = layerToWord8 ly
         in writeWord8OffAddr# (a# `plusAddr#` 32#) 0# ly# $
              writeWord32OffAddr# (a# `plusAddr#` 28#) 0# cnt# $
                writeWord32OffAddr# (a# `plusAddr#` 24#) 0# off# $
                  writeIntOffAddr# (a# `plusAddr#` 16#) 0# tex# $
                    writeFloatOffAddr# (a# `plusAddr#` 12#) 0# h# $
                      writeFloatOffAddr# (a# `plusAddr#` 8#) 0# w# $
                        writeFloatOffAddr# (a# `plusAddr#` 4#) 0# y# $
                          writeFloatOffAddr# a# 0# x# s0
  setOffAddr# = defaultSetOffAddr#

-- | One frame's geometry and batches. Vertex/index pointers refer to reusable
-- arena storage: render or copy them before running another frame on the
-- context. Counts describe the used prefix, not buffer capacity.
data DrawData = DrawData
  { drawVertices :: ForeignPtr Word8
  , drawVertexCount :: {-# UNPACK #-} !Int
  , drawIndices :: ForeignPtr Word8
  , drawIndexCount :: {-# UNPACK #-} !Int
  , drawCommands :: !(PrimArray DrawCmd)
  , drawLayerSlices :: !(PrimArray LayerSlice)
  }

-- | Number of batches across all layers.
{-# INLINE drawCmdCount #-}
drawCmdCount :: DrawData -> Int
drawCmdCount dd = sizeofPrimArray (drawCommands dd)

-- | Whether there are no draw batches.
{-# INLINE drawCmdNull #-}
drawCmdNull :: DrawData -> Bool
drawCmdNull dd = drawCmdCount dd == 0

-- | Visit one layer's commands in recorded order without constructing a list.
{-# INLINE forDrawCmdsInLayer_ #-}
forDrawCmdsInLayer_ :: Layer -> DrawData -> (DrawCmd -> IO ()) -> IO ()
forDrawCmdsInLayer_ ly dd f =
  let LayerSlice off cnt = indexPrimArray (drawLayerSlices dd) (fromEnum ly)
      cmds = drawCommands dd
      go !i
        | i >= cnt = pure ()
        | otherwise = f (indexPrimArray cmds (off + i)) >> go (i + 1)
   in go 0

-- | Copy command values into a list in recorded order.
drawCmdElems :: DrawData -> [DrawCmd]
drawCmdElems dd =
  let cmds = drawCommands dd
   in [indexPrimArray cmds i | i <- [0 .. sizeofPrimArray cmds - 1]]

type BufferPool = IORef [(ForeignPtr Word8, Int)]

data DrawArena = DrawArena
  { daVertexFPtr :: !(IORef (ForeignPtr Word8))
  , daVertexPtr :: !(IORef (Ptr Word8))
  , daVertexCap :: !(IORef Int)
  , daVertexCount :: !(IORef Int)
  , daVertexPool :: !BufferPool
  , daIndexFPtr :: !(IORef (ForeignPtr Word8))
  , daIndexPtr :: !(IORef (Ptr Word8))
  , daIndexCap :: !(IORef Int)
  , daIndexCount :: !(IORef Int)
  , daIndexPool :: !BufferPool
  , daCmdStore :: !(IORef (MutablePrimArray RealWorld DrawCmd))
  , daCmdCount :: !(IORef Int)
  , daCmdCapacity :: !(IORef Int)
  , daCurrentLayer :: !(IORef Layer)
  , daCurrentClip :: !(MutablePrimArray RealWorld Float)
  -- ^ The current clip rect: x, y, width and height.
  , daCurrentTexture :: !(IORef Int)
  , daCmdStartIndex :: !(IORef Int)
  , daSnapScale :: !(IORef Float)
  , daSquareGeometry :: !(IORef Bool)
  , daExternalText :: !(IORef Bool)
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
