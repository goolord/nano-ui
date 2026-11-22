-- | The constants of @include/nano_ui_ttf_shared.h@, read by hsc2hs, so the
-- Haskell side of shaping and the glyph atlas uses the values the C side
-- was compiled with.
module NanoUI.Ttf.Internal.Header
  ( -- * Draw buffer and glyph atlas
    vertexBytes
  , glyphAtlasPixels
  , glyphAtlasPageCount
  , glyphAtlasPad
  , whitePatchSize
    -- * Shaped lines
  , shapedWidth
  , shapedHeight
  , shapedGlyphCount
  , shapedClusterCount
  , shapedGlyphs
  , shapedGlyphFonts
  , shapedClusters
  , glyphTextOffset
  , glyphIndex
  , glyphDstX
  , glyphDstY
  , glyphDstW
  , glyphDstH
  , glyphSrcX
  , glyphSrcY
  , glyphSrcW
  , glyphSrcH
  , shapedGlyphInts
  , clusterOffset
  , clusterLength
  , clusterX
  , clusterWidth
  , clusterFlags
  , shapedClusterInts
  , directionAuto
  , directionLtr
  , directionRtl
  , directionMask
  ) where

#include "nano_ui_ttf_shared.h"

-- | NANO_UI_VERTEX_BYTES: bytes in one vertex of the core's draw buffer.
-- Equal to the core's 'NanoUI.Internal.Draw.Types.vertexSize', which
-- nano-ui-sdl-web-test checks.
vertexBytes :: Int
vertexBytes = #const NANO_UI_VERTEX_BYTES

-- | NANO_UI_GLYPH_ATLAS_SIZE: width and height of a glyph atlas page.
glyphAtlasPixels :: Int
glyphAtlasPixels = #const NANO_UI_GLYPH_ATLAS_SIZE

-- | NANO_UI_GLYPH_ATLAS_PAGES. Equal to the core's
-- 'NanoUI.Internal.Draw.Types.glyphAtlasPages', which nano-ui-sdl-web-test
-- checks.
glyphAtlasPageCount :: Int
glyphAtlasPageCount = #const NANO_UI_GLYPH_ATLAS_PAGES

-- | NANO_UI_GLYPH_ATLAS_PAD: transparent pixels around each glyph.
glyphAtlasPad :: Int
glyphAtlasPad = #const NANO_UI_GLYPH_ATLAS_PAD

-- | NANO_UI_WHITE_PATCH_SIZE: the opaque square at each page's top left.
whitePatchSize :: Int
whitePatchSize = #const NANO_UI_WHITE_PATCH_SIZE

-- | The fields of @nano_ui_ttf_shaped_int@.
shapedWidth, shapedHeight, shapedGlyphCount, shapedClusterCount :: Int
shapedWidth = #const NANO_UI_SHAPED_WIDTH
shapedHeight = #const NANO_UI_SHAPED_HEIGHT
shapedGlyphCount = #const NANO_UI_SHAPED_GLYPH_COUNT
shapedClusterCount = #const NANO_UI_SHAPED_CLUSTER_COUNT

-- | The fields of @nano_ui_ttf_shaped_ptr@.
shapedGlyphs, shapedGlyphFonts, shapedClusters :: Int
shapedGlyphs = #const NANO_UI_SHAPED_GLYPHS
shapedGlyphFonts = #const NANO_UI_SHAPED_GLYPH_FONTS
shapedClusters = #const NANO_UI_SHAPED_CLUSTERS

-- | The ints of a shaped glyph, at these offsets from its first, and
-- 'shapedGlyphInts' of them a glyph.
glyphTextOffset, glyphIndex, glyphDstX, glyphDstY, glyphDstW, glyphDstH, glyphSrcX, glyphSrcY, glyphSrcW, glyphSrcH, shapedGlyphInts :: Int
glyphTextOffset = #const NANO_UI_GLYPH_TEXT_OFFSET
glyphIndex = #const NANO_UI_GLYPH_INDEX
glyphDstX = #const NANO_UI_GLYPH_DST_X
glyphDstY = #const NANO_UI_GLYPH_DST_Y
glyphDstW = #const NANO_UI_GLYPH_DST_W
glyphDstH = #const NANO_UI_GLYPH_DST_H
glyphSrcX = #const NANO_UI_GLYPH_SRC_X
glyphSrcY = #const NANO_UI_GLYPH_SRC_Y
glyphSrcW = #const NANO_UI_GLYPH_SRC_W
glyphSrcH = #const NANO_UI_GLYPH_SRC_H
shapedGlyphInts = #const NANO_UI_SHAPED_GLYPH_INTS

-- | The ints of a shaped cluster, at these offsets from its first, and
-- 'shapedClusterInts' of them a cluster.
clusterOffset, clusterLength, clusterX, clusterWidth, clusterFlags, shapedClusterInts :: Int
clusterOffset = #const NANO_UI_CLUSTER_OFFSET
clusterLength = #const NANO_UI_CLUSTER_LENGTH
clusterX = #const NANO_UI_CLUSTER_X
clusterWidth = #const NANO_UI_CLUSTER_WIDTH
clusterFlags = #const NANO_UI_CLUSTER_FLAGS
shapedClusterInts = #const NANO_UI_SHAPED_CLUSTER_INTS

-- | Shaping directions (SDL_ttf's @TTF_Direction@), and the mask that takes
-- a cluster's direction out of its flags.
directionAuto, directionLtr, directionRtl, directionMask :: Int
directionAuto = #const NANO_UI_DIRECTION_AUTO
directionLtr = #const NANO_UI_DIRECTION_LTR
directionRtl = #const NANO_UI_DIRECTION_RTL
directionMask = #const NANO_UI_DIRECTION_MASK
