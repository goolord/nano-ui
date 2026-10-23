-- | Render a whole painted frame of any view to an SVG document.
--
-- Widget chrome never becomes 'DrawOp' values: paint lowers every widget
-- straight to the vertex buffer, and 'DrawOp's (canvases, drawings,
-- diagrams) are interpreted into that same buffer. This module re-renders
-- a finished frame from the three forms a host already sees: solid
-- geometry from 'DrawData' as clip-cut rectangles and triangles, vector
-- drawings from the frame's draw-op caches through "NanoUI.Draw.Svg", and
-- text from 'collectRasterSpans'.
--
-- Enable 'setDrawSquareGeometry' and 'setDrawExternalText' on the context
-- before running frames: the first makes every widget shape a flat
-- axis-aligned quad, the second moves all text -- including text inside
-- drawings -- onto the raster spans consumed here. Without them the output
-- degrades rather than fails: anti-aliasing fringes take their first
-- vertex's colour, and drawing text is missing. Atlas image quads are
-- omitted (the atlas has no URI); 'DrawOp' images inside drawings resolve
-- through 'fsImageUri'.
--
-- > (_, dd, _) <- runFrame ctx inp view
-- > svg <- frameToSvg defaultFrameSvgConfig ctx inp dd
module NanoUI.Draw.FrameSvg
  ( FrameSvgConfig (..)
  , defaultFrameSvgConfig
  , frameToSvg
  )
where

import Control.Monad (forM)
import Data.Foldable (toList)
import Data.IORef (readIORef)
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Primitive.SmallArray (SmallArray, smallArrayFromList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Word (Word32, Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekByteOff)
import NanoUI
  ( Color (..)
  , DrawOp (..)
  , Input (..)
  , Rect (..)
  , Size (..)
  , colorA
  , colorRGBA
  , colorToHex
  , rectIntersect
  , themeWindow
  )
import NanoUI.Backend
  ( DrawCmd (..)
  , DrawData (..)
  , FontMetrics (..)
  , atlasTextureId
  , collectRasterSpans
  , drawCmdElems
  , indexSize
  , vertexSize
  )
import NanoUI.Draw.Svg (SvgConfig (..), defaultSvgConfig, renderDrawOpsSvg)
import NanoUI.Internal.Context
  ( Context (..)
  , DrawingCacheState (..)
  , intKey
  )
import NanoUI.Internal.Context.Types
  ( CustomDrawOpCacheEntry (..)
  , DrawOpCacheEntry (..)
  )
import NanoUI.Internal.Layout.Arena (arenaCount, getWidgetId)

-- | Options for 'frameToSvg'.
data FrameSvgConfig = FrameSvgConfig
  { fsPrecision :: !Int
  -- ^ Maximum decimal places for coordinates and lengths (clamped to
  -- @0..6@). One or two digits suits a whole window.
  , fsFontFamily :: !Text
  -- ^ The @font-family@ spans render in. Text is fitted to its measured
  -- width with @textLength@, so any family that lays out similarly to the
  -- context's font reads correctly.
  , fsImageUri :: !(Int -> Maybe Text)
  -- ^ As 'svgImageUri': resolves a drawing's image id to an @href@ or data
  -- URI. Images without a resolution are omitted.
  }

-- | One decimal place, a sans-serif stack, and no image resolver.
defaultFrameSvgConfig :: FrameSvgConfig
defaultFrameSvgConfig =
  FrameSvgConfig
    { fsPrecision = 1
    , fsFontFamily = "Inter, 'Segoe UI', system-ui, sans-serif"
    , fsImageUri = const Nothing
    }

-- | Render the frame 'runFrame' returned for @inp@ into a standalone SVG
-- document sized to the input's window. Text draws last, over the shapes
-- and vector drawings beneath it, in paint order for each.
frameToSvg :: FrameSvgConfig -> Context -> Input -> DrawData -> IO Text
frameToSvg cfg ctx inp dd = do
  (pageSpans, overlaySpans) <- collectRasterSpans ctx inp
  theme <- readIORef (ctxTheme ctx)
  live <- liveDrawingIds ctx
  dc <- readIORef (ctxDrawingCache ctx)
  let
    cached =
      [ (doeBounds e, doeOps e)
      | (k, e) <- IM.toList (dcsDrawOpCache dc)
      , IS.member k live
      ]
        ++ [ (cdeBounds e, cdeOps e)
           | (k, e) <- IM.toList (dcsCustomDrawOpCache dc)
           , IS.member k live
           ]
  shapes <- frameShapes (map fst cached) dd
  let
    Size vw vh = inputWindowSize inp
    fm = ctxFontMetrics ctx
    sizeRatio = ctxFontSize ctx / fmLineHeight fm
    fragCfg =
      defaultSvgConfig
        { svgStandalone = False
        , svgPrecision = fsPrecision cfg
        , svgImageUri = fsImageUri cfg
        }
    vp = Rect 0 0 vw vh
    frags =
      mconcat
        [ TB.fromText (renderDrawOpsSvg fragCfg vp (vectorOnly ops))
        | (_, ops) <- cached
        ]
    (textDefs, textBody) = spanTexts (fsFontFamily cfg) sizeRatio (pageSpans ++ overlaySpans)
    doc =
      "<svg xmlns=\"http://www.w3.org/2000/svg\""
        <> attr "width" (fmt 0 vw)
        <> attr "height" (fmt 0 vh)
        <> attr
          "viewBox"
          (fmt 0 0 <> " " <> fmt 0 0 <> " " <> fmt 0 vw <> " " <> fmt 0 vh)
        <> ">"
        <> textDefs
        <> rectEl (Rect 0 0 vw vh) (themeWindow theme)
        <> mconcat (map shapeEl shapes)
        <> frags
        <> textBody
        <> "</svg>"
  pure (TL.toStrict (TB.toLazyText doc))

-- | The widget ids the view declared this frame, so drawings left in the
-- caches by widgets no longer shown (another tab's charts) are skipped.
liveDrawingIds :: Context -> IO IS.IntSet
liveDrawingIds ctx = do
  let
    na = ctxNodeArena ctx
  n <- arenaCount na
  IS.fromList <$> mapM (\i -> intKey <$> getWidgetId na i) [0 .. n - 1]

-- | A drawing's ops re-rendered as vectors; its text arrives as a span
-- instead, and its images resolve through the config.
vectorOnly :: SmallArray DrawOp -> SmallArray DrawOp
vectorOnly = smallArrayFromList . filter keep . toList
 where
  keep DrawText {} = False
  keep DrawTextStyled {} = False
  keep DrawTextAligned {} = False
  keep DrawImage {} = False
  keep _ = True

--------------------------------------------------------------------------------
-- Shapes from the vertex buffer
--------------------------------------------------------------------------------

data Shape
  = ShapeRect !Rect !Color
  | ShapeTri !Float !Float !Float !Float !Float !Float !Color

-- | A frame's solid geometry as flat shapes in paint order, each cut to its
-- command's clip. Shapes inside @skip@ (the drawing widgets' bounds) are
-- left out: those render from their draw ops instead. Textured commands
-- (glyph quads, atlas images) draw nothing here.
frameShapes :: [Rect] -> DrawData -> IO [Shape]
frameShapes skip dd =
  withForeignPtr (drawVertices dd) $ \vp ->
    withForeignPtr (drawIndices dd) $ \ip -> do
      let
        -- Solid shapes draw with the glyph atlas's texture (they sample
        -- its white pixel), so only the image atlas's quads are left out;
        -- under external text no glyph quads exist to confuse with them.
        cmds = [c | c <- drawCmdElems dd, cmdTextureId c /= atlasTextureId]
      perCmd <- forM cmds $ \c -> do
        let
          ioff = fromIntegral (cmdIndexOffset c)
          icnt = fromIntegral (cmdIndexCount c) :: Int
          clip = Rect (cmdClipX c) (cmdClipY c) (cmdClipW c) (cmdClipH c)
        idxs <-
          forM [ioff .. ioff + icnt - 1] $ \ii ->
            peekByteOff ip (ii * indexSize) :: IO Word32
        concat <$> mapM (chunkShapes vp clip skip) (chunksOf6 idxs)
      pure (concat perCmd)

chunksOf6 :: [a] -> [[a]]
chunksOf6 [] = []
chunksOf6 xs = take 6 xs : chunksOf6 (drop 6 xs)

-- One six-index group: a quad when it holds four distinct corners, else two
-- triangles; a trailing three is a lone triangle.
chunkShapes :: Ptr Word8 -> Rect -> [Rect] -> [Word32] -> IO [Shape]
chunkShapes vp clip skip = \case
  [a, b, c, d, e, f]
    | length (nub [a, b, c, d, e, f]) == 4 -> quadOf [a, b, c, d, e, f]
    | otherwise -> (++) <$> triOf a b c <*> triOf d e f
  [a, b, c] -> triOf a b c
  _ -> pure []
 where
  quadOf [] = pure []
  quadOf (q0 : rest) = do
    xy <- mapM (vert vp) (nub (q0 : rest))
    col <- vertColor vp q0
    let
      x0 = minimum (map fst xy)
      y0 = minimum (map snd xy)
      x1 = maximum (map fst xy)
      y1 = maximum (map snd xy)
      r = Rect x0 y0 (x1 - x0) (y1 - y0)
    pure
      [ ShapeRect r' col
      | colorA col > 0
      , not (inSkip r)
      , Just r' <- [rectIntersect clip r]
      ]
  triOf a b c = do
    (x0, y0) <- vert vp a
    (x1, y1) <- vert vp b
    (x2, y2) <- vert vp c
    col <- vertColor vp a
    let
      bx0 = minimum [x0, x1, x2]
      by0 = minimum [y0, y1, y2]
      bx1 = maximum [x0, x1, x2]
      by1 = maximum [y0, y1, y2]
      bounds = Rect bx0 by0 (bx1 - bx0) (by1 - by0)
    pure
      [ ShapeTri x0 y0 x1 y1 x2 y2 col
      | colorA col > 0
      , rectW bounds > 0.01
      , rectH bounds > 0.01
      , not (inSkip bounds)
      , rectIntersect bounds clip /= Nothing
      ]
  inSkip r = any (fullyInside r) skip

vert :: Ptr Word8 -> Word32 -> IO (Float, Float)
vert vp vi = do
  x <- peekByteOff vp (fromIntegral vi * vertexSize) :: IO Float
  y <- peekByteOff vp (fromIntegral vi * vertexSize + 4)
  pure (x, y)

-- A vertex's straight RGBA colour, as the flat passes paint it.
vertColor :: Ptr Word8 -> Word32 -> IO Color
vertColor vp vi = do
  r <- channel 8
  g <- channel 12
  b <- channel 16
  a <- channel 20
  pure (colorRGBA r g b a)
 where
  channel o = do
    f <- peekByteOff vp (fromIntegral vi * vertexSize + o) :: IO Float
    pure (fromIntegral (max 0 (min 255 (round (f * 255))) :: Int) :: Word8)

--------------------------------------------------------------------------------
-- Text from the raster spans
--------------------------------------------------------------------------------

-- Blank-glyph markers some span labels carry (sort arrows, reserve padding).
dropMarkers :: Text -> Text
dropMarkers = T.dropWhile (`elem` ['\x01', '\x02', '\x05'])

-- | Every span as background, clipped group, and fitted text. Spans whose
-- line box is not fully inside their clip share one @clipPath@ per clip.
spanTexts ::
  Text -> Float -> [(Rect, Text, Color, Color, Rect)] -> (TB.Builder, TB.Builder)
spanTexts fontFam ratio spans = (defs, body)
 where
  kept =
    [ (r, clean t, fg, bg, c)
    | (r, t, fg, bg, c) <- spans
    , not (T.null (clean t))
    , rectW r > 0.5
    , rectH r > 0.5
    , rectIntersect r c /= Nothing
    ]
  clean = T.strip . dropMarkers
  needClips = nub [c | (r, _, _, _, c) <- kept, not (fullyInside r c)]
  clipId c = fromMaybe 0 (lookup c (zip needClips [1 :: Int ..]))
  defs
    | null needClips = mempty
    | otherwise =
        "<defs>"
          <> mconcat
            [ "<clipPath id=\"tclip_"
                <> fmt 0 i
                <> "\"><rect"
                <> num "x" 1 (rectX c)
                <> num "y" 1 (rectY c)
                <> num "width" 1 (rectW c)
                <> num "height" 1 (rectH c)
                <> "/></clipPath>"
            | (c, i) <- zip needClips [1 ..]
            ]
          <> "</defs>"
  body = mconcat (map el kept)
  el (r@(Rect x y w h), t, fg, bg, c) = wrapClipped (bgEl <> textEl)
   where
    wrapClipped b
      | fullyInside r c = b
      | otherwise =
          "<g clip-path=\"url(#tclip_"
            <> fmt 0 (fromIntegral (clipId c))
            <> ")\">"
            <> b
            <> "</g>"
    bgEl = if colorA bg > 0 then rectEl r bg else mempty
    textEl =
      "<text"
        <> num "x" 1 x
        <> num "y" 1 (y + h / 2)
        <> attr "dominant-baseline" "central"
        <> num "font-size" 1 (h * ratio)
        <> attr "font-family" (TB.fromText fontFam)
        <> attr "xml:space" "preserve"
        <> fillAttrs fg
        <> num "textLength" 1 (max 1 w)
        <> attr "lengthAdjust" "spacingAndGlyphs"
        <> ">"
        <> escape t
        <> "</text>"

--------------------------------------------------------------------------------
-- SVG fragments
--------------------------------------------------------------------------------

shapeEl :: Shape -> TB.Builder
shapeEl (ShapeRect r col) = rectEl r col
shapeEl (ShapeTri x0 y0 x1 y1 x2 y2 col)
  | colorA col == 0 = mempty
  | otherwise =
      "<polygon"
        <> attr
          "points"
          ( fmt 1 x0
              <> ","
              <> fmt 1 y0
              <> " "
              <> fmt 1 x1
              <> ","
              <> fmt 1 y1
              <> " "
              <> fmt 1 x2
              <> ","
              <> fmt 1 y2
          )
        <> fillAttrs col
        <> "/>"

rectEl :: Rect -> Color -> TB.Builder
rectEl (Rect x y w h) col
  | colorA col == 0 || w <= 0.01 || h <= 0.01 = mempty
  | otherwise =
      "<rect"
        <> num "x" 1 x
        <> num "y" 1 y
        <> num "width" 1 w
        <> num "height" 1 h
        <> fillAttrs col
        <> "/>"

fillAttrs :: Color -> TB.Builder
fillAttrs col =
  attr "fill" (TB.fromText (colorToHex col))
    <> ( if a < 255
           then attr "fill-opacity" (fmt 2 (fromIntegral a / 255))
           else mempty
       )
 where
  a = colorA col

fullyInside :: Rect -> Rect -> Bool
fullyInside (Rect x y w h) (Rect sx sy sw sh) =
  x >= sx && y >= sy && x + w <= sx + sw && y + h <= sy + sh

attr :: TB.Builder -> TB.Builder -> TB.Builder
attr k v = " " <> k <> "=\"" <> v <> "\""

num :: TB.Builder -> Int -> Float -> TB.Builder
num k p v = attr k (fmt p v)

-- | Decimal digits, rounded half up, without trailing zeros or exponents.
fmt :: Int -> Float -> TB.Builder
fmt p v
  | isNaN v || isInfinite v = "0"
  | n == 0 = "0"
  | frac == 0 = sign <> TB.fromString (show whole)
  | otherwise =
      let
        raw = show frac
        padded = replicate (p - length raw) '0' ++ raw
        trimmed = reverse (dropWhile (== '0') (reverse padded))
       in
        sign <> TB.fromString (show whole) <> "." <> TB.fromString trimmed
 where
  !scale = 10 ^ p :: Integer
  !mag = floor (abs (realToFrac v :: Double) * fromIntegral scale + 0.5) :: Integer
  !n = if v < 0 then negate mag else mag
  !sign = if n < 0 then "-" else mempty
  !(whole, frac) = quotRem mag scale

escape :: Text -> TB.Builder
escape = T.foldl' (\acc c -> acc <> esc c) mempty
 where
  esc = \case
    '&' -> "&amp;"
    '<' -> "&lt;"
    '>' -> "&gt;"
    '"' -> "&quot;"
    c -> TB.singleton c
