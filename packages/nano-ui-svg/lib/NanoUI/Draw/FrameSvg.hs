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

import Control.Applicative ((<|>))
import Control.Monad (forM)
import Data.Bits (shiftR, (.&.))
import Data.Foldable (toList)
import Data.IORef (readIORef)
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.List (nub, partition)
import Data.Maybe (fromMaybe, listToMaybe)
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
  , Layer (..)
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
  , nodeTheme
  )
import NanoUI.Internal.Context.Types
  ( CustomDrawOpCacheEntry (..)
  , DrawOpCacheEntry (..)
  , SpanCacheEntry (..)
  )
import NanoUI.Internal.Layout.Arena
  ( NodeType (..)
  , arenaCount
  , getNodeRect
  , getNodeType
  , getStyleIdx
  , getWidgetId
  )

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
  spanStyles <- collectSpanStyles ctx
  baseTheme <- readIORef (ctxTheme ctx)
  theme <- do
    cnt <- arenaCount (ctxNodeArena ctx)
    if cnt > 0
      then nodeTheme ctx 0
      else pure baseTheme
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
  (pageShapes, overlayShapes) <- frameShapes (map fst cached) dd
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
    (pageShapeDefs, pageShapeBody, nextSClip) = renderShapes 1 pageShapes
    (overlayShapeDefs, overlayShapeBody, _) = renderShapes nextSClip overlayShapes
    (pageTextDefs, pageTextBody, nextTClip) = spanTexts (fsFontFamily cfg) fm sizeRatio spanStyles 1 pageSpans
    (overlayTextDefs, overlayTextBody, _) = spanTexts (fsFontFamily cfg) fm sizeRatio spanStyles nextTClip overlaySpans
    allDefs
      | nullDefs = mempty
      | otherwise = "<defs>" <> pageShapeDefs <> overlayShapeDefs <> pageTextDefs <> overlayTextDefs <> "</defs>"
    nullDefs =
      TL.null (TB.toLazyText pageShapeDefs)
        && TL.null (TB.toLazyText overlayShapeDefs)
        && TL.null (TB.toLazyText pageTextDefs)
        && TL.null (TB.toLazyText overlayTextDefs)
    doc =
      "<svg xmlns=\"http://www.w3.org/2000/svg\""
        <> attr "width" (fmt 0 vw)
        <> attr "height" (fmt 0 vh)
        <> attr
          "viewBox"
          (fmt 0 0 <> " " <> fmt 0 0 <> " " <> fmt 0 vw <> " " <> fmt 0 vh)
        <> ">"
        <> allDefs
        <> rectEl (Rect 0 0 vw vh) (themeWindow theme)
        <> pageShapeBody
        <> frags
        <> pageTextBody
        <> overlayShapeBody
        <> overlayTextBody
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
  | ShapeRoundedRect !Rect !Float !Color !(Maybe Rect)
  | ShapeRoundedStroke !Rect !Float !Float !Color !(Maybe Rect)
  | ShapeLine !Float !Float !Float !Float !Float !Color !(Maybe Rect)
  | ShapeQuad !Float !Float !Float !Float !Float !Float !Float !Float !Color !(Maybe Rect)
  | ShapeTri !Float !Float !Float !Float !Float !Float !Color !(Maybe Rect)

-- | A frame's solid geometry as vector shapes in paint order, partitioned
-- into page shapes ('LayerBackground', 'LayerContent') and overlay shapes
-- ('LayerOverlay', 'LayerChrome'), each cut or clipped to its command's clip.
-- Shapes inside @skip@ (the drawing widgets' bounds) are left out: those render
-- from their draw ops instead. Textured commands (glyph quads, atlas images)
-- draw nothing here.
frameShapes :: [Rect] -> DrawData -> IO ([Shape], [Shape])
frameShapes skip dd =
  withForeignPtr (drawVertices dd) $ \vp ->
    withForeignPtr (drawIndices dd) $ \ip -> do
      let
        -- Solid shapes draw with the glyph atlas's texture (they sample
        -- its white pixel), so only the image atlas's quads are left out;
        -- under external text no glyph quads exist to confuse with them.
        cmds = [c | c <- drawCmdElems dd, cmdTextureId c /= atlasTextureId]
        (pageCmds, overlayCmds) = partition (\c -> cmdLayer c < LayerOverlay) cmds
        walkCmd c = do
          let
            ioff = fromIntegral (cmdIndexOffset c)
            icnt = fromIntegral (cmdIndexCount c) :: Int
            clip = Rect (cmdClipX c) (cmdClipY c) (cmdClipW c) (cmdClipH c)
          idxs <-
            forM [ioff .. ioff + icnt - 1] $ \ii ->
              peekByteOff ip (ii * indexSize) :: IO Word32
          walkShapes vp clip skip idxs
      pageShapes <- concat <$> mapM walkCmd pageCmds
      overlayShapes <- concat <$> mapM walkCmd overlayCmds
      pure (pageShapes, overlayShapes)

-- Walk the index stream, recognising composite tessellations emitted by
-- 'NanoUI.Internal.Draw.Shapes' (rounded fills, rounded borders, AA line
-- strips, AA triangles) before falling back to single quads and triangles.
walkShapes :: Ptr Word8 -> Rect -> [Rect] -> [Word32] -> IO [Shape]
walkShapes vp clip skip idxs = case idxs of
  [] -> pure []
  b0 : _
    | Just (quadCount, needI) <- matchRoundedFill b0 idxs -> do
        sh <- roundedFillOf b0 quadCount
        (sh ++) <$> walkShapes vp clip skip (drop needI idxs)
    | Just (stripCount, hasCore, hasBands, baseNeedI) <- matchRoundedStroke b0 idxs -> do
        (sh, extraI) <- roundedStrokeOf b0 stripCount hasCore hasBands
        (sh ++) <$> walkShapes vp clip skip (drop (baseNeedI + extraI) idxs)
    | matchAATriangle b0 idxs -> do
        sh <- aaTriOf b0
        (sh ++) <$> walkShapes vp clip skip (drop 21 idxs)
    | matchAAStrip b0 idxs -> do
        sh <- aaStripOf b0
        (sh ++) <$> walkShapes vp clip skip (drop 18 idxs)
  a : b : c : d : e : f : rest
    | a == d && c == e && length (nub [a, b, c, f]) == 4 ->
        (++) <$> quadOf a b c f <*> walkShapes vp clip skip rest
  a : b : c : rest ->
    (++) <$> triOf a b c <*> walkShapes vp clip skip rest
  _ -> pure []
 where
  roundedFillOf b0 quadCount = do
    let c0 = b0 + fromIntegral (quadCount * 4)
    (cx0, cy0) <- vert vp c0
    (x0, _) <- vert vp (c0 + 6)
    (cx2, cy2) <- vert vp (c0 + 22)
    col <- vertColor vp c0
    let
      rad = max 0 (cx0 - x0)
      x = x0
      y = cy0 - rad
      w = (cx2 + rad) - x
      h = (cy2 + rad) - y
      r = Rect x y w h
      mClip = if fullyInside r clip then Nothing else Just clip
    pure
      [ ShapeRoundedRect r rad col mClip
      | colorA col > 0
      , w > 0.01
      , h > 0.01
      , not (inSkip r)
      , rectIntersect clip r /= Nothing
      ]

  roundedStrokeOf b0 stripCount hasCore hasBands = do
    let
      stride = if hasCore then 4 else 3 :: Int
      arcV = 5 * stride
      a0 = b0 + fromIntegral (stripCount * 8)
      a2 = a0 + fromIntegral (2 * arcV)
    (xInnerAA, ccy0) <- vert vp a0
    (xInner, _) <- vert vp (a0 + 1)
    (xOuterR, _) <- vert vp (a0 + (if hasCore then 2 else 1))
    (xOuterAA, _) <- vert vp (a0 + fromIntegral stride - 1)
    (ccx0, _) <- vert vp (a0 + fromIntegral (4 * stride))
    (_, ccy2) <- vert vp a2
    (ccx2, _) <- vert vp (a2 + fromIntegral (4 * stride))
    col <- vertColor vp (a0 + 1)
    let
      inner = ccx0 - xInner
      outerR = ccx0 - xOuterR
      rad = max 0 (outerR + 0.25)
      px = ccx0 - rad
      py = ccy0 - rad
      w = (ccx2 + rad) - px
      h = (ccy2 + rad) - py
      r = Rect px py w h
      mClip = if fullyInside r clip then Nothing else Just clip
    (ibw, extraI) <-
      if hasBands && inner < 0.25
        then do
          (bx, by0) <- vert vp (a0 + fromIntegral (4 * arcV))
          (_, by1) <- vert vp (a0 + fromIntegral (4 * arcV) + 2)
          if abs (bx - px) < 1e-2 && abs (by0 - (py + rad)) < 1e-2
            then pure (max 0.5 (by1 - py), 24)
            else pure (max 0.5 ((outerR - inner) + 0.5), 0)
        else
          pure
            ( if hasCore
                then max 0.5 ((outerR - inner) + 0.5)
                else max 0.5 ((ccx0 - xOuterAA) - (ccx0 - xInnerAA) - 0.5)
            , 0
            )
    pure
      ( [ ShapeRoundedStroke r rad ibw col mClip
        | colorA col > 0
        , w > 0.01
        , h > 0.01
        , ibw > 0.01
        , not (inSkip r)
        , rectIntersect clip r /= Nothing
        ]
      , extraI
      )

  aaTriOf b0 = do
    (ix0, iy0) <- vert vp b0
    (ox0, oy0) <- vert vp (b0 + 3)
    (ix1, iy1) <- vert vp (b0 + 1)
    (ox1, oy1) <- vert vp (b0 + 4)
    (ix2, iy2) <- vert vp (b0 + 2)
    (ox2, oy2) <- vert vp (b0 + 5)
    col <- vertColor vp b0
    let
      x0 = (ix0 + ox0) * 0.5
      y0 = (iy0 + oy0) * 0.5
      x1 = (ix1 + ox1) * 0.5
      y1 = (iy1 + oy1) * 0.5
      x2 = (ix2 + ox2) * 0.5
      y2 = (iy2 + oy2) * 0.5
      bx0 = minimum [x0, x1, x2]
      by0 = minimum [y0, y1, y2]
      bx1 = maximum [x0, x1, x2]
      by1 = maximum [y0, y1, y2]
      bounds = Rect bx0 by0 (bx1 - bx0) (by1 - by0)
      mClip = if fullyInside bounds clip then Nothing else Just clip
    pure
      [ ShapeTri x0 y0 x1 y1 x2 y2 col mClip
      | colorA col > 0
      , rectW bounds > 0.01
      , rectH bounds > 0.01
      , not (inSkip bounds)
      , rectIntersect bounds clip /= Nothing
      ]

  aaStripOf b0 = do
    (p0x, p0y) <- vert vp b0
    (p3x, p3y) <- vert vp (b0 + 3)
    (q0x, q0y) <- vert vp (b0 + 4)
    (q3x, q3y) <- vert vp (b0 + 7)
    col <- vertColor vp (b0 + 1)
    let
      x0 = (p0x + p3x) * 0.5
      y0 = (p0y + p3y) * 0.5
      x1 = (q0x + q3x) * 0.5
      y1 = (q0y + q3y) * 0.5
      d0 = sqrt ((p3x - p0x) ^ (2 :: Int) + (p3y - p0y) ^ (2 :: Int))
      d1 = sqrt ((q3x - q0x) ^ (2 :: Int) + (q3y - q0y) ^ (2 :: Int))
      bw = max 0.5 (min d0 d1 - 1.0)
    if abs (y0 - y1) < 1e-3
      then do
        let r = Rect (min x0 x1) (y0 - bw * 0.5) (abs (x1 - x0)) bw
        pure
          [ ShapeRect r' col
          | colorA col > 0
          , not (inSkip r)
          , Just r' <- [rectIntersect clip r]
          ]
      else if abs (x0 - x1) < 1e-3
        then do
          let r = Rect (x0 - bw * 0.5) (min y0 y1) bw (abs (y1 - y0))
          pure
            [ ShapeRect r' col
            | colorA col > 0
            , not (inSkip r)
            , Just r' <- [rectIntersect clip r]
            ]
        else do
          let
            bx0 = min x0 x1 - bw * 0.5
            by0 = min y0 y1 - bw * 0.5
            bx1 = max x0 x1 + bw * 0.5
            by1 = max y0 y1 + bw * 0.5
            bounds = Rect bx0 by0 (bx1 - bx0) (by1 - by0)
            mClip = if fullyInside bounds clip then Nothing else Just clip
          pure
            [ ShapeLine x0 y0 x1 y1 bw col mClip
            | colorA col > 0
            , not (inSkip bounds)
            , rectIntersect bounds clip /= Nothing
            ]

  quadOf v0 v1 v2 v3 = do
    (x0, y0) <- vert vp v0
    (x1, y1) <- vert vp v1
    (x2, y2) <- vert vp v2
    (x3, y3) <- vert vp v3
    col <- vertColor vp v0
    cols <- mapM (vertColor vp) [v0, v1, v2, v3]
    let
      allOpaque = all (\c -> colorA c > 0) cols
      bx0 = minimum [x0, x1, x2, x3]
      by0 = minimum [y0, y1, y2, y3]
      bx1 = maximum [x0, x1, x2, x3]
      by1 = maximum [y0, y1, y2, y3]
      r = Rect bx0 by0 (bx1 - bx0) (by1 - by0)
      axisAligned =
        (abs (y0 - y1) < 1e-3 && abs (x1 - x2) < 1e-3 && abs (y2 - y3) < 1e-3 && abs (x3 - x0) < 1e-3)
          || (abs (x0 - x1) < 1e-3 && abs (y1 - y2) < 1e-3 && abs (x2 - x3) < 1e-3 && abs (y3 - y0) < 1e-3)
      mClip = if fullyInside r clip then Nothing else Just clip
    if not allOpaque || inSkip r
      then pure []
      else
        if axisAligned
          then pure [ShapeRect r' col | Just r' <- [rectIntersect clip r]]
          else pure [ShapeQuad x0 y0 x1 y1 x2 y2 x3 y3 col mClip | rectIntersect clip r /= Nothing]

  triOf a b c = do
    (x0, y0) <- vert vp a
    (x1, y1) <- vert vp b
    (x2, y2) <- vert vp c
    col <- vertColor vp a
    cols <- mapM (vertColor vp) [a, b, c]
    let
      allOpaque = all (\ch -> colorA ch > 0) cols
      bx0 = minimum [x0, x1, x2]
      by0 = minimum [y0, y1, y2]
      bx1 = maximum [x0, x1, x2]
      by1 = maximum [y0, y1, y2]
      bounds = Rect bx0 by0 (bx1 - bx0) (by1 - by0)
      mClip = if fullyInside bounds clip then Nothing else Just clip
    pure
      [ ShapeTri x0 y0 x1 y1 x2 y2 col mClip
      | allOpaque
      , rectW bounds > 0.01
      , rectH bounds > 0.01
      , not (inSkip bounds)
      , rectIntersect bounds clip /= Nothing
      ]
  inSkip r = any (fullyInside r) skip

-- Recognise 'pushRoundedRectRaw' (0, 2, or 5 inner quads + 4 corner fans/fringes).
matchRoundedFill :: Word32 -> [Word32] -> Maybe (Int, Int)
matchRoundedFill b0 idxs =
  case [ (qc, needI)
       | qc <- [5, 2, 0]
       , let needI = qc * 6 + 144
       , let c0 = b0 + fromIntegral (qc * 4)
       , case drop (qc * 6) idxs of
           v0 : v1 : v2 : v3 : v4 : v5 : _ ->
             v0 == c0 && v1 == c0 + 1 && v2 == c0 + 2 && v3 == c0 && v4 == c0 + 2 && v5 == c0 + 3
           _ -> False
       , take needI idxs == roundedFillIndices b0 qc
       ] of
    (hit : _) -> Just hit
    [] -> Nothing

roundedFillIndices :: Word32 -> Int -> [Word32]
roundedFillIndices b0 quadCount =
  concat
    [ [vq, vq + 1, vq + 2, vq, vq + 2, vq + 3]
    | m <- [0 .. quadCount - 1]
    , let vq = b0 + fromIntegral (m * 4)
    ]
    ++ concat
      [ let
          cb = b0 + fromIntegral (quadCount * 4 + q * 11)
          fan = concat [[cb, cb + k, cb + k + 1] | k <- [1 .. 4]]
          fringe =
            concat
              [ [cb + k, cb + k + 5, cb + k + 6, cb + k, cb + k + 6, cb + k + 1]
              | k <- [1 .. 4]
              ]
         in
          fan ++ fringe
      | q <- [0 .. 3]
      ]

-- Recognise 'pushRoundedStrokeRaw' (0, 2, or 4 straight AA strips + 4 corner arcs + optional bands).
matchRoundedStroke :: Word32 -> [Word32] -> Maybe (Int, Bool, Bool, Int)
matchRoundedStroke b0 idxs =
  case [ (sc, hasCore, hasBands, baseNeedI)
       | sc <- [4, 2, 0]
       , hasCore <- [True, False]
       , let stride = if hasCore then 4 else 3 :: Word32
       , let arcV = 5 * stride
       , let arcI = 4 * (if hasCore then 18 else 12 :: Int)
       , let iiC = sc * 18
       , let baseNeedI = iiC + 4 * arcI
       , let a0 = b0 + fromIntegral (sc * 8)
       , case drop iiC idxs of
           v0 : v1 : v2 : _ -> v0 == a0 && v1 == a0 + 1 && v2 == a0 + stride + 1
           _ -> False
       , take baseNeedI idxs == roundedStrokeIndices hasCore b0 sc
       , let bBand = a0 + 4 * arcV
       , let hasBands = take 24 (drop baseNeedI idxs) == bandQuadIndices bBand
       ] of
    (hit : _) -> Just hit
    [] -> Nothing

roundedStrokeIndices :: Bool -> Word32 -> Int -> [Word32]
roundedStrokeIndices hasCore b0 stripCount =
  let
    stride = if hasCore then 4 else 3 :: Word32
    arcV = 5 * stride
    a0 = b0 + fromIntegral (stripCount * 8)
   in
    concat
      [ bandIndices True vs (vs + 4)
      | s <- [0 .. stripCount - 1]
      , let vs = b0 + fromIntegral (s * 8)
      ]
      ++ concat
        [ bandIndices hasCore va (va + stride)
        | q <- [0 .. 3 :: Word32]
        , i <- [0 .. 3 :: Word32]
        , let va = a0 + q * arcV + i * stride
        ]

bandIndices :: Bool -> Word32 -> Word32 -> [Word32]
bandIndices hasCore va vb =
  [va, va + 1, vb + 1, va, vb + 1, vb]
    ++ [va + 1, va + 2, vb + 2, va + 1, vb + 2, vb + 1]
    ++ if hasCore
      then [va + 2, va + 3, vb + 3, va + 2, vb + 3, vb + 2]
      else []

bandQuadIndices :: Word32 -> [Word32]
bandQuadIndices bBand =
  concat
    [ [vq, vq + 1, vq + 2, vq, vq + 2, vq + 3]
    | k <- [0 .. 3 :: Word32]
    , let vq = bBand + k * 4
    ]

matchAATriangle :: Word32 -> [Word32] -> Bool
matchAATriangle b0 = \case
  _ : b1 : b2 : b0' : b1' : b4 : rest ->
    b1 == b0 + 1
      && b2 == b0 + 2
      && b0' == b0
      && b1' == b0 + 1
      && b4 == b0 + 4
      && take 15 rest
        == [b0, b0 + 4, b0 + 3]
          ++ [b0 + 1, b0 + 2, b0 + 5, b0 + 1, b0 + 5, b0 + 4]
          ++ [b0 + 2, b0, b0 + 3, b0 + 2, b0 + 3, b0 + 5]
  _ -> False

matchAAStrip :: Word32 -> [Word32] -> Bool
matchAAStrip b0 = \case
  _ : b1 : b5 : b0' : b5' : b4 : rest ->
    b1 == b0 + 1
      && b5 == b0 + 5
      && b0' == b0
      && b5' == b0 + 5
      && b4 == b0 + 4
      && take 12 rest
        == [b0 + 1, b0 + 2, b0 + 6, b0 + 1, b0 + 6, b0 + 5]
          ++ [b0 + 2, b0 + 3, b0 + 7, b0 + 2, b0 + 7, b0 + 6]
  _ -> False

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

-- | Collect packed font-style codes for 'NodeText' and 'NodeTextInput' spans.
collectSpanStyles :: Context -> IO [(Rect, Int)]
collectSpanStyles ctx = do
  sc <- readIORef (ctxSpanCache ctx)
  let
    textStyles =
      [ (r, sceStyle e)
      | e <- IM.elems sc
      , sceStyle e /= 0
      , (r, _, _, _) <- sceSpans e
      ]
    na = ctxNodeArena ctx
  n <- arenaCount na
  inputStyles <-
    forM [0 .. n - 1] $ \i -> do
      nt <- getNodeType na i
      if nt == NodeTextInput
        then do
          si <- getStyleIdx na i
          if si /= 0
            then Just . (,si) <$> getNodeRect na i
            else pure Nothing
        else pure Nothing
  pure (textStyles ++ [p | Just p <- inputStyles])


-- | Every span as clipped group and fitted text. Spans whose line box is
-- not fully inside their clip share one @clipPath@ per clip.
spanTexts ::
  Text -> FontMetrics -> Float -> [(Rect, Int)] -> Int -> [(Rect, Text, Color, Color, Rect)] -> (TB.Builder, TB.Builder, Int)
spanTexts fontFam fm ratio spanStyles startId spans = (defs, body, startId + length needClips)
 where
  baseLineH = fmLineHeight fm
  spaceAdv = fmAdvance fm ' '
  lookupStyle r0 =
    fromMaybe
      0
      ( lookup r0 spanStyles
          <|> listToMaybe [si | (nr, si) <- spanStyles, fullyInside r0 nr]
      )
  fontStyleAttrs si =
    attr "font-family" (TB.fromText fam)
      <> weightAttr
      <> slantAttr
      <> decoAttr
   where
    fam = case si .&. 0x0F of
      1 -> "'Cascadia Mono', 'JetBrains Mono', monospace"
      _ -> fontFam
    weightAttr = case (si `shiftR` 8) .&. 0x0F of
      1 -> attr "font-weight" "100"
      2 -> attr "font-weight" "200"
      3 -> attr "font-weight" "300"
      4 -> mempty -- normal
      5 -> attr "font-weight" "500"
      6 -> attr "font-weight" "600"
      7 -> attr "font-weight" "700"
      8 -> attr "font-weight" "800"
      9 -> attr "font-weight" "900"
      _ -> mempty
    slantAttr = case (si `shiftR` 12) .&. 0x03 of
      1 -> attr "font-style" "italic"
      2 -> attr "font-style" "oblique"
      _ -> mempty
    decoAttr = case (si `shiftR` 14) .&. 0x03 of
      1 -> attr "text-decoration" "underline"
      2 -> attr "text-decoration" "line-through"
      3 -> attr "text-decoration" "underline line-through"
      _ -> mempty
  trimSpan (Rect x y w h) raw =
    let
      t0 = dropMarkers raw
      lead = T.length (T.takeWhile (== ' ') t0)
      trail = T.length (T.takeWhileEnd (== ' ') t0)
      t = T.strip t0
      spW = if baseLineH > 0 then spaceAdv * (h / baseLineH) else spaceAdv
      x' = x + fromIntegral lead * spW
      w' = max 1 (w - fromIntegral (lead + trail) * spW)
    in
      (Rect x' y w' h, t)
  kept =
    [ (r', t', fg, c, lookupStyle r)
    | (r, t, fg, _, c) <- spans
    , let (r', t') = trimSpan r t
    , not (T.null t')
    , rectW r > 0.5
    , rectH r > 0.5
    , rectIntersect r c /= Nothing
    ]
  needClips = nub [c | (r, _, _, c, _) <- kept, not (fullyInside r c)]
  clipId c = fromMaybe 0 (lookup c (zip needClips [startId ..]))
  defs
    | null needClips = mempty
    | otherwise =
        mconcat
          [ "<clipPath id=\"tclip_"
              <> fmt 0 (fromIntegral i)
              <> "\"><rect"
              <> num "x" 1 (rectX c)
              <> num "y" 1 (rectY c)
              <> num "width" 1 (rectW c)
              <> num "height" 1 (rectH c)
              <> "/></clipPath>"
          | (c, i) <- zip needClips [startId ..]
          ]
  body = mconcat (map el kept)
  el (r@(Rect x y w h), t, fg, c, si) = wrapClipped textEl
   where
    wrapClipped b
      | fullyInside r c = b
      | otherwise =
          "<g clip-path=\"url(#tclip_"
            <> fmt 0 (fromIntegral (clipId c))
            <> ")\">"
            <> b
            <> "</g>"
    textEl =
      "<text"
        <> num "x" 1 x
        <> num "y" 1 (y + h / 2)
        <> attr "dominant-baseline" "central"
        <> num "font-size" 1 (h * ratio)
        <> fontStyleAttrs si
        <> attr "xml:space" "preserve"
        <> fillAttrs fg
        <> num "textLength" 1 (max 1 w)
        <> attr "lengthAdjust" "spacing"
        <> ">"
        <> escape t
        <> "</text>"

--------------------------------------------------------------------------------
-- SVG fragments
--------------------------------------------------------------------------------

renderShapes :: Int -> [Shape] -> (TB.Builder, TB.Builder, Int)
renderShapes startId shapes = (defs, mconcat (map emit shapes), startId + length needClips)
 where
  needClips = nub [c | sh <- shapes, Just c <- [shapeClip sh]]
  clipId c = fromMaybe 0 (lookup c (zip needClips [startId ..]))
  defs
    | null needClips = mempty
    | otherwise =
        mconcat
          [ "<clipPath id=\"sclip_"
              <> fmt 0 (fromIntegral i)
              <> "\"><rect"
              <> num "x" 1 (rectX c)
              <> num "y" 1 (rectY c)
              <> num "width" 1 (rectW c)
              <> num "height" 1 (rectH c)
              <> "/></clipPath>"
          | (c, i) <- zip needClips [startId ..]
          ]
  emit sh = case shapeClip sh of
    Nothing -> shapeEl sh
    Just c ->
      "<g clip-path=\"url(#sclip_"
        <> fmt 0 (fromIntegral (clipId c))
        <> ")\">"
        <> shapeEl sh
        <> "</g>"

shapeClip :: Shape -> Maybe Rect
shapeClip = \case
  ShapeRect {} -> Nothing
  ShapeRoundedRect _ _ _ mc -> mc
  ShapeRoundedStroke _ _ _ _ mc -> mc
  ShapeLine _ _ _ _ _ _ mc -> mc
  ShapeQuad _ _ _ _ _ _ _ _ _ mc -> mc
  ShapeTri _ _ _ _ _ _ _ mc -> mc

shapeEl :: Shape -> TB.Builder
shapeEl (ShapeRect r col) = rectEl r col
shapeEl (ShapeRoundedRect (Rect x y w h) r col _)
  | colorA col == 0 || w <= 0.01 || h <= 0.01 = mempty
  | otherwise =
      let rad = min (max 0 r) (min (w * 0.5) (h * 0.5))
       in "<rect"
            <> num "x" 1 x
            <> num "y" 1 y
            <> num "width" 1 w
            <> num "height" 1 h
            <> (if rad > 0 then num "rx" 1 rad else mempty)
            <> fillAttrs col
            <> "/>"
shapeEl (ShapeRoundedStroke (Rect x y w h) r bw col _)
  | colorA col == 0 || w <= 0.01 || h <= 0.01 || bw <= 0.01 = mempty
  | otherwise =
      let
        ibw = min bw (min (w * 0.5) (h * 0.5))
        rad = min (max 0 r) (min (w * 0.5) (h * 0.5))
        rx = max 0 (rad - ibw * 0.5)
       in
        "<rect"
          <> num "x" 1 (x + ibw * 0.5)
          <> num "y" 1 (y + ibw * 0.5)
          <> num "width" 1 (max 0 (w - ibw))
          <> num "height" 1 (max 0 (h - ibw))
          <> (if rx > 0 then num "rx" 1 rx else mempty)
          <> strokeAttrs col ibw
          <> attr "fill" "none"
          <> "/>"
shapeEl (ShapeLine x0 y0 x1 y1 bw col _)
  | colorA col == 0 || bw <= 0.01 = mempty
  | otherwise =
      "<line"
        <> num "x1" 1 x0
        <> num "y1" 1 y0
        <> num "x2" 1 x1
        <> num "y2" 1 y1
        <> strokeAttrs col bw
        <> "/>"
shapeEl (ShapeQuad x0 y0 x1 y1 x2 y2 x3 y3 col _)
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
              <> " "
              <> fmt 1 x3
              <> ","
              <> fmt 1 y3
          )
        <> fillAttrs col
        <> "/>"
shapeEl (ShapeTri x0 y0 x1 y1 x2 y2 col _)
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

strokeAttrs :: Color -> Float -> TB.Builder
strokeAttrs col bw =
  attr "stroke" (TB.fromText (colorToHex col))
    <> ( if a < 255
           then attr "stroke-opacity" (fmt 2 (fromIntegral a / 255))
           else mempty
       )
    <> num "stroke-width" 1 bw
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
