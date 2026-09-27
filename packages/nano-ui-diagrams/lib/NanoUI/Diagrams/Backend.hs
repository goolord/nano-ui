-- | A diagrams-lib backend that renders a diagram to nano-ui @DrawOp@ values,
-- scaled into a target size.
module NanoUI.Diagrams.Backend
  ( NanoUIBackend (..)
  , B
  , diagramOps
  , diagramTextOps
  , letterbox
  )
where

import Control.Applicative ((<|>))
import Control.Lens (Lens', (^.), (^?))
import Data.Colour (AlphaColour, alphaChannel, black, over)
import Data.Colour.SRGB (RGB (..), toSRGB)
import Data.DList (DList)
import Data.DList qualified as DL
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Tree (Tree (Node))
import Data.Typeable (Typeable)
import Data.Primitive.PrimArray (primArrayToList)
import Data.Primitive.SmallArray (SmallArray, emptySmallArray, mapSmallArray', smallArrayFromList)
import Diagrams.Attributes (Dashing (..), LineCap (..), LineJoin (..), _dashingU, _lineCap, _lineJoin, _lineMiterLimit, _lineWidthU)
import Diagrams.Core
  ( Backend (..)
  , N
  , QDiagram
  , Renderable (..)
  , V
  , renderDia
  )
import Diagrams.Core qualified as DiaCore
import Diagrams.Core.Types (Annotation, RNode (..), RTree)
import Diagrams.Located (Located, unLoc)
import Diagrams.Path (Path, pathTrails)
import Diagrams.Prelude
  ( Any
  , P2
  , SizeSpec
  , Trail
  , V2 (..)
  , origin
  , papply
  , reflectY
  , reflectionY
  , size
  , unp2
  , (#)
  )
import Diagrams.Segment (FixedSegment (..))
import Diagrams.Trail (fixTrail, isLoop)
import Diagrams.TwoD.Adjust (adjustDia2D)
import Diagrams.TwoD.Attributes (_AC, _fillTexture, _lineTexture)
import Diagrams.TwoD.Path (FillRule (..), _fillRule)
import Diagrams.TwoD.Size (mkHeight)
import Diagrams.TwoD.Text (Text (..), TextAlignment (..))
import NanoUI
  ( Color
  , DrawOp (..)
  , Rect (..)
  , colorA
  , colorRGBA
  , defaultTheme
  , shiftDrawOp
  , themeMuted
  )
import NanoUI.Internal.Path qualified as P

-- | Diagrams backend that emits nano-ui draw operations for paths and text.
data NanoUIBackend = NanoUIBackend
  deriving (Eq, Show)

-- | Short backend name for @Diagram B@ type signatures.
type B = NanoUIBackend

type instance V NanoUIBackend = V2

type instance N NanoUIBackend = Double

fullSize :: Lens' (Options NanoUIBackend V2 n) (SizeSpec V2 n)
fullSize f (NanoUIOptions sz textOnly) = fmap (\sz' -> NanoUIOptions sz' textOnly) (f sz)

instance (Typeable n, RealFloat n) => Backend NanoUIBackend V2 n where
  newtype Render NanoUIBackend V2 n
    = NRenderFull (Bool -> DiaCore.Style V2 n -> DList DrawOp)
    deriving (Semigroup, Monoid)
  type Result NanoUIBackend V2 n = SmallArray DrawOp
  data Options NanoUIBackend V2 n = NanoUIOptions (SizeSpec V2 n) Bool
  renderRTree _ (NanoUIOptions _ textOnly) rt = smallArrayFromList (DL.toList (walkFull textOnly mempty rt))
  adjustDia c opts d = (sz, t <> reflectionY, d')
   where
    (sz, t, d') = adjustDia2D fullSize c opts (d # reflectY)

walkFull ::
  (Typeable n, RealFloat n) =>
  Bool
  -> DiaCore.Style V2 n
  -> RTree NanoUIBackend V2 n Annotation
  -> DList DrawOp
walkFull textOnly sty (Node n cs) =
  case n of
    RPrim prim ->
      let
        NRenderFull f = render NanoUIBackend prim
       in
        f textOnly sty
    RStyle s -> foldMap (walkFull textOnly (sty <> s)) cs
    _ -> foldMap (walkFull textOnly sty) cs

instance (Typeable n, RealFloat n) => Renderable (Path V2 n) NanoUIBackend where
  render _ path = NRenderFull $ \textOnly sty ->
    if textOnly
      then DL.empty
      else DL.fromList (pathOps sty (pathTrails path))

textOps ::
  (Typeable n, RealFloat n) => Text n -> DiaCore.Style V2 n -> DList DrawOp
textOps (Text tr align str) sty
  | null str = DL.empty
  | otherwise =
      let
        p = papply tr origin
        (x, y) = unp2 p
        (ax, ay) =
          case align of
            BaselineText -> (0, -1)
            BoxAlignedText bx by -> (toF bx, toF by)
        col = fromMaybe (themeMuted defaultTheme) (fillColour sty <|> lineColour sty)
       in
        DL.singleton (DrawText (toF x) (toF y) ax ay (T.pack str) col)

instance (Typeable n, RealFloat n) => Renderable (Text n) NanoUIBackend where
  render _ t = NRenderFull (const (textOps t))

-- | A path's ops: all loops filled together under the style's fill rule (so
-- an inner loop can be a hole), then every trail stroked with the style's
-- caps, joins and dashes.
pathOps ::
  (Typeable n, RealFloat n) =>
  DiaCore.Style V2 n -> [Located (Trail V2 n)] -> [DrawOp]
pathOps sty trails = fills ++ strokes
  where
    trailOf lt = let closed = isLoop (unLoc lt) in (closed, trailPath closed (fixTrail lt))
    paths = map trailOf trails
    loops = mconcat [path | (True, path) <- paths]
    lineW = case toF <$> sty ^. _lineWidthU of
      Nothing -> 1
      Just w
        | w <= 0 -> 0
        | otherwise -> max 1 w
    rule = case sty ^. _fillRule of
      Winding -> P.NonZero
      EvenOdd -> P.EvenOdd
    lineStroke =
      (P.stroke lineW)
        { P.strokeCap = case sty ^. _lineCap of
            LineCapButt -> P.ButtCap
            LineCapRound -> P.RoundCap
            LineCapSquare -> P.SquareCap
        , P.strokeJoin = case sty ^. _lineJoin of
            LineJoinMiter -> P.MiterJoin
            LineJoinRound -> P.RoundJoin
            LineJoinBevel -> P.BevelJoin
        , P.strokeMiterLimit = toF (sty ^. _lineMiterLimit)
        , P.strokeDash = maybe [] (\(Dashing ds _) -> map toF ds) (sty ^. _dashingU)
        , P.strokeDashOffset = maybe 0 (\(Dashing _ o) -> toF o) (sty ^. _dashingU)
        }
    fills = case fillColour sty of
      Just c | colorA c > 0 && any fst paths -> map rectOp (P.fillPathOps curveTol mempty rule loops (P.Solid c))
      _ -> []
    strokes = case lineColour sty of
      Just c | colorA c > 0 && lineW > 0 -> P.strokePathOps curveTol mempty lineStroke (mconcat (map snd paths)) (P.Solid c)
      _ -> []

-- | Maximum distance of a flattened curve from the true one, in logical pixels.
curveTol :: Float
curveTol = 0.5

-- | A trail as a canvas path, closed when it is a loop.
trailPath :: Real n => Bool -> [FixedSegment V2 n] -> P.Path
trailPath _ [] = mempty
trailPath closed segs@(s0 : _) = P.Path (start s0 : map seg segs ++ [P.SegClose | closed])
  where
    start (FLinear p _) = at P.SegMove p
    start (FCubic p _ _ _) = at P.SegMove p
    seg (FLinear _ p) = at P.SegLine p
    seg (FCubic _ c1 c2 p) =
      let (ax, ay) = pointFloats c1
          (bx, by) = pointFloats c2
       in at (P.SegCubic ax ay bx by) p
    at f p = uncurry f (pointFloats p)

-- | Turn a flat-filled axis-aligned rectangle (a chart bar) into a rect op,
-- which avoids the seams a polygon's triangles can show.
rectOp :: DrawOp -> DrawOp
rectOp op@(FillPolygon pts _ _ (P.Flat col)) = case primArrayToList pts of
  [x0, y0, x1, y1, x2, y2, x3, y3]
    | level x0 y0 x1 y1 x2 y2 x3 y3 || level y0 x0 y1 x1 y2 x2 y3 x3 ->
        FillRect (Rect (min x0 x2) (min y0 y2) (abs (x2 - x0)) (abs (y2 - y0))) col
  _ -> op
  where
    -- Edges alternate between the two axes.
    level ax ay bx by cx cy dx dy = near ay by && near bx cx && near cy dy && near dx ax
    near a b = abs (a - b) <= 1e-3
rectOp op = op

pointFloats :: Real n => P2 n -> (Float, Float)
pointFloats p = let (x, y) = unp2 p in (toF x, toF y)

-- | A style's solid fill and line colours, if they are visible.
fillColour, lineColour :: (Typeable n, Floating n) => DiaCore.Style V2 n -> Maybe Color
fillColour sty = solidColour (sty ^? (_fillTexture . _AC))
lineColour sty = solidColour (sty ^? (_lineTexture . _AC))

solidColour :: Maybe (AlphaColour Double) -> Maybe Color
solidColour mc = do
  ac <- mc
  let
    a = alphaChannel ac
  if a <= 0
    then Nothing
    else
      let
        RGB r g b = toSRGB (ac `over` black)
        q x = round (clamp01 x * 255)
       in
        Just (colorRGBA (q r) (q g) (q b) (q a))

clamp01 :: Double -> Double
clamp01 x = max 0 (min 1 x)

toF :: Real n => n -> Float
toF = realToFrac

-- | Render into a logical-pixel viewport of the given width and height,
-- preserving aspect ratio and centring the result. Non-positive sizes return no ops.
diagramOps ::
  Double -> Double -> QDiagram NanoUIBackend V2 Double Any -> SmallArray DrawOp
diagramOps = renderFull False

-- | Text-only form of 'diagramOps', using the same viewport transform.
-- Used to measure labels before building all geometry.
diagramTextOps ::
  Double -> Double -> QDiagram NanoUIBackend V2 Double Any -> SmallArray DrawOp
-- Text uses the same backend and viewport as geometry. In particular, do not
-- coerce a QDiagram between backends: its primitives carry Renderable dictionaries.
diagramTextOps = renderFull True

renderFull ::
  Bool
  -> Double
  -> Double
  -> QDiagram NanoUIBackend V2 Double Any
  -> SmallArray DrawOp
renderFull textOnly w h d
  | w <= 0 || h <= 0 = emptySmallArray
  | otherwise =
      let
        V2 dw dh = size d
        (_, outH, dx, dy) = letterbox dw dh w h
        ops = renderDia NanoUIBackend (NanoUIOptions (mkHeight outH) textOnly) d
       in
        mapSmallArray' (shiftDrawOp (realToFrac dx) (realToFrac dy)) ops

-- | Scale a @dw@ by @dh@ diagram uniformly to fit a @w@ by @h@ box and centre
-- it: the drawn width and height, and the x and y offsets inside the box.
letterbox :: Double -> Double -> Double -> Double -> (Double, Double, Double, Double)
letterbox dw dh w h =
  let
    outH = if dw <= 1e-9 || dh <= 1e-9 then h else min h (w * dh / dw)
    outW = if dh <= 1e-9 then w else outH * dw / dh
   in
    (outW, outH, (w - outW) / 2, (h - outH) / 2)
