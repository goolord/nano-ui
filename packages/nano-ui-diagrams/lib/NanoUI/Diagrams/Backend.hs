{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module NanoUI.Diagrams.Backend
  ( NanoUIBackend (..)
  , B
  , diagramOps
  , diagramTextOps
  , letterbox
  )
where

import Control.Lens (Lens', (^.), (^?))
import Data.Colour (AlphaColour, alphaChannel, black, over)
import Data.Colour.SRGB (RGB (..), toSRGB)
import Data.DList (DList)
import Data.DList qualified as DL
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Tree (Tree (Node))
import Data.Typeable (Typeable)
import Data.Primitive.SmallArray (SmallArray, emptySmallArray, mapSmallArray', smallArrayFromList)
import Diagrams.Attributes (_lineWidthU)
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
  , p2
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
import Diagrams.TwoD.Size (mkHeight)
import Diagrams.TwoD.Text (Text (..), TextAlignment (..))
import NanoUI
  ( Color
  , DrawOp (..)
  , colorA
  , colorRGBA
  , defaultTheme
  , shiftDrawOp
  , themeMuted
  )
import NanoUI.Diagrams.Tessellation
  ( fillPolygon
  , flattenCubic
  , strokePolyline
  )

data NanoUIBackend = NanoUIBackend
  deriving (Eq, Show)

type B = NanoUIBackend

type instance V NanoUIBackend = V2

type instance N NanoUIBackend = Double

fullSize :: Lens' (Options NanoUIBackend V2 n) (SizeSpec V2 n)
fullSize f (NanoUIOptions sz textOnly) = fmap (\sz' -> NanoUIOptions sz' textOnly) (f sz)

instance (Typeable n, RealFloat n) => Backend NanoUIBackend V2 n where
  newtype Render NanoUIBackend V2 n
    = NRenderFull (Bool -> DiaCore.Style V2 n -> DList DrawOp)
  type Result NanoUIBackend V2 n = SmallArray DrawOp
  data Options NanoUIBackend V2 n = NanoUIOptions (SizeSpec V2 n) Bool
  renderRTree _ (NanoUIOptions _ textOnly) rt = smallArrayFromList (DL.toList (walkFull textOnly mempty rt))
  adjustDia c opts d = (sz, t <> reflectionY, d')
   where
    (sz, t, d') = adjustDia2D fullSize c opts (d # reflectY)

instance Semigroup (Render NanoUIBackend V2 n) where
  NRenderFull f <> NRenderFull g = NRenderFull (\textOnly sty -> f textOnly sty <> g textOnly sty)

instance Monoid (Render NanoUIBackend V2 n) where
  mempty = NRenderFull (\_ _ -> DL.empty)

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
      else foldMap (DL.fromList . trailOps sty) (pathTrails path)

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
        col =
          case solidColour (sty ^? (_fillTexture . _AC)) of
            Just c -> c
            Nothing ->
              fromMaybe
                (themeMuted defaultTheme)
                (solidColour (sty ^? (_lineTexture . _AC)))
       in
        DL.singleton (DrawText (toF x) (toF y) ax ay (T.pack str) col)

instance (Typeable n, RealFloat n) => Renderable (Text n) NanoUIBackend where
  render _ t = NRenderFull (const (textOps t))

trailOps ::
  (Typeable n, RealFloat n) =>
  DiaCore.Style V2 n -> Located (Trail V2 n) -> [DrawOp]
trailOps sty lt =
  let
    pts = [(toF x, toF y) | (x, y) <- map unp2 (trailSamples lt)]
    lineW0 = sty ^. _lineWidthU
    lineW =
      case fmap toF lineW0 of
        Nothing -> 1
        Just w
          | w <= 0 -> 0
          | w < 1 -> 1
          | otherwise -> w
    fillC = solidColour (sty ^? (_fillTexture . _AC))
    lineC = solidColour (sty ^? (_lineTexture . _AC))
    closed = isLoop (unLoc lt)
    fills =
      case fillC of
        Just c
          | colorA c > 0 && closed && length pts >= 3 -> fillPolygon c pts
        _ -> []
    strokes =
      case lineC of
        Just c
          | colorA c > 0 && lineW > 0 && length pts >= 2 ->
              strokePolyline c lineW closed pts
        _ -> []
   in
    fills ++ strokes

trailSamples :: RealFloat n => Located (Trail V2 n) -> [P2 n]
trailSamples lt =
  case map sampleSeg (fixTrail lt) of
    [] -> []
    (firstSeg : rest) ->
      let
        pts = firstSeg ++ concatMap (drop 1) rest
       in
        if isLoop (unLoc lt) && not (null pts)
          then pts ++ take 1 pts
          else pts

sampleSeg :: RealFloat n => FixedSegment V2 n -> [P2 n]
sampleSeg (FLinear p0 p1) = [p0, p1]
sampleSeg (FCubic p0 c1 c2 p1) =
  [ p2 (realToFrac x, realToFrac y)
  | (x, y) <- flattenCubic (f p0) (f c1) (f c2) (f p1)
  ]
 where
  f p = let (x, y) = unp2 p in (toF x, toF y)

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

diagramOps ::
  Double -> Double -> QDiagram NanoUIBackend V2 Double Any -> SmallArray DrawOp
diagramOps = renderFull False

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
