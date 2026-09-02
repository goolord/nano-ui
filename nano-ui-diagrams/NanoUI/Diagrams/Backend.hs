{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module NanoUI.Diagrams.Backend
  ( NanoUIBackend (..)
  , NanoUITextBackend (..)
  , B
  , diagramOps
  , diagramTextOps
  , uniformHeight
  ) where

import Control.Lens (Lens', (^.), (^?))
import Data.Colour (AlphaColour, alphaChannel, black, over)
import Data.Colour.SRGB (RGB (..), toSRGB)
import Data.DList (DList)
import Data.DList qualified as DL
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import Data.Vector qualified as V
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
import Data.Tree (Tree (Node))
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
import Unsafe.Coerce (unsafeCoerce)
import NanoUI
  ( Color
  , DrawOp (..)
  , shiftDrawOp
  , themeMuted
  , colorA
  , colorRGBA
  , defaultTheme
  )
import NanoUI.Diagrams.Tessellation
  ( fillPolygon
  , flattenCubic
  , strokePolyline
  )

data NanoUIBackend = NanoUIBackend
  deriving (Eq, Show)

data NanoUITextBackend = NanoUITextBackend
  deriving (Eq, Show)

type B = NanoUIBackend

type instance V NanoUIBackend = V2
type instance N NanoUIBackend = Double

type instance V NanoUITextBackend = V2
type instance N NanoUITextBackend = Double

fullSize :: Lens' (Options NanoUIBackend V2 n) (SizeSpec V2 n)
fullSize f (NanoUIOptions sz _) = fmap (\sz' -> NanoUIOptions sz' False) (f sz)

instance (Typeable n, RealFloat n) => Backend NanoUIBackend V2 n where
  newtype Render NanoUIBackend V2 n = NRenderFull (DiaCore.Style V2 n -> DList DrawOp)
  type Result NanoUIBackend V2 n = Vector DrawOp
  data Options NanoUIBackend V2 n = NanoUIOptions (SizeSpec V2 n) Bool
  renderRTree _ _ rt = V.fromList (DL.toList (walkFull mempty rt))
  adjustDia c opts d = (sz, t <> reflectionY, d')
    where
      (sz, t, d') = adjustDia2D fullSize c opts (d # reflectY)

instance (Typeable n, RealFloat n) => Backend NanoUITextBackend V2 n where
  newtype Render NanoUITextBackend V2 n = NRenderText (DiaCore.Style V2 n -> DList DrawOp)
  type Result NanoUITextBackend V2 n = Vector DrawOp
  data Options NanoUITextBackend V2 n = NanoUITextOptions (SizeSpec V2 n)
  renderRTree _ _ rt = V.fromList (DL.toList (walkText mempty rt))
  adjustDia c opts d = (sz, t <> reflectionY, d')
    where
      (sz, t, d') = adjustDia2D nanoTextSize c opts (d # reflectY)

nanoTextSize :: Lens' (Options NanoUITextBackend V2 n) (SizeSpec V2 n)
nanoTextSize f (NanoUITextOptions sz) = fmap NanoUITextOptions (f sz)

instance Semigroup (Render NanoUIBackend V2 n) where
  NRenderFull f <> NRenderFull g = NRenderFull (\sty -> f sty <> g sty)

instance Monoid (Render NanoUIBackend V2 n) where
  mempty = NRenderFull (const DL.empty)

instance Semigroup (Render NanoUITextBackend V2 n) where
  NRenderText f <> NRenderText g = NRenderText (\sty -> f sty <> g sty)

instance Monoid (Render NanoUITextBackend V2 n) where
  mempty = NRenderText (const DL.empty)

walkFull :: (Typeable n, RealFloat n) => DiaCore.Style V2 n -> RTree NanoUIBackend V2 n Annotation -> DList DrawOp
walkFull sty (Node n cs) =
  case n of
    RPrim prim ->
      let NRenderFull f = render NanoUIBackend prim
       in f sty
    RStyle s -> foldMap (walkFull (sty <> s)) cs
    _ -> foldMap (walkFull sty) cs

walkText :: (Typeable n, RealFloat n) => DiaCore.Style V2 n -> RTree NanoUITextBackend V2 n Annotation -> DList DrawOp
walkText sty (Node n cs) =
  case n of
    RPrim prim ->
      let NRenderText f = render NanoUITextBackend prim
       in f sty
    RStyle s -> foldMap (walkText (sty <> s)) cs
    _ -> foldMap (walkText sty) cs

instance (Typeable n, RealFloat n) => Renderable (Path V2 n) NanoUIBackend where
  render _ path = NRenderFull $ \sty -> DL.fromList (concatMap (trailOps sty) (pathTrails path))

instance RealFloat n => Renderable (Path V2 n) NanoUITextBackend where
  render _ _ = NRenderText (const DL.empty)

textOps :: (Typeable n, RealFloat n) => Text n -> DiaCore.Style V2 n -> DList DrawOp
textOps (Text tr align str) sty
  | null str = DL.empty
  | otherwise =
      let p = papply tr origin
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
       in DL.singleton (DrawText (toF x) (toF y) ax ay (T.pack str) col)

instance (Typeable n, RealFloat n) => Renderable (Text n) NanoUIBackend where
  render _ t = NRenderFull (textOps t)

instance (Typeable n, RealFloat n) => Renderable (Text n) NanoUITextBackend where
  render _ t = NRenderText (textOps t)

trailOps :: (Typeable n, RealFloat n) => DiaCore.Style V2 n -> Located (Trail V2 n) -> [DrawOp]
trailOps sty lt =
  let pts = [(toF x, toF y) | (x, y) <- map unp2 (trailSamples lt)]
      lineW = toF (fromMaybe 1 (sty ^. _lineWidthU))
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
            | colorA c > 0 && lineW > 0 && length pts >= 2 -> strokePolyline c lineW closed pts
          _ -> []
   in fills ++ strokes

trailSamples :: RealFloat n => Located (Trail V2 n) -> [P2 n]
trailSamples lt =
  case map sampleSeg (fixTrail lt) of
    [] -> []
    (firstSeg : rest) ->
      let pts = firstSeg ++ concatMap (drop 1) rest
       in if isLoop (unLoc lt) && not (null pts)
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
  let a = alphaChannel ac
  if a <= 0
    then Nothing
    else
      let RGB r g b = toSRGB (ac `over` black)
          q x = round (clamp01 x * 255)
       in Just (colorRGBA (q r) (q g) (q b) (q a))

clamp01 :: Double -> Double
clamp01 x = max 0 (min 1 x)

toF :: Real n => n -> Float
toF = realToFrac

diagramOps :: Double -> Double -> QDiagram NanoUIBackend V2 Double Any -> Vector DrawOp
diagramOps w h d = renderFull w h d

diagramTextOps :: Double -> Double -> QDiagram NanoUIBackend V2 Double Any -> Vector DrawOp
diagramTextOps w h d = renderText w h (coerceDiagram d)

renderFull :: Double -> Double -> QDiagram NanoUIBackend V2 Double Any -> Vector DrawOp
renderFull w h d
  | w <= 0 || h <= 0 = V.empty
  | otherwise =
      let outH = uniformHeight w h d
          V2 dw dh = size d
          outW = if dh <= 1e-9 then w else outH * dw / dh
          ops = renderDia NanoUIBackend (NanoUIOptions (mkHeight outH) False) d
          dx = realToFrac ((w - outW) / 2)
          dy = realToFrac ((h - outH) / 2)
       in V.map (shiftDrawOp dx dy) ops

renderText :: Double -> Double -> QDiagram NanoUITextBackend V2 Double Any -> Vector DrawOp
renderText w h d
  | w <= 0 || h <= 0 = V.empty
  | otherwise =
      let outH = uniformHeight w h d
          V2 dw dh = size d
          outW = if dh <= 1e-9 then w else outH * dw / dh
          ops = renderDia NanoUITextBackend (NanoUITextOptions (mkHeight outH)) d
          dx = realToFrac ((w - outW) / 2)
          dy = realToFrac ((h - outH) / 2)
       in V.map (shiftDrawOp dx dy) ops

uniformHeight :: Double -> Double -> QDiagram b V2 Double Any -> Double
uniformHeight w h d =
  let V2 dw dh = size d
   in if dw <= 1e-9 || dh <= 1e-9
        then h
        else min h (w * dh / dw)

coerceDiagram ::
  QDiagram NanoUIBackend V2 Double Any -> QDiagram NanoUITextBackend V2 Double Any
coerceDiagram = unsafeCoerce
