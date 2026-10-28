{-# LANGUAGE StrictData #-}

-- | Render 'DrawOp' arrays to SVG. Build the ops with a canvas
-- ("NanoUI.Widgets.Custom"), a 'NanoUI.drawing', or a diagrams-lib backend
-- ("NanoUI.Diagrams.Backend"), then export them as vector graphics:
--
-- > let ops = runCanvasFor cdc $ do
-- >       drawRoundedRect r 8 (colorRGBA 37 99 235 255)
-- >       drawText (V2 20 20) AlignCenter AlignMiddle "Ready" colorWhite
-- > TIO.writeFile "badge.svg" (drawOpsToSvg r ops)
--
-- 'renderDrawOpsSvg' takes a 'SvgConfig': @svgStandalone = False@ emits an
-- inner @\<g\>@ fragment for embedding in a larger document, 'svgPrecision'
-- bounds coordinate digits, and 'svgImageUri' resolves 'DrawOp' image ids to
-- @href@ URIs (images without a resolution are omitted).
--
-- Ops draw in array order, as paint does. Degenerate geometry (zero sizes,
-- non-finite coordinates, fully transparent colours) draws nothing.
module NanoUI.Draw.Svg
  ( SvgConfig (..)
  , defaultSvgConfig
  , renderDrawOpsSvg
  , drawOpsToSvg
  )
where

import Data.Bits (shiftL, (.|.))
import Data.List (intersperse, sortOn)
import Data.Primitive.PrimArray (PrimArray, indexPrimArray, sizeofPrimArray)
import Data.Primitive.SmallArray (SmallArray, indexSmallArray, sizeofSmallArray)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Word (Word32)
import NanoUI
  ( Color (..)
  , DrawOp (..)
  , FontVariant (..)
  , Rect (..)
  , TextFont (..)
  , colorA
  , colorB
  , colorG
  , colorR
  , colorToHex
  , lerpColor
  )
import NanoUI.Draw.Svg.Attr
  ( attr
  , escapeXml
  , fillAttrs
  , finite
  , fmtNum
  , fmtPt
  , fontStyleAttrs
  , lineEl
  , numAttr
  , opacityAttr
  , polygonEl
  , rectEl
  , roundedRectEl
  , roundedStrokeEl
  , strokeAttrs
  )
import NanoUI.Internal.Path
  ( LineCap (..)
  , LineJoin (..)
  , Shade (..)
  )

-- | Options for 'renderDrawOpsSvg'.
data SvgConfig = SvgConfig
  { svgStandalone :: !Bool
  -- ^ Wrap the output in an outer @\<svg\>@ element with @viewBox@ and
  -- namespace when 'True', or emit an inner @\<g\>@ fragment when 'False'.
  , svgPrecision :: !Int
  -- ^ Maximum decimal places for coordinates and lengths (clamped to @0..6@).
  -- Trailing fractional zeros are omitted.
  , svgImageUri :: !(Int -> Maybe Text)
  -- ^ Resolve an image or texture id to an @href@ or data URI for 'DrawImage'.
  -- When it returns 'Nothing', the image op is omitted.
  , svgIdPrefix :: !Text
  -- ^ Prepended to every @clip_N@ and @grad_N@ id, so several renders placed
  -- in one document, or one page, keep their ids apart.
  }

-- | Standalone SVG with 2 decimal places, no image resolver, and no id
-- prefix.
defaultSvgConfig :: SvgConfig
defaultSvgConfig =
  SvgConfig
    { svgStandalone = True
    , svgPrecision = 2
    , svgImageUri = const Nothing
    , svgIdPrefix = T.empty
    }

-- | Render draw ops over the given viewport rectangle as an SVG document
-- with 'defaultSvgConfig'.
drawOpsToSvg :: Rect -> SmallArray DrawOp -> Text
drawOpsToSvg = renderDrawOpsSvg defaultSvgConfig

-- | Render draw ops into an SVG document or @\<g\>@ fragment.
renderDrawOpsSvg :: SvgConfig -> Rect -> SmallArray DrawOp -> Text
renderDrawOpsSvg cfg (Rect vx0 vy0 vw0 vh0) ops =
  TL.toStrict . TB.toLazyText $
    if svgStandalone cfg
      then
        "<svg xmlns=\"http://www.w3.org/2000/svg\""
          <> numAttr p "width" vw
          <> numAttr p "height" vh
          <> attr
            "viewBox"
            (fmtNum p vx <> " " <> fmtNum p vy <> " " <> fmtNum p vw <> " " <> fmtNum p vh)
          <> ">"
          <> defsBlock
          <> body
          <> "</svg>"
      else "<g>" <> defsBlock <> body <> "</g>"
 where
  !p = max 0 (min 6 (svgPrecision cfg))
  !pfx = TB.fromText (svgIdPrefix cfg)
  !vx = if finite vx0 then vx0 else 0
  !vy = if finite vy0 then vy0 else 0
  !vw = if finite vw0 && vw0 > 0 then vw0 else 0
  !vh = if finite vh0 && vh0 > 0 then vh0 else 0
  !n = sizeofSmallArray ops
  !(defs, body) = walk 0 0 0 0 mempty mempty
  defsBlock
    | defsEmpty = mempty
    | otherwise = "<defs>" <> defs <> "</defs>"
  defsEmpty = TL.null (TB.toLazyText defs)
  closeClips k = mconcat (replicate k "</g>")

  walk ::
    Int -> Int -> Int -> Int -> TB.Builder -> TB.Builder -> (TB.Builder, TB.Builder)
  walk !i !clips !nextClip !nextGrad !dAcc !bAcc
    | i >= n = (dAcc, bAcc <> closeClips clips)
    | otherwise =
        case indexSmallArray ops i of
          PushClip (Rect x y w h) ->
            let
              !cid = nextClip + 1
              !cidStr = pfx <> "clip_" <> TB.fromString (show cid)
              !(cx, cy, cw, ch)
                | all finite [x, y, w, h] && w > 0 && h > 0 = (x, y, w, h)
                | otherwise = (0, 0, 0, 0)
              clipDef =
                "<clipPath id=\""
                  <> cidStr
                  <> "\"><rect"
                  <> numAttr p "x" cx
                  <> numAttr p "y" cy
                  <> numAttr p "width" cw
                  <> numAttr p "height" ch
                  <> "/></clipPath>"
              clipOpen = "<g clip-path=\"url(#" <> cidStr <> ")\">"
             in
              walk (i + 1) (clips + 1) cid nextGrad (dAcc <> clipDef) (bAcc <> clipOpen)
          PopClip
            | clips > 0 -> walk (i + 1) (clips - 1) nextClip nextGrad dAcc (bAcc <> "</g>")
            | otherwise -> walk (i + 1) 0 nextClip nextGrad dAcc bAcc
          FillQuadGradient r@(Rect x y w h) tl tr br bl
            | not (w > 0 && h > 0 && all finite [x, y, w, h]) ->
                walk (i + 1) clips nextClip nextGrad dAcc bAcc
            | all (\c -> colorA c == 0) [tl, tr, br, bl] ->
                walk (i + 1) clips nextClip nextGrad dAcc bAcc
            | tl == tr && tr == br && br == bl ->
                walk (i + 1) clips nextClip nextGrad dAcc (bAcc <> emitOp p cfg (FillRect r tl))
            | otherwise ->
                let
                  !gid = nextGrad + 1
                  !gidStr = gradId gid
                  !(coords, stops)
                    | tl == bl && tr == br =
                        ( attr "x1" "0%" <> attr "y1" "0%" <> attr "x2" "100%" <> attr "y2" "0%"
                        , stopElem p "0%" tl <> stopElem p "100%" tr
                        )
                    | tl == tr && bl == br =
                        ( attr "x1" "0%" <> attr "y1" "0%" <> attr "x2" "0%" <> attr "y2" "100%"
                        , stopElem p "0%" tl <> stopElem p "100%" bl
                        )
                    | otherwise =
                        ( attr "x1" "0%" <> attr "y1" "0%" <> attr "x2" "100%" <> attr "y2" "100%"
                        , stopElem p "0%" tl
                            <> stopElem p "50%" (lerpColor tr bl 0.5)
                            <> stopElem p "100%" br
                        )
                  gradRect =
                    "<rect"
                      <> numAttr p "x" x
                      <> numAttr p "y" y
                      <> numAttr p "width" w
                      <> numAttr p "height" h
                      <> gradFill gidStr
                      <> "/>"
                 in
                  walk (i + 1) clips nextClip gid (dAcc <> gradientDef gidStr coords stops) (bAcc <> gradRect)
          -- A canvas 'Linear' fill is one gradient cut into triangles at its
          -- stops; draw it as one, so the seams between triangles never show.
          FillPolygon pts rings tris (Shaded cs)
            | Just d <- ringsPathData p pts rings
            , Just ((x1, y1), (x2, y2), stops) <- linearShade pts tris cs ->
                let
                  !gid = nextGrad + 1
                  !gidStr = gradId gid
                  gradDef =
                    gradientDef
                      gidStr
                      ( attr "gradientUnits" "userSpaceOnUse"
                          <> numAttr p "x1" x1
                          <> numAttr p "y1" y1
                          <> numAttr p "x2" x2
                          <> numAttr p "y2" y2
                      )
                      (foldMap (\(t, col) -> stopElem p (fmtNum p (t * 100) <> "%") col) stops)
                  el =
                    "<path"
                      <> attr "d" d
                      <> attr "fill-rule" "evenodd"
                      <> gradFill gidStr
                      <> "/>"
                 in
                  walk (i + 1) clips nextClip gid (dAcc <> gradDef) (bAcc <> el)
          op -> walk (i + 1) clips nextClip nextGrad dAcc (bAcc <> emitOp p cfg op)

  gradId g = pfx <> "grad_" <> TB.fromString (show g)
  gradFill gidStr = attr "fill" ("url(#" <> gidStr <> ")")
  gradientDef gidStr attrs stops =
    "<linearGradient id=\"" <> gidStr <> "\"" <> attrs <> ">" <> stops <> "</linearGradient>"

emitOp :: Int -> SvgConfig -> DrawOp -> TB.Builder
emitOp p cfg = \case
  FillRect r@(Rect x y w h) col
    | w > 0 && h > 0 && colorA col > 0 && all finite [x, y, w, h] -> rectEl p r col
    | otherwise -> mempty
  FillRoundedRect rect@(Rect x y w h) r col
    | w > 0 && h > 0 && colorA col > 0 && all finite [x, y, w, h, r] -> roundedRectEl p rect r col
    | otherwise -> mempty
  FillTriangle x0 y0 x1 y1 x2 y2 col
    | colorA col > 0
    , all finite [x0, y0, x1, y1, x2, y2]
    , abs ((x1 - x0) * (y2 - y0) - (x2 - x0) * (y1 - y0)) > 0 ->
        polygonEl p [(x0, y0), (x1, y1), (x2, y2)] col
    | otherwise -> mempty
  FillCircle cx cy r col
    | r > 0 && colorA col > 0 && all finite [cx, cy, r] ->
        "<circle"
          <> numAttr p "cx" cx
          <> numAttr p "cy" cy
          <> numAttr p "r" r
          <> fillAttrs p col
          <> "/>"
    | otherwise -> mempty
  Stroke x0 y0 x1 y1 bw col -> emitLine p x0 y0 x1 y1 bw col
  StrokeLineAA x0 y0 x1 y1 bw col -> emitLine p x0 y0 x1 y1 bw col
  StrokeRoundedRect rect@(Rect x y w h) r bw col
    | w > 0 && h > 0 && bw > 0 && colorA col > 0 && all finite [x, y, w, h, r, bw] ->
        roundedStrokeEl p rect r bw col
    | otherwise -> mempty
  StrokeCircle cx cy r bw col
    -- As 'StrokeRoundedRect': the border never exceeds the radius, so the
    -- stroked circle is always at least half of it wide.
    | r > 0 && bw > 0 && colorA col > 0 && all finite [cx, cy, r, bw] ->
        let
          !ibw = min bw r
          !ir = r - ibw * 0.5
         in
          "<circle"
            <> numAttr p "cx" cx
            <> numAttr p "cy" cy
            <> numAttr p "r" ir
            <> strokeAttrs p col ibw
            <> attr "fill" "none"
            <> "/>"
    | otherwise -> mempty
  FillPolygon pts rings tris shade -> emitPolygon p pts rings tris shade
  StrokePolyline pts w closed cap join limit shade ->
    emitPolyline p pts w closed cap join limit shade
  DrawImage (Rect x y w h) angle tex _u0 _v0 _u1 _v1 col
    | w > 0 && h > 0 && colorA col > 0 && all finite [x, y, w, h] ->
        case svgImageUri cfg tex of
          Nothing -> mempty
          Just uri ->
            let
              rotAttr
                | finite angle && angle /= 0 =
                    let
                      !deg = angle * 180 / pi
                      !cx = x + w * 0.5
                      !cy = y + h * 0.5
                     in
                      attr
                        "transform"
                        ("rotate(" <> fmtNum p deg <> " " <> fmtNum p cx <> " " <> fmtNum p cy <> ")")
                | otherwise = mempty
             in
              "<image"
                <> numAttr p "x" x
                <> numAttr p "y" y
                <> numAttr p "width" w
                <> numAttr p "height" h
                <> attr "preserveAspectRatio" "none"
                <> attr "href" (escapeXml uri)
                <> opacityAttr p "opacity" (colorA col)
                <> rotAttr
                <> "/>"
    | otherwise -> mempty
  DrawText x y ax ay txt col -> emitText p x y ax ay 1 Nothing txt col
  DrawTextStyled x y font txt col -> emitText p x y 0 1 1 (Just font) txt col
  DrawTextAligned x y ax ay scale font txt col ->
    emitText p x y ax ay scale (Just font) txt col
  FillQuadGradient {} -> mempty
  PushClip {} -> mempty
  PopClip -> mempty

emitLine ::
  Int -> Float -> Float -> Float -> Float -> Float -> Color -> TB.Builder
emitLine p x0 y0 x1 y1 bw col
  | bw > 0
  , colorA col > 0
  , all finite [x0, y0, x1, y1, bw]
  , let dx = x1 - x0; dy = y1 - y0 in dx * dx + dy * dy >= 1.0e-6 =
      lineEl p x0 y0 x1 y1 bw col
  | otherwise = mempty

emitPolygon ::
  Int
  -> PrimArray Float
  -> PrimArray Int
  -> PrimArray Int
  -> Shade
  -> TB.Builder
emitPolygon p pts rings tris shade =
  case uniformShade n shade of
    Just col
      | colorA col > 0
      , Just d <- ringsPathData p pts rings ->
          "<path"
            <> attr "d" d
            <> attr "fill-rule" "evenodd"
            <> fillAttrs p col
            <> "/>"
      | otherwise -> mempty
    Nothing ->
      case shade of
        Shaded cs
          | shadedValid pts tris cs ->
              foldMap
                ( \k ->
                    let
                      !i0 = indexPrimArray tris k
                      !i1 = indexPrimArray tris (k + 1)
                      !i2 = indexPrimArray tris (k + 2)
                      !col =
                        averageColor3
                          (Color (indexPrimArray cs i0))
                          (Color (indexPrimArray cs i1))
                          (Color (indexPrimArray cs i2))
                     in
                      if colorA col == 0
                        then mempty
                        else
                          polygonEl p [pt i0, pt i1, pt i2] col
                )
                [0, 3 .. nt - 3]
        _ -> mempty
 where
  !n = sizeofPrimArray pts `div` 2
  !nt = sizeofPrimArray tris - sizeofPrimArray tris `mod` 3
  pt i = (indexPrimArray pts (2 * i), indexPrimArray pts (2 * i + 1))

-- | Whether a 'Shaded' polygon is well formed: a whole triangle, finite
-- points, a colour for each, and triangles that name only those points.
shadedValid :: PrimArray Float -> PrimArray Int -> PrimArray Word32 -> Bool
shadedValid pts tris cs =
  sizeofPrimArray tris >= 3 && sizeofPrimArray cs >= n && pointsFinite n pts && trianglesValid n tris
 where
  !n = sizeofPrimArray pts `div` 2

-- | Whether the first @m@ points are finite.
pointsFinite :: Int -> PrimArray Float -> Bool
pointsFinite m pts = and [finite (indexPrimArray pts k) | k <- [0 .. 2 * m - 1]]

-- | Whether every index of every whole triangle names one of @n@ points.
trianglesValid :: Int -> PrimArray Int -> Bool
trianglesValid n tris =
  and [t >= 0 && t < n | k <- [0 .. nt - 1], let t = indexPrimArray tris k]
 where
  !nt = sizeofPrimArray tris - sizeofPrimArray tris `mod` 3

-- | A 'FillPolygon''s rings as path data, outline first, or 'Nothing' when
-- the rings or their points are malformed.
ringsPathData :: Int -> PrimArray Float -> PrimArray Int -> Maybe TB.Builder
ringsPathData p pts rings
  | ringsValid && pointsFinite nOut pts =
      Just (joinNonEmpty " " [ringPath r | r <- [0 .. nr - 1]])
  | otherwise = Nothing
 where
  !n = sizeofPrimArray pts `div` 2
  !nr = sizeofPrimArray rings - 1
  ringAt r = indexPrimArray rings r
  !nOut = if nr >= 1 then ringAt nr else 0
  ringsValid =
    n >= 3
      && nr >= 1
      && ringAt 0 == 0
      && nOut <= n
      && and [ringAt r + 3 <= ringAt (r + 1) | r <- [0 .. nr - 1]]
  ptAt i = fmtPt p (indexPrimArray pts (2 * i)) (indexPrimArray pts (2 * i + 1))
  ringPath r =
    let
      !from = ringAt r
      !to = ringAt (r + 1)
     in
      if to - from < 3
        then mempty
        else
          "M"
            <> ptAt from
            <> foldMap (\i -> " L" <> ptAt i) [from + 1 .. to - 1]
            <> " Z"

-- | The single linear gradient a shaded polygon's colours follow, as its
-- start and end points and its stops (offset in @0..1@, colour). A canvas
-- 'NanoUI.Path.Linear' fill has one: its triangles are split at the
-- gradient's stops, so each is coloured along the gradient's axis alone.
-- A four-corner gradient has none, and gives 'Nothing'.
--
-- The axis is the dominant direction of the triangles' colour gradients,
-- weighted by squared area: 8-bit colours pin a large triangle's gradient
-- down, and a sliver's hardly at all. Along the axis, each triangle's
-- middle corner must then take the blend of the other two, within two
-- colour levels. The stops are the vertices' colours in order along the
-- axis, less those a straight blend of their neighbours already gives.
linearShade ::
  PrimArray Float -> PrimArray Int -> PrimArray Word32 -> Maybe ((Float, Float), (Float, Float), [(Float, Color)])
linearShade pts tris cs
  | n < 3 || not (shadedValid pts tris cs) = Nothing
  | sxx + syy <= 1.0e-12 || span' < 1.0e-3 = Nothing
  | not (all followsAxis triangles) || not (sameWhereLevel samples) = Nothing
  | otherwise =
      Just
        ( (ux * tMin, uy * tMin)
        , (ux * tMax, uy * tMax)
        , [((t - tMin) / span', col) | (t, col) <- simplify samples]
        )
 where
  !n = sizeofPrimArray pts `div` 2
  !nt = sizeofPrimArray tris - sizeofPrimArray tris `mod` 3
  px i = indexPrimArray pts (2 * i)
  py i = indexPrimArray pts (2 * i + 1)
  colourAt i = Color (indexPrimArray cs i)
  channels :: Color -> [Float]
  channels c = map fromIntegral [colorR c, colorG c, colorB c, colorA c]
  triangles =
    [ (i0, i1, i2)
    | k <- [0, 3 .. nt - 3]
    , let
        i0 = indexPrimArray tris k
        i1 = indexPrimArray tris (k + 1)
        i2 = indexPrimArray tris (k + 2)
    ]
  -- Per channel, the colour gradient across one triangle and its squared
  -- doubled area. Colour is affine across a triangle, so three corners fix
  -- it.
  triangleGradients (i0, i1, i2)
    | abs det <= 1.0e-6 = []
    | otherwise =
        [ ((dc1 * dy2 - dc2 * dy1) / det, (dx1 * dc2 - dx2 * dc1) / det, det * det)
        | (c0, c1, c2) <- zip3 (channels (colourAt i0)) (channels (colourAt i1)) (channels (colourAt i2))
        , let
            dc1 = c1 - c0
            dc2 = c2 - c0
        ]
   where
    dx1 = px i1 - px i0
    dy1 = py i1 - py i0
    dx2 = px i2 - px i0
    dy2 = py i2 - py i0
    det = dx1 * dy2 - dx2 * dy1
  gradients = concatMap triangleGradients triangles
  (sxx, sxy, syy) =
    foldl'
      (\(!a, !b, !c) (gx, gy, w) -> (a + w * gx * gx, b + w * gx * gy, c + w * gy * gy))
      (0, 0, 0)
      gradients
  angle = 0.5 * atan2 (2 * sxy) (sxx - syy)
  ux = cos angle
  uy = sin angle
  along i = ux * px i + uy * py i
  followsAxis (i0, i1, i2) = case sortOn fst [(along i, colourAt i) | i <- [i0, i1, i2]] of
    [(t0, c0), (t1, c1), (t2, c2)]
      | t2 - t0 < 1.0e-4 -> closeColour 2 c0 c1 && closeColour 2 c0 c2
      | otherwise -> closeColour 2 c1 (lerpColor c0 c2 ((t1 - t0) / (t2 - t0)))
    _ -> False
  samples = sortOn fst [(along i, colourAt i) | i <- [0 .. n - 1]]
  tMin = minimum (map fst samples)
  tMax = maximum (map fst samples)
  span' = tMax - tMin
  -- Vertices at the same place along the axis share a colour.
  sameWhereLevel ss =
    and [closeColour 3 a b | ((ta, a), (tb, b)) <- zip ss (drop 1 ss), tb - ta < 0.05]
  closeColour tol a b = and (zipWith (\x y -> abs (x - y) <= tol) (channels a) (channels b))
  -- Keep a sample only where a straight blend from the last kept one to a
  -- later one would miss a colour in between by over a level.
  simplify = \case
    s0 : s1 : rest -> s0 : thin s0 [] s1 rest
    ss -> ss
  thin _ _ s [] = [s]
  thin kept mids s (next : more)
    | fits kept (s : mids) next = thin kept (s : mids) next more
    | otherwise = s : thin s [] next more
  fits (t0, c0) mids (t1, c1) =
    t1 - t0 > 1.0e-4
      && all (\(t, c) -> closeColour 1 c (lerpColor c0 c1 ((t - t0) / (t1 - t0)))) mids

emitPolyline ::
  Int
  -> PrimArray Float
  -> Float
  -> Bool
  -> LineCap
  -> LineJoin
  -> Float
  -> Shade
  -> TB.Builder
emitPolyline p pts w closed cap join limit shade
  | n < 2 || not (w > 0) || not (finite w && finite limit) || not (pointsFinite n pts) =
      mempty
  | otherwise =
      case uniformShade n shade of
        Just col
          | colorA col > 0 ->
              let
                indices = [0 .. n - 1] ++ [0 | closed]
                pointsStr =
                  joinNonEmpty " " [ptAt i | i <- indices]
               in
                "<polyline"
                  <> attr "points" pointsStr
                  <> styleAttrs col
                  <> "/>"
          | otherwise -> mempty
        Nothing ->
          case shade of
            Shaded cs
              | sizeofPrimArray cs >= n ->
                  let
                    pairs = zip [0 .. n - 1] ([1 .. n - 1] ++ [0 | closed])
                   in
                    foldMap
                      ( \(i, j) ->
                          let
                            !col =
                              lerpColor
                                (Color (indexPrimArray cs i))
                                (Color (indexPrimArray cs j))
                                0.5
                           in
                            if colorA col == 0
                              then mempty
                              else
                                "<line"
                                  <> numAttr p "x1" (px i)
                                  <> numAttr p "y1" (py i)
                                  <> numAttr p "x2" (px j)
                                  <> numAttr p "y2" (py j)
                                  <> styleAttrs col
                                  <> "/>"
                      )
                      pairs
            _ -> mempty
 where
  !n = sizeofPrimArray pts `div` 2
  px i = indexPrimArray pts (2 * i)
  py i = indexPrimArray pts (2 * i + 1)
  ptAt i = fmtPt p (px i) (py i)
  capStr = case cap of
    ButtCap -> "butt"
    SquareCap -> "square"
    RoundCap -> "round"
  joinStr = case join of
    MiterJoin -> "miter"
    RoundJoin -> "round"
    BevelJoin -> "bevel"
  styleAttrs col =
    attr "fill" "none"
      <> strokeAttrs p col w
      <> attr "stroke-linecap" capStr
      <> attr "stroke-linejoin" joinStr
      <> numAttr p "stroke-miterlimit" limit

emitText ::
  Int
  -> Float
  -> Float
  -> Float
  -> Float
  -> Float
  -> Maybe TextFont
  -> Text
  -> Color
  -> TB.Builder
emitText p x y ax ay scale mFont txt col
  | T.null txt
      || colorA col == 0
      || not (all finite [x, y, ax, ay, scale] && scale > 0) =
      mempty
  | otherwise =
      "<text"
        <> numAttr p "x" x
        <> numAttr p "y" y
        <> attr "text-anchor" anchor
        <> attr "dominant-baseline" baseline
        <> fontAttrs
        <> fillAttrs p col
        <> ">"
        <> escapeXml txt
        <> "</text>"
 where
  -- The nearest of the three anchors SVG names: fractions below 0.25 and
  -- above 0.75 snap to the outer ones, covering the values 'drawText' and
  -- its alignment helpers produce (0, 0.5, 1, and the baseline's -1).
  anchor
    | ax < 0.25 = "start"
    | ax <= 0.75 = "middle"
    | otherwise = "end"
  baseline
    | ay < 0.25 = "alphabetic"
    | ay <= 0.75 = "central"
    | otherwise = "hanging"
  fontAttrs = case mFont of
    Nothing -> mempty
    Just font ->
      let
        baseSz = textFontSize font
        sz
          | baseSz > 0 = baseSz * scale
          | scale /= 1 = 16 * scale
          | otherwise = 0
        sizeAttr = if sz > 0 then numAttr p "font-size" sz else mempty
        familyAttr = case textFontVariant font of
          FontMono -> attr "font-family" "monospace"
          _ -> mempty
       in
        sizeAttr
          <> familyAttr
          <> fontStyleAttrs (textFontWeight font) (textFontStyle font) (textFontDecoration font)

-- | Extract a single color when the shade is flat or all vertex colors match.
uniformShade :: Int -> Shade -> Maybe Color
uniformShade n = \case
  Flat c -> Just c
  Shaded cs
    | n > 0 && sizeofPrimArray cs >= n ->
        let
          !w0 = indexPrimArray cs 0
         in
          if all (\i -> indexPrimArray cs i == w0) [1 .. n - 1]
            then Just (Color w0)
            else Nothing
    | otherwise -> Nothing

averageColor3 :: Color -> Color -> Color -> Color
averageColor3 a b c =
  let
    avg f =
      (fromIntegral (f a) + fromIntegral (f b) + fromIntegral (f c) + 1) `div` 3 ::
        Word32
   in
    Color
      ( (avg colorR `shiftL` 24)
          .|. (avg colorG `shiftL` 16)
          .|. (avg colorB `shiftL` 8)
          .|. avg colorA
      )

joinNonEmpty :: TB.Builder -> [TB.Builder] -> TB.Builder
joinNonEmpty sep = mconcat . intersperse sep

stopElem :: Int -> TB.Builder -> Color -> TB.Builder
stopElem p offset col =
  "<stop"
    <> attr "offset" offset
    <> attr "stop-color" (TB.fromText (colorToHex col))
    <> opacityAttr p "stop-opacity" (colorA col)
    <> "/>"
