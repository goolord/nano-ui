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
import Data.List (intersperse)
import Data.Primitive.PrimArray (PrimArray, indexPrimArray, sizeofPrimArray)
import Data.Primitive.SmallArray (SmallArray, indexSmallArray, sizeofSmallArray)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Word (Word32, Word8)
import NanoUI
  ( Color (..)
  , DrawOp (..)
  , FontStyle (..)
  , FontVariant (..)
  , FontWeight (..)
  , Rect (..)
  , TextDecoration (..)
  , TextFont (..)
  , colorA
  , colorB
  , colorG
  , colorR
  , colorToHex
  , lerpColor
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
  }

-- | Standalone SVG with 2 decimal places and no image resolver.
defaultSvgConfig :: SvgConfig
defaultSvgConfig =
  SvgConfig
    { svgStandalone = True
    , svgPrecision = 2
    , svgImageUri = const Nothing
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
              !cidStr = TB.fromString (show cid)
              !(cx, cy, cw, ch)
                | all finite [x, y, w, h] && w > 0 && h > 0 = (x, y, w, h)
                | otherwise = (0, 0, 0, 0)
              clipDef =
                "<clipPath id=\"clip_"
                  <> cidStr
                  <> "\"><rect"
                  <> numAttr p "x" cx
                  <> numAttr p "y" cy
                  <> numAttr p "width" cw
                  <> numAttr p "height" ch
                  <> "/></clipPath>"
              clipOpen = "<g clip-path=\"url(#clip_" <> cidStr <> ")\">"
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
                  !gidStr = TB.fromString (show gid)
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
                  gradDef =
                    "<linearGradient id=\"grad_"
                      <> gidStr
                      <> "\""
                      <> coords
                      <> ">"
                      <> stops
                      <> "</linearGradient>"
                  gradRect =
                    "<rect"
                      <> numAttr p "x" x
                      <> numAttr p "y" y
                      <> numAttr p "width" w
                      <> numAttr p "height" h
                      <> attr "fill" ("url(#grad_" <> gidStr <> ")")
                      <> "/>"
                 in
                  walk (i + 1) clips nextClip gid (dAcc <> gradDef) (bAcc <> gradRect)
          op -> walk (i + 1) clips nextClip nextGrad dAcc (bAcc <> emitOp p cfg op)

emitOp :: Int -> SvgConfig -> DrawOp -> TB.Builder
emitOp p cfg = \case
  FillRect (Rect x y w h) col
    | w > 0 && h > 0 && colorA col > 0 && all finite [x, y, w, h] ->
        "<rect"
          <> numAttr p "x" x
          <> numAttr p "y" y
          <> numAttr p "width" w
          <> numAttr p "height" h
          <> fillAttrs p col
          <> "/>"
    | otherwise -> mempty
  FillRoundedRect (Rect x y w h) r col
    | w > 0 && h > 0 && colorA col > 0 && all finite [x, y, w, h, r] ->
        let
          !rad = min (max 0 r) (min (w * 0.5) (h * 0.5))
         in
          "<rect"
            <> numAttr p "x" x
            <> numAttr p "y" y
            <> numAttr p "width" w
            <> numAttr p "height" h
            <> (if rad > 0 then numAttr p "rx" rad else mempty)
            <> fillAttrs p col
            <> "/>"
    | otherwise -> mempty
  FillTriangle x0 y0 x1 y1 x2 y2 col
    | colorA col > 0
    , all finite [x0, y0, x1, y1, x2, y2]
    , abs ((x1 - x0) * (y2 - y0) - (x2 - x0) * (y1 - y0)) > 0 ->
        "<polygon"
          <> attr "points" (fmtPt p x0 y0 <> " " <> fmtPt p x1 y1 <> " " <> fmtPt p x2 y2)
          <> fillAttrs p col
          <> "/>"
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
  StrokeRoundedRect (Rect x y w h) r bw col
    | w > 0 && h > 0 && bw > 0 && colorA col > 0 && all finite [x, y, w, h, r, bw] ->
        let
          !ibw = min bw (min (w * 0.5) (h * 0.5))
          !rad = min (max 0 r) (min (w * 0.5) (h * 0.5))
          !rx = max 0 (rad - ibw * 0.5)
         in
          "<rect"
            <> numAttr p "x" (x + ibw * 0.5)
            <> numAttr p "y" (y + ibw * 0.5)
            <> numAttr p "width" (max 0 (w - ibw))
            <> numAttr p "height" (max 0 (h - ibw))
            <> (if rx > 0 then numAttr p "rx" rx else mempty)
            <> strokeAttrs p col ibw
            <> attr "fill" "none"
            <> "/>"
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
      "<line"
        <> numAttr p "x1" x0
        <> numAttr p "y1" y0
        <> numAttr p "x2" x1
        <> numAttr p "y2" y1
        <> strokeAttrs p col bw
        <> "/>"
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
      | colorA col > 0 && ringsValid && ptsFinite nOut ->
          let
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
            d = joinNonEmpty " " [ringPath r | r <- [0 .. nr - 1]]
           in
            "<path"
              <> attr "d" d
              <> attr "fill-rule" "evenodd"
              <> fillAttrs p col
              <> "/>"
      | otherwise -> mempty
    Nothing ->
      case shade of
        Shaded cs
          | nt >= 3 && sizeofPrimArray cs >= n && ptsFinite n && trisValid ->
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
                          "<polygon"
                            <> attr "points" (ptAt i0 <> " " <> ptAt i1 <> " " <> ptAt i2)
                            <> fillAttrs p col
                            <> "/>"
                )
                [0, 3 .. nt - 3]
        _ -> mempty
 where
  !n = sizeofPrimArray pts `div` 2
  !nr = sizeofPrimArray rings - 1
  !nt = sizeofPrimArray tris - sizeofPrimArray tris `mod` 3
  ringAt r = indexPrimArray rings r
  !nOut = if nr >= 1 then ringAt nr else 0
  ringsValid =
    n >= 3
      && nr >= 1
      && ringAt 0 == 0
      && nOut <= n
      && and [ringAt r + 3 <= ringAt (r + 1) | r <- [0 .. nr - 1]]
  trisValid =
    and
      [ t >= 0 && t < n
      | k <- [0 .. nt - 1]
      , let
          t = indexPrimArray tris k
      ]
  ptsFinite m = and [finite (indexPrimArray pts k) | k <- [0 .. 2 * m - 1]]
  ptAt i = fmtPt p (indexPrimArray pts (2 * i)) (indexPrimArray pts (2 * i + 1))

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
  | n < 2 || not (w > 0) || not (finite w && finite limit) || not ptsFinite =
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
  ptsFinite = and [finite (indexPrimArray pts k) | k <- [0 .. 2 * n - 1]]
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
        weightAttr = case textFontWeight font of
          WeightNormal -> mempty
          WeightBold -> attr "font-weight" "bold"
          WeightLight -> attr "font-weight" "300"
          WeightMedium -> attr "font-weight" "500"
          WeightSemiBold -> attr "font-weight" "600"
          WeightExtraBold -> attr "font-weight" "800"
          WeightBlack -> attr "font-weight" "900"
        styleAttr = case textFontStyle font of
          FontStyleNormal -> mempty
          FontStyleItalic -> attr "font-style" "italic"
          FontStyleOblique -> attr "font-style" "oblique"
        decoAttr = case textFontDecoration font of
          DecorationNone -> mempty
          DecorationUnderline -> attr "text-decoration" "underline"
          DecorationStrikethrough -> attr "text-decoration" "line-through"
          DecorationUnderlineStrike -> attr "text-decoration" "underline line-through"
       in
        sizeAttr <> familyAttr <> weightAttr <> styleAttr <> decoAttr

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

attr :: TB.Builder -> TB.Builder -> TB.Builder
attr k v = " " <> k <> "=\"" <> v <> "\""

numAttr :: Int -> TB.Builder -> Float -> TB.Builder
numAttr p k v = attr k (fmtNum p v)

fmtPt :: Int -> Float -> Float -> TB.Builder
fmtPt p x y = fmtNum p x <> "," <> fmtNum p y

fillAttrs :: Int -> Color -> TB.Builder
fillAttrs p col =
  attr "fill" (TB.fromText (colorToHex col))
    <> opacityAttr p "fill-opacity" (colorA col)

strokeAttrs :: Int -> Color -> Float -> TB.Builder
strokeAttrs p col bw =
  attr "stroke" (TB.fromText (colorToHex col))
    <> opacityAttr p "stroke-opacity" (colorA col)
    <> numAttr p "stroke-width" bw

stopElem :: Int -> TB.Builder -> Color -> TB.Builder
stopElem p offset col =
  "<stop"
    <> attr "offset" offset
    <> attr "stop-color" (TB.fromText (colorToHex col))
    <> opacityAttr p "stop-opacity" (colorA col)
    <> "/>"

-- | The named opacity attribute unless the colour is opaque. Uses at least
-- three digits: a step of 1/255 is just under 0.004.
opacityAttr :: Int -> TB.Builder -> Word8 -> TB.Builder
opacityAttr p name a
  | a < 255 = numAttr (max 3 p) name (fromIntegral a / 255)
  | otherwise = mempty

finite :: Float -> Bool
finite v = not (isNaN v || isInfinite v)

fmtNum :: Int -> Float -> TB.Builder
fmtNum p v
  | not (finite v) = "0"
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

escapeXml :: Text -> TB.Builder
escapeXml = T.foldl' (\acc c -> acc <> escapeChar c) mempty
 where
  escapeChar = \case
    '&' -> "&amp;"
    '<' -> "&lt;"
    '>' -> "&gt;"
    '"' -> "&quot;"
    '\'' -> "&apos;"
    c -> TB.singleton c
