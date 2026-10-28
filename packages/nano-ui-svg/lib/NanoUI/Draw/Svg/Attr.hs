-- | SVG attribute, number and element builders shared by "NanoUI.Draw.Svg"
-- and "NanoUI.Draw.FrameSvg". Each takes the precision @p@ (decimal places)
-- first. The element builders emit unconditionally; callers guard degenerate
-- geometry first.
module NanoUI.Draw.Svg.Attr
  ( attr
  , numAttr
  , fmtNum
  , fmtPt
  , fillAttrs
  , strokeAttrs
  , opacityAttr
  , fontStyleAttrs
  , escapeXml
  , finite
  , rectEl
  , roundedRectEl
  , roundedStrokeEl
  , lineEl
  , polygonEl
  )
where

import Data.List (intersperse)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy.Builder qualified as TB
import Data.Word (Word8)
import NanoUI
  ( Color
  , FontStyle (..)
  , FontWeight (..)
  , Rect (..)
  , TextDecoration (..)
  , colorA
  , colorToHex
  )

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

-- | The named opacity attribute unless the colour is opaque. Uses at least
-- three digits: a step of 1/255 is just under 0.004.
opacityAttr :: Int -> TB.Builder -> Word8 -> TB.Builder
opacityAttr p name a
  | a < 255 = numAttr (max 3 p) name (fromIntegral a / 255)
  | otherwise = mempty

-- | The weight, slant and decoration attributes of a font, each omitted at
-- its default.
fontStyleAttrs :: FontWeight -> FontStyle -> TextDecoration -> TB.Builder
fontStyleAttrs weight slant deco = weightAttr <> styleAttr <> decoAttr
 where
  weightAttr = case weight of
    WeightNormal -> mempty
    WeightBold -> attr "font-weight" "bold"
    WeightLight -> attr "font-weight" "300"
    WeightMedium -> attr "font-weight" "500"
    WeightSemiBold -> attr "font-weight" "600"
    WeightExtraBold -> attr "font-weight" "800"
    WeightBlack -> attr "font-weight" "900"
  styleAttr = case slant of
    FontStyleNormal -> mempty
    FontStyleItalic -> attr "font-style" "italic"
    FontStyleOblique -> attr "font-style" "oblique"
  decoAttr = case deco of
    DecorationNone -> mempty
    DecorationUnderline -> attr "text-decoration" "underline"
    DecorationStrikethrough -> attr "text-decoration" "line-through"
    DecorationUnderlineStrike -> attr "text-decoration" "underline line-through"

finite :: Float -> Bool
finite v = not (isNaN v || isInfinite v)

-- | Decimal digits, rounded half up, without trailing zeros or exponents.
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

rectEl :: Int -> Rect -> Color -> TB.Builder
rectEl p r col = rectGeom p r <> fillAttrs p col <> "/>"

-- | A filled rectangle with corners of radius @r@, limited to half the
-- shorter side.
roundedRectEl :: Int -> Rect -> Float -> Color -> TB.Builder
roundedRectEl p rect@(Rect _ _ w h) r col =
  let
    !rad = min (max 0 r) (min (w * 0.5) (h * 0.5))
   in
    rectGeom p rect
      <> (if rad > 0 then numAttr p "rx" rad else mempty)
      <> fillAttrs p col
      <> "/>"

-- | A border of width @bw@ inset inside the rectangle, as paint draws it:
-- the stroke is centred on a rectangle half the border in from each side.
roundedStrokeEl :: Int -> Rect -> Float -> Float -> Color -> TB.Builder
roundedStrokeEl p (Rect x y w h) r bw col =
  let
    !ibw = min bw (min (w * 0.5) (h * 0.5))
    !rad = min (max 0 r) (min (w * 0.5) (h * 0.5))
    !rx = max 0 (rad - ibw * 0.5)
   in
    rectGeom p (Rect (x + ibw * 0.5) (y + ibw * 0.5) (max 0 (w - ibw)) (max 0 (h - ibw)))
      <> (if rx > 0 then numAttr p "rx" rx else mempty)
      <> strokeAttrs p col ibw
      <> attr "fill" "none"
      <> "/>"

lineEl :: Int -> Float -> Float -> Float -> Float -> Float -> Color -> TB.Builder
lineEl p x0 y0 x1 y1 bw col =
  "<line"
    <> numAttr p "x1" x0
    <> numAttr p "y1" y0
    <> numAttr p "x2" x1
    <> numAttr p "y2" y1
    <> strokeAttrs p col bw
    <> "/>"

polygonEl :: Int -> [(Float, Float)] -> Color -> TB.Builder
polygonEl p pts col =
  "<polygon"
    <> attr "points" (mconcat (intersperse " " [fmtPt p x y | (x, y) <- pts]))
    <> fillAttrs p col
    <> "/>"

-- | An open @<rect@ tag with its position and size.
rectGeom :: Int -> Rect -> TB.Builder
rectGeom p (Rect x y w h) =
  "<rect"
    <> numAttr p "x" x
    <> numAttr p "y" y
    <> numAttr p "width" w
    <> numAttr p "height" h
