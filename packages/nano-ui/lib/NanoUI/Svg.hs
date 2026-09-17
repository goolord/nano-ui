{-# LANGUAGE BangPatterns #-}

-- | SVG documents for icons: a parser for the static subset icon sets use and
-- an anti-aliased rasterizer.
--
-- Supported: @svg@ (with @viewBox@, @width@, @height@), @g@, @path@, @rect@,
-- @circle@, @ellipse@, @line@, @polyline@ and @polygon@; the presentation
-- attributes @fill@, @stroke@, @stroke-width@, @stroke-linecap@,
-- @stroke-linejoin@, @stroke-miterlimit@, @fill-rule@, @opacity@,
-- @fill-opacity@ and @stroke-opacity@, also inside @style@; @transform@; and
-- colours as names, @#rgb@, @#rrggbb@, @rgb()@ and @currentColor@. Gradients,
-- patterns, text, masks, clipping, filters and @use@ are ignored.
module NanoUI.Svg
  ( Svg
  , svgSize
  , svgKey
  , svgMonochrome
  , parseSvg
  , rasterizeSvg
  ) where

import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.Bits (xor)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.Char (isAlpha, isDigit, isSpace, toLower)
import Data.List (sortOn)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Data.Vector.Storable qualified as VS
import Data.Vector.Storable.Mutable qualified as VSM
import Data.Vector.Unboxed.Mutable qualified as MV
import Data.Word (Word8)
import NanoUI.Types (Color (..), colorA, colorB, colorG, colorR, colorRGBA)

-- | A parsed SVG document.
data Svg = Svg
  { svgViewBox :: !Box
  , svgSize :: !(Float, Float)
  -- ^ The document's own width and height, from its @width@ and @height@ or
  -- else its @viewBox@.
  , svgShapes :: ![Shape]
  , svgKey :: !Int
  -- ^ A hash of the source, for caching rasters.
  , svgMonochrome :: !Bool
  -- ^ Every paint is @currentColor@ or unspecified, so the drawing is one
  -- colour and can be tinted.
  }

-- | Documents are equal when their sources hash the same.
instance Eq Svg where
  a == b = svgKey a == svgKey b

instance Show Svg where
  show doc = "<svg " <> show (svgSize doc) <> ">"

data Box = Box !Float !Float !Float !Float

data Paint = PaintNone | PaintCurrent | PaintColor !Color
  deriving (Eq)

data FillRule = NonZero | EvenOdd
  deriving (Eq)

data LineCap = CapButt | CapRound | CapSquare
  deriving (Eq)

data LineJoin = JoinMiter | JoinRound | JoinBevel
  deriving (Eq)

-- | Presentation state inherited from ancestors.
data PaintStyle = PaintStyle
  { psFill :: !(Maybe Paint)
  , psStroke :: !(Maybe Paint)
  , psStrokeWidth :: !Float
  , psCap :: !LineCap
  , psJoin :: !LineJoin
  , psMiterLimit :: !Float
  , psRule :: !FillRule
  , psOpacity :: !Float
  , psFillOpacity :: !Float
  , psStrokeOpacity :: !Float
  }

-- | Segments, the transform to user space, and the paint.
data Shape = Shape ![Segment] !Matrix !PaintStyle

shapeStyle :: Shape -> PaintStyle
shapeStyle (Shape _ _ style) = style

data Segment
  = MoveTo !P
  | LineTo !P
  | CubicTo !P !P !P
  | QuadTo !P !P
  | ArcTo !Float !Float !Float !Bool !Bool !P
  | ClosePath

data P = P {-# UNPACK #-} !Float {-# UNPACK #-} !Float

-- | @a c e / b d f@, mapping @(x, y)@ to @(a x + c y + e, b x + d y + f)@.
data Matrix = Matrix !Float !Float !Float !Float !Float !Float

identity :: Matrix
identity = Matrix 1 0 0 1 0 0

mul :: Matrix -> Matrix -> Matrix
mul (Matrix a b c d e f) (Matrix a' b' c' d' e' f') =
  Matrix
    (a * a' + c * b')
    (b * a' + d * b')
    (a * c' + c * d')
    (b * c' + d * d')
    (a * e' + c * f' + e)
    (b * e' + d * f' + f)

apply :: Matrix -> P -> P
apply (Matrix a b c d e f) (P x y) = P (a * x + c * y + e) (b * x + d * y + f)

--------------------------------------------------------------------------------
-- XML
--------------------------------------------------------------------------------

data Element = Element !Text ![(Text, Text)] ![Element]

-- | The elements of a document, ignoring text, comments, processing
-- instructions and doctype declarations.
parseElements :: Text -> Either String [Element]
parseElements = fmap fst . content
  where
    content t0 =
      let t = T.dropWhile (/= '<') t0
       in case T.uncons t of
            Nothing -> Right ([], T.empty)
            Just _
              | "</" `T.isPrefixOf` t -> Right ([], t)
              | "<!--" `T.isPrefixOf` t -> content (snd (T.breakOn "-->" t) `dropPrefix` "-->")
              | "<![CDATA[" `T.isPrefixOf` t -> content (snd (T.breakOn "]]>" t) `dropPrefix` "]]>")
              | "<?" `T.isPrefixOf` t -> content (snd (T.breakOn "?>" t) `dropPrefix` "?>")
              | "<!" `T.isPrefixOf` t -> content (T.drop 1 (T.dropWhile (/= '>') t))
              | otherwise -> do
                  (el, rest) <- element (T.drop 1 t)
                  (more, rest') <- content rest
                  pure (el : more, rest')
    dropPrefix t p = fromMaybe T.empty (T.stripPrefix p t)
    element t = do
      let (name, afterName) = T.span (\c -> not (isSpace c) && c /= '>' && c /= '/') t
      when (T.null name) (Left "empty element name")
      (attrs, rest) <- attributes afterName []
      case T.uncons rest of
        Just ('/', r) -> Right (Element (localName name) attrs [], T.drop 1 (T.dropWhile (/= '>') r))
        Just ('>', r) -> do
          (children, afterChildren) <- content r
          let closing = T.drop 1 (T.dropWhile (/= '>') afterChildren)
          if "</" `T.isPrefixOf` afterChildren
            then Right (Element (localName name) attrs children, closing)
            else Left ("unclosed element " <> T.unpack name)
        _ -> Left ("malformed element " <> T.unpack name)
    attributes t acc =
      let s = T.stripStart t
       in case T.uncons s of
            Just (c, _) | c == '>' || c == '/' -> Right (reverse acc, s)
            Nothing -> Left "unterminated tag"
            _ -> do
              let (key, afterKey) = T.span (\c -> c /= '=' && not (isSpace c) && c /= '>' && c /= '/') s
                  afterEq = T.stripStart (T.drop 1 (T.stripStart afterKey))
              case T.uncons afterEq of
                Just (q, r) | q == '"' || q == '\'' ->
                  let (value, r') = T.break (== q) r
                   in attributes (T.drop 1 r') ((localName key, decodeEntities value) : acc)
                _ -> attributes afterKey acc
    localName n = T.takeWhileEnd (/= ':') n

decodeEntities :: Text -> Text
decodeEntities =
  T.replace "&amp;" "&" . T.replace "&lt;" "<" . T.replace "&gt;" ">" . T.replace "&quot;" "\"" . T.replace "&apos;" "'"

--------------------------------------------------------------------------------
-- Document
--------------------------------------------------------------------------------

-- | Parse an SVG document.
parseSvg :: Text -> Either String Svg
parseSvg src = do
  els <- parseElements src
  root <- case [e | e@(Element n _ _) <- els, n == "svg"] of
    r : _ -> Right r
    [] -> Left "no svg element"
  let Element _ attrs _ = root
      attr k = lookup k attrs
      box = case attr "viewBox" >>= numbers4 of
        Just (x, y, w, h) | w > 0 && h > 0 -> Box x y w h
        _ -> Box 0 0 (fromMaybe 24 (attr "width" >>= length1)) (fromMaybe 24 (attr "height" >>= length1))
      Box _ _ bw bh = box
      width = fromMaybe bw (attr "width" >>= length1)
      height = fromMaybe bh (attr "height" >>= length1)
      shapes = collect identity defaultStyle root
      -- Unspecified paints and currentColor follow the tint; an explicit
      -- colour anywhere makes the drawing multicoloured.
      monochromePaint p = case p of
        Just (PaintColor _) -> False
        _ -> True
  pure
    Svg
      { svgViewBox = box
      , svgSize = (width, height)
      , svgShapes = shapes
      , svgKey = T.foldl' (\h c -> (h * 16777619) `xor` fromEnum c) 2166136261 src
      , svgMonochrome = all (\sh -> monochromePaint (psFill (shapeStyle sh)) && monochromePaint (psStroke (shapeStyle sh))) shapes
      }
  where
    numbers4 t = case numberList t of
      [a, b, c, d] -> Just (a, b, c, d)
      _ -> Nothing

defaultStyle :: PaintStyle
defaultStyle =
  PaintStyle
    { psFill = Nothing
    , psStroke = Just PaintNone
    , psStrokeWidth = 1
    , psCap = CapButt
    , psJoin = JoinMiter
    , psMiterLimit = 4
    , psRule = NonZero
    , psOpacity = 1
    , psFillOpacity = 1
    , psStrokeOpacity = 1
    }

-- | Shapes in document order, each with its full transform and style.
collect :: Matrix -> PaintStyle -> Element -> [Shape]
collect m0 style0 (Element name attrs children) =
  let props = attrs ++ styleProperties (fromMaybe "" (lookup "style" attrs))
      m = maybe m0 (mul m0 . parseTransform) (lookup "transform" attrs)
      -- Opacity multiplies down the tree; the other properties replace.
      style = (applyProperties props style0) {psOpacity = psOpacity style0 * maybe 1 clampUnit (lookup "opacity" props >>= number1)}
      shape segs = [Shape segs m style]
      num k = fromMaybe 0 (lookup k attrs >>= length1)
   in case name of
        "svg" -> concatMap (collect m style) children
        "g" -> concatMap (collect m style) children
        "a" -> concatMap (collect m style) children
        "path" -> shape (parsePath (fromMaybe "" (lookup "d" attrs)))
        "rect" -> shape (rectSegments (num "x") (num "y") (num "width") (num "height") (lookup "rx" attrs >>= length1) (lookup "ry" attrs >>= length1))
        "circle" -> shape (ellipseSegments (num "cx") (num "cy") (num "r") (num "r"))
        "ellipse" -> shape (ellipseSegments (num "cx") (num "cy") (num "rx") (num "ry"))
        "line" -> shape [MoveTo (P (num "x1") (num "y1")), LineTo (P (num "x2") (num "y2"))]
        "polyline" -> shape (polySegments False (fromMaybe "" (lookup "points" attrs)))
        "polygon" -> shape (polySegments True (fromMaybe "" (lookup "points" attrs)))
        _ -> []

styleProperties :: Text -> [(Text, Text)]
styleProperties =
  mapMaybe
    ( \decl -> case T.breakOn ":" decl of
        (k, v) | not (T.null v) -> Just (T.strip k, T.strip (T.drop 1 v))
        _ -> Nothing
    )
    . T.splitOn ";"

applyProperties :: [(Text, Text)] -> PaintStyle -> PaintStyle
applyProperties props s0 = foldl step s0 props
  where
    step s (k, v) = case k of
      "fill" -> s {psFill = Just (parsePaint v)}
      "stroke" -> s {psStroke = Just (parsePaint v)}
      "stroke-width" -> maybe s (\w -> s {psStrokeWidth = max 0 w}) (length1 v)
      "stroke-linecap" -> case v of
        "round" -> s {psCap = CapRound}
        "square" -> s {psCap = CapSquare}
        _ -> s {psCap = CapButt}
      "stroke-linejoin" -> case v of
        "round" -> s {psJoin = JoinRound}
        "bevel" -> s {psJoin = JoinBevel}
        _ -> s {psJoin = JoinMiter}
      "stroke-miterlimit" -> maybe s (\l -> s {psMiterLimit = max 1 l}) (number1 v)
      "fill-rule" -> s {psRule = if v == "evenodd" then EvenOdd else NonZero}
      "fill-opacity" -> maybe s (\o -> s {psFillOpacity = clampUnit o}) (number1 v)
      "stroke-opacity" -> maybe s (\o -> s {psStrokeOpacity = clampUnit o}) (number1 v)
      _ -> s

clampUnit :: Float -> Float
clampUnit = max 0 . min 1

parsePaint :: Text -> Paint
parsePaint raw
  | v == "none" || v == "transparent" = PaintNone
  | v == "currentcolor" = PaintCurrent
  | Just hex <- T.stripPrefix "#" v = maybe PaintNone PaintColor (hexColor hex)
  | Just args <- T.stripPrefix "rgb(" v = case numberList (T.takeWhile (/= ')') args) of
      [r, g, b] -> PaintColor (colorRGBA (channel r) (channel g) (channel b) 255)
      _ -> PaintNone
  | otherwise = maybe (PaintColor (colorRGBA 0 0 0 255)) PaintColor (lookup v namedColors)
  where
    v = T.toLower (T.strip raw)
    channel x = fromIntegral (max 0 (min 255 (round x :: Int)))
    hexColor h = case T.unpack h of
      [r, g, b] -> rgb (hex2 r r) (hex2 g g) (hex2 b b)
      [r1, r2, g1, g2, b1, b2] -> rgb (hex2 r1 r2) (hex2 g1 g2) (hex2 b1 b2)
      _ -> Nothing
    rgb (Just r) (Just g) (Just b) = Just (colorRGBA r g b 255)
    rgb _ _ _ = Nothing
    hex2 a b = (\x y -> fromIntegral (x * 16 + y)) <$> hexDigit a <*> hexDigit b
    hexDigit c
      | isDigit c = Just (fromEnum c - fromEnum '0')
      | c >= 'a' && c <= 'f' = Just (fromEnum c - fromEnum 'a' + 10)
      | otherwise = Nothing

namedColors :: [(Text, Color)]
namedColors =
  [ ("black", colorRGBA 0 0 0 255)
  , ("white", colorRGBA 255 255 255 255)
  , ("red", colorRGBA 255 0 0 255)
  , ("green", colorRGBA 0 128 0 255)
  , ("lime", colorRGBA 0 255 0 255)
  , ("blue", colorRGBA 0 0 255 255)
  , ("yellow", colorRGBA 255 255 0 255)
  , ("orange", colorRGBA 255 165 0 255)
  , ("purple", colorRGBA 128 0 128 255)
  , ("gray", colorRGBA 128 128 128 255)
  , ("grey", colorRGBA 128 128 128 255)
  , ("silver", colorRGBA 192 192 192 255)
  , ("navy", colorRGBA 0 0 128 255)
  , ("teal", colorRGBA 0 128 128 255)
  , ("maroon", colorRGBA 128 0 0 255)
  , ("olive", colorRGBA 128 128 0 255)
  , ("aqua", colorRGBA 0 255 255 255)
  , ("cyan", colorRGBA 0 255 255 255)
  , ("fuchsia", colorRGBA 255 0 255 255)
  , ("magenta", colorRGBA 255 0 255 255)
  ]

--------------------------------------------------------------------------------
-- Numbers, transforms and path data
--------------------------------------------------------------------------------

-- | Numbers separated by spaces or commas, stopping at the first thing that
-- is not a number.
numberList :: Text -> [Float]
numberList t0 = go (skipSep t0)
  where
    go t = case readNumber t of
      Just (x, rest) -> x : go (skipSep rest)
      Nothing -> []

skipSep :: Text -> Text
skipSep = T.dropWhile (\c -> isSpace c || c == ',')

-- | A number at the start of the text, allowing @.5@, @-.5e-3@ and a
-- following number that starts with a sign or a second decimal point.
readNumber :: Text -> Maybe (Float, Text)
readNumber t =
  let (sign, t1) = case T.uncons t of
        Just (c, r) | c == '-' || c == '+' -> (T.singleton c, r)
        _ -> (T.empty, t)
      intPart = T.takeWhile isDigit t1
      afterInt = T.drop (T.length intPart) t1
      (fracPart, afterFrac) = case T.uncons afterInt of
        Just ('.', r) -> let ds = T.takeWhile isDigit r in (T.cons '.' ds, T.drop (T.length ds) r)
        _ -> (T.empty, afterInt)
      (expPart, rest) = case T.uncons afterFrac of
        Just (e, r)
          | e == 'e' || e == 'E' ->
              let (esign, r1) = case T.uncons r of
                    Just (c, r') | c == '-' || c == '+' -> (T.singleton c, r')
                    _ -> (T.empty, r)
                  eds = T.takeWhile isDigit r1
               in if T.null eds then (T.empty, afterFrac) else (T.concat ["e", esign, eds], T.drop (T.length eds) r1)
        _ -> (T.empty, afterFrac)
      mantissa = intPart <> fracPart
   in if T.null (T.filter isDigit mantissa)
        then Nothing
        else case TR.signed TR.rational (T.concat [sign, if T.null intPart then "0" else "", mantissa, expPart]) of
          Right (x, _) -> Just (realToFrac (x :: Double), rest)
          Left _ -> Nothing

number1 :: Text -> Maybe Float
number1 t = fst <$> readNumber (T.strip t)

-- | A length in user units: a number with an optional @px@. Percentages and
-- other units are not lengths here.
length1 :: Text -> Maybe Float
length1 t = case readNumber (T.strip t) of
  Just (x, rest) | T.null rest || rest == "px" -> Just x
  _ -> Nothing

parseTransform :: Text -> Matrix
parseTransform t0 = go identity (T.stripStart t0)
  where
    go m t
      | T.null t = m
      | otherwise =
          let (name, rest) = T.span isAlpha t
              (args, rest') = T.breakOn ")" (T.drop 1 (T.dropWhile (/= '(') rest))
              next = T.dropWhile (\c -> isSpace c || c == ',') (T.drop 1 rest')
              m' = case (name, numberList args) of
                ("matrix", [a, b, c, d, e, f]) -> Matrix a b c d e f
                ("translate", [x]) -> Matrix 1 0 0 1 x 0
                ("translate", [x, y]) -> Matrix 1 0 0 1 x y
                ("scale", [s]) -> Matrix s 0 0 s 0 0
                ("scale", [sx, sy]) -> Matrix sx 0 0 sy 0 0
                ("rotate", [a]) -> rotation a
                ("rotate", [a, cx, cy]) -> Matrix 1 0 0 1 cx cy `mul` rotation a `mul` Matrix 1 0 0 1 (-cx) (-cy)
                ("skewX", [a]) -> Matrix 1 0 (tan (a * pi / 180)) 1 0 0
                ("skewY", [a]) -> Matrix 1 (tan (a * pi / 180)) 0 1 0 0
                _ -> identity
           in if T.null name then m else go (m `mul` m') next
    rotation a =
      let r = a * pi / 180
       in Matrix (cos r) (sin r) (negate (sin r)) (cos r) 0 0

-- | Path data as absolute segments. Parsing stops at the first error, keeping
-- what came before, as renderers do.
parsePath :: Text -> [Segment]
parsePath = go 'M' (P 0 0) (P 0 0) Nothing . skipSep
  where
    -- cmd: the current (repeatable) command; cur: the current point; start:
    -- the subpath start; ctrl: the last control point, for S and T.
    go cmd cur start ctrl t = case T.uncons t of
      Nothing -> []
      Just (c, rest)
        | isAlpha c && c /= 'e' && c /= 'E' ->
            if toLower c == 'z'
              then ClosePath : go (if c == 'z' then 'm' else 'M') start start Nothing (skipSep rest)
              else run c cur start ctrl (skipSep rest)
        | otherwise -> run cmd cur start ctrl t
    run cmd cur@(P cx cy) start ctrl t =
      let rel = cmd >= 'a'
          pt (P x y) = if rel then P (cx + x) (cy + y) else P x y
          nums n = takeNumbers n t
       in case toLower cmd of
            'm' -> case nums 2 of
              Just ([x, y], r) ->
                let p = pt (P x y)
                 in MoveTo p : go (if rel then 'l' else 'L') p p Nothing (skipSep r)
              _ -> []
            'l' -> case nums 2 of
              Just ([x, y], r) -> let p = pt (P x y) in LineTo p : go cmd p start Nothing (skipSep r)
              _ -> []
            'h' -> case nums 1 of
              Just ([x], r) -> let p = P (if rel then cx + x else x) cy in LineTo p : go cmd p start Nothing (skipSep r)
              _ -> []
            'v' -> case nums 1 of
              Just ([y], r) -> let p = P cx (if rel then cy + y else y) in LineTo p : go cmd p start Nothing (skipSep r)
              _ -> []
            'c' -> case nums 6 of
              Just ([x1, y1, x2, y2, x, y], r) ->
                let c2 = pt (P x2 y2)
                    p = pt (P x y)
                 in CubicTo (pt (P x1 y1)) c2 p : go cmd p start (Just c2) (skipSep r)
              _ -> []
            's' -> case nums 4 of
              Just ([x2, y2, x, y], r) ->
                let c1 = maybe cur (reflect cur) ctrl
                    c2 = pt (P x2 y2)
                    p = pt (P x y)
                 in CubicTo c1 c2 p : go cmd p start (Just c2) (skipSep r)
              _ -> []
            'q' -> case nums 4 of
              Just ([x1, y1, x, y], r) ->
                let c1 = pt (P x1 y1)
                    p = pt (P x y)
                 in QuadTo c1 p : go cmd p start (Just c1) (skipSep r)
              _ -> []
            't' -> case nums 2 of
              Just ([x, y], r) ->
                let c1 = maybe cur (reflect cur) ctrl
                    p = pt (P x y)
                 in QuadTo c1 p : go cmd p start (Just c1) (skipSep r)
              _ -> []
            'a' -> case arcArgs t of
              Just ((rx, ry, rot, large, sweep, x, y), r) ->
                let p = pt (P x y)
                 in ArcTo rx ry rot large sweep p : go cmd p start Nothing (skipSep r)
              _ -> []
            _ -> []
    reflect (P cx cy) (P x y) = P (2 * cx - x) (2 * cy - y)
    takeNumbers :: Int -> Text -> Maybe ([Float], Text)
    takeNumbers 0 t = Just ([], t)
    takeNumbers n t = do
      (x, rest) <- readNumber t
      (xs, rest') <- takeNumbers (n - 1) (skipSep rest)
      pure (x : xs, rest')
    -- Arc flags may be written without separators: @a1 1 0 00.5.5@.
    arcArgs t = do
      (rx, r1) <- readNumber t
      (ry, r2) <- readNumber (skipSep r1)
      (rot, r3) <- readNumber (skipSep r2)
      (large, r4) <- flag (skipSep r3)
      (sweep, r5) <- flag (skipSep r4)
      (x, r6) <- readNumber (skipSep r5)
      (y, r7) <- readNumber (skipSep r6)
      pure ((rx, ry, rot, large, sweep, x, y), r7)
    flag t = case T.uncons t of
      Just ('0', r) -> Just (False, r)
      Just ('1', r) -> Just (True, r)
      _ -> Nothing

rectSegments :: Float -> Float -> Float -> Float -> Maybe Float -> Maybe Float -> [Segment]
rectSegments x y w h mrx mry
  | w <= 0 || h <= 0 = []
  | rx <= 0 || ry <= 0 = [MoveTo (P x y), LineTo (P (x + w) y), LineTo (P (x + w) (y + h)), LineTo (P x (y + h)), ClosePath]
  | otherwise =
      [ MoveTo (P (x + rx) y)
      , LineTo (P (x + w - rx) y)
      , ArcTo rx ry 0 False True (P (x + w) (y + ry))
      , LineTo (P (x + w) (y + h - ry))
      , ArcTo rx ry 0 False True (P (x + w - rx) (y + h))
      , LineTo (P (x + rx) (y + h))
      , ArcTo rx ry 0 False True (P x (y + h - ry))
      , LineTo (P x (y + ry))
      , ArcTo rx ry 0 False True (P (x + rx) y)
      , ClosePath
      ]
  where
    rx = min (w / 2) (fromMaybe (fromMaybe 0 mry) mrx)
    ry = min (h / 2) (fromMaybe (fromMaybe 0 mrx) mry)

ellipseSegments :: Float -> Float -> Float -> Float -> [Segment]
ellipseSegments cx cy rx ry
  | rx <= 0 || ry <= 0 = []
  | otherwise =
      [ MoveTo (P (cx + rx) cy)
      , ArcTo rx ry 0 False True (P (cx - rx) cy)
      , ArcTo rx ry 0 False True (P (cx + rx) cy)
      , ClosePath
      ]

polySegments :: Bool -> Text -> [Segment]
polySegments closed pts = case pairs (numberList pts) of
  [] -> []
  p : ps -> MoveTo p : map LineTo ps ++ [ClosePath | closed]
  where
    pairs (x : y : rest) = P x y : pairs rest
    pairs _ = []

--------------------------------------------------------------------------------
-- Flattening
--------------------------------------------------------------------------------

-- | A flattened subpath in device pixels and whether it is closed.
data Contour = Contour ![P] !Bool

-- | Flatten segments through a transform into contours. Curves are split
-- until they are within a quarter pixel of their chords.
flatten :: Matrix -> [Segment] -> [Contour]
flatten m = go [] False (P 0 0) (P 0 0)
  where
    -- acc: points of the open contour, newest first.
    go acc _ _ _ [] = finish acc False []
    go acc started cur start (s : rest) = case s of
      MoveTo p -> finish acc False (go [apply m p] True p p rest)
      LineTo p -> go (apply m p : begin acc started cur) True p start rest
      CubicTo c1 c2 p ->
        go (reverse (cubic (apply m cur) (apply m c1) (apply m c2) (apply m p)) ++ begin acc started cur) True p start rest
      QuadTo c1 p ->
        let P x0 y0 = cur
            P x1 y1 = c1
            P x2 y2 = p
            q1 = P (x0 + 2 / 3 * (x1 - x0)) (y0 + 2 / 3 * (y1 - y0))
            q2 = P (x2 + 2 / 3 * (x1 - x2)) (y2 + 2 / 3 * (y1 - y2))
         in go (reverse (cubic (apply m cur) (apply m q1) (apply m q2) (apply m p)) ++ begin acc started cur) True p start rest
      ArcTo rx ry rot large sweep p ->
        go (reverse (map (apply m) (arcPoints cur rx ry rot large sweep p)) ++ begin acc started cur) True p start rest
      ClosePath -> finish acc True (go [] False start start rest)
    begin acc started cur = if started || not (null acc) then acc else [apply m cur]
    finish acc closed more = case acc of
      _ : _ : _ -> Contour (reverse acc) closed : more
      [p] -> Contour [p] closed : more
      [] -> more

-- | Points after the start of a cubic, subdividing by flatness.
cubic :: P -> P -> P -> P -> [P]
cubic p0 p1 p2 p3 = go (0 :: Int) p0 p1 p2 p3 []
  where
    go depth a b c d acc
      | depth >= 12 || flat a b c d = d : acc
      | otherwise =
          let ab = mid a b
              bc = mid b c
              cd = mid c d
              abc = mid ab bc
              bcd = mid bc cd
              abcd = mid abc bcd
           in go (depth + 1) a ab abc abcd (go (depth + 1) abcd bcd cd d acc)
    mid (P x0 y0) (P x1 y1) = P ((x0 + x1) / 2) ((y0 + y1) / 2)
    flat (P x0 y0) (P x1 y1) (P x2 y2) (P x3 y3) =
      let ux = 3 * x1 - 2 * x0 - x3
          uy = 3 * y1 - 2 * y0 - y3
          vx = 3 * x2 - 2 * x3 - x0
          vy = 3 * y2 - 2 * y3 - y0
       in max (ux * ux) (vx * vx) + max (uy * uy) (vy * vy) <= 16 * 0.25 * 0.25

-- | Points after the start of an SVG arc, by the endpoint-to-centre
-- conversion in the SVG specification, in user space.
arcPoints :: P -> Float -> Float -> Float -> Bool -> Bool -> P -> [P]
arcPoints (P x1 y1) rx0 ry0 rotDeg large sweep (P x2 y2)
  | rx0 == 0 || ry0 == 0 || (x1 == x2 && y1 == y2) = [P x2 y2]
  | otherwise =
      let phi = rotDeg * pi / 180
          cosP = cos phi
          sinP = sin phi
          dx = (x1 - x2) / 2
          dy = (y1 - y2) / 2
          x1' = cosP * dx + sinP * dy
          y1' = negate sinP * dx + cosP * dy
          lambda = (x1' * x1') / (rx0 * rx0) + (y1' * y1') / (ry0 * ry0)
          scale = if lambda > 1 then sqrt lambda else 1
          rx = abs rx0 * scale
          ry = abs ry0 * scale
          num = rx * rx * ry * ry - rx * rx * y1' * y1' - ry * ry * x1' * x1'
          den = rx * rx * y1' * y1' + ry * ry * x1' * x1'
          coef = (if large == sweep then -1 else 1) * sqrt (max 0 (num / den))
          cx' = coef * rx * y1' / ry
          cy' = coef * negate (ry * x1' / rx)
          cx = cosP * cx' - sinP * cy' + (x1 + x2) / 2
          cy = sinP * cx' + cosP * cy' + (y1 + y2) / 2
          angle ux uy vx vy =
            let a = atan2 (ux * vy - uy * vx) (ux * vx + uy * vy)
             in a
          theta1 = angle 1 0 ((x1' - cx') / rx) ((y1' - cy') / ry)
          dtheta0 = angle ((x1' - cx') / rx) ((y1' - cy') / ry) ((negate x1' - cx') / rx) ((negate y1' - cy') / ry)
          dtheta
            | not sweep && dtheta0 > 0 = dtheta0 - 2 * pi
            | sweep && dtheta0 < 0 = dtheta0 + 2 * pi
            | otherwise = dtheta0
          steps = max 4 (ceiling (abs dtheta / (pi / 16)) :: Int)
          pointAt i =
            let t = theta1 + dtheta * fromIntegral i / fromIntegral steps
                ex = rx * cos t
                ey = ry * sin t
             in P (cosP * ex - sinP * ey + cx) (sinP * ex + cosP * ey + cy)
       in map pointAt [1 .. steps - 1] ++ [P x2 y2]

--------------------------------------------------------------------------------
-- Stroking
--------------------------------------------------------------------------------

-- | Polygons covering a stroke of width @w@ along the contours, each wound
-- counter-clockwise so a non-zero fill of all of them is their union.
strokePolygons :: Float -> LineCap -> LineJoin -> Float -> [Contour] -> [[P]]
strokePolygons w cap join miterLimit = concatMap contour
  where
    hw = w / 2
    contour (Contour raw closed) =
      let deduped = dedupe raw
          -- A closed contour that returns to its start ends on that point.
          pts = case deduped of
            first : _ : _ | closed && close first (last deduped) -> init deduped
            _ -> deduped
       in case pts of
            [] -> []
            [p] -> [disc p | cap == CapRound] ++ [square p | cap == CapSquare]
            first : second : _ ->
              let final = last pts
                  segs = zip pts (drop 1 pts) ++ [(final, first) | closed]
                  bodies = map segmentQuad segs
                  corners
                    | closed = zip3 (final : pts) pts (drop 1 pts ++ [first])
                    | otherwise = zip3 pts (drop 1 pts) (drop 2 pts)
                  joins = concatMap corner corners
                  caps
                    | closed = []
                    | otherwise = endCap second first ++ endCap (pts !! (length pts - 2)) final
               in map ccw (bodies ++ joins ++ caps)
    dedupe (a : b : rest)
      | close a b = dedupe (a : rest)
      | otherwise = a : dedupe (b : rest)
    dedupe xs = xs
    close (P x0 y0) (P x1 y1) = abs (x0 - x1) < 1e-4 && abs (y0 - y1) < 1e-4
    normal (P x0 y0) (P x1 y1) =
      let dx = x1 - x0
          dy = y1 - y0
          len = max 1e-6 (sqrt (dx * dx + dy * dy))
       in (negate dy / len * hw, dx / len * hw)
    segmentQuad (a@(P ax ay), b@(P bx by)) =
      let (nx, ny) = normal a b
       in [P (ax + nx) (ay + ny), P (bx + nx) (by + ny), P (bx - nx) (by - ny), P (ax - nx) (ay - ny)]
    corner (prev, v@(P vx vy), next) =
      let (n1x, n1y) = normal prev v
          (n2x, n2y) = normal v next
       in case join of
            JoinRound -> [disc v]
            JoinBevel -> [[v, P (vx + n1x) (vy + n1y), P (vx + n2x) (vy + n2y)], [v, P (vx - n1x) (vy - n1y), P (vx - n2x) (vy - n2y)]]
            JoinMiter ->
              let mx = n1x + n2x
                  my = n1y + n2y
                  mlen2 = mx * mx + my * my
                  -- The miter point sits along the bisector at hw / cos(half angle).
                  scale = if mlen2 < 1e-9 then 0 else 2 * hw * hw / mlen2
                  ratio = if mlen2 < 1e-9 then 1 / 0 else sqrt (scale * scale * mlen2) / hw
               in if ratio > miterLimit
                    then [[v, P (vx + n1x) (vy + n1y), P (vx + n2x) (vy + n2y)], [v, P (vx - n1x) (vy - n1y), P (vx - n2x) (vy - n2y)]]
                    else
                      [ [v, P (vx + n1x) (vy + n1y), P (vx + mx * scale) (vy + my * scale), P (vx + n2x) (vy + n2y)]
                      , [v, P (vx - n1x) (vy - n1y), P (vx - mx * scale) (vy - my * scale), P (vx - n2x) (vy - n2y)]
                      ]
    endCap inner@(P ix iy) end@(P ex ey) = case cap of
      CapButt -> []
      CapRound -> [disc end]
      CapSquare ->
        let dx = ex - ix
            dy = ey - iy
            len = max 1e-6 (sqrt (dx * dx + dy * dy))
            ux = dx / len * hw
            uy = dy / len * hw
            (nx, ny) = normal inner end
         in [[P (ex + nx) (ey + ny), P (ex + nx + ux) (ey + ny + uy), P (ex - nx + ux) (ey - ny + uy), P (ex - nx) (ey - ny)]]
    disc (P cx cy) =
      let n = max 8 (min 48 (ceiling (hw * 2.5) :: Int))
       in [P (cx + hw * cos t) (cy + hw * sin t) | i <- [0 .. n - 1], let t = 2 * pi * fromIntegral i / fromIntegral n]
    square (P cx cy) = [P (cx - hw) (cy - hw), P (cx + hw) (cy - hw), P (cx + hw) (cy + hw), P (cx - hw) (cy + hw)]
    ccw poly = if signedArea poly < 0 then reverse poly else poly

signedArea :: [P] -> Float
signedArea poly = case poly of
  [] -> 0
  p0 : _ -> go 0 (poly ++ [p0])
  where
    go !acc (P x0 y0 : rest@(P x1 y1 : _)) = go (acc + (x0 * y1 - x1 * y0)) rest
    go acc _ = acc / 2

--------------------------------------------------------------------------------
-- Rasterizing
--------------------------------------------------------------------------------

-- | Render the document into a @width@ by @height@ RGBA image (rows top to
-- bottom), scaled to fit and centred as SVG's default @xMidYMid meet@ does.
-- @current@ is what @currentColor@, and an unspecified fill, paint with.
rasterizeSvg :: Int -> Int -> Color -> Svg -> ByteString
rasterizeSvg width height current svg
  | width <= 0 || height <= 0 = BS.empty
  | otherwise = runST $ do
      -- Premultiplied RGBA in [0, 1].
      acc <- MV.replicate (width * height * 4) (0 :: Float)
      cov <- MV.replicate (width * height) (0 :: Float)
      let Box vx vy vw vh = svgViewBox svg
          s = min (fromIntegral width / vw) (fromIntegral height / vh)
          tx = (fromIntegral width - vw * s) / 2 - vx * s
          ty = (fromIntegral height - vh * s) / 2 - vy * s
          view = Matrix s 0 0 s tx ty
      forM_ (svgShapes svg) $ \(Shape segs m style) -> do
        let full = view `mul` m
            contours = flatten full segs
            Matrix a b c d _ _ = full
            scaleOf = sqrt (abs (a * d - b * c))
            opacity = psOpacity style
            paintColor p = case p of
              PaintNone -> Nothing
              PaintCurrent -> Just current
              PaintColor col -> Just col
            -- An unspecified fill paints black, or the current colour in a
            -- monochrome document, so an icon without paints tints.
            fill = fromMaybe (if svgMonochrome svg then PaintCurrent else PaintColor (colorRGBA 0 0 0 255)) (psFill style)
        forM_ (paintColor fill) $ \col -> do
          coverPolygons width height cov (psRule style) [pts | Contour pts _ <- contours, length pts >= 3]
          composite width height acc cov col (opacity * psFillOpacity style)
        forM_ (paintColor (fromMaybe PaintNone (psStroke style))) $ \col ->
          when (psStrokeWidth style > 0) $ do
            let wanted = psStrokeWidth style * scaleOf
                w = max 1 wanted
                polys = strokePolygons w (psCap style) (psJoin style) (psMiterLimit style) contours
            coverPolygons width height cov NonZero polys
            -- A hairline thinner than a pixel keeps its weight as opacity.
            composite width height acc cov col (opacity * psStrokeOpacity style * min 1 (wanted / w))
      out <- VSM.new (width * height * 4)
      forM_ [0 .. width * height - 1] $ \i -> do
        al <- MV.read acc (i * 4 + 3)
        let byte x = fromIntegral (max 0 (min 255 (round (x * 255) :: Int))) :: Word8
            unpremul k = do
              ch <- MV.read acc (i * 4 + k)
              VSM.write out (i * 4 + k) (if al <= 0 then 0 else byte (ch / al))
        unpremul 0
        unpremul 1
        unpremul 2
        VSM.write out (i * 4 + 3) (byte al)
      frozen <- VS.unsafeFreeze out
      let (fp, len) = VS.unsafeToForeignPtr0 frozen
      pure (BSI.BS fp len)

-- | Coverage of the polygons in @cov@ (cleared first): five sample rows a
-- pixel, each span's coverage split exactly across the pixels it crosses.
coverPolygons :: Int -> Int -> MV.MVector s Float -> FillRule -> [[P]] -> ST s ()
coverPolygons width height cov rule polys = do
  MV.set cov 0
  let edges =
        [ (y0, y1, x0, (x1 - x0) / (y1 - y0), dir)
        | poly <- polys
        , (P ax ay, P bx by) <- zip poly (drop 1 poly ++ take 1 poly)
        , ay /= by
        , let (x0, y0, x1, y1, dir) = if ay < by then (ax, ay, bx, by, 1 :: Int) else (bx, by, ax, ay, -1)
        ]
      samples = 5 :: Int
      weight = 1 / fromIntegral samples
      inside w = case rule of
        NonZero -> w /= 0
        EvenOdd -> odd w
      yLo = max 0 (floor (minimum (1e9 : [y0 | (y0, _, _, _, _) <- edges])))
      yHi = min height (ceiling (maximum (-1e9 : [y1 | (_, y1, _, _, _) <- edges])))
  forM_ [yLo .. yHi - 1] $ \row ->
    forM_ [0 .. samples - 1] $ \si -> do
      let sy = fromIntegral row + (fromIntegral si + 0.5) * weight
          crossings = sortOn fst [(x0 + (sy - y0) * slope, dir) | (y0, y1, x0, slope, dir) <- edges, sy >= y0, sy < y1]
          spans _ [] = []
          spans w ((xa, d) : rest@((xb, _) : _)) =
            let w' = w + d in if inside w' then (xa, xb) : spans w' rest else spans w' rest
          spans _ [_] = []
      forM_ (spans 0 crossings) $ \(xa0, xb0) -> do
        let xa = max 0 (min (fromIntegral width) xa0)
            xb = max 0 (min (fromIntegral width) xb0)
        when (xb > xa) $ do
          let ia = floor xa :: Int
              ib = min (width - 1) (floor xb)
              base = row * width
          if ia == ib
            then MV.modify cov (+ (xb - xa) * weight) (base + ia)
            else do
              MV.modify cov (+ (fromIntegral (ia + 1) - xa) * weight) (base + ia)
              forM_ [ia + 1 .. ib - 1] $ \i -> MV.modify cov (+ weight) (base + i)
              when (ib < width) $ MV.modify cov (+ (xb - fromIntegral ib) * weight) (base + ib)

-- | Draw @col@ at @alpha@ through the coverage over the accumulated image.
composite :: Int -> Int -> MV.MVector s Float -> MV.MVector s Float -> Color -> Float -> ST s ()
composite width height acc cov col alpha =
  forM_ [0 .. width * height - 1] $ \i -> do
    c <- MV.read cov i
    when (c > 0) $ do
      let sa = min 1 c * alpha * fromIntegral (colorA col) / 255
          blend k src = do
            dst <- MV.read acc (i * 4 + k)
            MV.write acc (i * 4 + k) (src * sa + dst * (1 - sa))
      blend 0 (fromIntegral (colorR col) / 255)
      blend 1 (fromIntegral (colorG col) / 255)
      blend 2 (fromIntegral (colorB col) / 255)
      dstA <- MV.read acc (i * 4 + 3)
      MV.write acc (i * 4 + 3) (sa + dstA * (1 - sa))
