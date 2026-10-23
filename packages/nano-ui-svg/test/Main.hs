module Main (main) where

import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.Primitive.PrimArray (primArrayFromList)
import Data.Primitive.SmallArray (smallArrayFromList)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import NanoUI
import NanoUI.Backend (monospaceMetrics)
import NanoUI.Draw.Svg
  ( SvgConfig (..)
  , defaultSvgConfig
  , drawOpsToSvg
  , renderDrawOpsSvg
  )
import NanoUI.Internal.Path (Shade (Shaded))
import NanoUI.Path (LineCap (ButtCap), LineJoin (MiterJoin))
import NanoUI.Path qualified as P
import NanoUI.Svg (rasterizeSvg)

check :: String -> Bool -> IO ()
check msg ok = unless ok (fail msg)

main :: IO ()
main = do
  testStandaloneAndPrecision
  testPrimitiveShapes
  testPathsAndShaded
  testGradientsAndClips
  testTextAndEscaping
  testImages
  testDegenerateOps
  testRoundTripRaster
  putStrLn "nano-ui-svg tests passed."

cdc :: CustomDrawContext
cdc =
  CustomDrawContext
    False
    False
    False
    False
    False
    defaultTheme
    (monospaceMetrics 16)

vp :: Rect
vp = Rect 0 0 100 80

red, green, halfBlue :: Color
red = colorRGBA 255 0 0 255
green = colorRGBA 0 255 0 255
halfBlue = colorRGBA 0 0 255 128

testStandaloneAndPrecision :: IO ()
testStandaloneAndPrecision = do
  let
    ops = smallArrayFromList [FillRect (Rect 10.256 (-0.0001) 30.5 40) red]
    standalone = drawOpsToSvg vp ops
    fragment =
      renderDrawOpsSvg
        defaultSvgConfig {svgStandalone = False, svgPrecision = 1}
        vp
        ops
    intPrec =
      renderDrawOpsSvg
        defaultSvgConfig {svgStandalone = False, svgPrecision = 0}
        vp
        ops
  check
    "standalone header"
    ( T.isPrefixOf
        "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"100\" height=\"80\" viewBox=\"0 0 100 80\">"
        standalone
    )
  check "standalone footer" (T.isSuffixOf "</svg>" standalone)
  check "no empty defs" (not (T.isInfixOf "<defs>" standalone))
  check
    "precision 2 and no negative zero"
    ( T.isInfixOf
        "<rect x=\"10.26\" y=\"0\" width=\"30.5\" height=\"40\" fill=\"#ff0000\"/>"
        standalone
    )
  check
    "fragment precision 1"
    ( fragment
        == "<g><rect x=\"10.3\" y=\"0\" width=\"30.5\" height=\"40\" fill=\"#ff0000\"/></g>"
    )
  check
    "fragment precision 0"
    ( intPrec
        == "<g><rect x=\"10\" y=\"0\" width=\"31\" height=\"40\" fill=\"#ff0000\"/></g>"
    )

testPrimitiveShapes :: IO ()
testPrimitiveShapes = do
  let
    ops =
      smallArrayFromList
        [ FillRoundedRect (Rect 0 0 20 10) 4 halfBlue
        , FillRoundedRect (Rect 0 0 20 10) 0 red
        , FillCircle 50 40 12 red
        , FillTriangle 0 0 10 0 5 8 green
        , Stroke 1 2 3 4 1.5 red
        , StrokeLineAA 5 6 7 8 2 halfBlue
        , StrokeRoundedRect (Rect 10 20 40 30) 6 4 red
        , StrokeCircle 50 50 20 4 green
        ]
    svg = drawOpsToSvg vp ops
  check
    "rounded rect with fill-opacity"
    ( T.isInfixOf
        "<rect x=\"0\" y=\"0\" width=\"20\" height=\"10\" rx=\"4\" fill=\"#0000ff\" fill-opacity=\"0.502\"/>"
        svg
    )
  check
    "rounded rect with zero radius omits rx"
    ( T.isInfixOf
        "<rect x=\"0\" y=\"0\" width=\"20\" height=\"10\" fill=\"#ff0000\"/>"
        svg
    )
  check
    "filled circle"
    (T.isInfixOf "<circle cx=\"50\" cy=\"40\" r=\"12\" fill=\"#ff0000\"/>" svg)
  check
    "filled triangle"
    (T.isInfixOf "<polygon points=\"0,0 10,0 5,8\" fill=\"#00ff00\"/>" svg)
  check
    "stroke line"
    ( T.isInfixOf
        "<line x1=\"1\" y1=\"2\" x2=\"3\" y2=\"4\" stroke=\"#ff0000\" stroke-width=\"1.5\"/>"
        svg
    )
  check
    "stroke line AA with stroke-opacity"
    ( T.isInfixOf
        "<line x1=\"5\" y1=\"6\" x2=\"7\" y2=\"8\" stroke=\"#0000ff\" stroke-opacity=\"0.502\" stroke-width=\"2\"/>"
        svg
    )
  check
    "inset stroke rounded rect"
    ( T.isInfixOf
        "<rect x=\"12\" y=\"22\" width=\"36\" height=\"26\" rx=\"4\" stroke=\"#ff0000\" stroke-width=\"4\" fill=\"none\"/>"
        svg
    )
  check
    "inset stroke circle"
    ( T.isInfixOf
        "<circle cx=\"50\" cy=\"50\" r=\"18\" stroke=\"#00ff00\" stroke-width=\"4\" fill=\"none\"/>"
        svg
    )

testPathsAndShaded :: IO ()
testPathsAndShaded = do
  let
    flatOps = runCanvasFor cdc $ do
      let
        outer = P.rect (Rect 0 0 40 40)
        hole = P.rect (Rect 10 10 20 20)
      drawPathWith P.EvenOdd (outer <> hole) (P.Solid red)
      drawStrokePathWith
        (P.stroke 3)
          { P.strokeCap = P.RoundCap
          , P.strokeJoin = P.BevelJoin
          , P.strokeMiterLimit = 5
          }
        (P.polyline [V2 0 0, V2 10 5, V2 20 0])
        (P.Solid green)
      drawStrokePath (P.polygon [V2 0 0, V2 10 0, V2 10 10]) 2 red
    flatSvg = drawOpsToSvg vp flatOps
  check
    "polygon with hole emits evenodd path"
    ( T.isInfixOf
        "<path d=\"M0,0 L40,0 L40,40 L0,40 Z M10,30 L30,30 L30,10 L10,10 Z\" fill-rule=\"evenodd\" fill=\"#ff0000\"/>"
        flatSvg
    )
  check
    "open polyline with cap, join, miterlimit"
    ( T.isInfixOf
        "<polyline points=\"0,0 10,5 20,0\" fill=\"none\" stroke=\"#00ff00\" stroke-width=\"3\" stroke-linecap=\"round\" stroke-linejoin=\"bevel\" stroke-miterlimit=\"5\"/>"
        flatSvg
    )
  check
    "closed polyline repeats start point"
    ( T.isInfixOf
        "<polyline points=\"0,0 10,0 10,10 0,0\" fill=\"none\" stroke=\"#ff0000\" stroke-width=\"2\" stroke-linecap=\"butt\" stroke-linejoin=\"miter\" stroke-miterlimit=\"4\"/>"
        flatSvg
    )

  let
    gradPaint = P.Linear (V2 0 0) (V2 20 0) [(0, red), (1, green)]
    shadedOps = runCanvasFor cdc $ do
      drawPathWith P.NonZero (P.rect (Rect 0 0 20 10)) gradPaint
      drawStrokePathWith
        (P.stroke 2)
        (P.polyline [V2 0 0, V2 10 0, V2 20 0])
        gradPaint
    shadedSvg = drawOpsToSvg vp shadedOps
  check
    "shaded polygon emits triangles"
    (T.isInfixOf "<polygon points=" shadedSvg)
  check
    "shaded polyline emits segment lines"
    (T.isInfixOf "<line x1=\"0\" y1=\"0\" x2=\"10\" y2=\"0\"" shadedSvg)

  -- A shaded op whose vertex colours all match is one flat shape.
  let
    redW = colorToWord32 red
    uniform =
      smallArrayFromList
        [ StrokePolyline
            (primArrayFromList [0, 0, 10, 0, 10, 10])
            2
            False
            ButtCap
            MiterJoin
            4
            (Shaded (primArrayFromList [redW, redW, redW]))
        ]
    uniformSvg = renderDrawOpsSvg defaultSvgConfig {svgStandalone = False} vp uniform
  check
    "uniform shaded polyline collapses to one polyline"
    ( uniformSvg
        == "<g><polyline points=\"0,0 10,0 10,10\" fill=\"none\" stroke=\"#ff0000\" stroke-width=\"2\" stroke-linecap=\"butt\" stroke-linejoin=\"miter\" stroke-miterlimit=\"4\"/></g>"
    )

testGradientsAndClips :: IO ()
testGradientsAndClips = do
  let
    ops = runCanvasFor cdc $ do
      withClip (Rect 5 5 50 50) $ do
        drawLinearGradientH (Rect 5 5 20 10) red green
        drawLinearGradientV (Rect 5 20 20 10) red halfBlue
        drawQuadGradient (Rect 5 35 20 10) red green halfBlue colorWhite
      drawQuadGradient (Rect 60 5 10 10) red red red red
    svg = drawOpsToSvg vp ops
  check "defs present" (T.isInfixOf "<defs>" svg)
  check
    "clipPath in defs"
    ( T.isInfixOf
        "<clipPath id=\"clip_1\"><rect x=\"5\" y=\"5\" width=\"50\" height=\"50\"/></clipPath>"
        svg
    )
  check
    "horizontal linearGradient"
    ( T.isInfixOf
        "<linearGradient id=\"grad_1\" x1=\"0%\" y1=\"0%\" x2=\"100%\" y2=\"0%\"><stop offset=\"0%\" stop-color=\"#ff0000\"/><stop offset=\"100%\" stop-color=\"#00ff00\"/></linearGradient>"
        svg
    )
  check
    "vertical linearGradient"
    ( T.isInfixOf
        "<linearGradient id=\"grad_2\" x1=\"0%\" y1=\"0%\" x2=\"0%\" y2=\"100%\"><stop offset=\"0%\" stop-color=\"#ff0000\"/><stop offset=\"100%\" stop-color=\"#0000ff\" stop-opacity=\"0.502\"/></linearGradient>"
        svg
    )
  check
    "diagonal quad gradient approximation"
    ( T.isInfixOf
        "<linearGradient id=\"grad_3\" x1=\"0%\" y1=\"0%\" x2=\"100%\" y2=\"100%\">"
        svg
    )
  check
    "clipped group body"
    ( T.isInfixOf
        "<g clip-path=\"url(#clip_1)\"><rect x=\"5\" y=\"5\" width=\"20\" height=\"10\" fill=\"url(#grad_1)\"/>"
        svg
    )
  check
    "uniform quad gradient collapses to solid rect"
    ( T.isInfixOf
        "<rect x=\"60\" y=\"5\" width=\"10\" height=\"10\" fill=\"#ff0000\"/>"
        svg
    )

  -- Unclosed PushClip and stray PopClip stay well-formed.
  let
    unbalanced =
      smallArrayFromList
        [ PopClip
        , PushClip (Rect 0 0 10 10)
        , FillRect (Rect 1 1 2 2) red
        ]
    unbalancedSvg = renderDrawOpsSvg defaultSvgConfig {svgStandalone = False} vp unbalanced
  check
    "unclosed PushClip is closed at end"
    ( unbalancedSvg
        == "<g><defs><clipPath id=\"clip_1\"><rect x=\"0\" y=\"0\" width=\"10\" height=\"10\"/></clipPath></defs><g clip-path=\"url(#clip_1)\"><rect x=\"1\" y=\"1\" width=\"2\" height=\"2\" fill=\"#ff0000\"/></g></g>"
    )

testTextAndEscaping :: IO ()
testTextAndEscaping = do
  let
    styledFont =
      TextFont
        { textFontSize = 18
        , textFontVariant = FontMono
        , textFontWeight = WeightBold
        , textFontStyle = FontStyleItalic
        , textFontDecoration = DecorationUnderline
        }
    strikeFont =
      defaultTextFont
        { textFontWeight = WeightSemiBold
        , textFontStyle = FontStyleOblique
        , textFontDecoration = DecorationUnderlineStrike
        }
    ops =
      smallArrayFromList
        [ DrawText 10 20 0.5 (-1) "a < b & \"c\" > 'd'" red
        , DrawTextStyled 5 6 styledFont "mono" green
        , DrawTextAligned 30 40 1 0.5 2 strikeFont {textFontSize = 12} "scaled" halfBlue
        ]
    svg = drawOpsToSvg vp ops
  check
    "escaped text and baseline alignment"
    ( T.isInfixOf
        "<text x=\"10\" y=\"20\" text-anchor=\"middle\" dominant-baseline=\"alphabetic\" fill=\"#ff0000\">a &lt; b &amp; &quot;c&quot; &gt; &apos;d&apos;</text>"
        svg
    )
  check
    "styled text attributes"
    ( T.isInfixOf
        "<text x=\"5\" y=\"6\" text-anchor=\"start\" dominant-baseline=\"hanging\" font-size=\"18\" font-family=\"monospace\" font-weight=\"bold\" font-style=\"italic\" text-decoration=\"underline\" fill=\"#00ff00\">mono</text>"
        svg
    )
  check
    "aligned scaled text attributes"
    ( T.isInfixOf
        "<text x=\"30\" y=\"40\" text-anchor=\"end\" dominant-baseline=\"central\" font-size=\"24\" font-weight=\"600\" font-style=\"oblique\" text-decoration=\"underline line-through\" fill=\"#0000ff\" fill-opacity=\"0.502\">scaled</text>"
        svg
    )

testImages :: IO ()
testImages = do
  let
    ops =
      smallArrayFromList
        [ DrawImage (Rect 10 20 30 40) 0 1 0 0 1 1 red
        , DrawImage (Rect 0 0 20 20) (pi / 2) 2 0 0 1 1 halfBlue
        , DrawImage (Rect 0 0 10 10) 0 99 0 0 1 1 red
        ]
    unresolved = drawOpsToSvg vp ops
    cfg =
      defaultSvgConfig
        { svgImageUri = \case
            1 -> Just "https://example.com/a.png?x=1&y=2"
            2 -> Just "data:image/png;base64,AAAA"
            _ -> Nothing
        }
    resolved = renderDrawOpsSvg cfg vp ops
  check "unresolved images omitted" (not (T.isInfixOf "<image" unresolved))
  check
    "unrotated image with escaped href"
    ( T.isInfixOf
        "<image x=\"10\" y=\"20\" width=\"30\" height=\"40\" preserveAspectRatio=\"none\" href=\"https://example.com/a.png?x=1&amp;y=2\"/>"
        resolved
    )
  check
    "rotated translucent image"
    ( T.isInfixOf
        "<image x=\"0\" y=\"0\" width=\"20\" height=\"20\" preserveAspectRatio=\"none\" href=\"data:image/png;base64,AAAA\" opacity=\"0.502\" transform=\"rotate(90 10 10)\"/>"
        resolved
    )

testDegenerateOps :: IO ()
testDegenerateOps = do
  let
    nan = 0 / 0
    inf = 1 / 0
    ops =
      smallArrayFromList
        [ FillRect (Rect 0 0 0 10) red
        , FillRect (Rect nan 0 10 10) red
        , FillRect (Rect 0 0 10 10) colorTransparent
        , FillCircle 10 10 (-2) red
        , FillTriangle 0 0 5 5 10 10 red
        , Stroke 1 1 1 1 2 red
        , Stroke 0 0 10 10 0 red
        , DrawText 0 0 0 0 "" red
        , DrawText inf 0 0 0 "x" red
        ]
    svg = renderDrawOpsSvg defaultSvgConfig {svgStandalone = False} vp ops
  check "degenerate ops emit nothing" (svg == "<g></g>")

testRoundTripRaster :: IO ()
testRoundTripRaster = do
  let
    boxBounds = Rect 0 0 20 20
    blue = colorRGBA 0 0 255 255
    ops = runCanvasFor cdc $ do
      drawPathWith
        P.EvenOdd
        (P.rect (Rect 0 0 14 14) <> P.rect (Rect 3 3 8 8))
        (P.Solid red)
      drawCircle (V2 17 3) 2 blue
    svgText = drawOpsToSvg boxBounds ops
    pixelAt w bytes x y = [BS.index bytes ((y * w + x) * 4 + k) | k <- [0 .. 3]]
  case parseSvg (TE.encodeUtf8 svgText) of
    Left err -> fail ("parseSvg failed on drawOpsToSvg output: " <> err)
    Right doc -> do
      check "roundtrip svgSize" (svgSize doc == (20, 20))
      let
        px = rasterizeSvg 20 20 colorWhite doc
      check "outer ring pixel" (pixelAt 20 px 1 1 == [255, 0, 0, 255])
      check "hole pixel is transparent" (pixelAt 20 px 7 7 == [0, 0, 0, 0])
      check "circle pixel" (pixelAt 20 px 17 3 == [0, 0, 255, 255])
