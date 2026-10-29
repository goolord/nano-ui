module Main (main) where

import Control.Exception (IOException, try)
import Control.Monad (forM_, unless)
import Data.ByteString qualified as BS
import Data.Primitive.PrimArray (primArrayFromList)
import Data.Primitive.SmallArray (SmallArray, smallArrayFromList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import NanoUI
import NanoUI.Backend (monospaceMetrics)
import NanoUI.Draw.FrameSvg (FrameSvgConfig (..), defaultFrameSvgConfig, enableSvgExport, frameToSvg)
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
import NanoUI.Testing (newPixelContext)
import NanoUI.Testing.Assert (run2Frames, withInput)

check :: String -> Bool -> IO ()
check msg ok = unless ok (fail msg)

-- | Fail on the first needle the document lacks.
contains :: Text -> [Text] -> IO ()
contains doc = mapM_ (\n -> check ("missing " <> show n <> " in " <> show doc) (T.isInfixOf n doc))

main :: IO ()
main = do
  testStandaloneAndPrecision
  testPrimitiveShapes
  testPathsAndShaded
  testGradientsAndClips
  testTextAndImages
  testRoundTripRaster
  testFrameToSvg
  putStrLn "nano-ui-svg tests passed."

cdc :: CustomDrawContext
cdc = CustomDrawContext False False False False False defaultTheme (monospaceMetrics 16)

vp :: Rect
vp = Rect 0 0 100 80

-- | A bare @\<g\>@ fragment of the ops.
fragment :: SmallArray DrawOp -> Text
fragment = renderDrawOpsSvg defaultSvgConfig {svgStandalone = False} vp

red, green, blue, halfBlue :: Color
red = colorRGBA 255 0 0 255
green = colorRGBA 0 255 0 255
blue = colorRGBA 0 0 255 255
halfBlue = colorRGBA 0 0 255 128

-- | Standalone documents, precision, and ops that draw nothing.
testStandaloneAndPrecision :: IO ()
testStandaloneAndPrecision = do
  let
    ops = smallArrayFromList [FillRect (Rect 10.256 (-0.0001) 30.5 40) red]
    standalone = drawOpsToSvg vp ops
    atPrecision p = renderDrawOpsSvg defaultSvgConfig {svgStandalone = False, svgPrecision = p} vp ops
    rectAt x w = "<g><rect x=\"" <> x <> "\" y=\"0\" width=\"" <> w <> "\" height=\"40\" fill=\"#ff0000\"/></g>"
  check "standalone header" $
    T.isPrefixOf "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"100\" height=\"80\" viewBox=\"0 0 100 80\">" standalone
  check "standalone footer, no empty defs" (T.isSuffixOf "</svg>" standalone && not (T.isInfixOf "<defs>" standalone))
  -- Precision 2 by default, and no negative zero.
  contains standalone ["<rect x=\"10.26\" y=\"0\" width=\"30.5\" height=\"40\" fill=\"#ff0000\"/>"]
  check "precision 1" (atPrecision 1 == rectAt "10.3" "30.5")
  check "precision 0" (atPrecision 0 == rectAt "10" "31")
  let
    nan = 0 / 0
    inf = 1 / 0
    degenerate =
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
  check "degenerate ops emit nothing" (fragment (smallArrayFromList degenerate) == "<g></g>")

testPrimitiveShapes :: IO ()
testPrimitiveShapes =
  contains
    ( drawOpsToSvg vp . smallArrayFromList $
        [ FillRoundedRect (Rect 0 0 20 10) 4 halfBlue
        , FillRoundedRect (Rect 0 0 20 10) 0 red
        , FillCircle 50 40 12 red
        , FillTriangle 0 0 10 0 5 8 green
        , Stroke 1 2 3 4 1.5 red
        , StrokeLineAA 5 6 7 8 2 halfBlue
        , StrokeRoundedRect (Rect 10 20 40 30) 6 4 red
        , StrokeCircle 50 50 20 4 green
        ]
    )
    [ "<rect x=\"0\" y=\"0\" width=\"20\" height=\"10\" rx=\"4\" fill=\"#0000ff\" fill-opacity=\"0.502\"/>"
    , "<rect x=\"0\" y=\"0\" width=\"20\" height=\"10\" fill=\"#ff0000\"/>"
    , "<circle cx=\"50\" cy=\"40\" r=\"12\" fill=\"#ff0000\"/>"
    , "<polygon points=\"0,0 10,0 5,8\" fill=\"#00ff00\"/>"
    , "<line x1=\"1\" y1=\"2\" x2=\"3\" y2=\"4\" stroke=\"#ff0000\" stroke-width=\"1.5\"/>"
    , "<line x1=\"5\" y1=\"6\" x2=\"7\" y2=\"8\" stroke=\"#0000ff\" stroke-opacity=\"0.502\" stroke-width=\"2\"/>"
    , -- Strokes are inset inside their bounds.
      "<rect x=\"12\" y=\"22\" width=\"36\" height=\"26\" rx=\"4\" stroke=\"#ff0000\" stroke-width=\"4\" fill=\"none\"/>"
    , "<circle cx=\"50\" cy=\"50\" r=\"18\" stroke=\"#00ff00\" stroke-width=\"4\" fill=\"none\"/>"
    ]

testPathsAndShaded :: IO ()
testPathsAndShaded = do
  let
    flatSvg = drawOpsToSvg vp . runCanvasFor cdc $ do
      drawPathWith P.EvenOdd (P.rect (Rect 0 0 40 40) <> P.rect (Rect 10 10 20 20)) (P.Solid red)
      drawStrokePathWith
        (P.stroke 3) {P.strokeCap = P.RoundCap, P.strokeJoin = P.BevelJoin, P.strokeMiterLimit = 5}
        (P.polyline [V2 0 0, V2 10 5, V2 20 0])
        (P.Solid green)
      drawStrokePath (P.polygon [V2 0 0, V2 10 0, V2 10 10]) 2 red
  contains
    flatSvg
    [ "<path d=\"M0,0 L40,0 L40,40 L0,40 Z M10,30 L30,30 L30,10 L10,10 Z\" fill-rule=\"evenodd\" fill=\"#ff0000\"/>"
    , "<polyline points=\"0,0 10,5 20,0\" fill=\"none\" stroke=\"#00ff00\" stroke-width=\"3\" stroke-linecap=\"round\" stroke-linejoin=\"bevel\" stroke-miterlimit=\"5\"/>"
    , -- A closed polyline repeats its start point.
      "<polyline points=\"0,0 10,0 10,10 0,0\" fill=\"none\" stroke=\"#ff0000\" stroke-width=\"2\" stroke-linecap=\"butt\" stroke-linejoin=\"miter\" stroke-miterlimit=\"4\"/>"
    ]
  let
    gradPaint = P.Linear (V2 0 0) (V2 20 0) [(0, red), (1, green)]
    shadedSvg = drawOpsToSvg vp . runCanvasFor cdc $ do
      drawPathWith P.NonZero (P.rect (Rect 0 0 20 10)) gradPaint
      drawStrokePathWith (P.stroke 2) (P.polyline [V2 0 0, V2 10 0, V2 20 0]) gradPaint
  -- A linear gradient fill is one gradient path; a shaded polyline is segment lines.
  contains
    shadedSvg
    [ "<linearGradient id=\"grad_1\" gradientUnits=\"userSpaceOnUse\" x1=\"0\" y1=\"0\" x2=\"20\" y2=\"0\"><stop offset=\"0%\" stop-color=\"#ff0000\"/><stop offset=\"100%\" stop-color=\"#00ff00\"/></linearGradient>"
    , "fill=\"url(#grad_1)\""
    , "<line x1=\"0\" y1=\"0\" x2=\"10\" y2=\"0\""
    ]
  check "a gradient fill fell back to triangles" (not (T.isInfixOf "<polygon points=" shadedSvg))
  -- Four different corners follow no single linear gradient, so the
  -- triangles are emitted one by one.
  let
    cornersSvg =
      drawOpsToSvg vp $
        smallArrayFromList
          [ FillPolygon
              (primArrayFromList [0, 0, 20, 0, 20, 20, 0, 20])
              (primArrayFromList [0, 4])
              (primArrayFromList [0, 1, 2, 0, 2, 3])
              (Shaded (primArrayFromList (map colorToWord32 [red, green, blue, colorRGBA 255 255 0 255])))
          ]
  check "four-corner shaded polygon falls back to triangles" $
    T.isInfixOf "<polygon points=" cornersSvg && not (T.isInfixOf "linearGradient" cornersSvg)
  -- A shaded op whose vertex colours all match is one flat shape.
  let
    redW = colorToWord32 red
    uniform = StrokePolyline (primArrayFromList [0, 0, 10, 0, 10, 10]) 2 False ButtCap MiterJoin 4 (Shaded (primArrayFromList [redW, redW, redW]))
  check "uniform shaded polyline collapses to one polyline" $
    fragment (smallArrayFromList [uniform])
      == "<g><polyline points=\"0,0 10,0 10,10\" fill=\"none\" stroke=\"#ff0000\" stroke-width=\"2\" stroke-linecap=\"butt\" stroke-linejoin=\"miter\" stroke-miterlimit=\"4\"/></g>"

testGradientsAndClips :: IO ()
testGradientsAndClips = do
  contains
    ( drawOpsToSvg vp . runCanvasFor cdc $ do
        withClip (Rect 5 5 50 50) $ do
          drawLinearGradientH (Rect 5 5 20 10) red green
          drawLinearGradientV (Rect 5 20 20 10) red halfBlue
          drawQuadGradient (Rect 5 35 20 10) red green halfBlue colorWhite
        drawQuadGradient (Rect 60 5 10 10) red red red red
    )
    [ "<defs>"
    , "<clipPath id=\"clip_1\"><rect x=\"5\" y=\"5\" width=\"50\" height=\"50\"/></clipPath>"
    , "<linearGradient id=\"grad_1\" x1=\"0%\" y1=\"0%\" x2=\"100%\" y2=\"0%\"><stop offset=\"0%\" stop-color=\"#ff0000\"/><stop offset=\"100%\" stop-color=\"#00ff00\"/></linearGradient>"
    , "<linearGradient id=\"grad_2\" x1=\"0%\" y1=\"0%\" x2=\"0%\" y2=\"100%\"><stop offset=\"0%\" stop-color=\"#ff0000\"/><stop offset=\"100%\" stop-color=\"#0000ff\" stop-opacity=\"0.502\"/></linearGradient>"
    , -- A quad gradient is approximated diagonally.
      "<linearGradient id=\"grad_3\" x1=\"0%\" y1=\"0%\" x2=\"100%\" y2=\"100%\">"
    , "<g clip-path=\"url(#clip_1)\"><rect x=\"5\" y=\"5\" width=\"20\" height=\"10\" fill=\"url(#grad_1)\"/>"
    , -- A uniform quad gradient is a solid rect.
      "<rect x=\"60\" y=\"5\" width=\"10\" height=\"10\" fill=\"#ff0000\"/>"
    ]
  -- An unclosed PushClip and a stray PopClip stay well-formed.
  check "unbalanced clips" $
    fragment (smallArrayFromList [PopClip, PushClip (Rect 0 0 10 10), FillRect (Rect 1 1 2 2) red])
      == "<g><defs><clipPath id=\"clip_1\"><rect x=\"0\" y=\"0\" width=\"10\" height=\"10\"/></clipPath></defs><g clip-path=\"url(#clip_1)\"><rect x=\"1\" y=\"1\" width=\"2\" height=\"2\" fill=\"#ff0000\"/></g></g>"

testTextAndImages :: IO ()
testTextAndImages = do
  let
    styledFont = TextFont 18 FontMono WeightBold FontStyleItalic DecorationUnderline
    strikeFont =
      defaultTextFont
        { textFontSize = 12
        , textFontWeight = WeightSemiBold
        , textFontStyle = FontStyleOblique
        , textFontDecoration = DecorationUnderlineStrike
        }
  contains
    ( drawOpsToSvg vp . smallArrayFromList $
        [ DrawText 10 20 0.5 (-1) "a < b & \"c\" > 'd'" red
        , DrawTextStyled 5 6 styledFont "mono" green
        , DrawTextAligned 30 40 1 0.5 2 strikeFont "scaled" halfBlue
        ]
    )
    [ "<text x=\"10\" y=\"20\" text-anchor=\"middle\" dominant-baseline=\"alphabetic\" fill=\"#ff0000\">a &lt; b &amp; &quot;c&quot; &gt; &apos;d&apos;</text>"
    , "<text x=\"5\" y=\"6\" text-anchor=\"start\" dominant-baseline=\"hanging\" font-size=\"18\" font-family=\"monospace\" font-weight=\"bold\" font-style=\"italic\" text-decoration=\"underline\" fill=\"#00ff00\">mono</text>"
    , "<text x=\"30\" y=\"40\" text-anchor=\"end\" dominant-baseline=\"central\" font-size=\"24\" font-weight=\"600\" font-style=\"oblique\" text-decoration=\"underline line-through\" fill=\"#0000ff\" fill-opacity=\"0.502\">scaled</text>"
    ]
  let
    images =
      smallArrayFromList
        [ DrawImage (Rect 10 20 30 40) 0 1 0 0 1 1 red
        , DrawImage (Rect 0 0 20 20) (pi / 2) 2 0 0 1 1 halfBlue
        , DrawImage (Rect 0 0 10 10) 0 99 0 0 1 1 red
        ]
    uri = \case
      1 -> Just "https://example.com/a.png?x=1&y=2"
      2 -> Just "data:image/png;base64,AAAA"
      _ -> Nothing
    resolved = renderDrawOpsSvg defaultSvgConfig {svgImageUri = uri} vp images
  check "unresolved images omitted" (not (T.isInfixOf "<image" (drawOpsToSvg vp images)))
  check "an image with an unknown id was drawn" (T.count "<image" resolved == 2)
  contains
    resolved
    [ "<image x=\"10\" y=\"20\" width=\"30\" height=\"40\" preserveAspectRatio=\"none\" href=\"https://example.com/a.png?x=1&amp;y=2\"/>"
    , "<image x=\"0\" y=\"0\" width=\"20\" height=\"20\" preserveAspectRatio=\"none\" href=\"data:image/png;base64,AAAA\" opacity=\"0.502\" transform=\"rotate(90 10 10)\"/>"
    ]

testRoundTripRaster :: IO ()
testRoundTripRaster = do
  let
    svgText = drawOpsToSvg (Rect 0 0 20 20) . runCanvasFor cdc $ do
      drawPathWith P.EvenOdd (P.rect (Rect 0 0 14 14) <> P.rect (Rect 3 3 8 8)) (P.Solid red)
      drawCircle (V2 17 3) 2 blue
    pixelAt bytes x y = [BS.index bytes ((y * 20 + x) * 4 + k) | k <- [0 .. 3]]
  doc <- either (fail . ("parseSvg failed on drawOpsToSvg output: " <>)) pure (parseSvg (TE.encodeUtf8 svgText))
  check "roundtrip svgSize" (svgSize doc == (20, 20))
  let px = rasterizeSvg 20 20 colorWhite doc
  check "ring, hole and circle pixels" $
    map (uncurry (pixelAt px)) [(1, 1), (7, 7), (17, 3)] == [[255, 0, 0, 255], [0, 0, 0, 0], [0, 0, 255, 255]]

-- | A whole frame of ordinary widgets renders through 'frameToSvg', which
-- 'enableSvgExport' sets up: chrome as rects, a canvas as the shapes it
-- paints, labels as text, and clip ids under a per-document prefix.
testFrameToSvg :: IO ()
testFrameToSvg = do
  let
    inp = withInput 240 200
    circle = canvas (fixedWH 60 60) (\(Rect x y _ _) -> drawCircle (V2 (x + 30) (y + 30)) 12 red)
    ui :: Int -> NanoUI ()
    ui n = column $ do
      label "Frame label"
      labelWith fontBold "Bold label"
      _ <- buttonWith fontBold "Strong"
      _ <- circle
      -- Text longer than its field overhangs the field's clip.
      textInputConfigured
        defaultTextInputConfig {ticLayout = fixedW 80 defaultLayout}
        (constVar ("A value far too long for its field " <> T.pack (show n)))
    exportOf ctx cfg view = run2Frames ctx inp view >>= \(_, dd, _) -> frameToSvg cfg ctx inp dd
  bare <- newPixelContext
  (_, bareDraw, _) <- run2Frames bare inp (ui 0)
  refused <- try @IOException (frameToSvg defaultFrameSvgConfig bare inp bareDraw)
  check "frameToSvg refuses a context that records no shapes" (either (const True) (const False) refused)
  ctx <- newPixelContext
  enableSvgExport ctx
  svg1 <- exportOf ctx defaultFrameSvgConfig (ui 0)
  svg2 <- exportOf ctx defaultFrameSvgConfig (ui 10)
  named <- exportOf ctx defaultFrameSvgConfig {fsIdPrefix = "mine-"} (ui 0)
  let
    fill c = "fill=\"" <> colorToHex c <> "\""
    lastId svg = T.takeWhile (/= '"') (snd (T.breakOnEnd "clipPath id=\"" svg))
    textEl t = snd (T.breakOnEnd "<text" (fst (T.breakOn (">" <> t <> "</text>") svg1)))
  check "frame svg header" (T.isPrefixOf "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"240\" height=\"200\"" svg1)
  contains svg1 $
    [fill (pageBackground defaultTheme), "<rect ", "rx=\"12\" fill=\"#ff0000\""]
      <> [">" <> t <> "</text>" | t <- ["Frame label", "Bold label", "Strong"]]
  forM_ ["Bold label", "Strong"] $ \t ->
    check (T.unpack t <> " does not export bold") (T.isInfixOf "font-weight=\"bold\"" (textEl t))
  check "an overflowing field exports a clip id" (T.isInfixOf "clipPath id=" svg1)
  check "no placeholder is left in the document" (not (T.isInfixOf "<id>" svg1))
  check "documents that differ have different ids" (lastId svg1 /= lastId svg2)
  check "ids carry the document prefix" (T.isPrefixOf "n" (lastId svg1) && T.isInfixOf "-tclip_" (lastId svg1))
  check "a set prefix is kept" (T.isPrefixOf "mine-" (lastId named))
  -- A page scoped onto the panel surface exports that surface.
  panelSvg <- exportOf ctx defaultFrameSvgConfig (styled panelPage (ui 0))
  contains panelSvg [fill (styleBg (themePanel defaultTheme))]
  -- A drawing in a modal draws over the modal's panel, in its layer.
  modalSvg <- exportOf ctx defaultFrameSvgConfig (modal (constVar True) "Dialog" (() <$ circle))
  let (_, afterPanel) = T.breakOnEnd (fill (styleBg (themeFloatingWindow defaultTheme))) modalSvg
  check "modal drawing over its panel" (T.isInfixOf "rx=\"12\" fill=\"#ff0000\"" afterPanel)
  -- Widget images export under their id.
  (iid, _, _) <- run2Frames ctx inp $ do
    i <- freshImageId
    _ <- registerImageRgba i 2 2 (BS.replicate 16 255)
    pure i
  imageSvg <- exportOf ctx defaultFrameSvgConfig {fsImageUri = \_ -> Just "data:image/png;base64,AA=="} (image (fixedWH 20 20) iid)
  check "widget image under its id" (T.isInfixOf "<image " imageSvg)
