module Cases.Svg (tests) where

import Data.ByteString qualified as BS
import Data.IORef (IORef)
import NanoUI
import NanoUI.Svg (rasterizeSvg, svgMonochrome)
import NanoUI.Testing
import NanoUI.Testing.Assert (assert, assertEq)
import NanoUI.Testing.Harness (drawQuads, warmupDraw, withInputOff)
import Spec (Spec, spec)

tests :: [Spec]
tests =
  [ spec "svg-raster" runSvgRasterTest
  , spec "svg-icon" runSvgIconTest
  ]

-- | Strokes, even-odd holes and transforms rasterize where they should.
runSvgRasterTest :: Context -> IORef Int -> IO ()
runSvgRasterTest _ failed = do
  let white = colorRGBA 255 255 255 255
      alphaAt w bytes x y = BS.index bytes ((y * w + x) * 4 + 3)
      pixelAt w bytes x y = [BS.index bytes ((y * w + x) * 4 + k) | k <- [0 .. 3]]
      clock =
        "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"24\" height=\"24\" viewBox=\"0 0 24 24\" fill=\"none\" \
        \stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\">\
        \<!-- a clock --><circle cx=\"12\" cy=\"12\" r=\"10\"/><path d=\"M12 6v6l4 2\"/></svg>"
  case parseSvg clock of
    Left err -> putStrLn err >> assert failed False
    Right doc -> do
      assert failed (svgMonochrome doc)
      assertEq failed (24, 24) (svgSize doc)
      let px = rasterizeSvg 48 48 white doc
      assertEq failed (48 * 48 * 4) (BS.length px)
      -- On the ring, on the hands, and in the empty face between them.
      assert failed (alphaAt 48 px 24 44 > 200)
      assert failed (alphaAt 48 px 24 18 > 200)
      assertEq failed 0 (alphaAt 48 px 24 36)
      assertEq failed 0 (alphaAt 48 px 1 1)
  let holed =
        "<svg viewBox='0 0 10 10'><path fill-rule='evenodd' fill='#ff0000' d='M0 0h10v10H0z M3 3h4v4H3z'/>\
        \<g transform='translate(5 0) scale(0.5)'><rect width='2' height='2' fill='rgb(0,0,255)'/></g></svg>"
  case parseSvg holed of
    Left err -> putStrLn err >> assert failed False
    Right doc -> do
      assert failed (not (svgMonochrome doc))
      let px = rasterizeSvg 10 10 white doc
      assertEq failed [255, 0, 0, 255] (pixelAt 10 px 1 8)
      assertEq failed 0 (alphaAt 10 px 5 5)
      assertEq failed [0, 0, 255, 255] (pixelAt 10 px 5 0)
  let arcs = "<svg viewBox='0 0 20 20'><path d='M2 10a8 8 0 1 1 16 0a8 8 0 1 1-16 0z'/></svg>"
  case parseSvg arcs of
    Left err -> putStrLn err >> assert failed False
    Right doc -> do
      let px = rasterizeSvg 20 20 white doc
      assertEq failed 255 (alphaAt 20 px 10 10)
      assertEq failed 0 (alphaAt 20 px 1 1)
  assert failed (either (const True) (const False) (parseSvg "<nope/>"))

-- | An icon draws its raster tinted with the text colour, and later frames
-- reuse the raster.
runSvgIconTest :: Context -> IORef Int -> IO ()
runSvgIconTest ctx failed = do
  theme <- getTheme ctx
  doc <- either fail pure (parseSvg "<svg viewBox='0 0 24 24'><rect x='2' y='2' width='20' height='20'/></svg>")
  let inp = withInputOff 200 120
      red = colorRGBA 220 40 40 255
      ui = column $ do
        svgIcon 24 doc
        svgIconWith (fixedWH 16 16 . fontColor red) doc
  (_, draw) <- warmupDraw ctx inp ui
  quads <- drawQuads draw
  let colors = map snd quads
  assert failed (styleFg (themePanel theme) `elem` colors)
  assert failed (red `elem` colors)
  (_, draw2) <- warmupDraw ctx inp ui
  quads2 <- drawQuads draw2
  assertEq failed (length quads) (length quads2)
