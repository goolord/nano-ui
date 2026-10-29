{-# OPTIONS_GHC -O0 #-}
module Cases.Svg (tests) where

import Spec
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import NanoUI.Internal.Context (Context (..), lookupImageSize)
import NanoUI.Svg (rasterizeSvg, rasterizeSvgIn, svgMonochrome)

tests :: [Spec]
tests =
  [ spec "svg-raster" runSvgRasterTest
  , spec "svg-raster-cache" runSvgRasterCacheTest
  , spec "svg-fit-content" runSvgFitContentTest
  ]

parsed :: BS.ByteString -> IO Svg
parsed = either fail pure . parseSvg

square :: IO Svg
square = parsed "<svg viewBox='0 0 24 24'><rect x='2' y='2' width='20' height='20'/></svg>"

-- | The ids of the cached rasters, in key order.
svgRasters :: Context -> IO [ImageId]
svgRasters ctx = map (\(iid, _, _) -> iid) . Map.elems <$> readIORef (ctxSvgRasters ctx)

-- | Strokes, even-odd holes, transforms and arcs rasterize where they
-- should, and a document that is not SVG does not parse.
runSvgRasterTest :: Context -> IORef Int -> IO ()
runSvgRasterTest _ failed = do
  let pixelAt w bytes x y = [BS.index bytes ((y * w + x) * 4 + k) | k <- [0 .. 3]]
      alphaAt w bytes x y = last (pixelAt w bytes x y)
  clock <-
    parsed
      "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"24\" height=\"24\" viewBox=\"0 0 24 24\" fill=\"none\" \
      \stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\">\
      \<!-- a clock --><circle cx=\"12\" cy=\"12\" r=\"10\"/><path d=\"M12 6v6l4 2\"/></svg>"
  let px = rasterizeSvg 48 48 colorWhite clock
  assertEq failed (True, (24, 24), 48 * 48 * 4) (svgMonochrome clock, svgSize clock, BS.length px)
  -- On the ring, on the hands, and in the empty face between them.
  assert failed (alphaAt 48 px 24 44 > 200 && alphaAt 48 px 24 18 > 200)
  assertEq failed [0, 0] [alphaAt 48 px 24 36, alphaAt 48 px 1 1]
  holed <-
    parsed
      "<svg viewBox='0 0 10 10'><path fill-rule='evenodd' fill='#ff0000' d='M0 0h10v10H0z M3 3h4v4H3z'/>\
      \<g transform='translate(5 0) scale(0.5)'><rect width='2' height='2' fill='rgb(0,0,255)'/></g></svg>"
  let hpx = rasterizeSvg 10 10 colorWhite holed
  assert failed (not (svgMonochrome holed))
  assertEq failed [[255, 0, 0, 255], [0, 0, 0, 0], [0, 0, 255, 255]] [pixelAt 10 hpx 1 8, pixelAt 10 hpx 5 5, pixelAt 10 hpx 5 0]
  arcs <- parsed "<svg viewBox='0 0 20 20'><path d='M2 10a8 8 0 1 1 16 0a8 8 0 1 1-16 0z'/></svg>"
  let apx = rasterizeSvg 20 20 colorWhite arcs
  assertEq failed [255, 0] [alphaAt 20 apx 10 10, alphaAt 20 apx 1 1]
  assert failed (either (const True) (const False) (parseSvg "<nope/>"))

-- | An icon's raster is tinted with the text colour and leased: kept while
-- drawn (or hidden for a frame), replaced once undrawn, remembered when too
-- big for the atlas, and made again once there is room.
runSvgRasterCacheTest :: Context -> IORef Int -> IO ()
runSvgRasterCacheTest ctx failed = do
  doc <- square
  theme <- getTheme ctx
  let inp = withInputOff 200 120
      red = colorRGBA 220 40 40 255
      icons = column (svgIcon 24 doc >> svgIconWith (fixedWH 16 16 . fontColor red) doc)
      colorsOf c ui = map snd <$> (drawQuads . snd =<< warmupDraw c inp ui)
  colors <- colorsOf ctx icons
  assert failed (styleFg (themePanel theme) `elem` colors && red `elem` colors)
  shown <- svgRasters ctx
  colorsOf ctx icons >>= assertEq failed (length colors) . length
  assertEq failed shown =<< svgRasters ctx
  lease <- newContext
  let registered iid = isJust <$> lookupImageSize lease iid
      showing ui = warmup2 lease inp (column ui)
  showing (svgIcon 16 doc)
  small <- svgRasters lease
  assertEq failed [True] =<< mapM registered small
  showing (pure ()) >> showing (svgIcon 16 doc)
  assertEq failed small =<< svgRasters lease
  showing (pure ()) >> showing (svgIcon 20 doc)
  assertEq failed 1 . length =<< svgRasters lease
  assertEq failed [False] =<< mapM registered small
  showing (svgIconWith (fixedWH 5000 10) doc)
  assert failed . elem (ImageId 0) =<< svgRasters lease
  showing (pure ()) >> showing (svgIcon 16 doc)
  assertEq failed [True] =<< mapM registered =<< svgRasters lease
  -- A shelf as tall as the atlas, then an image across the rest of its
  -- width, leave no room for another.
  full <- newContext
  let fill iid w h = registerImage full (ImageId iid) w h (BS.replicate (w * h * 4) 255)
      ui = column (svgIcon 16 doc)
  assert failed . and =<< sequence [fill 1 1 4094, fill 2 4092 17]
  _ <- warmup2 full inp ui
  assertEq failed [ImageId 0] =<< svgRasters full
  releaseImage full (ImageId 2)
  _ <- warmup2 full inp ui
  retried <- svgRasters full
  assert failed (ImageId 0 `notElem` retried)
  assertEq failed [True] =<< mapM (fmap isJust . lookupImageSize full) retried

-- | Fit and alignment place the document's own 2:1 shape, not a square raster
-- it was first letterboxed into.
runSvgFitContentTest :: Context -> IORef Int -> IO ()
runSvgFitContentTest ctx failed = do
  doc <- parsed "<svg width='20' height='10' viewBox='0 0 20 10'><rect width='20' height='10' fill='#ff0000'/></svg>"
  let alphas bytes = [BS.index bytes (i * 4 + 3) | i <- [0 .. 99]]
      opaque r = all (== 255) (alphas (rasterizeSvgIn 10 10 r colorWhite doc))
  -- Letterboxed by 'rasterizeSvg'; covering and stretched by 'rasterizeSvgIn'.
  assertEq failed 0 (BS.index (rasterizeSvg 10 10 colorWhite doc) 3)
  assert failed (opaque (Rect (-5) 0 20 10) && opaque (Rect 0 0 10 10))
  assert failed (all (== 0) (alphas (rasterizeSvgIn 10 10 (Rect 0 0 0 0) colorWhite doc)))
  let inp = withInputOff 200 200
      icon fit ax = svgIconConfigured defaultImageConfig {icLayout = fixedWH 20 20, icFit = fit, icAlignX = ax} doc
      -- After a frame without an icon, the next raster made takes the place
      -- of the last one. The key holds the content rect in eighths of a
      -- raster pixel.
      contentOf ui = do
        warmup ctx inp (column (pure ()))
        _ <- warmupDraw ctx inp ui
        let pixels v = fromIntegral v / 8 :: Float
        map (\(_, _, _, _, (x, y, w, h)) -> (pixels x, pixels y, pixels w, pixels h)) . Map.keys <$> readIORef (ctxSvgRasters ctx)
  forM_
    [ (icon FitCover AlignCenter, (-10, 0, 40, 20))
    , (icon FitCover AlignStart, (0, 0, 40, 20))
    , (icon FitFill AlignCenter, (0, 0, 20, 20))
    , (icon FitContain AlignCenter, (0, 5, 20, 10))
    , (svgIconWith (fixedWH 20 20) doc, (0, 5, 20, 10))
    ]
    $ \(ui, want) -> assertEq failed [want] =<< contentOf ui
