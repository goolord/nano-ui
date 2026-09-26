module Cases.Images (tests) where

import Spec
import Data.ByteString qualified as BS
import Data.List (unzip4)
import Data.Maybe (fromMaybe)
import Data.Word (Word32)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (peekByteOff)
import NanoUI.Internal.Context (lookupImageSize, lookupImageUv)
import Data.Text qualified as T

tests :: [Spec]
tests =
  [ spec "image-fit-modes" runImageFitTest
  , spec "image-natural-size" runImageNaturalSizeTest
  , spec "image-opacity" runImageOpacityTest
  , spec "image-rotation" runImageRotationTest
  , spec "image-drawing-ops" runImageDrawingOpsTest
  , spec "image-own-aspect" runImageOwnAspectTest
  , spec "image-crop-scale" runImageCropScaleTest
  , spec "image-fit-rect" runImageFitRectTest
  , spec "image-look-damage" runImageLookDamageTest
  , spec "svg-icon-configured" runSvgIconConfiguredTest
  , spec "use-image-rgba" runUseImageRgbaTest
  ]

-- | One corner of a drawn quad: x, y, u, v and alpha.
type Corner = (Float, Float, Float, Float, Float)

-- | Run @ui@ in a column for two frames; return its result, the draw data, and each atlas
-- quad's corners with UVs normalized to image @iid@'s 0-1 range.
frame :: Context -> ImageId -> NanoUI a -> IO (a, DrawData, [[Corner]])
frame ctx iid ui = do
  (a, _, dd, _) <- run2Frames ctx (withInput 400 400) (column ui)
  (a0, b0, a1, b1) <- fromMaybe (0, 0, 1, 1) <$> lookupImageUv ctx iid
  fmap ((,,) a dd . concat) . withForeignPtr (drawVertices dd) $ \vp -> withForeignPtr (drawIndices dd) $ \ip ->
    forM [c | c <- drawCmdElems dd, cmdTextureId c == atlasTextureId] $ \cmd -> do
      let off = fromIntegral (cmdIndexOffset cmd)
      -- Quad indices are a b c a c d.
      forM [off, off + 6 .. off + fromIntegral (cmdIndexCount cmd) - 6] $ \q ->
        forM [q, q + 1, q + 2, q + 5] $ \ii -> do
          vi <- fromIntegral <$> (peekByteOff ip (ii * indexSize) :: IO Word32)
          let at o = peekByteOff vp (vi * vertexSize + o) :: IO Float
          (\x y u v alpha -> (x, y, (u - a0) / (a1 - a0), (v - b0) / (b1 - b0), alpha)) <$> at 0 <*> at 4 <*> at 24 <*> at 28 <*> at 20

-- | Each quad's position relative to @o@, its size, and its UV bounds.
bounds :: V2 -> [[Corner]] -> [[Float]]
bounds (V2 ox oy) quads =
  [ [x0 - ox, y0 - oy, maximum xs - x0, maximum ys - y0, minimum us, minimum vs, maximum us, maximum vs]
  | cs <- quads
  , let (xs, ys, us, vs) = unzip4 [(x, y, u, v) | (x, y, u, v, _) <- cs]
        (x0, y0) = (minimum xs, minimum ys)
  ]

topLeft :: Response -> V2
topLeft r = V2 (rectX (respRect r)) (rectY (respRect r))

near :: Float -> Float -> Bool
near a b = abs (a - b) < 1.0e-3

-- | Assert equal-length lists of nearly equal numbers; on failure show both.
assertNear :: IORef Int -> [Float] -> [Float] -> IO ()
assertNear failed want got = unless (length want == length got && and (zipWith near want got)) (assertEq failed want got)

-- | Register a white @w@ x @h@ image under id @n@.
whiteImage :: Context -> Int -> Int -> Int -> IO ImageId
whiteImage ctx n w h = ImageId n <$ registerImage ctx (ImageId n) w h (BS.replicate (w * h * 4) 255)

-- | Each fit places wide and tall images in wide and tall boxes, cropping via UVs, never past the box.
runImageFitTest :: Context -> IORef Int -> IO ()
runImageFitTest ctx failed = do
  wide <- whiteImage ctx 1 40 20
  tall <- whiteImage ctx 2 20 40
  big <- whiteImage ctx 3 200 100
  let (wb, tb, full, mid) = ((100, 80), (60, 120), [0, 0, 1, 1], (AlignCenter, AlignMiddle))
  forM_
    [ (wide, wb, FitContain, mid, [0, 15, 100, 50] ++ full), (wide, wb, FitCover, mid, [0, 0, 100, 80, 0.1875, 0, 0.8125, 1])
    , (wide, wb, FitFill, mid, [0, 0, 100, 80] ++ full), (wide, wb, FitNone, mid, [30, 30, 40, 20] ++ full)
    , (wide, wb, FitScaleDown, mid, [30, 30, 40, 20] ++ full), (big, wb, FitScaleDown, mid, [0, 15, 100, 50] ++ full)
    , (big, wb, FitNone, mid, [0, 0, 100, 80, 0.25, 0.1, 0.75, 0.9]), (wide, tb, FitContain, mid, [0, 45, 60, 30] ++ full)
    , (wide, tb, FitCover, mid, [0, 0, 60, 120, 0.375, 0, 0.625, 1]), (tall, wb, FitContain, mid, [30, 0, 40, 80] ++ full)
    , (tall, wb, FitCover, mid, [0, 0, 100, 80, 0, 0.3, 1, 0.7]), (tall, tb, FitContain, mid, [0, 0, 60, 120] ++ full)
      -- Alignment positions a contained image and picks which part a cover crop keeps.
    , (wide, tb, FitContain, (AlignStart, AlignTop), [0, 0, 60, 30] ++ full), (wide, tb, FitContain, (AlignEnd, AlignBottom), [0, 90, 60, 30] ++ full)
    , (wide, wb, FitCover, (AlignEnd, AlignMiddle), [0, 0, 100, 80, 0.375, 0, 1, 1]), (tall, wb, FitCover, (AlignStart, AlignTop), [0, 0, 100, 80, 0, 0, 1, 0.4])
    ]
    $ \(iid, (bw, bh), fit, (ax, ay), want) -> do
      let cfg = defaultImageConfig {icLayout = fixedWH bw bh, icFit = fit, icAlignX = ax, icAlignY = ay}
      (resp, _, quads) <- frame ctx iid (imageConfigured' cfg iid)
      assertNear failed want (concat (bounds (topLeft resp) quads))
  -- The default config stretches the image over its rect, like 'image'.
  let drawn ui = (\(_, _, quads) -> bounds (V2 0 0) quads) <$> frame ctx wide ui
  plain <- drawn (image (fixedWH 100 80) wide)
  drawn (imageConfigured defaultImageConfig {icLayout = fixedWH 100 80} wide) >>= assertEq failed plain

-- | An unsized axis takes the image's size, keeps its aspect beside a fixed axis, and obeys limits.
runImageNaturalSizeTest :: Context -> IORef Int -> IO ()
runImageNaturalSizeTest ctx failed = do
  wide <- whiteImage ctx 1 40 20
  -- An unregistered image gets the same size as with 'image'.
  forM_ [(id, wide, (40, 20)), (fixedW 80, wide, (80, 40)), (fixedH 10, wide, (20, 10)), (maxW 20, wide, (20, 10)), (fixedWH 30 30, wide, (30, 30)), (id, ImageId 99, (32, 32))] $
    \(f, iid, want) -> do
      Rect _ _ w h <- respRect <$> warmup2 ctx (withInput 400 400) (column (imageConfigured' defaultImageConfig {icLayout = f} iid))
      assertEq failed want (w, h)

-- | Opacity scales the tint's alpha, disabling fades further, and 0 draws nothing.
runImageOpacityTest :: Context -> IORef Int -> IO ()
runImageOpacityTest ctx failed = do
  iid <- whiteImage ctx 1 10 10
  let cfg = defaultImageConfig {icLayout = fixedWH 20 20}
      alphas c = (\(_, _, quads) -> [a | (_, _, _, _, a) <- concat quads]) <$> frame ctx iid c
  alphas (imageConfigured cfg iid) >>= assertEq failed (replicate 4 1)
  alphas (imageConfigured cfg {icOpacity = 0.5} iid) >>= assertNear failed (replicate 4 (128 / 255))
  alphas (imageConfigured cfg {icOpacity = 0.5, icLayout = fontColor (colorRGBA 255 0 0 200) . icLayout cfg} iid)
    >>= assertNear failed (replicate 4 (100 / 255))
  faded <- alphas (disabledWhen True (imageConfigured cfg {icOpacity = 0.5} iid))
  assert failed (length faded == 4 && all (< 128 / 255) faded)
  alphas (imageConfigured cfg {icOpacity = 0} iid) >>= assertEq failed []

-- | A solid rotation reserves the rotated bounds; a floating one keeps the layout and is clipped.
runImageRotationTest :: Context -> IORef Int -> IO ()
runImageRotationTest ctx failed = do
  wide <- whiteImage ctx 1 40 20
  let turned rot = imageConfigured' defaultImageConfig {icRotation = rot} wide
      sizeOf rot = (\(r, _, _) -> [rectW (respRect r), rectH (respRect r)]) <$> frame ctx wide (turned rot)
  sizeOf (RotateFloating (pi / 2)) >>= assertEq failed [40, 20]
  sizeOf (RotateSolid (pi / 2)) >>= assertNear failed [20, 40]
  -- Layout rounds widget sizes to whole pixels.
  [dw, dh] <- sizeOf (RotateSolid (pi / 4))
  assert failed (abs (dw - 60 / sqrt 2) < 1 && dw == dh)
  -- A quarter turn fills its reserved 20x40 box, with the image's top-left at the box's top-right.
  (solid, _, solidQuads) <- frame ctx wide (turned (RotateSolid (pi / 2)))
  let Rect bx by bw bh = respRect solid
  assertNear failed [0, 0, bw, bh, 0, 0, 1, 1] (concat (bounds (topLeft solid) solidQuads))
  assert failed (or [near x (bx + bw) && near y by | (x, y, u, v, _) <- concat solidQuads, near u 0, near v 0])
  -- A floating image turned 45 degrees overflows its rect, which clips it.
  (floating, floatDraw, floatQuads) <- frame ctx wide (turned (RotateFloating (pi / 4)))
  let Rect _ _ fw fh = respRect floating
  case bounds (topLeft floating) floatQuads of
    [x : y : w : h : _] -> assert failed (x < 0 && y < 0 && x + w > fw && y + h > fh)
    other -> assertEq failed 1 (length other)
  assertEq failed [respRect floating] [Rect (cmdClipX c) (cmdClipY c) (cmdClipW c) (cmdClipH c) | c <- drawCmdElems floatDraw, cmdTextureId c == atlasTextureId]
  -- A changed angle (as in an animation) repaints the image.
  _ <- runFrame ctx (withInput 400 400) (column (turned (RotateFloating (pi / 3))))
  takeDamage ctx >>= assert failed . (`clipCovers` respRect floating)
  -- A rotated image that only moves keeps its ops, translated.
  let at top = (\(_, _, quads) -> bounds (V2 0 top) quads) <$> frame ctx wide (box (fixedWH 10 top) (colorRGBA 0 0 0 0) >> turned (RotateFloating (pi / 3)))
  high <- at 10
  low <- at 30
  assertEq failed 1 (length low)
  assertNear failed (concat high) (concat low)

-- | A drawing's image ops draw registered images from the atlas, including rotated ones.
runImageDrawingOpsTest :: Context -> IORef Int -> IO ()
runImageDrawingOpsTest ctx failed = do
  iid <- whiteImage ctx 1 16 8
  let white = colorRGBA 255 255 255 255
      drawn paint = (\(r, _, quads) -> concat (bounds (topLeft r) quads)) <$> frame ctx iid (canvas (fixedWH 40 40) paint)
  drawn (\(Rect x y _ _) -> drawImage (Rect (x + 10) (y + 5) 16 8) iid white) >>= assertNear failed [10, 5, 16, 8, 0, 0, 1, 1]
  drawn (\(Rect x y _ _) -> drawImageWith (imageDraw (Rect (x + 10) (y + 10) 16 8) iid) {imageAngle = pi / 2}) >>= assertNear failed [14, 6, 8, 16, 0, 0, 1, 1]
  -- A sub-rect of the image, then faded; opacity 0 draws nothing.
  drawn (\(Rect x y _ _) -> drawImageWith (imageDraw (Rect x y 8 8) iid) {imageUV = Rect 0.5 0 0.5 1}) >>= assertNear failed [0, 0, 8, 8, 0.5, 0, 1, 1]
  let alphas paint = (\(_, _, quads) -> [a | (_, _, _, _, a) <- concat quads]) <$> frame ctx iid (canvas (fixedWH 40 40) paint)
  alphas (\r -> drawImageWith (imageDraw r iid) {imageOpacity = 0.5}) >>= assertNear failed (replicate 4 (128 / 255))
  alphas (\r -> drawImageWith (imageDraw r iid) {imageOpacity = 0}) >>= assertEq failed []

-- | When the layout sizes one axis, a registered image keeps its aspect
-- ratio (filling a column's width sets its height). A layout 'aspect'
-- overrides it, and a plain 'image' stays 32 pixels high.
runImageOwnAspectTest :: Context -> IORef Int -> IO ()
runImageOwnAspectTest ctx failed = do
  wide <- whiteImage ctx 1 40 20
  let sized ui = (\(Rect _ _ w h) -> (w, h)) . respRect <$> warmup2 ctx (withInput 400 400) (columnWith (tight . fixedW 300) ui)
  sized (imageConfigured' defaultImageConfig {icLayout = fillW} wide) >>= assertEq failed (300, 150)
  sized (imageConfigured' defaultImageConfig {icLayout = fillW . aspect 1} wide) >>= assertEq failed (300, 300)
  sized (image' fillW wide) >>= assertEq failed (300, 32)
  -- In a row that splits its width, the height follows the share it gets.
  sized (rowWith (tight . gap 0 . fillW) (imageConfigured' defaultImageConfig {icLayout = fillW} wide <* box (fixedWH 100 10) (colorRGBA 0 0 0 0)))
    >>= assertEq failed (200, 100)

-- | A crop treats part of the image as the whole image for size and fit. A
-- scale zooms the fitted image about its centre, clipped to the rect.
runImageCropScaleTest :: Context -> IORef Int -> IO ()
runImageCropScaleTest ctx failed = do
  wide <- whiteImage ctx 1 40 20
  let drawnWith cfg = (\(r, _, quads) -> (rectW (respRect r), rectH (respRect r), concat (bounds (topLeft r) quads))) <$> frame ctx wide (imageConfigured' cfg wide)
      check cfg want = drawnWith cfg >>= \(w, h, got) -> assertNear failed want (w : h : got)
  check defaultImageConfig {icCrop = Just (Rect 10 0 20 20)} [20, 20, 0, 0, 20, 20, 0.25, 0, 0.75, 1]
  -- A crop past the image edge is clamped to it.
  check defaultImageConfig {icCrop = Just (Rect 30 10 100 100)} [10, 10, 0, 0, 10, 10, 0.75, 0.5, 1, 1]
  check defaultImageConfig {icLayout = fixedWH 100 80, icScale = 2} [100, 80, 0, 0, 100, 80, 0.25, 0.25, 0.75, 0.75]
  check defaultImageConfig {icLayout = fixedWH 100 80, icScale = 0.5} [100, 80, 25, 20, 50, 40, 0, 0, 1, 1]
  check defaultImageConfig {icLayout = fixedWH 100 80, icCrop = Just (Rect 0 0 20 20), icScale = 2} [100, 80, 0, 0, 100, 80, 0.125, 0.25, 0.375, 0.75]
  check defaultImageConfig {icLayout = fixedWH 100 80, icCrop = Just (Rect 0 0 20 20), icFit = FitContain} [100, 80, 10, 0, 80, 80, 0, 0, 0.5, 1]

-- | 'fitRect' places content for each fit and alignment, extending past the box when the fit overflows.
runImageFitRectTest :: Context -> IORef Int -> IO ()
runImageFitRectTest _ failed = do
  let box0 = Rect 10 10 100 80
      at fit ax ay size = fitRect fit ax ay size box0
  assertEq failed
    [Rect 10 25 100 50, Rect (-20) 10 160 80, Rect 10 10 100 80, Rect 50 45 20 10, Rect 90 80 20 10, Rect 10 10 20 10, Rect 10 10 0 0]
    [ at FitContain AlignCenter AlignMiddle (40, 20)
    , at FitCover AlignCenter AlignTop (40, 20)
    , at FitFill AlignEnd AlignBottom (40, 20)
    , at FitNone AlignCenter AlignMiddle (20, 10)
    , at FitScaleDown AlignEnd AlignBottom (20, 10)
    , at FitScaleDown AlignStart AlignTop (20, 10)
    , at FitContain AlignStart AlignTop (0, 10)
    ]

-- | Changing how a configured image draws at the same rect (e.g. an
-- animated fade) repaints it; drawing it unchanged does not.
runImageLookDamageTest :: Context -> IORef Int -> IO ()
runImageLookDamageTest ctx failed = do
  writeIORef (ctxPaintFull ctx) False
  wide <- whiteImage ctx 1 40 20
  let ui o = column (imageConfigured' defaultImageConfig {icLayout = fixedWH 80 40, icOpacity = o} wide)
  r <- warmup2 ctx (withInput 400 400) (ui 1)
  _ <- runFrame ctx (withInput 400 400) (ui 1)
  takeDamage ctx >>= assert failed . damageIsEmpty
  _ <- runFrame ctx (withInput 400 400) (ui 0.5)
  takeDamage ctx >>= assert failed . (`clipCovers` respRect r)
  _ <- runFrame ctx (withInput 400 400) (ui 0.5)
  takeDamage ctx >>= assert failed . damageIsEmpty
  writeIORef (ctxPaintFull ctx) True

-- | A configured SVG icon fades like an image, and the default config draws
-- the same as 'svgIconWith'.
runSvgIconConfiguredTest :: Context -> IORef Int -> IO ()
runSvgIconConfiguredTest ctx failed = do
  doc <- either fail pure (parseSvg "<svg viewBox='0 0 24 24'><rect x='2' y='2' width='20' height='20'/></svg>")
  let quadsOf ui = (\(_, _, dd, _) -> dd) <$> run2Frames ctx (withInput 200 200) (column ui) >>= drawQuads
      white = colorRGBA 255 255 255
  plain <- quadsOf (svgIconWith (fixedWH 16 16) doc)
  quadsOf (svgIconConfigured defaultImageConfig {icLayout = fixedWH 16 16} doc) >>= assertEq failed plain
  quadsOf (svgIconConfigured defaultImageConfig {icLayout = fixedWH 16 16 . fontColor (white 255), icOpacity = 0.5} doc)
    >>= assert failed . any ((== white 128) . snd)

-- | 'useImageRgba' registers once per key and returns the same id while
-- called. A new key registers a new image and frees the old one. A frame
-- that skips the hook frees its image, and the next image reuses that
-- atlas space.
runUseImageRgbaTest :: Context -> IORef Int -> IO ()
runUseImageRgbaTest ctx failed = do
  let px v = BS.replicate (8 * 8 * 4) v
      hook k = useImageRgba k 8 8 (px 90)
      frameOf :: NanoUI x -> IO x
      frameOf ui = (\(a, _, _, _) -> a) <$> runFrame ctx (withInput 100 100) ui
      originOf iid = fmap (\(u0, v0, _, _) -> (u0, v0)) <$> lookupImageUv ctx iid
  Just a <- frameOf (hook ("a" :: T.Text))
  frameOf (hook ("a" :: T.Text)) >>= assertEq failed (Just a)
  lookupImageSize ctx a >>= assertEq failed (Just (8, 8))
  aAt <- originOf a
  -- A new key: the old image is freed and the new one takes its space.
  Just b <- frameOf (hook ("b" :: T.Text))
  assert failed (b /= a)
  lookupImageSize ctx a >>= assertEq failed Nothing
  originOf b >>= assertEq failed aAt
  -- A frame without the hook frees its image; the next image reuses the space.
  frameOf (pure ())
  lookupImageSize ctx b >>= assertEq failed Nothing
  Just c <- frameOf (hook (1 :: Int))
  originOf c >>= assertEq failed aAt
  assert failed (c /= a && c /= b)
  -- A hook in a scope keeps its image while the scope is shown and frees it
  -- when not; the preceding hook keeps its own.
  let shown on = (,) <$> hook (1 :: Int) <*> scope (if on then hook ("s" :: T.Text) else pure Nothing)
  (c1, s) <- frameOf (shown True)
  assertEq failed (Just c) c1
  assertJust failed s $ \sid -> do
    lookupImageSize ctx sid >>= assertEq failed (Just (8, 8))
    frameOf (shown False) >>= assertEq failed (Just c) . fst
    lookupImageSize ctx sid >>= assertEq failed Nothing
