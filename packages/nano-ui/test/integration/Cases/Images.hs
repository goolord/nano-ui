{-# OPTIONS_GHC -O0 #-}
module Cases.Images (tests) where

import Spec
import Data.ByteString qualified as BS
import Data.List (unzip4)
import Data.Maybe (fromMaybe)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import NanoUI.Internal.Context (lookupImageSize, lookupImageUv)

tests :: [Spec]
tests =
  [ spec "image-fit" runImageFitTest
  , spec "image-size" runImageSizeTest
  , spec "image-look" runImageLookTest
  , spec "image-rotation" runImageRotationTest
  , spec "image-damage" runImageDamageTest
  , spec "image-rgba-lifetime" runImageRgbaLifetimeTest
  ]

-- | One corner of a drawn quad: x, y, u, v and alpha.
type Corner = (Float, Float, Float, Float, Float)

-- | Run @ui@ in a column for two frames; return its result, the draw data, and each atlas
-- quad's corners with UVs normalized to image @iid@'s 0-1 range.
frame :: Context -> ImageId -> NanoUI a -> IO (a, DrawData, [[Corner]])
frame ctx iid ui = do
  (a, dd, _) <- run2Frames ctx (withInput 400 400) (column ui)
  (a0, b0, a1, b1) <- fromMaybe (0, 0, 1, 1) <$> lookupImageUv ctx iid
  let corner v = (vX v, vY v, (vU v - a0) / (a1 - a0), (vV v - b0) / (b1 - b0), vA v)
      -- Quad indices are a b c a c d.
      quads (p : q : r : _ : _ : t : rest) = map corner [p, q, r, t] : quads rest
      quads _ = []
  (,,) a dd . quads <$> textureVertices atlasTextureId dd

-- | Each quad's position relative to @o@, its size, and its UV bounds.
bounds :: V2 -> [[Corner]] -> [[Float]]
bounds (V2 ox oy) quads =
  [ [x0 - ox, y0 - oy, maximum xs - x0, maximum ys - y0, minimum us, minimum vs, maximum us, maximum vs]
  | cs <- quads
  , let (xs, ys, us, vs) = unzip4 [(x, y, u, v) | (x, y, u, v, _) <- cs]
        (x0, y0) = (minimum xs, minimum ys)
  ]

-- | The quads' bounds relative to the response's corner.
boundsAt :: Response -> [[Corner]] -> [Float]
boundsAt r = concat . bounds (V2 (rectX (respRect r)) (rectY (respRect r)))

-- | The alpha of every corner drawn.
alphas :: (a, b, [[Corner]]) -> [Float]
alphas (_, _, quads) = [a | (_, _, _, _, a) <- concat quads]

near :: Float -> Float -> Bool
near a b = abs (a - b) < 1.0e-3

-- | Assert equal-length lists of nearly equal numbers; on failure show both.
assertNear :: IORef Int -> [Float] -> [Float] -> IO ()
assertNear failed want got = unless (length want == length got && and (zipWith near want got)) (assertEq failed want got)

-- | Register a white @w@ x @h@ image under id @n@.
whiteImage :: Context -> Int -> Int -> Int -> IO ImageId
whiteImage ctx n w h = ImageId n <$ registerImage ctx (ImageId n) w h (BS.replicate (w * h * 4) 255)

-- | Each fit and alignment places wide and tall images in wide and tall
-- boxes, cropping via UVs, never past the box; the default stretches like
-- 'image'. A crop is the whole image for size and fit; a scale zooms.
runImageFitTest :: Context -> IORef Int -> IO ()
runImageFitTest ctx failed = do
  wide <- whiteImage ctx 1 40 20
  tall <- whiteImage ctx 2 20 40
  big <- whiteImage ctx 3 200 100
  let (wb, tb, full, mid) = ((100, 80), (60, 120), [0, 0, 1, 1], (AlignCenter, AlignMiddle))
      fitted (bw, bh) fit (ax, ay) = defaultImageConfig {icLayout = fixedWH bw bh, icFit = fit, icAlignX = ax, icAlignY = ay}
      -- The rect's size, then the quads' bounds.
      drawnWith iid cfg =
        (\(r, _, quads) -> rectW (respRect r) : rectH (respRect r) : boundsAt r quads) <$> frame ctx iid (imageConfigured' cfg iid)
      sized (bw, bh) = [bw, bh]
  forM_
    [ (wide, wb, FitContain, mid, [0, 15, 100, 50] ++ full), (wide, wb, FitCover, mid, [0, 0, 100, 80, 0.1875, 0, 0.8125, 1])
    , (wide, wb, FitFill, mid, [0, 0, 100, 80] ++ full), (wide, wb, FitNone, mid, [30, 30, 40, 20] ++ full)
    , (wide, wb, FitScaleDown, mid, [30, 30, 40, 20] ++ full), (big, wb, FitScaleDown, mid, [0, 15, 100, 50] ++ full)
    , (big, wb, FitNone, mid, [0, 0, 100, 80, 0.25, 0.1, 0.75, 0.9]), (wide, tb, FitContain, mid, [0, 45, 60, 30] ++ full)
    , (wide, tb, FitCover, mid, [0, 0, 60, 120, 0.375, 0, 0.625, 1]), (tall, wb, FitContain, mid, [30, 0, 40, 80] ++ full)
    , (tall, wb, FitCover, mid, [0, 0, 100, 80, 0, 0.3, 1, 0.7]), (tall, tb, FitContain, mid, [0, 0, 60, 120] ++ full)
    , (wide, tb, FitContain, (AlignStart, AlignTop), [0, 0, 60, 30] ++ full)
    , (wide, tb, FitContain, (AlignEnd, AlignBottom), [0, 90, 60, 30] ++ full)
    , (wide, wb, FitCover, (AlignEnd, AlignMiddle), [0, 0, 100, 80, 0.375, 0, 1, 1])
    , (tall, wb, FitCover, (AlignStart, AlignTop), [0, 0, 100, 80, 0, 0, 1, 0.4])
    ]
    $ \(iid, box', fit, align, want) -> drawnWith iid (fitted box' fit align) >>= assertNear failed (sized box' ++ want)
  let drawn ui = (\(_, _, quads) -> bounds (V2 0 0) quads) <$> frame ctx wide ui
  plain <- drawn (image (fixedWH 100 80) wide)
  drawn (imageConfigured defaultImageConfig {icLayout = fixedWH 100 80} wide) >>= assertEq failed plain
  forM_
    [ (defaultImageConfig {icCrop = Just (Rect 10 0 20 20)}, [20, 20, 0, 0, 20, 20, 0.25, 0, 0.75, 1])
    , (defaultImageConfig {icCrop = Just (Rect 30 10 100 100)}, [10, 10, 0, 0, 10, 10, 0.75, 0.5, 1, 1])
    , (defaultImageConfig {icLayout = fixedWH 100 80, icScale = 2}, [100, 80, 0, 0, 100, 80, 0.25, 0.25, 0.75, 0.75])
    , (defaultImageConfig {icLayout = fixedWH 100 80, icScale = 0.5}, [100, 80, 25, 20, 50, 40, 0, 0, 1, 1])
    , ( defaultImageConfig {icLayout = fixedWH 100 80, icCrop = Just (Rect 0 0 20 20), icScale = 2}
      , [100, 80, 0, 0, 100, 80, 0.125, 0.25, 0.375, 0.75] )
    , ( defaultImageConfig {icLayout = fixedWH 100 80, icCrop = Just (Rect 0 0 20 20), icFit = FitContain}
      , [100, 80, 10, 0, 80, 80, 0, 0, 0.5, 1] )
    ]
    $ \(cfg, want) -> drawnWith wide cfg >>= assertNear failed want
  -- Empty content fits as an empty rect rather than dividing by zero.
  assertEq failed (Rect 10 10 0 0) (fitRect FitContain AlignStart AlignTop (0, 10) (Rect 10 10 100 80))

-- | An unsized axis takes the image's size or keeps its aspect beside a
-- sized one, within limits; a layout 'aspect' overrides it, and a plain
-- 'image' (or an unregistered one) gets the default size.
runImageSizeTest :: Context -> IORef Int -> IO ()
runImageSizeTest ctx failed = do
  wide <- whiteImage ctx 1 40 20
  let sized ui = (\(Rect _ _ w h) -> (w, h)) . respRect <$> warmup2 ctx (withInput 400 400) (columnWith (tight . fixedW 300) ui)
      configured f iid = imageConfigured' defaultImageConfig {icLayout = f} iid
  forM_
    [ (configured id wide, (40, 20)), (configured (fixedW 80) wide, (80, 40)), (configured (fixedH 10) wide, (20, 10))
    , (configured (maxW 20) wide, (20, 10)), (configured (fixedWH 30 30) wide, (30, 30)), (configured id (ImageId 99), (32, 32))
    , (configured fillW wide, (300, 150)), (configured (fillW . aspect 1) wide, (300, 300)), (image' fillW wide, (300, 32))
    , (rowWith (tight . gap 0 . fillW) (configured fillW wide <* box (fixedWH 100 10) (colorRGBA 0 0 0 0)), (200, 100))
    ]
    $ \(ui, want) -> sized ui >>= assertEq failed want

-- | Opacity, tint and disabling fade configured images, drawing image ops
-- (also rotated or cropped) and configured SVG icons. An image id past what
-- a 'Float' holds exactly still draws its own image.
runImageLookTest :: Context -> IORef Int -> IO ()
runImageLookTest ctx failed = do
  iid <- whiteImage ctx 1 40 20
  let cfg = defaultImageConfig {icLayout = fixedWH 20 20}
      alphasOf ui = alphas <$> frame ctx iid ui
      half = replicate 4 (128 / 255)
  alphasOf (imageConfigured cfg iid) >>= assertEq failed (replicate 4 1)
  alphasOf (imageConfigured cfg {icOpacity = 0.5} iid) >>= assertNear failed half
  alphasOf (imageConfigured cfg {icOpacity = 0.5, icLayout = fontColor (colorRGBA 255 0 0 200) . icLayout cfg} iid)
    >>= assertNear failed (replicate 4 (100 / 255))
  faded <- alphasOf (disabledWhen True (imageConfigured cfg {icOpacity = 0.5} iid))
  assert failed (length faded == 4 && all (< 128 / 255) faded)
  alphasOf (imageConfigured cfg {icOpacity = 0} iid) >>= assertEq failed []
  let painted paint = frame ctx iid (canvas (fixedWH 40 40) paint)
      drawn paint = (\(r, _, quads) -> boundsAt r quads) <$> painted paint
  drawn (\(Rect x y _ _) -> drawImage (Rect (x + 10) (y + 5) 16 8) iid colorWhite) >>= assertNear failed [10, 5, 16, 8, 0, 0, 1, 1]
  drawn (\(Rect x y _ _) -> drawImageWith (imageDraw (Rect (x + 10) (y + 10) 16 8) iid) {imageAngle = pi / 2})
    >>= assertNear failed [14, 6, 8, 16, 0, 0, 1, 1]
  drawn (\(Rect x y _ _) -> drawImageWith (imageDraw (Rect x y 8 8) iid) {imageUV = Rect 0.5 0 0.5 1})
    >>= assertNear failed [0, 0, 8, 8, 0.5, 0, 1, 1]
  alphas <$> painted (\r -> drawImageWith (imageDraw r iid) {imageOpacity = 0.5}) >>= assertNear failed half
  alphas <$> painted (\r -> drawImageWith (imageDraw r iid) {imageOpacity = 0}) >>= assertEq failed []
  _ <- whiteImage ctx (2 ^ (24 :: Int)) 40 20
  big <- whiteImage ctx (2 ^ (24 :: Int) + 1) 20 40
  forM_ [image (fixedWH 20 40), imageConfigured defaultImageConfig {icLayout = fixedWH 20 40, icFit = FitContain}] $ \img -> do
    (_, _, quads) <- frame ctx big (img big)
    assertNear failed [0, 0, 1, 1] (concat [drop 4 q | q <- bounds (V2 0 0) quads])
  doc <- either fail pure (parseSvg "<svg viewBox='0 0 24 24'><rect x='2' y='2' width='20' height='20'/></svg>")
  let quadsOf ui = (\(_, dd, _) -> dd) <$> run2Frames ctx (withInput 200 200) (column ui) >>= drawQuads
  plain <- quadsOf (svgIconWith (fixedWH 16 16) doc)
  quadsOf (svgIconConfigured defaultImageConfig {icLayout = fixedWH 16 16} doc) >>= assertEq failed plain
  quadsOf (svgIconConfigured defaultImageConfig {icLayout = fixedWH 16 16 . fontColor colorWhite, icOpacity = 0.5} doc)
    >>= assert failed . any ((== colorRGBA 255 255 255 128) . snd)

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
  assertNear failed [0, 0, bw, bh, 0, 0, 1, 1] (boundsAt solid solidQuads)
  assert failed (or [near x (bx + bw) && near y by | (x, y, u, v, _) <- concat solidQuads, near u 0, near v 0])
  -- A floating image turned 45 degrees overflows its rect, which clips it.
  (floating, floatDraw, floatQuads) <- frame ctx wide (turned (RotateFloating (pi / 4)))
  let Rect _ _ fw fh = respRect floating
  case bounds (V2 (rectX (respRect floating)) (rectY (respRect floating))) floatQuads of
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

-- | A new look (an animated fade) or a new image at the same rect repaints
-- the node; drawing it unchanged does not.
runImageDamageTest :: Context -> IORef Int -> IO ()
runImageDamageTest ctx failed = do
  writeIORef (ctxPaintFull ctx) False
  a <- whiteImage ctx 1 40 20
  b <- whiteImage ctx 2 40 20
  let inp = withInput 400 400
      looked o = imageConfigured' defaultImageConfig {icLayout = fixedWH 80 40, icFit = FitContain, icOpacity = o}
      repaints :: (x -> NanoUI Response) -> x -> x -> IO ()
      repaints ui x y = do
        r <- warmup2 ctx inp (column (ui x))
        forM_ [(x, damageIsEmpty), (y, (`clipCovers` respRect r)), (y, damageIsEmpty)] $ \(v, ok) ->
          runFrame ctx inp (column (ui v)) >> takeDamage ctx >>= assert failed . ok
  repaints (\o -> looked o a) 1 0.5
  repaints (looked 1) a b

-- | 'useImageRgba' keeps one image per key while called and frees it on a
-- new key or a skipped frame, for the next to reuse; 'registerImageRgba'
-- keeps an image until 'releaseImageRgba'. A failed image does not stop
-- the rest of a 'registerImages' batch.
runImageRgbaLifetimeTest :: Context -> IORef Int -> IO ()
runImageRgbaLifetimeTest ctx failed = do
  textImage <- newImageHandle
  intImage <- newImageHandle
  scopedImage <- newImageHandle
  let hook owner k = useImageRgba owner k 8 8 (BS.replicate (8 * 8 * 4) 90)
      frameOf :: NanoUI x -> IO x
      frameOf = evalUi ctx (withInput 100 100)
      originOf iid = fmap (\(u0, v0, _, _) -> (u0, v0)) <$> lookupImageUv ctx iid
      sizeIs want iid = lookupImageSize ctx iid >>= assertEq failed want
  Just a <- frameOf (hook textImage ("a" :: T.Text))
  frameOf (hook textImage ("a" :: T.Text)) >>= assertEq failed (Just a)
  sizeIs (Just (8, 8)) a
  aAt <- originOf a
  Just b <- frameOf (hook textImage ("b" :: T.Text))
  assert failed (b /= a)
  sizeIs Nothing a
  originOf b >>= assertEq failed aAt
  frameOf (pure ())
  sizeIs Nothing b
  Just c <- frameOf (hook intImage (1 :: Int))
  originOf c >>= assertEq failed aAt
  assert failed (c /= a && c /= b)
  let shown on = (,) <$> hook intImage (1 :: Int) <*> scope (if on then hook scopedImage ("s" :: T.Text) else pure Nothing)
  (c1, s) <- frameOf (shown True)
  assertEq failed (Just c) c1
  assertJust failed s $ \sid -> do
    sizeIs (Just (8, 8)) sid
    frameOf (shown False) >>= assertEq failed (Just c) . fst
    sizeIs Nothing sid
  own <- newContext
  let register = freshImageId >>= \iid -> (iid,) <$> registerImageRgba iid 8 8 (BS.replicate (8 * 8 * 4) 90)
      ownFrame :: NanoUI x -> IO x
      ownFrame = evalUi own (withInput 100 100)
  (r, okR) <- ownFrame register
  ownFrame (pure ())
  lookupImageSize own r >>= assertEq failed (Just (8, 8))
  rAt <- lookupImageUv own r
  ownFrame (releaseImageRgba r)
  lookupImageSize own r >>= assertEq failed Nothing
  (r2, okR2) <- ownFrame register
  assert failed (okR && okR2)
  lookupImageUv own r2 >>= assertEq failed rAt
  let pixel = BS.replicate 4 255
      batch = Seq.fromList [(ImageId 0, 1, 1, pixel), (ImageId 42, 1, 1, pixel)]
  assertEq failed False =<< registerImages own batch
  assert failed . (/= Nothing) =<< lookupImageUv own (ImageId 42)
