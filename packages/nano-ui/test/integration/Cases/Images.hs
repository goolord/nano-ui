module Cases.Images (tests) where

import Spec
import Data.ByteString qualified as BS
import Data.List (unzip4)
import Data.Maybe (fromMaybe)
import Data.Word (Word32)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (peekByteOff)
import NanoUI.Internal.Context (lookupImageUv)

tests :: [Spec]
tests =
  [ spec "image-fit-modes" runImageFitTest
  , spec "image-natural-size" runImageNaturalSizeTest
  , spec "image-opacity" runImageOpacityTest
  , spec "image-rotation" runImageRotationTest
  , spec "image-drawing-ops" runImageDrawingOpsTest
  ]

-- | One corner of a drawn quad: x, y, u, v and alpha.
type Corner = (Float, Float, Float, Float, Float)

-- | Two frames of @ui@ in a column: its result, the draw data, and each atlas quad's corners,
-- with UVs within image @iid@'s own 0-1 UVs.
frame :: Context -> ImageId -> NanoUI a -> IO (a, DrawData, [[Corner]])
frame ctx iid ui = do
  (a, _, dd, _) <- run2Frames ctx (withInput 400 400) (column ui)
  (a0, b0, a1, b1) <- fromMaybe (0, 0, 1, 1) <$> lookupImageUv ctx iid
  fmap ((,,) a dd . concat) . withForeignPtr (drawVertices dd) $ \vp -> withForeignPtr (drawIndices dd) $ \ip ->
    forM [c | c <- drawCmdElems dd, cmdTextureId c == atlasTextureId] $ \cmd -> do
      let off = fromIntegral (cmdIndexOffset cmd)
      -- Each quad's indices run a b c a c d.
      forM [off, off + 6 .. off + fromIntegral (cmdIndexCount cmd) - 6] $ \q ->
        forM [q, q + 1, q + 2, q + 5] $ \ii -> do
          vi <- fromIntegral <$> (peekByteOff ip (ii * indexSize) :: IO Word32)
          let at o = peekByteOff vp (vi * vertexSize + o) :: IO Float
          (\x y u v alpha -> (x, y, (u - a0) / (a1 - a0), (v - b0) / (b1 - b0), alpha)) <$> at 0 <*> at 4 <*> at 24 <*> at 28 <*> at 20

-- | Each quad's x and y from @o@, its width and height, and its UV bounds.
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

-- | Require lists of the same length whose numbers are near, showing both otherwise.
assertNear :: IORef Int -> [Float] -> [Float] -> IO ()
assertNear failed want got = unless (length want == length got && and (zipWith near want got)) (assertEq failed want got)

-- | Register a white image of @w@ by @h@ pixels under @n@.
whiteImage :: Context -> Int -> Int -> Int -> IO ImageId
whiteImage ctx n w h = ImageId n <$ registerImage ctx (ImageId n) w h (BS.replicate (w * h * 4) 255)

-- | Each fit places wide and tall images in wide and tall boxes, cropping by UVs, not past the box.
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
      -- Alignment places what the fit leaves room around, and picks what a crop keeps.
    , (wide, tb, FitContain, (AlignStart, AlignTop), [0, 0, 60, 30] ++ full), (wide, tb, FitContain, (AlignEnd, AlignBottom), [0, 90, 60, 30] ++ full)
    , (wide, wb, FitCover, (AlignEnd, AlignMiddle), [0, 0, 100, 80, 0.375, 0, 1, 1]), (tall, wb, FitCover, (AlignStart, AlignTop), [0, 0, 100, 80, 0, 0, 1, 0.4])
    ]
    $ \(iid, (bw, bh), fit, (ax, ay), want) -> do
      let cfg = defaultImageConfig {icLayout = fixedWH bw bh defaultLayout, icFit = fit, icAlignX = ax, icAlignY = ay}
      (resp, _, quads) <- frame ctx iid (imageConfigured' cfg iid)
      assertNear failed want (concat (bounds (topLeft resp) quads))
  -- The default configuration stretches the image over its rect, as 'image' does.
  let drawn ui = (\(_, _, quads) -> bounds (V2 0 0) quads) <$> frame ctx wide ui
  plain <- drawn (image (fixedWH 100 80) wide)
  drawn (imageConfigured defaultImageConfig {icLayout = fixedWH 100 80 defaultLayout} wide) >>= assertEq failed plain

-- | An unsized axis takes the image's size, keeps its shape beside a fixed axis, and obeys limits.
runImageNaturalSizeTest :: Context -> IORef Int -> IO ()
runImageNaturalSizeTest ctx failed = do
  wide <- whiteImage ctx 1 40 20
  -- An image not registered takes what 'image' does.
  forM_ [(id, wide, (40, 20)), (fixedW 80, wide, (80, 40)), (fixedH 10, wide, (20, 10)), (maxW 20, wide, (20, 10)), (fixedWH 30 30, wide, (30, 30)), (id, ImageId 99, (32, 32))] $
    \(f, iid, want) -> do
      Rect _ _ w h <- respRect <$> warmup2 ctx (withInput 400 400) (column (imageConfigured' defaultImageConfig {icLayout = f defaultLayout} iid))
      assertEq failed want (w, h)

-- | Opacity scales the tint's alpha; disabling fades it further, and nothing is drawn at 0.
runImageOpacityTest :: Context -> IORef Int -> IO ()
runImageOpacityTest ctx failed = do
  iid <- whiteImage ctx 1 10 10
  let cfg = defaultImageConfig {icLayout = fixedWH 20 20 defaultLayout}
      alphas c = (\(_, _, quads) -> [a | (_, _, _, _, a) <- concat quads]) <$> frame ctx iid c
  alphas (imageConfigured cfg iid) >>= assertEq failed (replicate 4 1)
  alphas (imageConfigured cfg {icOpacity = 0.5} iid) >>= assertNear failed (replicate 4 (128 / 255))
  alphas (imageConfigured cfg {icOpacity = 0.5, icLayout = fontColor (colorRGBA 255 0 0 200) (icLayout cfg)} iid)
    >>= assertNear failed (replicate 4 (100 / 255))
  faded <- alphas (disabledWhen True (imageConfigured cfg {icOpacity = 0.5} iid))
  assert failed (length faded == 4 && all (< 128 / 255) faded)
  alphas (imageConfigured cfg {icOpacity = 0} iid) >>= assertEq failed []

-- | A solid rotation reserves the turned image's bounds; a floating one keeps the layout, clipped.
runImageRotationTest :: Context -> IORef Int -> IO ()
runImageRotationTest ctx failed = do
  wide <- whiteImage ctx 1 40 20
  let turned rot = imageConfigured' defaultImageConfig {icRotation = rot} wide
      sizeOf rot = (\(r, _, _) -> [rectW (respRect r), rectH (respRect r)]) <$> frame ctx wide (turned rot)
  sizeOf (RotateFloating (pi / 2)) >>= assertEq failed [40, 20]
  sizeOf (RotateSolid (pi / 2)) >>= assertNear failed [20, 40]
  -- Layout sizes a widget in whole pixels.
  [dw, dh] <- sizeOf (RotateSolid (pi / 4))
  assert failed (abs (dw - 60 / sqrt 2) < 1 && dw == dh)
  -- A quarter turn fills the 20 by 40 box it reserved, the image's top-left at its top right.
  (solid, _, solidQuads) <- frame ctx wide (turned (RotateSolid (pi / 2)))
  let Rect bx by bw bh = respRect solid
  assertNear failed [0, 0, bw, bh, 0, 0, 1, 1] (concat (bounds (topLeft solid) solidQuads))
  assert failed (or [near x (bx + bw) && near y by | (x, y, u, v, _) <- concat solidQuads, near u 0, near v 0])
  -- Turned an eighth, a floating image leaves its rect, which clips it.
  (floating, floatDraw, floatQuads) <- frame ctx wide (turned (RotateFloating (pi / 4)))
  let Rect _ _ fw fh = respRect floating
  case bounds (topLeft floating) floatQuads of
    [x : y : w : h : _] -> assert failed (x < 0 && y < 0 && x + w > fw && y + h > fh)
    other -> assertEq failed 1 (length other)
  assertEq failed [respRect floating] [Rect (cmdClipX c) (cmdClipY c) (cmdClipW c) (cmdClipH c) | c <- drawCmdElems floatDraw, cmdTextureId c == atlasTextureId]
  -- A turn that changes, as an animated one does, repaints the image.
  _ <- runFrame ctx (withInput 400 400) (column (turned (RotateFloating (pi / 3))))
  takeDamage ctx >>= assert failed . (`clipCovers` respRect floating)
  -- A turned image that only moves keeps its ops, moved with it.
  let at top = (\(_, _, quads) -> bounds (V2 0 top) quads) <$> frame ctx wide (box (fixedWH 10 top) (colorRGBA 0 0 0 0) >> turned (RotateFloating (pi / 3)))
  high <- at 10
  low <- at 30
  assertEq failed 1 (length low)
  assertNear failed (concat high) (concat low)

-- | A drawing's image ops draw registered images from the atlas, turned ones turned.
runImageDrawingOpsTest :: Context -> IORef Int -> IO ()
runImageDrawingOpsTest ctx failed = do
  iid <- whiteImage ctx 1 16 8
  let white = colorRGBA 255 255 255 255
      drawn paint = (\(r, _, quads) -> concat (bounds (topLeft r) quads)) <$> frame ctx iid (canvas (fixedWH 40 40) paint)
  drawn (\(Rect x y _ _) -> drawImage (Rect (x + 10) (y + 5) 16 8) iid white) >>= assertNear failed [10, 5, 16, 8, 0, 0, 1, 1]
  drawn (\(Rect x y _ _) -> drawImageRotated (Rect (x + 10) (y + 10) 16 8) (pi / 2) iid white) >>= assertNear failed [14, 6, 8, 16, 0, 0, 1, 1]
