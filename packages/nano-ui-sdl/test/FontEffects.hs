-- | The SDL session's fonts: start-up failures release what they took,
-- shaped text measures as SDL draws it, caches and font replacement leave
-- earlier text intact, and font handles outlive neither their font nor the
-- session.
module Main (main) where

import Control.Exception (IOException, evaluate, finally, try)
import Control.Monad (forM_, void)
import Data.ByteString qualified as BS
import Data.IORef (IORef, writeIORef)
import Data.List (isInfixOf)
import Data.Primitive.PrimArray (indexPrimArray, primArrayToList, sizeofPrimArray)
import Data.Text qualified as T
import Data.Word (Word32)
import NanoUI
import NanoUI.Backend
import NanoUI.Backend.Sdl
  ( NanoUIFont (..), SdlDebugSnapshot (..), SdlOptions (..), defaultSdlOptions, sdlDebugSnapshot, syncDisplay
  , withSdl, withSdlBench
  )
import NanoUI.Internal.Context (ctxResolveFont, ctxResolveMeasure)
import NanoUI.Sdl.Internal.Window (sdlFontRequestRef)
import NanoUI.Testing (textIndexAtX)
import NanoUI.Testing.Runner (check, runChecks)
import NanoUI.Ttf.Internal.Inter (fontInterBytes)
import System.Directory (createDirectory, getTemporaryDirectory, removeFile, removePathForcibly)
import System.Environment (setEnv)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.Mem (performGC)

foreign import ccall unsafe "SDL_WasInit" wasInit :: Word32 -> IO Word32

-- | Whether two widths agree.
near :: Float -> Float -> Bool
near a b = abs (a - b) < 0.01

-- | The floats of a run's glyph quads, eight per glyph.
quadFloats :: Maybe ShapedGlyphs -> Int
quadFloats = maybe 0 (\(ShapedGlyphs q) -> sizeofPrimArray q)

main :: IO ()
main = do
  setEnv "SDL_VIDEO_DRIVER" "dummy"
  setEnv "SDL_RENDER_DRIVER" "software"
  ctx0 <- newPixelContext
  runChecks [("failed-start", failedStartChecks ctx0), ("session-fonts", sessionFontChecks ctx0)]

-- | Window creation fails after SDL starts: what was acquired is released,
-- and a later session opens normally.
failedStartChecks :: Context -> IORef Int -> IO ()
failedStartChecks ctx0 failed = do
  setEnv "SDL_RENDER_DRIVER" "nano-ui-test-unavailable-renderer"
  let hidden = defaultSdlOptions {sdlWindowSettings = defaultWindowSettings {wsMode = Hidden}}
  failedStart <- try (withSdl hidden ctx0 (\_ _ -> pure ()))
  setEnv "SDL_RENDER_DRIVER" "software"
  check failed "an unavailable renderer fails at creation" $ case failedStart of
    Left (e :: IOException) -> "SDL_CreateWindowAndRenderer failed" `isInfixOf` show e
    Right () -> False
  (== 0) <$> wasInit 0 >>= check failed "a failed start leaves no SDL subsystem initialised"

-- | Shaping, caches, font replacement and handle lifetimes in one session.
sessionFontChecks :: Context -> IORef Int -> IO ()
sessionFontChecks ctx0 failed = do
  -- The bundled Inter twice, as a family's regular and bold files: the
  -- "bold" is at the regular's own weight. The directory is this run's own,
  -- since another run may hold its files open.
  fixtures <- do
    (path, handle) <- (`openTempFile` "nano-ui-font-effects") =<< getTemporaryDirectory
    hClose handle >> removeFile path >> createDirectory path
    pure path
  let regularFile = fixtures </> "NanoFixture-Regular.ttf"
      text = "AV To fi café λ"
  BS.writeFile regularFile fontInterBytes
  BS.writeFile (fixtures </> "NanoFixture-Bold.ttf") fontInterBytes
  (font, snapshot, width, quad) <- (`finally` removePathForcibly fixtures) . withSdlBench ctx0 $ \ctx env -> do
    let resolve c w s = ctxResolveFont c 16 w s FontRegular
        measure c w s = fst <$> ctxResolveMeasure c 16 w s FontRegular text
    (fm, _) <- resolve ctx WeightNormal FontStyleNormal
    prepared <- prepareFontMetrics fm text
    measured <- evaluate (lineWidth prepared text)
    hostWidth <- measure ctx WeightNormal FontStyleNormal
    scaled <- lineWidthIO (scaleFontMetrics 1.5 fm) text
    (italicFm, _) <- resolve ctx WeightNormal FontStyleItalic
    italicWidth <- lineWidthIO italicFm text
    italicHost <- measure ctx WeightNormal FontStyleItalic
    check failed ("prepared shaped width matches SDL's " ++ show (hostWidth, measured)) (near hostWidth measured)
    check failed "font scaling keeps its metric scale" (near scaled (measured * 1.5))
    check failed ("italic shaped width matches SDL's " ++ show (italicHost, italicWidth)) $
      near italicHost italicWidth
    before <- drawShaped fm text
    check failed "shaped drawing returns glyph quads" (quadFloats before >= 8 * 10)
    -- Exceeding both cache caps through measurement alone neither fills nor
    -- resets the atlas, nor alters a run already rasterised.
    forM_ [1 .. 1100 :: Int] $ \n -> do
      let counter = "counter " <> T.pack (show n)
      p <- prepareFontMetrics fm counter
      void (evaluate (lineWidth p counter))
    after <- drawShaped fm text
    check failed "metric preparation leaves the atlas and raster cache alone" (before == after)
    let oversized = T.replicate 1000 "W"
    large <- prepareFontMetrics fm oversized
    largeGlyphs <- drawShaped fm oversized
    check failed "text wider than the atlas keeps its glyphs" $
      quadFloats largeGlyphs == 8 * 1000 && lineWidth large oversized > 0
    shapingChecks failed fm
    performGC
    lineWidthIO fm text >>= check failed "font metrics are unchanged after cache eviction and GC" . near measured
    -- Replacing the font and atlas keeps old pure snapshots valid, while
    -- old native callbacks reject their closed font.
    writeIORef (sdlFontRequestRef env) (FontSearch [])
    (replacement, _) <- syncDisplay ctx env emptyInput
    expectClosed failed "a replaced font rejects drawing" (drawShaped fm text)
    (fresh, _) <- resolve replacement WeightNormal FontStyleNormal
    freshWidth <- lineWidthIO fresh text
    freshQuad <- drawShaped fresh text
    check failed "font and atlas replacement restores shaped text" (near freshWidth measured && freshQuad /= Nothing)
    -- A file named as the font's bold but no heavier is not its bold face:
    -- bold text stays synthetic, and measures as it draws.
    writeIORef (sdlFontRequestRef env) (FontFilePath regularFile)
    (fromFile, _) <- syncDisplay replacement env emptyInput
    (boldFm, native) <- resolve fromFile WeightBold FontStyleNormal
    check failed "a file at the regular weight is not taken for the bold face" (not native)
    boldWidth <- measure fromFile WeightBold FontStyleNormal
    drawnWidth <- lineWidthIO boldFm text
    check failed ("bold text measures as it draws " ++ show (boldWidth, drawnWidth)) (near boldWidth drawnWidth)
    -- A file that will not open draws in bundled Inter, and the debug
    -- readout names the font drawn, not the one asked for.
    let missing = fixtures </> "NanoMissing.ttf"
    writeIORef (sdlFontRequestRef env) (FontFilePath missing)
    _ <- syncDisplay fromFile env emptyInput
    shown <- dbgFontPath <$> sdlDebugSnapshot env
    check failed "the debug readout names the font drawn" (shown /= missing)
    pure (fresh, prepared, measured, freshQuad)
  -- Pure metric and quad values stay safe after the native font, atlas and
  -- session close; native effects fail before dereferencing.
  afterClose <- evaluate (lineWidth snapshot text)
  check failed "an immutable snapshot is unchanged after shutdown" (afterClose == width)
  void (evaluate quad)
  expectClosed failed "a closed font rejects metric preparation" (prepareFontMetrics font "new text")
  expectClosed failed "a closed font rejects drawing" (drawShaped font text)

-- | Shaping reorders right-to-left text and gives every character a caret:
-- an Arabic word's carets run right to left (the script sets the direction,
-- whichever font draws it), a mixed line keeps its Latin carets increasing,
-- and a click lands on the nearest caret.
shapingChecks :: IORef Int -> FontMetrics -> IO ()
shapingChecks failed fm = do
  let arabic = "مرحبا"
      mixed = "Hi مرحبا 12"
  pArabic <- prepareFontMetrics fm arabic
  pMixed <- prepareFontMetrics fm mixed
  case (fmShape pArabic arabic, fmShape pMixed mixed) of
    (Just a, Just m) -> do
      let carets = primArrayToList (stCarets a)
          caret = indexPrimArray (stCarets m)
      check failed "Arabic carets cover every character" (length carets == T.length arabic + 1)
      check failed ("Arabic carets decrease " <> show carets) (and (zipWith (>=) carets (drop 1 carets)))
      check failed "a click right of Arabic text lands before it" $
        textIndexAtX pArabic arabic (maximum carets + 1) == 0
      check failed "Latin carets in a mixed line increase" (caret 0 < caret 1 && caret 1 < caret 2)
      check failed "digits after right-to-left text run left to right" (caret 9 < caret 10 && caret 10 < caret 11)
    _ -> check failed "Arabic and mixed text are shaped" False

-- | Check that the action fails with the error a closed native handle raises.
expectClosed :: IORef Int -> String -> IO a -> IO ()
expectClosed failed name action =
  try (void action) >>= \case
    Left (err :: IOException) -> check failed name ("used after" `isInfixOf` show err)
    Right () -> check failed name False
