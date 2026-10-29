-- | The SDL session's fonts: start-up failures release what they took,
-- shaped text measures as SDL draws it, caches and font replacement leave
-- earlier text intact, and font handles outlive neither their font nor the
-- session.
module Main (main) where

import Control.Exception (IOException, evaluate, finally, try)
import Control.Monad (forM_, unless, void, when)
import Data.IORef (writeIORef)
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
import System.Directory (copyFile, createDirectory, getTemporaryDirectory, removeFile, removePathForcibly)
import System.Environment (setEnv)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.Mem (performGC)

foreign import ccall unsafe "SDL_WasInit" wasInit :: Word32 -> IO Word32

check :: String -> Bool -> IO ()
check name ok = unless ok (fail name)

-- | Whether two widths agree.
near :: Float -> Float -> Bool
near a b = abs (a - b) < 0.01

-- | The floats of a run's glyph quads, eight per glyph.
quadFloats :: Maybe ShapedGlyphs -> Int
quadFloats = maybe 0 (\(ShapedGlyphs q) -> sizeofPrimArray q)

main :: IO ()
main = do
  setEnv "SDL_VIDEODRIVER" "dummy"
  ctx0 <- newPixelContext
  -- Window creation fails after SDL starts: what was acquired is released,
  -- and a later session opens normally.
  setEnv "SDL_RENDER_DRIVER" "nano-ui-test-unavailable-renderer"
  let hidden = defaultSdlOptions {sdlWindowSettings = defaultWindowSettings {wsMode = Hidden}}
  failedStart <- try (withSdl hidden ctx0 (\_ _ -> pure ()))
  setEnv "SDL_RENDER_DRIVER" "software"
  check "an unavailable renderer did not fail at creation" $ case failedStart of
    Left (e :: IOException) -> "SDL_CreateWindowAndRenderer failed" `isInfixOf` show e
    Right () -> False
  (== 0) <$> wasInit 0 >>= check "a failed start leaked initialised SDL subsystems"
  -- The bundled Inter twice, as a family's regular and bold files: the
  -- "bold" is at the regular's own weight. The directory is this run's own,
  -- since another run may hold its files open.
  fixtures <- do
    (path, handle) <- (`openTempFile` "nano-ui-font-effects") =<< getTemporaryDirectory
    hClose handle >> removeFile path >> createDirectory path
    pure path
  let regularFile = fixtures </> "NanoFixture-Regular.ttf"
      text = "AV To fi café λ"
  copyFile "data/inter.ttf" regularFile
  copyFile "data/inter.ttf" (fixtures </> "NanoFixture-Bold.ttf")
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
    check ("prepared shaped width differs from SDL's: " ++ show (hostWidth, measured)) (near hostWidth measured)
    check "font scaling lost its metric scale" (near scaled (measured * 1.5))
    check ("italic shaped width differs from SDL's: " ++ show (italicHost, italicWidth)) $
      near italicHost italicWidth
    before <- drawShaped fm text
    check "shaped drawing returned no glyph quads" (quadFloats before >= 8 * 10)
    -- Exceeding both cache caps through measurement alone neither fills nor
    -- resets the atlas, nor alters a run already rasterised.
    forM_ [1 .. 1100 :: Int] $ \n -> do
      let counter = "counter " <> T.pack (show n)
      p <- prepareFontMetrics fm counter
      void (evaluate (lineWidth p counter))
    after <- drawShaped fm text
    check "metric preparation mutated the atlas or raster cache" (before == after)
    let oversized = T.replicate 1000 "W"
    large <- prepareFontMetrics fm oversized
    largeGlyphs <- drawShaped fm oversized
    check "text wider than the atlas lost its glyphs" $
      quadFloats largeGlyphs == 8 * 1000 && lineWidth large oversized > 0
    shapingChecks fm
    performGC
    lineWidthIO fm text >>= check "font metrics changed after cache eviction and GC" . near measured
    -- Replacing the font and atlas keeps old pure snapshots valid, while
    -- old native callbacks reject their closed font.
    writeIORef (sdlFontRequestRef env) (FontSearch [])
    (replacement, _) <- syncDisplay ctx env emptyInput
    expectClosed (drawShaped fm text)
    (fresh, _) <- resolve replacement WeightNormal FontStyleNormal
    freshWidth <- lineWidthIO fresh text
    freshQuad <- drawShaped fresh text
    check "font/atlas replacement failed to restore shaped text" (near freshWidth measured && freshQuad /= Nothing)
    -- A file named as the font's bold but no heavier is not its bold face:
    -- bold text stays synthetic, and measures as it draws.
    writeIORef (sdlFontRequestRef env) (FontFilePath regularFile)
    (fromFile, _) <- syncDisplay replacement env emptyInput
    (boldFm, native) <- resolve fromFile WeightBold FontStyleNormal
    when native $ fail "a file at the regular weight was taken for the bold face"
    boldWidth <- measure fromFile WeightBold FontStyleNormal
    drawnWidth <- lineWidthIO boldFm text
    check ("bold text measures unlike it draws: " ++ show (boldWidth, drawnWidth)) (near boldWidth drawnWidth)
    -- A file that will not open draws in bundled Inter, and the debug
    -- readout names the font drawn, not the one asked for.
    let missing = fixtures </> "NanoMissing.ttf"
    writeIORef (sdlFontRequestRef env) (FontFilePath missing)
    _ <- syncDisplay fromFile env emptyInput
    shown <- dbgFontPath <$> sdlDebugSnapshot env
    check "the debug readout names a font that did not open" (shown /= missing)
    pure (fresh, prepared, measured, freshQuad)
  -- Pure metric and quad values stay safe after the native font, atlas and
  -- session close; native effects fail before dereferencing.
  afterClose <- evaluate (lineWidth snapshot text)
  check "an immutable snapshot changed after shutdown" (afterClose == width)
  void (evaluate quad)
  expectClosed (prepareFontMetrics font "new text")
  expectClosed (drawShaped font text)
  putStrLn "font effects: ok"

-- | Shaping reorders right-to-left text and gives every character a caret:
-- an Arabic word's carets run right to left (the script sets the direction,
-- whichever font draws it), a mixed line keeps its Latin carets increasing,
-- and a click lands on the nearest caret.
shapingChecks :: FontMetrics -> IO ()
shapingChecks fm = do
  let arabic = "مرحبا"
      mixed = "Hi مرحبا 12"
  pArabic <- prepareFontMetrics fm arabic
  pMixed <- prepareFontMetrics fm mixed
  case (fmShape pArabic arabic, fmShape pMixed mixed) of
    (Just a, Just m) -> do
      let carets = primArrayToList (stCarets a)
          caret = indexPrimArray (stCarets m)
      check "Arabic carets do not cover every character" (length carets == T.length arabic + 1)
      check ("Arabic carets do not decrease: " <> show carets) (and (zipWith (>=) carets (drop 1 carets)))
      check "a click right of Arabic text did not land before it" $
        textIndexAtX pArabic arabic (maximum carets + 1) == 0
      check "Latin carets in a mixed line do not increase" (caret 0 < caret 1 && caret 1 < caret 2)
      check "digits after right-to-left text do not run left to right" (caret 9 < caret 10 && caret 10 < caret 11)
    _ -> fail "Arabic or mixed text was not shaped"

expectClosed :: IO a -> IO ()
expectClosed action =
  try (void action) >>= \case
    Left (err :: IOException) | "used after" `isInfixOf` show err -> pure ()
    _ -> fail "a retained font callback did not reject its closed native handle"
