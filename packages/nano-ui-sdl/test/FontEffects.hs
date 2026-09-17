module Main (main) where

import Control.Exception (IOException, evaluate, try)
import Control.Monad (forM_, unless, void)
import Data.List (isInfixOf)
import Data.IORef (writeIORef)
import qualified Data.Text as T
import Data.Primitive.PrimArray (indexPrimArray, sizeofPrimArray)
import NanoUI
import NanoUI.Testing (newPixelContext, textIndexAtX)
import NanoUI.Backend.Sdl (NanoUIFont (..), SdlEnv (..), syncDisplay, withSdlBench)
import NanoUI.Context (ctxResolveFont, ctxResolveMeasure)
import System.Environment (setEnv)
import System.Mem (performGC)

main :: IO ()
main = do
  setEnv "SDL_VIDEODRIVER" "dummy"
  setEnv "SDL_RENDER_DRIVER" "software"
  ctx0 <- newPixelContext
  (font, snapshot, width, quad) <- withSdlBench ctx0 $ \ctx env -> do
    (fm, _) <- ctxResolveFont ctx 16 WeightNormal FontStyleNormal FontRegular
    let text = "AV To fi café λ"
    prepared <- prepareFontMetrics fm text
    (hostWidth, _) <- ctxResolveMeasure ctx 16 WeightNormal FontStyleNormal FontRegular text
    measured <- evaluate (lineWidth prepared text)
    unless (abs (hostWidth - measured) < 0.01) $
      fail ("prepared shaped width differs from SDL measurement: " ++ show (hostWidth, measured))
    scaled <- lineWidthIO (scaleFontMetrics 1.5 fm) text
    unless (abs (scaled - measured * 1.5) < 0.01) $
      fail "effectful font scaling lost its metric scale"
    before <- drawShaped fm text
    unless (maybe False (\(ShapedGlyphs q) -> sizeofPrimArray q >= 8 * 10) before) $
      fail "shaped drawing returned no glyph quads"
    -- Exceed both cache caps through measurement only. This must not fill or
    -- reset the atlas, nor alter an already-rasterised run.
    forM_ [1 .. 1100 :: Int] $ \n -> do
      let labelText = "counter " <> T.pack (show n)
      p <- prepareFontMetrics fm labelText
      void (evaluate (lineWidth p labelText))
    after <- drawShaped fm text
    unless (before == after) $
      fail "metric preparation mutated the atlas or raster cache"
    let oversized = T.replicate 1000 "W"
    large <- prepareFontMetrics fm oversized
    largeGlyphs <- drawShaped fm oversized
    unless (maybe False (\(ShapedGlyphs q) -> sizeofPrimArray q == 8 * 1000) largeGlyphs && lineWidth large oversized > 0) $
      fail "text wider than the atlas lost its glyphs"
    shapingChecks fm
    performGC
    again <- lineWidthIO fm text
    unless (abs (again - measured) < 0.01) $
      fail "font metrics changed after cache eviction and GC"
    -- Exercise the actual font/atlas replacement path. Old pure snapshots
    -- remain valid while old native callbacks must reject their closed font.
    writeIORef (sdlFontRequestRef env) (FontSearch [])
    (replacement, _) <- syncDisplay ctx env emptyInput
    expectClosed (drawShaped fm text)
    (fresh, _) <- ctxResolveFont replacement 16 WeightNormal FontStyleNormal FontRegular
    freshWidth <- lineWidthIO fresh text
    freshQuad <- drawShaped fresh text
    unless (abs (freshWidth - measured) < 0.01 && freshQuad /= Nothing) $
      fail "font/atlas replacement failed to restore shaped text"
    pure (fresh, prepared, measured, freshQuad)
  -- Pure metric and quad values are safe to evaluate after the native font,
  -- atlas and SDL session are closed; native effects fail before dereferencing.
  afterClose <- evaluate (lineWidth snapshot "AV To fi café λ")
  unless (afterClose == width) $ fail "immutable snapshot changed after shutdown"
  void (evaluate quad)
  expectClosed (prepareFontMetrics font "new text")
  expectClosed (drawShaped font "AV To fi café λ")
  expectClosed (drawGlyph font 'A')
  putStrLn "font effects: ok"

-- | Shaping reorders right-to-left text and gives every character a caret:
-- an Arabic word's carets run right to left, a mixed line keeps its Latin
-- carets increasing, and a click lands on the nearest caret.
shapingChecks :: FontMetrics -> IO ()
shapingChecks fm = do
  let arabic = "مرحبا"
      mixed = "Hi مرحبا 12"
  pArabic <- prepareFontMetrics fm arabic
  case fmShape pArabic arabic of
    Nothing -> fail "Arabic text was not shaped"
    Just st -> do
      let carets = [indexPrimArray (stCarets st) i | i <- [0 .. sizeofPrimArray (stCarets st) - 1]]
      unless (length carets == T.length arabic + 1) $ fail "Arabic carets do not cover every character"
      -- The script sets the direction, so the word runs right to left
      -- whichever font draws it.
      unless (and (zipWith (>=) carets (drop 1 carets))) $
        fail ("Arabic carets do not decrease: " <> show carets)
      unless (textIndexAtX pArabic arabic (maximum carets + 1) == 0) $
        fail "a click at the right edge of Arabic text did not land before its first character"
  pMixed <- prepareFontMetrics fm mixed
  case fmShape pMixed mixed of
    Nothing -> fail "mixed text was not shaped"
    Just st -> do
      let caret i = indexPrimArray (stCarets st) i
      unless (caret 0 < caret 1 && caret 1 < caret 2) $ fail "Latin carets in a mixed line do not increase"
      unless (caret 9 < caret 10 && caret 10 < caret 11) $ fail "digits after right-to-left text do not run left to right"

expectClosed :: IO a -> IO ()
expectClosed action = do
  result <- try (void action) :: IO (Either IOException ())
  case result of
    Left err | "used after" `isInfixOf` show err -> pure ()
    _ -> fail "retained font callback did not reject its closed native handle"
