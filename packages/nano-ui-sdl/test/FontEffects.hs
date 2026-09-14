module Main (main) where

import Control.Exception (IOException, evaluate, try)
import Control.Monad (forM_, unless, void)
import Data.List (isInfixOf)
import Data.IORef (writeIORef)
import qualified Data.Text as T
import NanoUI
import NanoUI.Backend.Sdl (NanoUIFont (..), SdlEnv (..), newSdlContext, syncDisplay, withSdlBench)
import NanoUI.Context (ctxResolveFont, ctxResolveMeasure)
import System.Environment (setEnv)
import System.Mem (performGC)

main :: IO ()
main = do
  setEnv "SDL_VIDEODRIVER" "dummy"
  setEnv "SDL_RENDER_DRIVER" "software"
  ctx0 <- newSdlContext
  (font, snapshot, width, quad) <- withSdlBench ctx0 $ \ctx env -> do
    (fm, _) <- ctxResolveFont ctx 16 WeightNormal FontStyleNormal FontRegular
    let text = "AV To fi café λ"
    prepared <- prepareFontMetrics fm text
    (hostWidth, _) <- ctxResolveMeasure ctx 16 WeightNormal FontStyleNormal FontRegular text
    measured <- evaluate (lineWidth prepared text)
    unless (abs (hostWidth - measured) < 0.01) $
      fail "prepared shaped width differs from SDL measurement"
    scaled <- lineWidthIO (scaleFontMetrics 1.5 fm) text
    unless (abs (scaled - measured * 1.5) < 0.01) $
      fail "effectful font scaling lost its metric scale"
    before <- drawRun fm text
    unless (maybe False (\q -> rqU1 q > rqU0 q && rqV1 q > rqV0 q) before) $
      fail "explicit run rasterisation returned no atlas quad"
    -- Exceed both cache caps through measurement only. This must not fill or
    -- reset the atlas, nor alter an already-rasterised run.
    forM_ [1 .. 1100 :: Int] $ \n -> do
      let labelText = "counter " <> T.pack (show n)
      p <- prepareFontMetrics fm labelText
      void (evaluate (lineWidth p labelText))
    after <- drawRun fm text
    unless (before == after) $
      fail "metric preparation mutated the atlas or raster cache"
    let oversized = T.replicate 1000 "W"
    large <- prepareFontMetrics fm oversized
    largeRun <- drawRun fm oversized
    unless (largeRun == Nothing && lineWidth large oversized > 0) $
      fail "oversized text lost its per-glyph fallback"
    performGC
    again <- lineWidthIO fm text
    unless (abs (again - measured) < 0.01) $
      fail "font metrics changed after cache eviction and GC"
    -- Exercise the actual font/atlas replacement path. Old pure snapshots
    -- remain valid while old native callbacks must reject their closed font.
    writeIORef (sdlFontRequestRef env) (FontSearch [])
    (replacement, _) <- syncDisplay ctx env emptyInput
    expectClosed (drawRun fm text)
    (fresh, _) <- ctxResolveFont replacement 16 WeightNormal FontStyleNormal FontRegular
    freshWidth <- lineWidthIO fresh text
    freshQuad <- drawRun fresh text
    unless (abs (freshWidth - measured) < 0.01 && freshQuad /= Nothing) $
      fail "font/atlas replacement failed to restore shaped text"
    pure (fresh, prepared, measured, freshQuad)
  -- Pure metric and quad values are safe to evaluate after the native font,
  -- atlas and SDL session are closed; native effects fail before dereferencing.
  afterClose <- evaluate (lineWidth snapshot "AV To fi café λ")
  unless (afterClose == width) $ fail "immutable snapshot changed after shutdown"
  void (evaluate (fmap rqU0 quad))
  expectClosed (prepareFontMetrics font "new text")
  expectClosed (drawRun font "AV To fi café λ")
  expectClosed (drawGlyph font 'A')
  putStrLn "font effects: ok"

expectClosed :: IO a -> IO ()
expectClosed action = do
  result <- try (void action) :: IO (Either IOException ())
  case result of
    Left err | "used after" `isInfixOf` show err -> pure ()
    _ -> fail "retained font callback did not reject its closed native handle"
