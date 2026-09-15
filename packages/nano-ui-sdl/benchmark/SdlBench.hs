{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (replicateM_, void, when)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import GHC.Conc (getAllocationCounter)
import NanoUI
import NanoUI.Context (Context (..))
import NanoUI.Testing (runFrame)
import NanoUI.Backend.Sdl (SdlEnv (..), newSdlContext, sdlDrawFrame, syncDisplay, withSdlBench)
import System.Exit (exitFailure)
import System.IO (hSetEncoding, stderr, stdout)
import System.Mem (performGC)
import Test.Tasty.Bench
import Text.Printf (printf)
#if defined(mingw32_HOST_OS)
import System.Win32 (setConsoleCP, setConsoleOutputCP)
#endif

benchWindowSize :: Size
benchWindowSize = Size 800 600

benchInput :: Input
benchInput =
  emptyInput
    { inputWindowSize = benchWindowSize
    , inputMousePos = V2 400 300
    , inputMouseDown = True
    }

smallUi, mediumUi, largeUi :: NanoUI ()
smallUi =
  columnWith (gap 8) $ do
    void (button "OK")
    label "Hello"

mediumUi =
  columnWith
    (grow . gap 8)
    ( do
        replicateM_ 12 $
          gridWith 8 (gap 8) $
            replicateM_ 8 (void (button "OK"))
        label "nano-ui SDL bench"
    )

largeUi =
  columnWith
    (grow . gap 6)
    ( do
        replicateM_ 20 $
          gridWith 10 (gap 6) $
            replicateM_ 10 (void (button "Item"))
        replicateM_ 8 (label "Status line with a bit of text")
    )

configureBenchIO :: IO ()
configureBenchIO = do
  setLocaleEncoding utf8
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
#if defined(mingw32_HOST_OS)
  void $ setConsoleCP 65001
  void $ setConsoleOutputCP 65001
#endif

-- | Warm ASCII glyph lookups must not allocate: the atlas UV/bearing record
-- is cached and shared per font, so a steady-state 'fmGlyph' hit is array
-- reads and a pointer return. This gate catches reintroducing a
-- per-character 'GlyphQuad' / 'Just' allocation on the text hot path.
--
-- The probe walks a shared 'Char' list rather than 'T.index', because
-- 'T.index' allocates in this context and would mask the lookup cost.
glyphLookupAlloc :: Context -> IO Integer
glyphLookupAlloc ctx = do
  (fm, _) <- ctxResolveFont ctx 16 WeightNormal FontStyleNormal FontRegular
  let sample = "The quick brown fox jumps over the lazy dog 0123456789!?.,;:"
      chars = sample
      len = length chars
      lookups = 20000 :: Int
      step :: Int -> Float -> IO Float
      step !n !acc =
        if n <= 0
           then pure acc
          else
            -- Force selection before the indirect glyph call; otherwise the
            -- benchmark allocates a character-selection thunk per lookup.
            let !c = chars !! (n `mod` len)
              in drawGlyph fm c >>= \case
                  Just gq -> step (n - 1) (acc + gqW gq)
                  Nothing -> step (n - 1) acc
  -- Warm every character so every lookup shares a cached 'Maybe'.
  mapM_ (drawGlyph fm) chars
  performGC
  -- The thread allocation counter is current even if this probe never fills
  -- the nursery. RTSStats.allocated_bytes only catches up at a GC.
  before <- getAllocationCounter
  _ <- step lookups 0
  after <- getAllocationCounter
  pure (fromIntegral before - fromIntegral after)

-- | Bytes per warm lookup tolerated before the gate trips. The cached path
-- should be zero; a reintroduced per-hit record would cost tens of bytes.
glyphLookupAllocBudget :: Double
glyphLookupAllocBudget = 1.0

glyphLookupGate :: Context -> IO ()
glyphLookupGate ctx = do
  bytes <- glyphLookupAlloc ctx
  let lookups = 20000 :: Int
      perLookup = fromIntegral bytes / fromIntegral lookups :: Double
  printf "glyph-lookup: %.3f B/lookup (budget %.1f)\n" perLookup glyphLookupAllocBudget
  when (perLookup > glyphLookupAllocBudget) $ do
    putStrLn "FAIL: warm glyph lookups allocate; expected the cached quad to be shared"
    exitFailure

main :: IO ()
main = do
  configureBenchIO
  ctx0 <- newSdlContext
  withSdlBench ctx0 $ \ctx sdlEnv -> do
    (ctx', inp) <- syncDisplay ctx sdlEnv benchInput
    warmup ctx' sdlEnv inp
    glyphLookupGate ctx'
    configureBenchIO
    defaultMain
      [ bgroup
          "ui/runFrame"
          [ benchRunFrame ctx' inp smallUi "small"
          , benchRunFrame ctx' inp mediumUi "medium"
          , benchRunFrame ctx' inp largeUi "large"
          ]
      , bgroup
          "sdl3/draw"
          [ benchDraw ctx' sdlEnv inp smallUi "small"
          , benchDraw ctx' sdlEnv inp mediumUi "medium"
          , benchDraw ctx' sdlEnv inp largeUi "large"
          ]
      ]

warmup :: Context -> SdlEnv -> Input -> IO ()
warmup ctx sdlEnv inp = do
  void (runFrame ctx inp mediumUi)
  void (sdlDrawFrame ctx mediumUi sdlEnv inp False)
  void (runFrame ctx inp mediumUi)
  void (sdlDrawFrame ctx mediumUi sdlEnv inp False)

benchRunFrame :: Context -> Input -> NanoUI () -> String -> Benchmark
benchRunFrame ctx inp ui name =
  bench name $ whnfIO (void . runFrame ctx inp $ ui)

benchDraw :: Context -> SdlEnv -> Input -> NanoUI () -> String -> Benchmark
benchDraw ctx sdlEnv inp ui name =
  bench name $ whnfIO (void . sdlDrawFrame ctx ui sdlEnv inp $ False)
