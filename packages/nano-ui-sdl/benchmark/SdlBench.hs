module Main (main) where

import Control.Monad (replicateM_, void, when)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import GHC.Conc (getAllocationCounter)
import NanoUI
import NanoUI.Backend
import NanoUI.Internal.Context (Context (..))
import NanoUI.Testing (newPixelContext, runFrame)
import NanoUI.Backend.Sdl (SdlEnv (..), sdlDrawFrame, syncDisplay, withSdlBench)
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
    , inputButtonsHeld = buttonsFromList [MouseLeft]
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

-- | Warm shaped-line lookups must not allocate: a drawn line's quads are
-- cached per text and returned as the shared cached value, so a steady-state
-- hit is a hash lookup and a pointer return. This gate catches reintroducing
-- a per-draw allocation on the text hot path.
--
-- The probe walks a shared list of lines so selecting one allocates nothing.
glyphLookupAlloc :: Context -> IO Integer
glyphLookupAlloc ctx = do
  (fm, _) <- ctxResolveFont ctx 16 WeightNormal FontStyleNormal FontRegular
  let sample = ["The quick brown fox", "jumps over", "the lazy dog", "0123456789!?.,;:"]
      len = length sample
      lookups = 20000 :: Int
      step :: Int -> Int -> IO Int
      step !n !acc =
        if n <= 0
          then pure acc
          else
            -- Force selection before the indirect call; otherwise the
            -- benchmark allocates a selection thunk per lookup.
            let !txt = sample !! (n `mod` len)
             in drawShaped fm txt >>= \case
                  Just _ -> step (n - 1) (acc + 1)
                  Nothing -> step (n - 1) acc
  -- Warm every line so every lookup hits the cache.
  mapM_ (drawShaped fm) sample
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
    putStrLn "FAIL: warm shaped lookups allocate; expected the cached quads to be shared"
    exitFailure

main :: IO ()
main = do
  configureBenchIO
  ctx0 <- newPixelContext
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
