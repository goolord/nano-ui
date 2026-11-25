-- | Shared fallback-font coverage probing, generic CSS family expansion, and
-- font-cache reload synchronization for SDL3 and WebGL2 backends.
module NanoUI.Ttf.Internal.Support
  ( -- * Family normalization and generic expansion
    normalizeFamily
  , expandGenericFamily
    -- * Fallback glyph coverage
  , CoveragePool
  , newCoveragePool
  , ensurePoolCoverage
  , closeCoveragePool
    -- * Cache reload
  , syncSdlFontCache
  ) where

import Control.Monad (forM_, unless, when)
import Data.Char (isPrint, isSpace, ord, toLower)
import Data.Foldable (traverse_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.IntMap.Strict qualified as IM
import Data.Primitive.SmallArray (SmallArray, indexSmallArray, sizeofSmallArray, smallArrayFromList)
import Data.Text (Text)
import Data.Text qualified as T
import Foreign.Ptr (Ptr, nullPtr)
import NanoUI.Backend (Context, clearMeasureCache, damageFull, markDirty)
import NanoUI.Ttf.Internal.Font
  ( FontSource
  , SdlFont (..)
  , SdlFontCache
  , attachFallback
  , reloadSdlFontCache
  , ttfCloseFont
  , ttfHasGlyph
  , withSdlFontCache
  )

-- | Fold case and drop whitespace, hyphens, and underscores so family and
-- file names can be matched loosely (@"DejaVu Sans Mono"@ vs @DejaVuSansMono@).
normalizeFamily :: String -> String
normalizeFamily =
  map toLower
    . filter (\c -> not (isSpace c) && c /= '-' && c /= '_')

-- | Expand a normalized generic CSS family (@sansserif@, @serif@, @monospace@)
-- into concrete families to search in preference order.
expandGenericFamily :: String -> Maybe [String]
expandGenericFamily = \case
  "monospace" ->
    Just
      [ "DejaVu Sans Mono"
      , "Liberation Mono"
      , "Ubuntu Mono"
      , "Noto Sans Mono"
      , "Consolas"
      , "Courier New"
      ]
  "sansserif" ->
    Just
      [ "DejaVu Sans"
      , "Liberation Sans"
      , "Noto Sans"
      , "Open Sans"
      , "Helvetica Neue"
      ]
  "serif" ->
    Just
      [ "DejaVu Serif"
      , "Liberation Serif"
      , "Noto Serif"
      , "Times New Roman"
      ]
  _ -> Nothing

data PoolState src = PoolState
  { psSources :: !(SmallArray src)
  , psProbes :: !(IM.IntMap (Ptr ()))
  , psChars :: !(IM.IntMap (Maybe Int))
  }

-- | Lazy fallback probe cache mapping non-ASCII codepoints to the first
-- fallback source carrying a glyph for them.
data CoveragePool src = CoveragePool
  { cpLoadSources :: !(IO [src])
  , cpOpenProbe :: !(src -> IO (Ptr ()))
  , cpState :: !(IORef (Maybe (PoolState src)))
  }

-- | Create a fallback coverage pool backed by a lazy source loader and an
-- opener returning a @TTF_Font*@ probe (or 'nullPtr' on failure).
newCoveragePool :: IO [src] -> (src -> IO (Ptr ())) -> IO (CoveragePool src)
newCoveragePool loadSources openProbe =
  CoveragePool loadSources openProbe <$> newIORef Nothing

-- | Attach fallback fonts from @pool@ to @sf@ for any printable non-ASCII
-- characters in @txt@ that @sf@ lacks.
ensurePoolCoverage :: CoveragePool src -> SdlFont -> Text -> IO ()
ensurePoolCoverage pool sf txt =
  unless (T.isAscii txt) $
    forM_ (T.unpack txt) $ \c ->
      when (ord c >= 128 && isPrint c) $ do
        has <- ttfHasGlyph (sfFont sf) (fromIntegral (ord c))
        unless has $ probeForCodepoint pool (ord c) >>= traverse_ (uncurry (attachFallback sf))

probeForCodepoint :: CoveragePool src -> Int -> IO (Maybe (Int, Ptr ()))
probeForCodepoint pool cp = do
  st <- readIORef (cpState pool) >>= maybe loadState pure
  let sources = psSources st
      n = sizeofSmallArray sources
      search probes i
        | i >= n = pure (Nothing, probes)
        | otherwise = do
            probe <- maybe (cpOpenProbe pool (indexSmallArray sources i)) pure (IM.lookup i probes)
            has <- if probe == nullPtr then pure False else ttfHasGlyph probe (fromIntegral cp)
            let probes' = IM.insert i probe probes
            if has then pure (Just i, probes') else search probes' (i + 1)
  (source, probes) <- case IM.lookup cp (psChars st) of
    Just known -> pure (known, psProbes st)
    Nothing -> do
      found@(first, opened) <- search (psProbes st) 0
      found <$ writeIORef (cpState pool) (Just st {psProbes = opened, psChars = IM.insert cp first (psChars st)})
  pure (source >>= \i -> (i,) <$> IM.lookup i probes)
  where
    loadState = do
      srcs <- cpLoadSources pool
      let st = PoolState (smallArrayFromList srcs) IM.empty IM.empty
      st <$ writeIORef (cpState pool) (Just st)

-- | Close all opened probe faces and clear the codepoint map (before @TTF_Quit@).
closeCoveragePool :: CoveragePool src -> IO ()
closeCoveragePool pool =
  readIORef (cpState pool) >>= \case
    Nothing -> pure ()
    Just st -> do
      forM_ (psProbes st) $ \probe -> unless (probe == nullPtr) (ttfCloseFont probe)
      writeIORef (cpState pool) (Just st {psProbes = IM.empty, psChars = IM.empty})

-- | Reload @cache@ with @source@, install the updated font resolver onto @ctx@
-- (passing it through @decorate@ for backend hooks such as clipboard), clear
-- measurement caches, and queue a full repaint.
syncSdlFontCache :: SdlFontCache -> Context -> FontSource -> (Context -> Context) -> IO Context
syncSdlFontCache cache ctx source decorate = do
  reloadSdlFontCache cache source
  ctx' <- decorate <$> withSdlFontCache cache ctx
  clearMeasureCache ctx
  damageFull ctx
  markDirty ctx
  pure ctx'
