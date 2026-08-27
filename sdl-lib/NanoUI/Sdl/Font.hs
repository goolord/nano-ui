module NanoUI.Sdl.Font
  ( SdlFont (..)
  , TextCache
  , withTtf
  , openFont
  , closeFont
  , findFontPath
  , findMonoFontPath
  , newTextCache
  , destroyTextCache
  , withTtfMeasure
  , withTtfMeasureScaled
  , ttfFontMetricsScaled
  , measureTtfText
  , renderTextSpans
  ) where

import Control.Exception (bracket)
import Control.Monad (forM_, void, when)
import Data.Bits (shiftR, (.&.))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32, Word8)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CFloat (..), CSize (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import NanoUI (Color (..), Context, FontMetrics (..), Rect (..), hasMonoFontMarker, monospaceMetrics, stripMonoFontMarker, withExternalText, withFontMetrics, withMeasureText, withMonoFontMetrics, wrapMeasureCache)
import NanoUI.Sdl.Render (clearLogicalClipRect, logicalClipKey, setLogicalClipRect)
import SDL3.Sys.Bindgen.Render (SDL_Renderer)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as TE

data SdlFont = SdlFont
  { sfFont :: Ptr ()
  , sfLineSkip :: Float
  , sfAscent :: Float
  , sfSpaceAdvance :: Float
  , sfPath :: FilePath
  }

data TextCache = TextCache
  { tcEntries :: IORef (Map.Map CacheKey (Ptr (), Float, Float))
  , tcOrder :: IORef [CacheKey]
  }

type CacheKey = (Ptr (), Text, Word32)

textCacheLimit :: Int
textCacheLimit = 256

newTextCache :: IO TextCache
newTextCache = do
  entries <- newIORef Map.empty
  order <- newIORef []
  pure TextCache {tcEntries = entries, tcOrder = order}

destroyTextCache :: TextCache -> IO ()
destroyTextCache cache = do
  entries <- readIORef (tcEntries cache)
  forM_ (Map.elems entries) $ \(tex, _, _) ->
    destroyTexture tex
  writeIORef (tcEntries cache) Map.empty
  writeIORef (tcOrder cache) []

withTtf :: IO a -> IO a
withTtf act =
  bracket startup shutdown $ \_ -> act
  where
    startup = do
      ok <- ttfInit
      when (not ok) $ fail "TTF_Init failed"
    shutdown _ = ttfQuit

openFont :: FilePath -> Float -> IO SdlFont
openFont path ptsize =
  withCString path $ \cpath -> do
    font <- ttfOpenFont cpath (realToFrac ptsize)
    when (font == nullPtr) $
      fail ("TTF_OpenFont failed for " ++ path)
    lineSkip <- ttfLineSkip font
    ascent <- ttfAscent font
    spaceAdv <- ttfSpaceAdvance font
    pure
      SdlFont
        { sfFont = font
        , sfLineSkip = realToFrac lineSkip
        , sfAscent = realToFrac ascent
        , sfSpaceAdvance = realToFrac spaceAdv
        , sfPath = path
        }

closeFont :: SdlFont -> IO ()
closeFont = ttfCloseFont . sfFont

withTtfMeasure :: Context -> SdlFont -> SdlFont -> Context
withTtfMeasure ctx sf monoSf = withTtfMeasureScaled ctx sf monoSf 1

withTtfMeasureScaled :: Context -> SdlFont -> SdlFont -> Float -> Context
withTtfMeasureScaled ctx sf monoSf scale =
  let fm = ttfFontMetricsScaled sf scale
      monoFm = ttfFontMetricsScaled monoSf scale
      measure txt =
        if hasMonoFontMarker txt
          then measureTtfTextScaled monoSf scale (stripMonoFontMarker txt)
          else measureTtfTextScaled sf scale txt
      ctx1 =
        withExternalText
          ( withMeasureText
              (withMonoFontMetrics (withFontMetrics ctx fm) monoFm)
              measure
          )
          True
   in -- Per-frame measure cache; SDL contexts only (see newSdlContext).
      wrapMeasureCache scale ctx1 measure

ttfFontMetricsScaled :: SdlFont -> Float -> FontMetrics
ttfFontMetricsScaled sf scale =
  let inv = if scale > 0 then scale else 1
   in (monospaceMetrics (sfLineSkip sf / inv))
        { fmAscent = sfAscent sf / inv
        , fmAdvance = const (sfSpaceAdvance sf / inv)
        }

measureTtfTextScaled :: SdlFont -> Float -> Text -> IO (Float, Float)
measureTtfTextScaled sf scale txt = do
  (w, h) <- measureTtfText sf txt
  let inv = if scale > 0 then scale else 1
  pure (w / inv, h / inv)

measureTtfText :: SdlFont -> Text -> IO (Float, Float)
measureTtfText sf txt =
  withUtf8 txt $ \cstr len ->
    alloca $ \wp ->
      alloca $ \hp -> do
        ok <- ttfStringSize (sfFont sf) cstr len wp hp
        if ok
          then do
            w <- peek wp
            h <- peek hp
            pure (realToFrac w, realToFrac h)
          else pure (0, sfLineSkip sf)

renderTextSpans :: Ptr SDL_Renderer -> Float -> SdlFont -> SdlFont -> TextCache -> [(Rect, Text, Color, Color, Rect)] -> IO ()
renderTextSpans ren scale font monoFont cache spans = do
  lastClip <- newIORef (Nothing :: Maybe (Int, Int, Int, Int))
  forM_ spans $ \(Rect x y _ _, txt, fg, _bg, clip) -> do
    let clipKey = logicalClipKey scale clip
    prev <- readIORef lastClip
    when (prev /= Just clipKey) $ do
      writeIORef lastClip (Just clipKey)
      setLogicalClipRect ren scale clip
    drawSpan ren scale font monoFont cache txt fg x y
  clearLogicalClipRect ren

drawSpan :: Ptr SDL_Renderer -> Float -> SdlFont -> SdlFont -> TextCache -> Text -> Color -> Float -> Float -> IO ()
drawSpan _ _ _ _ _ txt _ _ _
  | T.null txt = pure ()
drawSpan ren scale font monoFont cache txt col x y =
  let (pick, shown) =
        if hasMonoFontMarker txt
          then (monoFont, stripMonoFontMarker txt)
          else (font, txt)
   in drawSpanFont ren scale pick cache shown col x y

drawSpanFont :: Ptr SDL_Renderer -> Float -> SdlFont -> TextCache -> Text -> Color -> Float -> Float -> IO ()
drawSpanFont _ _ _ _ txt _ _ _
  | T.null txt = pure ()
drawSpanFont ren scale font cache txt col x y =
  withUtf8 txt $ \cstr len -> do
    let keyCol = colorWord col
        cacheKey = (sfFont font, txt, keyCol)
    (tex, tw, th) <-
      lookupCache cache cacheKey >>= \case
        Just hit -> pure hit
        Nothing -> createCached ren font cache cacheKey cstr len col
    let px = x * scale
        py = y * scale
    drawTexture ren tex tw th px py

createCached ::
  Ptr SDL_Renderer ->
  SdlFont ->
  TextCache ->
  CacheKey ->
  CString ->
  CSize ->
  Color ->
  IO (Ptr (), Float, Float)
createCached ren font cache cacheKey cstr len col =
  alloca $ \(outTex :: Ptr (Ptr ())) ->
    alloca $ \outW ->
      alloca $ \outH -> do
        ok <-
          ttfCreateTexture
            ren
            (sfFont font)
            cstr
            len
            r
            g
            b
            a
            outTex
            outW
            outH
        when (not ok) $ fail "TTF render failed"
        tex <- peek outTex
        tw <- realToFrac <$> peek outW
        th <- realToFrac <$> peek outH
        insertCache cache cacheKey (tex, tw, th)
        pure (tex, tw, th)
  where
    (r, g, b, a) = unpackColor col

insertCache :: TextCache -> CacheKey -> (Ptr (), Float, Float) -> IO ()
insertCache cache key val = do
  let entriesRef = tcEntries cache
      orderRef = tcOrder cache
  mOld <- Map.lookup key <$> readIORef entriesRef
  case mOld of
    Just (tex, _, _) -> destroyTexture tex
    Nothing -> evictUntilRoom cache
  modifyIORef entriesRef (Map.insert key val)
  modifyIORef orderRef (\ord -> key : filter (/= key) ord)

evictUntilRoom :: TextCache -> IO ()
evictUntilRoom cache = do
  sz <- Map.size <$> readIORef (tcEntries cache)
  when (sz >= textCacheLimit) $ do
    evictOldest cache
    evictUntilRoom cache

evictOldest :: TextCache -> IO ()
evictOldest cache = do
  ord <- readIORef (tcOrder cache)
  case reverse ord of
    [] -> pure ()
    (oldest : _) -> do
      entries <- readIORef (tcEntries cache)
      case Map.lookup oldest entries of
        Nothing -> writeIORef (tcOrder cache) (filter (/= oldest) ord)
        Just (tex, _, _) -> do
          destroyTexture tex
          writeIORef (tcEntries cache) (Map.delete oldest entries)
          writeIORef (tcOrder cache) (filter (/= oldest) ord)

drawTexture :: Ptr SDL_Renderer -> Ptr () -> Float -> Float -> Float -> Float -> IO ()
drawTexture ren tex tw th x y =
  void $ renderTextureSized ren tex (cf x) (cf y) (cf tw) (cf th)

lookupCache :: TextCache -> CacheKey -> IO (Maybe (Ptr (), Float, Float))
lookupCache cache key = do
  entries <- readIORef (tcEntries cache)
  case Map.lookup key entries of
    Nothing -> pure Nothing
    Just hit -> do
      modifyIORef (tcOrder cache) (\ord -> key : filter (/= key) ord)
      pure (Just hit)

findMonoFontPath :: IO (Maybe FilePath)
findMonoFontPath = do
  envPath <- lookupEnv "NANO_UI_MONO_FONT"
  case envPath of
    Just p -> exists p
    Nothing -> firstExisting monoFontCandidates
  where
    exists p = do
      ok <- doesFileExist p
      pure (if ok then Just p else Nothing)
    firstExisting [] = pure Nothing
    firstExisting (p : ps) = do
      ok <- doesFileExist p
      if ok then pure (Just p) else firstExisting ps

monoFontCandidates :: [FilePath]
monoFontCandidates =
  [ "C:\\Windows\\Fonts\\consola.ttf"
  , "C:\\Windows\\Fonts\\Consolas.ttf"
  , "C:\\Windows\\Fonts\\CascadiaMono.ttf"
  , "C:\\Windows\\Fonts\\lucon.ttf"
  , "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf"
  , "/usr/share/fonts/TTF/DejaVuSansMono.ttf"
  , "/usr/share/fonts/truetype/liberation/LiberationMono-Regular.ttf"
  , "/System/Library/Fonts/Menlo.ttc"
  , "/System/Library/Fonts/Supplemental/Courier New.ttf"
  , "C:\\msys64\\ucrt64\\share\\fonts\\TTF\\DejaVuSansMono.ttf"
  ]

findFontPath :: IO (Maybe FilePath)
findFontPath = do
  envPath <- lookupEnv "NANO_UI_FONT"
  case envPath of
    Just p -> exists p
    Nothing -> firstExisting fontCandidates
  where
    exists p = do
      ok <- doesFileExist p
      pure (if ok then Just p else Nothing)
    firstExisting [] = pure Nothing
    firstExisting (p : ps) = do
      ok <- doesFileExist p
      if ok then pure (Just p) else firstExisting ps

fontCandidates :: [FilePath]
fontCandidates =
  [ "/usr/share/fonts/adwaita-sans/AdwaitaSans-Regular.ttf"
  , "/usr/share/fonts/truetype/adwaita/AdwaitaSans-Regular.ttf"
  , "/usr/share/fonts/cantarell/Cantarell-Regular.otf"
  , "/usr/share/fonts/truetype/cantarell/Cantarell-Regular.otf"
  , "/usr/share/fonts/truetype/cantarell/Cantarell-Regular.ttf"
  , "/usr/share/fonts/inter/Inter-Regular.ttf"
  , "/usr/share/fonts/truetype/inter/Inter-Regular.ttf"
  , "C:\\Windows\\Fonts\\segoeui.ttf"
  , "C:\\Windows\\Fonts\\SegoeUI.ttf"
  , "C:\\Windows\\Fonts\\segoeuib.ttf"
  , "C:\\Windows\\Fonts\\arial.ttf"
  , "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"
  , "/usr/share/fonts/TTF/DejaVuSans.ttf"
  , "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf"
  , "/System/Library/Fonts/Supplemental/Arial.ttf"
  , "/System/Library/Fonts/Supplemental/Helvetica.ttf"
  , "C:\\msys64\\ucrt64\\share\\fonts\\TTF\\DejaVuSans.ttf"
  ]

withUtf8 :: Text -> (CString -> CSize -> IO a) -> IO a
withUtf8 txt k =
  let bytes = TE.encodeUtf8 txt
   in BS.useAsCStringLen bytes $ \(cstr, len) -> k cstr (fromIntegral len)

modifyIORef :: IORef a -> (a -> a) -> IO ()
modifyIORef ref f = do
  v <- readIORef ref
  writeIORef ref (f v)

colorWord :: Color -> Word32
colorWord (Color w) = w

cf :: Float -> CFloat
cf = realToFrac

foreign import ccall safe "nano_ui_ttf_init"
  ttfInit :: IO Bool

foreign import ccall safe "nano_ui_ttf_quit"
  ttfQuit :: IO ()

foreign import ccall safe "nano_ui_ttf_open_font"
  ttfOpenFont :: CString -> CFloat -> IO (Ptr ())

foreign import ccall safe "nano_ui_ttf_close_font"
  ttfCloseFont :: Ptr () -> IO ()

foreign import ccall safe "nano_ui_ttf_line_skip"
  ttfLineSkip :: Ptr () -> IO CFloat

foreign import ccall safe "nano_ui_ttf_ascent"
  ttfAscent :: Ptr () -> IO CFloat

foreign import ccall safe "nano_ui_ttf_space_advance"
  ttfSpaceAdvance :: Ptr () -> IO CFloat

foreign import ccall safe "nano_ui_ttf_string_size"
  ttfStringSize :: Ptr () -> CString -> CSize -> Ptr CFloat -> Ptr CFloat -> IO Bool

foreign import ccall safe "nano_ui_ttf_create_texture"
  ttfCreateTexture ::
    Ptr SDL_Renderer ->
    Ptr () ->
    CString ->
    CSize ->
    Word8 ->
    Word8 ->
    Word8 ->
    Word8 ->
    Ptr (Ptr ()) ->
    Ptr CFloat ->
    Ptr CFloat ->
    IO Bool

foreign import ccall unsafe "nano_ui_render_texture_sized"
  renderTextureSized ::
    Ptr SDL_Renderer ->
    Ptr () ->
    CFloat ->
    CFloat ->
    CFloat ->
    CFloat ->
    IO Bool

foreign import ccall unsafe "nano_ui_destroy_texture"
  destroyTexture :: Ptr () -> IO ()

unpackColor :: Color -> (Word8, Word8, Word8, Word8)
unpackColor (Color w) =
  ( fromIntegral $ (w `shiftR` 24) .&. 0xFF
  , fromIntegral $ (w `shiftR` 16) .&. 0xFF
  , fromIntegral $ (w `shiftR` 8) .&. 0xFF
  , fromIntegral $ w .&. 0xFF
  )
