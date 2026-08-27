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
import Control.Monad (forM_, when)
import Data.Bits (shiftR, (.&.))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32, Word8)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CFloat (..), CSize (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke)
import NanoUI (Color (..), Context, FontMetrics (..), Rect (..), hasMonoFontMarker, monospaceMetrics, stripMonoFontMarker, withExternalText, withFontMetrics, withMeasureText, withMonoFontMetrics, wrapMeasureCache)
import NanoUI.Sdl.Batch (RenderBatch, batchTextureDst, flushRenderBatch)
import NanoUI.Sdl.Render (logicalClipKey, setLogicalClipRect)
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

data TextSlot = TextSlot
  { tsU0 :: Float
  , tsV0 :: Float
  , tsU1 :: Float
  , tsV1 :: Float
  , tsW :: Float
  , tsH :: Float
  }

data TextCache = TextCache
  { tcAtlas :: Ptr ()
  , tcEntries :: IORef (Map.Map CacheKey TextSlot)
  , tcOrder :: IORef [CacheKey]
  }

type CacheKey = (Ptr (), Text, Word32)

textCacheLimit :: Int
textCacheLimit = 256

newTextCache :: Ptr SDL_Renderer -> IO TextCache
newTextCache ren = do
  atlas <- textAtlasCreate ren
  when (atlas == nullPtr) $ fail "nano_ui_text_atlas_create failed"
  entries <- newIORef Map.empty
  order <- newIORef []
  pure TextCache {tcAtlas = atlas, tcEntries = entries, tcOrder = order}

destroyTextCache :: TextCache -> IO ()
destroyTextCache cache = textAtlasDestroy (tcAtlas cache)

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
   in wrapMeasureCache scale ctx1 measure

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

renderTextSpans ::
  RenderBatch ->
  Ptr SDL_Renderer ->
  Float ->
  SdlFont ->
  SdlFont ->
  TextCache ->
  [(Rect, Text, Color, Color, Rect)] ->
  IO ()
renderTextSpans batch ren scale font monoFont cache spans = do
  lastClip <- newIORef (Nothing :: Maybe (Int, Int, Int, Int))
  forM_ spans $ \(Rect x y _ _, txt, fg, _bg, clip) -> do
    let clipKey = logicalClipKey scale clip
    prev <- readIORef lastClip
    when (prev /= Just clipKey) $ do
      flushRenderBatch batch
      writeIORef lastClip (Just clipKey)
      setLogicalClipRect ren scale clip
    drawSpan batch scale font monoFont cache txt fg x y
  flushRenderBatch batch

drawSpan :: RenderBatch -> Float -> SdlFont -> SdlFont -> TextCache -> Text -> Color -> Float -> Float -> IO ()
drawSpan _ _ _ _ _ txt _ _ _
  | T.null txt = pure ()
drawSpan batch scale font monoFont cache txt col x y =
  let (pick, shown) =
        if hasMonoFontMarker txt
          then (monoFont, stripMonoFontMarker txt)
          else (font, txt)
   in drawSpanFont batch scale pick cache shown col x y

drawSpanFont :: RenderBatch -> Float -> SdlFont -> TextCache -> Text -> Color -> Float -> Float -> IO ()
drawSpanFont _ _ _ _ txt _ _ _
  | T.null txt = pure ()
drawSpanFont batch scale font cache txt col x y =
  withUtf8 txt $ \cstr len -> do
    let keyCol = colorWord col
        cacheKey = (sfFont font, txt, keyCol)
    slot <-
      lookupCache cache cacheKey >>= \case
        Just hit -> pure hit
        Nothing -> createCached font cache cacheKey cstr len col
    (atW, atH) <- atlasSize (tcAtlas cache)
    tex <- textAtlasTexture (tcAtlas cache)
    let px = x * scale
        py = y * scale
    batchTextureDst
      batch
      tex
      atW
      atH
      px
      py
      (tsW slot)
      (tsH slot)
      (tsU0 slot)
      (tsV0 slot)
      (tsU1 slot)
      (tsV1 slot)
      255
      255
      255
      255

createCached ::
  SdlFont ->
  TextCache ->
  CacheKey ->
  CString ->
  CSize ->
  Color ->
  IO TextSlot
createCached font cache cacheKey cstr len col =
  alloca $ \(surfPtr :: Ptr (Ptr ())) -> do
    poke surfPtr nullPtr
    ok <-
      ttfRenderSurface
        (sfFont font)
        cstr
        len
        r
        g
        b
        a
        surfPtr
        nullPtr
        nullPtr
    when (not ok) $ fail "TTF render failed"
    surf <- peek surfPtr
    slot <-
      alloca $ \u0 ->
        alloca $ \v0 ->
          alloca $ \u1 ->
            alloca $ \v1 ->
              alloca $ \tw ->
                alloca $ \th -> do
                  ok2 <-
                    textAtlasInsertSurface
                      (tcAtlas cache)
                      surf
                      u0
                      v0
                      u1
                      v1
                      tw
                      th
                  when (not ok2) $ fail "text atlas insert failed"
                  freeSurface surf
                  TextSlot
                    <$> (realToFrac <$> peek u0)
                    <*> (realToFrac <$> peek v0)
                    <*> (realToFrac <$> peek u1)
                    <*> (realToFrac <$> peek v1)
                    <*> (realToFrac <$> peek tw)
                    <*> (realToFrac <$> peek th)
    insertCache cache cacheKey slot
    pure slot
  where
    (r, g, b, a) = unpackColor col

insertCache :: TextCache -> CacheKey -> TextSlot -> IO ()
insertCache cache key val = do
  let entriesRef = tcEntries cache
      orderRef = tcOrder cache
  mOld <- Map.lookup key <$> readIORef entriesRef
  case mOld of
    Just _ -> pure ()
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
        Just _ -> do
          writeIORef (tcEntries cache) (Map.delete oldest entries)
          writeIORef (tcOrder cache) (filter (/= oldest) ord)

lookupCache :: TextCache -> CacheKey -> IO (Maybe TextSlot)
lookupCache cache key = do
  entries <- readIORef (tcEntries cache)
  case Map.lookup key entries of
    Nothing -> pure Nothing
    Just hit -> do
      modifyIORef (tcOrder cache) (\ord -> key : filter (/= key) ord)
      pure (Just hit)

atlasSize :: Ptr () -> IO (Float, Float)
atlasSize atlas =
  alloca $ \w ->
    alloca $ \h -> do
      ok <- textAtlasSize atlas w h
      when (not ok) $ fail "text atlas size failed"
      (,) <$> (realToFrac <$> peek w) <*> (realToFrac <$> peek h)

pathExists :: FilePath -> IO (Maybe FilePath)
pathExists p = do
  ok <- doesFileExist p
  pure (if ok then Just p else Nothing)

firstExistingPath :: [FilePath] -> IO (Maybe FilePath)
firstExistingPath [] = pure Nothing
firstExistingPath (p : ps) = do
  ok <- doesFileExist p
  if ok then pure (Just p) else firstExistingPath ps

findMonoFontPath :: IO (Maybe FilePath)
findMonoFontPath = do
  envPath <- lookupEnv "NANO_UI_MONO_FONT"
  case envPath of
    Just p -> pathExists p
    Nothing -> firstExistingPath monoFontCandidates

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
    Just p -> pathExists p
    Nothing -> firstExistingPath fontCandidates

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

foreign import ccall unsafe "nano_ui_ttf_render_surface"
  ttfRenderSurface ::
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

foreign import ccall unsafe "nano_ui_text_atlas_create"
  textAtlasCreate :: Ptr SDL_Renderer -> IO (Ptr ())

foreign import ccall unsafe "nano_ui_text_atlas_destroy"
  textAtlasDestroy :: Ptr () -> IO ()

foreign import ccall unsafe "nano_ui_text_atlas_texture"
  textAtlasTexture :: Ptr () -> IO (Ptr ())

foreign import ccall unsafe "nano_ui_text_atlas_size"
  textAtlasSize :: Ptr () -> Ptr CFloat -> Ptr CFloat -> IO Bool

foreign import ccall unsafe "nano_ui_text_atlas_insert_surface"
  textAtlasInsertSurface ::
    Ptr () ->
    Ptr () ->
    Ptr CFloat ->
    Ptr CFloat ->
    Ptr CFloat ->
    Ptr CFloat ->
    Ptr CFloat ->
    Ptr CFloat ->
    IO Bool

foreign import ccall unsafe "nano_ui_free_surface"
  freeSurface :: Ptr () -> IO ()

unpackColor :: Color -> (Word8, Word8, Word8, Word8)
unpackColor (Color w) =
  ( fromIntegral $ (w `shiftR` 24) .&. 0xFF
  , fromIntegral $ (w `shiftR` 16) .&. 0xFF
  , fromIntegral $ (w `shiftR` 8) .&. 0xFF
  , fromIntegral $ w .&. 0xFF
  )
