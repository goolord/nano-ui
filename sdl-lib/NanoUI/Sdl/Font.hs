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

import Control.Exception (IOException, bracket, catch)
import Control.Monad (filterM, forM_, when)
import Data.Bits (shiftR, (.&.))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Primitive.SmallArray
  ( SmallArray
  , emptySmallArray
  , indexSmallArray
  , sizeofSmallArray
  , smallArrayFromList
  )
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32, Word8)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CFloat (..), CSize (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke)
import NanoUI (Color (..), FontMetrics (..), Rect (..), hasMonoFontMarker, monospaceMetrics, stripMonoFontMarker)
import NanoUI.Testing
  ( Context
  , withExternalText
  , withFontMetrics
  , withMeasureText
  , withMonoFontMetrics
  , wrapMeasureCache
  )
import NanoUI.Sdl.Batch (RenderBatch, batchTextureDst, flushRenderBatch)
import NanoUI.Sdl.Render (logicalClipKey, setLogicalClipRect)
import SDL3.Sys.Bindgen.Render (SDL_Renderer)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
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
  { tsW :: {-# UNPACK #-} !Float
  , tsH :: {-# UNPACK #-} !Float
  , tsU0 :: {-# UNPACK #-} !Float
  , tsV0 :: {-# UNPACK #-} !Float
  , tsU1 :: {-# UNPACK #-} !Float
  , tsV1 :: {-# UNPACK #-} !Float
  , tsAtlasW :: {-# UNPACK #-} !Float
  , tsAtlasH :: {-# UNPACK #-} !Float
  , tsTex :: !(Ptr ())
  }

data TextCache = TextCache
  { tcAtlas :: !(Ptr ())
  , tcEntries :: !(IORef (Map.Map CacheKey TextSlot))
  }

type CacheKey = (Ptr (), Text, Word32)

textCacheLimit :: Int
textCacheLimit = 256

newTextCache :: Ptr SDL_Renderer -> IO TextCache
newTextCache ren = do
  atlas <- textAtlasCreate ren
  when (atlas == nullPtr) $ fail "nano_ui_text_atlas_create failed"
  entries <- newIORef Map.empty
  pure TextCache {tcAtlas = atlas, tcEntries = entries}

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
withTtfMeasure ctx font monoFont = withTtfMeasureScaled ctx font monoFont 1.0

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
renderTextSpans _ _ _ _ _ _ [] = pure ()
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

{-# INLINE drawSpanFont #-}
drawSpanFont :: RenderBatch -> Float -> SdlFont -> TextCache -> Text -> Color -> Float -> Float -> IO ()
drawSpanFont _ _ _ _ txt _ _ _
  | T.null txt = pure ()
drawSpanFont batch scale font cache txt col x y = do
  let !keyCol = colorWord col
      !cacheKey = (sfFont font, txt, keyCol)
  mSlot <-
    lookupCache cache cacheKey >>= \case
      Just hit -> pure (Just hit)
      Nothing -> do
        flushRenderBatch batch
        withUtf8 txt $ \cstr len ->
          createCached font cache cacheKey cstr len col
  case mSlot of
    Nothing -> pure ()
    Just (TextSlot aw ah u0 v0 u1 v1 atW atH tex) -> do
      let !px = x * scale
          !py = y * scale
      batchTextureDst
        batch
        tex
        atW
        atH
        px
        py
        aw
        ah
        u0
        v0
        u1
        v1
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
  IO (Maybe TextSlot)
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
    sz <- Map.size <$> readIORef (tcEntries cache)
    when (sz >= textCacheLimit) $ resetTextCache cache
    surf <- peek surfPtr
    mSlot <- insertSurface cache surf
    freeSurface surf
    case mSlot of
      Nothing -> pure Nothing
      Just (px, py, tw, th) -> do
        (atW, atH) <- atlasSize (tcAtlas cache)
        tex <- textAtlasTexture (tcAtlas cache)
        let slot =
              TextSlot
                { tsW = tw
                , tsH = th
                , tsU0 = px / atW
                , tsV0 = py / atH
                , tsU1 = (px + tw) / atW
                , tsV1 = (py + th) / atH
                , tsAtlasW = atW
                , tsAtlasH = atH
                , tsTex = tex
                }
        insertCache cache cacheKey slot
        pure (Just slot)
  where
    (r, g, b, a) = unpackColor col

insertSurface :: TextCache -> Ptr () -> IO (Maybe (Float, Float, Float, Float))
insertSurface cache surf = do
  let atlas = tcAtlas cache
  tryInsert atlas surf >>= \case
    Just slot -> pure (Just slot)
    Nothing -> do
      clearTextCacheEntries cache
      textAtlasReset atlas
      tryInsert atlas surf

tryInsert :: Ptr () -> Ptr () -> IO (Maybe (Float, Float, Float, Float))
tryInsert atlas surf =
  alloca $ \px ->
    alloca $ \py ->
      alloca $ \tw ->
        alloca $ \th -> do
          ok <- textAtlasInsertSurface atlas surf px py tw th
          if ok
            then do
              x <- realToFrac <$> peek px
              y <- realToFrac <$> peek py
              w <- realToFrac <$> peek tw
              h <- realToFrac <$> peek th
              pure (Just (x, y, w, h))
            else pure Nothing

clearTextCacheEntries :: TextCache -> IO ()
clearTextCacheEntries cache =
  writeIORef (tcEntries cache) Map.empty

resetTextCache :: TextCache -> IO ()
resetTextCache cache = do
  clearTextCacheEntries cache
  textAtlasReset (tcAtlas cache)

insertCache :: TextCache -> CacheKey -> TextSlot -> IO ()
insertCache cache key val =
  modifyIORef' (tcEntries cache) (Map.insert key val)

lookupCache :: TextCache -> CacheKey -> IO (Maybe TextSlot)
lookupCache cache key = do
  entries <- readIORef (tcEntries cache)
  pure $! Map.lookup key entries

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

firstExistingPath :: SmallArray FilePath -> IO (Maybe FilePath)
firstExistingPath paths = go 0
  where
    len = sizeofSmallArray paths
    go i
      | i >= len = pure Nothing
      | otherwise = do
          let p = indexSmallArray paths i
          ok <- doesFileExist p
          if ok then pure (Just p) else go (i + 1)

joinDir :: FilePath -> FilePath -> FilePath
joinDir dir name = dir ++ "/" ++ name

-- Distro layouts differ (Arch: /usr/share/fonts/Adwaita, Debian: truetype/).
-- Check known paths first, then look up preferred file names under common roots.
resolveFont :: SmallArray FilePath -> SmallArray FilePath -> Maybe String -> IO (Maybe FilePath)
resolveFont candidates names envPath =
  case envPath of
    Just p -> pathExists p
    Nothing -> do
      hit <- firstExistingPath candidates
      case hit of
        Just p -> pure (Just p)
        Nothing -> findNamedFont names

findMonoFontPath :: IO (Maybe FilePath)
findMonoFontPath = lookupEnv "NANO_UI_MONO_FONT" >>= resolveFont monoFontCandidates monoFontFileNames

findFontPath :: IO (Maybe FilePath)
findFontPath = lookupEnv "NANO_UI_FONT" >>= resolveFont fontCandidates fontFileNames

fontSearchRoots :: IO (SmallArray FilePath)
fontSearchRoots = do
  home <- lookupEnv "HOME"
  profile <- lookupEnv "USERPROFILE"
  let userRoots =
        maybe [] (\h -> [joinDir h ".local/share/fonts", joinDir h ".fonts", joinDir h ".nix-profile/share/fonts"]) home
          ++ maybe [] (\p -> [p ++ "\\AppData\\Local\\Microsoft\\Windows\\Fonts"]) profile
  pure $
    smallArrayFromList
      ( [ "/usr/share/fonts"
        , "/usr/local/share/fonts"
        , "/usr/share/fonts/TTF"
        , "/usr/share/fonts/OTF"
        , "/usr/share/fonts/truetype"
        , "/usr/share/fonts/opentype"
        , "/run/current-system/sw/share/fonts"
        , "C:\\Windows\\Fonts"
        ]
          ++ userRoots
      )

maxFontDirDepth :: Int
maxFontDirDepth = 6

findNamedFont :: SmallArray FilePath -> IO (Maybe FilePath)
findNamedFont names = do
  roots <- fontSearchRoots
  searchNames names roots

searchNames :: SmallArray FilePath -> SmallArray FilePath -> IO (Maybe FilePath)
searchNames names roots = go 0
  where
    len = sizeofSmallArray names
    go i
      | i >= len = pure Nothing
      | otherwise = do
          hit <- findInTree (indexSmallArray names i) roots 0
          case hit of
            Just p -> pure (Just p)
            Nothing -> go (i + 1)

findInTree :: FilePath -> SmallArray FilePath -> Int -> IO (Maybe FilePath)
findInTree _ dirs depth
  | sizeofSmallArray dirs == 0 || depth > maxFontDirDepth = pure Nothing
findInTree name dirs depth = do
  let testPaths = fmap (`joinDir` name) dirs
  hit <- firstExistingPath testPaths
  case hit of
    Just p -> pure (Just p)
    Nothing -> do
      kids <- concatSubdirs dirs
      findInTree name kids (depth + 1)

concatSubdirs :: SmallArray FilePath -> IO (SmallArray FilePath)
concatSubdirs dirs = go 0 emptySmallArray
  where
    len = sizeofSmallArray dirs
    go i acc
      | i >= len = pure acc
      | otherwise = do
          sub <- listSubdirs (indexSmallArray dirs i)
          go (i + 1) (acc <> sub)

listSubdirs :: FilePath -> IO (SmallArray FilePath)
listSubdirs dir =
  ( do
      names <- listDirectory dir
      dirs <- filterM doesDirectoryExist (map (joinDir dir) names)
      pure (smallArrayFromList dirs)
  )
    `catch` \(_ :: IOException) -> pure emptySmallArray

monoFontFileNames :: SmallArray FilePath
monoFontFileNames =
  smallArrayFromList
    [ "AdwaitaMono-Regular.ttf"
    , "DejaVuSansMono.ttf"
    , "LiberationMono-Regular.ttf"
    , "NotoSansMono-Regular.ttf"
    , "consola.ttf"
    , "Consolas.ttf"
    , "CascadiaMono.ttf"
    , "lucon.ttf"
    , "Menlo.ttc"
    , "Courier New.ttf"
    ]

monoFontCandidates :: SmallArray FilePath
monoFontCandidates =
  smallArrayFromList
    [ "/usr/share/fonts/Adwaita/AdwaitaMono-Regular.ttf"
    , "/usr/share/fonts/adwaita-mono/AdwaitaMono-Regular.ttf"
    , "C:\\Windows\\Fonts\\consola.ttf"
    , "C:\\Windows\\Fonts\\Consolas.ttf"
    , "C:\\Windows\\Fonts\\CascadiaMono.ttf"
    , "C:\\Windows\\Fonts\\lucon.ttf"
    , "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf"
    , "/usr/share/fonts/TTF/DejaVuSansMono.ttf"
    , "/usr/share/fonts/liberation/LiberationMono-Regular.ttf"
    , "/usr/share/fonts/truetype/liberation/LiberationMono-Regular.ttf"
    , "/usr/share/fonts/noto/NotoSansMono-Regular.ttf"
    , "/System/Library/Fonts/Menlo.ttc"
    , "/System/Library/Fonts/Supplemental/Courier New.ttf"
    , "C:\\msys64\\ucrt64\\share\\fonts\\TTF\\DejaVuSansMono.ttf"
    ]

fontFileNames :: SmallArray FilePath
fontFileNames =
  smallArrayFromList
    [ "AdwaitaSans-Regular.ttf"
    , "Cantarell-Regular.otf"
    , "Cantarell-Regular.ttf"
    , "Inter-Regular.ttf"
    , "segoeui.ttf"
    , "SegoeUI.ttf"
    , "arial.ttf"
    , "DejaVuSans.ttf"
    , "LiberationSans-Regular.ttf"
    , "NotoSans-Regular.ttf"
    , "FreeSans.otf"
    , "FreeSans.ttf"
    , "Arial.ttf"
    , "Helvetica.ttf"
    ]

fontCandidates :: SmallArray FilePath
fontCandidates =
  smallArrayFromList
    [ "/usr/share/fonts/Adwaita/AdwaitaSans-Regular.ttf"
    , "/usr/share/fonts/adwaita-sans/AdwaitaSans-Regular.ttf"
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
    , "/usr/share/fonts/liberation/LiberationSans-Regular.ttf"
    , "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf"
    , "/usr/share/fonts/noto/NotoSans-Regular.ttf"
    , "/usr/share/fonts/gnu-free/FreeSans.otf"
    , "/System/Library/Fonts/Supplemental/Arial.ttf"
    , "/System/Library/Fonts/Supplemental/Helvetica.ttf"
    , "C:\\msys64\\ucrt64\\share\\fonts\\TTF\\DejaVuSans.ttf"
    ]

withUtf8 :: Text -> (CString -> CSize -> IO a) -> IO a
withUtf8 txt k =
  let bytes = TE.encodeUtf8 txt
   in BS.useAsCStringLen bytes $ \(cstr, len) -> k cstr (fromIntegral len)

colorWord :: Color -> Word32
colorWord (Color w) = w

{-# INLINE unpackColor #-}
unpackColor :: Color -> (Word8, Word8, Word8, Word8)
unpackColor (Color w) =
  ( fromIntegral ((w `shiftR` 24) .&. 0xFF)
  , fromIntegral ((w `shiftR` 16) .&. 0xFF)
  , fromIntegral ((w `shiftR` 8) .&. 0xFF)
  , fromIntegral (w .&. 0xFF)
  )

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

foreign import ccall unsafe "nano_ui_text_atlas_insert_surface"
  textAtlasInsertSurface :: Ptr () -> Ptr () -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO Bool

foreign import ccall unsafe "nano_ui_text_atlas_reset"
  textAtlasReset :: Ptr () -> IO ()

foreign import ccall unsafe "nano_ui_text_atlas_size"
  textAtlasSize :: Ptr () -> Ptr CFloat -> Ptr CFloat -> IO Bool

foreign import ccall unsafe "SDL_DestroySurface"
  freeSurface :: Ptr () -> IO ()
