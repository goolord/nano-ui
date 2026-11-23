-- | Fonts beyond the bundled Inter: the families installed on the machine,
-- which the browser lists and hands over as bytes through the Local Font
-- Access API once the user allows it, a font service's families, and font
-- files fetched by URL. Also the WebGL renderer's name, for the debug
-- snapshot. Each reads a
-- 'WebHost'; the browser's calls are in "NanoUI.SdlWeb.Internal.Host".
module NanoUI.SdlWeb.Internal.Fonts
  ( FontOrigin (..)
  , findFontFamilies
  , knownFontFamilies
  , loadFontBytes
  , searchCandidates
  , rendererName
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (castPtr, nullPtr)
import NanoUI.SdlWeb.Internal.Host (WebHost (..))

-- | Where 'loadFontBytes' finds a font.
data FontOrigin
  = InstalledFamily
  -- ^ An installed family, by name; its regular face, else its first.
  | FontUrl
  -- ^ A font file at a URL, fetched with the page's credentials rules.
  | FamilyByName
  -- ^ A family by name: installed when the user has allowed it, else from
  -- the font service, if the session has one.
  deriving (Eq, Show)

-- | The installed font families, sorted and without repeats, or why they
-- cannot be listed. Browsers ask the user the first time, and only right
-- after a click or key press on the page; Chromium-based browsers have the
-- API, Firefox and Safari do not.
findFontFamilies :: WebHost -> IO (Either String [String])
findFontFamilies host = do
  n <- whFindFonts host
  case n of
    -1 -> pure (Left "this browser cannot list installed fonts (Chromium-based browsers can)")
    _
      | n < 0 -> pure (Left "access to the installed fonts was declined")
      | n == 0 -> pure (Right [])
      | otherwise -> allocaBytes n $ \buf -> do
          whTakeFontList host buf
          names <- TE.decodeUtf8Lenient <$> BS.packCStringLen (castPtr buf, n)
          pure (Right (nub (filter (not . null) (map T.unpack (T.lines names)))))

-- | A font file's bytes, or 'Nothing' when it could not be loaded (no such
-- family, a failed fetch, access declined).
loadFontBytes :: WebHost -> FontOrigin -> String -> IO (Maybe ByteString)
loadFontBytes host origin name = do
  let kind = case origin of InstalledFamily -> 0; FontUrl -> 1; FamilyByName -> 2
  n <- unsafeUseAsCStringLen (TE.encodeUtf8 (T.pack name)) $ \(p, len) ->
    whLoadFont host kind (castPtr p) len
  if n <= 0
    then pure Nothing
    else Just <$> BSI.create n (whTakeFontBytes host)

-- | The font families known so far: the installed ones the user has let
-- the browser list, then the font service's catalogue, without repeats.
knownFontFamilies :: WebHost -> IO [String]
knownFontFamilies host = do
  n <- whFontFamilies host nullPtr 0
  if n <= 0
    then pure []
    else allocaBytes n $ \buf -> do
      m <- whFontFamilies host buf n
      names <- TE.decodeUtf8Lenient <$> BS.packCStringLen (castPtr buf, min n m)
      pure (filter (not . null) (map T.unpack (T.lines names)))

-- | The families a search tries for a name: the name itself, or for the
-- generic CSS families (@sans-serif@, @serif@, @monospace@) the same common
-- families nano-ui-sdl's search tries.
searchCandidates :: String -> [String]
searchCandidates name = case filter (`notElem` (" -_" :: String)) (T.unpack (T.toLower (T.pack name))) of
  "sansserif" -> ["DejaVu Sans", "Liberation Sans", "Noto Sans", "Open Sans", "Helvetica Neue"]
  "serif" -> ["DejaVu Serif", "Liberation Serif", "Noto Serif", "Times New Roman"]
  "monospace" -> ["DejaVu Sans Mono", "Liberation Mono", "Ubuntu Mono", "Noto Sans Mono", "Consolas", "Courier New"]
  _ -> [name]

-- | The WebGL renderer's description, as the browser gives it.
rendererName :: WebHost -> IO Text
rendererName host = do
  n <- whRendererInfo host nullPtr 0
  if n <= 0
    then pure "WebGL2"
    else allocaBytes n $ \buf -> do
      m <- whRendererInfo host buf n
      TE.decodeUtf8Lenient <$> BS.packCStringLen (castPtr buf, min n m)
