-- | Fonts beyond the bundled Inter: the families installed on the machine,
-- which the browser lists and hands over as bytes through the Local Font
-- Access API once the user allows it, and font files fetched by URL. Also
-- the WebGL renderer's name, for the debug snapshot. Each reads a
-- 'WebHost'; the browser's calls are in "NanoUI.SdlWeb.Internal.Host".
module NanoUI.SdlWeb.Internal.Fonts
  ( FontOrigin (..)
  , findFontFamilies
  , loadFontBytes
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
  let kind = case origin of InstalledFamily -> 0; FontUrl -> 1
  n <- unsafeUseAsCStringLen (TE.encodeUtf8 (T.pack name)) $ \(p, len) ->
    whLoadFont host kind (castPtr p) len
  if n <= 0
    then pure Nothing
    else Just <$> BSI.create n (whTakeFontBytes host)

-- | The WebGL renderer's description, as the browser gives it.
rendererName :: WebHost -> IO Text
rendererName host = do
  n <- whRendererInfo host nullPtr 0
  if n <= 0
    then pure "WebGL2"
    else allocaBytes n $ \buf -> do
      m <- whRendererInfo host buf n
      TE.decodeUtf8Lenient <$> BS.packCStringLen (castPtr buf, min n m)
