-- | File dialogs in the browser. A page sees no paths: opening hands over
-- the chosen files' names and bytes, and saving hands the browser bytes to
-- write where the user says (a save dialog where the browser has one, else
-- a download under the suggested name). Browsers show these only right
-- after a click or key press on the page, so run them from a task the press
-- starts ('NanoUI.useTask'); each waits for the user.
module NanoUI.SdlWeb.Internal.Files
  ( FileFilter (..)
  , FileDialogOptions (..)
  , defaultFileDialogOptions
  , WebFile (..)
  , FileDialogMode (..)
  , pickFiles
  , saveFile
  , decodePicked
  , openWebFiles
  , openWebFolder
  , saveWebFile
  ) where

import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)
import Foreign.Ptr (Ptr, castPtr)
import NanoUI.SdlWeb.Internal.Host (WebHost (..), browserAvailable, browserHost)

-- | A file type to offer, as nano-ui-sdl's dialogs take it.
data FileFilter = FileFilter
  { filterName :: !Text
  -- ^ Human-readable label, e.g. @"Haskell source"@.
  , filterPattern :: !Text
  -- ^ Semicolon-separated extension list, e.g. @"hs;lhs"@, or @"*"@.
  }
  deriving (Eq, Show)

-- | Options for the browser's file dialogs. Browsers pick the starting
-- folder themselves, so there is no default location.
data FileDialogOptions = FileDialogOptions
  { dialogFilters :: ![FileFilter]
  -- ^ File types to offer (ignored when choosing a folder).
  , dialogAllowMany :: !Bool
  -- ^ Allow choosing more than one file.
  }
  deriving (Eq, Show)

-- | No filters, one file.
defaultFileDialogOptions :: FileDialogOptions
defaultFileDialogOptions = FileDialogOptions [] False

-- | A file the user chose: its name (for a folder's files, the path inside
-- the folder, with @/@) and its bytes.
data WebFile = WebFile
  { webFileName :: !String
  , webFileBytes :: !ByteString
  }
  deriving (Eq, Show)

-- | What 'pickFiles' asks the user for.
data FileDialogMode = PickFiles | PickFolder
  deriving (Eq, Show)

-- | Show the file chooser on a host. 'Nothing' when the user cancelled or
-- the chooser failed.
pickFiles :: WebHost -> FileDialogMode -> FileDialogOptions -> IO (Maybe [WebFile])
pickFiles host mode opts = do
  let modeCode = case mode of PickFiles -> 0; PickFolder -> 1
  n <- withUtf8 (filterLines (dialogFilters opts)) $ \p len ->
    whPickFiles host modeCode (dialogAllowMany opts) p len
  if n < 0
    then pure Nothing
    else Just . decodePicked <$> BSI.create n (whTakePicked host)

-- | Save bytes on a host, suggesting a name. The name the file was saved
-- under (the suggested one for a download), or 'Nothing' when the user
-- cancelled or saving failed.
saveFile :: WebHost -> FileDialogOptions -> String -> ByteString -> IO (Maybe String)
saveFile host opts name bytes = do
  n <-
    withUtf8 (T.pack name) $ \np nn ->
      withUtf8 (filterLines (dialogFilters opts)) $ \fp fn ->
        unsafeUseAsCStringLen bytes $ \(dp, dn) ->
          whSaveFile host np nn fp fn (castPtr dp) dn
  if n < 0
    then pure Nothing
    else Just . T.unpack . TE.decodeUtf8Lenient <$> BSI.create n (whTakePicked host)

-- | The files in 'whPickFiles'' answer: per file a 32-bit little-endian
-- name length, the UTF-8 name, a 32-bit length and the bytes. A truncated
-- record ends the list.
decodePicked :: ByteString -> [WebFile]
decodePicked buf
  | BS.length buf < 4 = []
  | otherwise =
      let (nameLen, rest) = word32 buf
          (name, rest') = BS.splitAt nameLen rest
          (dataLen, rest'') = word32 rest'
          (bytes, rest''') = BS.splitAt dataLen rest''
       in if BS.length name < nameLen || BS.length rest' < 4 || BS.length bytes < dataLen
            then []
            else WebFile (T.unpack (TE.decodeUtf8Lenient name)) bytes : decodePicked rest'''
  where
    word32 b =
      ( foldr (\i acc -> acc `shiftL` 8 .|. fromIntegral (BS.index b i)) 0 [0 .. 3]
      , BS.drop 4 b
      )

-- | The filters as the page reads them: a line per filter, its name, a tab
-- and its extension list.
filterLines :: [FileFilter] -> Text
filterLines = T.intercalate "\n" . map (\f -> filterName f <> "\t" <> filterPattern f)

withUtf8 :: Text -> (Ptr Word8 -> Int -> IO a) -> IO a
withUtf8 t k = unsafeUseAsCStringLen (TE.encodeUtf8 t) $ \(p, n) -> k (castPtr p) n

-- | Let the user choose files, and read them; 'Nothing' when they
-- cancelled, or outside a browser. Run it from a task a press starts.
openWebFiles :: FileDialogOptions -> IO (Maybe [WebFile])
openWebFiles opts = inBrowser Nothing $ \host -> pickFiles host PickFiles opts

-- | Let the user choose a folder, and read every file in it, named by its
-- path inside the folder; 'Nothing' when they cancelled, or outside a
-- browser. Run it from a task a press starts.
openWebFolder :: IO (Maybe [WebFile])
openWebFolder = inBrowser Nothing $ \host -> pickFiles host PickFolder defaultFileDialogOptions

-- | Save bytes as a file, suggesting a name. Chromium-based browsers show a
-- save dialog (@showSaveFilePicker@, which no other browser implements);
-- the others download the file under the suggested name, which Firefox
-- saves where its "Always ask you where to save files" setting says. The
-- name it was saved under, or 'Nothing' when the user cancelled, saving
-- failed, or this is not a browser. Run it from a task a press starts.
saveWebFile :: FileDialogOptions -> String -> ByteString -> IO (Maybe String)
saveWebFile opts name bytes = inBrowser Nothing $ \host -> saveFile host opts name bytes

inBrowser :: a -> (WebHost -> IO a) -> IO a
inBrowser outside k
  | browserAvailable = k =<< browserHost
  | otherwise = pure outside
