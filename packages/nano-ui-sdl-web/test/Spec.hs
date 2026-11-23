-- | Sessions run natively against a scripted host: it answers the canvas
-- queries, queues each step's events when the session waits, and records
-- the texture uploads and draws the session sends.
module Main (main) where

import Control.Exception (Exception, catch, throwIO)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Foreign.Marshal.Utils (copyBytes)
import NanoUI.Backend.SdlWeb (SdlWebDebugSnapshot (..), CoreDebugSnapshot (..), WebFont (..), askSdlWebDebug, listWebFontFamilies, setSdlWebUiFont)
import NanoUI.SdlWeb.Internal.Files
  ( FileDialogMode (..)
  , FileDialogOptions (..)
  , FileFilter (..)
  , WebFile (..)
  , decodePicked
  , defaultFileDialogOptions
  , openWebFiles
  , pickFiles
  , saveFile
  )
import NanoUI.SdlWeb.Internal.Fonts (findFontFamilies)
import NanoUI.Ttf.Internal.Inter (fontInterBytes, fontInterLabel)
import NanoUI.Ttf.Internal.Font (FontHooks (..), SdlFont (..), closeFont, openFontBytes, readSdlFont)
import qualified NanoUI.Ttf.Internal.Font as Ttf
import NanoUI.SdlWeb.Internal.Font (newWebFallbacks, webFontHooks)
import qualified Data.IntMap.Strict as IM
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int32)
import qualified Data.Primitive.PrimArray as PA
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)
import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peekByteOff, pokeByteOff, pokeElemOff)
import NanoUI (NanoUI, Rect (..), UiCursorKind (..), askInput, button, column, label, colorRGB)
import NanoUI.Backend
  ( Composition (..), Damage (..), DrawCmd (..), DrawData (..), Input (..), Key (..), Layer (..), glyphAtlasPages
  , glyphPageTextureId, pixelEdges, snapDamage, vertexSize
  )
import NanoUI.SdlWeb.Internal.Host
import NanoUI.SdlWeb.Internal.Input (cssCursor, decodeKey)
import NanoUI.SdlWeb.Internal.Render (DrawCommand (..), drawCommands')
import NanoUI.SdlWeb.Internal.Session (WebFrame, defaultSdlWebOptions, runWebSession)
import NanoUI.Testing (runFrame)
import NanoUI.Testing.Runner (check, runChecks)
import NanoUI.Ttf.Internal.Header (glyphAtlasPageCount, vertexBytes)

-- ---------------------------------------------------------------------------
-- The scripted host

-- | One event record: kind, a, b, c, modifiers, x, y, z.
type Record = (Int, Int, Int, Int, Int, Float, Float, Float)

-- | What happens at one wait: the canvas's new width, height and pixel
-- ratio, if they change, and the events queued.
type Step = (Maybe (Float, Float, Float), [Record])

data Upload = Upload {upSlot :: Int, upW :: Int, upRect :: (Int, Int, Int, Int), upInk :: Bool}
  deriving (Show)

data Frame = Frame {frClear :: Bool, frScale :: Double, frSize :: (Int, Int), frDraws :: [(Int, Int, Int, Maybe (Int, Int, Int, Int))], frIndexCount :: Int}
  deriving (Show)

data Recorded = Recorded
  { recUploads :: IORef [Upload]
  , recFrames :: IORef [Frame]
  , recBacking :: IORef [(Int, Int)]
  , recCursor :: IORef [T.Text]
  , recTitle :: IORef [T.Text]
  , recFontsAsked :: IORef [String]
  , recFilters :: IORef [T.Text]
  , recSaved :: IORef [(T.Text, BS.ByteString)]
  }

data ScriptDone = ScriptDone deriving (Show)

instance Exception ScriptDone

-- | A host whose canvas is @canvas@ and whose events come from the steps,
-- one step each time the session waits. Once the steps are out, a wait with
-- a limit ends without events, so a busy session settles; the session ends
-- at its first wait with no limit.
scriptedHost :: IORef (Float, Float, Float) -> [Step] -> IO (WebHost, Recorded)
scriptedHost canvas steps0 = do
  steps <- newIORef steps0
  queue <- newIORef []
  idle <- newIORef (0 :: Int)
  rec <- Recorded <$> newIORef [] <*> newIORef [] <*> newIORef [] <*> newIORef [] <*> newIORef [] <*> newIORef [] <*> newIORef [] <*> newIORef []
  pendingFont <- newIORef BS.empty
  picked <- newIORef BS.empty
  extended <- BS.readFile extendedFontPath
  let take' buf n = do
        evs <- readIORef queue
        let (now, later) = splitAt n evs
        writeIORef queue later
        forM_ (zip [0 ..] now) $ \(i, (k, a, b, c, m, x, y, z)) -> do
          let o = i * eventRecordSize
          forM_ (zip [0 ..] [k, a, b, c, m]) $ \(j, v) -> pokeByteOff buf (o + j * 4) (fromIntegral v :: Int32)
          forM_ (zip [5 ..] [x, y, z]) $ \(j, v) -> pokeByteOff buf (o + j * 4) v
        pure (length now)
      wait ms = do
        pending <- readIORef queue
        when (null pending) $
          readIORef steps >>= \case
            (resized, evs) : rest -> do
              writeIORef steps rest
              mapM_ (writeIORef canvas) resized
              writeIORef queue evs
            [] -> do
              n <- readIORef idle
              writeIORef idle (n + 1)
              when (ms < 0 || n > 50) (throwIO ScriptDone)
      utf8 p n = TE.decodeUtf8 <$> BS.packCStringLen (castPtr p, n)
      host =
        WebHost
          { whQueryCanvas = \p -> do
              (w, h, ratio) <- readIORef canvas
              forM_ (zip [0 ..] [w, h, ratio, 1, 2]) $ \(i, v) -> pokeElemOff p i v
          , whTakeEvents = take'
          , whWaitEvents = wait
          , whWake = pure ()
          , whResizeBacking = \w h -> modifyIORef' (recBacking rec) ((w, h) :)
          , whUploadTexture = \slot w _ pixels row x y rw rh -> do
              ink <- anyInk pixels row x y rw rh
              modifyIORef' (recUploads rec) (Upload slot w (x, y, rw, rh) ink :)
          , whBeginFrame = \clear _ _ _ _ scale w h ->
              modifyIORef' (recFrames rec) (Frame clear scale (w, h) [] 0 :)
          , whDraw = \_ _ _ icount cmds n -> do
              draws <- mapM (readCommand cmds) [0 .. n - 1]
              modifyIORef' (recFrames rec) $ \case
                f : fs -> f {frDraws = draws, frIndexCount = icount} : fs
                [] -> []
          , whSetCursor = \p n -> utf8 p n >>= \t -> modifyIORef' (recCursor rec) (t :)
          , whTextInput = \_ _ _ _ _ -> pure ()
          , whClipboardRead = \_ _ -> pure (-1)
          , whClipboardWrite = \_ _ -> pure ()
          , whSetTitle = \p n -> utf8 p n >>= \t -> modifyIORef' (recTitle rec) (t :)
          , whRendererInfo = \p n -> copyOut p n testRenderer
          , whFindFonts = pure (BS.length testFamilies)
          , whTakeFontList = \p -> () <$ copyOut p (BS.length testFamilies) testFamilies
          , whLoadFont = \kind p n -> do
              name <- T.unpack <$> utf8 p n
              modifyIORef' (recFontsAsked rec) (name :)
              -- The host serves the bundled Inter at one URL and the wider
              -- fallback Inter where the default options look for it, and
              -- has no installed families.
              let serve b = BS.length b <$ writeIORef pendingFont b
              case (kind, name) of
                (1, "fonts/inter.ttf") -> serve fontInterBytes
                (1, "fonts/inter-extended.ttf") -> serve extended
                _ -> pure (-1)
          , whTakeFontBytes = \p -> readIORef pendingFont >>= \b -> () <$ copyOut p (BS.length b) b
          -- The chooser picks two files, or is cancelled when asked for a
          -- folder.
          , whPickFiles = \mode _ p n -> do
              utf8 p n >>= \t -> modifyIORef' (recFilters rec) (t :)
              if mode == 1
                then pure (-1)
                else BS.length testPicked <$ writeIORef picked testPicked
          , whSaveFile = \np nn _ _ dp dn -> do
              name <- utf8 np nn
              bytes <- BS.packCStringLen (castPtr dp, dn)
              modifyIORef' (recSaved rec) ((name, bytes) :)
              let saved = TE.encodeUtf8 name
              BS.length saved <$ writeIORef picked saved
          , whTakePicked = \p -> readIORef picked >>= \b -> () <$ copyOut p (BS.length b) b
          }
  pure (host, rec)

-- | The fallback font the page serves, from the package's sources.
extendedFontPath :: FilePath
extendedFontPath = "web/fonts/inter-extended.ttf"

-- | What the scripted host reports as its WebGL renderer and its installed
-- font families.
testRenderer, testFamilies :: BS.ByteString
testRenderer = "WebGL2, Scripted"
testFamilies = "Inter\nNoto Sans\nInter"

-- | Two files as the page packs them: a 32-bit little-endian name length,
-- the name, a 32-bit length and the bytes, each.
testPicked :: BS.ByteString
testPicked = BS.concat [record "notes.txt" "hello", record "d/\195\169.bin" "\0\1\2"]
  where
    record name bytes = BS.concat [le (BS.length name), name, le (BS.length bytes), bytes]
    le n = BS.pack [fromIntegral (n `div` (256 ^ k)) | k <- [0 .. 3 :: Int]]

-- | Copy up to @n@ bytes of @b@ to @p@, and answer @b@'s full length.
copyOut :: Ptr Word8 -> Int -> BS.ByteString -> IO Int
copyOut p n b = do
  when (n > 0) $ unsafeUseAsCStringLen b $ \(src, len) -> copyBytes p (castPtr src) (min n len)
  pure (BS.length b)

readCommand :: Ptr Word8 -> Int -> IO (Int, Int, Int, Maybe (Int, Int, Int, Int))
readCommand buf i = do
  let at k = fromIntegral <$> (peekByteOff buf (i * commandRecordSize + k * 4) :: IO Int32)
  [slot, first, count, on, x, y, w, h] <- mapM at [0 .. 7]
  pure (slot, first, count, if on /= 0 then Just (x, y, w, h) else Nothing)

-- | Whether any pixel in a rect has alpha, skipping the 4x4 white patch
-- every page starts with.
anyInk :: Ptr Word8 -> Int -> Int -> Int -> Int -> Int -> IO Bool
anyInk pixels row x0 y0 w h = go x0 y0
  where
    go x y
      | y >= y0 + h = pure False
      | x >= x0 + w = go x0 (y + 1)
      | x < 4 && y < 4 = go (x + 1) y
      | otherwise = do
          a <- peekByteOff pixels ((y * row + x) * 4 + 3) :: IO Word8
          if a /= 0 then pure True else go (x + 1) y

runScript :: (Float, Float, Float) -> [Step] -> WebFrame -> IO Recorded
runScript canvas0 steps frame = do
  canvas <- newIORef canvas0
  (host, rec) <- scriptedHost canvas steps
  runWebSession host defaultSdlWebOptions frame `catch` \ScriptDone -> pure ()
  pure rec

viewFrame :: NanoUI () -> WebFrame
viewFrame ui ctx inp = (\(_, dd, dirty) -> (dd, dirty)) <$> runFrame ctx inp ui

motion :: Float -> Float -> Record
motion x y = (2, 0, 0, 0, 0, x, y, 0)

press, release, resize :: Record
press = (3, 0, 1, 0, 0, 0, 0, 0)
release = (3, 0, 0, 0, 0, 0, 0, 0)
resize = (1, 0, 0, 0, 0, 0, 0, 0)

events :: [Record] -> Step
events evs = (Nothing, evs)

-- ---------------------------------------------------------------------------

main :: IO ()
main =
  runChecks
    [ ("pure", pureChecks)
    , ("opening", openingFrame)
    , ("click", clickFrame)
    , ("pixel-ratio", pixelRatioChange)
    , ("long-composition", longComposition)
    , ("fonts-and-debug", fontsAndDebug)
    , ("file-dialogs", fileDialogs)
    , ("fallback-fonts", fallbackFonts)
    ]

pureChecks :: IORef Int -> IO ()
pureChecks failed = do
  check failed "cssCursor nwse-resize" (cssCursor UiCursorNwseResize == "nwse-resize")
  check failed "cssCursor context-menu" (cssCursor UiCursorContextMenu == "context-menu")
  check failed "cssCursor hidden is none" (cssCursor UiCursorHidden == "none")
  check failed "decodeKey character" (decodeKey False 97 == Just (KeyChar 'a'))
  check failed "decodeKey uppercase lowers" (decodeKey False 65 == Just (KeyChar 'a'))
  check failed "decodeKey named" (decodeKey False (-1) == Just KeyBackspace && decodeKey False (-21) == Just KeyMenu)
  check failed "decodeKey F5" (decodeKey False (-105) == Just (KeyF 5))
  check failed "decodeKey keypad under Num Lock" (decodeKey True (-(300 + 55)) == Just (KeyChar '7'))
  check failed "decodeKey keypad without Num Lock" (decodeKey False (-(300 + 55)) == Just KeyHome)
  check failed "decodeKey modifier" (decodeKey False 0 == Nothing)
  -- Commands that continue each other's indices with one texture and clip
  -- join; a command outside the damage is dropped.
  dd <- fakeDrawData
    [ DrawCmd 0 0 100 100 (glyphPageTextureId 0) 0 6 LayerContent
    , DrawCmd 0 0 100 100 (glyphPageTextureId 0) 6 6 LayerContent
    , DrawCmd 0 0 100 100 (glyphPageTextureId 1) 12 6 LayerContent
    , DrawCmd 500 500 10 10 (glyphPageTextureId 1) 18 6 LayerContent
    ]
  let full = drawCommands' 1 (800, 600) DamageFull dd
  check failed "drawCommands' joins neighbours" (map dcCount full == [12, 6, 6])
  check failed "drawCommands' slots" (map dcSlot full == [0, 1, 1])
  let partial = drawCommands' 2 (800, 600) (DamageClip (Rect 10 10 20 20)) dd
  check failed "drawCommands' drops commands outside the damage" (length partial == 2)
  check failed "drawCommands' scissors to the damage in pixels" (all ((== Just (20, 20, 40, 40)) . dcScissor) partial)
  -- Damage snapped to whole pixels comes back from layout units a rounding
  -- error off them at some scales; the scissor stays on the snapped pixels.
  let cases = [(s, Rect x x 20.3 7.9) | s <- [1, 1.1, 1.2, 1.25, 1.3, 1.4, 1.5, 1.75, 2.25, 3], x <- [0.3, 3.7, 10.1, 33.3, 77.7]]
      snapped s r = case snapDamage s (DamageClip r) of
        DamageClip r' -> r'
        DamageFull -> r
      want s r = let (x0, y0, x1, y1) = pixelEdges s r in Just (x0, y0, x1 - x0, y1 - y0)
      scissorOf s r = map dcScissor (drawCommands' s (800, 600) (DamageClip (snapped s r)) dd)
  check failed "drawCommands' test cases include a damage edge a rounding error off its pixel"
    (any (\(s, r) -> pixelEdges s (snapped s r) /= pixelEdges s r) cases)
  check failed "drawCommands' scissors snapped damage to its own pixels at fractional scales"
    (all (\(s, r) -> all (== want s r) (take 1 (scissorOf s r))) cases)
  check failed "nano_ui_ttf_shared.h: NANO_UI_VERTEX_BYTES is the core's vertexSize" (vertexBytes == vertexSize)
  check failed "nano_ui_ttf_shared.h: NANO_UI_GLYPH_ATLAS_PAGES is the core's glyphAtlasPages" (glyphAtlasPageCount == glyphAtlasPages)

fakeDrawData :: [DrawCmd] -> IO DrawData
fakeDrawData cmds = do
  fp <- newForeignPtr_ nullPtr
  pure
    DrawData
      { drawVertices = fp
      , drawVertexCount = 0
      , drawIndices = fp
      , drawIndexCount = 24
      , drawCommands = U.fromList cmds
      , drawLayerOffsets = PA.primArrayFromList [0, 0, length cmds, length cmds, length cmds]
      , drawClear = colorRGB 0 0 0
      }

openingFrame :: IORef Int -> IO ()
openingFrame failed = do
  rec <- runScript (800, 600, 1) [] (viewFrame (column (label "Hello from SDL_ttf")))
  frames <- reverse <$> readIORef (recFrames rec)
  uploads <- reverse <$> readIORef (recUploads rec)
  backing <- readIORef (recBacking rec)
  titles <- readIORef (recTitle rec)
  check failed "opening: drawing buffer sized to the canvas" (backing == [(800, 600)])
  check failed "opening: title set" (titles == ["nano-ui"])
  case frames of
    f : _ -> do
      check failed "opening: first frame clears" (frClear f)
      check failed "opening: first frame draws" (not (null (frDraws f)))
      check failed "opening: draws stay inside the indices" (all (\(_, first, n, _) -> first >= 0 && first + n <= frIndexCount f) (frDraws f))
      check failed "opening: draws use glyph pages, the image atlas or no texture" (all (\(s, _, _, _) -> s >= -1 && s <= 4) (frDraws f))
      check failed "opening: text draws from glyph page 0" (any (\(s, _, _, _) -> s == 0) (frDraws f))
    [] -> check failed "opening: a frame was drawn" False
  let page0 = filter ((== 0) . upSlot) uploads
  check failed "opening: glyph page 0 uploaded at 2048" (not (null page0) && all ((== 2048) . upW) page0)
  check failed "opening: glyph page 0 holds rasterised glyphs" (any upInk page0)

-- | A UI font asked for from the view is loaded through the host before the
-- next frame, the debug snapshot names it and the host's renderer, a font
-- that cannot be loaded falls back to Inter, and the host's family list
-- comes back sorted out of repeats.
fontsAndDebug :: IORef Int -> IO ()
fontsAndDebug failed = do
  snaps <- newIORef []
  passes <- newIORef (0 :: Int)
  let view font = column $ do
        n <- liftIO (atomicModifyIORef' passes (\k -> (k + 1, k)))
        if n == 0
          then setSdlWebUiFont font
          else askSdlWebDebug >>= mapM_ (\s -> liftIO (modifyIORef' snaps (s :)))
        label "Fonts"
      script = [events [motion 5 5], events [motion 6 6]]
  rec <- runScript (800, 600, 1) script (viewFrame (view (WebFontUrl "fonts/inter.ttf")))
  asked <- readIORef (recFontsAsked rec)
  check failed "fonts: the fallback at start, then the URL, through the host" (asked == ["fonts/inter.ttf", "fonts/inter-extended.ttf"])
  readIORef snaps >>= \case
    s : _ -> do
      check failed ("fonts: the snapshot names the loaded font (" <> dbgFont s <> ")") (dbgFont s == "fonts/inter.ttf")
      check failed "fonts: the snapshot names the host's renderer" (dbgRenderer s == "WebGL2, Scripted")
      check failed "fonts: the snapshot has the canvas size" (dbgWinW (dbgCore s) == 800 && dbgWinH (dbgCore s) == 600)
    [] -> check failed "fonts: askSdlWebDebug answers in a session" False
  writeIORef snaps [] >> writeIORef passes 0
  _ <- runScript (800, 600, 1) script (viewFrame (view (WebFontInstalled "Missing Sans")))
  readIORef snaps >>= \case
    s : _ -> check failed ("fonts: a missing family falls back to Inter (" <> dbgFont s <> ")") (dbgFont s == fontInterLabel)
    [] -> check failed "fonts: askSdlWebDebug answers after a failed load" False
  (host, _) <- newIORef (800, 600, 1) >>= \c -> scriptedHost c []
  families <- findFontFamilies host
  check failed "fonts: the family list drops repeats" (families == Right ["Inter", "Noto Sans"])
  outside <- listWebFontFamilies
  check failed "fonts: listing needs a browser" (either (const True) (const False) outside)

-- | A face gets the fallback attached for a character it lacks that the
-- fallback has (the ellipsis, which the bundled ASCII Inter lacks), and none
-- for ASCII or for a character neither has.
fallbackFonts :: IORef Int -> IO ()
fallbackFonts failed = do
  extended <- BS.readFile extendedFontPath
  fallbacks <- newWebFallbacks [extended]
  let hooks = webFontHooks fallbacks
  Ttf.withTtf hooks $ do
    let face = openFontBytes fontInterBytes 16 >>= readSdlFont 16
        attachedFor txt = do
          sf <- face
          fhEnsureCoverage hooks sf txt
          IM.size <$> readIORef (sfFallbacks sf) <* closeFont sf
    ascii <- attachedFor "Open File..."
    check failed "fallback: none for ASCII" (ascii == 0)
    ellipsis <- attachedFor "Open File\8230"
    check failed "fallback: attached for the ellipsis" (ellipsis == 1)
    accents <- attachedFor "caf\233 \945\946\947 \1046"
    check failed "fallback: one attachment covers accents, Greek and Cyrillic" (accents == 1)
    han <- attachedFor "\20013"
    check failed "fallback: none for a character neither font has" (han == 0)

-- | The file chooser's files come back decoded with UTF-8 names, the
-- filters reach the page as lines, a cancel is 'Nothing', saving hands the
-- page the name and bytes, and outside a browser nothing opens.
fileDialogs :: IORef Int -> IO ()
fileDialogs failed = do
  (host, rec) <- newIORef (800, 600, 1) >>= \c -> scriptedHost c []
  let opts = defaultFileDialogOptions {dialogFilters = [FileFilter "Text" "txt;md"], dialogAllowMany = True}
  files <- pickFiles host PickFiles opts
  check failed "files: both files, in order, with their bytes"
    (files == Just [WebFile "notes.txt" "hello", WebFile "d/\233.bin" "\0\1\2"])
  filters <- readIORef (recFilters rec)
  check failed "files: filters reach the page as name, tab, extensions" (filters == ["Text\ttxt;md"])
  folder <- pickFiles host PickFolder defaultFileDialogOptions
  check failed "files: a cancelled chooser is Nothing" (folder == Nothing)
  saved <- saveFile host opts "out.txt" "bytes"
  check failed "files: saving answers the name saved under" (saved == Just "out.txt")
  readIORef (recSaved rec) >>= check failed "files: the page gets the name and bytes" . (== [("out.txt", "bytes")])
  check failed "files: a truncated record ends the list" (decodePicked (BS.take 10 testPicked) == [])
  outside <- openWebFiles opts
  check failed "files: nothing opens outside a browser" (outside == Nothing)

clickFrame :: IORef Int -> IO ()
clickFrame failed = do
  clicks <- newIORef (0 :: Int)
  let view = column $ do
        clicked <- button "Press"
        when clicked (liftIO (modifyIORef' clicks (+ 1)))
  -- The button sits at the window's top-left; a press and release over it
  -- is one click.
  rec <- runScript (800, 600, 1) [events [motion 20 15], events [press], events [release]] (viewFrame view)
  n <- readIORef clicks
  frames <- reverse <$> readIORef (recFrames rec)
  cursors <- readIORef (recCursor rec)
  check failed "click: one click" (n == 1)
  check failed "click: later frames paint their damage only" (any (not . frClear) (drop 1 frames))
  check failed "click: partial frames are scissored" (all (\f -> frClear f || all (\(_, _, _, s) -> s /= Nothing) (frDraws f)) frames)
  check failed "click: the button sets a CSS cursor" (any (`elem` ["pointer", "default"]) cursors)

-- | A composition longer than the 256 records the session reads at a time
-- arrives whole, though its characters run on into the next batch.
longComposition :: IORef Int -> IO ()
longComposition failed = do
  seen <- newIORef []
  let txt = take 300 (cycle ['\x3042' .. '\x3093'])
      composition = (8, length txt, 2, 1, 0, 0, 0, 0) : [(9, fromEnum ch, 0, 0, 0, 0, 0, 0) | ch <- txt]
      view = do
        inp <- askInput
        liftIO (modifyIORef' seen (inputComposition inp :))
        label "Composing"
  _ <- runScript (800, 600, 1) [events (composition ++ [motion 5 5])] (viewFrame view)
  got <- readIORef seen
  check failed "composition: one longer than a batch arrives whole" (Just (Composition (T.pack txt) 2 1) `elem` got)

pixelRatioChange :: IORef Int -> IO ()
pixelRatioChange failed = do
  -- The second step moves the canvas to a pixel ratio of 2, as a browser
  -- zoom or a move to another display does.
  rec <- runScript (800, 600, 1) [events [motion 5 5], (Just (800, 600, 2), [resize])] (viewFrame (label "Scaled"))
  frames <- reverse <$> readIORef (recFrames rec)
  backing <- reverse <$> readIORef (recBacking rec)
  uploads <- readIORef (recUploads rec)
  let scaled = dropWhile ((/= 2) . frScale) frames
  check failed "ratio: drawing buffer resized to device pixels" (backing == [(800, 600), (1600, 1200)])
  check failed "ratio: the first frame at scale 2 clears and draws" (case scaled of f : _ -> frClear f && not (null (frDraws f)); [] -> False)
  check failed "ratio: glyph page 0 uploaded again for the new size" (length (filter ((== 0) . upSlot) uploads) >= 2)
