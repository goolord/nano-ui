-- | Sessions run natively against a scripted host: it answers the canvas
-- queries, queues each step's events when the session waits, and records
-- the texture uploads and draws the session sends.
module Main (main) where

import Control.Exception (Exception, catch, throwIO)
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
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
import NanoUI.Backend (Composition (..), Damage (..), DrawCmd (..), DrawData (..), Input (..), Key (..), Layer (..), glyphPageTextureId)
import NanoUI.SdlWeb.Internal.Host
import NanoUI.SdlWeb.Internal.Input (cssCursor, decodeKey)
import NanoUI.SdlWeb.Internal.Render (DrawCommand (..), drawCommands')
import NanoUI.SdlWeb.Internal.Session (WebFrame, defaultSdlWebOptions, runWebSession)
import NanoUI.Testing (runFrame)
import System.Exit (exitFailure)

assert :: String -> Bool -> IO ()
assert name True = putStrLn ("[PASS] " ++ name)
assert name False = putStrLn ("[FAIL] " ++ name) >> exitFailure

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
  rec <- Recorded <$> newIORef [] <*> newIORef [] <*> newIORef [] <*> newIORef [] <*> newIORef []
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
          }
  pure (host, rec)

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
main = do
  pureChecks
  openingFrame
  clickFrame
  pixelRatioChange
  longComposition

pureChecks :: IO ()
pureChecks = do
  assert "cssCursor nwse-resize" (cssCursor UiCursorNwseResize == "nwse-resize")
  assert "cssCursor context-menu" (cssCursor UiCursorContextMenu == "context-menu")
  assert "cssCursor hidden is none" (cssCursor UiCursorHidden == "none")
  assert "decodeKey character" (decodeKey False 97 == Just (KeyChar 'a'))
  assert "decodeKey uppercase lowers" (decodeKey False 65 == Just (KeyChar 'a'))
  assert "decodeKey named" (decodeKey False (-1) == Just KeyBackspace && decodeKey False (-21) == Just KeyMenu)
  assert "decodeKey F5" (decodeKey False (-105) == Just (KeyF 5))
  assert "decodeKey keypad under Num Lock" (decodeKey True (-(300 + 55)) == Just (KeyChar '7'))
  assert "decodeKey keypad without Num Lock" (decodeKey False (-(300 + 55)) == Just KeyHome)
  assert "decodeKey modifier" (decodeKey False 0 == Nothing)
  -- Commands that continue each other's indices with one texture and clip
  -- join; a command outside the damage is dropped.
  dd <- fakeDrawData
    [ DrawCmd 0 0 100 100 (glyphPageTextureId 0) 0 6 LayerContent
    , DrawCmd 0 0 100 100 (glyphPageTextureId 0) 6 6 LayerContent
    , DrawCmd 0 0 100 100 (glyphPageTextureId 1) 12 6 LayerContent
    , DrawCmd 500 500 10 10 (glyphPageTextureId 1) 18 6 LayerContent
    ]
  let full = drawCommands' 1 (800, 600) DamageFull dd
  assert "drawCommands' joins neighbours" (map dcCount full == [12, 6, 6])
  assert "drawCommands' slots" (map dcSlot full == [0, 1, 1])
  let partial = drawCommands' 2 (800, 600) (DamageClip (Rect 10 10 20 20)) dd
  assert "drawCommands' drops commands outside the damage" (length partial == 2)
  assert "drawCommands' scissors to the damage in pixels" (all ((== Just (20, 20, 40, 40)) . dcScissor) partial)

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

openingFrame :: IO ()
openingFrame = do
  rec <- runScript (800, 600, 1) [] (viewFrame (column (label "Hello from SDL_ttf")))
  frames <- reverse <$> readIORef (recFrames rec)
  uploads <- reverse <$> readIORef (recUploads rec)
  backing <- readIORef (recBacking rec)
  titles <- readIORef (recTitle rec)
  assert "opening: drawing buffer sized to the canvas" (backing == [(800, 600)])
  assert "opening: title set" (titles == ["nano-ui"])
  case frames of
    f : _ -> do
      assert "opening: first frame clears" (frClear f)
      assert "opening: first frame draws" (not (null (frDraws f)))
      assert "opening: draws stay inside the indices" (all (\(_, first, n, _) -> first >= 0 && first + n <= frIndexCount f) (frDraws f))
      assert "opening: draws use glyph pages, the image atlas or no texture" (all (\(s, _, _, _) -> s >= -1 && s <= 4) (frDraws f))
      assert "opening: text draws from glyph page 0" (any (\(s, _, _, _) -> s == 0) (frDraws f))
    [] -> assert "opening: a frame was drawn" False
  let page0 = filter ((== 0) . upSlot) uploads
  assert "opening: glyph page 0 uploaded at 2048" (not (null page0) && all ((== 2048) . upW) page0)
  assert "opening: glyph page 0 holds rasterised glyphs" (any upInk page0)

clickFrame :: IO ()
clickFrame = do
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
  assert "click: one click" (n == 1)
  assert "click: later frames paint their damage only" (any (not . frClear) (drop 1 frames))
  assert "click: partial frames are scissored" (all (\f -> frClear f || all (\(_, _, _, s) -> s /= Nothing) (frDraws f)) frames)
  assert "click: the button sets a CSS cursor" (any (`elem` ["pointer", "default"]) cursors)

-- | A composition longer than the 256 records the session reads at a time
-- arrives whole, though its characters run on into the next batch.
longComposition :: IO ()
longComposition = do
  seen <- newIORef []
  let txt = take 300 (cycle ['\x3042' .. '\x3093'])
      composition = (8, length txt, 2, 1, 0, 0, 0, 0) : [(9, fromEnum ch, 0, 0, 0, 0, 0, 0) | ch <- txt]
      view = do
        inp <- askInput
        liftIO (modifyIORef' seen (inputComposition inp :))
        label "Composing"
  _ <- runScript (800, 600, 1) [events (composition ++ [motion 5 5])] (viewFrame view)
  got <- readIORef seen
  assert "composition: one longer than a batch arrives whole" (Just (Composition (T.pack txt) 2 1) `elem` got)

pixelRatioChange :: IO ()
pixelRatioChange = do
  -- The second step moves the canvas to a pixel ratio of 2, as a browser
  -- zoom or a move to another display does.
  rec <- runScript (800, 600, 1) [events [motion 5 5], (Just (800, 600, 2), [resize])] (viewFrame (label "Scaled"))
  frames <- reverse <$> readIORef (recFrames rec)
  backing <- reverse <$> readIORef (recBacking rec)
  uploads <- readIORef (recUploads rec)
  let scaled = dropWhile ((/= 2) . frScale) frames
  assert "ratio: drawing buffer resized to device pixels" (backing == [(800, 600), (1600, 1200)])
  assert "ratio: the first frame at scale 2 clears and draws" (case scaled of f : _ -> frClear f && not (null (frDraws f)); [] -> False)
  assert "ratio: glyph page 0 uploaded again for the new size" (length (filter ((== 0) . upSlot) uploads) >= 2)
