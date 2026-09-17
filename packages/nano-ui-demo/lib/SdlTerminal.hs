-- | Minimal 80x24 PTY terminal with ANSI colors. Run with @cabal run nano-ui-sdl-terminal@.
module SdlTerminal (main, Term (..), blank, feed, scrollBy, viewport, withPty, drain, send) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket, bracketOnError, catch, throwIO, try)
import Control.Monad (foldM, void, when)
import Control.Monad.ST (ST, runST)
import Data.Bits ((.&.))
import Data.ByteString qualified as B
import Data.Char (chr, isPrint, ord, toUpper)
import Data.Ord (clamp)
import Data.Primitive.PrimArray (PrimArray, indexPrimArray, primArrayFromList)
import Data.STRef (STRef, modifySTRef', newSTRef, readSTRef, writeSTRef)
import Data.Text qualified as T
import Data.Text.Encoding qualified as E
import Data.Text.Encoding.Error (lenientDecode)
import Data.Vector.Unboxed qualified as V
import Data.Vector.Unboxed.Mutable qualified as MV
import Data.Word (Word32, Word8)
import Foreign.C.Error
  ( Errno (..)
  , eAGAIN
  , eIO
  , eWOULDBLOCK
  , throwErrnoIfMinus1_
  )
import Foreign.C.Types (CInt (..))
import GHC.IO.Exception (IOException (..))
import NanoUI hiding (scrollBy)
import NanoUI.Backend.Sdl
import NanoUI.Sdl.Input (SdlEvent (..), applyEvent, pollEvents)
import NanoUI.Testing (newPixelContext)
import Streaming (Of (..))
import Streaming.Prelude qualified as S
import System.Environment (setEnv)
import System.Posix hiding (setEnv)
import System.Posix.IO.ByteString qualified as P
import Text.Read (readMaybe)

-- | A glyph with its foreground and background RGBA.
type Cell = (Char, Word32, Word32)

-- | Foreground and background palette indices, bold, and inverse video.
type Pen = (Word8, Word8, Bool, Bool)

data Esc = Normal | Esc | Csi String | Osc | St | Cs | Discard deriving Eq

data Term = Term
  { screen :: V.Vector Cell
  , cursor :: (Int, Int)
  , pen :: Pen
  , escape :: Esc
  , utf8 :: B.ByteString
  , history :: V.Vector Cell
  , back :: Float
  }

-- | Scrollback keeps at most 2000 rows.
historyCells :: Int
historyCells = 2000 * 80

plainPen :: Pen
plainPen = (0, 0, False, False)

blank :: Term
blank =
  Term
    (V.replicate 1920 (cell plainPen ' '))
    (0, 0)
    plainPen
    Normal
    B.empty
    V.empty
    0

-- | Nord colors by pen index: 0 is the default foreground, 1-8 the ANSI
-- colors and 9-16 their bright variants.
palette :: PrimArray Word32
palette =
  primArrayFromList
    [ 0xD8DEE9FF
    , 0x3B4252FF
    , 0xBF616AFF
    , 0xA3BE8CFF
    , 0xEBCB8BFF
    , 0x81A1C1FF
    , 0xB48EADFF
    , 0x88C0D0FF
    , 0xE5E9F0FF
    , 0x4C566AFF
    , 0xD08770FF
    , 0xB1D196FF
    , 0xF0D399FF
    , 0x8FBCBBFF
    , 0xC69FC0FF
    , 0x98D3E4FF
    , 0xECEFF4FF
    ]

cell :: Pen -> Char -> Cell
cell (f, b, bd, rev) c = if rev then (c, bg, fg) else (c, fg, bg)
 where
  color i = indexPrimArray palette (fromIntegral (min 16 i))
  fg = color (if bd && f <= 8 then if f == 0 then 16 else f + 8 else f)
  bg = if b == 0 then 0x181D26FF else color b

-- | Screen cells and the rows scrolled off the top during one 'feed', newest
-- first.
data Grid s = Grid (MV.MVector s Cell) (STRef s [V.Vector Cell])

feed :: Term -> B.ByteString -> Term
feed t0 bytes = case E.streamDecodeUtf8With lenientDecode (utf8 t0 <> bytes) of
  E.Some text rest _ -> runST $ do
    cells <- V.thaw (screen t0)
    pushed <- newSTRef []
    t <- foldM (step (Grid cells pushed)) t0 (T.unpack text)
    rows <- readSTRef pushed
    scr <- V.unsafeFreeze cells
    let
      h
        | null rows = history t
        | otherwise =
            let
              all' = V.concat (history t : reverse rows)
             in
              V.drop (max 0 (V.length all' - historyCells)) all'
    pure
      t
        { screen = scr
        , utf8 = rest
        , history = h
        , back = min (back t) (fromIntegral (V.length h `div` 80))
        }

move :: Int -> Int -> Term -> Term
move x y t = t {cursor = (clamp (0, 79) x, clamp (0, 23) y)}

-- | Move down @n@ rows, scrolling at the top and bottom edges. A row
-- scrolled off the top goes to history, and a view into history stays on
-- the same rows.
lineFeed :: Grid s -> Int -> Term -> ST s Term
lineFeed (Grid cells pushed) n t
  | y + n < 0 = do
      MV.move (MV.slice 80 1840 cells) (MV.slice 0 1840 cells)
      MV.set (MV.slice 0 80 cells) spaceCell
      pure moved
  | y + n > 23 = do
      top <- V.freeze (MV.slice 0 80 cells)
      modifySTRef' pushed (top :)
      MV.move (MV.slice 0 1840 cells) (MV.slice 80 1840 cells)
      MV.set (MV.slice 1840 80 cells) spaceCell
      pure moved {back = if back t > 0 then back t + 1 else 0}
  | otherwise = pure moved
 where
  (x, y) = cursor t
  moved = move x (y + n) t
  spaceCell = cell (pen t) ' '

scrollBy :: Float -> Term -> Term
scrollBy n t =
  t {back = clamp (0, fromIntegral (V.length (history t) `div` 80)) (back t + n)}

viewport :: Term -> V.Vector Cell
viewport t
  | rows == 0 = screen t
  | rows >= 24 = V.slice start 1920 h
  | otherwise =
      V.slice start (80 * rows) h <> V.take (1920 - 80 * rows) (screen t)
 where
  h = history t
  rows = floor (back t)
  start = V.length h - 80 * rows

-- | The wheel scrolls the view three rows a notch; a key returns it to the
-- live screen.
navigate :: Term -> Input -> Term
navigate t inp@(Input {inputScroll = V2 _ dy})
  | B.null (keys inp) = scrollBy (-3 * dy) t
  | otherwise = t {back = 0}

step :: Grid s -> Term -> Char -> ST s Term
step g@(Grid cells pushed) t c
  | c == '\ESC' && escape t `notElem` [Osc, St] = pure (setEsc Esc)
  | c == '\CAN' || c == '\SUB' = pure done
  | otherwise = case escape t of
      Osc ->
        pure (setEsc (if c == '\BEL' then Normal else if c == '\ESC' then St else Osc))
      St ->
        pure
          ( setEsc
              (if c `elem` ['\\', '\BEL'] then Normal else if c == '\ESC' then St else Osc)
          )
      Cs -> pure done
      Esc -> case c of
        '[' -> pure (setEsc (Csi ""))
        ']' -> pure (setEsc Osc)
        'c' -> do
          MV.set cells (cell plainPen ' ')
          writeSTRef pushed []
          pure blank
        'M' -> lineFeed g (-1) done
        '(' -> pure (setEsc Cs)
        ')' -> pure (setEsc Cs)
        _ -> pure done
      Csi raw
        | final -> csi g raw c done
        | c < ' ' -> pure t
        | length raw < 64 -> pure (setEsc (Csi (raw ++ [c])))
        | otherwise -> pure (setEsc Discard)
      Discard -> pure (if final then done else t)
      Normal -> case c of
        '\r' -> pure t {cursor = (0, y)}
        '\n' -> lineFeed g 1 t
        '\b' -> pure (move (min 79 x - 1) y t)
        '\t' -> pure (move ((x `div` 8 + 1) * 8) y t)
        _
          | isPrint c -> do
              -- Column 80 is a pending wrap: the next glyph starts a new row.
              u <- if x == 80 then lineFeed g 1 t {cursor = (0, y)} else pure t
              let
                (cx, cy) = cursor u
              MV.write cells (cy * 80 + cx) (cell (pen t) c)
              pure u {cursor = (cx + 1, cy)}
          | otherwise -> pure t
 where
  (x, y) = cursor t
  setEsc e = t {escape = e}
  done = setEsc Normal
  final = c >= '@' && c <= '~'

csi :: Grid s -> String -> Char -> Term -> ST s Term
csi _ raw _ t | any (\c -> c /= ';' && (c < '0' || c > '9')) raw = pure t
csi (Grid cells pushed) raw cmd t = case cmd of
  'A' -> pure (move x (y - n) t)
  'B' -> pure (move x (y + n) t)
  'C' -> pure (move (x + n) y t)
  'D' -> pure (move (x - n) y t)
  'G' -> pure (move (n - 1) y t)
  'd' -> pure (move x (n - 1) t)
  _ | cmd == 'H' || cmd == 'f' -> pure (move (max 1 q - 1) (n - 1) t)
  'J' | p == 3 -> do
    writeSTRef pushed []
    pure t {history = V.empty, back = 0}
  'J' -> erase 0 1919
  'K' -> erase (y * 80) (y * 80 + 79)
  'm' -> pure t {pen = sgr (pen t) codes}
  _ -> pure t
 where
  parseCode = maybe 0 (fromInteger . clamp (0, 10000)) . readMaybe . T.unpack
  codes = map parseCode (T.splitOn ";" (T.pack raw))
  p = sum (take 1 codes)
  q = sum (take 1 (drop 1 codes))
  n = max 1 p
  (col, y) = cursor t
  x = min 79 col
  i = y * 80 + x
  -- Mode 0 erases from the cursor to the end, 1 from the start to the
  -- cursor, and 2 everything.
  erase a b = case p of
    0 -> wipe i b
    1 -> wipe a i
    2 -> wipe a b
    _ -> pure t
  wipe a b = do
    MV.set (MV.slice a (b - a + 1) cells) (cell (pen t) ' ')
    pure t

sgr :: Pen -> [Int] -> Pen
sgr p [] = p
sgr p (c : m : cs)
  | c `elem` [38, 48] && m `elem` [2, 5] =
      sgr p (drop (if m == 2 then 3 else 1) cs)
sgr p@(f, b, bd, rev) (c : cs) = flip sgr cs $ case c of
  0 -> plainPen
  1 -> (f, b, True, rev)
  22 -> (f, b, False, rev)
  7 -> (f, b, bd, True)
  27 -> (f, b, bd, False)
  39 -> (0, b, bd, rev)
  49 -> (f, 0, bd, rev)
  _
    | c >= 30 && c <= 37 -> (fromIntegral (c - 29), b, bd, rev)
    | c >= 40 && c <= 47 -> (f, fromIntegral (c - 39), bd, rev)
    | c >= 90 && c <= 97 -> (fromIntegral (c - 81), b, bd, rev)
    | c >= 100 && c <= 107 -> (f, fromIntegral (c - 91), bd, rev)
    | otherwise -> p

-- | Set the PTY to 80x24.
foreign import ccall unsafe "nano_terminal_size" setSize :: Fd -> IO CInt

-- | Run an action with the master side of a PTY running an interactive shell.
withPty :: (Fd -> IO a) -> IO a
withPty action = bracket boot close (action . fst)
 where
  boot = bracketOnError openPseudoTerminal (\(m, s) -> closeFd m >> closeFd s) $
    \(m, s) -> do
      throwErrnoIfMinus1_ "PTY size" (setSize m) >> setFdOption m NonBlockingRead True
      path <- getTerminalName s
      pid <- forkProcess $ do
        closeFd m >> closeFd s >> void createSession
        slave <- openFd path ReadWrite defaultFileFlags
        mapM_ (dupTo slave) [stdInput, stdOutput, stdError]
        when (slave > stdError) (closeFd slave)
        setEnv "TERM" "ansi"
        executeFile "/bin/sh" False ["-i"] Nothing
      closeFd s >> pure (m, pid)
  close (m, pid) = do
    closeFd m
    signalProcess sigKILL pid `catch` \(_ :: IOException) -> pure ()
    void (getProcessStatus True False pid)

isE :: [Errno] -> IOException -> Bool
isE es = maybe False (`elem` es) . fmap Errno . ioe_errno

-- | Feed up to four 1024-byte reads into the terminal. The flag is set when
-- the shell has exited.
drain :: Fd -> Term -> IO (Of Term Bool)
drain fd t = S.fold feed t id (S.unfoldr readChunk (4 :: Int))
 where
  readChunk 0 = pure (Left False)
  readChunk n =
    try (P.fdRead fd 1024) >>= \case
      Right b -> pure (if B.null b then Left True else Right (b, n - 1))
      Left e
        | isE [eAGAIN, eWOULDBLOCK] e -> pure (Left False)
        | isE [eIO] e -> pure (Left True)
        | otherwise -> throwIO e

-- | Write what the PTY accepts and return the rest.
send :: Fd -> B.ByteString -> IO B.ByteString
send _ b | B.null b = pure b
send fd b =
  (flip B.drop b . fromIntegral <$> P.fdWrite fd b)
    `catch` \e ->
      if isE [eAGAIN, eWOULDBLOCK] e
        then pure b
        else
          if isE [eIO] e
            then pure B.empty
            else throwIO e

keys :: Input -> B.ByteString
keys inp = E.encodeUtf8 (foldMap key (inputKeys inp) <> prefix <> text)
 where
  mods = inputModifiers inp
  ctrlChar = chr . (.&. 31) . ord . toUpper
  text = (if modCtrl mods then T.map ctrlChar else id) (inputChars inp)
  prefix = if modAlt mods && not (T.null text) then "\ESC" else ""
  key = \case
    KeyEnter -> "\r"
    KeyBackspace -> "\DEL"
    KeyTab -> "\t"
    KeyEscape -> "\ESC"
    KeyUp -> "\ESC[A"
    KeyDown -> "\ESC[B"
    KeyRight -> "\ESC[C"
    KeyLeft -> "\ESC[D"
    KeyHome -> "\ESC[H"
    KeyEnd -> "\ESC[F"
    KeyDelete -> "\ESC[3~"

main :: IO ()
main = withPty $ \fd -> do
  ctx0 <- newPixelContext
  let
    monoFont = FontSearch ["Input Mono", "JetBrains Mono", "Menlo", "monospace"]
  withSdl
    defaultSdlOptions
      { sdlWindowTitle = "nano-ui Terminal"
      , sdlWindowSize = Size 816 592
      , sdlWindowResizable = False
      , sdlAppFontSize = 16
      , sdlAppFont = monoFont
      , sdlAppMonoFont = monoFont
      }
    ctx0
    $ \ctx env -> do
      let
        advance (t, pending, _, _) = do
          threadDelay 16000
          events <- pollEvents
          let
            inputs = fmap (applyEvent emptyInput) events
          rest <- send fd (pending <> foldMap keys inputs)
          next :> ended <- drain fd (foldl' navigate t inputs)
          let
            changed =
              not (null events)
                || cursor next /= cursor t
                || back next /= back t
                || screen next /= screen t
          pure (next, rest, ended || any (== EvQuit) events, changed)
      S.mapM_
        ( \(t, _, _, changed) -> when changed (void (sdlDrawFrame ctx (view t) env emptyInput True))
        )
        . S.takeWhile (\(_, _, ended, _) -> not ended)
        $ S.iterateM advance (pure (blank, B.empty, False, True))

view :: Term -> NanoUI ()
view t = void $ canvas (fontMono . grow) $ \(Rect x y w h) -> do
  drawRect (Rect x y w h) 0x181D26FF
  V.imapM_
    ( \i (c, f, b) -> do
        let
          (ry, col) = i `quotRem` 80
          px = x + 8 + fromIntegral col * 10
          py = y + 8 + fromIntegral ry * 24
        when (b /= 0x181D26FF) $ drawRect (Rect px py 10 24) (Color b)
        when (c /= ' ') $
          drawText (V2 px py) AlignStart AlignTop (T.singleton c) (Color f)
    )
    (viewport t)
  let
    (cx, cy) = cursor t
    hRows = fromIntegral (V.length (history t) `div` 80)
    curX = x + 8 + fromIntegral (min 79 cx) * 10
    curY = y + 29 + fromIntegral cy * 24
  when (back t < 1) $ drawRect (Rect curX curY 10 2) 0x81A1C1FF
  when (back t > 0 && hRows > 0) $ do
    let
      thumbH = max 24 (h * (24 / (hRows + 24)))
    drawRect
      (Rect (x + w - 4) (y + (h - thumbH) * (1 - back t / hRows)) 2 thumbH)
      0x4C566AA0
