{- FOURMOLU_DISABLE -}                                                                            -- | Minimal 80x24 PTY terminal with ANSI colors. Run with @cabal run nano-ui-sdl-terminal@.
module SdlTerminal (main, Term (..), blank, feed, scrollBy, viewport, withPty, drain, send) where -- exported types, state, and terminal actions

import Control.Concurrent (threadDelay)                                                           -- frame rate pacing (~60 FPS)
import Control.Exception (bracket, bracketOnError, catch, throwIO, try)                           -- PTY & process resource cleanup
import Control.Monad (void, when)                                                                 -- monadic control helpers
import Data.Bits ((.&.))                                                                          -- Ctrl modifier ASCII masking
import Data.ByteString qualified as B                                                             -- raw PTY byte buffer I/O
import Data.Char (chr, isPrint, ord, toUpper)                                                     -- character mapping & classification
import Data.Ord (clamp)                                                                           -- bound cursor, scroll offset & CSI params
import Data.Text qualified as T                                                                   -- Unicode text processing
import Data.Text.Encoding qualified as E                                                          -- streaming UTF-8 text decoder
import Data.Text.Encoding.Error (lenientDecode)                                                   -- replacement char on bad UTF-8
import Data.Vector.Unboxed qualified as V                                                         -- unboxed 80x24 screen grid buffer
import Data.Word (Word8, Word32)                                                                  -- color indices and RGBA words
import Foreign.C.Error (Errno (..), eAGAIN, eIO, eWOULDBLOCK, throwErrnoIfMinus1_)                -- POSIX errno checks
import Foreign.C.Types (CInt (..))                                                                -- C ABI types for winsize ioctl
import GHC.IO.Exception (IOException (..))                                                        -- inspect errno on I/O exceptions
import NanoUI                                                                                     -- core UI canvas & drawing monad
import NanoUI.Backend.Sdl                                                                         -- SDL2 window & context initialization
import NanoUI.Sdl.Input (SdlEvent (..), applyEvent, pollEvents)                                   -- SDL event loop & input handling
import Streaming (Of (..))                                                                        -- stream pair result type
import Streaming.Prelude qualified as S                                                           -- stream iteration over PTY chunks
import System.Environment (setEnv)                                                                -- export TERM environment variable
import System.Posix hiding (setEnv)                                                               -- fork, dup2, and signal control
import System.Posix.IO.ByteString qualified as P                                                  -- POSIX non-blocking read/write
import Text.Read (readMaybe)                                                                      -- safe parse of CSI numeric parameters

type Cell = (Char, Word32, Word32)                                                                -- (character glyph, fg RGBA, bg RGBA)
type Pen = (Word8, Word8, Bool, Bool)                                                             -- (fg index, bg index, bold, inverse)

data Esc = Normal | Esc | Csi String | Osc | St | Cs | Discard deriving Eq                        -- ANSI escape sequence parser states

data Term = Term {screen :: V.Vector Cell, cursor :: (Int, Int), pen :: Pen, escape :: Esc,       -- 80x24 cells, cursor (col,row), pen, parser
                  utf8 :: B.ByteString, history :: V.Vector Cell, back :: Float}                  -- pending UTF-8 bytes, scrollback, offset

plainPen :: Pen                                                                                   -- default reset pen styling
plainPen = (0, 0, False, False)                                                                   -- fg 0, bg 0, regular weight, normal video
blank :: Term                                                                                     -- initial empty terminal state
blank = Term (V.replicate 1920 (cell plainPen ' ')) (0, 0) plainPen Normal B.empty V.empty 0      -- 80*24 = 1920 blank space cells at (0,0)

palette :: Word8 -> Word32                                                                        -- map 0-15 ANSI color index to 32-bit RGBA
palette = (colors !!) . fromIntegral . min 16                                                     -- index lookup clamped to palette size (16)
  where colors = [ 0xD8DEE9FF, 0x3B4252FF, 0xBF616AFF, 0xA3BE8CFF, 0xEBCB8BFF, 0x81A1C1FF         -- Nord theme colors 0-5
                 , 0xB48EADFF, 0x88C0D0FF, 0xE5E9F0FF, 0x4C566AFF, 0xD08770FF, 0xB1D196FF         -- Nord theme colors 6-7, default fg (16), bright 8-10
                 , 0xF0D399FF, 0x8FBCBBFF, 0xC69FC0FF, 0x98D3E4FF, 0xECEFF4FF ]                   -- Nord bright colors 11-15 & bright white

cell :: Pen -> Char -> Cell                                                                       -- construct rendered cell with resolved colors
cell (f, b, bd, rev) c = if rev then (c, bg, fg) else (c, fg, bg)                                 -- swap foreground/background on inverse video
  where fg = palette (if bd && f <= 8 then if f == 0 then 16 else f + 8 else f)                   -- bold maps colors 1-8 to bright variants 9-16
        bg = if b == 0 then 0x181D26FF else palette b                                             -- 0 uses default dark background #181D26

feed :: Term -> B.ByteString -> Term                                                              -- consume raw bytes into terminal model
feed t bytes = case E.streamDecodeUtf8With lenientDecode (utf8 t <> bytes) of                     -- stream decode UTF-8 with unconsumed rest
  E.Some text rest _ -> (T.foldl' step t text) {utf8 = rest}                                      -- fold step over decoded chars and save rest

move :: Int -> Int -> Term -> Term                                                                -- move cursor position with screen clamping
move x y t = t {cursor = (clamp (0, 79) x, clamp (0, 23) y)}                                      -- keep cursor within 80 cols and 24 rows

lineFeed :: Int -> Term -> Term                                                                   -- move cursor vertically by n with scrolling
lineFeed n t                                                                                      -- handle screen scroll boundaries
  | y + n < 0 = moved {screen = spaces <> V.take 1840 (screen t)}                                 -- scroll down: prepend blank line at top
  | y + n > 23 = scrollBy 0 moved {screen = V.drop 80 (screen t) <> spaces,                       -- scroll up: push top line to history
      history = V.drop (max 0 (V.length rows - 160000)) rows,                                     -- retain at most 2000 scrollback rows (160k cells)
      back = if back t > 0 then back t + 1 else 0}                                                -- advance scrollback offset when viewing history
  | otherwise = moved                                                                             -- cursor remains within visible 24 rows
  where (x, y) = cursor t; moved = move x (y + n) t                                               -- new clamped cursor position
        spaces = V.replicate 80 (cell (pen t) ' '); rows = history t <> V.take 80 (screen t)      -- blank 80-char row and updated history rows

scrollBy :: Float -> Term -> Term                                                                 -- offset visible viewport into scrollback
scrollBy n t = t {back = clamp (0, fromIntegral (V.length (history t) `div` 80)) (back t + n)}    -- clamp scroll offset to available history

viewport :: Term -> V.Vector Cell                                                                 -- slice 1920 cells for current viewport
viewport t                                                                                        -- extract visible cells from screen or history
  | back t == 0 = screen t                                                                        -- live view: return active screen buffer
  | otherwise   = V.slice (V.length h - 80 * floor (back t)) 1920 (h <> screen t)                 -- scrollback: slice 80x24 window from history
  where h = history t                                                                             -- terminal history buffer alias

navigate :: Term -> Input -> Term                                                                 -- update scrollback position from user input
navigate t inp@(Input {inputScroll = V2 _ dy})                                                    -- handle scroll wheel and keypress navigation
  | B.null (keys inp) = scrollBy (-3 * dy) t                                                      -- mouse wheel: scroll viewport by 3 lines per notch
  | otherwise         = t {back = 0}                                                              -- key press: snap viewport back to bottom

step :: Term -> Char -> Term                                                                      -- step terminal state on single input char
step t c                                                                                          -- state machine character handler
  | c == '\ESC' && escape t `notElem` [Osc, St] = setEsc Esc                                      -- start of escape sequence (unless in OSC/ST)
  | c == '\CAN' || c == '\SUB' = done                                                             -- cancel or substitute terminates escape
  | otherwise = case escape t of                                                                  -- dispatch by current parser escape state
  Osc     -> setEsc (if c == '\BEL' then Normal else if c == '\ESC' then St else Osc)             -- OSC terminated by BEL or ESC
  St      -> setEsc (if c `elem` ['\\', '\BEL'] then Normal else if c == '\ESC' then St else Osc) -- ST string terminator (\ or BEL)
  Cs      -> done                                                                                 -- charset designation complete (ignore)
  Esc     -> case c of                                                                            -- 2-character escape sequence dispatcher
    '[' -> setEsc (Csi ""); ']' -> setEsc Osc; 'c' -> blank                                       -- CSI sequence, OSC sequence, or reset terminal
    'M' -> lineFeed (-1) done; '(' -> setEsc Cs; ')' -> setEsc Cs; _ -> done                      -- reverse line feed or character set select
  Csi raw | final -> csi raw c done                                                               -- execute CSI command when final byte arrives
          | c < ' ' -> t                                                                          -- ignore C0 control codes inside CSI
          | length raw < 64 -> setEsc (Csi (raw ++ [c]))                                          -- buffer CSI parameter and intermediate chars
          | otherwise -> setEsc Discard                                                           -- overflow guard: discard parameter string
  Discard -> if final then done else t                                                            -- skip characters until final byte
  Normal  -> case c of                                                                            -- handle normal ASCII characters
    '\r'   -> t {cursor = (0, y)}                                                                 -- carriage return: reset x to column 0
    '\n'   -> lineFeed 1 t                                                                        -- line feed: advance cursor down / scroll up
    '\b'   -> move (min 79 x - 1) y t                                                             -- backspace: move left clamped to column 0
    '\t'   -> move ((x `div` 8 + 1) * 8) y t                                                      -- horizontal tab: advance to next 8-col stop
    _ | isPrint c -> let u = if x == 80 then lineFeed 1 t {cursor = (0, y)} else t                -- printable character: auto-wrap if at col 80
                         (cx, cy) = cursor u                                                      -- resolved cursor position for write
                     in u { screen = screen u V.// [(cy * 80 + cx, cell (pen t) c)]               -- write character cell to screen buffer
                          , cursor = (cx + 1, cy)                                                 -- advance cursor x position by one
                          }                                                                       -- close updated terminal state record
      | otherwise -> t                                                                            -- drop all other unhandled control codes
  where (x, y) = cursor t; setEsc e = t {escape = e}                                              -- cursor coordinates and escape setter
        done = setEsc Normal; final = c >= '@' && c <= '~'                                        -- return to Normal; CSI final byte check

csi :: String -> Char -> Term -> Term                                                             -- handle CSI (Control Sequence Introducer)
csi raw _ t | any (\c -> c /= ';' && (c < '0' || c > '9')) raw = t                                -- ignore non-standard / invalid parameter strings
csi raw cmd t = case cmd of                                                                       -- dispatch on command letter
  'A' -> move x (y - n) t; 'B' -> move x (y + n) t                                                -- CUU, CUD: cursor up / down by n rows
  'C' -> move (x + n) y t; 'D' -> move (x - n) y t                                                -- CUF, CUB: cursor forward / backward by n cols
  'G' -> move (n - 1) y t; 'd' -> move x (n - 1) t                                                -- CHA (cursor horizontal abs), VPA (vertical abs)
  _ | cmd == 'H' || cmd == 'f' -> move (max 1 q - 1) (n - 1) t                                    -- CUP / HVP: set cursor position (col, row)
  'J' | p == 3 -> t {history = V.empty, back = 0}                                                 -- ED 3: clear scrollback buffer
  'J' -> erase 0 1919                                                                             -- ED: erase in display (below, above, or all)
  'K' -> erase (y * 80) end                                                                       -- EL: erase in line (to end, from start, or all)
  'm' -> t {pen = sgr (pen t) codes}                                                              -- SGR: Select Graphic Rendition (colors/styles)
  _   -> t                                                                                        -- ignore unhandled CSI commands
  where                                                                                           -- argument parsing and screen wiping helpers
    parseCode = maybe 0 (fromInteger . clamp (0, 10000)) . readMaybe . T.unpack                   -- parse single integer parameter clamped to 10000
    codes = map parseCode (T.splitOn ";" (T.pack raw))                                            -- parse semicolon-separated parameter list
    p = sum (take 1 codes); q = sum (take 1 (drop 1 codes))                                       -- first argument p (default 0), second argument q
    n = max 1 p; (col, y) = cursor t; x = min 79 col; i = y * 80 + x; end = y * 80 + 79           -- count n (default 1), cursor offsets, row bounds
    erase a b = case p of 0 -> wipe i b; 1 -> wipe a i; 2 -> wipe a b; _ -> t                     -- erase mode: 0=cursor to end, 1=start to cursor, 2=all
    wipe a b = t {screen = screen t V.// [(j, cell (pen t) ' ') | j <- [a .. b]]}                 -- fill specified cell index range with blank spaces

sgr :: Pen -> [Int] -> Pen                                                                        -- apply ANSI Select Graphic Rendition code list
sgr p [] = p                                                                                      -- base case: all SGR codes processed
sgr p (c:m:cs)                                                                                    -- handle extended color sequences (256-color / 24-bit RGB)
  | c `elem` [38, 48] && m `elem` [2, 5] = sgr p (drop (if m == 2 then 3 else 1) cs)              -- skip extended color parameter arguments
sgr p@(f, b, bd, rev) (c:cs) = flip sgr cs $ case c of                                            -- process head SGR code and recurse on remainder
  0 -> plainPen; 1 -> (f, b, True, rev); 22 -> (f, b, False, rev)                                 -- 0: reset; 1: bold on; 22: bold off
  7 -> (f, b, bd, True); 27 -> (f, b, bd, False)                                                  -- 7: inverse video on; 27: inverse off
  39 -> (0, b, bd, rev); 49 -> (f, 0, bd, rev)                                                    -- 39: default foreground; 49: default background
  _ | c >= 30 && c <= 37 -> (fromIntegral (c - 29), b, bd, rev)                                   -- 30-37: standard foreground color (1..8)
    | c >= 40 && c <= 47 -> (f, fromIntegral (c - 39), bd, rev)                                   -- 40-47: standard background color (1..8)
    | c >= 90 && c <= 97 -> (fromIntegral (c - 81), b, bd, rev)                                   -- 90-97: high-intensity foreground color (9..16)
    | c >= 100 && c <= 107 -> (f, fromIntegral (c - 91), bd, rev)                                 -- 100-107: high-intensity background color (9..16)
    | otherwise -> p                                                                              -- ignore unhandled SGR formatting codes

foreign import ccall unsafe "nano_terminal_size" setSize :: Fd -> IO CInt                         -- C helper: TIOCSWINSZ ioctl to 80x24 size

withPty :: (Fd -> IO a) -> IO a                                                                   -- allocate PTY pair, fork shell, and manage lifecycle
withPty action = bracket boot close (action . fst)                                                -- bracket PTY lifetime around callback action
  where                                                                                           -- sub-process spawning and teardown
    boot = bracketOnError openPseudoTerminal (\(m, s) -> closeFd m >> closeFd s) $                -- bracket openPseudoTerminal with closeFd cleanup
      \(m, s) -> do                                                                               -- configure master and fork child shell process
      throwErrnoIfMinus1_ "PTY size" (setSize m) >> setFdOption m NonBlockingRead True            -- set 80x24 window size and non-blocking I/O
      path <- getTerminalName s                                                                   -- retrieve slave device name (/dev/pts/N)
      pid <- forkProcess $ do                                                                     -- fork child process for interactive shell
        closeFd m >> closeFd s >> void createSession                                              -- detach from parent terminal / setsid
        slave <- openFd path ReadWrite defaultFileFlags                                           -- open slave as controlling terminal
        mapM_ (dupTo slave) [stdInput, stdOutput, stdError]                                       -- bind slave PTY to stdin, stdout, stderr
        when (slave > stdError) (closeFd slave)                                                   -- close slave fd if allocated above fd 2
        setEnv "TERM" "ansi"                                                                      -- configure terminal type for subprocess
        executeFile "/bin/sh" False ["-i"] Nothing                                                -- execute interactive Bourne shell
      closeFd s >> pure (m, pid)                                                                  -- close slave in parent, return master & pid
    close (m, pid) = do                                                                           -- cleanup PTY and child shell process
      closeFd m                                                                                   -- close master pseudo-terminal file descriptor
      signalProcess sigKILL pid `catch` \(_ :: IOException) -> pure ()                            -- send SIGKILL to terminate child shell process
      void (getProcessStatus True False pid)                                                      -- wait for child process exit status to avoid zombie

isE :: [Errno] -> IOException -> Bool                                                             -- check if IOException matches given errno list
isE es = maybe False (`elem` es) . fmap Errno . ioe_errno                                         -- extract errno from IOException and test membership

drain :: Fd -> Term -> IO (Of Term Bool)                                                          -- drain available bytes from PTY into terminal
drain fd t = S.fold feed t id (S.unfoldr readChunk (4 :: Int))                                    -- read up to 4 chunks (4KB) per tick
  where                                                                                           -- streaming unfold chunk reader
    readChunk 0 = pure (Left False)                                                               -- quota reached, continue running (False)
    readChunk n = try (P.fdRead fd 1024) >>= \case                                                -- attempt 1024-byte non-blocking read
      Right b -> pure (if B.null b then Left True else Right (b, n - 1))                          -- EOF on empty read (True = process exited)
      Left e | isE [eAGAIN, eWOULDBLOCK] e -> pure (Left False)                                   -- EAGAIN/EWOULDBLOCK: no data currently ready
             | isE [eIO] e -> pure (Left True)                                                    -- EIO: child process closed slave PTY (exited)
             | otherwise -> throwIO e                                                             -- re-throw unexpected I/O exceptions

send :: Fd -> B.ByteString -> IO B.ByteString                                                     -- send pending input bytes to PTY
send _ b | B.null b = pure b                                                                      -- early return if buffer is empty
send fd b = (flip B.drop b . fromIntegral <$> P.fdWrite fd b)                                     -- write bytes and advance remaining slice
  `catch` \e ->                                                                                   -- catch I/O exceptions on PTY write
    if isE [eAGAIN, eWOULDBLOCK] e then pure b                                                    -- EAGAIN/EWOULDBLOCK: retain unwritten buffer
    else if isE [eIO] e then pure B.empty                                                         -- EIO: slave closed, discard remaining output
    else throwIO e                                                                                -- re-throw any other unexpected I/O exception

keys :: Input -> B.ByteString                                                                     -- convert NanoUI key inputs to ANSI byte sequence
keys inp = E.encodeUtf8 (foldMap key (inputKeys inp) <> prefix <> text)                           -- encode special keys, Alt escape, and text
  where                                                                                           -- modifier handling and keycode mapping
    mods = inputModifiers inp                                                                     -- active keyboard modifiers (Ctrl, Alt, Shift)
    ctrlChar = chr . (.&. 31) . ord . toUpper                                                     -- mask character to ASCII control code (0..31)
    text = (if modCtrl mods then T.map ctrlChar else id) (inputChars inp)                         -- transform input text with Ctrl modifier if active
    prefix = if modAlt mods && not (T.null text) then "\ESC" else ""                              -- prefix ESC character when Alt modifier is held
    key = \case                                                                                   -- map special key identifiers to escape codes
      KeyEnter -> "\r"; KeyBackspace -> "\DEL"; KeyTab -> "\t"; KeyEscape -> "\ESC"               -- Enter (CR), Backspace (DEL), Tab, Escape
      KeyUp -> "\ESC[A"; KeyDown -> "\ESC[B"; KeyRight -> "\ESC[C"; KeyLeft -> "\ESC[D"           -- ANSI cursor navigation escape sequences
      KeyHome -> "\ESC[H"; KeyEnd -> "\ESC[F"; KeyDelete -> "\ESC[3~"                             -- Home, End, and Delete escape sequences

main :: IO ()                                                                                     -- executable entry point
main = withPty $ \fd -> do                                                                        -- run terminal session with PTY master fd
  ctx0 <- newSdlContext                                                                           -- create new SDL rendering context
  let monoFont = FontSearch ["Input Mono", "JetBrains Mono", "Menlo", "monospace"]                -- monospace font fallback chain
  withSdl defaultSdlOptions                                                                       -- start SDL backend with options
    { sdlWindowTitle = "nano-ui Terminal"                                                         -- window title bar text
    , sdlWindowSize = Size 816 592                                                                -- 816x592 fixed window (80x24 + margins)
    , sdlWindowResizable = False                                                                  -- fixed-size terminal window
    , sdlAppFontSize = 16                                                                         -- base font point size
    , sdlAppFont = monoFont                                                                       -- primary UI font face
    , sdlAppMonoFont = monoFont                                                                   -- monospace terminal font face
    } ctx0 $ \ctx env -> do                                                                       -- initialize SDL context and run event loop
    let advance (t, pending, _) = do                                                              -- single frame event & I/O tick
          threadDelay 16000                                                                       -- sleep ~16ms for 60 FPS refresh rate
          events <- pollEvents                                                                    -- poll pending SDL window events
          let inputs = fmap (applyEvent emptyInput) events                                        -- convert raw SDL events to UI inputs
          rest <- send fd (pending <> foldMap keys inputs)                                        -- transmit user keystrokes to shell
          next :> ended <- drain fd (foldl' navigate t inputs)                                    -- drain shell output and update scroll
          pure (next, rest, ended || any (== EvQuit) events)                                      -- return next state and exit condition
    S.mapM_ (\(t, _, _) -> void (sdlDrawFrame ctx (view t) env emptyInput True))                  -- render frame using view function
      . S.takeWhile (\(_, _, ended) -> not ended)                                                 -- terminate stream on quit event or shell exit
      $ S.iterateM advance (pure (blank, B.empty, False))                                         -- iterate frame advance step from initial blank state

view :: Term -> NanoUI ()                                                                         -- draw terminal UI widget tree
view t = void $ canvas (fontMono . grow $ defaultLayout) $ \(Rect x y w h) -> do                  -- create responsive canvas element
  drawRect (Rect x y w h) 0x181D26FF                                                              -- clear window background (#181D26)
  V.imapM_ (\i (c, f, b) -> do                                                                    -- iterate through all cells in viewport
    let (ry, col) = i `quotRem` 80                                                                -- row index (0..23) and column index (0..79)
        px = x + 8 + fromIntegral col * 10                                                        -- horizontal cell pixel coordinate (10px per col)
        py = y + 8 + fromIntegral ry * 24                                                         -- vertical cell pixel coordinate (24px per row)
    when (b /= 0x181D26FF) $ drawRect (Rect px py 10 24) (Color b)                                -- draw non-default cell background
    when (c /= ' ') $                                                                             -- check for non-space printable character
      drawText (V2 px py) AlignStart AlignTop (T.singleton c) (Color f)                           -- render glyph with cell foreground color
    ) (viewport t)                                                                                -- apply per-cell renderer to 1920 viewport cells
  let (cx, cy) = cursor t                                                                         -- active cursor screen coordinates
      hRows = fromIntegral (V.length (history t) `div` 80)                                        -- total scrollback rows count
      curX = x + 8 + fromIntegral (min 79 cx) * 10                                                -- cursor horizontal pixel position
      curY = y + 29 + fromIntegral cy * 24                                                        -- cursor vertical underline pixel position
  when (back t < 1) $ drawRect (Rect curX curY 10 2) 0x81A1C1FF                                   -- draw cursor underline (#81A1C1) at bottom
  when (back t > 0 && hRows > 0) $ do                                                             -- draw scrollbar when looking at history
    let thumbH = max 24 (h * (24 / (hRows + 24)))                                                 -- calculate scroll thumb height
    drawRect (Rect (x + w - 4) (y + (h - thumbH) * (1 - back t / hRows)) 2 thumbH) 0x4C566AA0     -- render scroll indicator on right edge
