-- | Minimal 80x24 PTY terminal. Run with @cabal run nano-ui-sdl-terminal@.
module SdlTerminal (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket, bracketOnError, catch, throwIO, try)
import Control.Monad (void, when)
import Data.Bits ((.&.))
import Data.ByteString qualified as B
import Data.Char (chr, isPrint, ord, toUpper)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as E
import Data.Text.Encoding.Error (lenientDecode)
import Data.Vector.Unboxed qualified as V
import Foreign.C.Error (Errno (..), eAGAIN, eWOULDBLOCK, eIO, throwErrnoIfMinus1_)
import Foreign.C.Types (CInt (..))
import GHC.IO.Exception (IOException (..))
import NanoUI
import NanoUI.Backend.Sdl
import NanoUI.Sdl.Input (SdlEvent (..), applyEvent, pollEvents)
import Streaming (Of (..))
import Streaming.Prelude qualified as S
import System.Environment (setEnv)
import System.Posix.IO
import System.Posix.IO.ByteString qualified as P
import System.Posix.Process
import System.Posix.Signals
import System.Posix.Terminal
import System.Posix.Types (Fd (..))
import Text.Read (readMaybe)

data Term = Term {screen :: V.Vector Char, cursor :: (Int, Int), escape :: String, utf8 :: B.ByteString,
                  history :: V.Vector Char, back :: Float}
blank :: Term
blank = Term (V.replicate 1920 ' ') (0, 0) "" B.empty V.empty 0

feed :: Term -> B.ByteString -> Term
feed t bytes = let E.Some text rest _ = E.streamDecodeUtf8With lenientDecode (utf8 t <> bytes)
               in (T.foldl' step t text) {utf8 = rest}

move :: Int -> Int -> Term -> Term
move x y t = t {cursor = (max 0 (min 79 x), max 0 (min 23 y))}

newline :: Term -> Term
newline t = if y < 23 then move x (y + 1) t else scrollBy 0 (move x y t)
  {screen = V.drop 80 (screen t) <> V.replicate 80 ' ', history = V.drop (max 0 (V.length rows - 160000)) rows,
   back = if back t > 0 then back t + 1 else 0}
  where (x, y) = cursor t
        rows = history t <> V.take 80 (screen t)

-- Keep 2,000 history rows; fractional wheel deltas also work on touchpads.
scrollBy :: Float -> Term -> Term
scrollBy n t = t {back = max 0 (min (fromIntegral (V.length (history t) `div` 80)) (back t + n))}

viewport :: Term -> V.Vector Char
viewport t = V.slice (V.length (history t) - 80 * floor (back t)) 1920 (history t <> screen t)

navigate :: Term -> Input -> Term
navigate t inp = if B.null (keys inp) then scrollBy (-3 * dy) t else t {back = 0}
  where V2 _ dy = inputScroll inp

step :: Term -> Char -> Term
step t c = case escape t of
  "osc" -> parsing (if c == '\BEL' then "" else if c == '\ESC' then "st" else "osc")
  "st" -> parsing (if c == '\\' then "" else "osc")
  "charset" -> done
  "esc" -> parsing $ case c of '[' -> "["; ']' -> "osc"; '(' -> "charset"; ')' -> "charset"; _ -> ""
  '[' : args | c >= '@' && c <= '~' -> csi args c done
             | otherwise -> parsing (if length args < 64 then '[' : args ++ [c] else "discard")
  "discard" -> parsing (if c >= '@' && c <= '~' then "" else "discard")
  _ -> case c of
    '\ESC' -> parsing "esc"
    '\r' -> t {cursor = (0, y)}
    '\n' -> newline t
    '\b' -> move (min 79 x - 1) y t
    '\t' -> move ((x `div` 8 + 1) * 8) y t
    _ | isPrint c -> let u = if x == 80 then newline t {cursor = (0, y)} else t
                         (cx, cy) = cursor u
                     in u {screen = screen u V.// [(cy * 80 + cx, c)], cursor = (cx + 1, cy)}
      | otherwise -> t
  where (x, y) = cursor t; parsing e = t {escape = e}; done = parsing ""

csi :: String -> Char -> Term -> Term
csi raw command t = case command of
  'A' -> move x (y - n) t; 'B' -> move x (y + n) t
  'C' -> move (x + n) y t; 'D' -> move (x - n) y t
  'G' -> move (n - 1) y t; 'H' -> move (max 1 q - 1) (n - 1) t
  'f' -> move (max 1 q - 1) (n - 1) t
  'J' -> case p of 0 -> erase i 1919; 1 -> erase 0 i; 2 -> erase 0 1919; _ -> t
  'K' -> case p of 0 -> erase i end; 1 -> erase (y * 80) i; 2 -> erase (y * 80) end; _ -> t
  _ -> t
  where
    args = map (min 10000 . max 0 . fromMaybe 0 . readMaybe . T.unpack) (T.splitOn ";" (T.pack raw)) ++ repeat 0
    p = args !! 0; q = args !! 1; n = max 1 p
    (col, y) = cursor t; x = min 79 col; i = y * 80 + x; end = y * 80 + 79
    erase a b = t {screen = screen t V.// [(j, ' ') | j <- [a .. b]]}

foreign import ccall unsafe "nano_terminal_size" setSize :: CInt -> IO CInt

withPty :: (Fd -> IO a) -> IO a
withPty action = bracket boot close (action . fst)
  where
    boot = bracketOnError openPseudoTerminal (\(m, s) -> closeFd m >> closeFd s) $ \(m, s) -> do
      path <- getTerminalName s
      let Fd raw = m
      throwErrnoIfMinus1_ "PTY size" (setSize raw)
      setFdOption m NonBlockingRead True
      pid <- forkProcess $ do
        closeFd m >> closeFd s
        void createSession
        slave <- openFd path ReadWrite defaultFileFlags -- acquire controlling TTY after setsid
        mapM_ (dupTo slave) [stdInput, stdOutput, stdError]
        when (slave > stdError) (closeFd slave)
        setEnv "TERM" "vt100"
        executeFile "/bin/sh" False ["-i"] Nothing
      closeFd s
      pure (m, pid)
    close (m, pid) = closeFd m >> (signalProcess sigKILL pid `catch` \(_ :: IOException) -> pure ()) >> void (getProcessStatus True False pid)

isErrno :: IOException -> [Errno] -> Bool
isErrno e = any (\(Errno n) -> ioe_errno e == Just n)

-- Bounded, nonblocking PTY chunks -> pure terminal fold; EAGAIN is not EOF.
drain :: Fd -> Term -> IO (Of Term Bool)
drain fd t = S.fold feed t id (S.unfoldr readChunk (4 :: Int))
  where
    readChunk 0 = pure (Left False)
    readChunk n = try (P.fdRead fd 1024) >>= \case
      Right b -> pure (if B.null b then Left True else Right (b, n - 1))
      Left e | isErrno e [eAGAIN, eWOULDBLOCK] -> pure (Left False)
             | isErrno e [eIO] -> pure (Left True)
             | otherwise -> throwIO e

send :: Fd -> B.ByteString -> IO B.ByteString
send _ b | B.null b = pure b
send fd b = ((\n -> B.drop (fromIntegral n) b) <$> P.fdWrite fd b)
  `catch` \e -> if isErrno e [eAGAIN, eWOULDBLOCK] then pure b else if isErrno e [eIO] then pure B.empty else throwIO e

keys :: Input -> B.ByteString
keys inp = E.encodeUtf8 (foldMap key (inputKeys inp) <> prefix <> text)
  where
    mods = inputModifiers inp
    text = (if modCtrl mods then T.map (chr . (.&. 31) . ord . toUpper) else id) (inputChars inp)
    prefix = if modAlt mods && not (T.null text) then "\ESC" else ""
    key = \case
      KeyEnter -> "\r"; KeyBackspace -> "\DEL"; KeyTab -> "\t"; KeyEscape -> "\ESC"
      KeyUp -> "\ESC[A"; KeyDown -> "\ESC[B"; KeyRight -> "\ESC[C"; KeyLeft -> "\ESC[D"
      KeyHome -> "\ESC[H"; KeyEnd -> "\ESC[F"; KeyDelete -> "\ESC[3~"

main :: IO ()
main = withPty $ \fd -> do
  ctx0 <- newSdlContext
  let monoFont = FontSearch ["Input Mono", "JetBrains Mono", "Menlo", "monospace"]
  withSdl defaultSdlOptions
    {sdlWindowTitle = "nano-ui Terminal", sdlWindowSize = Size 816 592, sdlWindowResizable = False,
     sdlAppFontSize = 16, sdlAppFont = monoFont, sdlAppMonoFont = monoFont} ctx0 $ \ctx env -> do
    let advance (t, pending, _) = do
          threadDelay 16000
          events <- pollEvents
          rest <- send fd (pending <> foldMap (keys . applyEvent emptyInput) events)
          next :> ended <- drain fd (foldl' (\u e -> navigate u (applyEvent emptyInput e)) t events)
          pure (next, rest, ended || any (\case EvQuit -> True; _ -> False) events)
        paint (t, _, _) = void (sdlDrawFrame ctx (view t) env emptyInput True)
    S.mapM_ paint . S.takeWhile (\(_, _, ended) -> not ended) $ S.iterateM advance (pure (blank, B.empty, False))

view :: Term -> NanoUI ()
view t = void $ canvas (fontMono . grow $ defaultLayout) $ \(Rect x y w h) -> do
  drawRect (Rect x y w h) (colorRGBA 24 29 38 255)
  V.imapM_ (\i c -> when (c /= ' ') $ drawText (V2 (x + 8 + fromIntegral (i `mod` 80) * 10) (y + 8 + fromIntegral (i `div` 80) * 24))
    AlignStart AlignTop (T.singleton c) (colorRGBA 216 222 233 255)) (viewport t)
  let (cx, cy) = cursor t
  when (back t < 1) $ drawRect (Rect (x + 8 + fromIntegral (min 79 cx) * 10) (y + 29 + fromIntegral cy * 24) 10 2) (colorRGBA 129 161 193 255)
