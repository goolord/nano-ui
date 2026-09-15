module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (unless, void)
import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Text.Encoding qualified as E
import Data.Vector.Unboxed qualified as V
import SdlTerminal hiding (main)
import Streaming (Of (..))
import System.Timeout (timeout)

check :: String -> Bool -> IO ()
check name ok = unless ok (fail name)

main :: IO ()
main = do
  let
    run = feed blank
    at x y t = screen t V.! (y * 80 + x)
    charAt x y t = let (c, _, _) = at x y t in c
    text = T.pack . V.toList . V.map (\(c, _, _) -> c) . screen
    linesOf ns = E.encodeUtf8 (T.concat [T.pack (show n) <> "\r\n" | n <- ns :: [Int]])
    equal a b =
      screen a == screen b
        && cursor a == cursor b
        && pen a == pen b
        && escape a == escape b
        && utf8 a == utf8 b
        && history a == history b
        && back a == back b
    sample = E.encodeUtf8 "abc\rZ\ESC[2;4Héλ\ESC[;H!\ESC]0;hidden\ESC\\\ESC[7;31m.\ESCc☃"
  check "UTF-8 / escape chunk boundaries" $
    all
      (\i -> equal (run sample) (feed (run (B.take i sample)) (B.drop i sample)))
      [0 .. B.length sample]
  check "RI consumes escape, scrolls down at top and moves up below it" $
    charAt 0 0 (run "A\r\ESCMZ") == 'Z' && charAt 0 1 (run "A\r\ESCMZ") == 'A'
      && charAt 3 1 (run "\ESC[3;4H\ESCMZ") == 'Z'
  check "backspace cancels pending wrap" $
    cursor (run (B.replicate 80 120 <> "\b")) == (78, 0)
  let
    bottom = run ("\ESC[24;1H" <> B.replicate 80 120 <> "\nY")
  check "LF at bottom cancels pending wrap" $
    V.length (history bottom) == 80 && charAt 79 23 bottom == 'Y'
  check "omitted CSI fields keep their position" $
    cursor (run "\ESC[;5H") == (4, 0)
  check "huge arguments clamp before Int conversion" $
    cursor (run "\ESC[999999999999999999999999999999;1H") == (0, 23)
  mapM_
    ( \cmd ->
        check
          "invalid erase leaves content intact"
          (screen (run ("abc" <> cmd)) == screen (run "abc"))
    )
    ["\ESC[9J", "\ESC[9K", "\ESC[?2J", "\ESC[-1K"]
  check "ESC restarts a partial CSI" $
    screen (run "abc\ESC[12\ESC[2J") == screen blank
  check "CAN cancels CSI" $ charAt 0 0 (run "\ESC[12\CANZ") == 'Z'
  check "oversized CSI stays consumed" $
    screen (run ("\ESC[" <> B.replicate 1000 57 <> "H")) == screen blank
  mapM_
    (\(name, sgr, same) -> check name (at 0 0 (run sgr) == at 0 0 (run same)))
    [ ("repeated inverse is idempotent", "\ESC[7;7mX", "\ESC[7mX")
    , ("inverse reset restores defaults", "\ESC[7;27mX", "X")
    , ("normal intensity retains explicit bright color", "\ESC[91;22mX", "\ESC[91mX")
    , ("bold reset restores default foreground", "\ESC[1;22mX", "X")
    , ("empty SGR parameter resets pen", "\ESC[31;;1mX", "\ESC[1mX")
    , ("extended color payload is not interpreted as SGR", "\ESC[38;5;1;48;2;0;7;22mX", "X")
    ]
  let
    (_, fg, _) = at 0 0 (run "X")
    (_, _, bg) = at 0 0 (run "\ESC[44mX")
  check "erase and scrolling retain background" $
    at 0 0 (run "\ESC[44m\ESC[2J") == (' ', fg, bg)
      && at 0 23 (run "\ESC[44m\ESC[24;1H\n") == (' ', fg, bg)
  let
    old = scrollBy 3 (run (linesOf [0 .. 29])); cleared = feed old "\ESC[3J"
  check "clear history preserves live screen and resets view" $
    V.null (history cleared) && back cleared == 0 && viewport cleared == screen old
  check "new output anchors scrollback" $
    viewport (feed old "extra\r\n") == viewport old
  let
    capped = feed (scrollBy 9999 (run (linesOf [0 .. 2024]))) "next\r\n"
  check "history cap keeps oldest viewport valid" $
    V.length (history capped) <= 2000 * 80
      && floor (back capped) * 80 == V.length (history capped)
      && V.length (viewport capped) == V.length (screen blank)

  result <- timeout 8000000 $ withPty $ \fd -> do
    let
      write bytes =
        send fd bytes >>= \rest -> unless (B.null rest) (threadDelay 1000 >> write rest)
      await predicate t = do
        u :> ended <- drain fd t
        if predicate u ended then pure u else threadDelay 10000 >> await predicate u
      contains needle t _ = needle `T.isInfixOf` text t
    write "stty -echo; PS1=''; printf '\\033[2J\\033[HREADY\\n'\r"
    ready <- await (\t _ -> "READY" `T.isPrefixOf` text t) blank
    idle <- timeout 100000 (drain fd ready)
    check "idle drain is nonblocking" (maybe False (\(_ :> eof) -> not eof) idle)
    write
      "stty size; test -t 0 && test -t 1 && test -t 2 && printf 'TTY_OK:%s\\n' \"$TERM\"\r"
    sized <- await (contains "TTY_OK:ansi") ready
    check "PTY size" (contains "24 80" sized False)
    write "sleep 30\r"
    threadDelay 100000
    write "\ETX"
    write "printf 'INTERRUPTED\\n'\r"
    stopped <- await (contains "INTERRUPTED") sized
    write "\EOT"
    void (await (\_ eof -> eof) stopped)
  check "PTY integration timeout" (maybe False (const True) result)
  putStrLn "terminal regression checks passed"
