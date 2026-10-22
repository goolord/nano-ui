module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (unless, void)
import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Text.Encoding qualified as E
import Data.Vector.Unboxed qualified as V
import NanoUI (Input (..), Key (..), Modifiers (..), noModifiers)
import NanoUI.Backend (emptyInput, inputKeysFromList)
import NanoUI.Sdl.Internal.Input (SdlEvent (..))
import Terminal hiding (main)
import Streaming (Of (..))
import System.Info (os)
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
    sample = E.encodeUtf8 "abc\rZ\ESC[2;4Héλ\ESC[;H!\ESC]0;hidden\ESC\\\ESC[7;31m.\ESCc☃"
  check "UTF-8 / escape chunk boundaries" $
    all
      (\i -> run sample == feed (run (B.take i sample)) (B.drop i sample))
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
  check "ESC ends an unterminated OSC" $ charAt 0 0 (run "\ESC]0;title\ESC[1mZ") == 'Z'
  let
    row = T.take 6 . text
    column y0 y1 t = map (\y -> charAt 0 y t) [y0 .. y1]
  check "insert, delete and erase characters edit the cursor row" $
    row (run "abcd\ESC[1;3H\ESC[@X") == "abXcd "
      && row (run "abcd\ESC[1;2H\ESC[2P") == "ad    "
      && row (run "abcd\ESC[1;2H\ESC[2X") == "a  d  "
      && charAt 79 0 (run "\ESC[1;80Hz\ESC[H\ESC[@") == ' '
  check "insert and delete lines move the rows below the cursor" $
    column 0 3 (run "A\r\nB\r\nC\ESC[2;3H\ESC[L") == "A BC"
      && cursor (run "A\r\nB\r\nC\ESC[2;3H\ESC[L") == (0, 1)
      && column 0 2 (run "A\r\nB\r\nC\ESC[2H\ESC[M") == "AC "
  check "scroll up and down move the whole screen, and REP repeats the last glyph" $
    column 0 2 (run "A\r\nB\r\nC\ESC[S") == "BC "
      && cursor (run "A\r\nB\r\nC\ESC[S") == (1, 2)
      && column 0 4 (run "A\r\nB\r\nC\ESC[2T") == "  ABC"
      && row (run "-\ESC[4b") == "----- "
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
  let
    pressed mods k = keys emptyInput {inputKeys = inputKeysFromList [k], inputModifiers = mods}
  check "function keys and Shift+Tab send xterm's sequences" $
    map (pressed noModifiers) [KeyF 1, KeyF 5, KeyF 12, KeyTab] == ["\ESCOP", "\ESC[15~", "\ESC[24~", "\t"]
      && pressed noModifiers {modShift = True} KeyTab == "\ESC[Z"
  let ctrl = noModifiers {modCtrl = True}
  check "Ctrl sends a control code, or nothing for a key without one" $
    map (pressed ctrl . KeyChar) "c[6-0," == ["\ETX", "\ESC", "\RS", "", "", ""]
      && pressed ctrl {modShift = True} (KeyChar '-') == "\US"
  check "a frame's text goes before its command key" $
    keys emptyInput {inputChars = "ls", inputKeys = inputKeysFromList [KeyChar 'l', KeyChar 's', KeyEnter]} == "ls\r"
  let
    typed = snd . foldl' typeEvent (emptyInput, B.empty)
  check "text keeps the modifiers of the key that typed it" $
    typed [EvKey (KeyChar 'b') True noModifiers {modAlt = True}, EvText "b"] == (if os == "darwin" then "b" else "\ESCb")
      && typed [EvKey (KeyChar 'c') True ctrl, EvText "c"] == "\ETX"

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
