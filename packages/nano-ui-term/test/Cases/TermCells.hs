module Cases.TermCells (runVtTest, runCellsTest) where

import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Builder (toLazyByteString)
import Data.IORef (IORef)
import Data.List (isInfixOf)
import Data.Text qualified as T
import NanoUI
import NanoUI.Testing
import NanoUI.Testing.Assert (assert, assertEq, withInput)
import NanoUI.Testing.Term
  ( MouseAction (..)
  , MouseBtn (..)
  , TermEvent (..)
  , cellRows
  , decode
  , flushPending
  , frameBytes
  , narrowChar
  , noMods
  , rasterize
  )

runVtTest :: Context -> IORef Int -> IO ()
runVtTest _ failed = do
  let evs s = fst (decode (BS8.pack s))
      leftover s = snd (decode (BS8.pack s))
  assertEq failed (evs "\ESC[<35;10;5M") [EvMouse MouseMove 9 4 noMods]
  assertEq failed (evs "\ESC[<35;10;5M\ESC[<0;12;6M") [EvMouse MouseMove 9 4 noMods, EvMouse (MousePress BtnLeft) 11 5 noMods]
  assertEq failed (evs "\ESC[?1;2p\ESC[<0;1;1M") [EvMouse (MousePress BtnLeft) 0 0 noMods]
  assertEq failed (evs "\ESC[<0;12;6m") [EvMouse (MouseRelease (Just BtnLeft)) 11 5 noMods]
  assertEq failed (evs "\ESC[<32;3;4M") [EvMouse (MouseDrag BtnLeft) 2 3 noMods]
  assertEq failed (evs "\ESC[<64;3;4M") [EvMouse MouseScrollUp 2 3 noMods]
  assertEq failed (evs "\ESC[<65;3;4M") [EvMouse MouseScrollDown 2 3 noMods]
  assertEq failed (evs "\ESC[<66;3;4M") []
  assertEq failed (evs "\ESC[<67;3;4M") []
  assertEq failed (evs "\ESC[MC*%") [EvMouse MouseMove 9 4 noMods]
  assertEq failed (evs "\ESC[M *%") [EvMouse (MousePress BtnLeft) 9 4 noMods]
  assertEq failed (evs "\ESC[M#*%") [EvMouse (MouseRelease Nothing) 9 4 noMods]
  assertEq failed (evs "\ESC[<35;10") []
  assertEq failed (leftover "\ESC[<35;10") (BS8.pack "\ESC[<35;10")
  assertEq failed (evs "\ESC") []
  assertEq failed (leftover "\ESC") (BS8.pack "\ESC")
  assertEq failed (flushPending (BS8.pack "\ESC")) [EvKey KeyEscape noMods]
  assertEq failed (evs "\ESC[A") [EvKey KeyUp noMods]
  assertEq failed (evs "\ESCOB") [EvKey KeyDown noMods]
  assertEq failed (evs "\ESC[3~") [EvKey KeyDelete noMods]
  assertEq failed (evs "\ESC[H") [EvKey KeyHome noMods]
  assertEq failed (evs "hi") [EvChar 'h' noMods, EvChar 'i' noMods]
  assertEq failed (evs "\r") [EvKey KeyEnter noMods]
  assertEq failed (evs "\DEL") [EvKey KeyBackspace noMods]
  assertEq failed (evs "\xc3\xa9") [EvChar '\233' noMods]
  assertEq failed (evs "\xc3") []

runCellsTest :: Context -> IORef Int -> IO ()
runCellsTest ctx failed = do
  let inp = withInput 200 80
      ui = column (defaultLayout {layoutWidth = Grow 1, layoutHeight = Grow 1}) (label "hello")
  (_, _, draw, _) <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  cells <- rasterize 40 10 draw spans
  let rows = cellRows cells
  assertEq failed (length rows) 10
  assert failed (any (isInfixOf "hello") rows)
  assert failed (BL.null (toLazyByteString (frameBytes (Just cells) cells)))
  assert failed (not (BL.null (toLazyByteString (frameBytes Nothing cells))))
  assert failed (narrowChar '\x2502' && narrowChar '\x2588' && narrowChar '\x2591' && not (narrowChar '\x4E00'))
  assert failed (all narrowChar (concatMap T.unpack (glyphIconTexts glyphIcons)))
  assertEq failed (terminalTextColumns "\xf046") 2
  assertEq failed (terminalPaintColumns "\xf046") 1
  assertEq failed (terminalPaintColumns (iconClose glyphIcons)) 1
  assertEq failed (terminalTextColumns (iconClose glyphIcons)) 2
  assertEq failed (terminalTextColumns (iconChecked glyphIcons)) 3
  assertEq failed (terminalPaintColumns (iconChecked glyphIcons)) 3
  assertEq failed (terminalTextColumns (iconUnchecked glyphIcons)) 3

glyphIconTexts :: Icons -> [T.Text]
glyphIconTexts icons =
  [ iconChecked icons
  , iconUnchecked icons
  , iconClose icons
  , iconSelectOpen icons
  , iconSelectClosed icons
  , iconScrollUp icons
  , iconScrollDown icons
  , iconWindowTitle icons
  , iconModalTitle icons
  ]
