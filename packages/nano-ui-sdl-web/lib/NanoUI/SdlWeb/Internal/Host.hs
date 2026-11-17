{-# LANGUAGE CPP #-}

-- | What a session asks of the page it runs in: the canvas, the event
-- queue, WebGL, the text input element and the clipboard. 'browserHost'
-- calls @web/nano-ui-sdl-web.js@ through GHC's JavaScript FFI when built
-- with @wasm32-wasi-ghc@. A native build has the same module with every
-- browser call failing, so the rest of the package builds and its tests
-- run against a 'WebHost' of their own.
module NanoUI.SdlWeb.Internal.Host
  ( WebHost (..)
  , browserHost
  , browserAvailable
    -- * Event records
  , eventRecordSize
    -- * Draw command records
  , commandRecordSize
  , imageAtlasSlot
    -- * Canvas facts
  , CanvasInfo (..)
  , queryCanvas
  ) where

import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Ptr (Ptr)
import Data.Word (Word8)
#if defined(wasm32_HOST_ARCH)
import Control.Exception (evaluate)
import Control.Monad (void)
#endif

-- | The page's side of a session. Pointers are into the module's memory;
-- the page reads and writes through views of it. Text goes across as UTF-8
-- bytes and a length.
data WebHost = WebHost
  { whQueryCanvas :: Ptr Float -> IO ()
  -- ^ Write five floats: the canvas's CSS width and height, the device
  -- pixel ratio, 1 when the page has keyboard focus (else 0), and the
  -- colour scheme the page prefers (0 none, 1 light, 2 dark).
  , whTakeEvents :: Ptr Word8 -> Int -> IO Int
  -- ^ Move up to @n@ queued events into the buffer, 'eventRecordSize'
  -- bytes each, and answer how many.
  , whWaitEvents :: Int -> IO ()
  -- ^ Return once an event is queued, a wake ('whWake') arrived, or the
  -- milliseconds passed; a negative wait has no limit. Returns at once when
  -- either is already pending.
  , whWake :: IO ()
  -- ^ End the current wait, or the next one. Safe from any Haskell thread.
  , whResizeBacking :: Int -> Int -> IO ()
  -- ^ Size the canvas's drawing buffer in device pixels.
  , whUploadTexture :: Int -> Int -> Int -> Ptr Word8 -> Int -> Int -> Int -> Int -> Int -> IO ()
  -- ^ Texture slot, its width and height, RGBA pixels and their row length
  -- in pixels, then the rect to copy (x, y, width, height). The texture is
  -- made, or remade at a new size, before the copy.
  , whBeginFrame :: Bool -> Double -> Double -> Double -> Double -> Double -> Int -> Int -> IO ()
  -- ^ Whether to clear first, the clear colour (red, green, blue, alpha,
  -- 0 to 1), the scale from layout units to device pixels, and the
  -- drawing buffer's width and height.
  , whDraw :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr Word8 -> Int -> IO ()
  -- ^ Vertices and their count (32 bytes each: position, colour, UV, as
  -- 32-bit floats), indices and their count (32-bit), then draw commands
  -- and their count ('commandRecordSize' bytes each).
  , whSetCursor :: Ptr Word8 -> Int -> IO ()
  -- ^ The canvas's CSS cursor, @none@ to hide it.
  , whTextInput :: Int -> Double -> Double -> Double -> Double -> IO ()
  -- ^ Start or move text input for a purpose (0 text, 1 password, 2
  -- number) over a rect in CSS pixels, so the input method's window sits
  -- at the caret; a negative purpose stops it.
  , whClipboardRead :: Ptr Word8 -> Int -> IO Int
  -- ^ Copy up to @n@ bytes of the clipboard text the page last saw into
  -- the buffer, and answer its full length, or -1 when it has seen none.
  , whClipboardWrite :: Ptr Word8 -> Int -> IO ()
  , whSetTitle :: Ptr Word8 -> Int -> IO ()
  }

-- | Bytes in one event record: kind, three integer fields and the
-- modifiers as 32-bit integers, then three 32-bit floats.
-- "NanoUI.SdlWeb.Internal.Input" reads them.
eventRecordSize :: Int
eventRecordSize = 32

-- | Bytes in one draw command record, eight 32-bit integers: texture slot
-- (-1 for none), first index, index count, whether to scissor, and the
-- scissor rect in device pixels from the top left (x, y, width, height).
commandRecordSize :: Int
commandRecordSize = 32

-- | The texture slot of the image atlas. Slots 0 to 3 are the glyph pages.
imageAtlasSlot :: Int
imageAtlasSlot = 4

-- | What 'whQueryCanvas' reports.
data CanvasInfo = CanvasInfo
  { ciWidth :: !Float
  , ciHeight :: !Float
  , ciPixelRatio :: !Float
  , ciFocused :: !Bool
  , ciScheme :: !Int
  }
  deriving (Eq, Show)

queryCanvas :: WebHost -> IO CanvasInfo
queryCanvas host = allocaArray 5 $ \p -> do
  whQueryCanvas host p
  peekArray 5 p >>= \case
    [w, h, ratio, focused, scheme] -> pure (CanvasInfo w h (if ratio > 0 then ratio else 1) (focused /= 0) (round scheme))
    _ -> pure (CanvasInfo 0 0 1 False 0)

-- | Whether 'browserHost' reaches a browser: a @wasm32@ build.
browserAvailable :: Bool

-- | The page's own host, @globalThis.nanoUiSdlWeb@ from
-- @web/nano-ui-sdl-web.js@.
browserHost :: IO WebHost

#if defined(wasm32_HOST_ARCH)
browserAvailable = True

browserHost = do
  jsAttach
  pure
    WebHost
      { whQueryCanvas = jsQueryCanvas
      , whTakeEvents = jsTakeEvents
      , whWaitEvents = \ms -> void (evaluate =<< jsWaitEvents ms)
      , whWake = jsWake
      , whResizeBacking = jsResizeBacking
      , whUploadTexture = jsUploadTexture
      , whBeginFrame = \clear r g b a scale w h -> jsBeginFrame (fromEnum clear) r g b a scale w h
      , whDraw = jsDraw
      , whSetCursor = jsSetCursor
      , whTextInput = jsTextInput
      , whClipboardRead = jsClipboardRead
      , whClipboardWrite = jsClipboardWrite
      , whSetTitle = jsSetTitle
      }

foreign import javascript unsafe "globalThis.nanoUiSdlWeb.attach(__exports.memory)"
  jsAttach :: IO ()

foreign import javascript unsafe "globalThis.nanoUiSdlWeb.queryCanvas($1)"
  jsQueryCanvas :: Ptr Float -> IO ()

foreign import javascript unsafe "globalThis.nanoUiSdlWeb.takeEvents($1, $2)"
  jsTakeEvents :: Ptr Word8 -> Int -> IO Int

-- A safe import runs as an async function: the calling Haskell thread waits
-- for its promise, and the browser's event loop runs meanwhile, which is
-- what delivers the events.
foreign import javascript safe "await globalThis.nanoUiSdlWeb.waitEvents($1); return 0;"
  jsWaitEvents :: Int -> IO Int

foreign import javascript unsafe "globalThis.nanoUiSdlWeb.wake()"
  jsWake :: IO ()

foreign import javascript unsafe "globalThis.nanoUiSdlWeb.resizeBacking($1, $2)"
  jsResizeBacking :: Int -> Int -> IO ()

foreign import javascript unsafe "globalThis.nanoUiSdlWeb.uploadTexture($1, $2, $3, $4, $5, $6, $7, $8, $9)"
  jsUploadTexture :: Int -> Int -> Int -> Ptr Word8 -> Int -> Int -> Int -> Int -> Int -> IO ()

foreign import javascript unsafe "globalThis.nanoUiSdlWeb.beginFrame($1, $2, $3, $4, $5, $6, $7, $8)"
  jsBeginFrame :: Int -> Double -> Double -> Double -> Double -> Double -> Int -> Int -> IO ()

foreign import javascript unsafe "globalThis.nanoUiSdlWeb.draw($1, $2, $3, $4, $5, $6)"
  jsDraw :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr Word8 -> Int -> IO ()

foreign import javascript unsafe "globalThis.nanoUiSdlWeb.setCursor($1, $2)"
  jsSetCursor :: Ptr Word8 -> Int -> IO ()

foreign import javascript unsafe "globalThis.nanoUiSdlWeb.textInput($1, $2, $3, $4, $5)"
  jsTextInput :: Int -> Double -> Double -> Double -> Double -> IO ()

foreign import javascript unsafe "globalThis.nanoUiSdlWeb.clipboardRead($1, $2)"
  jsClipboardRead :: Ptr Word8 -> Int -> IO Int

foreign import javascript unsafe "globalThis.nanoUiSdlWeb.clipboardWrite($1, $2)"
  jsClipboardWrite :: Ptr Word8 -> Int -> IO ()

foreign import javascript unsafe "globalThis.nanoUiSdlWeb.setTitle($1, $2)"
  jsSetTitle :: Ptr Word8 -> Int -> IO ()
#else
browserAvailable = False

browserHost = ioError (userError "nano-ui-sdl-web runs in a browser: build the program with wasm32-wasi-ghc")
#endif
