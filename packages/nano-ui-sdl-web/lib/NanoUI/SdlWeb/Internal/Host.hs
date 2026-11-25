{-# LANGUAGE CPP #-}

-- | What a session asks of the page it runs in: the canvas, the event
-- queue, WebGL, the text input element and the clipboard. 'browserHost'
-- calls @web/nano-ui-sdl-web.js@ through GHC's JavaScript FFI when built
-- with @wasm32-wasi-ghc@. A native build has the same module with every
-- browser call failing, so the rest of the package builds and its tests
-- run against a 'WebHost' of their own.
--
-- FFI conventions. The JavaScript imports are @unsafe@, synchronous calls,
-- except the wait for events, the font queries and the file dialogs: a
-- @safe@ import runs as an async function, so the calling Haskell thread
-- waits for its promise while the browser's event loop runs and delivers
-- the events. Pointers passed to the page point into the module's memory, which the page reads and writes through views of it.
-- Text crosses as UTF-8 bytes and a length. The wake ('whWake') is safe from
-- any Haskell thread. The C imports in "NanoUI.SdlWeb.Internal.Font" are
-- @unsafe@ calls into the text atlas.
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

import Data.Int (Int32)
import Data.Word (Word8)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Ptr (Ptr)
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
  , whRegisterShader :: Int -> Ptr Word8 -> Int -> IO Bool
  -- ^ Compile and link a GLSL ES 300 fragment shader under shader slot
  -- @slot@ (@>= 1@), returning whether compilation and linking succeeded.
  , whUploadUniforms :: Ptr Word8 -> Int -> IO ()
  -- ^ Copy the frame's std140 shader uniform bytes to the host before 'whDraw'.
  , whAcquireTarget :: Int -> Int -> Ptr Int32 -> IO Int
  -- ^ Acquire a 64px-bucketed pooled RGBA8 offscreen WebGL2 framebuffer,
  -- writing its physical @(capW, capH)@ into the 2-int buffer and returning
  -- its non-negative slot index (or -1 on failure).
  , whReleaseTarget :: Int -> IO ()
  -- ^ Return a pooled offscreen target slot to the free pool.
  , whBindTarget :: Int -> Bool -> Double -> Double -> IO ()
  -- ^ Bind pooled target @slot >= 0@, or the default canvas backbuffer when
  -- @slot < 0@, optionally clearing to transparent black and setting logical
  -- vertex origin @(ox, oy)@.
  , whCompositeEffect :: Int -> Int -> Bool -> Ptr Float -> Ptr Int32 -> Ptr Word8 -> Int -> IO ()
  -- ^ Composite pooled texture @srcSlot@ through shader @shaderSlot@ onto the
  -- bound target. @blendCopy@ disables blending for intermediate passes;
  -- @rectUvPtr@ holds 8 floats @(dx, dy, dw, dh, u0, v0, u1, v1)@;
  -- @scissorPtr@ holds 5 ints @(hasScissor, sx, sy, sw, sh)@; @uPtr/uLen@
  -- holds the pass's patched std140 uniform block.
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
  , whRendererInfo :: Ptr Word8 -> Int -> IO Int
  -- ^ Copy up to @n@ bytes of the WebGL renderer's description into the
  -- buffer, and answer its full length.
  , whFindFonts :: IO Int
  -- ^ Ask the browser for the installed font families (the Local Font
  -- Access API, which asks the user). Answers the byte length of their
  -- names, one a line, for 'whTakeFontList'; -1 when the browser has no
  -- such API, -2 when the user or the page's policy declined.
  , whTakeFontList :: Ptr Word8 -> IO ()
  -- ^ Copy the names 'whFindFonts' found into the buffer.
  , whLoadFont :: Int -> Ptr Word8 -> Int -> IO Int
  -- ^ Load a font file's bytes: an installed family's regular face (kind
  -- 0), a URL (kind 1), or a family by name (kind 2: installed when the user
  -- has allowed it, else from the font service), named in UTF-8. Answers
  -- their length for 'whTakeFontBytes', or -1 when it could not be loaded.
  , whTakeFontBytes :: Ptr Word8 -> IO ()
  -- ^ Copy the bytes 'whLoadFont' loaded into the buffer.
  , whStartFontDiscovery :: Int -> IO ()
  -- ^ Start finding font families, once: the installed ones (at once when
  -- the user has allowed it; while the browser has not asked yet, at the
  -- next press on the page) and a font service's catalogue (0 none, 1
  -- Fontsource). Each arrival bumps 'whFontsVersion' and wakes the session.
  , whFontsVersion :: IO Int
  -- ^ How many times the known families changed.
  , whFontFamilies :: Ptr Word8 -> Int -> IO Int
  -- ^ Copy up to @n@ bytes of the known families (installed first, one a
  -- line) into the buffer, and answer the full length.
  , whPickFiles :: Int -> Bool -> Ptr Word8 -> Int -> IO Int
  -- ^ Show the browser's file chooser: files (mode 0) or a folder's files
  -- (mode 1), several or one, with the filters as UTF-8 lines of a name, a
  -- tab and a semicolon-separated extension list. Answers the byte length
  -- of the files it read, for 'whTakePicked': each a 32-bit little-endian
  -- name length, the name, a 32-bit length and the bytes. -1 when the user
  -- cancelled, -2 when it failed.
  , whSaveFile :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr Word8 -> Int -> IO Int
  -- ^ Save bytes as a file: the suggested name, the filters (as for
  -- 'whPickFiles'), then the bytes. Where the browser has a save dialog it
  -- shows it, else it downloads the file under the suggested name. Answers
  -- the byte length of the name saved under, for 'whTakePicked'; -1 when
  -- the user cancelled, -2 when it failed.
  , whTakePicked :: Ptr Word8 -> IO ()
  -- ^ Copy what 'whPickFiles' or 'whSaveFile' answered into the buffer.
  }

-- | Bytes in one event record: kind, three integer fields and the
-- modifiers as 32-bit integers, then three 32-bit floats.
-- "NanoUI.SdlWeb.Internal.Input" reads them.
eventRecordSize :: Int
eventRecordSize = 32

-- | Bytes in one draw command record, twelve 32-bit integers: texture slot
-- (-1 for none), first index, index count, whether to scissor, the scissor
-- rect in device pixels from the top left (x, y, width, height), shader slot
-- (0 for standard geometry), uniform byte offset, uniform byte length, and a
-- reserved word.
commandRecordSize :: Int
commandRecordSize = 48

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
      , whRegisterShader = \slot p n -> (/= 0) <$> jsRegisterShader slot p n
      , whUploadUniforms = jsUploadUniforms
      , whAcquireTarget = jsAcquireTarget
      , whReleaseTarget = jsReleaseTarget
      , whBindTarget = \slot clear ox oy -> jsBindTarget slot (fromEnum clear) ox oy
      , whCompositeEffect = \srcSlot shSlot blendCopy rPtr sPtr uPtr uLen ->
          jsCompositeEffect srcSlot shSlot (fromEnum blendCopy) rPtr sPtr uPtr uLen
      , whSetCursor = jsSetCursor
      , whTextInput = jsTextInput
      , whClipboardRead = jsClipboardRead
      , whClipboardWrite = jsClipboardWrite
      , whSetTitle = jsSetTitle
      , whRendererInfo = jsRendererInfo
      , whFindFonts = evaluate =<< jsFindFonts
      , whTakeFontList = jsTakeFontList
      , whLoadFont = \kind p n -> evaluate =<< jsLoadFont kind p n
      , whTakeFontBytes = jsTakeFontBytes
      , whStartFontDiscovery = jsStartFontDiscovery
      , whFontsVersion = jsFontsVersion
      , whFontFamilies = jsFontFamilies
      , whPickFiles = \mode many p n -> evaluate =<< jsPickFiles mode (fromEnum many) p n
      , whSaveFile = \np nn fp fn dp dn -> evaluate =<< jsSaveFile np nn fp fn dp dn
      , whTakePicked = jsTakePicked
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

foreign import javascript unsafe "globalThis.nanoUiSdlWeb.registerShader($1, $2, $3)"
  jsRegisterShader :: Int -> Ptr Word8 -> Int -> IO Int

foreign import javascript unsafe "globalThis.nanoUiSdlWeb.uploadUniforms($1, $2)"
  jsUploadUniforms :: Ptr Word8 -> Int -> IO ()

foreign import javascript unsafe "globalThis.nanoUiSdlWeb.acquireTarget($1, $2, $3)"
  jsAcquireTarget :: Int -> Int -> Ptr Int32 -> IO Int

foreign import javascript unsafe "globalThis.nanoUiSdlWeb.releaseTarget($1)"
  jsReleaseTarget :: Int -> IO ()

foreign import javascript unsafe "globalThis.nanoUiSdlWeb.bindTarget($1, $2, $3, $4)"
  jsBindTarget :: Int -> Int -> Double -> Double -> IO ()

foreign import javascript unsafe "globalThis.nanoUiSdlWeb.compositeEffect($1, $2, $3, $4, $5, $6, $7)"
  jsCompositeEffect :: Int -> Int -> Int -> Ptr Float -> Ptr Int32 -> Ptr Word8 -> Int -> IO ()

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

foreign import javascript unsafe "globalThis.nanoUiSdlWeb.rendererInfo($1, $2)"
  jsRendererInfo :: Ptr Word8 -> Int -> IO Int

foreign import javascript safe "return await globalThis.nanoUiSdlWeb.findFonts();"
  jsFindFonts :: IO Int

foreign import javascript unsafe "globalThis.nanoUiSdlWeb.takeFontList($1)"
  jsTakeFontList :: Ptr Word8 -> IO ()

foreign import javascript safe "return await globalThis.nanoUiSdlWeb.loadFont($1, $2, $3);"
  jsLoadFont :: Int -> Ptr Word8 -> Int -> IO Int

foreign import javascript unsafe "globalThis.nanoUiSdlWeb.takeFontBytes($1)"
  jsTakeFontBytes :: Ptr Word8 -> IO ()

foreign import javascript unsafe "globalThis.nanoUiSdlWeb.startFontDiscovery($1)"
  jsStartFontDiscovery :: Int -> IO ()

foreign import javascript unsafe "globalThis.nanoUiSdlWeb.fontsVersionNow()"
  jsFontsVersion :: IO Int

foreign import javascript unsafe "globalThis.nanoUiSdlWeb.fontFamilies($1, $2)"
  jsFontFamilies :: Ptr Word8 -> Int -> IO Int

foreign import javascript safe "return await globalThis.nanoUiSdlWeb.pickFiles($1, $2, $3, $4);"
  jsPickFiles :: Int -> Int -> Ptr Word8 -> Int -> IO Int

foreign import javascript safe "return await globalThis.nanoUiSdlWeb.saveFile($1, $2, $3, $4, $5, $6);"
  jsSaveFile :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr Word8 -> Int -> IO Int

foreign import javascript unsafe "globalThis.nanoUiSdlWeb.takePicked($1)"
  jsTakePicked :: Ptr Word8 -> IO ()
#else
browserAvailable = False

browserHost = ioError (userError "nano-ui-sdl-web runs in a browser: build the program with wasm32-wasi-ghc")
#endif
