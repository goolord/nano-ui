-- | Foreign imports of RGFW and the accessor functions in @cbits/RGFW.c@, plus
-- the event type, mouse button, key, modifier and window flag constants.
module RGFW.Raw
  ( RGFW_window
  , RGFW_event
  , c_RGFW_window_close
  , c_RGFW_window_checkEvent
  , c_RGFW_waitForEvent
  , c_rgfw_create_window_gl
  , c_RGFW_window_swapBuffers_OpenGL
  , c_rgfw_event_type
  , c_rgfw_event_mouse_x
  , c_rgfw_event_mouse_y
  , c_rgfw_event_button_value
  , c_rgfw_event_delta_x
  , c_rgfw_event_delta_y
  , c_rgfw_event_key_value
  , c_rgfw_event_key_mod
  , c_rgfw_event_keyChar_value
  , c_rgfw_event_update_w
  , c_rgfw_event_update_h
  , c_rgfw_event_size
  , c_rgfw_window_w
  , c_rgfw_window_h
  , c_rgfw_window_scale
  , c_rgfw_event_scale_x
  , c_rgfw_event_scale_y
  , c_rgfw_read_clipboard_text
  , c_rgfw_write_clipboard_text
  -- Event types
  , rgfw_eventNone
  , rgfw_keyPressed
  , rgfw_keyReleased
  , rgfw_keyChar
  , rgfw_mouseButtonPressed
  , rgfw_mouseButtonReleased
  , rgfw_mouseScroll
  , rgfw_mouseMotion
  , rgfw_windowMoved
  , rgfw_windowResized
  , rgfw_windowFocusIn
  , rgfw_windowFocusOut
  , rgfw_scaleUpdated
  , rgfw_windowClose
  -- Mouse buttons
  , rgfw_mouseLeft
  , rgfw_mouseMiddle
  , rgfw_mouseRight
  -- Keys
  , rgfw_keyBackSpace
  , rgfw_keyTab
  , rgfw_keyReturn
  , rgfw_keyEscape
  , rgfw_keyDelete
  , rgfw_keyA
  , rgfw_keyZ
  , rgfw_keyUp
  , rgfw_keyDown
  , rgfw_keyLeft
  , rgfw_keyRight
  , rgfw_keyEnd
  , rgfw_keyHome
  -- Key modifier bits
  , rgfw_modCapsLock
  , rgfw_modNumLock
  , rgfw_modControl
  , rgfw_modAlt
  , rgfw_modShift
  , rgfw_modSuper
  , rgfw_modScrollLock
  -- Window flags
  , rgfw_windowCenter
  , rgfw_windowHide
  -- Mouse cursors
  , c_rgfw_window_set_mouse_standard
  , c_rgfw_window_set_mouse_default
  , rgfw_mouseNormal
  , rgfw_mouseArrow
  , rgfw_mouseIbeam
  , rgfw_mouseCrosshair
  , rgfw_mousePointingHand
  , rgfw_mouseResizeEW
  , rgfw_mouseResizeNS
  , rgfw_mouseResizeNWSE
  , rgfw_mouseResizeNESW
  , rgfw_mouseResizeNW
  , rgfw_mouseResizeN
  , rgfw_mouseResizeNE
  , rgfw_mouseResizeE
  , rgfw_mouseResizeSE
  , rgfw_mouseResizeS
  , rgfw_mouseResizeSW
  , rgfw_mouseResizeW
  , rgfw_mouseResizeAll
  ) where

import Data.Word (Word8, Word32)
import Foreign.C.String (CString)
import Foreign.C.Types (CFloat (..), CInt (..), CSize (..), CUChar (..), CUInt (..))
import Foreign.Ptr (Ptr)

-- | Opaque native window. The creating thread owns its lifetime.
data RGFW_window
-- | Native tagged event union. Allocate 'c_rgfw_event_size' bytes and read
-- only the fields valid for the tag returned by 'c_rgfw_event_type'.
data RGFW_event

-- Foreign function imports
-- | Destroy a live window and its OpenGL context. Do not reuse the pointer.
foreign import ccall "RGFW_window_close"
  c_RGFW_window_close :: Ptr RGFW_window -> IO ()

-- | Poll into caller-owned event storage. Zero means no event was written.
foreign import ccall unsafe "RGFW_window_checkEvent"
  c_RGFW_window_checkEvent :: Ptr RGFW_window -> Ptr RGFW_event -> IO CUChar

-- | Wait in milliseconds: negative blocks indefinitely; zero does not wait.
foreign import ccall "RGFW_waitForEvent"
  c_RGFW_waitForEvent :: CInt -> IO ()

-- | Create title/x/y/width/height/flags with a core OpenGL major/minor version.
-- Returns null on failure; the resulting context is current on this OS thread.
foreign import ccall "rgfw_create_window_gl"
  c_rgfw_create_window_gl :: CString -> CInt -> CInt -> CInt -> CInt -> CUInt -> CInt -> CInt -> IO (Ptr RGFW_window)

-- | Present the window's OpenGL back buffer on the context's owning thread.
foreign import ccall "RGFW_window_swapBuffers_OpenGL"
  c_RGFW_window_swapBuffers_OpenGL :: Ptr RGFW_window -> IO ()

-- Accessors (cbits/RGFW.c)
-- | Event tag; inspect it before reading fields of the event union.
foreign import ccall unsafe "rgfw_event_type"
  c_rgfw_event_type :: Ptr RGFW_event -> IO CUChar

-- | Mouse-motion x coordinate in native window pixels.
foreign import ccall unsafe "rgfw_event_mouse_x"
  c_rgfw_event_mouse_x :: Ptr RGFW_event -> IO CInt

-- | Mouse-motion y coordinate in native window pixels.
foreign import ccall unsafe "rgfw_event_mouse_y"
  c_rgfw_event_mouse_y :: Ptr RGFW_event -> IO CInt

-- | Button code from a mouse-button event; see 'rgfw_mouseLeft'.
foreign import ccall unsafe "rgfw_event_button_value"
  c_rgfw_event_button_value :: Ptr RGFW_event -> IO CUChar

-- | Horizontal wheel delta from a scroll event.
foreign import ccall unsafe "rgfw_event_delta_x"
  c_rgfw_event_delta_x :: Ptr RGFW_event -> IO CFloat

-- | Vertical wheel delta from a scroll event.
foreign import ccall unsafe "rgfw_event_delta_y"
  c_rgfw_event_delta_y :: Ptr RGFW_event -> IO CFloat

-- | RGFW key code from a key press or release, not a Unicode text character.
foreign import ccall unsafe "rgfw_event_key_value"
  c_rgfw_event_key_value :: Ptr RGFW_event -> IO CUInt

-- | Modifier bit mask from a key event; test with the @rgfw_mod*@ constants.
foreign import ccall unsafe "rgfw_event_key_mod"
  c_rgfw_event_key_mod :: Ptr RGFW_event -> IO CUChar

-- | Code point from a character event. Validate it before converting to 'Char'.
foreign import ccall unsafe "rgfw_event_keyChar_value"
  c_rgfw_event_keyChar_value :: Ptr RGFW_event -> IO CUInt

-- | Native pixel width carried by a resize event.
foreign import ccall unsafe "rgfw_event_update_w"
  c_rgfw_event_update_w :: Ptr RGFW_event -> IO CInt

-- | Native pixel height carried by a resize event.
foreign import ccall unsafe "rgfw_event_update_h"
  c_rgfw_event_update_h :: Ptr RGFW_event -> IO CInt

-- | Native event structure size in bytes, for allocating event storage.
foreign import ccall unsafe "rgfw_event_size"
  c_rgfw_event_size :: IO CSize

-- | Current native window width in pixels. Requires a live non-null pointer.
foreign import ccall unsafe "rgfw_window_w"
  c_rgfw_window_w :: Ptr RGFW_window -> IO CInt

-- | Current native window height in pixels. Requires a live non-null pointer.
foreign import ccall unsafe "rgfw_window_h"
  c_rgfw_window_h :: Ptr RGFW_window -> IO CInt

-- | Monitor's horizontal display scale, falling back to 1 if unavailable.
foreign import ccall unsafe "rgfw_window_scale"
  c_rgfw_window_scale :: Ptr RGFW_window -> IO CFloat

-- | Horizontal display scale from a scale-update event.
foreign import ccall unsafe "rgfw_event_scale_x"
  c_rgfw_event_scale_x :: Ptr RGFW_event -> IO CFloat

-- | Vertical display scale from a scale-update event.
foreign import ccall unsafe "rgfw_event_scale_y"
  c_rgfw_event_scale_y :: Ptr RGFW_event -> IO CFloat

-- Clipboard (cbits/RGFW.c)
-- | Borrow clipboard UTF-8 until the next clipboard read. Writes the byte
-- length, excluding trailing NULs, to the supplied pointer. Null means no text.
foreign import ccall "rgfw_read_clipboard_text"
  c_rgfw_read_clipboard_text :: Ptr CSize -> IO CString

-- | Copy UTF-8 bytes to the clipboard. Length is in bytes; zero is allowed.
-- Returns zero on failure. The buffer only needs to live through the call.
foreign import ccall "rgfw_write_clipboard_text"
  c_rgfw_write_clipboard_text :: CString -> CSize -> IO CUChar

-- | Event tags for no event, key press, key release, and typed character.
rgfw_eventNone, rgfw_keyPressed, rgfw_keyReleased, rgfw_keyChar :: Word8
-- | Event tags for button press/release, wheel movement, and pointer movement.
rgfw_mouseButtonPressed, rgfw_mouseButtonReleased, rgfw_mouseScroll, rgfw_mouseMotion :: Word8
-- | Event tags for window movement, resize, focus, scale, and close requests.
rgfw_windowMoved, rgfw_windowResized, rgfw_windowFocusIn, rgfw_windowFocusOut, rgfw_scaleUpdated, rgfw_windowClose :: Word8

rgfw_eventNone           = 0
rgfw_keyPressed          = 1
rgfw_keyReleased         = 2
rgfw_keyChar             = 3
rgfw_mouseButtonPressed  = 4
rgfw_mouseButtonReleased = 5
rgfw_mouseScroll         = 6
rgfw_mouseMotion         = 7
rgfw_windowMoved         = 11
rgfw_windowResized       = 12
rgfw_windowFocusIn       = 13
rgfw_windowFocusOut      = 14
rgfw_scaleUpdated        = 22
rgfw_windowClose         = 16

-- | RGFW button codes, distinct from cursor-shape codes.
rgfw_mouseLeft, rgfw_mouseMiddle, rgfw_mouseRight :: Word8
rgfw_mouseLeft   = 0
rgfw_mouseMiddle = 1
rgfw_mouseRight  = 2

-- | Editing key codes carried by key events.
rgfw_keyBackSpace, rgfw_keyTab, rgfw_keyReturn, rgfw_keyEscape, rgfw_keyDelete :: Word32
-- | Inclusive A-Z key-code bounds. Text input comes from character events.
rgfw_keyA, rgfw_keyZ :: Word32
-- | Navigation key codes carried by key events.
rgfw_keyUp, rgfw_keyDown, rgfw_keyLeft, rgfw_keyRight, rgfw_keyEnd, rgfw_keyHome :: Word32
rgfw_keyBackSpace = 8
rgfw_keyTab       = 9
rgfw_keyReturn    = 10
rgfw_keyEscape    = 27
rgfw_keyDelete    = 127
rgfw_keyA         = 97
rgfw_keyZ         = 122
rgfw_keyUp        = 162
rgfw_keyDown      = 163
rgfw_keyLeft      = 164
rgfw_keyRight     = 165
rgfw_keyEnd       = 168
rgfw_keyHome      = 169

-- | Independent modifier bits, combined with bitwise OR in key events.
rgfw_modCapsLock, rgfw_modNumLock, rgfw_modControl, rgfw_modAlt, rgfw_modShift, rgfw_modSuper, rgfw_modScrollLock :: Word8
rgfw_modCapsLock   = 1
rgfw_modNumLock    = 2
rgfw_modControl    = 4
rgfw_modAlt        = 8
rgfw_modShift      = 16
rgfw_modSuper      = 32
rgfw_modScrollLock = 64

-- | Window creation flags for centred placement and initially hidden windows.
rgfw_windowCenter, rgfw_windowHide :: Word32
rgfw_windowCenter = 64
rgfw_windowHide   = 512

-- Mouse cursors
-- | Set a standard cursor shape. Returns zero on failure or for a null window.
foreign import ccall "rgfw_window_set_mouse_standard"
  c_rgfw_window_set_mouse_standard :: Ptr RGFW_window -> CUChar -> IO CUChar

-- | Restore the native default cursor. Returns zero on failure.
foreign import ccall "rgfw_window_set_mouse_default"
  c_rgfw_window_set_mouse_default :: Ptr RGFW_window -> IO CUChar

-- | Standard cursor codes for default, arrow, text, crosshair, and link cursors.
rgfw_mouseNormal, rgfw_mouseArrow, rgfw_mouseIbeam, rgfw_mouseCrosshair, rgfw_mousePointingHand :: Word8
-- | Bidirectional resize cursor codes: horizontal, vertical, and diagonals.
rgfw_mouseResizeEW, rgfw_mouseResizeNS, rgfw_mouseResizeNWSE, rgfw_mouseResizeNESW :: Word8
-- | Directional resize cursor codes: northwest, north, northeast, and east.
rgfw_mouseResizeNW, rgfw_mouseResizeN, rgfw_mouseResizeNE, rgfw_mouseResizeE :: Word8
-- | Directional resize cursor codes: southeast, south, southwest, west, and all directions.
rgfw_mouseResizeSE, rgfw_mouseResizeS, rgfw_mouseResizeSW, rgfw_mouseResizeW, rgfw_mouseResizeAll :: Word8

rgfw_mouseNormal       = 0
rgfw_mouseArrow        = 1
rgfw_mouseIbeam        = 2
rgfw_mouseCrosshair    = 3
rgfw_mousePointingHand = 4
rgfw_mouseResizeEW     = 5
rgfw_mouseResizeNS     = 6
rgfw_mouseResizeNWSE   = 7
rgfw_mouseResizeNESW   = 8
rgfw_mouseResizeNW     = 9
rgfw_mouseResizeN      = 10
rgfw_mouseResizeNE     = 11
rgfw_mouseResizeE      = 12
rgfw_mouseResizeSE     = 13
rgfw_mouseResizeS      = 14
rgfw_mouseResizeSW     = 15
rgfw_mouseResizeW      = 16
rgfw_mouseResizeAll    = 17
