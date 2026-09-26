-- | RGFW imports and header-derived event accessors and constants. The C
-- compiler supplies field offsets and enum values from the bundled header.
module RGFW.Raw
  ( RGFW_window
  , RGFW_event
  , c_RGFW_window_close
  , c_RGFW_window_checkEvent
  , c_RGFW_waitForEvent
  , c_RGFW_stopCheckEvents
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
  , c_rgfw_event_key_repeat
  , c_RGFW_physicalToMappedKey
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
  , rgfw_mouseLeave
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
  , rgfw_mouseMisc1
  , rgfw_mouseMisc2
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
  , rgfw_keySpace
  , rgfw_keyInsert
  , rgfw_keyPageUp
  , rgfw_keyPageDown
  , rgfw_keyMenu
  , rgfw_keyF1
  , rgfw_keyF24
  , rgfw_keyCapsLock
  , rgfw_keyNumLock
  , rgfw_keyScrollLock
  , rgfw_keyPrintScreen
  , rgfw_keyPause
  , rgfw_keyPad0
  , rgfw_keyPad1
  , rgfw_keyPad2
  , rgfw_keyPad3
  , rgfw_keyPad4
  , rgfw_keyPad5
  , rgfw_keyPad6
  , rgfw_keyPad7
  , rgfw_keyPad8
  , rgfw_keyPad9
  , rgfw_keyPadPeriod
  , rgfw_keyPadSlash
  , rgfw_keyPadMultiply
  , rgfw_keyPadMinus
  , rgfw_keyPadPlus
  , rgfw_keyPadEqual
  , rgfw_keyPadReturn
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
  -- Window options
  , c_RGFW_window_setIcon
  , c_RGFW_window_setMinSize
  , c_RGFW_window_setMaxSize
  , c_RGFW_window_move
  , c_RGFW_window_center
  , rgfw_formatRGBA8
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
  , rgfw_mouseNotAllowed
  , rgfw_mouseWait
  , rgfw_mouseProgress
  ) where

import Data.Word (Word8, Word32)
import Foreign.C.String (CString)
import Foreign.C.Types (CFloat (..), CInt (..), CSize (..), CUChar (..), CUInt (..))
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekByteOff)

#include "RGFW.h"

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

-- | End a 'c_RGFW_waitForEvent' in progress, or the next one, from any thread.
foreign import ccall "RGFW_stopCheckEvents"
  c_RGFW_stopCheckEvents :: IO ()

-- | Create title/x/y/width/height/flags with a core OpenGL major/minor version.
-- Returns null on failure; the resulting context is current on this OS thread.
foreign import ccall "rgfw_create_window_gl"
  c_rgfw_create_window_gl :: CString -> CInt -> CInt -> CInt -> CInt -> CUInt -> CInt -> CInt -> IO (Ptr RGFW_window)

-- | Present the window's OpenGL back buffer on the context's owning thread.
foreign import ccall "RGFW_window_swapBuffers_OpenGL"
  c_RGFW_window_swapBuffers_OpenGL :: Ptr RGFW_window -> IO ()

-- Event accessors read only the union member selected by the event tag.
-- | Event tag; inspect it before reading fields of the event union.
c_rgfw_event_type :: Ptr RGFW_event -> IO CUChar
c_rgfw_event_type = #{peek RGFW_event, type}

-- | Mouse-motion x coordinate in native window pixels.
c_rgfw_event_mouse_x :: Ptr RGFW_event -> IO CInt
c_rgfw_event_mouse_x = #{peek RGFW_event, mouse.x}

-- | Mouse-motion y coordinate in native window pixels.
c_rgfw_event_mouse_y :: Ptr RGFW_event -> IO CInt
c_rgfw_event_mouse_y = #{peek RGFW_event, mouse.y}

-- | Button code from a mouse-button event; see 'rgfw_mouseLeft'.
c_rgfw_event_button_value :: Ptr RGFW_event -> IO CUChar
c_rgfw_event_button_value = #{peek RGFW_event, button.value}

-- | Horizontal wheel delta from a scroll event.
c_rgfw_event_delta_x :: Ptr RGFW_event -> IO CFloat
c_rgfw_event_delta_x = #{peek RGFW_event, delta.x}

-- | Vertical wheel delta from a scroll event.
c_rgfw_event_delta_y :: Ptr RGFW_event -> IO CFloat
c_rgfw_event_delta_y = #{peek RGFW_event, delta.y}

-- | RGFW key code from a key press or release, not a Unicode text character.
c_rgfw_event_key_value :: Ptr RGFW_event -> IO CUInt
c_rgfw_event_key_value p = fromIntegral <$> (#{peek RGFW_event, key.value} p :: IO #{type RGFW_key})

-- | Modifier bit mask from a key event; test with the @rgfw_mod*@ constants.
c_rgfw_event_key_mod :: Ptr RGFW_event -> IO CUChar
c_rgfw_event_key_mod = #{peek RGFW_event, key.mod}

-- | Nonzero when a key press is the auto-repeat of a held key.
c_rgfw_event_key_repeat :: Ptr RGFW_event -> IO CUChar
c_rgfw_event_key_repeat p = fromIntegral <$> (#{peek RGFW_event, key.repeat} p :: IO #{type RGFW_bool})

-- | The key a physical key code types in the current keyboard layout, as an
-- RGFW key code; 'RGFW_keyNULL' (0) when it has none. Needs an open window.
foreign import ccall unsafe "RGFW_physicalToMappedKey"
  c_RGFW_physicalToMappedKey :: CUChar -> IO CUChar

-- | Code point from a character event. Validate it before converting to 'Char'.
c_rgfw_event_keyChar_value :: Ptr RGFW_event -> IO CUInt
c_rgfw_event_keyChar_value = #{peek RGFW_event, keyChar.value}

-- | Native pixel width carried by a resize event.
c_rgfw_event_update_w :: Ptr RGFW_event -> IO CInt
c_rgfw_event_update_w = #{peek RGFW_event, update.w}

-- | Native pixel height carried by a resize event.
c_rgfw_event_update_h :: Ptr RGFW_event -> IO CInt
c_rgfw_event_update_h = #{peek RGFW_event, update.h}

-- | Native event structure size in bytes, for allocating event storage.
c_rgfw_event_size :: IO CSize
c_rgfw_event_size = pure #{size RGFW_event}

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
c_rgfw_event_scale_x :: Ptr RGFW_event -> IO CFloat
c_rgfw_event_scale_x = #{peek RGFW_event, scale.x}

-- | Vertical display scale from a scale-update event.
c_rgfw_event_scale_y :: Ptr RGFW_event -> IO CFloat
c_rgfw_event_scale_y = #{peek RGFW_event, scale.y}

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
-- | Event tags for button press/release, wheel movement, pointer movement,
-- and the pointer leaving the window.
rgfw_mouseButtonPressed, rgfw_mouseButtonReleased, rgfw_mouseScroll, rgfw_mouseMotion, rgfw_mouseLeave :: Word8
-- | Event tags for window movement, resize, focus, scale, and close requests.
rgfw_windowMoved, rgfw_windowResized, rgfw_windowFocusIn, rgfw_windowFocusOut, rgfw_scaleUpdated, rgfw_windowClose :: Word8

rgfw_eventNone           = #{const RGFW_eventNone}
rgfw_keyPressed          = #{const RGFW_keyPressed}
rgfw_keyReleased         = #{const RGFW_keyReleased}
rgfw_keyChar             = #{const RGFW_keyChar}
rgfw_mouseButtonPressed  = #{const RGFW_mouseButtonPressed}
rgfw_mouseButtonReleased = #{const RGFW_mouseButtonReleased}
rgfw_mouseScroll         = #{const RGFW_mouseScroll}
rgfw_mouseMotion         = #{const RGFW_mouseMotion}
rgfw_mouseLeave          = #{const RGFW_mouseLeave}
rgfw_windowMoved         = #{const RGFW_windowMoved}
rgfw_windowResized       = #{const RGFW_windowResized}
rgfw_windowFocusIn       = #{const RGFW_windowFocusIn}
rgfw_windowFocusOut      = #{const RGFW_windowFocusOut}
rgfw_scaleUpdated        = #{const RGFW_scaleUpdated}
rgfw_windowClose         = #{const RGFW_windowClose}

-- | RGFW button codes, distinct from cursor-shape codes.
rgfw_mouseLeft, rgfw_mouseMiddle, rgfw_mouseRight :: Word8
rgfw_mouseLeft   = #{const RGFW_mouseLeft}
rgfw_mouseMiddle = #{const RGFW_mouseMiddle}
rgfw_mouseRight  = #{const RGFW_mouseRight}

-- | The side buttons, back (X1) and forward (X2), on X11, Windows and macOS.
rgfw_mouseMisc1, rgfw_mouseMisc2 :: Word8
rgfw_mouseMisc1 = #{const RGFW_mouseMisc1}
rgfw_mouseMisc2 = #{const RGFW_mouseMisc2}

-- | Editing key codes carried by key events.
rgfw_keyBackSpace, rgfw_keyTab, rgfw_keyReturn, rgfw_keyEscape, rgfw_keyDelete :: Word32
-- | Inclusive A-Z key-code bounds. Text input comes from character events.
rgfw_keyA, rgfw_keyZ :: Word32
-- | Navigation key codes carried by key events.
rgfw_keyUp, rgfw_keyDown, rgfw_keyLeft, rgfw_keyRight, rgfw_keyEnd, rgfw_keyHome :: Word32
rgfw_keyBackSpace = #{const RGFW_keyBackSpace}
rgfw_keyTab       = #{const RGFW_keyTab}
rgfw_keyReturn    = #{const RGFW_keyReturn}
rgfw_keyEscape    = #{const RGFW_keyEscape}
rgfw_keyDelete    = #{const RGFW_keyDelete}
rgfw_keyA         = #{const RGFW_keyA}
rgfw_keyZ         = #{const RGFW_keyZ}
rgfw_keyUp        = #{const RGFW_keyUp}
rgfw_keyDown      = #{const RGFW_keyDown}
rgfw_keyLeft      = #{const RGFW_keyLeft}
rgfw_keyRight     = #{const RGFW_keyRight}
rgfw_keyEnd       = #{const RGFW_keyEnd}
rgfw_keyHome      = #{const RGFW_keyHome}

-- | Further named key codes carried by key events. The function keys
-- 'rgfw_keyF1' to 'rgfw_keyF24' are consecutive, as are the keypad digits
-- 'rgfw_keyPad1' to 'rgfw_keyPad9'.
rgfw_keySpace, rgfw_keyInsert, rgfw_keyPageUp, rgfw_keyPageDown, rgfw_keyMenu, rgfw_keyF1, rgfw_keyF24 :: Word32
rgfw_keyCapsLock, rgfw_keyNumLock, rgfw_keyScrollLock, rgfw_keyPrintScreen, rgfw_keyPause :: Word32
rgfw_keyPad0, rgfw_keyPad1, rgfw_keyPad2, rgfw_keyPad3, rgfw_keyPad4, rgfw_keyPad5, rgfw_keyPad6, rgfw_keyPad7, rgfw_keyPad8, rgfw_keyPad9 :: Word32
rgfw_keyPadPeriod, rgfw_keyPadSlash, rgfw_keyPadMultiply, rgfw_keyPadMinus, rgfw_keyPadPlus, rgfw_keyPadEqual, rgfw_keyPadReturn :: Word32
rgfw_keySpace       = #{const RGFW_keySpace}
rgfw_keyInsert      = #{const RGFW_keyInsert}
rgfw_keyPageUp      = #{const RGFW_keyPageUp}
rgfw_keyPageDown    = #{const RGFW_keyPageDown}
rgfw_keyMenu        = #{const RGFW_keyMenu}
rgfw_keyF1          = #{const RGFW_keyF1}
rgfw_keyF24         = #{const RGFW_keyF24}
rgfw_keyCapsLock    = #{const RGFW_keyCapsLock}
rgfw_keyNumLock     = #{const RGFW_keyNumLock}
rgfw_keyScrollLock  = #{const RGFW_keyScrollLock}
rgfw_keyPrintScreen = #{const RGFW_keyPrintScreen}
rgfw_keyPause       = #{const RGFW_keyPause}
rgfw_keyPad0        = #{const RGFW_keyPad0}
rgfw_keyPad1        = #{const RGFW_keyPad1}
rgfw_keyPad2        = #{const RGFW_keyPad2}
rgfw_keyPad3        = #{const RGFW_keyPad3}
rgfw_keyPad4        = #{const RGFW_keyPad4}
rgfw_keyPad5        = #{const RGFW_keyPad5}
rgfw_keyPad6        = #{const RGFW_keyPad6}
rgfw_keyPad7        = #{const RGFW_keyPad7}
rgfw_keyPad8        = #{const RGFW_keyPad8}
rgfw_keyPad9        = #{const RGFW_keyPad9}
rgfw_keyPadPeriod   = #{const RGFW_keyPadPeriod}
rgfw_keyPadSlash    = #{const RGFW_keyPadSlash}
rgfw_keyPadMultiply = #{const RGFW_keyPadMultiply}
rgfw_keyPadMinus    = #{const RGFW_keyPadMinus}
rgfw_keyPadPlus     = #{const RGFW_keyPadPlus}
rgfw_keyPadEqual    = #{const RGFW_keyPadEqual}
rgfw_keyPadReturn   = #{const RGFW_keyPadReturn}

-- | Independent modifier bits, combined with bitwise OR in key events.
rgfw_modCapsLock, rgfw_modNumLock, rgfw_modControl, rgfw_modAlt, rgfw_modShift, rgfw_modSuper, rgfw_modScrollLock :: Word8
rgfw_modCapsLock   = #{const RGFW_modCapsLock}
rgfw_modNumLock    = #{const RGFW_modNumLock}
rgfw_modControl    = #{const RGFW_modControl}
rgfw_modAlt        = #{const RGFW_modAlt}
rgfw_modShift      = #{const RGFW_modShift}
rgfw_modSuper      = #{const RGFW_modSuper}
rgfw_modScrollLock = #{const RGFW_modScrollLock}

-- | Window creation flags for centred placement and initially hidden windows.
rgfw_windowCenter, rgfw_windowHide :: Word32
rgfw_windowCenter = #{const RGFW_windowCenter}
rgfw_windowHide   = #{const RGFW_windowHide}

-- Window options
-- | Set the window and taskbar icon from pixels of a format, @w@ by @h@.
-- RGFW copies them. Returns zero on failure.
foreign import ccall "RGFW_window_setIcon"
  c_RGFW_window_setIcon :: Ptr RGFW_window -> Ptr Word8 -> CInt -> CInt -> CUChar -> IO CUChar

-- | Smallest size the user may resize the window to, in native pixels;
-- zero is no limit.
foreign import ccall "RGFW_window_setMinSize"
  c_RGFW_window_setMinSize :: Ptr RGFW_window -> CInt -> CInt -> IO ()

-- | Largest size the user may resize the window to, in native pixels; zero
-- is no limit.
foreign import ccall "RGFW_window_setMaxSize"
  c_RGFW_window_setMaxSize :: Ptr RGFW_window -> CInt -> CInt -> IO ()

-- | Move the window's top-left corner to a point on the desktop.
foreign import ccall "RGFW_window_move"
  c_RGFW_window_move :: Ptr RGFW_window -> CInt -> CInt -> IO ()

-- | Centre the window on its monitor.
foreign import ccall "RGFW_window_center"
  c_RGFW_window_center :: Ptr RGFW_window -> IO ()

-- | Pixel format of tightly packed RGBA bytes, for 'c_RGFW_window_setIcon'.
rgfw_formatRGBA8 :: Word8
rgfw_formatRGBA8 = #{const RGFW_formatRGBA8}

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
-- | Standard cursor codes for a forbidden action, a busy application, and one
-- busy in the background.
rgfw_mouseNotAllowed, rgfw_mouseWait, rgfw_mouseProgress :: Word8

rgfw_mouseNormal       = #{const RGFW_mouseNormal}
rgfw_mouseArrow        = #{const RGFW_mouseArrow}
rgfw_mouseIbeam        = #{const RGFW_mouseIbeam}
rgfw_mouseCrosshair    = #{const RGFW_mouseCrosshair}
rgfw_mousePointingHand = #{const RGFW_mousePointingHand}
rgfw_mouseResizeEW     = #{const RGFW_mouseResizeEW}
rgfw_mouseResizeNS     = #{const RGFW_mouseResizeNS}
rgfw_mouseResizeNWSE   = #{const RGFW_mouseResizeNWSE}
rgfw_mouseResizeNESW   = #{const RGFW_mouseResizeNESW}
rgfw_mouseResizeNW     = #{const RGFW_mouseResizeNW}
rgfw_mouseResizeN      = #{const RGFW_mouseResizeN}
rgfw_mouseResizeNE     = #{const RGFW_mouseResizeNE}
rgfw_mouseResizeE      = #{const RGFW_mouseResizeE}
rgfw_mouseResizeSE     = #{const RGFW_mouseResizeSE}
rgfw_mouseResizeS      = #{const RGFW_mouseResizeS}
rgfw_mouseResizeSW     = #{const RGFW_mouseResizeSW}
rgfw_mouseResizeW      = #{const RGFW_mouseResizeW}
rgfw_mouseResizeAll    = #{const RGFW_mouseResizeAll}
rgfw_mouseNotAllowed   = #{const RGFW_mouseNotAllowed}
rgfw_mouseWait         = #{const RGFW_mouseWait}
rgfw_mouseProgress     = #{const RGFW_mouseProgress}
