{-# LANGUAGE CApiFFI #-}

module RGFW.Raw
  ( RGFW_window
  , RGFW_surface
  , RGFW_event
  , c_rgfw_init
  , c_rgfw_deinit
  , c_RGFW_createWindow
  , c_RGFW_window_close
  , c_RGFW_window_checkEvent
  , c_RGFW_createSurface
  , c_RGFW_window_blitSurface
  , c_RGFW_surface_free
  , c_rgfw_event_type
  , c_rgfw_event_mouse_x
  , c_rgfw_event_mouse_y
  , c_rgfw_event_button_value
  , c_rgfw_event_button_state
  , c_rgfw_event_delta_x
  , c_rgfw_event_delta_y
  , c_rgfw_event_key_value
  , c_rgfw_event_key_state
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
  -- Formats
  , rgfw_formatRGB8
  , rgfw_formatBGR8
  , rgfw_formatRGBA8
  , rgfw_formatARGB8
  , rgfw_formatBGRA8
  , rgfw_formatABGR8
  -- Window flags
  , rgfw_windowNoBorder
  , rgfw_windowNoResize
  , rgfw_windowAllowDND
  , rgfw_windowCenter
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

data RGFW_window
data RGFW_surface
data RGFW_event

-- Foreign function imports
foreign import ccall "rgfw_init"
  c_rgfw_init :: CString -> IO CInt

foreign import ccall "rgfw_deinit"
  c_rgfw_deinit :: IO ()

foreign import ccall "rgfw_create_window"
  c_RGFW_createWindow :: CString -> CInt -> CInt -> CInt -> CInt -> CUInt -> IO (Ptr RGFW_window)

foreign import ccall "RGFW_window_close"
  c_RGFW_window_close :: Ptr RGFW_window -> IO ()

foreign import ccall "RGFW_window_checkEvent"
  c_RGFW_window_checkEvent :: Ptr RGFW_window -> Ptr RGFW_event -> IO CUChar

foreign import ccall "rgfw_create_surface"
  c_RGFW_createSurface :: Ptr RGFW_window -> Ptr CUChar -> CInt -> CInt -> CUChar -> IO (Ptr RGFW_surface)

foreign import ccall "RGFW_window_blitSurface"
  c_RGFW_window_blitSurface :: Ptr RGFW_window -> Ptr RGFW_surface -> IO ()

foreign import ccall "RGFW_surface_free"
  c_RGFW_surface_free :: Ptr RGFW_surface -> IO ()

-- Accessor functions from rgfw_shim.c
foreign import ccall "rgfw_shim.c rgfw_event_type"
  c_rgfw_event_type :: Ptr RGFW_event -> IO CUChar

foreign import ccall "rgfw_shim.c rgfw_event_mouse_x"
  c_rgfw_event_mouse_x :: Ptr RGFW_event -> IO CInt

foreign import ccall "rgfw_shim.c rgfw_event_mouse_y"
  c_rgfw_event_mouse_y :: Ptr RGFW_event -> IO CInt

foreign import ccall "rgfw_shim.c rgfw_event_button_value"
  c_rgfw_event_button_value :: Ptr RGFW_event -> IO CUChar

foreign import ccall "rgfw_shim.c rgfw_event_button_state"
  c_rgfw_event_button_state :: Ptr RGFW_event -> IO CUChar

foreign import ccall "rgfw_shim.c rgfw_event_delta_x"
  c_rgfw_event_delta_x :: Ptr RGFW_event -> IO CFloat

foreign import ccall "rgfw_shim.c rgfw_event_delta_y"
  c_rgfw_event_delta_y :: Ptr RGFW_event -> IO CFloat

foreign import ccall "rgfw_shim.c rgfw_event_key_value"
  c_rgfw_event_key_value :: Ptr RGFW_event -> IO CUInt

foreign import ccall "rgfw_shim.c rgfw_event_key_state"
  c_rgfw_event_key_state :: Ptr RGFW_event -> IO CUChar

foreign import ccall "rgfw_shim.c rgfw_event_key_mod"
  c_rgfw_event_key_mod :: Ptr RGFW_event -> IO CUChar

foreign import ccall "rgfw_shim.c rgfw_event_keyChar_value"
  c_rgfw_event_keyChar_value :: Ptr RGFW_event -> IO CUInt

foreign import ccall "rgfw_shim.c rgfw_event_update_w"
  c_rgfw_event_update_w :: Ptr RGFW_event -> IO CInt

foreign import ccall "rgfw_shim.c rgfw_event_update_h"
  c_rgfw_event_update_h :: Ptr RGFW_event -> IO CInt

foreign import ccall "rgfw_shim.c rgfw_event_size"
  c_rgfw_event_size :: IO CSize

foreign import ccall "rgfw_shim.c rgfw_window_w"
  c_rgfw_window_w :: Ptr RGFW_window -> IO CInt

foreign import ccall "rgfw_shim.c rgfw_window_h"
  c_rgfw_window_h :: Ptr RGFW_window -> IO CInt

foreign import ccall "rgfw_shim.c rgfw_window_scale"
  c_rgfw_window_scale :: Ptr RGFW_window -> IO CFloat

foreign import ccall "rgfw_shim.c rgfw_event_scale_x"
  c_rgfw_event_scale_x :: Ptr RGFW_event -> IO CFloat

foreign import ccall "rgfw_shim.c rgfw_event_scale_y"
  c_rgfw_event_scale_y :: Ptr RGFW_event -> IO CFloat

-- Event types
rgfw_eventNone, rgfw_keyPressed, rgfw_keyReleased, rgfw_keyChar :: Word8
rgfw_mouseButtonPressed, rgfw_mouseButtonReleased, rgfw_mouseScroll, rgfw_mouseMotion :: Word8
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

-- Mouse buttons
rgfw_mouseLeft, rgfw_mouseMiddle, rgfw_mouseRight :: Word8
rgfw_mouseLeft   = 0
rgfw_mouseMiddle = 1
rgfw_mouseRight  = 2

-- Pixel formats
rgfw_formatRGB8, rgfw_formatBGR8, rgfw_formatRGBA8, rgfw_formatARGB8, rgfw_formatBGRA8, rgfw_formatABGR8 :: Word8
rgfw_formatRGB8  = 0
rgfw_formatBGR8  = 1
rgfw_formatRGBA8 = 2
rgfw_formatARGB8 = 3
rgfw_formatBGRA8 = 4
rgfw_formatABGR8 = 5

-- Window flags
rgfw_windowNoBorder, rgfw_windowNoResize, rgfw_windowAllowDND, rgfw_windowCenter :: Word32
rgfw_windowNoBorder = 1
rgfw_windowNoResize = 2
rgfw_windowAllowDND = 4
rgfw_windowCenter   = 64

-- Mouse cursors
foreign import ccall "rgfw_shim.c rgfw_window_set_mouse_standard"
  c_rgfw_window_set_mouse_standard :: Ptr RGFW_window -> CUChar -> IO CUChar

foreign import ccall "rgfw_shim.c rgfw_window_set_mouse_default"
  c_rgfw_window_set_mouse_default :: Ptr RGFW_window -> IO CUChar

rgfw_mouseNormal, rgfw_mouseArrow, rgfw_mouseIbeam, rgfw_mouseCrosshair, rgfw_mousePointingHand :: Word8
rgfw_mouseResizeEW, rgfw_mouseResizeNS, rgfw_mouseResizeNWSE, rgfw_mouseResizeNESW :: Word8
rgfw_mouseResizeNW, rgfw_mouseResizeN, rgfw_mouseResizeNE, rgfw_mouseResizeE :: Word8
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
