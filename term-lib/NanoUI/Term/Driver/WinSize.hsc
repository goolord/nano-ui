{-# LANGUAGE CApiFFI #-}

-- | Terminal size via @TIOCGWINSZ@. The request constant and struct layout are
-- platform specific, so they come from the system headers rather than being
-- hardcoded.
module NanoUI.Term.Driver.WinSize
  ( terminalSize
  ) where

import Foreign.C.Types (CInt (..), CUShort)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable (..))

#include <sys/ioctl.h>
#include <unistd.h>

data WinSize = WinSize !CUShort !CUShort

instance Storable WinSize where
  sizeOf _ = #{size struct winsize}
  alignment _ = #{alignment struct winsize}
  peek p = do
    rows <- #{peek struct winsize, ws_row} p
    cols <- #{peek struct winsize, ws_col} p
    pure (WinSize rows cols)
  poke p (WinSize rows cols) = do
    #{poke struct winsize, ws_row} p rows
    #{poke struct winsize, ws_col} p cols

foreign import capi unsafe "sys/ioctl.h ioctl"
  c_ioctl :: CInt -> CInt -> Ptr WinSize -> IO CInt

-- | Falls back to 80x24 if the ioctl fails, which happens when output is not a
-- terminal. stdout is not always the tty, so try the usual fds in order.
terminalSize :: IO (Int, Int)
terminalSize = querySize [1, 0, 2] (80, 24)
  where
    querySize [] fallback = pure fallback
    querySize (fd : rest) fallback =
      alloca $ \p -> do
        result <- c_ioctl fd #{const TIOCGWINSZ} p
        if result /= 0
          then querySize rest fallback
          else do
            WinSize rows cols <- peek p
            pure (max 1 (fromIntegral cols), max 1 (fromIntegral rows))
