-- | SDL display facts combined with the core frame-timing sampler.
module NanoUI.Sdl.Internal.Debug
  ( SdlDebugSnapshot (..)
  , traceFrame
  ) where

import Data.Text (Text, unpack)
import NanoUI.Internal.Debug (CoreDebugSnapshot (..))
import Text.Printf (printf)

-- | Published timing, font, renderer, and display information. Scale is
-- physical pixels per layout unit and refresh rate is in hertz.
data SdlDebugSnapshot = SdlDebugSnapshot
  { dbgCore      :: !CoreDebugSnapshot
  , dbgScale     :: !Float
  , dbgFontPath  :: !FilePath
  , dbgRenderer  :: !Text
  , dbgVsync     :: !Bool
  , dbgRefreshHz :: !Int
  }
  deriving (Eq, Show)

-- | Per-refresh timing trace (NANO_FRAME_TRACE). Prints the snapshot's phase
-- EMAs so live-loop costs can be compared across builds.
traceFrame :: SdlDebugSnapshot -> IO ()
traceFrame s =
  printf
    "TRACE refreshHz=%3d rend=%s vsync=%d presentFps=%6.0f loopFps=%6.0f frameMs=%6.3f uiMs=%6.3f renderMs=%6.3f presentMs=%6.3f verts=%5d cmds=%2d presents=%d skips=%d\n"
    (dbgRefreshHz s)
    (unpack (dbgRenderer s))
    (fromEnum (dbgVsync s))
    (dbgPresentFps c)
    (dbgLoopFps c)
    (dbgFrameMs c)
    (dbgUiMs c)
    (dbgRenderMs c)
    (dbgPresentMs c)
    (dbgVerts c)
    (dbgCmds c)
    (dbgPresents c)
    (dbgSkips c)
  where
    c = dbgCore s
