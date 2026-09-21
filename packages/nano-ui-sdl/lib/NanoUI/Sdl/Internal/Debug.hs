-- | SDL display facts combined with the core frame-timing sampler.
module NanoUI.Sdl.Internal.Debug
  ( SdlDebugSnapshot (..)
  , SdlDebugSampler (..)
  , newSdlDebugSampler
  , emptySdlDebug
  , traceFrame
  ) where

import Data.IORef (IORef, newIORef)
import Data.Maybe (isJust)
import Data.Text (Text, unpack)
import NanoUI.Internal.Debug
  ( CoreDebugSnapshot (..)
  , DebugSamplerRef
  , emptyCoreDebugSnapshot
  , newDebugSampler
  )
import System.Environment (lookupEnv)
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

-- | The core sampler, the last published snapshot, and whether
-- NANO_FRAME_TRACE (read once at creation) is set.
data SdlDebugSampler = SdlDebugSampler
  { sdsSampler  :: !DebugSamplerRef
  , sdsSnapshot :: !(IORef SdlDebugSnapshot)
  , sdsTrace    :: !Bool
  }

-- | Start an empty sampler and read whether @NANO_FRAME_TRACE@ is set.
newSdlDebugSampler :: IO SdlDebugSampler
newSdlDebugSampler =
  SdlDebugSampler
    <$> newDebugSampler
    <*> newIORef emptySdlDebug
    <*> (isJust <$> lookupEnv "NANO_FRAME_TRACE")

-- | Placeholder snapshot before an SDL session publishes measurements.
emptySdlDebug :: SdlDebugSnapshot
emptySdlDebug =
  SdlDebugSnapshot
    { dbgCore     = emptyCoreDebugSnapshot
    , dbgScale    = 1
    , dbgFontPath = ""
    , dbgRenderer = ""
    , dbgVsync    = True
    , dbgRefreshHz = 0
    }

-- | Per-refresh timing trace (NANO_FRAME_TRACE). Prints the snapshot's phase
-- EMAs so live-loop costs can be compared across builds.
traceFrame :: SdlDebugSnapshot -> IO ()
traceFrame s =
  printf
    "TRACE refreshHz=%3d rend=%s vsync=%d presentFps=%6.0f loopFps=%6.0f frameMs=%6.3f uiMs=%6.3f renderMs=%6.3f presentMs=%6.3f verts=%5d cmds=%2d presents=%d skips=%d\n"
    (dbgRefreshHz s)
    (unpack (dbgRenderer s))
    (if dbgVsync s then 1 else 0 :: Int)
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
