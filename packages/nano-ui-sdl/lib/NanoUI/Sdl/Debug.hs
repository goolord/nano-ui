module NanoUI.Sdl.Debug
  ( SdlDebugSnapshot (..)
  , SdlDebugSampler
  , newSdlDebugSampler
  , noteLoop
  , notePresent
  , noteSkip
  , isDebugActive
  , takeDebugLive
  , readSdlDebug
  , emptySdlDebug
  ) where

import Control.Monad (when)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)
import Data.Text (Text, unpack)
import GHC.Clock (getMonotonicTime)
import NanoUI (Size (..), V2 (..))
import NanoUI.Debug
  ( CoreDebugSnapshot (..)
  , DebugSampler (..)
  , DebugSamplerRef
  , debugRefreshSec
  , emptyCoreDebugSnapshot
  , makeCoreDebugSnapshot
  , newDebugSampler
  , noteDebugLoop
  , noteDebugPresent
  , noteDebugSkip
  , presentRate
  , readRtsSnapshot
  )
import qualified NanoUI.Debug as D
import NanoUI.Testing (DrawData (..), drawCmdCount)
import System.Environment (lookupEnv)
import Text.Printf (printf)

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

newSdlDebugSampler :: IO SdlDebugSampler
newSdlDebugSampler =
  SdlDebugSampler
    <$> newDebugSampler
    <*> newIORef emptySdlDebug
    <*> (isJust <$> lookupEnv "NANO_FRAME_TRACE")

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

noteLoop :: SdlDebugSampler -> Float -> IO ()
noteLoop = noteDebugLoop . sdsSampler

noteSkip :: SdlDebugSampler -> IO ()
noteSkip = noteDebugSkip . sdsSampler

isDebugActive :: SdlDebugSampler -> IO Bool
isDebugActive = D.isDebugActive . sdsSampler

takeDebugLive :: SdlDebugSampler -> Bool -> IO Bool
takeDebugLive = D.takeDebugLive . sdsSampler

notePresent :: SdlDebugSampler -> Double -> Double -> Double -> Double -> DrawData -> IO ()
notePresent s uiMs renderMs presentMs frameMs dd =
  noteDebugPresent
    (sdsSampler s)
    uiMs
    renderMs
    presentMs
    frameMs
    (drawVertexCount dd)
    (drawIndexCount dd)
    (drawCmdCount dd)

readSdlDebug :: SdlDebugSampler -> Size -> V2 -> FilePath -> Float -> Text -> Bool -> Int -> IO SdlDebugSnapshot
readSdlDebug s (Size ww wh) (V2 mx my) fontPath scale renderer vsync refreshHz = do
  now <- getMonotonicTime
  refresh <-
    atomicModifyIORef' (sdsSampler s) $ \cur ->
      let due = smLastDebugT cur <= 0 || now - smLastDebugT cur >= debugRefreshSec
       in (cur {smWantFrame = due || smWantFrame cur, smLastQueryT = now}, due)
  if not refresh
    then readIORef (sdsSnapshot s)
    else do
      rts <- readRtsSnapshot
      (rate, sampled) <-
        atomicModifyIORef' (sdsSampler s) $ \cur ->
          let (rated, rate) = presentRate now cur
              cur' = rated {smLastDebugT = now, smWantFrame = False, smLastQueryT = now}
           in (cur', (rate, cur'))
      let snap =
            SdlDebugSnapshot
              { dbgCore     = (makeCoreDebugSnapshot sampled ww wh mx my rts) {dbgPresentFps = rate}
              , dbgScale    = scale
              , dbgFontPath = fontPath
              , dbgRenderer = renderer
              , dbgVsync    = vsync
              , dbgRefreshHz = refreshHz
              }
      writeIORef (sdsSnapshot s) snap
      when (sdsTrace s) (traceFrame snap)
      pure snap

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
