module NanoUI.Sdl.Debug
  ( SdlDebugSnapshot (..)
  , SdlDebugSampler (..)
  , SamplerRef
  , newSdlDebugSampler
  , noteLoop
  , notePresent
  , noteSkip
  , isDebugActive
  , takeDebugLive
  , readSdlDebug
  , emptySdlDebug
  ) where

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text, unpack)
import GHC.Clock (getMonotonicTime)
import NanoUI (Size (..), V2 (..))
import NanoUI.Debug
  ( CoreDebugSnapshot (..)
  , DebugSampler (..)
  , debugRefreshSec
  , emptyCoreDebugSnapshot
  , makeCoreDebugSnapshot
  , newDebugSampler
  , noteDebugLoop
  , noteDebugPresent
  , noteDebugSkip
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


data SdlDebugSampler = SdlDebugSampler
  { sdsSampler  :: !(IORef DebugSampler)
  , sdsSnapshot :: !(IORef SdlDebugSnapshot)
  }

type SamplerRef = IORef SdlDebugSampler

newSdlDebugSampler :: IO SamplerRef
newSdlDebugSampler = do
  sRef <- newDebugSampler
  snapRef <- newIORef emptySdlDebug
  newIORef $ SdlDebugSampler sRef snapRef

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

noteLoop :: SamplerRef -> Float -> IO ()
noteLoop ref dt = do
  s <- readIORef ref
  noteDebugLoop (sdsSampler s) dt

noteSkip :: SamplerRef -> IO ()
noteSkip ref = do
  s <- readIORef ref
  noteDebugSkip (sdsSampler s)

isDebugActive :: SamplerRef -> IO Bool
isDebugActive ref = do
  s <- readIORef ref
  D.isDebugActive (sdsSampler s)

takeDebugLive :: SamplerRef -> Bool -> IO Bool
takeDebugLive ref windowOpen = do
  s <- readIORef ref
  D.takeDebugLive (sdsSampler s) windowOpen

notePresent :: SamplerRef -> Double -> Double -> Double -> Double -> DrawData -> IO ()
notePresent ref uiMs renderMs presentMs frameMs dd = do
  s <- readIORef ref
  noteDebugPresent
    (sdsSampler s)
    uiMs
    renderMs
    presentMs
    frameMs
    (drawVertexCount dd)
    (drawIndexCount dd)
    (drawCmdCount dd)

readSdlDebug :: SamplerRef -> Size -> V2 -> FilePath -> Float -> Text -> Bool -> Int -> IO SdlDebugSnapshot
readSdlDebug ref (Size ww wh) (V2 mx my) fontPath scale renderer vsync refreshHz = do
  s <- readIORef ref
  now <- getMonotonicTime
  (refresh, _cur) <-
    atomicModifyIORef' (sdsSampler s) $ \curSampler ->
      let elapsed = now - smLastDebugT curSampler
          refresh = smLastDebugT curSampler <= 0 || elapsed >= debugRefreshSec
       in (curSampler {smWantFrame = refresh || smWantFrame curSampler, smLastQueryT = now}, (refresh, curSampler))
  if not refresh
    then readIORef (sdsSnapshot s)
    else do
      rts <- readRtsSnapshot
      (rate, cur2) <-
        atomicModifyIORef' (sdsSampler s) $ \curSampler ->
          let (rated, rate) = D.presentRate now curSampler
              s' =
                rated
                  { smLastDebugT = now
                  , smWantFrame = False
                  , smLastQueryT = now
                  }
           in (s', (rate, s'))
      let core = (makeCoreDebugSnapshot cur2 ww wh mx my rts) {dbgPresentFps = rate}
          snap =
            SdlDebugSnapshot
              { dbgCore     = core
              , dbgScale    = scale
              , dbgFontPath = fontPath
              , dbgRenderer = renderer
              , dbgVsync    = vsync
              , dbgRefreshHz = refreshHz
              }
      writeIORef (sdsSnapshot s) snap
      traceFrame snap
      pure snap

-- | Env-gated per-refresh timing trace (NANO_FRAME_TRACE=1). Prints the
-- snapshot's phase EMAs so live-loop costs can be compared across builds.
traceFrame :: SdlDebugSnapshot -> IO ()
traceFrame s = do
  let c = dbgCore s
  on <- lookupEnv "NANO_FRAME_TRACE"
  case on of
    Nothing -> pure ()
    Just _ ->
      Text.Printf.printf
        "TRACE refreshHz=%3d rend=%s vsync=%d presentFps=%6.0f loopFps=%6.0f frameMs=%6.3f uiMs=%6.3f renderMs=%6.3f presentMs=%6.3f verts=%5d cmds=%2d presents=%d skips=%d\n"
        (dbgRefreshHz s)
        (unpack (dbgRenderer s))
        (dbgBoolInt (dbgVsync s))
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
    dbgBoolInt b = if b then (1 :: Int) else 0
