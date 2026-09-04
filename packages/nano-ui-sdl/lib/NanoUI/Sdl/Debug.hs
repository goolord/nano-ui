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
import Data.Text (Text)
import GHC.Clock (getMonotonicTime)
import NanoUI (Size (..), V2 (..))
import NanoUI.Debug
  ( CoreDebugSnapshot
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

data SdlDebugSnapshot = SdlDebugSnapshot
  { dbgCore     :: !CoreDebugSnapshot
  , dbgScale    :: !Float
  , dbgFontPath :: !FilePath
  , dbgRenderer :: !Text
  , dbgVsync    :: !Bool
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
    }

noteLoop :: SamplerRef -> Float -> IO ()
noteLoop ref dt = do
  s <- readIORef ref
  noteDebugLoop (sdsSampler s) dt

noteSkip :: SamplerRef -> IO ()
noteSkip ref = do
  s <- readIORef ref
  noteDebugSkip (sdsSampler s)

isDebugActive :: SamplerRef -> Bool -> IO Bool
isDebugActive ref windowOpen = do
  s <- readIORef ref
  D.isDebugActive (sdsSampler s) windowOpen

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

readSdlDebug :: SamplerRef -> Size -> V2 -> FilePath -> Float -> Text -> Bool -> IO SdlDebugSnapshot
readSdlDebug ref (Size ww wh) (V2 mx my) fontPath scale renderer vsync = do
  s <- readIORef ref
  now <- getMonotonicTime
  (refresh, cur) <-
    atomicModifyIORef' (sdsSampler s) $ \curSampler ->
      let elapsed = now - smLastDebugT curSampler
          refresh = smLastDebugT curSampler <= 0 || elapsed >= debugRefreshSec
       in (curSampler {smWantFrame = refresh || smWantFrame curSampler, smLastQueryT = now}, (refresh, curSampler))
  if not refresh
    then readIORef (sdsSnapshot s)
    else do
      rts <- readRtsSnapshot
      let core = makeCoreDebugSnapshot cur ww wh mx my rts
          snap =
            SdlDebugSnapshot
              { dbgCore     = core
              , dbgScale    = scale
              , dbgFontPath = fontPath
              , dbgRenderer = renderer
              , dbgVsync    = vsync
              }
      atomicModifyIORef' (sdsSampler s) $ \curSampler ->
        (curSampler {smLastDebugT = now, smWantFrame = False, smLastQueryT = now}, ())
      writeIORef (sdsSnapshot s) snap
      pure snap
