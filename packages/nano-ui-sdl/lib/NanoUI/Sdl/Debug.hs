module NanoUI.Sdl.Debug
  ( SdlDebugSnapshot (..)
  , SdlDebugSampler
  , newSdlDebugSampler
  , noteLoop
  , notePresent
  , noteSkip
  , takeDebugLive
  , readSdlDebug
  , emptySdlDebug
  ) where

import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Text (Text)
import Data.Word (Word32, Word64)
import GHC.Clock (getMonotonicTime)
import NanoUI (Size (..), V2 (..))
import NanoUI.Debug (RtsStatsSnapshot (..), blend, debugRefreshSec, readRtsSnapshot)
import NanoUI.Testing (DrawData (..), drawCmdCount)

data SdlDebugSnapshot = SdlDebugSnapshot
  { dbgPresentFps :: Double
  , dbgLoopFps :: Double
  , dbgFrameMs :: Double
  , dbgUiMs :: Double
  , dbgRenderMs :: Double
  , dbgPresentMs :: Double
  , dbgPresents :: Word64
  , dbgSkips :: Word64
  , dbgVerts :: Int
  , dbgIndices :: Int
  , dbgCmds :: Int
  , dbgWinW :: Float
  , dbgWinH :: Float
  , dbgMouseX :: Float
  , dbgMouseY :: Float
  , dbgScale :: Float
  , dbgFontPath :: FilePath
  , dbgRenderer :: Text
  , dbgVsync :: Bool
  , dbgRtsOn :: Bool
  , dbgGcs :: Word32
  , dbgMajorGcs :: Word32
  , dbgAllocMb :: Double
  , dbgLiveMb :: Double
  , dbgMaxMemMb :: Double
  , dbgCopiedMb :: Double
  , dbgGcPct :: Double
  , dbgLastGcGen :: Word32
  , dbgLastGcMs :: Double
  , dbgCaps :: Int
  , dbgCpus :: Int
  }
  deriving (Eq, Show)

data SdlDebugSampler = SdlDebugSampler
  { smPresentEma :: Double
  , smLoopEma :: Double
  , smLastPresentT :: Double
  , smLastDebugT :: Double
  , smPresents :: Word64
  , smSkips :: Word64
  , smUiMs :: Double
  , smRenderMs :: Double
  , smPresentMs :: Double
  , smFrameMs :: Double
  , smVerts :: Int
  , smIndices :: Int
  , smCmds :: Int
  , smWantFrame :: Bool
  , smSnapshot :: SdlDebugSnapshot
  }

type SamplerRef = IORef SdlDebugSampler

newSdlDebugSampler :: IO SamplerRef
newSdlDebugSampler = do
  now <- getMonotonicTime
  newIORef
    SdlDebugSampler
      { smPresentEma = 0
      , smLoopEma = 0
      , smLastPresentT = now
      , smLastDebugT = 0
      , smPresents = 0
      , smSkips = 0
      , smUiMs = 0
      , smRenderMs = 0
      , smPresentMs = 0
      , smFrameMs = 0
      , smVerts = 0
      , smIndices = 0
      , smCmds = 0
      , smWantFrame = False
      , smSnapshot = emptySdlDebug
      }

emptySdlDebug :: SdlDebugSnapshot
emptySdlDebug =
  SdlDebugSnapshot
    { dbgPresentFps = 0
    , dbgLoopFps = 0
    , dbgFrameMs = 0
    , dbgUiMs = 0
    , dbgRenderMs = 0
    , dbgPresentMs = 0
    , dbgPresents = 0
    , dbgSkips = 0
    , dbgVerts = 0
    , dbgIndices = 0
    , dbgCmds = 0
    , dbgWinW = 0
    , dbgWinH = 0
    , dbgMouseX = 0
    , dbgMouseY = 0
    , dbgScale = 1
    , dbgFontPath = ""
    , dbgRenderer = ""
    , dbgVsync = True
    , dbgRtsOn = False
    , dbgGcs = 0
    , dbgMajorGcs = 0
    , dbgAllocMb = 0
    , dbgLiveMb = 0
    , dbgMaxMemMb = 0
    , dbgCopiedMb = 0
    , dbgGcPct = 0
    , dbgLastGcGen = 0
    , dbgLastGcMs = 0
    , dbgCaps = 0
    , dbgCpus = 0
    }

noteLoop :: SamplerRef -> Float -> IO ()
noteLoop ref dt =
  atomicModifyIORef' ref $ \s ->
    let fps = if dt > 1e-4 then 1 / realToFrac dt else 0
     in (s {smLoopEma = blend (smLoopEma s) fps}, ())

noteSkip :: SamplerRef -> IO ()
noteSkip ref =
  atomicModifyIORef' ref $ \s -> (s {smSkips = smSkips s + 1}, ())

takeDebugLive :: SamplerRef -> Bool -> IO Bool
takeDebugLive _ False = pure False
takeDebugLive ref True = do
  now <- getMonotonicTime
  atomicModifyIORef' ref $ \s ->
    let elapsed = now - smLastDebugT s
        due = smLastDebugT s <= 0 || elapsed >= debugRefreshSec
        want = smWantFrame s || due
     in (s {smWantFrame = False}, want)

notePresent :: SamplerRef -> Double -> Double -> Double -> Double -> DrawData -> IO ()
notePresent ref uiMs renderMs presentMs frameMs dd = do
  now <- getMonotonicTime
  atomicModifyIORef' ref $ \s ->
    let dt = now - smLastPresentT s
        fps = if dt > 1e-4 then 1 / dt else 0
     in
      ( s
          { smPresentEma = blend (smPresentEma s) fps
          , smLastPresentT = now
          , smPresents = smPresents s + 1
          , smUiMs = uiMs
          , smRenderMs = renderMs
          , smPresentMs = presentMs
          , smFrameMs = frameMs
          , smVerts = drawVertexCount dd
          , smIndices = drawIndexCount dd
          , smCmds = drawCmdCount dd
          }
      , ()
      )

readSdlDebug :: SamplerRef -> Size -> V2 -> FilePath -> Float -> Text -> Bool -> IO SdlDebugSnapshot
readSdlDebug ref (Size ww wh) (V2 mx my) fontPath scale renderer vsync = do
  now <- getMonotonicTime
  (refresh, cur) <-
    atomicModifyIORef' ref $ \s ->
      let elapsed = now - smLastDebugT s
          refresh = smLastDebugT s <= 0 || elapsed >= debugRefreshSec
       in (s {smWantFrame = refresh}, (refresh, s))
  if not refresh
    then pure (smSnapshot cur)
    else do
      rts <- readRtsSnapshot
      let snap' =
            SdlDebugSnapshot
              { dbgPresentFps = smPresentEma cur
              , dbgLoopFps = smLoopEma cur
              , dbgFrameMs = smFrameMs cur
              , dbgUiMs = smUiMs cur
              , dbgRenderMs = smRenderMs cur
              , dbgPresentMs = smPresentMs cur
              , dbgPresents = smPresents cur
              , dbgSkips = smSkips cur
              , dbgVerts = smVerts cur
              , dbgIndices = smIndices cur
              , dbgCmds = smCmds cur
              , dbgWinW = ww
              , dbgWinH = wh
              , dbgMouseX = mx
              , dbgMouseY = my
              , dbgScale = scale
              , dbgFontPath = fontPath
              , dbgRenderer = renderer
              , dbgVsync = vsync
              , dbgRtsOn = rtsEnabled rts
              , dbgGcs = rtsGcs rts
              , dbgMajorGcs = rtsMajorGcs rts
              , dbgAllocMb = rtsAllocMb rts
              , dbgLiveMb = rtsLiveMb rts
              , dbgMaxMemMb = rtsMaxMemMb rts
              , dbgCopiedMb = rtsCopiedMb rts
              , dbgGcPct = rtsGcPct rts
              , dbgLastGcGen = rtsLastGcGen rts
              , dbgLastGcMs = rtsLastGcMs rts
              , dbgCaps = rtsCaps rts
              , dbgCpus = rtsCpus rts
              }
      atomicModifyIORef' ref $ \s ->
        (s {smLastDebugT = now, smSnapshot = snap', smWantFrame = True}, ())
      pure snap'
