module NanoUI.Term.Debug
  ( TermDebugSnapshot (..)
  , TermDebugSampler
  , TermDrawStats (..)
  , TermDebugHost (..)
  , newTermDebugSampler
  , noteLoop
  , notePresent
  , noteSkip
  , takeDebugLive
  , readTermDebug
  , emptyTermDebug
  ) where

import Data.Bits ((.&.), shiftR)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Word (Word32, Word64, Word8)
import GHC.Clock (getMonotonicTime)
import NanoUI
  ( Color (..)
  , Size (..)
  , Style (..)
  , Theme (..)
  , V2 (..)
  , styleFg
  , themePanel
  , themeWindow
  )
import NanoUI.Debug (RtsStatsSnapshot (..), blend, debugRefreshSec, readRtsSnapshot)
import NanoUI.Testing (Context, DrawData (..), ctxTheme, drawCmdCount)

data TermDrawStats = TermDrawStats
  { tdsNodes :: Int
  , tdsBaseSpans :: Int
  , tdsOverlaySpans :: Int
  }
  deriving (Eq, Show)

data TermDebugSnapshot = TermDebugSnapshot
  { dbgPresentFps :: Double
  , dbgLoopFps :: Double
  , dbgFrameMs :: Double
  , dbgUiMs :: Double
  , dbgRedraws :: Word64
  , dbgBlits :: Word64
  , dbgSkips :: Word64
  , dbgVerts :: Int
  , dbgIndices :: Int
  , dbgCmds :: Int
  , dbgNodes :: Int
  , dbgBaseSpans :: Int
  , dbgOverlaySpans :: Int
  , dbgWinW :: Float
  , dbgWinH :: Float
  , dbgMouseX :: Float
  , dbgMouseY :: Float
  , dbgThemeFg :: (Word8, Word8, Word8)
  , dbgThemeBg :: (Word8, Word8, Word8)
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

data TermDebugSampler = TermDebugSampler
  { smPresentEma :: Double
  , smLoopEma :: Double
  , smLastPresentT :: Double
  , smLastDebugT :: Double
  , smRedraws :: Word64
  , smBlits :: Word64
  , smSkips :: Word64
  , smUiMs :: Double
  , smFrameMs :: Double
  , smVerts :: Int
  , smIndices :: Int
  , smCmds :: Int
  , smNodes :: Int
  , smBaseSpans :: Int
  , smOverlaySpans :: Int
  , smWantFrame :: Bool
  , smSnapshot :: TermDebugSnapshot
  }

newtype TermDebugHost = TermDebugHost {termDebugSampler :: IORef TermDebugSampler}

type SamplerRef = IORef TermDebugSampler

newTermDebugSampler :: IO SamplerRef
newTermDebugSampler = do
  now <- getMonotonicTime
  newIORef
    TermDebugSampler
      { smPresentEma = 0
      , smLoopEma = 0
      , smLastPresentT = now
      , smLastDebugT = 0
      , smRedraws = 0
      , smBlits = 0
      , smSkips = 0
      , smUiMs = 0
      , smFrameMs = 0
      , smVerts = 0
      , smIndices = 0
      , smCmds = 0
      , smNodes = 0
      , smBaseSpans = 0
      , smOverlaySpans = 0
      , smWantFrame = False
      , smSnapshot = emptyTermDebug
      }

emptyTermDebug :: TermDebugSnapshot
emptyTermDebug =
  TermDebugSnapshot
    { dbgPresentFps = 0
    , dbgLoopFps = 0
    , dbgFrameMs = 0
    , dbgUiMs = 0
    , dbgRedraws = 0
    , dbgBlits = 0
    , dbgSkips = 0
    , dbgVerts = 0
    , dbgIndices = 0
    , dbgCmds = 0
    , dbgNodes = 0
    , dbgBaseSpans = 0
    , dbgOverlaySpans = 0
    , dbgWinW = 0
    , dbgWinH = 0
    , dbgMouseX = 0
    , dbgMouseY = 0
    , dbgThemeFg = (0, 0, 0)
    , dbgThemeBg = (0, 0, 0)
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

notePresent :: SamplerRef -> Double -> DrawData -> TermDrawStats -> Bool -> IO ()
notePresent ref uiMs dd stats blitted = do
  now <- getMonotonicTime
  atomicModifyIORef' ref $ \s ->
    let dt = now - smLastPresentT s
        fps = if dt > 1e-4 then 1 / dt else 0
        frameMs = dt * 1000
     in
      ( s
          { smPresentEma = blend (smPresentEma s) fps
          , smLastPresentT = now
          , smRedraws = smRedraws s + 1
          , smBlits = smBlits s + if blitted then 1 else 0
          , smUiMs = uiMs
          , smFrameMs = frameMs
          , smVerts = drawVertexCount dd
          , smIndices = drawIndexCount dd
          , smCmds = drawCmdCount dd
          , smNodes = tdsNodes stats
          , smBaseSpans = tdsBaseSpans stats
          , smOverlaySpans = tdsOverlaySpans stats
          , smWantFrame = True
          }
      , ()
      )

readTermDebug :: SamplerRef -> Size -> V2 -> Context -> IO TermDebugSnapshot
readTermDebug ref (Size ww wh) (V2 mx my) ctx = do
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
      let theme = ctxTheme ctx
          fgCol = styleFg (themePanel theme)
          bgCol = themeWindow theme
          snap' =
            TermDebugSnapshot
              { dbgPresentFps = smPresentEma cur
              , dbgLoopFps = smLoopEma cur
              , dbgFrameMs = smFrameMs cur
              , dbgUiMs = smUiMs cur
              , dbgRedraws = smRedraws cur
              , dbgBlits = smBlits cur
              , dbgSkips = smSkips cur
              , dbgVerts = smVerts cur
              , dbgIndices = smIndices cur
              , dbgCmds = smCmds cur
              , dbgNodes = smNodes cur
              , dbgBaseSpans = smBaseSpans cur
              , dbgOverlaySpans = smOverlaySpans cur
              , dbgWinW = ww
              , dbgWinH = wh
              , dbgMouseX = mx
              , dbgMouseY = my
              , dbgThemeFg = colorRgb fgCol
              , dbgThemeBg = colorRgb bgCol
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

colorRgb :: Color -> (Word8, Word8, Word8)
colorRgb (Color w) =
  ( fromIntegral ((w `shiftR` 24) .&. 0xff)
  , fromIntegral ((w `shiftR` 16) .&. 0xff)
  , fromIntegral ((w `shiftR` 8) .&. 0xff)
  )
