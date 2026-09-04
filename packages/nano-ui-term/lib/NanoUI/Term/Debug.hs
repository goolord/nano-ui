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
  , dbgPresentFps
  , dbgLoopFps
  , dbgFrameMs
  , dbgUiMs
  , dbgSkips
  , dbgVerts
  , dbgIndices
  , dbgCmds
  , dbgWinW
  , dbgWinH
  , dbgMouseX
  , dbgMouseY
  , dbgRtsOn
  , dbgGcs
  , dbgMajorGcs
  , dbgAllocMb
  , dbgLiveMb
  , dbgMaxMemMb
  , dbgCopiedMb
  , dbgGcPct
  , dbgLastGcGen
  , dbgLastGcMs
  , dbgCaps
  , dbgCpus
  ) where

import Data.Bits ((.&.), shiftR)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
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
import NanoUI.Debug
  ( CoreDebugSnapshot
  , DebugSampler (..)
  , blend
  , debugRefreshSec
  , emptyCoreDebugSnapshot
  , makeCoreDebugSnapshot
  , newDebugSampler
  , noteDebugLoop
  , noteDebugSkip
  , readRtsSnapshot
  )
import qualified NanoUI.Debug as D
import NanoUI.Testing (Context, DrawData (..), ctxTheme, drawCmdCount)

data TermDrawStats = TermDrawStats
  { tdsNodes        :: !Int
  , tdsBaseSpans    :: !Int
  , tdsOverlaySpans :: !Int
  }
  deriving (Eq, Show)

data TermDebugSnapshot = TermDebugSnapshot
  { dbgCore         :: !CoreDebugSnapshot
  , dbgRedraws      :: !Word64
  , dbgBlits        :: !Word64
  , dbgNodes        :: !Int
  , dbgBaseSpans    :: !Int
  , dbgOverlaySpans :: !Int
  , dbgThemeFg      :: !(Word8, Word8, Word8)
  , dbgThemeBg      :: !(Word8, Word8, Word8)
  }
  deriving (Eq, Show)

-- Accessors for backward compatibility and clean field lookup
dbgPresentFps :: TermDebugSnapshot -> Double
dbgPresentFps = D.dbgPresentFps . dbgCore

dbgLoopFps :: TermDebugSnapshot -> Double
dbgLoopFps = D.dbgLoopFps . dbgCore

dbgFrameMs :: TermDebugSnapshot -> Double
dbgFrameMs = D.dbgFrameMs . dbgCore

dbgUiMs :: TermDebugSnapshot -> Double
dbgUiMs = D.dbgUiMs . dbgCore

dbgSkips :: TermDebugSnapshot -> Word64
dbgSkips = D.dbgSkips . dbgCore

dbgVerts :: TermDebugSnapshot -> Int
dbgVerts = D.dbgVerts . dbgCore

dbgIndices :: TermDebugSnapshot -> Int
dbgIndices = D.dbgIndices . dbgCore

dbgCmds :: TermDebugSnapshot -> Int
dbgCmds = D.dbgCmds . dbgCore

dbgWinW :: TermDebugSnapshot -> Float
dbgWinW = D.dbgWinW . dbgCore

dbgWinH :: TermDebugSnapshot -> Float
dbgWinH = D.dbgWinH . dbgCore

dbgMouseX :: TermDebugSnapshot -> Float
dbgMouseX = D.dbgMouseX . dbgCore

dbgMouseY :: TermDebugSnapshot -> Float
dbgMouseY = D.dbgMouseY . dbgCore

dbgRtsOn :: TermDebugSnapshot -> Bool
dbgRtsOn = D.dbgRtsOn . dbgCore

dbgGcs :: TermDebugSnapshot -> Word32
dbgGcs = D.dbgGcs . dbgCore

dbgMajorGcs :: TermDebugSnapshot -> Word32
dbgMajorGcs = D.dbgMajorGcs . dbgCore

dbgAllocMb :: TermDebugSnapshot -> Double
dbgAllocMb = D.dbgAllocMb . dbgCore

dbgLiveMb :: TermDebugSnapshot -> Double
dbgLiveMb = D.dbgLiveMb . dbgCore

dbgMaxMemMb :: TermDebugSnapshot -> Double
dbgMaxMemMb = D.dbgMaxMemMb . dbgCore

dbgCopiedMb :: TermDebugSnapshot -> Double
dbgCopiedMb = D.dbgCopiedMb . dbgCore

dbgGcPct :: TermDebugSnapshot -> Double
dbgGcPct = D.dbgGcPct . dbgCore

dbgLastGcGen :: TermDebugSnapshot -> Word32
dbgLastGcGen = D.dbgLastGcGen . dbgCore

dbgLastGcMs :: TermDebugSnapshot -> Double
dbgLastGcMs = D.dbgLastGcMs . dbgCore

dbgCaps :: TermDebugSnapshot -> Int
dbgCaps = D.dbgCaps . dbgCore

dbgCpus :: TermDebugSnapshot -> Int
dbgCpus = D.dbgCpus . dbgCore

data TermDebugSamplerState = TermDebugSamplerState
  { smSampler       :: !(IORef DebugSampler)
  , smSnapshot      :: !(IORef TermDebugSnapshot)
  , smRedraws       :: !Word64
  , smBlits         :: !Word64
  , smNodes         :: !Int
  , smBaseSpans     :: !Int
  , smOverlaySpans  :: !Int
  }

type TermDebugSampler = IORef TermDebugSamplerState

newtype TermDebugHost = TermDebugHost TermDebugSampler

newTermDebugSampler :: IO TermDebugSampler
newTermDebugSampler = do
  sRef <- newDebugSampler
  snapRef <- newIORef emptyTermDebug
  newIORef
    TermDebugSamplerState
      { smSampler       = sRef
      , smSnapshot      = snapRef
      , smRedraws       = 0
      , smBlits         = 0
      , smNodes         = 0
      , smBaseSpans     = 0
      , smOverlaySpans  = 0
      }

emptyTermDebug :: TermDebugSnapshot
emptyTermDebug =
  TermDebugSnapshot
    { dbgCore         = emptyCoreDebugSnapshot
    , dbgRedraws      = 0
    , dbgBlits        = 0
    , dbgNodes        = 0
    , dbgBaseSpans    = 0
    , dbgOverlaySpans = 0
    , dbgThemeFg      = (0, 0, 0)
    , dbgThemeBg      = (0, 0, 0)
    }

noteLoop :: TermDebugSampler -> Float -> IO ()
noteLoop ref dt = do
  s <- readIORef ref
  noteDebugLoop (smSampler s) dt

noteSkip :: TermDebugSampler -> IO ()
noteSkip ref = do
  s <- readIORef ref
  noteDebugSkip (smSampler s)

takeDebugLive :: TermDebugSampler -> Bool -> IO Bool
takeDebugLive ref windowOpen = do
  s <- readIORef ref
  D.takeDebugLive (smSampler s) windowOpen

notePresent :: TermDebugSampler -> Double -> DrawData -> TermDrawStats -> Bool -> IO ()
notePresent ref uiMs dd stats blitted = do
  now <- getMonotonicTime
  s <- readIORef ref
  let innerRef = smSampler s
  atomicModifyIORef' innerRef $ \cur ->
    let dt = now - smLastPresentT cur
        fps = if dt > 1e-4 then 1 / dt else 0
        frameMs = dt * 1000
        ema' =
          if fps > 0
            then if smPresentEma cur <= 0 then fps else blend (smPresentEma cur) fps
            else smPresentEma cur
     in ( cur
            { smPresentEma   = ema'
            , smLastPresentT = now
            , smPresents     = smPresents cur + 1
            , smUiMs         = uiMs
            , smFrameMs      = frameMs
            , smVerts        = drawVertexCount dd
            , smIndices      = drawIndexCount dd
            , smCmds         = drawCmdCount dd
            , smWantFrame    = True
            }
        , ()
        )
  atomicModifyIORef' ref $ \st ->
    ( st
        { smRedraws       = smRedraws st + 1
        , smBlits         = smBlits st + if blitted then 1 else 0
        , smNodes         = tdsNodes stats
        , smBaseSpans     = tdsBaseSpans stats
        , smOverlaySpans  = tdsOverlaySpans stats
        }
    , ()
    )

readTermDebug :: TermDebugSampler -> Size -> V2 -> Context -> IO TermDebugSnapshot
readTermDebug ref (Size ww wh) (V2 mx my) ctx = do
  now <- getMonotonicTime
  st <- readIORef ref
  (refresh, cur) <-
    atomicModifyIORef' (smSampler st) $ \s ->
      let elapsed = now - smLastDebugT s
          refresh = smLastDebugT s <= 0 || elapsed >= debugRefreshSec
       in (s {smWantFrame = refresh}, (refresh, s))
  if not refresh
    then readIORef (smSnapshot st)
    else do
      rts <- readRtsSnapshot
      let theme = ctxTheme ctx
          fgCol = styleFg (themePanel theme)
          bgCol = themeWindow theme
          core = makeCoreDebugSnapshot cur ww wh mx my rts
          snap' =
            TermDebugSnapshot
              { dbgCore         = core
              , dbgRedraws      = smRedraws st
              , dbgBlits        = smBlits st
              , dbgNodes        = smNodes st
              , dbgBaseSpans    = smBaseSpans st
              , dbgOverlaySpans = smOverlaySpans st
              , dbgThemeFg      = colorRgb fgCol
              , dbgThemeBg      = colorRgb bgCol
              }
      atomicModifyIORef' (smSampler st) $ \s ->
        (s {smLastDebugT = now, smWantFrame = True}, ())
      writeIORef (smSnapshot st) snap'
      pure snap'

colorRgb :: Color -> (Word8, Word8, Word8)
colorRgb (Color w) =
  ( fromIntegral ((w `shiftR` 24) .&. 0xff)
  , fromIntegral ((w `shiftR` 16) .&. 0xff)
  , fromIntegral ((w `shiftR` 8) .&. 0xff)
  )
