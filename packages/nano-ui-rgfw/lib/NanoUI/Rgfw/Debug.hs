{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Rgfw.Debug
  ( RgfwDebugSnapshot (..)
  , RgfwFrameStats (..)
  , RgfwDebugSampler
  , RgfwDebugHost (..)
  , newRgfwDebugSampler
  , noteLoop
  , notePresent
  , emptyRgfwDebug
  , askRgfwDebug
  , debugWindowBody
  ) where

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Effectful (Eff, (:>))
import GHC.Clock (getMonotonicTime)
import Text.Printf (printf)

import NanoUI
  ( Size (..)
  , Ui
  , V2 (..)
  , heading
  , inputMousePos
  , inputWindowSize
  , kvBlock
  , sep
  , uiIO
  )
import NanoUI.Monad
  ( askHost
  , askInput
  )
import NanoUI.Debug
  ( CoreDebugSnapshot (..)
  , DebugSampler (..)
  , DebugSamplerRef
  , debugRefreshSec
  , emptyCoreDebugSnapshot
  , formatCoreRtsRows
  , formatDrawRows
  , formatFpsRows
  , makeCoreDebugSnapshot
  , newDebugSampler
  , noteDebugLoop
  , noteDebugPresent
  , presentRate
  , readRtsSnapshot
  )
import NanoUI.Rgfw.Font.Cozette (CozetteScalePath (..), cozetteScalePath)
import NanoUI.Testing (DrawData (..), drawCmdCount)

-- | RGFW-specific facts about the last presented frame.
data RgfwFrameStats = RgfwFrameStats
  { fsNodes    :: !Int
  , fsPhysW    :: !Int
  , fsPhysH    :: !Int
  , fsScale    :: !Float -- ^ active logical -> physical scale
  , fsMonScale :: !Float -- ^ monitor-reported scale
  }
  deriving (Eq, Show)

data RgfwDebugSnapshot = RgfwDebugSnapshot
  { dbgCore  :: !CoreDebugSnapshot
  , dbgFrame :: !RgfwFrameStats
  }
  deriving (Eq, Show)

-- | The core sampler, the latest frame stats, and the last published
-- snapshot.
data RgfwDebugSampler = RgfwDebugSampler
  { rdsSampler  :: !DebugSamplerRef
  , rdsFrame    :: !(IORef RgfwFrameStats)
  , rdsSnapshot :: !(IORef RgfwDebugSnapshot)
  }

newtype RgfwDebugHost = RgfwDebugHost RgfwDebugSampler

newRgfwDebugSampler :: IO RgfwDebugSampler
newRgfwDebugSampler =
  RgfwDebugSampler
    <$> newDebugSampler
    <*> newIORef (dbgFrame emptyRgfwDebug)
    <*> newIORef emptyRgfwDebug

emptyRgfwDebug :: RgfwDebugSnapshot
emptyRgfwDebug =
  RgfwDebugSnapshot
    { dbgCore = emptyCoreDebugSnapshot
    , dbgFrame = RgfwFrameStats {fsNodes = 0, fsPhysW = 0, fsPhysH = 0, fsScale = 1, fsMonScale = 1}
    }

noteLoop :: RgfwDebugSampler -> Float -> IO ()
noteLoop = noteDebugLoop . rdsSampler

-- | Record a presented frame: UI, render, swap and total milliseconds, the
-- draw buffer it presented, and the RGFW frame stats.
notePresent :: RgfwDebugSampler -> Double -> Double -> Double -> Double -> DrawData -> RgfwFrameStats -> IO ()
notePresent s uiMs renderMs swapMs frameMs dd stats = do
  noteDebugPresent
    (rdsSampler s)
    uiMs
    renderMs
    swapMs
    frameMs
    (drawVertexCount dd)
    (drawIndexCount dd)
    (drawCmdCount dd)
  writeIORef (rdsFrame s) stats

-- | The snapshot, refreshed at most every 'debugRefreshSec'.
readRgfwDebug :: RgfwDebugSampler -> Size -> V2 -> IO RgfwDebugSnapshot
readRgfwDebug s (Size lw lh) (V2 mx my) = do
  now <- getMonotonicTime
  sampler <- readIORef (rdsSampler s)
  if smLastDebugT sampler > 0 && now - smLastDebugT sampler < debugRefreshSec
    then readIORef (rdsSnapshot s)
    else do
      rts <- readRtsSnapshot
      (rate, sampled) <-
        atomicModifyIORef' (rdsSampler s) $ \cur ->
          let (rated, rate) = presentRate now cur
              cur' = rated {smLastDebugT = now}
           in (cur', (rate, cur'))
      frame <- readIORef (rdsFrame s)
      let snap =
            RgfwDebugSnapshot
              { dbgCore = (makeCoreDebugSnapshot sampled lw lh mx my rts) {dbgPresentFps = rate}
              , dbgFrame = frame
              }
      writeIORef (rdsSnapshot s) snap
      pure snap

askRgfwDebug :: Ui :> es => Eff es RgfwDebugSnapshot
askRgfwDebug = do
  inp <- askInput
  mhost <- askHost @RgfwDebugHost
  case mhost of
    Nothing -> pure emptyRgfwDebug
    Just (RgfwDebugHost s) ->
      uiIO (readRgfwDebug s (inputWindowSize inp) (inputMousePos inp))

-- | Arena nodes plus the draw buffer sizes.
layoutRows :: RgfwDebugSnapshot -> [(Text, Text)]
layoutRows s = ("nodes", T.pack (show (fsNodes (dbgFrame s)))) : formatDrawRows (dbgCore s)

displayRows :: RgfwDebugSnapshot -> [(Text, Text)]
displayRows s =
  [ ("logical win", T.pack (printf "%.0fx%.0f" (dbgWinW c) (dbgWinH c)))
  , ("physical win", T.pack (printf "%dx%d" (fsPhysW f) (fsPhysH f)))
  , ("scale active", T.pack (printf "%.2fx" (fsScale f)))
  , ("scale monitor", T.pack (printf "%.2fx" (fsMonScale f)))
  , ("glyph path", glyphPath)
  , ("mouse pos", T.pack (printf "%.0f, %.0f" (dbgMouseX c) (dbgMouseY c)))
  ]
  where
    c = dbgCore s
    f = dbgFrame s
    glyphPath = case cozetteScalePath (fsScale f) of
      ScaleExact1x -> "1x bitmap"
      ScaleExact2x -> "2x EPX bitmap"
      ScaleExact4x -> "4x EPX bitmap"
      ScaleBoxFrom2x -> "box-averaged 2x EPX"
      ScaleBoxFrom4x -> "box-averaged 4x EPX"

debugWindowBody :: Ui :> es => RgfwDebugSnapshot -> Eff es ()
debugWindowBody snap = do
  heading "Frame"
  kvBlock (formatFpsRows (dbgCore snap))
  sep
  heading "Layout & Draw"
  kvBlock (layoutRows snap)
  sep
  heading "Display & Scale"
  kvBlock (displayRows snap)
  sep
  heading "RTS Runtime"
  kvBlock (formatCoreRtsRows (dbgCore snap))
