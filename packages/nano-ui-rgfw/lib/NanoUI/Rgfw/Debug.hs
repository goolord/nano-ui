{-# LANGUAGE OverloadedStrings #-}

-- | RGFW frame statistics and the debug window body that shows them with the
-- core debug rows.
module NanoUI.Rgfw.Debug
  ( RgfwDebugSnapshot (..)
  , RgfwFrameStats (..)
  , RgfwDebugSampler (..)
  , RgfwDebugHost (..)
  , newRgfwDebugSampler
  , emptyRgfwDebug
  , askRgfwDebug
  , debugWindowBody
  ) where

import Data.IORef (IORef, newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Effectful (Eff, (:>))
import Text.Printf (printf)

import NanoUI
  ( Size (..)
  , Ui
  , V2 (..)
  , heading
  , inputMousePos
  , inputWindowSize
  , kvBlock
  , separator
  , uiIO
  )
import NanoUI.Context (askHostIO, setHost)
import NanoUI.Monad
  ( askContext
  , askHost
  , askInput
  )
import NanoUI.Debug
  ( CoreDebugSnapshot (..)
  , DebugSamplerRef
  , emptyCoreDebugSnapshot
  , formatCoreRtsRows
  , formatDrawRows
  , formatFpsRows
  , newDebugSampler
  , refreshDebugSnapshot
  )
import NanoUI.Rgfw.Font.Cozette (CozetteScalePath (..), cozetteScalePath)

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

askRgfwDebug :: Ui :> es => Eff es RgfwDebugSnapshot
askRgfwDebug = do
  inp <- askInput
  mhost <- askHost @RgfwDebugHost
  case mhost of
    Nothing -> pure emptyRgfwDebug
    Just (RgfwDebugHost s) ->
      uiIO $ refreshDebugSnapshot (rdsSampler s) (rdsSnapshot s) $ \core -> do
        let Size lw lh = inputWindowSize inp
            V2 mx my = inputMousePos inp
        frame <- readIORef (rdsFrame s)
        pure RgfwDebugSnapshot {dbgCore = core {dbgWinW = lw, dbgWinH = lh, dbgMouseX = mx, dbgMouseY = my}, dbgFrame = frame}

-- | Arena nodes plus the draw buffer sizes.
layoutRows :: RgfwDebugSnapshot -> Rows
layoutRows s = ("nodes", T.pack (show (fsNodes (dbgFrame s)))) : formatDrawRows (dbgCore s)

displayRows :: RgfwDebugSnapshot -> Rows
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

-- | The readout's formatted rows with the snapshot they show. The snapshot
-- refreshes at 4 Hz, so rows are formatted once per refresh, not every frame.
data RgfwDebugRows = RgfwDebugRows !RgfwDebugSnapshot !(Rows, Rows, Rows, Rows)

type Rows = [(Text, Text)]

debugWindowBody :: Ui :> es => RgfwDebugSnapshot -> Eff es ()
debugWindowBody snap = do
  ctx <- askContext
  (fps, layout, display, rts) <- uiIO $ do
    cached <- askHostIO ctx
    case cached of
      Just (RgfwDebugRows shown rows) | shown == snap -> pure rows
      _ -> do
        let rows = (formatFpsRows (dbgCore snap), layoutRows snap, displayRows snap, formatCoreRtsRows (dbgCore snap))
        setHost ctx (RgfwDebugRows snap rows)
        pure rows
  heading "Frame"
  kvBlock fps
  separator
  heading "Layout & Draw"
  kvBlock layout
  separator
  heading "Display & Scale"
  kvBlock display
  separator
  heading "RTS Runtime"
  kvBlock rts
