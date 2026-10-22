{-# LANGUAGE TypeFamilies #-}
-- The 'BackendSession' instance for the RGFW session is an orphan by
-- construction: the core owns the backend kind, and this package owns its
-- session type.
{-# OPTIONS_GHC -Wno-orphans #-}

-- | RGFW frame statistics and the debug window body that shows them with the
-- core debug rows and the layout overlay toggle.
module NanoUI.Rgfw.Internal.Debug
  ( RgfwDebugSnapshot (..)
  , RgfwFrameStats (..)
  , RgfwDebugSampler (..)
  , newRgfwDebugSampler
  , rgfwSession
  , emptyRgfwDebug
  , askRgfwDebug
  , debugWindowBody
  ) where

import Control.Monad (when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.TypeLits (symbolSing)
import Text.Printf (printf)

import NanoUI
  ( NanoUI
  , Size (..)
  , V2 (..)
  , checkbox
  , explainLayout
  , explainedNode
  , explainingLayout
  , heading
  , inputMousePos
  , inputWindowSize
  , kvBlock
  , scope
  , separator
  , liftIO
  , withValue
  )
import NanoUI.Backend (Backend (..), BackendSession, SBackend (..), askBackendSession)
import NanoUI.Internal.Monad (askInput)
import NanoUI.Internal.Debug
  ( CoreDebugSnapshot (..)
  , DebugSamplerRef
  , emptyCoreDebugSnapshot
  , formatDrawRows
  , formatExplainRows
  , formatFpsRows
  , newDebugSampler
  , refreshDebugSnapshot
  )
import NanoUI.Rgfw.Internal.Font.Cozette (CozetteScalePath (..), cozetteScalePath)

-- | RGFW-specific facts about the last presented frame.
data RgfwFrameStats = RgfwFrameStats
  { fsNodes    :: !Int
  , fsPhysW    :: !Int
  , fsPhysH    :: !Int
  , fsScale    :: !Float -- ^ active logical -> physical scale
  , fsMonScale :: !Float -- ^ monitor-reported scale
  }
  deriving (Eq, Show)

-- | Published core timings and RGFW physical-frame statistics.
data RgfwDebugSnapshot = RgfwDebugSnapshot
  { dbgCore  :: !CoreDebugSnapshot
  , dbgFrame :: !RgfwFrameStats
  }
  deriving (Eq, Show)

-- | The core sampler and the latest frame stats.
data RgfwDebugSampler = RgfwDebugSampler
  { rdsSampler  :: !DebugSamplerRef
  , rdsFrame    :: !(IORef RgfwFrameStats)
  , rdsPublished :: !(IORef (Maybe RgfwDebugSnapshot))
  , rdsRows :: !(IORef (Maybe RgfwDebugRows))
  }

-- | An RGFW session keeps its debug sampler on the context, so views read
-- it without taking a handle.
type instance BackendSession ('Custom "rgfw") = RgfwDebugSampler

-- | The RGFW session's backend, for 'NanoUI.Backend.withBackendSession'.
rgfwSession :: SBackend ('Custom "rgfw")
rgfwSession = SCustom symbolSing

-- | Allocate a sampler with empty timing and frame stats. The runners make
-- their own and keep it on the context; a custom loop feeds this one and
-- installs it with @'NanoUI.Backend.withBackendSession' ctx 'rgfwSession'@
-- for 'askRgfwDebug' to find. The sampler's fields and 'rgfwSession' are
-- in "NanoUI.Rgfw.Internal.Debug".
newRgfwDebugSampler :: IO RgfwDebugSampler
newRgfwDebugSampler =
  RgfwDebugSampler
    <$> newDebugSampler
    <*> newIORef (dbgFrame emptyRgfwDebug)
    <*> newIORef Nothing
    <*> newIORef Nothing

-- | Placeholder with zero counts and unit scale before measurements exist.
emptyRgfwDebug :: RgfwDebugSnapshot
emptyRgfwDebug =
  RgfwDebugSnapshot
    { dbgCore = emptyCoreDebugSnapshot
    , dbgFrame = RgfwFrameStats {fsNodes = 0, fsPhysW = 0, fsPhysH = 0, fsScale = 1, fsMonScale = 1}
    }

-- | Read the RGFW session's debug data, refreshing at most four times per
-- second. Queries keep the session's debug refresh active. 'Nothing' when no
-- RGFW session drives the view, such as in headless tests.
askRgfwDebug :: NanoUI (Maybe RgfwDebugSnapshot)
askRgfwDebug = askBackendSession rgfwSession >>= traverse snapshot
  where
    snapshot s = do
      inp <- askInput
      liftIO $ refreshDebugSnapshot (rdsSampler s) (rdsPublished s) $ \core -> do
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

-- | Draw timing, geometry, display, and RTS rows for a snapshot, plus a
-- layout overlay checkbox that, when on, shows the node under the pointer.
-- Place this inside a window or panel; it creates no container.
debugWindowBody :: RgfwDebugSnapshot -> NanoUI ()
debugWindowBody snap = do
  owner <- askBackendSession rgfwSession
  (fps, layout, display, rts) <- liftIO $ do
    cached <- maybe (pure Nothing) (readIORef . rdsRows) owner
    case cached of
      Just (RgfwDebugRows shown rows) | shown == snap -> pure rows
      _ -> do
        let rows = (formatFpsRows (dbgCore snap), layoutRows snap, displayRows snap, dbgRts (dbgCore snap))
        mapM_ (\o -> writeIORef (rdsRows o) (Just (RgfwDebugRows snap rows))) owner
        pure rows
  heading "Frame"
  kvBlock fps
  separator
  heading "Layout & Draw"
  kvBlock layout
  on <- explainingLayout
  (_, explain) <- withValue on (checkbox "Outline layout nodes")
  explainLayout explain
  scope $ when explain (kvBlock . formatExplainRows =<< explainedNode)
  separator
  heading "Display & Scale"
  kvBlock display
  separator
  heading "RTS Runtime"
  kvBlock rts
