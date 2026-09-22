-- | Shared paint-pass inputs for the node walker and widget painters.
module NanoUI.Internal.Frame.Paint.Types
  ( PaintEnv (..)
  , buildPaintEnv
  , popupPanelRect
  ) where

import Data.IORef (readIORef)
import Data.Primitive.PrimArray (PrimArray)
import NanoUI.Internal.Context (Context (..))
import NanoUI.Internal.Draw (DrawArena, getClipPieces)
import NanoUI.Internal.Font (FontMetrics)
import NanoUI.Internal.Id (WidgetId (..))
import NanoUI.Internal.Layout.Arena
  ( NodeArena
  , NodeIdx
  , NodeType (..)
  , getNodeRect
  , getNodeType
  , getParent
  , walkAncestors
  )
import NanoUI.Internal.Style (Theme)
import NanoUI.Internal.Types (Rect (..))

-- | Context, arenas, fonts, and interaction state read once for a paint pass.
-- Fields retain boxed references so compiler unboxing does not expand the
-- context and theme records at every recursive call.
data PaintEnv = PaintEnv
  { peContext :: Context
  , peNodeArena :: NodeArena
  , peDrawArena :: DrawArena
  , peTheme :: Theme
  , peScope :: Int
    -- ^ The node scope 'peTheme' belongs to. A node in another scope repaints
    -- its subtree with that scope's theme.
  , peFontMetrics :: FontMetrics
  , peOccluders :: PrimArray Float
    -- ^ Opaque floating panel rects as @x0, y0, x1, y1@ runs; empty when the
    -- frame has none.
  , peFocusRing :: WidgetId
    -- ^ The focused widget while its keyboard focus ring shows, else 0.
  , pePieces :: PrimArray Float
    -- ^ The frame's damage pieces as @x0, y0, x1, y1@ runs, of which a node
    -- must meet one to paint; empty when the clip is the one piece.
  }

-- | Locality helper for callers inside the paint frame loop; a fresh env
-- re-reads the theme once.
{-# NOINLINE buildPaintEnv #-}
buildPaintEnv :: Context -> PrimArray Float -> IO PaintEnv
buildPaintEnv ctx occluders = do
  theme <- readIORef (ctxTheme ctx)
  focus <- readIORef (ctxFocusId ctx)
  focusVisible <- readIORef (ctxFocusVisible ctx)
  pieces <- getClipPieces (ctxDrawArena ctx)
  pure PaintEnv
    { peContext = ctx
    , peNodeArena = ctxNodeArena ctx
    , peDrawArena = ctxDrawArena ctx
    , peTheme = theme
    , peScope = 0
    , peFontMetrics = ctxFontMetrics ctx
    , peOccluders = occluders
    , peFocusRing = if focusVisible then focus else WidgetId 0
    , pePieces = pieces
    }

-- | Rect of the nearest popup-panel ancestor of @idx@, if any. Menu rows use
-- it to paint hover fills edge-to-edge across the panel.
popupPanelRect :: Context -> NodeIdx -> IO (Maybe Rect)
popupPanelRect ctx idx = do
  parent <- getParent na idx
  walkAncestors na parent $ \p -> do
    nt <- getNodeType na p
    if nt == NodePopup then Just <$> getNodeRect na p else pure Nothing
  where
    na = ctxNodeArena ctx
