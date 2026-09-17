-- Shared leaf module for NanoUI.Frame.Paint and its widget painter sibling:
-- both the walker (NanoUI.Frame.Paint) and the chrome painters
-- (NanoUI.Frame.Paint.Widgets) consume the paint env, so the record and its
-- small helpers live here to keep the module graph acyclic (Paint imports
-- Widgets, Widgets imports Types, Paint imports Types).
module NanoUI.Frame.Paint.Types
  ( PaintEnv (..)
  , buildPaintEnv
  , popupPanelRect
  ) where

import Data.IORef (readIORef)
import NanoUI.Context (Context (..))
import NanoUI.Draw (DrawArena)
import NanoUI.Font (FontMetrics)
import NanoUI.Id (WidgetId (..))
import NanoUI.Layout.Arena
  ( NodeArena
  , NodeIdx
  , NodeType (..)
  , getNodeType
  , getParent
  , getRect
  )
import NanoUI.Style (Theme)
import NanoUI.Types (Rect (..))

-- | Everything a paint pass needs, bundled so the walker does not re-read the
-- theme IORef (or rebuild arena handles) for every node. Baked once per
-- frame by 'buildPaintEnv'. Fields are deliberately lazy: under
-- -funbox-strict-fields a strict paint env would unbox every reachable
-- field of Context/Theme/Style recursively, turning each record selector into
-- a ~100-way case that dominates Core size; lazy fields stay single pointers
-- (all bindings here are already-evaluated values, so no thunks are paid).
data PaintEnv = PaintEnv
  { peContext :: Context
  , peNodeArena :: NodeArena
  , peDrawArena :: DrawArena
  , peTheme :: Theme
  , peScope :: Int
    -- ^ The node scope 'peTheme' belongs to. A node in another scope repaints
    -- its subtree with that scope's theme.
  , peFontMetrics :: FontMetrics
  , peOccluders :: [Rect]
  , peHasOccluders :: Bool
  , peFocusRing :: WidgetId
    -- ^ The focused widget while its keyboard focus ring shows, else 0.
  }

-- | Locality helper for callers inside the paint frame loop; a fresh env
-- re-reads the theme once.
{-# NOINLINE buildPaintEnv #-}
buildPaintEnv :: Context -> [Rect] -> IO PaintEnv
buildPaintEnv ctx occluders = do
  theme <- readIORef (ctxTheme ctx)
  focus <- readIORef (ctxFocusId ctx)
  focusVisible <- readIORef (ctxFocusVisible ctx)
  pure PaintEnv
    { peContext = ctx
    , peNodeArena = ctxNodeArena ctx
    , peDrawArena = ctxDrawArena ctx
    , peTheme = theme
    , peScope = 0
    , peFontMetrics = ctxFontMetrics ctx
    , peOccluders = occluders
    , peHasOccluders = not (null occluders)
    , peFocusRing = if focusVisible then focus else WidgetId 0
    }

-- | Rect of the nearest popup-panel ancestor of @idx@, if any. Menu rows use
-- it to paint hover fills edge-to-edge across the panel.
popupPanelRect :: Context -> NodeIdx -> IO (Maybe Rect)
popupPanelRect ctx = go
  where
    go i = do
      p <- getParent (ctxNodeArena ctx) i
      if p < 0
        then pure Nothing
        else do
          nt <- getNodeType (ctxNodeArena ctx) p
          if nt == NodePopup
            then do
              (px, py, pw, ph) <- getRect (ctxNodeArena ctx) p
              pure (Just (Rect px py pw ph))
            else go p
