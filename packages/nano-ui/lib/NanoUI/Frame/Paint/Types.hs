-- Shared leaf module for NanoUI.Frame.Paint and its widget painter sibling:
-- both the walker (NanoUI.Frame.Paint) and the chrome painters
-- (NanoUI.Frame.Paint.Widgets) consume the paint env, so the record and its
-- small helpers live here to keep the module graph acyclic (Paint imports
-- Widgets, Widgets imports Types, Paint imports Types).
module NanoUI.Frame.Paint.Types
  ( PaintEnv (..)
  , buildPaintEnv
  , popupPanelRect
  , resolveNodeFont
  ) where


import Data.IORef (readIORef)
import NanoUI.Context
  ( Context (..)
  )
import NanoUI.Draw (DrawArena)
import NanoUI.Font (FontMetrics)
import NanoUI.Types (HostProfile, isCellHost)
import NanoUI.Layout.Arena
  ( NodeArena
  , NodeIdx
  , NodeType (..)
  , getNodeType
  , getParent
  , getRect
  )
import NanoUI.Style
  ( FontStyle (..)
  , FontVariant (..)
  , FontWeight (..)
  , Theme
  )
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
  , peFontMetrics :: FontMetrics
  , peMonoMetrics :: FontMetrics
  , peHost :: HostProfile
  , peTerminal :: Bool
  , peOccluders :: [Rect]
  , peHasOccluders :: Bool
  }

-- | Locality helper for callers inside the paint frame loop; a fresh env
-- re-reads the theme once.
{-# NOINLINE buildPaintEnv #-}
buildPaintEnv :: Context -> [Rect] -> IO PaintEnv
buildPaintEnv ctx occluders = do
  theme <- readIORef (ctxTheme ctx)
  pure PaintEnv
    { peContext = ctx
    , peNodeArena = ctxNodeArena ctx
    , peDrawArena = ctxDrawArena ctx
    , peTheme = theme
    , peFontMetrics = ctxFontMetrics ctx
    , peMonoMetrics = ctxMonoFontMetrics ctx
    , peHost = ctxHostProfile ctx
    , peTerminal = isCellHost (ctxHostProfile ctx)
    , peOccluders = occluders
    , peHasOccluders = not (null occluders)
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

-- | Resolve the font for a text-bearing node. Base sans/mono sizes resolve to
-- the pre-read metrics with no per-node IORef traffic; anything else defers
-- to the host font resolver. The second component says whether the resolver
-- returned a native style-driven face (which suppresses weight/style tweaks
-- that the renderer applies itself).
resolveNodeFont :: PaintEnv -> Float -> FontWeight -> FontStyle -> FontVariant -> IO (FontMetrics, Bool)
resolveNodeFont env fontSizeVal fweight fstyle fvar
  | isBaseSans = pure (peFontMetrics env, False)
  | isBaseMono = pure (peMonoMetrics env, False)
  | otherwise = ctxResolveFont (peContext env) fontSizeVal fweight fstyle fvar
  where
    isBaseSans = fontSizeVal <= 0 && fweight == WeightNormal && fstyle == FontStyleNormal && fvar == FontRegular
    isBaseMono = fontSizeVal <= 0 && fweight == WeightNormal && fstyle == FontStyleNormal && fvar == FontMono