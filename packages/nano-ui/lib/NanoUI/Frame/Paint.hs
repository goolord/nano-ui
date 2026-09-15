-- Paint traversal for NanoUI. This module owns the node walk and the
-- structural painters; widget chrome painting lives in sibling
-- NanoUI.Frame.Paint.Widgets.
--
-- The module is shaped for GHC's optimizer: the recursive walker
--
--   paintNodeWithEnv -> lowerNodeVisible (explicit dispatch)
--         -> per-node painters (containers recurse via walkChildrenWithOccluders)
--
-- sits on top of {-# NOINLINE #-} seams, and the heavyweight painters (widget
-- chrome, text, scroll containers, drawings) stay out of line, so no single
-- binding carries the whole painting body inside the recursive loop. That
-- stops the simplifier / SpecConstr from seeing one monolithic binding in the
-- loop, which is what blew up compilation under -fspecialise-aggressively +
-- LLVM; hence the guard flags below.
{-# OPTIONS_GHC -fasm -fno-specialise-aggressively #-}

{-# LANGUAGE DataKinds #-}

module NanoUI.Frame.Paint
  ( lowerShapes
  , walkChildren
  ) where

import Control.Monad (forM_, unless, when)
import Data.IORef (readIORef)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Data.Word (Word32)
import NanoUI.Context
  ( Context (..)
  , DrawingEntry (..)
  , atlasTextureId
  , cachedCustomDrawingOps
  , cachedDrawingOps
  , getScrollOffset
  , getScrollOffset2D
  , lookupCustomDrawing
  , lookupDrawing
  , lookupImageUv
  )
import NanoUI.Draw
  ( DrawArena (..)
  , Layer (..)
  , beginLayer
  , currentLayer
  , emitDrawOps
  , pushImage
  , pushRect
  , pushText
  , pushTextStyled
  , withClip
  )
import NanoUI.Font (ScrollBarSlot (..))
import NanoUI.Frame.Chrome
  ( fillStyledRect
  , floatingAncestor
  , imageIdFromText
  , overlayMenuStyle
  , overlayModalStyle
  , overlayWindowStyle
  , paintScrollBarLayout
  , strokeStyledRect
  )
import NanoUI.Frame.Node (resolveFontFor, scrollViewportAt)
import NanoUI.Frame.Paint.Types (PaintEnv (..), buildPaintEnv)
import NanoUI.Frame.Paint.Widgets (paintTextAreaNode, paintTextInputNode, paintWidget)
import NanoUI.Frame.Scroll.Geometry
  ( borderContentClip
  , decodeScrollConfig
  , isScrollStyle2D
  , padContentClip
  , scrollBare
  , scrollBarLayout
  , scrollBarLayouts2D
  , scrollChromeActive
  )
import NanoUI.Frame.Spans (collectNodeTextSpans)
import NanoUI.Layout.Arena
  ( DirTag (..)
  , NodeIdx
  , NodeType (..)
  , SizingTag (..)
  , arenaCount
  , foldNodesM
  , forChildNodes_
  , getDirection
  , getHeightSizing
  , getNodeFontSize
  , getNodeType
  , getNodeValue
  , getPadding
  , getRect
  , getScrollContentW
  , getStyleIdx
  , getText
  , getWidgetId
  , getWidthSizing
  , isFloatingNode
  )
import NanoUI.Layout.Solve (scrollBarSlotOf)
import NanoUI.Style
  ( FontStyle (..)
  , FontWeight (..)
  , Style (..)
  , TextDecoration (..)
  , Theme (..)
  , scrollBarThumbColor
  , scrollBarTrackColor
  , themeAccent
  , themeFloatingWindow
  , themeInput
  , themePanel
  , themeSeparator
  , themeWindow
  , unpackPanelStyle
  )
import NanoUI.Types (Color (..), ImageId (..), Rect (..), V2 (..), colorA, colorRGBA, rectFullyInside, rectInflate)
import NanoUI.Widgets.Custom (mkCustomDrawContext)
import NanoUI.WidgetText
  ( tableStripeColor
  , textNodeFontStyle
  , textNodeFontWeight
  , textNodeTextDecoration
  )

lowerShapes :: Context -> IO ()
lowerShapes ctx = do
  count <- arenaCount (ctxNodeArena ctx)
  when (count > 0) $ do
    occluders <- collectFloatingOccluders ctx
    buildPaintEnv ctx occluders >>= (`paintNodeWithEnv` 0)

-- | Rects of opaque floating panels, inset past their rounded border, that
-- hide whatever lies fully behind them.
collectFloatingOccluders :: Context -> IO [Rect]
collectFloatingOccluders ctx = do
  theme <- readIORef (ctxTheme ctx)
  let na = ctxNodeArena ctx
      isOpaque s = colorA (styleBg s) == 255
      winOpaque = isOpaque (overlayWindowStyle theme)
      modalOpaque = isOpaque (overlayModalStyle theme)
      menuOpaque = isOpaque (overlayMenuStyle theme)
      occludes = \case
        NodeWindow -> winOpaque
        NodeModal -> modalOpaque
        NodePopup -> menuOpaque
        _ -> False
      addPanel acc idx = do
        nt <- getNodeType na idx
        if not (occludes nt)
          then pure acc
          else do
            (x, y, w, h) <- getRect na idx
            pure (if w > 6 && h > 6 then rectInflate (-3) (Rect x y w h) : acc else acc)
  foldNodesM na addPanel []

-- | Clip + occluder short-circuit, then lower the node. NOINLINE so the
-- recursive container walk never exposes the dispatch below to the simplifier.
{-# NOINLINE paintNodeWithEnv #-}
paintNodeWithEnv :: PaintEnv -> NodeIdx -> IO ()
paintNodeWithEnv env idx = do
  (x, y, w, h) <- getRect (peNodeArena env) idx
  (cx, cy, cw, ch) <- readIORef (daCurrentClip (peDrawArena env))
  let !l = max x cx
      !t = max y cy
      !r = min (x + w) (cx + cw)
      !b = min (y + h) (cy + ch)
  unless (r <= l || b <= t) $
    unless (peHasOccluders env && any (rectFullyInside (Rect l t (r - l) (b - t))) (peOccluders env)) $ do
      nt <- getNodeType (peNodeArena env) idx
      lowerNodeVisible env idx nt (Rect x y w h)

-- | Explicit per-node-type dispatch. Kept NOINLINE and thin so the recursive
-- loop never sees the branch bodies.
{-# NOINLINE lowerNodeVisible #-}
lowerNodeVisible :: PaintEnv -> NodeIdx -> NodeType -> Rect -> IO ()
lowerNodeVisible env idx nt rect =
  case nt of
    NodeContainer -> paintContainerNode env idx rect
    NodePanel -> paintPanelNode env idx rect
    NodeScrollContainer -> paintScrollContainerNode env idx rect
    NodeText -> paintTextNode env idx rect
    NodeSeparator -> paintSeparatorNode env rect
    NodeTextInput -> paintTextInputNode env idx rect
    NodeTextArea -> paintTextAreaNode env idx rect
    NodeSpacer -> pure ()
    NodeModal -> pure ()
    NodeWindow -> pure ()
    NodePopup -> pure ()
    NodeBox -> paintBoxNode env idx rect
    NodeImage -> paintImageNode env idx rect
    NodeDrawing -> paintDrawingNode env idx rect
    NodeWidget -> pure ()
    _ -> paintWidget env idx nt rect

paintContainerNode :: PaintEnv -> NodeIdx -> Rect -> IO ()
paintContainerNode env idx rect = do
  walkChildrenWithOccluders env idx
  let ctx = peContext env
  wid <- getWidgetId (peNodeArena env) idx
  mBuild <- lookupCustomDrawing ctx wid
  forM_ mBuild $ \build -> do
    let fm = peFontMetrics env
        da = peDrawArena env
    cdc <- mkCustomDrawContext ctx fm wid
    withClip da rect (emitDrawOps da fm (build cdc rect))

paintPanelNode :: PaintEnv -> NodeIdx -> Rect -> IO ()
paintPanelNode env idx rect@(Rect x y w h) = do
  let da = peDrawArena env
      panel = themePanel (peTheme env)
  si <- getStyleIdx (peNodeArena env) idx
  let style = if si /= 0 then unpackPanelStyle panel si else panel
  fillStyledRect da style rect
  strokeStyledRect da style x y w h
  withClip da (borderContentClip style rect) $ walkChildrenWithOccluders env idx

{-# NOINLINE paintScrollContainerNode #-}
paintScrollContainerNode :: PaintEnv -> NodeIdx -> Rect -> IO ()
paintScrollContainerNode env idx rect@(Rect x y w h) = do
  let ctx = peContext env
      arena = peNodeArena env
      da = peDrawArena env
      tm = peTheme env
  si <- getStyleIdx arena idx
  -- A bare scroller paints nothing at all: it only lends its clip and
  -- offset, so whatever sits behind it (window, panel) keeps showing
  -- through. Grow×grow scrollers (page-level) keep no well so they blend
  -- into the window backdrop. That backdrop only exists while the runner
  -- clears it on DamageFull frames; on clip frames (scrolling, resize)
  -- the strip vacated by scrolled content has no covering command and
  -- the retained texture would show stale pixels, a ghost of a previous
  -- scroll position. Paint the full rect with the window color instead:
  -- invisible on a cleared backdrop, and clip replay then always
  -- repaints the whole viewport.
  unless (scrollBare (decodeScrollConfig si)) $ do
    inFloating <- maybe False isFloatingNode <$> floatingAncestor ctx idx
    (wTag, _) <- getWidthSizing arena idx
    (hTag, _) <- getHeightSizing arena idx
    if wTag == SizingGrow && hTag == SizingGrow
      then pushRect da rect (if inFloating then styleBg (themeFloatingWindow tm) else themeWindow tm)
      else do
        let well = (if inFloating then themeFloatingWindow tm else themeInput tm) {styleCornerRadius = 0}
        fillStyledRect da well rect
        strokeStyledRect da well x y w h
  inner <- scrollViewportAt ctx idx x y w h
  withClip da inner $ walkChildrenWithOccluders env idx
  paintScrollChrome env idx rect

-- | Scrollbars of a scroll container whose chrome is active, drawn one layer
-- above the content so they stay on top of it.
paintScrollChrome :: PaintEnv -> NodeIdx -> Rect -> IO ()
paintScrollChrome env idx (Rect x y w h) = do
  let ctx = peContext env
      na = peNodeArena env
      da = peDrawArena env
      fm = peFontMetrics env
      theme = peTheme env
  si <- getStyleIdx na idx
  pad <- getPadding na idx
  slot <- scrollBarSlotOf na idx
  wid <- getWidgetId na idx
  contentMain <- getNodeValue na idx
  let cfg = decodeScrollConfig si
      Rect _ _ innerW innerH = padContentClip fm x y w h pad
  bars <-
    if isScrollStyle2D si
      then do
        contentW <- getScrollContentW na idx
        if scrollChromeActive cfg DirColumn contentMain innerH || scrollChromeActive cfg DirRow contentW innerW
          then do
            V2 offX offY <- getScrollOffset2D ctx wid
            let (mV, mH) = scrollBarLayouts2D fm slot cfg x y w h pad contentW contentMain offX offY
            pure (catMaybes [mV, mH])
          else pure []
      else do
        dir <- getDirection na idx
        let innerMain = case dir of
              DirColumn -> innerH
              DirRow -> innerW
        if scrollChromeActive cfg dir contentMain innerMain
          then do
            off <- getScrollOffset ctx wid
            pure (catMaybes [scrollBarLayout fm slot dir x y w h pad contentMain off])
          else pure []
  unless (null bars) $ do
    layer <- currentLayer da
    beginLayer da (if layer == LayerOverlay then LayerChrome else LayerContent)
    let base = case slot of
          ScrollBarWindow -> themeFloatingWindow theme
          _ -> themeInput theme
    mapM_ (paintScrollBarLayout da (scrollBarTrackColor base theme) (scrollBarThumbColor base theme)) bars
    beginLayer da layer

{-# NOINLINE paintTextNode #-}
paintTextNode :: PaintEnv -> NodeIdx -> Rect -> IO ()
paintTextNode env idx rect = do
  let arena = peNodeArena env
      da = peDrawArena env
  si <- getStyleIdx arena idx
  forM_ (tableStripeColor (peTheme env) si) (pushRect da rect)
  raw <- getText arena idx
  unless (T.null raw) $ do
    spans <- collectNodeTextSpans (peContext env) idx
    fontSize <- getNodeFontSize arena idx
    (fm, isNative, _) <- resolveFontFor (peContext env) fontSize si
    let deco = textNodeTextDecoration si
        weight = if isNative then WeightNormal else textNodeFontWeight si
        style = if isNative then FontStyleNormal else textNodeFontStyle si
        plain = not isNative && deco == DecorationNone
    forM_ spans $ \(Rect tx ty _ _, line, spanFg, _) ->
      unless (T.null line) $
        if plain
          then pushText da fm tx ty line spanFg
          else pushTextStyled da fm weight style deco tx ty line spanFg

paintSeparatorNode :: PaintEnv -> Rect -> IO ()
paintSeparatorNode env (Rect x y w h) =
  pushRect (peDrawArena env) line (themeSeparator (peTheme env))
  where
    line
      | w >= h = Rect x (y + (h - 1) / 2) w 1
      | otherwise = Rect (x + (w - 1) / 2) y 1 h

paintBoxNode :: PaintEnv -> NodeIdx -> Rect -> IO ()
paintBoxNode env idx rect = do
  si <- getStyleIdx (peNodeArena env) idx
  -- styleIdx holds RGBA Word32 bits; see `box` in NanoUI.Widgets.
  pushRect (peDrawArena env) rect (Color (fromIntegral si :: Word32))

paintImageNode :: PaintEnv -> NodeIdx -> Rect -> IO ()
paintImageNode env idx rect = do
  let da = peDrawArena env
  tex <- imageIdFromText <$> getText (peNodeArena env) idx
  mUv <- lookupImageUv (peContext env) (ImageId tex)
  case mUv of
    Just (u0, v0, u1, v1) ->
      pushImage da rect atlasTextureId u0 v0 u1 v1 (colorRGBA 255 255 255 255)
    _ -> pushRect da rect (themeAccent (peTheme env))

{-# NOINLINE paintDrawingNode #-}
paintDrawingNode :: PaintEnv -> NodeIdx -> Rect -> IO ()
paintDrawingNode env idx rect = do
  let ctx = peContext env
      fm = peFontMetrics env
      da = peDrawArena env
  wid <- getWidgetId (peNodeArena env) idx
  mCustomBuild <- lookupCustomDrawing ctx wid
  case mCustomBuild of
    Just customBuild -> do
      cdc <- mkCustomDrawContext ctx fm wid
      ops <- cachedCustomDrawingOps ctx wid rect cdc customBuild
      withClip da rect (emitDrawOps da fm ops)
    Nothing -> do
      mBuild <- lookupDrawing ctx wid
      forM_ mBuild $ \(DrawingEntry content build) -> do
        ops <- cachedDrawingOps ctx wid content rect build
        withClip da rect (emitDrawOps da fm ops)

-- | Lower the children of @idx@ with the current paint env. NOINLINE keeps
-- this recursive call out of the simplifier's loop analysis, so the whole
-- walker stays a call to opaque seams rather than one inlined monster.
{-# NOINLINE walkChildrenWithOccluders #-}
walkChildrenWithOccluders :: PaintEnv -> NodeIdx -> IO ()
walkChildrenWithOccluders env idx =
  forChildNodes_ (peNodeArena env) idx (paintNodeWithEnv env)

-- | Children walk for callers painting a subtree inside their own clip
-- (floating overlays); builds a fresh env without occluders.
{-# NOINLINE walkChildren #-}
walkChildren :: Context -> NodeIdx -> IO ()
walkChildren ctx idx = buildPaintEnv ctx [] >>= (`walkChildrenWithOccluders` idx)
