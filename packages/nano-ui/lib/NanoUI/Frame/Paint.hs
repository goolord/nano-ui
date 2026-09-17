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
import Data.Bits ((.&.))
import Data.IORef (readIORef)
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Text as T
import Data.Word (Word32)
import NanoUI.Context
  ( Context (..)
  , CustomDrawingEntry (..)
  , DrawingEntry (..)
  , atlasTextureId
  , cachedCustomDrawingOps
  , cachedDrawingOps
  , getScrollOffset
  , getScrollOffset2D
  , lookupCustomDrawing
  , lookupDrawing
  , lookupImageUv
  , nodeTheme
  , scopeTheme
  )
import NanoUI.Draw
  ( DrawArena (..)
  , Layer (..)
  , beginLayer
  , currentLayer
  , emitDrawOps
  , pushImage
  , pushRect
  , pushRoundedStroke
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
import NanoUI.Frame.Node (ScrollNode (..), readScrollNode, resolveFontFor, resolveTextFont, scrollNodeViewport)
import NanoUI.Frame.Paint.Types (PaintEnv (..), buildPaintEnv)
import NanoUI.Frame.Paint.Widgets (paintTextAreaNode, paintTextInputNode, paintWidget)
import NanoUI.Frame.Scroll.Geometry
  ( borderContentClip
  , padContentClip
  , scrollBare
  , scrollBarLayout
  , scrollBarLayouts2D
  , scrollChromeActive
  )
import NanoUI.Frame.Spans (collectNodeTextSpans)
import NanoUI.Id (hashWidgetId)
import NanoUI.Layout.Arena
  ( DirTag (..)
  , NodeIdx
  , NodeType (..)
  , SizingTag (..)
  , arenaCount
  , foldNodesM
  , forChildNodes_
  , getHeightSizing
  , getNodeFontColor
  , getNodeFontSize
  , getNodeScope
  , getNodeType
  , getRect
  , getStyleIdx
  , getText
  , getWidgetId
  , getWidthSizing
  , isFloatingNode
  )
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
  , fadeAlpha
  , themeDisabledFade
  , themeFocusRing
  )
import NanoUI.Types (Color (..), ImageId (..), Rect (..), V2 (..), colorA, colorRGBA, rectFullyInside, rectInflate)
import NanoUI.Widgets.ColorPicker (colorPickerPartRect)
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
  let na = ctxNodeArena ctx
      isOpaque s = colorA (styleBg s) == 255
      occludes theme = \case
        NodeWindow -> isOpaque (overlayWindowStyle theme)
        NodeModal -> isOpaque (overlayModalStyle theme)
        NodePopup -> isOpaque (overlayMenuStyle theme)
        _ -> False
      addPanel acc idx = do
        nt <- getNodeType na idx
        opaque <- if isFloatingNode nt then (`occludes` nt) <$> nodeTheme ctx idx else pure False
        if not opaque
          then pure acc
          else do
            (x, y, w, h) <- getRect na idx
            pure (if w > 6 && h > 6 then rectInflate (-3) (Rect x y w h) : acc else acc)
  foldNodesM na addPanel []

-- | Clip + occluder short-circuit, then lower the node. NOINLINE so the
-- recursive container walk never exposes the dispatch below to the simplifier.
--
-- The clip test widens the node by 'paintOverhang': a clip frame starts from a
-- blank backdrop, so a node whose focus ring reaches into the clip must repaint
-- even when its own rect stays outside.
{-# NOINLINE paintNodeWithEnv #-}
paintNodeWithEnv :: PaintEnv -> NodeIdx -> IO ()
paintNodeWithEnv env idx = do
  (x, y, w, h) <- getRect (peNodeArena env) idx
  (cx, cy, cw, ch) <- readIORef (daCurrentClip (peDrawArena env))
  let !l = max (x - paintOverhang) cx
      !t = max (y - paintOverhang) cy
      !r = min (x + w + paintOverhang) (cx + cw)
      !b = min (y + h + paintOverhang) (cy + ch)
  unless (w <= 0 || h <= 0 || r <= l || b <= t) $
    unless (peHasOccluders env && any (rectFullyInside (Rect l t (r - l) (b - t))) (peOccluders env)) $ do
      nt <- getNodeType (peNodeArena env) idx
      scope <- getNodeScope (peNodeArena env) idx
      if scope == peScope env
        then lowerNodeVisible env idx nt (Rect x y w h)
        else do
          theme <- scopeTheme (peContext env) scope
          lowerNodeVisible env {peTheme = theme, peScope = scope} idx nt (Rect x y w h)

-- | How far a node may paint outside its rect: the focus ring sits 2px out
-- with a 1.5px stroke.
paintOverhang :: Float
paintOverhang = 4

-- | Explicit per-node-type dispatch. Kept NOINLINE and thin so the recursive
-- loop never sees the branch bodies.
{-# NOINLINE lowerNodeVisible #-}
lowerNodeVisible :: PaintEnv -> NodeIdx -> NodeType -> Rect -> IO ()
lowerNodeVisible env idx nt rect = do
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
  unless (hashWidgetId (peFocusRing env) == 0) $
    paintFocusRing env idx nt rect

-- | Accent ring around the widget holding keyboard focus. Text fields and
-- selects already swap in an accent border while focused, so they get none.
-- Tree rows fill their scroller edge to edge, so their ring sits just inside
-- the row; colour picker parts ring the square or bar they draw.
{-# NOINLINE paintFocusRing #-}
paintFocusRing :: PaintEnv -> NodeIdx -> NodeType -> Rect -> IO ()
paintFocusRing env idx nt rect = do
  wid <- getWidgetId (peNodeArena env) idx
  when (wid == peFocusRing env && nt /= NodeTextInput && nt /= NodeTextArea && nt /= NodeSelect) $ do
    target <-
      if nt == NodeColorPicker
        then colorPickerPartRect (peNodeArena env) idx rect
        else pure rect
    let (ring, radius)
          | nt == NodeTree = (rectInflate (-1) target, 0)
          | otherwise = (rectInflate 2 target, 4)
    pushRoundedStroke (peDrawArena env) ring radius 1.5 (themeFocusRing (peTheme env))

paintContainerNode :: PaintEnv -> NodeIdx -> Rect -> IO ()
paintContainerNode env idx rect = do
  walkChildrenWithOccluders env idx
  let ctx = peContext env
  wid <- getWidgetId (peNodeArena env) idx
  mBuild <- lookupCustomDrawing ctx wid
  forM_ mBuild $ \(CustomDrawingEntry _ build) -> do
    let fm = peFontMetrics env
        da = peDrawArena env
    cdc <- mkCustomDrawContext ctx fm wid
    withClip da rect (emitDrawOps da fm (resolveTextFont ctx) (build cdc rect))

paintPanelNode :: PaintEnv -> NodeIdx -> Rect -> IO ()
paintPanelNode env idx rect@(Rect x y w h) = do
  let da = peDrawArena env
      style = themePanel (peTheme env)
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
  sn <- readScrollNode arena idx
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
  unless (scrollBare (snConfig sn)) $ do
    inFloating <- maybe False isFloatingNode <$> floatingAncestor ctx idx
    (wTag, _) <- getWidthSizing arena idx
    (hTag, _) <- getHeightSizing arena idx
    if wTag == SizingGrow && hTag == SizingGrow
      then pushRect da rect (if inFloating then styleBg (themeFloatingWindow tm) else themeWindow tm)
      else do
        let well = (if inFloating then themeFloatingWindow tm else themeInput tm) {styleCornerRadius = 0}
        fillStyledRect da well rect
        strokeStyledRect da well x y w h
  withClip da (scrollNodeViewport sn x y w h) $ walkChildrenWithOccluders env idx
  paintScrollChrome env idx sn rect

-- | Scrollbars of a scroll container whose chrome is active, drawn one layer
-- above the content so they stay on top of it.
paintScrollChrome :: PaintEnv -> NodeIdx -> ScrollNode -> Rect -> IO ()
paintScrollChrome env idx (ScrollNode slot cfg native2D dir pad contentMain contentW) (Rect x y w h) = do
  let ctx = peContext env
      da = peDrawArena env
      theme = peTheme env
      Rect _ _ innerW innerH = padContentClip x y w h pad
  wid <- getWidgetId (peNodeArena env) idx
  bars <-
    if native2D
      then
        if scrollChromeActive cfg DirColumn contentMain innerH || scrollChromeActive cfg DirRow contentW innerW
          then do
            V2 offX offY <- getScrollOffset2D ctx wid
            let (mV, mH) = scrollBarLayouts2D slot cfg x y w h pad contentW contentMain offX offY
            pure (catMaybes [mV, mH])
          else pure []
      else do
        let innerMain = case dir of
              DirColumn -> innerH
              DirRow -> innerW
        if scrollChromeActive cfg dir contentMain innerMain
          then do
            off <- getScrollOffset ctx wid
            pure (catMaybes [scrollBarLayout slot dir x y w h pad contentMain off])
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
    (fm, isNative, _) <- resolveFontFor (peContext env) NodeText fontSize si
    let deco = textNodeTextDecoration si
        weight = if isNative then WeightNormal else textNodeFontWeight si
        style = if isNative then FontStyleNormal else textNodeFontStyle si
        plain = weight == WeightNormal && style == FontStyleNormal && deco == DecorationNone
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
    Just (u0, v0, u1, v1) -> do
      -- An image may carry a tint in its font colour (an SVG icon). A
      -- disabled image fades the way disabled widget colours do.
      base <- fromMaybe (colorRGBA 255 255 255 255) <$> getNodeFontColor (peNodeArena env) idx
      let tint
            | peScope env .&. 1 /= 0 = fadeAlpha base (round (fromIntegral (colorA base) * (1 - themeDisabledFade (peTheme env))))
            | otherwise = base
      pushImage da rect atlasTextureId u0 v0 u1 v1 tint
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
    Just (CustomDrawingEntry content customBuild) -> do
      cdc <- mkCustomDrawContext ctx fm wid
      ops <- cachedCustomDrawingOps ctx wid content rect cdc customBuild
      withClip da rect (emitDrawOps da fm (resolveTextFont ctx) ops)
    Nothing -> do
      mBuild <- lookupDrawing ctx wid
      forM_ mBuild $ \(DrawingEntry content build) -> do
        ops <- cachedDrawingOps ctx wid content rect build
        withClip da rect (emitDrawOps da fm (resolveTextFont ctx) ops)

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
