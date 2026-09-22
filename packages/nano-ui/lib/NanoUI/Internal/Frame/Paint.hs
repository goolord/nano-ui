-- Paint traversal for NanoUI. This module owns the node walk and the
-- structural painters; widget chrome painting lives in sibling
-- NanoUI.Internal.Frame.Paint.Widgets.
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


-- | Walk solved nodes and emit geometry, respecting clips, layers, and paint scopes.
module NanoUI.Internal.Frame.Paint
  ( lowerShapes
  , walkChildren
  ) where

import Control.Monad (forM_, unless, when)
import Data.Bits ((.&.))
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Primitive.PrimArray
  ( PrimArray
  , emptyPrimArray
  , indexPrimArray
  , newPrimArray
  , shrinkMutablePrimArray
  , sizeofPrimArray
  , unsafeFreezePrimArray
  , writePrimArray
  )
import Data.Primitive.SmallArray (SmallArray)
import qualified Data.Text as T
import Data.Word (Word32)
import NanoUI.Internal.Context
  ( Context (..)
  , CustomDrawingEntry (..)
  , DrawingEntry (..)
  , SpanCacheEntry (..)
  , SpanLines (..)
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
import NanoUI.Internal.Draw
  ( DrawOp
  , Layer (..)
  , beginLayer
  , currentClip
  , currentLayer
  , emitDrawOps
  , pushImage
  , pushRect
  , pushRoundedStroke
  , pushPreparedTextStyled
  , withClip
  )
import NanoUI.Internal.Font (ScrollBarSlot (..))
import NanoUI.Internal.Frame.Chrome
  ( floatingAncestor
  , imageIdFromText
  , overlayMenuStyle
  , overlayWindowStyle
  , paintScrollBarLayout
  , paintStyledRect
  )
import NanoUI.Internal.Frame.Node (ScrollNode (..), nodeFontNative, readScrollNode, resolveTextFont, scrollNodeViewport)
import NanoUI.Internal.Frame.Paint.Types (PaintEnv (..), buildPaintEnv)
import NanoUI.Internal.Frame.Paint.Widgets (paintTextAreaNode, paintTextInputNode, paintWidget)
import NanoUI.Internal.Frame.Scroll.Geometry
  ( borderContentClip
  , padContentClip
  , scrollBare
  , scrollBarLayout
  , scrollBarLayouts2D
  , scrollChromeActive
  )
import NanoUI.Internal.Frame.Spans (textNodeSpanEntry)
import NanoUI.Internal.Id (hashWidgetId)
import NanoUI.Internal.Layout.Arena
  ( DirTag (..)
  , NodeIdx
  , NodeType (..)
  , SizingTag (..)
  , arenaCount
  , floatingNodeCount
  , foldFloatingNodesM
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
  )
import NanoUI.Internal.Style
  ( FontStyle (..)
  , FontWeight (..)
  , Style (..)
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
import NanoUI.Internal.Types (Color (..), ImageId (..), Rect (..), V2 (..), colorA, colorRGBA, rectInflate)
import NanoUI.Internal.Widgets.ColorPicker (colorPickerPartRect)
import NanoUI.Internal.Widgets.Custom (mkCustomDrawContext)
import NanoUI.Internal.WidgetText
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
-- hide whatever lies fully behind them, as @x0, y0, x1, y1@ runs. Frames
-- without floating nodes skip the arena walk.
collectFloatingOccluders :: Context -> IO (PrimArray Float)
collectFloatingOccluders ctx = do
  let na = ctxNodeArena ctx
  floating <- floatingNodeCount na
  if floating <= 0
    then pure emptyPrimArray
    else do
      buf <- newPrimArray (floating * 4)
      n <- foldFloatingNodesM na (addOccluder na buf) 0
      shrinkMutablePrimArray buf (n * 4)
      unsafeFreezePrimArray buf
  where
    isOpaque s = colorA (styleBg s) == 255
    occludes theme = \case
      NodeWindow -> isOpaque (overlayWindowStyle theme)
      nt | nt == NodeModal || nt == NodePopup -> isOpaque (overlayMenuStyle theme)
      _ -> False
    addOccluder na buf !n idx = do
      nt <- getNodeType na idx
      opaque <- (`occludes` nt) <$> nodeTheme ctx idx
      if not opaque
        then pure n
        else do
          (x, y, w, h) <- getRect na idx
          if not (w > 6 && h > 6)
            then pure n
            else do
              let !o = n * 4
                  Rect ox oy ow oh = rectInflate (-3) (Rect x y w h)
              writePrimArray buf o ox
              writePrimArray buf (o + 1) oy
              writePrimArray buf (o + 2) (ox + ow)
              writePrimArray buf (o + 3) (oy + oh)
              pure (n + 1)

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
  Rect cx cy cw ch <- currentClip (peDrawArena env)
  let !l = max (x - paintOverhang) cx
      !t = max (y - paintOverhang) cy
      !r = min (x + w + paintOverhang) (cx + cw)
      !b = min (y + h + paintOverhang) (cy + ch)
  unless (w <= 0 || h <= 0 || r <= l || b <= t) $
    unless (occluded (peOccluders env) l t r b || missesPieces (pePieces env) l t r b) $ do
      nt <- getNodeType (peNodeArena env) idx
      scope <- getNodeScope (peNodeArena env) idx
      if scope == peScope env
        then lowerNodeVisible env idx nt (Rect x y w h)
        else do
          theme <- scopeTheme (peContext env) scope
          lowerNodeVisible env {peTheme = theme, peScope = scope} idx nt (Rect x y w h)

-- | Whether an opaque floating panel fully covers the clipped node rect
-- @l, t, r, b@, which the caller has already checked is non-empty.
{-# INLINE occluded #-}
occluded :: PrimArray Float -> Float -> Float -> Float -> Float -> Bool
occluded occ !l !t !r !b = go 0
  where
    !end = sizeofPrimArray occ
    go !o
      | o >= end = False
      | l >= indexPrimArray occ o
          && t >= indexPrimArray occ (o + 1)
          && r <= indexPrimArray occ (o + 2)
          && b <= indexPrimArray occ (o + 3) =
          True
      | otherwise = go (o + 4)

-- | Whether the damage pieces leave out the clipped node rect @l, t, r, b@:
-- there are some, and it meets none of them.
{-# INLINE missesPieces #-}
missesPieces :: PrimArray Float -> Float -> Float -> Float -> Float -> Bool
missesPieces ps !l !t !r !b = end > 0 && go 0
  where
    !end = sizeofPrimArray ps
    go !o
      | o >= end = True
      | l < indexPrimArray ps (o + 2)
          && t < indexPrimArray ps (o + 3)
          && r > indexPrimArray ps o
          && b > indexPrimArray ps (o + 1) =
          False
      | otherwise = go (o + 4)

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
  forM_ mBuild $ \(CustomDrawingEntry _ build _ _ _) -> do
    cdc <- mkCustomDrawContext ctx (peFontMetrics env) wid
    emitDrawingOps env rect (build cdc rect)

-- | A drawing's ops clipped to its rect, in the env's default font.
emitDrawingOps :: PaintEnv -> Rect -> SmallArray DrawOp -> IO ()
emitDrawingOps env rect ops =
  let da = peDrawArena env
   in withClip da rect (emitDrawOps da (peFontMetrics env) (resolveTextFont (peContext env)) ops)

paintPanelNode :: PaintEnv -> NodeIdx -> Rect -> IO ()
paintPanelNode env idx rect = do
  let da = peDrawArena env
      style = themePanel (peTheme env)
  paintStyledRect da style rect
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
    inFloating <- isJust <$> floatingAncestor ctx idx
    (wTag, _) <- getWidthSizing arena idx
    (hTag, _) <- getHeightSizing arena idx
    if wTag == SizingGrow && hTag == SizingGrow
      then pushRect da rect (if inFloating then styleBg (themeFloatingWindow tm) else themeWindow tm)
      else do
        let well = (if inFloating then themeFloatingWindow tm else themeInput tm) {styleCornerRadius = 0}
        paintStyledRect da well rect
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

-- | A text node's lines, drawn with the metrics its span cache entry prepared
-- for each line when it was laid out.
{-# NOINLINE paintTextNode #-}
paintTextNode :: PaintEnv -> NodeIdx -> Rect -> IO ()
paintTextNode env idx rect@(Rect x y w h) = do
  let arena = peNodeArena env
      da = peDrawArena env
  si <- getStyleIdx arena idx
  forM_ (tableStripeColor (peTheme env) si) (pushRect da rect)
  raw <- getText arena idx
  unless (T.null raw) $ do
    e <- textNodeSpanEntry (peContext env) idx x y w h
    fontSize <- getNodeFontSize arena idx
    native <- nodeFontNative (peContext env) fontSize si
    let !deco = textNodeTextDecoration si
        !weight = if native then WeightNormal else textNodeFontWeight si
        !style = if native then FontStyleNormal else textNodeFontStyle si
        draw (Rect tx ty _ _, line, spanFg, _) prepared =
          unless (T.null line) $
            pushPreparedTextStyled da prepared weight style deco tx ty line spanFg
        -- 'placeSpanLines' makes one span per line, in order.
        wrapped (s : ss) ((_, prepared) : lns) = draw s prepared >> wrapped ss lns
        wrapped _ _ = pure ()
    case sceLines e of
      SpanWrapped lns -> wrapped (sceSpans e) lns
      SpanSingle _ prepared -> mapM_ (`draw` prepared) (sceSpans e)

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
  wid <- getWidgetId (peNodeArena env) idx
  mCustomBuild <- lookupCustomDrawing ctx wid
  case mCustomBuild of
    Just (CustomDrawingEntry content customBuild _ _ _) -> do
      ops <- cachedCustomDrawingOps ctx wid content rect (mkCustomDrawContext ctx (peFontMetrics env) wid) customBuild
      emitDrawingOps env rect ops
    Nothing -> do
      mBuild <- lookupDrawing ctx wid
      forM_ mBuild $ \(DrawingEntry content build) -> do
        ops <- cachedDrawingOps ctx wid content rect build
        emitDrawingOps env rect ops

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
walkChildren ctx idx = buildPaintEnv ctx emptyPrimArray >>= (`walkChildrenWithOccluders` idx)
