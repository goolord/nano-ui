-- The recursive walk (paintNodeWithEnv -> lowerNodeVisible -> per-node
-- painters -> walkChildrenWithOccluders) goes through NOINLINE seams, and
-- heavy painters stay out of line, so no single binding holds the whole
-- paint body inside the loop. That blew up compilation under
-- -fspecialise-aggressively with LLVM; the flags below also guard against it.
{-# OPTIONS_GHC -fasm -fno-specialise-aggressively #-}

-- | Walk solved nodes and emit geometry, respecting clips, layers, and paint
-- scopes. Widget chrome is painted in "NanoUI.Internal.Frame.Paint.Widgets".
module NanoUI.Internal.Frame.Paint
  ( lowerShapes
  , walkChildren
  ) where

import Control.Monad (forM_, unless, when)
import Data.Bits ((.&.))
import Data.Maybe (fromMaybe, isJust)
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
import NanoUI.Internal.Draw
import NanoUI.Internal.Font (ScrollBarSlot (..))
import NanoUI.Internal.Frame.Chrome
import NanoUI.Internal.Frame.Node (nodeFontNative, readScrollNode, resolveTextFont)
import NanoUI.Internal.Frame.Paint.Widgets (PaintEnv (..), buildPaintEnv, paintTextAreaNode, paintTextInputNode, paintWidget)
import NanoUI.Internal.Frame.Scroll.Geometry (ScrollNode (..), borderContentClip, scrollBare, scrollNodeBars, scrollNodeViewport)
import NanoUI.Internal.Frame.Spans (textNodeSpanEntry)
import NanoUI.Internal.Id (hashWidgetId)
import NanoUI.Internal.Image (ImageDraw (..), fadeBy, imageDrawOp, lookDraw)
import NanoUI.Internal.Layout.Arena
import NanoUI.Internal.Style hiding (fontSize)
import NanoUI.Internal.Types (Color (..), ImageId (..), Rect (..), V2 (..), colorA, colorRGBA, rectInflate)
import NanoUI.Internal.Widgets.ColorPicker (colorPickerPartRect)
import NanoUI.Internal.Widgets.Custom (mkCustomDrawContext)
import NanoUI.Internal.WidgetText

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
collectFloatingOccluders ctx@Context {ctxNodeArena = na} = do
  floating <- floatingNodeCount na
  if floating <= 0
    then pure emptyPrimArray
    else do
      buf <- newPrimArray (floating * 4)
      n <- foldClassNodesM na FloatingNodes (addOccluder buf) 0
      shrinkMutablePrimArray buf (n * 4)
      unsafeFreezePrimArray buf
  where
    isOpaque s = colorA (styleBg s) == 255
    occludes theme = \case
      NodeWindow -> isOpaque (overlayWindowStyle theme)
      nt | nt == NodeModal || nt == NodePopup -> isOpaque (overlayMenuStyle theme)
      _ -> False
    addOccluder buf !n idx = do
      nt <- getNodeType na idx
      opaque <- (`occludes` nt) <$> nodeTheme ctx idx
      if not opaque
        then pure n
        else do
          Rect x y w h <- getNodeRect na idx
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
  Rect x y w h <- getNodeRect (peNodeArena env) idx
  Rect cx cy cw ch <- currentClip (peDrawArena env)
  let !l = max (x - paintOverhang) cx
      !t = max (y - paintOverhang) cy
      !r = min (x + w + paintOverhang) (cx + cw)
      !b = min (y + h + paintOverhang) (cy + ch)
      -- An opaque floating panel covers the clipped rect, or there are damage
      -- pieces and it meets none of them.
      occluded = anyRun (peOccluders env) $ \x0 y0 x1 y1 -> l >= x0 && t >= y0 && r <= x1 && b <= y1
      missesPieces =
        sizeofPrimArray (pePieces env) > 0
          && not (anyRun (pePieces env) $ \x0 y0 x1 y1 -> l < x1 && t < y1 && r > x0 && b > y0)
  if w <= 0 || h <= 0 || r <= l || b <= t || occluded || missesPieces
    then paintPinnedBelow env idx
    else do
      nt <- getNodeType (peNodeArena env) idx
      scope <- getNodeScope (peNodeArena env) idx
      if scope == peScope env
        then lowerNodeVisible env idx nt (Rect x y w h)
        else do
          theme <- scopeTheme (peContext env) scope
          lowerNodeVisible env {peTheme = theme, peScope = scope} idx nt (Rect x y w h)

-- | Paint a skipped plain container's children when a pinned node is below
-- it. Plain containers draw nothing and do not clip, so a pinned node can
-- show outside one, even one with no size. Other containers clip their
-- children.
{-# NOINLINE paintPinnedBelow #-}
paintPinnedBelow :: PaintEnv -> NodeIdx -> IO ()
paintPinnedBelow env idx = do
  pinnedBelow <- hasPinnedBelow (peNodeArena env) idx
  nt <- getNodeType (peNodeArena env) idx
  when (pinnedBelow && nt == NodeContainer) $ walkChildrenWithOccluders env idx

-- | Whether @p@ holds for any of the @x0, y0, x1, y1@ runs of @rects@.
{-# INLINE anyRun #-}
anyRun :: PrimArray Float -> (Float -> Float -> Float -> Float -> Bool) -> Bool
anyRun rects p = go 0
  where
    at = indexPrimArray rects
    go !o = o < sizeofPrimArray rects && (p (at o) (at (o + 1)) (at (o + 2)) (at (o + 3)) || go (o + 4))

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
    NodeTextInput -> paintTextInputNode env idx rect >> paintWidgetChildren env idx rect
    NodeTextArea -> paintTextAreaNode env idx rect
    NodeSpacer -> pure ()
    NodeModal -> pure ()
    NodeWindow -> pure ()
    NodePopup -> pure ()
    NodeBox -> paintBoxNode env idx rect
    NodeImage -> paintImageNode env idx rect
    NodeDrawing -> paintDrawingNode env idx rect
    _ -> paintWidget env idx nt rect >> paintWidgetChildren env idx rect
  unless (hashWidgetId (peFocusRing env) == 0) $
    paintFocusRing env idx nt rect

-- | A widget's children, its adornments or content, clipped to it. Most
-- widgets have none, so only that check is inlined.
{-# INLINE paintWidgetChildren #-}
paintWidgetChildren :: PaintEnv -> NodeIdx -> Rect -> IO ()
paintWidgetChildren env idx rect = do
  kids <- getFirstChild (peNodeArena env) idx
  unless (kids < 0) $ paintClippedChildren env idx rect

{-# NOINLINE paintClippedChildren #-}
paintClippedChildren :: PaintEnv -> NodeIdx -> Rect -> IO ()
paintClippedChildren env idx rect = withClip (peDrawArena env) rect (walkChildrenWithOccluders env idx)

-- | Accent ring around the widget holding keyboard focus. Text fields and
-- selects already swap in an accent border while focused, so they get none.
-- Rows ('buttonFlagRow') fill their scroller edge to edge, so their ring sits
-- just inside the row; colour picker parts ring the square or bar they draw.
{-# NOINLINE paintFocusRing #-}
paintFocusRing :: PaintEnv -> NodeIdx -> NodeType -> Rect -> IO ()
paintFocusRing env idx nt rect = do
  wid <- getWidgetId (peNodeArena env) idx
  when (wid == peFocusRing env && nt /= NodeTextInput && nt /= NodeTextArea && nt /= NodeSelect) $ do
    target <-
      if nt == NodeColorPicker
        then colorPickerPartRect (peNodeArena env) idx rect
        else pure rect
    isRow <- if nt == NodeButton then hasFlag buttonFlagRow <$> getStyleIdx (peNodeArena env) idx else pure False
    let (ring, radius)
          | isRow = (rectInflate (-1) target, 0)
          | otherwise = (rectInflate 2 target, 4)
    pushRoundedStroke (peDrawArena env) ring radius 1.5 (themeFocusRing (peTheme env))

paintContainerNode :: PaintEnv -> NodeIdx -> Rect -> IO ()
paintContainerNode env@PaintEnv {peContext = ctx} idx rect = do
  walkChildrenWithOccluders env idx
  wid <- getWidgetId (peNodeArena env) idx
  mBuild <- lookupCustomDrawing ctx wid
  forM_ mBuild $ \(CustomDrawingEntry _ build _ _ _) -> do
    cdc <- mkCustomDrawContext ctx (peFontMetrics env) wid
    emitDrawingOps env rect (build cdc rect)

-- | A drawing's ops clipped to its rect, in the env's default font. Image
-- ops naming a registered 'ImageId' draw from the atlas.
emitDrawingOps :: PaintEnv -> Rect -> SmallArray DrawOp -> IO ()
emitDrawingOps env rect ops = withClip (peDrawArena env) rect (emitOps env ops)

-- | Ops in the env's default font, unclipped. NOINLINE: inlined into its
-- caller, the clip action would capture these arguments and allocate more
-- per drawing.
{-# NOINLINE emitOps #-}
emitOps :: PaintEnv -> SmallArray DrawOp -> IO ()
emitOps env@PaintEnv {peDrawArena = da} = emitDrawOps da (peFontMetrics env) (ctxFontSize ctx) (resolveTextFont ctx) (atlasImageUv ctx)
  where
    ctx = peContext env

-- | The atlas texture and UV bounds of a registered image.
atlasImageUv :: Context -> Int -> IO (Maybe (Int, (Float, Float, Float, Float)))
atlasImageUv ctx tid = fmap (atlasTextureId,) <$> lookupImageUv ctx (ImageId tid)

paintPanelNode :: PaintEnv -> NodeIdx -> Rect -> IO ()
paintPanelNode env@PaintEnv {peDrawArena = da} idx rect = do
  let style = themePanel (peTheme env)
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
    wTag <- axTag <$> getWidthSizing arena idx
    hTag <- axTag <$> getHeightSizing arena idx
    if wTag == SizingGrow && hTag == SizingGrow
      then pushRect da rect (if inFloating then styleBg (themeFloatingWindow tm) else themeWindow tm)
      else do
        let well = (if inFloating then themeFloatingWindow tm else themeInput tm) {styleCornerRadius = 0}
        paintStyledRect da well rect
  withClip da (scrollNodeViewport sn x y w h) $ walkChildrenWithOccluders env idx
  paintScrollChrome env idx sn rect

-- | A scroll container's scrollbars, drawn one layer above the content so
-- they stay on top of it.
paintScrollChrome :: PaintEnv -> NodeIdx -> ScrollNode -> Rect -> IO ()
paintScrollChrome env@PaintEnv {peDrawArena = da, peTheme = theme} idx sn (Rect x y w h) = do
  wid <- getWidgetId (peNodeArena env) idx
  V2 offX offY <- getScrollOffset2D (peContext env) wid
  case scrollNodeBars sn x y w h offX offY of
    (Nothing, Nothing) -> pure ()
    (mV, mH) -> do
      layer <- currentLayer da
      beginLayer da (if layer == LayerOverlay then LayerChrome else LayerContent)
      let base = case snSlot sn of
            ScrollBarWindow -> themeFloatingWindow theme
            _ -> themeInput theme
      paintScrollBars (peContext env) da theme base wid mV mH
      beginLayer da layer

-- | A text node's lines, drawn with the metrics its span cache entry prepared
-- for each line when it was laid out.
{-# NOINLINE paintTextNode #-}
paintTextNode :: PaintEnv -> NodeIdx -> Rect -> IO ()
paintTextNode env@PaintEnv {peNodeArena = arena, peDrawArena = da} idx rect@(Rect x y w h) = do
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

-- | An image node. Without a look it is stretched over its rect, tinted by
-- its font colour. With a look ('getImageNode') it is fitted, cropped,
-- zoomed, faded and rotated ('lookDraw'), and clipped to its rect when
-- rotated. Disabled images fade like disabled widget colours; unregistered
-- ones paint the accent.
paintImageNode :: PaintEnv -> NodeIdx -> Rect -> IO ()
paintImageNode env@PaintEnv {peDrawArena = da} idx rect = do
  let na = peNodeArena env
      fade = if peScope env .&. 1 /= 0 then 1 - themeDisabledFade (peTheme env) else 1
      iid = ImageId . imageIdFromText
  tex <- iid <$> getText na idx
  node <- getImageNode na idx
  case node of
    Nothing ->
      lookupImageUv (peContext env) tex >>= \case
        Just (u0, v0, u1, v1) -> do
          base <- fromMaybe (colorRGBA 255 255 255 255) <$> getNodeFontColor na idx
          pushImage da rect atlasTextureId u0 v0 u1 v1 (fadeBy fade base)
        Nothing -> accent
    Just ImageNode {inLook = look} ->
      lookupImageSize (peContext env) tex >>= \case
        Just size -> forM_ (lookDraw look size tex fade rect) $ \d ->
          (if imageAngle d /= 0 then withClip da rect else id) $
            forM_ (imageDrawOp d) (pushImageOp da (atlasImageUv (peContext env)))
        Nothing -> accent
  where
    accent = pushRect da rect (themeAccent (peTheme env))

{-# NOINLINE paintDrawingNode #-}
paintDrawingNode :: PaintEnv -> NodeIdx -> Rect -> IO ()
paintDrawingNode env@PaintEnv {peContext = ctx} idx rect = do
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

-- | Lower the children of @idx@ with the current paint env, topmost last
-- ('forChildrenInPaintOrder_'). NOINLINE keeps this recursive call out
-- of the simplifier's loop analysis, so the whole walker stays a call to
-- opaque seams rather than one inlined monster.
{-# NOINLINE walkChildrenWithOccluders #-}
walkChildrenWithOccluders :: PaintEnv -> NodeIdx -> IO ()
walkChildrenWithOccluders env idx =
  forChildrenInPaintOrder_ (peNodeArena env) idx (paintNodeWithEnv env)

-- | Children walk for callers painting a subtree inside their own clip
-- (floating overlays); builds a fresh env without occluders.
{-# NOINLINE walkChildren #-}
walkChildren :: Context -> NodeIdx -> IO ()
walkChildren ctx idx = buildPaintEnv ctx emptyPrimArray >>= (`walkChildrenWithOccluders` idx)
