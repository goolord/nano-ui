-- Paint traversal for NanoUI. This module owns the node walk; all widget
-- chrome painting lives in sibling NanoUI.Frame.Paint.Widgets.
--
-- The module is shaped for GHC's optimizer: the recursive walker
--
--   paintNodeWithEnv -> lowerNodeVisible (explicit dispatch)
--         -> per-node painters (containers recurse via walkChildrenWithOccluders)
--
-- sits on top of {-# NOINLINE #-} seams, and the two heavyweight parts
-- (widget chrome, leaf painters) were extracted into separate modules so no
-- single module carries the whole painting body inside the recursive loop.
-- That stops the simplifier / SpecConstr from seeing one monolithic binding
-- in the loop, which is what blew up compilation under
-- -fspecialise-aggressively + LLVM. See the note above the OPTIONS_GHC
-- pragma for the guard flag.
{-# OPTIONS_GHC -fasm -fno-specialise-aggressively #-}

{-# LANGUAGE DataKinds #-}

module NanoUI.Frame.Paint
  ( lowerShapes
  , lowerNode
  , walkChildren
  ) where


import Control.Monad (forM_, unless, when)
import Data.IORef (readIORef)
import Data.Word (Word32)
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import NanoUI.Widgets.Custom (mkCustomDrawContext)
import NanoUI.Context
  ( Context (..)
  , DrawingEntry (..)
  , atlasTextureId
  , cachedCustomDrawingOps
  , cachedDrawingOps
  , lookupCustomDrawing
  , lookupDrawing
  , lookupImageUv
  )
import NanoUI.Draw
  ( DrawArena (..)
  , emitDrawOps
  , pushImage
  , pushRect
  , pushText
  , pushTextStyled
  , withClip
  )
import NanoUI.Layout.Arena
  ( DirTag (..)
  , NodeIdx
  , NodeType (..)
  , SizingTag (..)
  , arenaCount
  , forChildNodes_
  , getScrollContentW
  , getDirection
  , getHeightSizing
  , getNodeType
  , getNodeValue
  , getPadding
  , getRect
  , getStyleIdx
  , getText
  , getWidthSizing
  , getWidgetId
  , isFloatingNode
  , getNodeFontSize
  )
import NanoUI.Layout.Solve (scrollBarSlotOf)
import NanoUI.Style
  ( FontStyle (..)
  , FontWeight (..)
  , TextDecoration (..)
  , Style (..)
  , Theme (..)
  , styleBg
  , themeAccent
  , themeFloatingWindow
  , themeInput
  , themePanel
  , unpackPanelStyle
  , themeSeparator
  , themeWindow
  )
import NanoUI.Types (Color (..), ImageId (..), Rect (..), colorA, colorRGBA, rectFullyInside, rectInflate, rectH, rectW, rectX, rectY)
import NanoUI.WidgetText
  ( tableStripeColor
  , textNodeFontVariant
  , textNodeFontWeight
  , textNodeFontStyle
  , textNodeTextDecoration
  )
import NanoUI.Frame.Chrome
  ( fillStyledRect
  , floatingAncestor
  , imageIdFromText
  , overlayModalStyle
  , overlayMenuStyle
  , overlayWindowStyle
  , strokeStyledRect
  )
import NanoUI.Frame.Scroll.Geometry (borderContentClip, padContentClip, scrollContentClip)
import NanoUI.Frame.Scroll (paintScrollChrome)
import NanoUI.Frame.Scroll.Geometry
  ( decodeScrollConfig
  , isScrollStyle2D
  , scrollBare
  , scrollChromeActive
  , scrollViewportClip2D
  )
import NanoUI.Frame.Spans (collectNodeTextSpans)
import NanoUI.Frame.Paint.Types (PaintEnv (..), buildPaintEnv, resolveNodeFont)
import NanoUI.Frame.Paint.Widgets (paintTextAreaNode, paintTextInputNode, paintWidget)

lowerShapes :: Context -> IO ()
lowerShapes ctx = do
  count <- arenaCount (ctxNodeArena ctx)
  when (count > 0) $ do
    occluders <- collectFloatingOccluders ctx
    buildPaintEnv ctx occluders >>= (`paintNodeWithEnv` 0)

collectFloatingOccluders :: Context -> IO [Rect]
collectFloatingOccluders ctx = do
  n <- arenaCount (ctxNodeArena ctx)
  theme <- readIORef (ctxTheme ctx)
  let winStyle = overlayWindowStyle theme
      modalStyle = overlayModalStyle theme
      menuStyle = overlayMenuStyle theme
      isOpaque s = colorA (styleBg s) == 255
      winOpaque = isOpaque winStyle
      modalOpaque = isOpaque modalStyle
      menuOpaque = isOpaque menuStyle
      go idx acc
        | idx >= n = pure acc
        | otherwise = do
            nt <- getNodeType (ctxNodeArena ctx) idx
            case nt of
              NodeWindow | winOpaque -> checkPanel idx acc
              NodeModal  | modalOpaque -> checkPanel idx acc
              NodePopup  | menuOpaque -> checkPanel idx acc
              _ -> go (idx + 1) acc
      checkPanel idx acc = do
        (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
        if w > 6 && h > 6
          then do
            let !r = rectInflate (-3) (Rect x y w h)
            go (idx + 1) (r : acc)
          else go (idx + 1) acc
  go 0 []

-- | Public single-node entry point: builds a fresh paint env (no occluders).
{-# NOINLINE lowerNode #-}
lowerNode :: Context -> NodeIdx -> IO ()
lowerNode ctx idx = buildPaintEnv ctx [] >>= (`paintNodeWithEnv` idx)

-- | Clip + occluder short-circuit, then lower the node. NOINLINE so the
-- recursive container walk never exposes the dispatch below to the simplifier.
{-# NOINLINE paintNodeWithEnv #-}
paintNodeWithEnv :: PaintEnv -> NodeIdx -> IO ()
paintNodeWithEnv env idx = do
  (x, y, w, h) <- getRect (peNodeArena env) idx
  let da = peDrawArena env
  (cx, cy, cw, ch) <- readIORef (daCurrentClip da)
  let !l = max x cx
      !t = max y cy
      !r = min (x + w) (cx + cw)
      !b = min (y + h) (cy + ch)
  if r <= l || b <= t
    then pure ()
    else do
      if peHasOccluders env && any (rectFullyInside (Rect l t (r - l) (b - t))) (peOccluders env)
        then pure ()
        else do
          nt <- getNodeType (peNodeArena env) idx
          let !rect = Rect x y w h
          lowerNodeVisible env idx nt rect

-- | Explicit per-node-type dispatch. Kept NOINLINE and thin so the recursive
-- loop never sees the branch bodies; the structural painters are INLINE and
-- fold into this one (non-recursive) function, while the heavyweight widget
-- chrome stays out-of-line in Paint.Widgets.
{-# NOINLINE lowerNodeVisible #-}
lowerNodeVisible :: PaintEnv -> NodeIdx -> NodeType -> Rect -> IO ()
lowerNodeVisible env idx nt rect =
  case nt of
    NodeContainer -> paintContainerNode env idx rect
    NodePanel -> paintPanelNode env idx rect
    NodeScrollContainer -> paintScrollContainerNode env idx rect
    NodeText -> paintTextNode env idx rect
    NodeSeparator -> paintSeparatorNode env rect
    NodeTextInput | not (peTerminal env) -> paintTextInputNode env idx rect
    NodeTextArea | not (peTerminal env) -> paintTextAreaNode env idx rect
    NodeSpacer -> pure ()
    NodeModal -> pure ()
    NodeWindow -> pure ()
    NodePopup -> pure ()
    NodeBox -> paintBoxNode env idx rect
    NodeImage -> paintImageNode env idx rect
    NodeDrawing -> paintDrawingNode env idx rect
    NodeWidget -> pure ()
    _ -> paintWidget env idx nt rect

{-# INLINE paintContainerNode #-}
paintContainerNode :: PaintEnv -> NodeIdx -> Rect -> IO ()
paintContainerNode env idx rect = do
  walkChildrenWithOccluders env idx
  let ctx = peContext env
  wid <- getWidgetId (peNodeArena env) idx
  mBuild <- lookupCustomDrawing ctx wid
  case mBuild of
    Nothing -> pure ()
    Just build -> do
      let fm = peFontMetrics env
          da = peDrawArena env
      cdc <- mkCustomDrawContext ctx fm wid
      withClip da rect (emitDrawOps da fm (build cdc rect))

{-# INLINE paintPanelNode #-}
paintPanelNode :: PaintEnv -> NodeIdx -> Rect -> IO ()
paintPanelNode env idx rect = do
  let da = peDrawArena env
      tm = peTheme env
  si <- getStyleIdx (peNodeArena env) idx
  let style = if si /= 0
                then unpackPanelStyle (themePanel tm) si
                else themePanel tm
  fillStyledRect da (peTerminal env) style rect
  strokeStyledRect da (peTerminal env) style (rectX rect) (rectY rect) (rectW rect) (rectH rect)
  withClip da (borderContentClip style rect) $ walkChildrenWithOccluders env idx

{-# INLINE paintScrollContainerNode #-}
paintScrollContainerNode :: PaintEnv -> NodeIdx -> Rect -> IO ()
paintScrollContainerNode env idx rect = do
  let ctx = peContext env
      arena = peNodeArena env
      da = peDrawArena env
      tm = peTheme env
      host = peHost env
      fm = peFontMetrics env
      terminal = peTerminal env
      Rect x y w h = rect
  mFloat <- floatingAncestor ctx idx
  let inFloating = maybe False isFloatingNode mFloat
      baseStyle
        | inFloating = themeFloatingWindow tm
        | otherwise  = themeInput tm
  pad <- getPadding arena idx
  (wTag, _) <- getWidthSizing arena idx
  (hTag, _) <- getHeightSizing arena idx
  si <- getStyleIdx arena idx
  dir <- getDirection arena idx
  slot <- scrollBarSlotOf arena idx
  let cfg = decodeScrollConfig si
      native2D = isScrollStyle2D si
      padClip = padContentClip host fm x y w h pad
      innerW = rectW padClip
      innerH = rectH padClip
      wellStyle = baseStyle {styleCornerRadius = 0}
  (showChrome, inner) <-
    if native2D
      then do
        contentH <- getNodeValue arena idx
        contentW <- getScrollContentW arena idx
        pure
          ( scrollChromeActive cfg True DirColumn contentH innerH
              || scrollChromeActive cfg True DirRow contentW innerW
          , scrollViewportClip2D host fm slot cfg x y w h pad contentW contentH
          )
      else do
        contentSize <- getNodeValue arena idx
        let innerMain =
              case dir of
                DirColumn -> innerH
                DirRow -> innerW
        pure
          ( scrollChromeActive cfg False dir contentSize innerMain
          , scrollContentClip host fm slot cfg dir x y w h pad contentSize
          )
  -- A bare scroller paints nothing at all: it only lends its clip and
  -- offset, so whatever sits behind it (window, panel) keeps showing
  -- through. Grow×grow scrollers (page-level) keep no well so they blend
  -- into the window backdrop. That backdrop only exists while the runner
  -- clears it on DamageFull frames; on clip frames (scrolling, resize)
  -- the strip vacated by scrolled content has no covering command and
  -- the retained texture would show stale pixels — a ghost of a previous
  -- scroll position. Paint the full rect with the window color instead:
  -- invisible on a cleared backdrop, and clip replay then always
  -- repaints the whole viewport.
  if scrollBare cfg
    then pure ()
    else
      if wTag == SizingGrow && hTag == SizingGrow
        then pushRect da rect (if inFloating then styleBg (themeFloatingWindow tm) else themeWindow tm)
        else do
          fillStyledRect da terminal wellStyle rect
          strokeStyledRect da terminal wellStyle x y w h
  withClip da inner $ walkChildrenWithOccluders env idx
  when showChrome $ do
    wid <- getWidgetId arena idx
    paintScrollChrome ctx da idx wid x y w h pad tm terminal

{-# INLINE paintTextNode #-}
paintTextNode :: PaintEnv -> NodeIdx -> Rect -> IO ()
paintTextNode env idx rect = do
  let ctx = peContext env
      arena = peNodeArena env
      da = peDrawArena env
      tm = peTheme env
      terminal = peTerminal env
  si <- getStyleIdx arena idx
  case tableStripeColor tm si of
    Just stripe | not terminal -> pushRect da rect stripe
    _ -> pure ()
  raw <- getText arena idx
  unless (T.null raw) $ do
    spans <- collectNodeTextSpans ctx IM.empty idx
    fontSizeVal <- getNodeFontSize arena idx
    let fvar = textNodeFontVariant si
        fweight = textNodeFontWeight si
        fstyle  = textNodeFontStyle si
        fdeco   = textNodeTextDecoration si
    (fm', isNative) <- resolveNodeFont env fontSizeVal fweight fstyle fvar
    if not isNative && fdeco == DecorationNone
      then forM_ spans $ \(Rect tx ty _ _, line, spanFg, _) ->
        unless (T.null line) $
          pushText da fm' tx ty line spanFg
      else do
        let effWeight = if isNative then WeightNormal else fweight
            effStyle  = if isNative then FontStyleNormal else fstyle
        forM_ spans $ \(Rect tx ty _ _, line, spanFg, _) ->
          unless (T.null line) $
            pushTextStyled da fm' effWeight effStyle fdeco tx ty line spanFg

{-# INLINE paintSeparatorNode #-}
paintSeparatorNode :: PaintEnv -> Rect -> IO ()
paintSeparatorNode env rect = do
  let da = peDrawArena env
      tm = peTheme env
      terminal = peTerminal env
      Rect x y w h = rect
      hair = 1
  when (not terminal) $
    if w >= h
      then pushRect da (Rect x (y + (h - hair) / 2) w hair) (themeSeparator tm)
      else pushRect da (Rect (x + (w - hair) / 2) y hair h) (themeSeparator tm)

{-# INLINE paintBoxNode #-}
paintBoxNode :: PaintEnv -> NodeIdx -> Rect -> IO ()
paintBoxNode env idx rect = do
  si <- getStyleIdx (peNodeArena env) idx
  -- styleIdx holds RGBA Word32 bits; see `box` in NanoUI.Widgets.
  pushRect (peDrawArena env) rect (Color (fromIntegral si :: Word32))

{-# INLINE paintImageNode #-}
paintImageNode :: PaintEnv -> NodeIdx -> Rect -> IO ()
paintImageNode env idx rect = do
  let ctx = peContext env
      da = peDrawArena env
  tex <- imageIdFromText <$> getText (peNodeArena env) idx
  mUv <- lookupImageUv ctx (ImageId tex)
  case mUv of
    Just (u0, v0, u1, v1)
      | not (peTerminal env) ->
          pushImage da rect atlasTextureId u0 v0 u1 v1 (colorRGBA 255 255 255 255)
    _ -> pushRect da rect (themeAccent (peTheme env))

{-# INLINE paintDrawingNode #-}
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
      case mBuild of
        Nothing -> pure ()
        Just (DrawingEntry content build) -> do
          ops <- cachedDrawingOps ctx wid content rect build
          withClip da rect (emitDrawOps da fm ops)

-- | Lower the children of @idx@ with the current paint env. NOINLINE keeps
-- this recursive call out of the simplifier's loop analysis, so the whole
-- walker stays a call to opaque seams rather than one inlined monster.
{-# NOINLINE walkChildrenWithOccluders #-}
walkChildrenWithOccluders :: PaintEnv -> NodeIdx -> IO ()
walkChildrenWithOccluders env idx =
  forChildNodes_ (peNodeArena env) idx (paintNodeWithEnv env)

-- | Public children-walk entry point (keeps the public signature of the
-- original 'walkChildren', which callers invoke inside their own clips).
{-# NOINLINE walkChildren #-}
walkChildren :: Context -> NodeIdx -> IO ()
walkChildren ctx idx = buildPaintEnv ctx [] >>= (`walkChildrenWithOccluders` idx)