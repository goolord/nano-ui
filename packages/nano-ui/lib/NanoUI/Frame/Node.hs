-- | Per-node queries shared by the paint, span, scroll and hit passes: the
-- font a node renders and measures in, and a scroll node's content viewport.
module NanoUI.Frame.Node
  ( resolveFontFor
  , nodeFontMetrics
  , scrollViewportAt
  ) where

import Data.Text (Text)
import NanoUI.Context (Context (..))
import NanoUI.Font (FontMetrics, isDefaultNodeFont, measureTextIO)
import NanoUI.Frame.Scroll.Geometry
  ( decodeScrollConfig
  , isScrollStyle2D
  , scrollContentClip
  , scrollViewportClip2D
  )
import NanoUI.Layout.Arena
  ( NodeIdx
  , getDirection
  , getNodeFontSize
  , getNodeValue
  , getPadding
  , getScrollContentW
  , getStyleIdx
  )
import NanoUI.Layout.Solve (scrollBarSlotOf)
import NanoUI.Style (FontVariant (..))
import NanoUI.Types (Rect)
import NanoUI.WidgetText (textNodeFontStyle, textNodeFontVariant, textNodeFontWeight)

-- | Font for an explicit node size and packed style: the metrics, whether the
-- host returned a native styled face (paint then skips synthetic weight and
-- slant), and the matching measure. Base sans and mono resolve to the
-- pre-read metrics; everything else defers to the host resolver. INLINE: it
-- runs for every text-bearing node painted, and inlining lets the result
-- triple fold away at each call site (measured: 30 MB less allocation over
-- the 3000-frame profile).
{-# INLINE resolveFontFor #-}
resolveFontFor :: Context -> Float -> Int -> IO (FontMetrics, Bool, Text -> IO (Float, Float))
resolveFontFor ctx size si
  | isDefaultNodeFont size weight style variant =
      pure $
        if variant == FontMono
          then (ctxMonoFontMetrics ctx, False, measureTextIO (ctxMonoFontMetrics ctx))
          else (ctxFontMetrics ctx, False, ctxMeasureText ctx)
  | otherwise = do
      (fm, native) <- ctxResolveFont ctx size weight style variant
      pure (fm, native, ctxResolveMeasure ctx size weight style variant)
  where
    variant = textNodeFontVariant si
    weight = textNodeFontWeight si
    style = textNodeFontStyle si

-- | Metrics of the font node @idx@ is styled with.
nodeFontMetrics :: Context -> NodeIdx -> IO FontMetrics
nodeFontMetrics ctx idx = do
  si <- getStyleIdx (ctxNodeArena ctx) idx
  size <- getNodeFontSize (ctxNodeArena ctx) idx
  (fm, _, _) <- resolveFontFor ctx size si
  pure fm

-- | Content viewport of scroll node @idx@ placed at @x y w h@: its padding box
-- minus the live scrollbar gutters.
scrollViewportAt :: Context -> NodeIdx -> Float -> Float -> Float -> Float -> IO Rect
scrollViewportAt ctx idx x y w h = do
  let na = ctxNodeArena ctx
      fm = ctxFontMetrics ctx
  si <- getStyleIdx na idx
  pad <- getPadding na idx
  slot <- scrollBarSlotOf na idx
  contentMain <- getNodeValue na idx
  let cfg = decodeScrollConfig si
  if isScrollStyle2D si
    then do
      contentW <- getScrollContentW na idx
      pure (scrollViewportClip2D fm slot cfg x y w h pad contentW contentMain)
    else do
      dir <- getDirection na idx
      pure (scrollContentClip fm slot cfg dir x y w h pad contentMain)
