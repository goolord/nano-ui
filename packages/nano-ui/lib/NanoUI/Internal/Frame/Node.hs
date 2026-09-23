-- | Per-node queries shared by the paint, span, scroll and hit passes: the
-- font a node renders and measures in, a scroll node's fields, and the room
-- a widget's adornments take.
module NanoUI.Internal.Frame.Node
  ( resolveFontFor
  , nodeFontNative
  , resolveTextFont
  , nodeFontMetrics
  , readScrollNode
  , nodeAdornmentInsets
  ) where

import Data.Text (Text)
import NanoUI.Internal.Context (Context (..))
import NanoUI.Internal.Draw.Types (TextFont (..))
import NanoUI.Internal.Font (FontMetrics, isDefaultNodeFont, measureTextIO)
import NanoUI.Internal.Frame.Scroll.Geometry
import NanoUI.Internal.Layout.Arena
import NanoUI.Internal.Layout.Solve (scrollBarSlotOf)
import NanoUI.Internal.Style (FontVariant (..), TextDecoration (..))
import NanoUI.Internal.Types (Rect (..))
import NanoUI.Internal.WidgetText (textNodeFontStyle, textNodeFontVariant, textNodeFontWeight)

-- | Font for a node of type @nt@ with an explicit size and packed style: the
-- metrics, whether the host returned a native styled face (paint then skips
-- synthetic weight and slant), and the matching measure. Base sans and mono
-- resolve to the pre-read metrics; everything else defers to the host
-- resolver. INLINE: it runs for every text-bearing node painted, and inlining
-- lets the result triple fold away at each call site (measured: 30 MB less
-- allocation over the 3000-frame profile).
--
-- Only a node that packs a font into its style ('packsNodeFont') has its
-- style read as one.
{-# INLINE resolveFontFor #-}
resolveFontFor :: Context -> NodeType -> Float -> Int -> IO (FontMetrics, Bool, Text -> IO (Float, Float))
resolveFontFor ctx nt size packed = do
  (fm, native) <- resolveTextFont ctx font
  let measure
        | not (isDefaultNodeFont size weight style variant) = ctxResolveMeasure ctx size weight style variant
        | variant == FontMono = measureTextIO fm
        | otherwise = ctxMeasureText ctx
  pure (fm, native, measure)
  where
    font@(TextFont _ variant weight style _) =
      packedTextFont size (if packsNodeFont nt then packed else 0)

-- | Whether the host draws the weight and slant of the font a text node of
-- @size@ and packed style @si@ is set in ('resolveFontFor'), for paint, which
-- takes the metrics from the span cache.
{-# INLINE nodeFontNative #-}
nodeFontNative :: Context -> Float -> Int -> IO Bool
nodeFontNative ctx size si = snd <$> resolveTextFont ctx (packedTextFont size si)

-- | The font of a text node of @size@ and packed style @si@.
{-# INLINE packedTextFont #-}
packedTextFont :: Float -> Int -> TextFont
packedTextFont size si =
  TextFont size (textNodeFontVariant si) (textNodeFontWeight si) (textNodeFontStyle si) DecorationNone

-- | The font a @DrawTextStyled@ op names, and whether the host draws its
-- weight and slant natively. Base sans and mono resolve to the pre-read
-- metrics; everything else defers to the host resolver.
{-# INLINE resolveTextFont #-}
resolveTextFont :: Context -> TextFont -> IO (FontMetrics, Bool)
resolveTextFont ctx (TextFont size variant weight style _)
  | isDefaultNodeFont size weight style variant =
      pure (if variant == FontMono then ctxMonoFontMetrics ctx else ctxFontMetrics ctx, False)
  | otherwise = ctxResolveFont ctx size weight style variant

-- | Metrics of the font node @idx@ is styled with.
nodeFontMetrics :: Context -> NodeIdx -> IO FontMetrics
nodeFontMetrics ctx idx = do
  nt <- getNodeType (ctxNodeArena ctx) idx
  si <- getStyleIdx (ctxNodeArena ctx) idx
  size <- getNodeFontSize (ctxNodeArena ctx) idx
  (fm, _, _) <- resolveFontFor ctx nt size si
  pure fm

{-# INLINE readScrollNode #-}
readScrollNode :: NodeArena -> NodeIdx -> IO ScrollNode
readScrollNode na idx = do
  si <- getStyleIdx na idx
  slot <- scrollBarSlotOf na idx
  dir <- getDirection na idx
  pad <- getPadding na idx
  contentMain <- getNodeValue na idx
  contentW <- getScrollContentW na idx
  let cfg = decodeScrollConfig si
  pure $! ScrollNode slot cfg (si /= 0 && scrollConfigNative2D cfg) dir pad contentMain contentW

-- | How far the adornment rows ('adornRows') of widget @idx@, whose left edge
-- is at @x@ and which is @w@ wide, reach in from its left edge and from its
-- right, each with the widget's gap beyond them: the room its text keeps
-- clear of them, 0 on a side without any. Every button and field asks, most
-- without adornments, so that check is inlined and the rest is not.
{-# INLINE nodeAdornmentInsets #-}
nodeAdornmentInsets :: NodeArena -> NodeIdx -> Float -> Float -> IO (Float, Float)
nodeAdornmentInsets na idx x w = do
  kids <- getFirstChild na idx
  if kids < 0 then pure (0, 0) else adornmentRowInsets na idx x w

{-# NOINLINE adornmentRowInsets #-}
adornmentRowInsets :: NodeArena -> NodeIdx -> Float -> Float -> IO (Float, Float)
adornmentRowInsets na idx x w = do
  AdornRows li _ ti _ _ <- adornRows na idx
  a <- arenaArrays na
  gap <- readStyle a idx StyleGap
  let reach ci edge = if ci < 0 then pure 0 else (\r -> edge r + gap) <$> getNodeRect na ci
  (,) <$> reach li (\(Rect cx _ cw _) -> cx + cw - x) <*> reach ti (\(Rect cx _ _ _) -> x + w - cx)
