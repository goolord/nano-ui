-- | Per-node queries shared by the paint, span, scroll and hit passes: the
-- font a node renders and measures in, and a scroll node's fields and content
-- viewport.
module NanoUI.Internal.Frame.Node
  ( resolveFontFor
  , nodeFontNative
  , resolveTextFont
  , nodeFontMetrics
  , ScrollNode (..)
  , readScrollNode
  , scrollNodeViewport
  ) where

import Data.Text (Text)
import NanoUI.Internal.Context (Context (..))
import NanoUI.Internal.Draw.Types (TextFont (..))
import NanoUI.Internal.Font (FontMetrics, ScrollBarSlot, isDefaultNodeFont, measureTextIO)
import NanoUI.Internal.Frame.Scroll.Geometry
  ( ScrollConfig
  , decodeScrollConfig
  , scrollConfigNative2D
  , scrollContentClip
  , scrollViewportClip2D
  )
import NanoUI.Internal.Layout.Arena
  ( DirTag
  , NodeArena
  , NodeIdx
  , NodeType (..)
  , getDirection
  , getNodeFontSize
  , getNodeType
  , getNodeValue
  , getPadding
  , getScrollContentW
  , getStyleIdx
  )
import NanoUI.Internal.Layout.Solve (scrollBarSlotOf)
import NanoUI.Internal.Style (FontVariant (..), Padding)
import NanoUI.Internal.Types (Rect)
import NanoUI.Internal.WidgetText (textNodeFontStyle, textNodeFontVariant, textNodeFontWeight)

-- | Font for a node of type @nt@ with an explicit size and packed style: the
-- metrics, whether the host returned a native styled face (paint then skips
-- synthetic weight and slant), and the matching measure. Base sans and mono
-- resolve to the pre-read metrics; everything else defers to the host
-- resolver. INLINE: it runs for every text-bearing node painted, and inlining
-- lets the result triple fold away at each call site (measured: 30 MB less
-- allocation over the 3000-frame profile).
--
-- Only text and text-input nodes pack a font into their style. Other widgets
-- keep their own data in those bits (a radio's option index, a colour picker
-- part, a tab's look), so their style must not be read as a font.
{-# INLINE resolveFontFor #-}
resolveFontFor :: Context -> NodeType -> Float -> Int -> IO (FontMetrics, Bool, Text -> IO (Float, Float))
resolveFontFor ctx nt size packed
  | isDefaultNodeFont size weight style variant =
      pure $
        if variant == FontMono
          then (ctxMonoFontMetrics ctx, False, measureTextIO (ctxMonoFontMetrics ctx))
          else (ctxFontMetrics ctx, False, ctxMeasureText ctx)
  | otherwise = do
      (fm, native) <- ctxResolveFont ctx size weight style variant
      pure (fm, native, ctxResolveMeasure ctx size weight style variant)
  where
    si = if nt == NodeText || nt == NodeTextInput then packed else 0
    variant = textNodeFontVariant si
    weight = textNodeFontWeight si
    style = textNodeFontStyle si

-- | Whether the host draws the weight and slant of the font a text node of
-- @size@ and packed style @si@ is set in ('resolveFontFor'), for paint, which
-- takes the metrics from the span cache.
{-# INLINE nodeFontNative #-}
nodeFontNative :: Context -> Float -> Int -> IO Bool
nodeFontNative ctx size si
  | isDefaultNodeFont size weight style variant = pure False
  | otherwise = snd <$> ctxResolveFont ctx size weight style variant
  where
    variant = textNodeFontVariant si
    weight = textNodeFontWeight si
    style = textNodeFontStyle si

-- | The font a @DrawTextStyled@ op names, and whether the host draws its
-- weight and slant natively.
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

-- | What the scroll passes read off a scroll container: its bar slot, scroll
-- config, whether it scrolls natively in 2D, direction, padding, the content
-- extent along its main axis (the content height for 2D) and, for 2D, the
-- content width.
data ScrollNode = ScrollNode
  { snSlot :: !ScrollBarSlot
  , snConfig :: !ScrollConfig
  , sn2D :: !Bool
  , snDir :: !DirTag
  , snPad :: {-# UNPACK #-} !Padding
  , snContentMain :: {-# UNPACK #-} !Float
  , snContentW :: {-# UNPACK #-} !Float
  }

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

-- | Content viewport of a scroll node placed at @x y w h@: its padding box
-- minus the live scrollbar gutters.
scrollNodeViewport :: ScrollNode -> Float -> Float -> Float -> Float -> Rect
scrollNodeViewport (ScrollNode slot cfg native2D dir pad contentMain contentW) x y w h
  | native2D = scrollViewportClip2D slot cfg x y w h pad contentW contentMain
  | otherwise = scrollContentClip slot cfg dir x y w h pad contentMain
