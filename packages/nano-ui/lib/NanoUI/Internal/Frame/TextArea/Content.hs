-- | Text-area geometry and store-backed content shared by painting, scrolling
-- and hit testing: the field box and content clip, which scrollbars show
-- where, the node font, the stored document buffer and the cached content
-- extent. Free of the editor widget modules so scroll code stays light.
module NanoUI.Internal.Frame.TextArea.Content
  ( textAreaLineHeight
  , textAreaFieldClip
  , textAreaBarLane
  , TextAreaBars (..)
  , textAreaBars
  , TextAreaScrollBarLayouts (..)
  , textAreaScrollBarLayouts
  , resolveTextAreaFont
  , textAreaBuffer
  , textAreaContentMetrics
  , textAreaContentGeom
  , isMouseOnTextAreaScrollBarAt
  ) where

import Data.IORef (readIORef)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import NanoUI.Internal.Context (Context (..), WidgetStore, getStore, intKey, modifyStore, slotKey)
import NanoUI.Internal.Font (FontMetrics (..), ScrollBarSlot (..), lineWidthIO, scrollBarGeomFor, scrollBarSideGap, widgetContentInset)
import NanoUI.Internal.Frame.Scroll.Geometry (ScrollBarLayout (..), scrollBarLayout, scrollChromeLane)
import NanoUI.Internal.Layout.Arena (DirTag (..), NodeIdx, getNodeFontSize, getNodeRect, getWidgetId)
import NanoUI.Internal.Store (Slot (..), fieldPoint, findSlot, insertDyn, lookupDyn)
import NanoUI.Internal.Style (FontStyle (..), FontVariant (..), FontWeight (..), Padding (..))
import NanoUI.Internal.Types (Rect (..), V2, onGrid, rectContains)
import qualified NanoUI.Widgets.TextBuffer as TB

-- | Text area row height, snapped to the device pixel grid.
textAreaLineHeight :: FontMetrics -> Float
textAreaLineHeight fm = onGrid (fmSnapScale fm) (fmLineHeight fm)

-- | Text clip of a text area field. A caption-less text area's field is its
-- whole node rect.
textAreaFieldClip :: FontMetrics -> Rect -> Rect
textAreaFieldClip fm (Rect fx fy fw fh) =
  let s = fmSnapScale fm
      (ix, iy) = widgetContentInset fm
   in Rect (fx + onGrid s ix) (fy + onGrid s iy) (max 0 (fw - 2 * ix)) (max 0 (fh - 2 * iy))

-- | Width of the vertical and height of the horizontal scrollbar lane.
textAreaBarLane :: Float
textAreaBarLane = fst (scrollBarGeomFor ScrollBarList) + scrollBarSideGap

-- | Which scrollbars a text area shows for its content extent, the text
-- viewport they leave, and the paddings that place each bar's lane.
data TextAreaBars = TextAreaBars
  { tabVertical :: !Bool
  , tabHorizontal :: !Bool
  , tabViewW :: !Float
  , tabViewH :: !Float
  , tabPadV :: !Padding
  , tabPadH :: !Padding
  }

textAreaBars :: FontMetrics -> Rect -> Float -> Float -> TextAreaBars
textAreaBars fm (Rect _ _ fw fh) contentW contentH =
  let (ix, iy) = widgetContentInset fm
      innerW = max 0 (fw - 2 * ix)
      innerH = max 0 (fh - 2 * iy)
      lane = textAreaBarLane
      -- Either bar's lane can push the other axis into overflow.
      hasV = contentH > (if contentW > innerW then max 0 (innerH - lane) else innerH)
      hasH = contentW > (if contentH > innerH then max 0 (innerW - lane) else innerW)
   in TextAreaBars
        { tabVertical = hasV
        , tabHorizontal = hasH
        , tabViewW = if hasV then max 0 (innerW - lane) else innerW
        , tabViewH = if hasH then max 0 (innerH - lane) else innerH
        , tabPadV = Padding 0 0 iy (if hasH then iy + lane else iy)
        , tabPadH = Padding ix (if hasV then ix + lane else ix) 0 0
        }

-- | Optional vertical and horizontal bars after accounting for their shared corner.
data TextAreaScrollBarLayouts = TextAreaScrollBarLayouts
  { tasbVertical :: !(Maybe ScrollBarLayout)
  , tasbHorizontal :: !(Maybe ScrollBarLayout)
  }
  deriving (Eq, Show)

-- | Compute both bars from field bounds, content width/height, and x/y offsets,
-- all in logical pixels. Absent bars have 'Nothing' layouts.
textAreaScrollBarLayouts :: FontMetrics -> Rect -> Float -> Float -> Float -> Float -> TextAreaScrollBarLayouts
textAreaScrollBarLayouts fm field@(Rect x y w h) contentW contentH scrollX scrollY =
  let bars = textAreaBars fm field contentW contentH
      layout shown dir pad content off
        | shown = scrollBarLayout ScrollBarList dir x y w h pad content off
        | otherwise = Nothing
   in TextAreaScrollBarLayouts
        { tasbVertical = layout (tabVertical bars) DirColumn (tabPadV bars) contentH scrollY
        , tasbHorizontal = layout (tabHorizontal bars) DirRow (tabPadH bars) contentW scrollX
        }

-- | Font the text-area content is laid out and painted in. Honors the node's
-- @layoutFontSize@ (set via 'fontSize' on the editor layout) so a single text
-- area can zoom without changing the rest of the UI. A size of 0 means the
-- base UI font.
resolveTextAreaFont :: Context -> NodeIdx -> IO FontMetrics
resolveTextAreaFont ctx idx = do
  size <- getNodeFontSize (ctxNodeArena ctx) idx
  if size <= 0
    then pure (ctxFontMetrics ctx)
    else fst <$> ctxResolveFont ctx size WeightNormal FontStyleNormal FontRegular

-- | The text area's 'TB.TextBuffer', which holds its document and caret. The
-- widget stores one over the lines of every document it adopts, so it is only
-- missing for a text area never declared, which holds an empty document.
textAreaBuffer :: WidgetStore -> Int -> TB.TextBuffer
textAreaBuffer store key = fromMaybe TB.empty (lookupDyn (slotKey SlotTextAreaBuffer key) store)

-- | Content extent of a text area, @(contentWidth, contentHeight)@. Measuring
-- the width scans every character of the document, so the widths are kept
-- per line, with the extent, and only the lines an edit touched since they
-- were measured ('TB.changedLines') are measured again, or every line once the
-- node font or the metrics change.
textAreaContentMetrics :: Context -> NodeIdx -> IO (Float, Float)
textAreaContentMetrics ctx idx = do
  wid <- getWidgetId (ctxNodeArena ctx) idx
  size <- getNodeFontSize (ctxNodeArena ctx) idx
  store <- getStore ctx
  let key = intKey wid
      widthsKey = slotKey SlotTextAreaWidths key
      buf = textAreaBuffer store key
      lns = TB.bufferLines buf
      (seenHead, seenTail) = TB.changedLines buf
      cached = lookupDyn widthsKey store
  case cached of
    Just (LineWidths font _ _ _ widestW contentH)
      | font == size && seenHead >= Seq.length lns -> pure (widestW, contentH)
    _ -> do
      fm <- resolveTextAreaFont ctx idx
      gen <- readIORef (ctxMetricGen ctx)
      let contentH = fromIntegral (max 1 (Seq.length lns)) * textAreaLineHeight fm
          LineWidths _ _ measured widest widestW _ = case cached of
            Just lw@(LineWidths font fontGen _ _ _ _) | font == size && fontGen == gen -> lw
            _ -> LineWidths size gen Seq.empty (-1) 0 0
          -- Keep the widths of the lines no edit touched since the last
          -- measurement and measure the rest.
          keepHead = min seenHead (Seq.length measured)
          keepTail = min seenTail (Seq.length measured - keepHead)
          changed = Seq.take (Seq.length lns - keepHead - keepTail) (Seq.drop keepHead lns)
      fresh <- traverse (lineWidthIO fm) changed
      let widths = Seq.take keepHead measured <> fresh <> Seq.drop (Seq.length measured - keepTail) measured
          shift = Seq.length lns - Seq.length measured
          widestFrom :: Int -> Seq Float -> (Int, Float)
          widestFrom off = Seq.foldlWithIndex (\best i w -> if w > snd best then (off + i, w) else best) (-1, 0)
          freshWidest = widestFrom keepHead fresh
          -- The widest line so far still counts when it was kept; only when
          -- an edit touched it do the widths need a full pass.
          -- A changed line at least as wide as the old widest also still wins.
          (widest', contentW)
            | widest >= 0 && widest < keepHead = pick (widest, widestW) freshWidest
            | widest >= 0 && widest >= Seq.length measured - keepTail = pick (widest + shift, widestW) freshWidest
            | widest >= 0 && snd freshWidest >= widestW = freshWidest
            | otherwise = widestFrom 0 widths
          pick a b = if snd b > snd a then b else a
      modifyStore ctx $
        insertDyn widthsKey (LineWidths size gen widths widest' contentW contentH)
          . insertDyn (slotKey SlotTextAreaBuffer key) (TB.markLinesSeen buf)
      pure (contentW, contentH)

-- | Measured widths of a text area's lines, the font size and metric
-- generation they were measured at, the widest line with its width, and the
-- content height.
data LineWidths = LineWidths !Float !Int !(Seq Float) !Int !Float !Float

-- | Node font, field rect and content extent @(width, height)@ of a text area.
-- Zoom changes the node font, so scroll and hit math resolve it here rather
-- than using the base font, or the scroll range would clamp short.
textAreaContentGeom :: Context -> NodeIdx -> IO (FontMetrics, Rect, Float, Float)
textAreaContentGeom ctx idx = do
  fm <- resolveTextAreaFont ctx idx
  rect <- getNodeRect (ctxNodeArena ctx) idx
  (contentW, contentH) <- textAreaContentMetrics ctx idx
  pure (fm, rect, contentW, contentH)

-- | Whether @mouse@ is over a shown bar's lane or track. Uses the cached
-- content extent: this runs on every hover through the cursor query.
isMouseOnTextAreaScrollBarAt :: Context -> NodeIdx -> V2 -> IO Bool
isMouseOnTextAreaScrollBarAt ctx idx mouse = do
  (fm, field@(Rect x y w h), contentW, contentH) <- textAreaContentGeom ctx idx
  wid <- getWidgetId (ctxNodeArena ctx) idx
  store <- getStore ctx
  let (sx, sy) = findSlot fieldPoint (0, 0) (slotKey SlotTextAreaScroll (intKey wid)) store
      bars = textAreaBars fm field contentW contentH
      layouts = textAreaScrollBarLayouts fm field contentW contentH sx sy
      onBar dir pad =
        maybe False $ \layout ->
          rectContains (scrollChromeLane ScrollBarList dir x y w h pad) mouse
            || rectContains (sbTrack layout) mouse
  pure (onBar DirColumn (tabPadV bars) (tasbVertical layouts) || onBar DirRow (tabPadH bars) (tasbHorizontal layouts))
