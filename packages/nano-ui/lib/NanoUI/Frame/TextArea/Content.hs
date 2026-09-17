{-# LANGUAGE BangPatterns #-}

-- | Store-backed text-area content shared by painting, scrolling and hit
-- testing: the node font, the cached document buffer and the cached content
-- extent. Free of the editor widget modules so scroll code stays light.
module NanoUI.Frame.TextArea.Content
  ( resolveTextAreaFont
  , ensureTextAreaBuffer
  , textAreaContentMetrics
  , textAreaContentGeom
  , isMouseOnTextAreaScrollBarAt
  ) where

import Data.Dynamic (fromDynamic, toDyn)
import Data.IORef (readIORef)
import qualified Data.IntMap.Strict as IM
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import NanoUI.Context (Context (..), WidgetStore (..), getStore, intKey, setStore, slotKey)
import NanoUI.Font (FontMetrics (..), lineWidthIO)
import NanoUI.Frame.TextArea.Geometry (isMouseOnTextAreaScrollBar)
import NanoUI.Layout.Arena (NodeIdx, getNodeFontSize, getRect, getWidgetId)
import NanoUI.Store
  ( slotTextAreaBuffer
  , slotTextAreaContentFont
  , slotTextAreaContentH
  , slotTextAreaContentW
  , slotTextAreaScroll
  , slotTextAreaWidths
  )
import NanoUI.Style (FontStyle (..), FontVariant (..), FontWeight (..))
import NanoUI.Types (Rect (..), V2, onGrid)
import qualified NanoUI.Widgets.TextBuffer as TB

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

-- | Return the text area's 'TB.TextBuffer', building it from the flat text only
-- when the cache is cold. Rebuilding splits the whole document into lines, so
-- caching it keeps loads and paint O(1) here. The cache is written together
-- with the flat text by 'saveTextAreaState', so a present entry is always the
-- buffer for the stored text.
ensureTextAreaBuffer :: Context -> Int -> Text -> IO TB.TextBuffer
ensureTextAreaBuffer ctx key text = do
  store <- getStore ctx
  case IM.lookup (slotKey slotTextAreaBuffer key) (storeDyn store) >>= fromDynamic of
    Just buf -> pure buf
    Nothing -> do
      let buf = TB.fromText text
      setStore ctx store {storeDyn = IM.insert (slotKey slotTextAreaBuffer key) (toDyn buf) (storeDyn store)}
      pure buf

-- | Content extent of a text area, @(contentWidth, contentHeight)@. Measuring
-- the width scans every character of the document, so the result is cached per
-- widget and only refreshed when the text changes (the editor clears
-- 'slotTextAreaContentFont') or the node font changes.
textAreaContentMetrics :: Context -> NodeIdx -> IO (Float, Float)
textAreaContentMetrics ctx idx = do
  wid <- getWidgetId (ctxNodeArena ctx) idx
  size <- getNodeFontSize (ctxNodeArena ctx) idx
  store <- getStore ctx
  let key = intKey wid
      cacheKeyF = slotKey slotTextAreaContentFont key
      cacheKeyW = slotKey slotTextAreaContentW key
      cacheKeyH = slotKey slotTextAreaContentH key
      widthsKey = slotKey slotTextAreaWidths key
      cachedFont = IM.findWithDefault (-1) cacheKeyF (storeFloat store)
      cachedW = IM.findWithDefault (-1) cacheKeyW (storeFloat store)
  if cachedFont == size && cachedW >= 0
    then pure (cachedW, IM.findWithDefault 0 cacheKeyH (storeFloat store))
    else do
      fm <- resolveTextAreaFont ctx idx
      gen <- readIORef (ctxMetricGen ctx)
      buf <- ensureTextAreaBuffer ctx key (IM.findWithDefault "" key (storeText store))
      let lns = TB.bufferLines buf
          lineH = onGrid (fmSnapScale fm) (fmLineHeight fm)
          contentH = fromIntegral (max 1 (Seq.length lns)) * lineH
          (seenHead, seenTail) = TB.changedLines buf
          previous = case IM.lookup widthsKey (storeDyn store) >>= fromDynamic of
            Just lw@(LineWidths font fontGen _ _ _) | font == size && fontGen == gen -> lw
            _ -> LineWidths size gen Seq.empty (-1) 0
          LineWidths _ _ measured widest widestW = previous
          -- Keep the widths of the lines no edit touched since the last
          -- measurement and measure the rest.
          keepHead = min seenHead (Seq.length measured)
          keepTail = min seenTail (Seq.length measured - keepHead)
          changed = Seq.take (Seq.length lns - keepHead - keepTail) (Seq.drop keepHead lns)
      fresh <- traverse (lineWidthIO fm) changed
      let widths = Seq.take keepHead measured <> fresh <> Seq.drop (Seq.length measured - keepTail) measured
          shift = Seq.length lns - Seq.length measured
          freshWidest = Seq.foldlWithIndex (\best i w -> if w > snd best then (keepHead + i, w) else best) (-1, 0) fresh
          -- The widest line so far still counts when it was kept; only when
          -- an edit touched it do the widths need a full pass.
          -- A changed line at least as wide as the old widest also still wins.
          (widest', contentW)
            | widest >= 0 && widest < keepHead = pick (widest, widestW) freshWidest
            | widest >= 0 && widest >= Seq.length measured - keepTail = pick (widest + shift, widestW) freshWidest
            | widest >= 0 && snd freshWidest >= widestW = freshWidest
            | otherwise = Seq.foldlWithIndex (\best i w -> if w > snd best then (i, w) else best) (-1, 0) widths
          pick a b = if snd b > snd a then b else a
      store' <- getStore ctx
      setStore
        ctx
        ( store'
            { storeFloat =
                IM.insert cacheKeyF size $
                  IM.insert cacheKeyH contentH $
                    IM.insert cacheKeyW contentW (storeFloat store')
            , storeDyn =
                IM.insert widthsKey (toDyn (LineWidths size gen widths widest' contentW)) $
                  IM.insert (slotKey slotTextAreaBuffer key) (toDyn (TB.markLinesSeen buf)) (storeDyn store')
            }
        )
      pure (contentW, contentH)

-- | Measured widths of a text area's lines, the font size and metric
-- generation they were measured at, and the widest line with its width.
data LineWidths = LineWidths !Float !Int !(Seq Float) !Int !Float

-- | Node font, field rect and content extent @(width, height)@ of a text area.
-- Zoom changes the node font, so scroll and hit math resolve it here rather
-- than using the base font, or the scroll range would clamp short.
textAreaContentGeom :: Context -> NodeIdx -> IO (FontMetrics, Rect, Float, Float)
textAreaContentGeom ctx idx = do
  fm <- resolveTextAreaFont ctx idx
  (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
  (contentW, contentH) <- textAreaContentMetrics ctx idx
  pure (fm, Rect x y w h, contentW, contentH)

-- | Whether @mouse@ is over one of the text area's shown scrollbars. Uses the
-- cached content extent: this runs on every hover through the cursor query.
isMouseOnTextAreaScrollBarAt :: Context -> NodeIdx -> V2 -> IO Bool
isMouseOnTextAreaScrollBarAt ctx idx mouse = do
  (fm, field, contentW, contentH) <- textAreaContentGeom ctx idx
  wid <- getWidgetId (ctxNodeArena ctx) idx
  store <- getStore ctx
  let (sx, sy) = IM.findWithDefault (0, 0) (slotKey slotTextAreaScroll (intKey wid)) (storePoint store)
  pure (isMouseOnTextAreaScrollBar fm field contentW contentH sx sy mouse)
