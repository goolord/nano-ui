-- | Render a whole painted frame of any view to an SVG document.
--
-- Paint lowers every widget straight to the vertex buffer. With
-- 'setDrawRecordOps' on, the shape emitters also record each solid shape they
-- draw as the 'DrawOp' that draws it, under its layer and clip; this module
-- renders those through "NanoUI.Draw.Svg", along with the vector drawings in
-- the frame's draw-op caches and the text from 'collectRasterSpans'.
--
-- Call 'enableSvgExport' on the context before running frames. It records
-- the shapes, moves all text (including text inside drawings) onto the
-- raster spans consumed here, and draws rounded shapes round. Atlas images
-- are omitted (the atlas has no URI); 'DrawOp' images inside drawings
-- resolve through 'fsImageUri'.
--
-- > enableSvgExport ctx
-- > (_, dd, _) <- runFrame ctx inp view
-- > svg <- frameToSvg defaultFrameSvgConfig ctx inp dd
--
-- Text is fitted to the widths the context measured. A context from
-- 'NanoUI.Testing.newPixelContext' measures in a 16 px monospace grid, so
-- for text that reads like the app's, run the frames inside a backend
-- session (@withSdl@) that measures with the real font.
module NanoUI.Draw.FrameSvg
  ( FrameSvgConfig (..)
  , defaultFrameSvgConfig
  , enableSvgExport
  , frameToSvg
  )
where

import Control.Monad (forM, unless)
import Data.Bits (xor)
import Data.Foldable (toList)
import Data.IORef (readIORef)
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.List (find, partition, sortOn)
import Data.Map.Strict qualified as M
import Data.Primitive.PrimArray (PrimArray, indexPrimArray, sizeofPrimArray)
import Data.Primitive.SmallArray (SmallArray, smallArrayFromList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Word (Word32)
import Numeric (showHex)
import NanoUI
  ( Color (..)
  , DrawOp (..)
  , FontVariant (..)
  , Input (..)
  , Rect (..)
  , Size (..)
  , colorA
  , rectIntersect
  )
import NanoUI.Backend
  ( DrawData (..)
  , FontMetrics (..)
  , Layer (..)
  , collectRasterSpans
  )
import NanoUI.Draw.Svg (SvgConfig (..), defaultSvgConfig, renderDrawOpsSvg)
import NanoUI.Draw.Svg.Attr
  ( attr
  , escapeXml
  , fillAttrs
  , fmtNum
  , fontStyleAttrs
  , numAttr
  , rectEl
  )
import NanoUI.Internal.Context
  ( Context (..)
  , DrawingCacheState (..)
  , RecordedOp (..)
  , drawRecordedOps
  , getDrawRecordOps
  , intKey
  , setDrawExternalText
  , setDrawRecordOps
  , setDrawSquareGeometry
  )
import NanoUI.Internal.Context.Types
  ( CustomDrawOpCacheEntry (..)
  , DrawOpCacheEntry (..)
  , SpanCacheEntry (..)
  )
import NanoUI.Internal.Layout.Arena
  ( NodeType (..)
  , arenaCount
  , getNodeRect
  , getNodeType
  , getStyleIdx
  , getWidgetId
  , hasCenteredLabel
  , nodeFontBits
  )
import NanoUI.Testing
  ( textNodeFontStyle
  , textNodeFontVariant
  , textNodeFontWeight
  , textNodeTextDecoration
  )

-- | Options for 'frameToSvg'.
data FrameSvgConfig = FrameSvgConfig
  { fsPrecision :: !Int
  -- ^ Maximum decimal places for coordinates and lengths (clamped to
  -- @0..6@). One or two digits suits a whole window.
  , fsFontFamily :: !Text
  -- ^ The @font-family@ spans render in. Text is fitted to its measured
  -- width with @textLength@, so any family that lays out similarly to the
  -- context's font reads correctly.
  , fsImageUri :: !(Int -> Maybe Text)
  -- ^ As 'svgImageUri': resolves a drawing's image id to an @href@ or data
  -- URI. Images without a resolution are omitted.
  , fsIdPrefix :: !Text
  -- ^ Prepended to every clip and gradient id in the document, so several
  -- documents inlined in one page keep their ids apart. Empty, the default,
  -- takes a prefix from a hash of the document, which differs between
  -- documents that differ.
  }

-- | One decimal place, a sans-serif stack, no image resolver, and an id
-- prefix from the document's hash.
defaultFrameSvgConfig :: FrameSvgConfig
defaultFrameSvgConfig =
  FrameSvgConfig
    { fsPrecision = 1
    , fsFontFamily = "Inter, 'Segoe UI', system-ui, sans-serif"
    , fsImageUri = const Nothing
    , fsIdPrefix = T.empty
    }

-- | Render the frame 'runFrame' returned for @inp@ into a standalone SVG
-- document sized to the input's window. Text draws last, over the shapes
-- and vector drawings beneath it, in paint order for each.
--
-- Fails if the context was not recording shapes ('enableSvgExport'), which
-- would export the text alone.
frameToSvg :: FrameSvgConfig -> Context -> Input -> DrawData -> IO Text
frameToSvg cfg0 ctx inp dd = do
  recording <- getDrawRecordOps ctx
  unless recording $
    ioError (userError "frameToSvg: the context does not record shapes; call enableSvgExport before running the frame")
  let
    autoPrefix = T.null (fsIdPrefix cfg0)
    -- An automatic prefix is filled in once the document is known. The
    -- placeholder holds a character that text and attribute values always
    -- carry escaped, so it can only be one of the ids.
    cfg = if autoPrefix then cfg0 {fsIdPrefix = idPlaceholder} else cfg0
  (pageSpans, overlaySpans) <- collectRasterSpans ctx inp
  spanStyles <- collectSpanStyles ctx
  live <- liveDrawingIds ctx
  dc <- readIORef (ctxDrawingCache ctx)
  recorded <- drawRecordedOps ctx
  let
    cached =
      [ (doeBounds e, doeOps e)
      | (k, e) <- IM.toList (dcsDrawOpCache dc)
      , IS.member k live
      ]
        ++ [ (cdeBounds e, cdeOps e)
           | (k, e) <- IM.toList (dcsCustomDrawOpCache dc)
           , IS.member k live
           ]
  let
    (pageShapes, overlayShapes) = frameShapes (map fst cached) recorded
    Size vw vh = inputWindowSize inp
    p = max 0 (min 6 (fsPrecision cfg))
    fm = ctxFontMetrics ctx
    sizeRatio = ctxFontSize ctx / fmLineHeight fm
    vp = Rect 0 0 vw vh
    -- Each render numbers its ids from 1, so each has its own prefix.
    render scope ops
      | null ops = mempty
      | otherwise =
          TB.fromText . renderDrawOpsSvg (fragCfg scope) vp $ smallArrayFromList ops
    fragCfg scope =
      defaultSvgConfig
        { svgStandalone = False
        , svgPrecision = p
        , svgImageUri = fsImageUri cfg
        , svgIdPrefix = fsIdPrefix cfg <> scope
        }
    frags =
      mconcat
        [ render ("d" <> T.pack (show k) <> "-") (vectorOnly ops)
        | (k, (_, ops)) <- zip [1 :: Int ..] cached
        ]
    tclip = TB.fromText (fsIdPrefix cfg) <> "tclip_"
    (pageTextDefs, pageTextBody, nextTClip) = spanTexts p (fsFontFamily cfg) fm sizeRatio spanStyles tclip 1 pageSpans
    (overlayTextDefs, overlayTextBody, _) = spanTexts p (fsFontFamily cfg) fm sizeRatio spanStyles tclip nextTClip overlaySpans
    allDefs
      | nullDefs = mempty
      | otherwise = "<defs>" <> pageTextDefs <> overlayTextDefs <> "</defs>"
    nullDefs = TL.null (TB.toLazyText pageTextDefs) && TL.null (TB.toLazyText overlayTextDefs)
    doc =
      "<svg xmlns=\"http://www.w3.org/2000/svg\""
        <> numAttr 0 "width" vw
        <> numAttr 0 "height" vh
        <> attr "viewBox" ("0 0 " <> fmtNum 0 vw <> " " <> fmtNum 0 vh)
        <> ">"
        <> allDefs
        <> solidRect p vp (drawClear dd)
        <> render "s" pageShapes
        <> frags
        <> pageTextBody
        <> render "o" overlayShapes
        <> overlayTextBody
        <> "</svg>"
    out = TL.toStrict (TB.toLazyText doc)
  pure (if autoPrefix then T.replace idPlaceholder (documentIdPrefix out) out else out)

-- | Turn on what 'frameToSvg' reads from a context: shape recording
-- ('setDrawRecordOps'), text as spans ('setDrawExternalText'), and rounded
-- geometry ('setDrawSquareGeometry' off). Call it before running the frames
-- to export.
enableSvgExport :: Context -> IO ()
enableSvgExport ctx = do
  setDrawRecordOps ctx True
  setDrawExternalText ctx True
  setDrawSquareGeometry ctx False

-- | Stands for the automatic id prefix while the document is built.
idPlaceholder :: Text
idPlaceholder = "<id>"

-- | An id prefix from an FNV-1a hash of a document, as eight hex digits.
documentIdPrefix :: Text -> Text
documentIdPrefix t =
  let h = T.foldl' (\acc c -> (acc `xor` fromIntegral (fromEnum c)) * 16777619) (2166136261 :: Word32) t
      hex = showHex h ""
   in "n" <> T.pack (replicate (8 - length hex) '0' <> hex) <> "-"

-- | The widget ids the view declared this frame, so drawings left in the
-- caches by widgets no longer shown (another tab's charts) are skipped.
liveDrawingIds :: Context -> IO IS.IntSet
liveDrawingIds ctx = do
  let
    na = ctxNodeArena ctx
  n <- arenaCount na
  IS.fromList <$> mapM (\i -> intKey <$> getWidgetId na i) [0 .. n - 1]

-- | A drawing's ops re-rendered as vectors; its text arrives as a span
-- instead, and its images resolve through the config.
vectorOnly :: SmallArray DrawOp -> [DrawOp]
vectorOnly = filter keep . toList
 where
  keep DrawText {} = False
  keep DrawTextStyled {} = False
  keep DrawTextAligned {} = False
  keep DrawImage {} = False
  keep _ = True

--------------------------------------------------------------------------------
-- Shapes the paint recorded
--------------------------------------------------------------------------------

-- | The recorded shapes in paint order (layer by layer, as the commands
-- run), split into page shapes ('LayerBackground', 'LayerContent') and
-- overlay shapes ('LayerOverlay', 'LayerChrome'). Each is cut to its clip,
-- or put in a clip group when it overhangs it. Shapes inside @skip@ (the
-- drawing widgets' bounds) are left out: those render from their draw ops
-- instead.
frameShapes :: [Rect] -> [RecordedOp] -> ([DrawOp], [DrawOp])
frameShapes skip recorded = (clipRuns page, clipRuns overlay)
 where
  shown =
    sortOn (\(RecordedOp layer _ _) -> layer)
      [ ro
      | ro@(RecordedOp _ _ op) <- recorded
      , maybe True (\b -> not (any (fullyInside b) skip)) (opBounds op)
      ]
  (page, overlay) = partition (\(RecordedOp layer _ _) -> layer < LayerOverlay) shown

-- | Ops cut to their clips: a rect (an upright or level stroke among them)
-- is cut to it, a shape inside it stands alone, one outside it is dropped,
-- and a run of shapes overhanging one clip share a 'PushClip' group.
clipRuns :: [RecordedOp] -> [DrawOp]
clipRuns = go . concatMap cut
 where
  cut (RecordedOp _ clip op) = case asRect op of
    FillRect r col -> [(Nothing, FillRect r' col) | Just r' <- [rectIntersect clip r]]
    _ -> case opBounds op of
      Just b
        | rectIntersect clip b == Nothing -> []
        | fullyInside b clip -> [(Nothing, op)]
      _ -> [(Just clip, op)]
  go [] = []
  go ((Nothing, op) : rest) = op : go rest
  go ((Just c, op) : rest) =
    let (same, rest') = span ((== Just c) . fst) rest
     in PushClip c : op : map snd same ++ PopClip : go rest'

-- | A straight stroke along an axis as the rect it fills; any other op as
-- it is.
asRect :: DrawOp -> DrawOp
asRect op = case op of
  Stroke x0 y0 x1 y1 bw col -> segment x0 y0 x1 y1 bw col
  StrokeLineAA x0 y0 x1 y1 bw col -> segment x0 y0 x1 y1 bw col
  _ -> op
 where
  segment x0 y0 x1 y1 bw col
    | x0 == x1 = FillRect (Rect (x0 - bw / 2) (min y0 y1) bw (abs (y1 - y0))) col
    | y0 == y1 = FillRect (Rect (min x0 x1) (y0 - bw / 2) (abs (x1 - x0)) bw) col
    | otherwise = op

-- | The box an op draws within, for the ops the emitters record.
opBounds :: DrawOp -> Maybe Rect
opBounds = \case
  FillRect r _ -> Just r
  FillRoundedRect r _ _ -> Just r
  StrokeRoundedRect r _ _ _ -> Just r
  FillQuadGradient r _ _ _ _ -> Just r
  Stroke x0 y0 x1 y1 bw _ -> Just (segmentBox x0 y0 x1 y1 bw)
  StrokeLineAA x0 y0 x1 y1 bw _ -> Just (segmentBox x0 y0 x1 y1 bw)
  FillPolygon pts _ _ _ -> pointsBox 0 pts
  StrokePolyline pts w _ _ _ _ _ -> pointsBox (w / 2) pts
  _ -> Nothing
 where
  segmentBox x0 y0 x1 y1 bw =
    let hw = bw / 2 in Rect (min x0 x1 - hw) (min y0 y1 - hw) (abs (x1 - x0) + bw) (abs (y1 - y0) + bw)

-- | The box around x/y pairs, grown by @pad@ on every side.
pointsBox :: Float -> PrimArray Float -> Maybe Rect
pointsBox pad pts
  | null xs = Nothing
  | otherwise = Just (Rect (minimum xs - pad) (minimum ys - pad) (maximum xs - minimum xs + 2 * pad) (maximum ys - minimum ys + 2 * pad))
 where
  n = sizeofPrimArray pts `div` 2
  xs = [indexPrimArray pts (2 * i) | i <- [0 .. n - 1]]
  ys = [indexPrimArray pts (2 * i + 1) | i <- [0 .. n - 1]]

--------------------------------------------------------------------------------
-- Text from the raster spans
--------------------------------------------------------------------------------

-- | Packed font-style codes for 'NodeText' spans, keyed by their exact
-- rects, and for text fields, buttons and selects, whose spans lie inside
-- them ('nodeFontBits').
collectSpanStyles :: Context -> IO (M.Map RectKey Int, [(Rect, Int)])
collectSpanStyles ctx = do
  sc <- readIORef (ctxSpanCache ctx)
  let
    textStyles =
      M.fromList
        [ (rectKey r, sceStyle e)
        | e <- IM.elems sc
        , sceStyle e /= 0
        , (r, _, _, _) <- sceSpans e
        ]
    na = ctxNodeArena ctx
  n <- arenaCount na
  inputStyles <-
    forM [0 .. n - 1] $ \i -> do
      nt <- getNodeType na i
      if nt == NodeTextInput || hasCenteredLabel nt
        then do
          si <- nodeFontBits nt <$> getStyleIdx na i
          if si /= 0
            then Just . (,si) <$> getNodeRect na i
            else pure Nothing
        else pure Nothing
  pure (textStyles, [st | Just st <- inputStyles])


-- | Every span as clipped group and fitted text. Spans whose line box is
-- not fully inside their clip share one @clipPath@ per clip.
spanTexts ::
  Int -> Text -> FontMetrics -> Float -> (M.Map RectKey Int, [(Rect, Int)]) -> TB.Builder -> Int -> [(Rect, Text, Color, Color, Rect)] -> (TB.Builder, TB.Builder, Int)
spanTexts p fontFam fm ratio (textStyles, inputStyles) clipPrefix startId spans = (defs, body, nextId)
 where
  baseLineH = fmLineHeight fm
  spaceAdv = fmAdvance fm ' '
  lookupStyle r0 = case M.lookup (rectKey r0) textStyles of
    Just si -> si
    Nothing -> maybe 0 snd (find (fullyInside r0 . fst) inputStyles)
  fontAttrs si =
    attr "font-family" (TB.fromText fam)
      <> fontStyleAttrs (textNodeFontWeight si) (textNodeFontStyle si) (textNodeTextDecoration si)
   where
    fam = case textNodeFontVariant si of
      FontMono -> "'Cascadia Mono', 'JetBrains Mono', monospace"
      _ -> fontFam
  trimSpan (Rect x y w h) t0 =
    let
      lead = T.length (T.takeWhile (== ' ') t0)
      trail = T.length (T.takeWhileEnd (== ' ') t0)
      t = T.strip t0
      spW = if baseLineH > 0 then spaceAdv * (h / baseLineH) else spaceAdv
      x' = x + fromIntegral lead * spW
      w' = max 1 (w - fromIntegral (lead + trail) * spW)
    in
      (Rect x' y w' h, t)
  kept =
    [ (r', t', fg, c, lookupStyle r)
    | (r, t, fg, _, c) <- spans
    , let (r', t') = trimSpan r t
    , not (T.null t')
    , rectW r > 0.5
    , rectH r > 0.5
    , rectIntersect r c /= Nothing
    ]
  (defs, wrapClip, nextId) = clipDefs p clipPrefix startId [c | (r, _, _, c, _) <- kept, not (fullyInside r c)]
  body = mconcat (map el kept)
  el (r@(Rect x y w h), t, fg, c, si)
    | fullyInside r c = textEl
    | otherwise = wrapClip c textEl
   where
    textEl =
      "<text"
        <> numAttr p "x" x
        <> numAttr p "y" (y + h / 2)
        <> attr "dominant-baseline" "central"
        <> numAttr p "font-size" (h * ratio)
        <> fontAttrs si
        <> attr "xml:space" "preserve"
        <> fillAttrs p fg
        <> numAttr p "textLength" (max 1 w)
        <> attr "lengthAdjust" "spacing"
        <> ">"
        <> escapeXml t
        <> "</text>"

--------------------------------------------------------------------------------
-- SVG fragments
--------------------------------------------------------------------------------

-- | A rect's fields, as an ordered key.
type RectKey = (Float, Float, Float, Float)

rectKey :: Rect -> RectKey
rectKey (Rect x y w h) = (x, y, w, h)

-- | One @clipPath@ per distinct clip, numbered from @startId@ under @prefix@
-- in order of first use; a wrapper putting a fragment in its clip's group;
-- and the next free number.
clipDefs :: Int -> TB.Builder -> Int -> [Rect] -> (TB.Builder, Rect -> TB.Builder -> TB.Builder, Int)
clipDefs p prefix startId clips = (defs, wrap, startId + M.size ids)
 where
  (ids, ordered) = foldl' assign (M.empty, []) clips
  assign acc@(m, os) c
    | M.member (rectKey c) m = acc
    | otherwise = let i = startId + M.size m in (M.insert (rectKey c) i m, (c, i) : os)
  clipRef i = prefix <> TB.fromString (show i)
  defs =
    mconcat
      [ "<clipPath id=\"" <> clipRef i <> "\"><rect"
          <> numAttr p "x" (rectX c)
          <> numAttr p "y" (rectY c)
          <> numAttr p "width" (rectW c)
          <> numAttr p "height" (rectH c)
          <> "/></clipPath>"
      | (c, i) <- reverse ordered
      ]
  wrap c b =
    "<g clip-path=\"url(#" <> clipRef (M.findWithDefault 0 (rectKey c) ids) <> ")\">" <> b <> "</g>"

-- | A filled rect, or nothing for a transparent or empty one.
solidRect :: Int -> Rect -> Color -> TB.Builder
solidRect p r@(Rect _ _ w h) col
  | colorA col == 0 || w <= 0.01 || h <= 0.01 = mempty
  | otherwise = rectEl p r col

fullyInside :: Rect -> Rect -> Bool
fullyInside (Rect x y w h) (Rect sx sy sw sh) =
  x >= sx && y >= sy && x + w <= sx + sw && y + h <= sy + sh
