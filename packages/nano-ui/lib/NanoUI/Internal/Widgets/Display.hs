-- | Display helpers: styled labels, key/value rows, cards, toolbars, images
-- and colour boxes.
module NanoUI.Internal.Widgets.Display
  ( heading
  , muted
  , mono
  , danger
  , bold
  , italic
  , underline
  , kv
  , kvMono
  , kvBlock
  , card
  , toolbar
  , image
  , image'
  , freshImageId
  , registerImageRgba
  , releaseImageRgba
  , svgIcon
  , svgIconWith
  , svgIconWith'
  , svgIconConfigured
  , svgIconConfigured'
  , loadSvg
  , box
  )
where

import Control.Exception (IOException, try)
import Control.Monad (filterM, void)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef (readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Primitive.PrimVar (newPrimVar, readPrimVar, writePrimVar)
import Data.Text (Text)
import Data.Text qualified as T
import NanoUI.Internal.Atlas qualified as Atlas
import NanoUI.Internal.Context (Context (..), frameNumber, registerImage, releaseImage)
import NanoUI.Internal.Draw (getDrawSnapScale)
import NanoUI.Internal.Layout.Arena (NodeType (..))
import NanoUI.Internal.Monad (NanoUI, nextId, uiTheme, withContext)
import NanoUI.Svg (Svg, parseSvg, rasterizeSvgIn, svgKey, svgMonochrome, svgSize)
import NanoUI.Internal.Style
import NanoUI.Internal.Image (ContentFit (..), ImageLook (..), fitRect, imageLook, turnedRect)
import NanoUI.Internal.Types (Color (..), ImageId (..), Rect (..), colorRGBA, colorToWord32)
import NanoUI.Internal.Widgets.Layout (labelEx, labelWith, panelWith, row', rowWith)
import NanoUI.Internal.Widgets.Image (ImageConfig (..), defaultImageConfig, imageConfigured', imageNode)
import NanoUI.Internal.Widgets.Node (Response, addWidgetStyled)

-- | Medium-weight label without padding. Uses the current font size.
heading :: Text -> NanoUI ()
heading = labelWith (tight . fontMedium)

-- | Full-width label in the theme's muted colour.
muted :: Text -> NanoUI ()
muted = labelWith (fillW . fontMuted)

-- | Label using the backend's monospace font variant.
mono :: Text -> NanoUI ()
mono = labelWith fontMono

-- | Full-width label in the theme's danger colour.
danger :: Text -> NanoUI ()
danger = labelWith (fillW . fontDanger)

-- | Label requesting bold weight.
bold :: Text -> NanoUI ()
bold = labelWith fontBold

-- | Label requesting italic styling.
italic :: Text -> NanoUI ()
italic = labelWith fontItalic

-- | Label with an underline.
underline :: Text -> NanoUI ()
underline = labelWith fontUnderline

-- | Key/value row: a muted key on the left, the value right-aligned. Trailing
-- whitespace in the value is dropped.
kv :: Text -> Text -> NanoUI ()
kv = kvRow fontMuted id

-- | Key/value row with a monospace value.
kvMono :: Text -> Text -> NanoUI ()
kvMono = kvRow id fontMono

kvRow :: (Layout -> Layout) -> (Layout -> Layout) -> Text -> Text -> NanoUI ()
kvRow keyF valF k v =
  row' (tight . gap 12 . alignMid . fillW $ defaultLayout) $ do
    void (labelEx (keyF . tight . minW 88 $ defaultLayout) k)
    void (labelEx (tight . fillW . alignEnd . valF $ defaultLayout) (T.stripEnd v))

-- | Key/value pairs as one monospace block with the keys padded to a column.
kvBlock :: Foldable f => f (Text, Text) -> NanoUI ()
kvBlock rows =
  let maxK = foldl' (\acc (k, _) -> max acc (T.length k)) 0 rows
      padK k = T.justifyLeft maxK ' ' k
   in void $
        labelEx
          (tight . gap 0 . fontMono $ defaultLayout)
          (T.concat (foldr (\(k, v) rest -> padK k : "  " : v : "\n" : rest) [] rows))

-- | Full-width panel with a 300-pixel minimum width, 12x10 padding, and 8-pixel gap.
card :: NanoUI a -> NanoUI a
card = panelWith (minW 300 . padXY 12 10 . gap 8 . fillW)

-- | Full-width row with no padding, an 8-pixel gap, and vertical centring.
toolbar :: NanoUI a -> NanoUI a
toolbar = rowWith (tight . gap 8 . alignMid . fillW)

-- | An image registered with the host, stretched to the rect the layout
-- modifier gives it; an unsized axis is 32 pixels. 'NanoUI.imageConfigured'
-- instead uses the image's own size and can fit, align, crop, fade and
-- rotate it.
image :: (Layout -> Layout) -> ImageId -> NanoUI ()
image f iid = void (image' f iid)

-- | 'image' with its 'Response', for example to 'NanoUI.keepAnimating' an
-- image whose id changes over time.
image' :: (Layout -> Layout) -> ImageId -> NanoUI Response
image' f iid = nextId >>= \wid -> imageNode wid Nothing (f defaultLayout) iid

-- | An image id that no registered image uses and no earlier call returned.
-- Take one for each image registered while the app runs.
freshImageId :: NanoUI ImageId
freshImageId = withContext (\ctx -> Atlas.freshImageId (ctxImageAtlas ctx))

-- | Register an RGBA image (4 bytes a pixel, rows top to bottom) under an id
-- while the app runs, for 'image' to draw. Returns 'False' when the size or
-- pixels are invalid, an image of another size already has the id, or the
-- atlas is full. An image of the same size is replaced.
registerImageRgba :: ImageId -> Int -> Int -> ByteString -> NanoUI Bool
registerImageRgba iid w h pixels = withContext (\ctx -> registerImage ctx iid w h pixels)

-- | Remove an image 'registerImageRgba' registered and free its atlas space
-- for later images. Anything still drawing the id shows the placeholder for
-- an unknown image.
releaseImageRgba :: ImageId -> NanoUI ()
releaseImageRgba iid = withContext (\ctx -> releaseImage ctx iid)

-- | Read and parse an SVG file.
loadSvg :: FilePath -> IO (Either String Svg)
loadSvg path =
  either (\(err :: IOException) -> Left (show err)) parseSvg <$> try (BS.readFile path)

-- | An SVG icon @size@ logical pixels square, drawn in the text colour where
-- it is used: a one-colour document (every paint @currentColor@ or
-- unspecified) takes the colour as a tint, and a multicoloured one paints
-- its @currentColor@ with it.
{-# INLINE svgIcon #-}
svgIcon :: Float -> Svg -> NanoUI ()
svgIcon size = svgIconWith (fixedWH size size)

-- | An SVG document sized by the layout modifier: a fixed width and height,
-- or else the document's own size. A 'NanoUI.fontColor' in the modifier
-- replaces the text colour.
{-# INLINE svgIconWith #-}
svgIconWith :: (Layout -> Layout) -> Svg -> NanoUI ()
svgIconWith f doc = void (svgIconWith' f doc)

-- | The document is rasterized once per pixel size and colour, at the
-- display's scale, and kept in the image atlas while frames draw it at that
-- size and colour; the first frame that does not frees its space.
-- A rect of another shape than the document's letterboxes it, centred, as
-- SVG's default @xMidYMid meet@ does.
svgIconWith' :: (Layout -> Layout) -> Svg -> NanoUI Response
svgIconWith' f = svgIconConfigured' defaultImageConfig {icLayout = f, icFit = FitContain}

-- | An SVG document drawn like 'NanoUI.imageConfigured' draws an image:
-- faded, rotated, fitted and aligned in its rect. The rect is sized as in
-- 'svgIconWith': a fixed width and height from 'icLayout', or else the
-- document's own size. Fit and alignment place the document's own shape
-- ('svgSize'), so 'FitCover' crops real content and 'FitFill' stretches it;
-- use 'FitContain' for the letterboxing 'svgIconWith' does. The document is
-- rasterized at the fitted size, so it stays sharp. 'icCrop' is in raster
-- pixels.
--
-- > svgIconConfigured defaultImageConfig {icLayout = fixedWH 24 24, icRotation = RotateFloating turn} spinnerIcon
svgIconConfigured :: ImageConfig -> Svg -> NanoUI ()
svgIconConfigured cfg doc = void (svgIconConfigured' cfg doc)

-- | 'svgIconConfigured' with its 'Response'.
svgIconConfigured' :: ImageConfig -> Svg -> NanoUI Response
svgIconConfigured' cfg doc = do
  theme <- uiTheme
  let lay0 = icLayout cfg defaultLayout
      (docW, docH) = svgSize doc
      fixedOr sizing dflt = case sizing of
        Fixed n -> n
        _ -> dflt
      w = fixedOr (layoutWidth lay0) docW
      h = fixedOr (layoutHeight lay0) docH
      color = fromMaybe (styleFg (themePanel theme)) (layoutFontColor lay0)
      oneColour = svgMonochrome doc
      white = colorRGBA 255 255 255 255
      lay = lay0 {layoutWidth = Fixed w, layoutHeight = Fixed h, layoutFontColor = Just (if oneColour then color else white)}
      -- The raster is the rect before any solid rotation, stretched over it
      -- when drawn; the document is fitted inside the raster instead.
      Rect _ _ bw bh = turnedRect FitFill AlignCenter AlignMiddle (lookRotation (imageLook cfg white)) (w, h) (Rect 0 0 w h)
      Rect cx cy cw ch = fitRect (icFit cfg) (icAlignX cfg) (icAlignY cfg) (docW, docH) (Rect 0 0 bw bh)
  iid <- withContext $ \ctx -> do
    scale <- getDrawSnapScale (ctxDrawArena ctx)
    let pw = max 1 (ceiling (bw * max 1 scale))
        ph = max 1 (ceiling (bh * max 1 scale))
        -- A zero-sized rect draws nothing, so its content is empty rather
        -- than infinite.
        per px b = if b > 0 then fromIntegral px / b else 0
        -- The content rect in eighths of a raster pixel: sizes a hair apart
        -- share a raster, and the key compares whole numbers.
        eighths v = round (v * 8) :: Int
        content@(qx, qy, qw, qh) = (eighths (cx * per pw bw), eighths (cy * per ph bh), eighths (cw * per pw bw), eighths (ch * per ph bh))
        -- A one-colour raster is white and tinted when drawn, so every colour
        -- shares it.
        rasterColor = if oneColour then white else color
        key = (svgKey doc, pw, ph, colorToWord32 rasterColor, content)
    let cache = ctxSvgRasters ctx
        atlas = ctxImageAtlas ctx
    frame <- frameNumber ctx
    known <- Map.lookup key <$> readIORef cache
    make <- case known of
      -- A raster the atlas had no room for is made again once an image has
      -- left the atlas.
      Just (ImageId 0, freed, _) -> (/= freed) <$> Atlas.atlasReleases atlas
      Just _ -> pure False
      Nothing -> pure True
    case known of
      Just (iid, _, drawn) | not make -> iid <$ writePrimVar drawn frame
      _ -> do
        -- Rasters no frame drew since the one before give their room to this
        -- one: nothing on screen shows them, so their release needs no
        -- repaint. Until then, an icon hidden and shown again keeps its own.
        rasters <- readIORef cache
        stale <- filterM (\(_, (_, _, drawn)) -> (< frame - 1) <$> readPrimVar drawn) (Map.toList rasters)
        mapM_ (\(_, (old, _, _)) -> Atlas.releaseImage atlas old) stale
        iid <- Atlas.freshImageId atlas
        let placed = Rect (fromIntegral qx / 8) (fromIntegral qy / 8) (fromIntegral qw / 8) (fromIntegral qh / 8)
        ok <- registerImage ctx iid pw ph (rasterizeSvgIn pw ph placed rasterColor doc)
        -- A raster the atlas has no room for is kept too, as id 0, so it is
        -- not made again on every pass while drawn.
        let shown = if ok then iid else ImageId 0
        freed <- Atlas.atlasReleases atlas
        drawn <- newPrimVar frame
        writeIORef cache $! Map.insert key (shown, freed, drawn) (foldr (Map.delete . fst) rasters stale)
        pure shown
  imageConfigured' cfg {icLayout = const lay, icFit = FitFill, icAlignX = AlignCenter, icAlignY = AlignMiddle} iid

-- | A solid rectangle sized by the layout modifier.
box :: (Layout -> Layout) -> Color -> NanoUI ()
box f col = do
  wid <- nextId
  void
    ( addWidgetStyled
        wid
        NodeBox
        T.empty
        0
        (f defaultLayout)
        (fromIntegral (colorToWord32 col))
    )
