{-# LANGUAGE OverloadedStrings #-}

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
  , svgIcon
  , svgIconWith
  , svgIconWith'
  , loadSvg
  , box
  )
where

import Control.Exception (IOException, try)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Effectful (Eff, type (:>))
import NanoUI.Internal.Atlas qualified as Atlas
import NanoUI.Internal.Context (Context (..), askHostIO, registerImage, setHost)
import NanoUI.Internal.Draw (getDrawSnapScale)
import NanoUI.Internal.Layout.Arena (NodeType (..))
import NanoUI.Internal.Monad (Ui, askContext, nextId, uiIO, uiTheme, withContext)
import NanoUI.Svg (Svg, parseSvg, rasterizeSvg, svgKey, svgMonochrome, svgSize)
import NanoUI.Internal.Style
  ( Layout (..)
  , Sizing (..)
  , alignEnd
  , alignMid
  , defaultLayout
  , fillW
  , fontBold
  , fontDanger
  , fontItalic
  , fontMedium
  , fontMono
  , fontMuted
  , fontUnderline
  , gap
  , minW
  , padXY
  , styleFg
  , themePanel
  , tight
  )
import Data.Word (Word32)
import NanoUI.Internal.Types (Color (..), ImageId (..), colorRGBA, colorToWord32)
import NanoUI.Internal.WidgetText (intValueText)
import NanoUI.Internal.Widgets.Layout (labelEx, labelWith, panelWith, row', rowWith)
import NanoUI.Internal.Widgets.Node (Response, addWidget, addWidgetStyled)

-- | Medium-weight label without padding. Uses the current font size.
heading :: Ui :> es => Text -> Eff es ()
heading = labelWith (tight . fontMedium)

-- | Full-width label in the theme's muted colour.
muted :: Ui :> es => Text -> Eff es ()
muted = labelWith (fillW . fontMuted)

-- | Label using the backend's monospace font variant.
mono :: Ui :> es => Text -> Eff es ()
mono = labelWith fontMono

-- | Full-width label in the theme's danger colour.
danger :: Ui :> es => Text -> Eff es ()
danger = labelWith (fillW . fontDanger)

-- | Label requesting bold weight.
bold :: Ui :> es => Text -> Eff es ()
bold = labelWith fontBold

-- | Label requesting italic styling.
italic :: Ui :> es => Text -> Eff es ()
italic = labelWith fontItalic

-- | Label with an underline.
underline :: Ui :> es => Text -> Eff es ()
underline = labelWith fontUnderline

-- | Key/value row: a muted key on the left, the value right-aligned. Trailing
-- whitespace in the value is dropped.
kv :: Ui :> es => Text -> Text -> Eff es ()
kv k v =
  row' (tight . gap 12 . alignMid . fillW $ defaultLayout) $ do
    void (labelEx (fontMuted . tight . minW 88 $ defaultLayout) k)
    void (labelEx (tight . fillW . alignEnd $ defaultLayout) (T.stripEnd v))

-- | Key/value row with a monospace value.
kvMono :: Ui :> es => Text -> Text -> Eff es ()
kvMono k v =
  row' (tight . gap 12 . alignMid . fillW $ defaultLayout) $ do
    void (labelEx (tight . minW 88 $ defaultLayout) k)
    void (labelEx (tight . fillW . alignEnd . fontMono $ defaultLayout) (T.stripEnd v))

-- | Key/value pairs as one monospace block with the keys padded to a column.
kvBlock :: (Foldable f, Ui :> es) => f (Text, Text) -> Eff es ()
kvBlock rows =
  let maxK = foldl' (\acc (k, _) -> max acc (T.length k)) 0 rows
      padK k = T.justifyLeft maxK ' ' k
   in void $
        labelEx
          (tight . gap 0 . fontMono $ defaultLayout)
          (T.concat (foldr (\(k, v) rest -> padK k : "  " : v : "\n" : rest) [] rows))

-- | Full-width panel with a 300-pixel minimum width, 12x10 padding, and 8-pixel gap.
card :: Ui :> es => Eff es a -> Eff es a
card = panelWith (minW 300 . padXY 12 10 . gap 8 . fillW)

-- | Full-width row with no padding, an 8-pixel gap, and vertical centring.
toolbar :: Ui :> es => Eff es a -> Eff es a
toolbar = rowWith (tight . gap 8 . alignMid . fillW)

-- | An image registered with the host, sized by the layout modifier.
image :: Ui :> es => (Layout -> Layout) -> ImageId -> Eff es ()
image f iid = void (image' f iid)

-- | 'image' with its 'Response', for example to 'NanoUI.keepAnimating' an
-- image whose id changes over time.
image' :: Ui :> es => (Layout -> Layout) -> ImageId -> Eff es Response
image' f (ImageId tid) = do
  wid <- nextId
  let
    stored = if tid <= 0 then T.empty else intValueText tid
  addWidget wid NodeImage stored 0 (f defaultLayout)

-- | An image id that no registered image uses and no earlier call returned.
-- Take one for each image registered while the app runs.
freshImageId :: Ui :> es => Eff es ImageId
freshImageId = withContext (\ctx -> Atlas.freshImageId (ctxImageAtlas ctx))

-- | Register an RGBA image (4 bytes a pixel, rows top to bottom) under an id
-- while the app runs, for 'image' to draw. Returns 'False' when the size or
-- pixels are invalid, an image of another size already has the id, or the
-- atlas is full. An image of the same size is replaced.
registerImageRgba :: Ui :> es => ImageId -> Int -> Int -> ByteString -> Eff es Bool
registerImageRgba iid w h pixels = withContext (\ctx -> registerImage ctx iid w h pixels)

-- | Read and parse an SVG file.
loadSvg :: FilePath -> IO (Either String Svg)
loadSvg path = do
  result <- try (BS.readFile path)
  pure $ case result of
    Left (err :: IOException) -> Left (show err)
    Right bytes -> parseSvg bytes

-- | An SVG icon @size@ logical pixels square, drawn in the text colour where
-- it is used: a one-colour document (every paint @currentColor@ or
-- unspecified) takes the colour as a tint, and a multicoloured one paints
-- its @currentColor@ with it.
{-# INLINE svgIcon #-}
svgIcon :: Ui :> es => Float -> Svg -> Eff es ()
svgIcon size = svgIconWith (fixedSquare size)
  where
    fixedSquare n l = l {layoutWidth = Fixed n, layoutHeight = Fixed n}

-- | An SVG document sized by the layout modifier: a fixed width and height,
-- or else the document's own size. A 'NanoUI.fontColor' in the modifier
-- replaces the text colour.
{-# INLINE svgIconWith #-}
svgIconWith :: Ui :> es => (Layout -> Layout) -> Svg -> Eff es ()
svgIconWith f doc = void (svgIconWith' f doc)

-- | The document is rasterized once per pixel size and colour, at the
-- display's scale, and kept in the image atlas for as long as the app runs.
svgIconWith' :: Ui :> es => (Layout -> Layout) -> Svg -> Eff es Response
svgIconWith' f doc = do
  ctx <- askContext
  theme <- uiTheme
  let lay0 = f defaultLayout
      (docW, docH) = svgSize doc
      fixedOr sizing dflt = case sizing of
        Fixed n -> n
        _ -> dflt
      w = fixedOr (layoutWidth lay0) docW
      h = fixedOr (layoutHeight lay0) docH
      color = maybe (styleFg (themePanel theme)) id (layoutFontColor lay0)
      oneColour = svgMonochrome doc
      white = colorRGBA 255 255 255 255
      lay = lay0 {layoutWidth = Fixed w, layoutHeight = Fixed h, layoutFontColor = Just (if oneColour then color else white)}
  iid <- uiIO $ do
    scale <- getDrawSnapScale (ctxDrawArena ctx)
    let pw = max 1 (ceiling (w * max 1 scale))
        ph = max 1 (ceiling (h * max 1 scale))
        -- A one-colour raster is white and tinted when drawn, so every colour
        -- shares it.
        rasterColor = if oneColour then white else color
        key = (svgKey doc, pw, ph, colorToWord32 rasterColor)
    cache <- svgRasterCache ctx
    known <- Map.lookup key <$> readIORef cache
    case known of
      Just iid -> pure iid
      Nothing -> do
        iid <- Atlas.freshImageId (ctxImageAtlas ctx)
        ok <- registerImage ctx iid pw ph (rasterizeSvg pw ph rasterColor doc)
        if ok
          then atomicModifyIORef' cache (\m -> (Map.insert key iid m, ()))
          else pure ()
        pure (if ok then iid else ImageId 0)
  image' (const lay) iid

-- | Rasterized SVG documents by document, pixel size and colour.
newtype SvgRasters = SvgRasters (IORef (Map.Map (Int, Int, Int, Word32) ImageId))

svgRasterCache :: Context -> IO (IORef (Map.Map (Int, Int, Int, Word32) ImageId))
svgRasterCache ctx =
  askHostIO ctx >>= \case
    Just (SvgRasters ref) -> pure ref
    Nothing -> do
      ref <- newIORef Map.empty
      setHost ctx (SvgRasters ref)
      pure ref

-- | A solid rectangle sized by the layout modifier.
box :: Ui :> es => (Layout -> Layout) -> Color -> Eff es ()
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
