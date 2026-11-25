-- | Headless rendering for the glossary: SVG contexts and font metrics, the
-- page assets, frame export and cropping, the probe themes that colour
-- each surface, and the compact encoding of SVG banks.
module Glossary.Render
  ( Assets (..)
  , canvasSlack
  , cropSvg
  , encodeJsonText
  , exportFrame
  , frameInkBounds
  , framePad
  , loadAssets
  , newSvgContext
  , packSvgBank
  , probeAccent
  , probeButton
  , probeInput
  , probePanel
  , probePopup
  , renderStyleBank
  , renderSvg
  , renderSvgClick
  , renderSvgHover
  , renderSvgPress
  , sdlFontCtxRef
  ) where

import Control.Monad (forM)
import Data.Aeson.Micro (ToJSON (..), Value, encode, object, (.=))
import Data.Bits (shiftL, shiftR, testBit, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.IORef (IORef, newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (peekByteOff)
import System.Directory (doesFileExist)
import NanoUI
import NanoUI.Backend (DrawData (..), FontMetrics (..), collectRasterSpans, damageFullNow, monospaceMetrics, vertexSize)
import NanoUI.Draw.FrameSvg (FrameSvgConfig (..), defaultFrameSvgConfig, enableSvgExport, frameToSvg)
import NanoUI.Internal.Context (Context (..))
import NanoUI.Testing (newPixelContext, runFrame, withFontMetrics, withMonoFontMetrics)
import NanoUI.Testing.Harness (clickPair, pressAt, withInputOff)
import NanoUI.Ttf.Internal.Inter (fontInterBytes)
import Paths_nano_ui_svg (getDataFileName)
import System.IO.Unsafe (unsafePerformIO)

-- ---------------------------------------------------------------------------
-- Headless Frame-to-SVG Renderers
-- ---------------------------------------------------------------------------

-- | Holds the live SDL context initialized with the embedded @inter.ttf@
-- ('DefaultFont') during 'main' so each specimen's fresh 'Context' inherits
-- real TrueType/HarfBuzz glyph shaping and line metrics.
{-# NOINLINE sdlFontCtxRef #-}
sdlFontCtxRef :: IORef (Maybe Context)
sdlFontCtxRef = unsafePerformIO (newIORef Nothing)

-- | Fallback proportional metrics if run outside 'withSdl'.
svgProportionalMetrics :: FontMetrics
svgProportionalMetrics = svgMonoMetrics {fmAdvance = charAdvance}
  where
    charAdvance :: Char -> Float
    charAdvance c
      | c == ' ' = 4
      | c `elem` ("ilI1.,:;!|'()[]" :: String) = 4
      | c `elem` ("fjrt-/\"" :: String) = 5
      | c `elem` ("mwMW@%#+=>" :: String) = 10
      | c >= 'A' && c <= 'Z' = 8.5
      | otherwise = 7.5

svgMonoMetrics :: FontMetrics
svgMonoMetrics =
  (monospaceMetrics 8)
    { fmLineHeight = 17
    , fmAscent = 13.5
    , fmSnapScale = 0
    , fmAdvance = \c -> if c == ' ' then 4 else 8
    }

newSvgContext :: IO Context
newSvgContext = do
  ctx0 <- newPixelContext
  mSdl <- readIORef sdlFontCtxRef
  let ctx = case mSdl of
        Just sdlCtx ->
          ctx0
            { ctxFontMetrics = ctxFontMetrics sdlCtx
            , ctxMonoFontMetrics = ctxMonoFontMetrics sdlCtx
            , ctxFontSize = ctxFontSize sdlCtx
            , ctxMeasureText = ctxMeasureText sdlCtx
            , ctxResolveFont = ctxResolveFont sdlCtx
            , ctxResolveMeasure = ctxResolveMeasure sdlCtx
            , ctxMetricSource = ctxMetricSource sdlCtx
            }
        Nothing ->
          withMonoFontMetrics (withFontMetrics ctx0 svgProportionalMetrics) svgMonoMetrics
  ctx <$ enableSvgExport ctx

-- | Pure Base64 encoder for embedding @packages/nano-ui-sdl/data/inter.ttf@
-- as a @data:font/ttf;base64,...@ @\@font-face@ in the HTML glossary.
encodeBase64 :: ByteString -> Text
encodeBase64 bs = T.pack (go 0)
  where
    len = BS.length bs
    alphabet :: ByteString
    alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    b64Char :: Int -> Char
    b64Char idx = toEnum (fromIntegral (BS.index alphabet (idx .&. 0x3F)))
    byteAt :: Int -> Int
    byteAt i = fromIntegral (BS.index bs i :: Word8)
    go :: Int -> String
    go i
      | i >= len = []
      | i + 2 < len =
          let n = (byteAt i `shiftL` 16) + (byteAt (i + 1) `shiftL` 8) + byteAt (i + 2)
           in b64Char (n `shiftR` 18)
                : b64Char (n `shiftR` 12)
                : b64Char (n `shiftR` 6)
                : b64Char n
                : go (i + 3)
      | i + 1 < len =
          let n = (byteAt i `shiftL` 16) + (byteAt (i + 1) `shiftL` 8)
           in [b64Char (n `shiftR` 18), b64Char (n `shiftR` 12), b64Char (n `shiftR` 6), '=']
      | otherwise =
          let n = byteAt i `shiftL` 16
           in [b64Char (n `shiftR` 18), b64Char (n `shiftR` 12), '=', '=']

loadInterFontCss :: IO Text
loadInterFontCss =
  pure $
    "@font-face { font-family: 'Inter'; font-style: normal; font-weight: 100 900; font-display: block; src: url('data:font/ttf;base64,"
      <> encodeBase64 fontInterBytes
      <> "') format('truetype'); }"

-- | Static assets that live beside the source instead of inside it: the
-- stylesheet and script are edited as plain CSS and JavaScript and spliced
-- into the generated document so the result stays self-contained.
data Assets = Assets
  { assetsFontCss :: !Text
  -- ^ @\@font-face@ rule for the embedded Inter face, or empty.
  , assetsCss :: !Text
  -- ^ @assets/glossary.css@.
  , assetsJs :: !Text
  -- ^ @assets/glossary.js@.
  }

-- | Load the embedded font rule and the page's stylesheet and script from the
-- package's data files. The document also embeds every specimen, so the data
-- payload (font + SVG banks) is written inline at generation time.
--
-- Resolution tries the installed data directory first (via @Paths_@) and then
-- a few source-relative paths, so the tool also runs straight from a checkout
-- with @cabal run@ or the built executable.
loadAssets :: IO Assets
loadAssets = do
  fontCss <- loadInterFontCss
  css <- readAsset "glossary.css"
  js <- readAsset "glossary.js"
  pure Assets {assetsFontCss = fontCss, assetsCss = css, assetsJs = js}
  where
    readAsset name = do
      dataPath <- getDataFileName ("assets/" <> T.unpack name)
      let candidates =
            [ dataPath
            , "packages/nano-ui-svg/assets/" <> T.unpack name
            , "../nano-ui-svg/assets/" <> T.unpack name
            , "assets/" <> T.unpack name
            ]
      found <- findFirst candidates
      case found of
        Just path -> T.stripEnd . decodeUtf8 <$> BS.readFile path
        Nothing ->
          ioError . userError $
            "glossary asset not found: " <> T.unpack name <> " (looked in " <> show candidates <> ")"
    findFirst [] = pure Nothing
    findFirst (p : ps) = do
      ok <- doesFileExist p
      if ok then pure (Just p) else findFirst ps

-- | Extra canvas height given to specimens. Widgets can lay out past the
-- nominal height (a tab strip's last row, an overlay's shadow), and the crop
-- recovers it; width is left exact so fill-width demos keep their measure.
canvasSlack :: Float
canvasSlack = 72

-- | Union of a frame's solid geometry (vertex positions) and text line boxes.
-- The full-window backdrop that 'frameToSvg' paints is in neither, so this is
-- the ink a reader sees.
frameInkBounds :: Context -> Input -> DrawData -> IO (Maybe Rect)
frameInkBounds ctx inp dd = do
  verts <- vertexPoints dd
  (pageSpans, overlaySpans) <- collectRasterSpans ctx inp
  let boxes = [Rect x y 0 0 | (x, y) <- verts] <> [r | (r, _, _, _, _) <- pageSpans <> overlaySpans]
  pure $ case boxes of
    (b : rest) -> Just (foldl' rectUnion b rest)
    [] -> Nothing

-- | Every vertex position in the frame's solid-geometry buffer.
vertexPoints :: DrawData -> IO [(Float, Float)]
vertexPoints dd =
  withForeignPtr (drawVertices dd) $ \vp ->
    forM [0 .. drawVertexCount dd - 1] $ \i -> do
      x <- peekByteOff vp (i * vertexSize) :: IO Float
      y <- peekByteOff vp (i * vertexSize + 4) :: IO Float
      pure (x, y)

-- | Crop a rendered SVG to its painted bounds (plus padding), rewriting the
-- root @<svg>@ tag's @width@, @height@, and @viewBox@. Height tightens to the
-- content but never past the canvas (whose backdrop is the frame's only
-- background); width stays at @canvasW@ unless content overhangs it. Scale
-- stays 1:1.
cropSvg :: Float -> Float -> Maybe Rect -> Text -> Text
cropSvg _ _ Nothing svg = svg
cropSvg canvasW canvasH (Just (Rect bx by bw bh)) svg
  | not ("<svg" `T.isPrefixOf` open) || T.null rest = svg
  | otherwise = header <> rest
  where
    (open, rest) = T.breakOn ">" svg
    pad = 8
    minY = max 0 (floor (by - pad) :: Int)
    maxY = min (ceiling canvasH) (ceiling (by + bh + pad))
    h = max 1 (maxY - minY)
    w =
      if bx + bw > canvasW + 0.5
        then ceiling (bx + bw + pad)
        else round canvasW
    header =
      "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\""
        <> tshow w
        <> "\" height=\""
        <> tshow h
        <> "\" viewBox=\"0 "
        <> tshow minY
        <> " "
        <> tshow w
        <> " "
        <> tshow h
        <> "\""
    tshow = T.pack . show

-- | Warm a view over zero or more frames, then run one final frame with
-- 'damageFullNow' and export it as an inline @<svg>@ element whose ids
-- carry @tag@, so the documents on one page never collide.
--
-- Every frame in 'warmInputs' is run once without damage to settle layout
-- and state; the 'captureInput' frame is the one returned.
captureSvg :: Text -> [Input] -> Input -> NanoUI () -> IO Text
captureSvg tag warmInputs captureInput ui = do
  ctx <- newSvgContext
  mapM_ (\inp -> runFrame ctx inp ui) warmInputs
  fst <$> exportFrame tag ctx captureInput ui

-- | Run one frame of @ui@ with 'damageFullNow' and export it with its ids
-- prefixed by @tag@, returning the SVG and the frame's draw data.
exportFrame :: Text -> Context -> Input -> NanoUI () -> IO (Text, DrawData)
exportFrame tag ctx inp ui = do
  (_, dd, _) <- runFrame ctx inp (ui >> damageFullNow)
  svg <- frameToSvg defaultFrameSvgConfig {fsIdPrefix = tag <> "-"} ctx inp dd
  pure (svg, dd)

-- | Render a two-frame warmed-up view with rounded geometry and external text
-- spans, returning a namespaced inline @<svg>@ element.
renderSvg :: Text -> Float -> Float -> NanoUI () -> IO Text
renderSvg tag w h ui = captureSvg tag [inp] inp ui
  where
    inp = withInputOff w h

-- | Render a view with the cursor hovering at @pos@ on the second frame.
renderSvgHover :: Text -> Float -> Float -> V2 -> NanoUI () -> IO Text
renderSvgHover tag w h pos ui = captureSvg tag [base] (base {inputMousePos = pos}) ui
  where
    base = withInputOff w h

-- | Render a view with the left mouse button held down at @pos@.
renderSvgPress :: Text -> Float -> Float -> V2 -> NanoUI () -> IO Text
renderSvgPress tag w h pos ui = captureSvg tag [base] (pressAt base pos) ui
  where
    base = withInputOff w h

-- | Warm up a view, click at @pos@ (press + release frames), and capture the
-- resulting frame (e.g. focused field or open dropdown overlay).
renderSvgClick :: Text -> Float -> Float -> V2 -> NanoUI () -> IO Text
renderSvgClick tag w h pos ui =
  captureSvg tag [base, press, release] base ui
  where
    base = withInputOff w h
    (press, release) = clickPair base pos

-- ---------------------------------------------------------------------------
-- Diagnostic Surface Probe Modifiers
-- ---------------------------------------------------------------------------

-- Distinct high-contrast surface probes used across the matrix so the reader
-- can visually trace which Style record inside Theme paints each pixel.

probeButton :: Theme -> Theme
probeButton =
  buttonStyle
    ( fillColor (colorRGBA 3 105 161 255)
        . foreground (colorRGBA 224 242 254 255)
        . borderColor (colorRGBA 56 189 248 255)
        . borderWidth 2
    )

probeInput :: Theme -> Theme
probeInput =
  inputStyle
    ( fillColor (colorRGBA 4 120 87 255)
        . foreground (colorRGBA 209 250 229 255)
        . borderColor (colorRGBA 52 211 153 255)
        . borderWidth 2
    )

probePanel :: Theme -> Theme
probePanel =
  panelStyle
    ( background (colorRGBA 88 28 135 255)
        . foreground (colorRGBA 243 232 255 255)
        . borderColor (colorRGBA 192 132 252 255)
        . borderWidth 2
    )

probePopup :: Theme -> Theme
probePopup =
  popupStyle
    ( fillColor (colorRGBA 180 83 9 255)
        . foreground (colorRGBA 254 243 199 255)
        . borderColor (colorRGBA 251 191 36 255)
        . borderWidth 2
    )

probeWindow :: Theme -> Theme
probeWindow =
  windowStyle
    ( background (colorRGBA 30 27 75 255)
        . foreground (colorRGBA 224 231 255 255)
        . borderColor (colorRGBA 129 140 248 255)
        . borderWidth 2
    )

probeAccent :: Theme -> Theme
probeAccent =
  accentColor (colorRGBA 225 29 72 255)

-- | Frame wrapper that keeps label raster spans transparent unless panelStyle
-- itself is being tested or a panel is drawn.
framePad :: NanoUI () -> NanoUI ()
framePad body =
  columnWith (fillW . fillH . padAll 8 . gap 6) body

-- ---------------------------------------------------------------------------
-- Compact Lossless SVG Combination Bank Encoder (via microaeson)
-- ---------------------------------------------------------------------------

encodeJsonText :: ToJSON a => a -> Text
encodeJsonText = decodeUtf8 . BSL.toStrict . encode

-- | Split each SVG document by @"><"@ (every XML tag boundary) and deduplicate
-- shared tags across all combinations of a widget into @{"l":[...],"c":[[...],...]}@.
-- In JS, @bank.c[mask].map(i => bank.l[i]).join('><')@ losslessly reconstructs
-- the exact 'frameToSvg' output for any bitmask combination.
packSvgBank :: [Text] -> Value
packSvgBank svgs =
  let step (nextIdx, tagMap, revTags, revCombos) svg =
        let tags = T.splitOn "><" svg
            (nextIdx', tagMap', revTags', idxs) =
              foldl'
                ( \(i, mp, rts, acc) tag ->
                    case Map.lookup tag mp of
                      Just existingIdx -> (i, mp, rts, existingIdx : acc)
                      Nothing -> (i + 1, Map.insert tag i mp, tag : rts, i : acc)
                )
                (nextIdx, tagMap, revTags, [])
                tags
         in (nextIdx', tagMap', revTags', reverse idxs : revCombos)
      (_, _, revUniqueTags, revComboLists) = foldl' step (0 :: Int, Map.empty, [], []) svgs
      uniqueTags = reverse revUniqueTags
      comboLists = reverse revComboLists
   in object
        [ "l" .= uniqueTags
        , "c" .= comboLists
        ]

-- | Apply a 7-bit style combination mask (0..127) to a widget subtree:
--
-- * Bit 0 (1):  'themed' 'defaultLightTheme'
-- * Bit 1 (2):  'styled' 'probeButton' (@buttonStyle@)
-- * Bit 2 (4):  'styled' 'probeInput' (@inputStyle@)
-- * Bit 3 (8):  'styled' 'probePanel' (@panelStyle@, plus @popupStyle@/@windowStyle@ on overlays)
-- * Bit 4 (16): 'styled' 'probeAccent' (@accentColor@)
-- * Bit 5 (32): 'styled' (@everyStyle rounded@) (@cornerRadius 999@)
-- * Bit 6 (64): 'disabledWhen' 'True'
applyStyleMask :: Bool -> Int -> NanoUI () -> NanoUI ()
applyStyleMask isOverlay mask view0 =
  let applyBase =
        if testBit mask 0
          then (setUiTheme defaultLightTheme >>) . themed defaultLightTheme
          else (setUiTheme defaultTheme >>)
      hasThemeMod = (mask .&. 0x3E) /= 0
      pnlMod = if isOverlay then probePanel . probePopup . probeWindow else probePanel
      themeMod =
        (if testBit mask 5 then rounded 999 else id)
          . (if testBit mask 4 then probeAccent else id)
          . (if testBit mask 3 then pnlMod else id)
          . (if testBit mask 2 then probeInput else id)
          . (if testBit mask 1 then probeButton else id)
      applyStyled = if hasThemeMod then styled themeMod else id
      applyDis = if testBit mask 6 then disabledWhen True else id
   in applyBase (applyStyled (applyDis view0))

-- | Render all 128 combinations (0..127) of the 7 interactive style toggles
-- for a widget, returning @(baselineSvg, packedBankJson)@. Every frame is
-- cropped to the bank's shared painted bounds, so toggling a style never
-- resizes the specimen.
renderStyleBank :: Text -> Float -> Float -> Bool -> NanoUI () -> IO (Text, Value)
renderStyleBank bankId w h isOverlay view = do
  ctx <- newSvgContext
  let inp = withInputOff w h
      nsTag = "wb-" <> bankId
  _ <- runFrame ctx inp view
  caps <- forM [0 .. 127] $ \mask -> do
    (svg, dd) <- exportFrame nsTag ctx inp (applyStyleMask isOverlay mask view)
    bounds <- frameInkBounds ctx inp dd
    pure (svg, bounds)
  let boxes = [b | (_, Just b) <- caps]
      cropBox = case boxes of
        (b0 : bs) -> Just (foldl' rectUnion b0 bs)
        [] -> Nothing
      svgs = [cropSvg w h cropBox svg | (svg, _) <- caps]
      baseSvg = case svgs of
        (s0 : _) -> s0
        [] -> ""
  pure (baseSvg, packSvgBank svgs)
