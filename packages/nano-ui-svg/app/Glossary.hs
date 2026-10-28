-- | Generate a self-contained HTML visual glossary of nano-ui styling
-- modifiers, widget surface mappings, and unexpected interactions, with every
-- specimen rendered headlessly by @nano-ui-svg@ ('frameToSvg').
--
-- The page's stylesheet and script live in @assets/glossary.css@ and
-- @assets/glossary.js@ and are spliced into the generated document, so the
-- output stays one portable file while the visual and interactive layers are
-- edited as plain CSS and JavaScript rather than Haskell string literals.
--
-- Run:
--
-- > cabal run -j1 nano-ui-svg-glossary -- [docs/styling-glossary.html]
module Main (main) where

import Control.Monad (forM, void)
import Data.Aeson.Micro (ToJSON (..), Value, encode, object, (.=))
import Data.Bits (shiftL, shiftR, testBit, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy qualified as TL
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (peekByteOff)
import Lucid
import Lucid.Base (makeAttributes)
import NanoUI
import NanoUI.Backend (DrawData (..), FontMetrics (..), collectRasterSpans, damageFullNow, emptyInput, vertexSize)
import NanoUI.Backend.Sdl (NanoUIFont (..), SdlOptions (..), defaultSdlOptions, withSdl)
import NanoUI.Draw.FrameSvg (defaultFrameSvgConfig, frameToSvg)
import NanoUI.Internal.Context (Context (..), setDrawExternalText, setDrawSquareGeometry)
import NanoUI.Testing (newPixelContext, runFrame, withFontMetrics, withMonoFontMetrics)
import Paths_nano_ui_svg (getDataFileName)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (getArgs)
import System.IO (IOMode (WriteMode), hSetEncoding, utf8, withFile)
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
svgProportionalMetrics =
  FontMetrics
    { fmLineHeight = 17
    , fmAscent = 13.5
    , fmSnapScale = 0
    , fmAdvance = charAdvance
    , fmKerning = \_ _ -> 0
    , fmShape = \_ -> Nothing
    , fmGlyph = \_ -> Nothing
    , fmBackend = Nothing
    }
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
  FontMetrics
    { fmLineHeight = 17
    , fmAscent = 13.5
    , fmSnapScale = 0
    , fmAdvance = \c -> if c == ' ' then 4 else 8
    , fmKerning = \_ _ -> 0
    , fmShape = \_ -> Nothing
    , fmGlyph = \_ -> Nothing
    , fmBackend = Nothing
    }

newSvgContext :: Bool -> IO Context
newSvgContext squareGeo = do
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
  setDrawSquareGeometry ctx squareGeo
  setDrawExternalText ctx True
  pure ctx

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
loadInterFontCss = do
  let candidates =
        [ "packages/nano-ui-sdl/data/inter.ttf"
        , "../nano-ui-sdl/data/inter.ttf"
        , "data/inter.ttf"
        ]
      findFirst [] = pure Nothing
      findFirst (p : ps) = do
        ok <- doesFileExist p
        if ok then Just <$> BS.readFile p else findFirst ps
  mBytes <- findFirst candidates
  pure $ case mBytes of
    Nothing -> ""
    Just bytes ->
      "@font-face { font-family: 'Inter'; font-style: normal; font-weight: 100 900; font-display: block; src: url('data:font/ttf;base64,"
        <> encodeBase64 bytes
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

-- | Event-free input with the pointer parked outside the viewport so nothing
-- is inadvertently hovered unless requested.
offInput :: Float -> Float -> Input
offInput w h =
  emptyInput
    { inputWindowSize = Size w h
    , inputMousePos = V2 (-20) (-20)
    }

-- | Painted extent of one frame in logical pixels, as @(x0, y0, x1, y1)@.
type Bounds = (Float, Float, Float, Float)

-- | Extra canvas height given to specimens. Widgets can lay out past the
-- nominal height (a tab strip's last row, an overlay's shadow), and the crop
-- recovers it; width is left exact so fill-width demos keep their measure.
canvasSlack :: Float
canvasSlack = 72

-- | Union of a frame's solid geometry (vertex positions) and text line boxes.
-- The full-window backdrop that 'frameToSvg' paints is in neither, so this is
-- the ink a reader sees.
frameInkBounds :: DrawData -> [(Rect, Text, Color, Color, Rect)] -> IO (Maybe Bounds)
frameInkBounds dd spans = do
  verts <- vertexPoints dd
  let pts = verts <> concatMap spanPts spans
  pure $ case pts of
    ((x, y) : rest) -> Just (foldl' growPoint (x, y, x, y) rest)
    [] -> Nothing
  where
    spanPts (Rect x y w h, _, _, _, _) = [(x, y), (x + w, y + h)]
    growPoint (x0, y0, x1, y1) (x, y) = (min x0 x, min y0 y, max x1 x, max y1 y)

-- | Every vertex position in the frame's solid-geometry buffer.
vertexPoints :: DrawData -> IO [(Float, Float)]
vertexPoints dd =
  withForeignPtr (drawVertices dd) $ \vp ->
    forM [0 .. drawVertexCount dd - 1] $ \i -> do
      x <- peekByteOff vp (i * vertexSize) :: IO Float
      y <- peekByteOff vp (i * vertexSize + 4) :: IO Float
      pure (x, y)

growBounds :: Bounds -> Bounds -> Bounds
growBounds (ax0, ay0, ax1, ay1) (bx0, by0, bx1, by1) =
  (min ax0 bx0, min ay0 by0, max ax1 bx1, max ay1 by1)

-- | Crop a rendered SVG to its painted bounds (plus padding), rewriting the
-- root @<svg>@ tag's @width@, @height@, and @viewBox@. Height tightens to the
-- content but never past the canvas (whose backdrop is the frame's only
-- background); width stays at @canvasW@ unless content overhangs it. Scale
-- stays 1:1.
cropSvg :: Float -> Float -> Maybe Bounds -> Text -> Text
cropSvg _ _ Nothing svg = svg
cropSvg canvasW canvasH (Just (_, by0, bx1, by1)) svg
  | not ("<svg" `T.isPrefixOf` open) || T.null rest = svg
  | otherwise = header <> rest
  where
    (open, rest) = T.breakOn ">" svg
    pad = 8
    minY = max 0 (floor (by0 - pad) :: Int)
    maxY = min (ceiling canvasH) (ceiling (by1 + pad))
    h = max 1 (maxY - minY)
    w =
      if bx1 > canvasW + 0.5
        then ceiling (bx1 + pad)
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

-- | Prefix SVG internal @id="tclip_N"@, @id="clip-N"@, and @id="grad-N"@
-- attributes so multiple inline @<svg>@ documents embedded on the same HTML
-- page never collide in the browser DOM.
namespaceSvg :: Text -> Text -> Text
namespaceSvg prefix =
  T.replace "id=\"tclip_" ("id=\"" <> prefix <> "-tclip_")
    . T.replace "url(#tclip_" ("url(#" <> prefix <> "-tclip_")
    . T.replace "id=\"sclip_" ("id=\"" <> prefix <> "-sclip_")
    . T.replace "url(#sclip_" ("url(#" <> prefix <> "-sclip_")
    . T.replace "id=\"clip-" ("id=\"" <> prefix <> "-clip-")
    . T.replace "url(#clip-" ("url(#" <> prefix <> "-clip-")
    . T.replace "id=\"grad-" ("id=\"" <> prefix <> "-grad-")
    . T.replace "url(#grad-" ("url(#" <> prefix <> "-grad-")

-- | Warm a view over zero or more frames, then run one final frame with
-- 'damageFullNow' and export it as a namespaced inline @<svg>@ element.
--
-- 'squareGeo' selects between flat square geometry (@True@) and rounded vector
-- geometry (@False@) for 'frameToSvg'. Every frame in 'warmInputs' is run once
-- without damage to settle layout and state; the 'captureInput' frame is the
-- one returned.
captureSvg :: Text -> Bool -> [Input] -> Input -> NanoUI () -> IO Text
captureSvg tag squareGeo warmInputs captureInput ui = do
  ctx <- newSvgContext squareGeo
  mapM_ (\inp -> runFrame ctx inp ui) warmInputs
  (_, dd, _) <- runFrame ctx captureInput (ui >> damageFullNow)
  namespaceSvg tag <$> frameToSvg defaultFrameSvgConfig ctx captureInput dd

-- | An input derived from @base@ with the left mouse button pressed at @pos@.
pressInputAt :: Input -> V2 -> Input
pressInputAt base pos =
  base
    { inputMousePos = pos
    , inputButtonsHeld = buttonsFromList [MouseLeft]
    , inputButtonsPressed = buttonsFromList [MouseLeft]
    }

-- | An input derived from @base@ with the left mouse button released at @pos@.
releaseInputAt :: Input -> V2 -> Input
releaseInputAt base pos =
  base
    { inputMousePos = pos
    , inputButtonsReleased = buttonsFromList [MouseLeft]
    }

-- | Render a two-frame warmed-up view with rounded geometry and external text
-- spans, returning a namespaced inline @<svg>@ element.
renderSvg :: Text -> Float -> Float -> NanoUI () -> IO Text
renderSvg tag w h ui = captureSvg tag False [inp] inp ui
  where
    inp = offInput w h

-- | Render with @setDrawSquareGeometry ctx True@ to compare flat square
-- geometry against rounded vector geometry in 'frameToSvg'.
renderSvgRound :: Text -> Float -> Float -> NanoUI () -> IO Text
renderSvgRound tag w h ui = captureSvg tag True [inp] inp ui
  where
    inp = offInput w h

-- | Render a view with the cursor hovering at @pos@ on the second frame.
renderSvgHover :: Text -> Float -> Float -> V2 -> NanoUI () -> IO Text
renderSvgHover tag w h pos ui = captureSvg tag False [base] (base {inputMousePos = pos}) ui
  where
    base = offInput w h

-- | Render a view with the left mouse button held down at @pos@.
renderSvgPress :: Text -> Float -> Float -> V2 -> NanoUI () -> IO Text
renderSvgPress tag w h pos ui = captureSvg tag False [base] (pressInputAt base pos) ui
  where
    base = offInput w h

-- | Warm up a view, click at @pos@ (press + release frames), and capture the
-- resulting frame (e.g. focused field or open dropdown overlay).
renderSvgClick :: Text -> Float -> Float -> V2 -> NanoUI () -> IO Text
renderSvgClick tag w h pos ui =
  captureSvg tag False [base, pressInputAt base pos, releaseInputAt base pos] base ui
  where
    base = offInput w h

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
  ctx <- newSvgContext False
  let inp = offInput w h
      nsTag = "wb-" <> bankId
  _ <- runFrame ctx inp view
  caps <- forM [0 .. 127] $ \mask -> do
    (_, dd, _) <- runFrame ctx inp (applyStyleMask isOverlay mask view >> damageFullNow)
    svg <- namespaceSvg nsTag <$> frameToSvg defaultFrameSvgConfig ctx inp dd
    spans <- collectRasterSpans ctx inp
    bounds <- frameInkBounds dd (fst spans <> snd spans)
    pure (svg, bounds)
  let boxes = [b | (_, Just b) <- caps]
      cropBox = case boxes of
        (b0 : bs) -> Just (foldl' growBounds b0 bs)
        [] -> Nothing
      svgs = [cropSvg w h cropBox svg | (svg, _) <- caps]
      baseSvg = case svgs of
        (s0 : _) -> s0
        [] -> ""
  pure (baseSvg, packSvgBank svgs)

-- ---------------------------------------------------------------------------
-- Section II: Interactive Per-Widget Style Workbench Cards (15 Families)
-- ---------------------------------------------------------------------------

data ImpactKind
  = ImpactPrimary
  | ImpactPartial
  | ImpactSurprise
  | ImpactNoOp

impactCode :: ImpactKind -> Text
impactCode = \case
  ImpactPrimary -> "primary"
  ImpactPartial -> "partial"
  ImpactSurprise -> "surprise"
  ImpactNoOp -> "noop"

impactBadgeText :: ImpactKind -> Text
impactBadgeText = \case
  ImpactPrimary -> "Primary"
  ImpactPartial -> "Partial"
  ImpactSurprise -> "Edge case"
  ImpactNoOp -> "No-op"

data ToggleNote = ToggleNote
  { tnBit :: !Int
  , tnKey :: !Text
  , tnLabel :: !Text
  , tnShort :: !Text
  , tnImpact :: !ImpactKind
  , tnDetail :: !Text
  }

mkToggleNotes :: [(ImpactKind, Text)] -> [ToggleNote]
mkToggleNotes specs =
  [ ToggleNote bit key lbl short imp det
  | ((bit, key, lbl, short), (imp, det)) <- zip meta specs
  ]
  where
    meta :: [(Int, Text, Text, Text)]
    meta =
      [ (0, "light", "defaultLightTheme", "Light Theme")
      , (1, "btn", "buttonStyle", "buttonStyle")
      , (2, "inp", "inputStyle", "inputStyle")
      , (3, "pnl", "panelStyle", "panelStyle")
      , (4, "acc", "accentColor", "accentColor")
      , (5, "rnd", "rounded 999", "rounded 999")
      , (6, "dis", "disabledWhen True", "disabledWhen")
      ]

data ProbeRow = ProbeRow
  { prId :: !Text
  , prIsOverlay :: !Bool
  , prCategory :: !Text
  , prTitle :: !Text
  , prSignature :: !Text
  , prPrimarySurface :: !Text
  , prSummary :: !Text
  , prCodeExpr :: !Text
  , prToggles :: ![ToggleNote]
  , prBaselineSvg :: !Text
  , prBankValue :: !Value
  }

buildProbeRow ::
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Bool ->
  Float ->
  Float ->
  [(ImpactKind, Text)] ->
  NanoUI () ->
  IO ProbeRow
buildProbeRow rid cat title sig surf summary codeExpr isOverlay w h notes view = do
  (baseSvg, bankVal) <- renderStyleBank rid w (h + canvasSlack) isOverlay (framePad view)
  pure
    ProbeRow
      { prId = rid
      , prIsOverlay = isOverlay
      , prCategory = cat
      , prTitle = title
      , prSignature = sig
      , prPrimarySurface = surf
      , prSummary = summary
      , prCodeExpr = codeExpr
      , prToggles = mkToggleNotes notes
      , prBaselineSvg = baseSvg
      , prBankValue = bankVal
      }

generateProbeMatrix :: IO [ProbeRow]
generateProbeMatrix =
  sequence
    [ buildProbeRow
        "button"
        "surface-button"
        "button (normal, subtle)"
        "button :: Text -> NanoUI Bool"
        "themeButton"
        "Reads `styleBg`, `styleHoverBg`, `styleActiveBg`, `styleFg`, `styleBorder`, `styleBorderWidth` and `styleCornerRadius` from `themeButton`. Focus adds a 2px `themeFocusRing` stroke."
        "rowWith (gap 8) $ button \"Save\" >> styled subtle (button \"Subtle\")"
        False
        236
        54
        [ (ImpactPrimary, "Switches the base theme to `defaultLightTheme` before scoped modifiers run.")
        , (ImpactPrimary, "Colors `Save` (`styleBg`, `styleBorder`, `styleFg`). `Subtle` keeps only its text color because its own `styled subtle` runs after the outer `buttonStyle` and clears the background and border.")
        , (ImpactNoOp, "No effect: the button never reads `themeInput`.")
        , (ImpactNoOp, "No effect: the button never reads `themePanel`.")
        , (ImpactNoOp, "No effect. `button` ignores `themeAccent`; use `styled primary` or `toggleButton` to bind it.")
        , (ImpactPrimary, "Sets `styleCornerRadius` to 999, so the button becomes a pill (`rx` = half the height).")
        , (ImpactPrimary, "Dims the fill, border and label, and blocks clicks and focus.")
        ]
        ( rowWith (gap 8 . alignMid) $ do
            void (button "Save")
            void (styled subtle (button "Subtle"))
        )
    , buildProbeRow
        "toggle-button"
        "surface-button"
        "toggleButton off and on"
        "toggleButton :: Tone -> Text -> Var Bool -> NanoUI ()"
        "themeButton + Tone"
        "Off uses `themeButton`, so `buttonStyle` applies. On replaces the fill, hover fill and border with `toneColor`, and the text with `readableOn`, so `buttonStyle` colors stop applying."
        "toggleButton Accent \"Off\" vOff >> toggleButton Accent \"On\" vOn"
        False
        236
        54
        [ (ImpactPrimary, "Switches the base theme. `readableOn` recomputes the on-state text color against the tone color.")
        , (ImpactPartial, "Affects the off state only. On, `widgetVisualStyle` replaces the fill, hover fill and border with `toneColor`, and the text with `readableOn`.")
        , (ImpactNoOp, "No effect: `toggleButton` never reads `themeInput`.")
        , (ImpactNoOp, "No effect: `toggleButton` never reads `themePanel`.")
        , (ImpactPrimary, "Colors the on state when the tone is `Accent`; `readableOn` picks black or white text.")
        , (ImpactPrimary, "Applies radius 999 to both states. The on state keeps `styleCornerRadius` from `themeButton`.")
        , (ImpactPrimary, "Dims both states and blocks toggling.")
        ]
        ( rowWith (gap 8 . alignMid) $ do
            vOff <- useVar False
            vOn <- useVar True
            toggleButton Accent "Off (False)" vOff
            toggleButton Accent "On (True)" vOn
        )
    , buildProbeRow
        "checkbox"
        "surface-button surface-input"
        "checkbox"
        "checkbox :: Text -> Var Bool -> NanoUI ()"
        "themeButton + themeInput + themeAccent"
        "Label and unchecked outline come from `themeButton` (`styleFg`, `styleBorder`). The unchecked fill comes from `themeInput` (`styleBg`). The checked box uses `themeAccent` and `themeOnAccent`. `clearStyle` removes the button background."
        "checkbox \"Unchecked\" vOff >> checkbox \"Checked\" vOn"
        False
        236
        68
        [ (ImpactPrimary, "Switches the base theme: light unchecked well, dark label.")
        , (ImpactPartial, "Colors the label and unchecked outline. `clearStyle` removes the background, so no button box is drawn.")
        , (ImpactPartial, "Colors only the 14x14 unchecked well.")
        , (ImpactNoOp, "No effect: `checkbox` never reads `themePanel`.")
        , (ImpactPrimary, "Colors the checked box fill and border; the checkmark uses `themeOnAccent`.")
        , (ImpactSurprise, "Ignored. `paintChoiceIndicator` hardcodes the radius to `min 3 (boxS * 0.2)`.")
        , (ImpactPrimary, "Dims the boxes, checkmark and label.")
        ]
        ( columnWith (gap 4 . tight) $ do
            vOff <- useVar False
            vOn <- useVar True
            checkbox "Unchecked" vOff
            checkbox "Checked" vOn
        )
    , buildProbeRow
        "radio"
        "surface-button surface-input"
        "radio"
        "radio :: Foldable f => f Text -> Var Int -> NanoUI ()"
        "themeButton + themeInput + themeAccent"
        "Shares the checkbox path. The unselected ring fill uses `themeInput`, the ring stroke and label use `themeButton`, and the selected dot and ring use `themeAccent`."
        "radio [\"Option A\", \"Option B\"] vSelected"
        False
        236
        68
        [ (ImpactPrimary, "Switches the base theme.")
        , (ImpactPartial, "Colors the labels and the unselected ring stroke. `clearStyle` removes the background.")
        , (ImpactPartial, "Colors only the unselected ring fill.")
        , (ImpactNoOp, "No effect: `radio` never reads `themePanel`.")
        , (ImpactPrimary, "Colors the selected ring and dot.")
        , (ImpactSurprise, "No effect: radio indicators are always circles (`r = boxS / 2`).")
        , (ImpactPrimary, "Dims the rings, dot and labels.")
        ]
        ( columnWith (gap 4 . tight) $ do
            vSel <- useVar (1 :: Int)
            radio ["Option A (Unselected)" :: Text, "Option B (Selected)"] vSel
        )
    , buildProbeRow
        "text-input"
        "surface-input"
        "textInput"
        "textInput :: Var Text -> NanoUI ()"
        "themeInput"
        "`themeInput` paints the well, text and border. The placeholder blends 40% toward the well fill. On focus the border becomes `themeAccent` and the fill becomes `styleActiveBg`, with no outer ring."
        "textInputConfigured (defaultTextInputConfig {ticPlaceholder = \"Filter...\"}) vEmpty >> textInput vText"
        False
        236
        84
        [ (ImpactPrimary, "Switches the base theme.")
        , (ImpactNoOp, "No effect: `textInput` reads `themeInput`.")
        , (ImpactPrimary, "Colors the well fill, border, value text and placeholder.")
        , (ImpactNoOp, "No effect: `textInput` never reads `themePanel`.")
        , (ImpactPartial, "Used for the focused border and caret.")
        , (ImpactPrimary, "Turns the well into a pill.")
        , (ImpactPrimary, "Dims the well, border and text, and blocks focus and typing.")
        ]
        ( columnWith (gap 6 . fillW . tight) $ do
            vEmpty <- useVar ("" :: Text)
            vText <- useVar ("nano-ui" :: Text)
            textInputConfigured (defaultTextInputConfig {ticPlaceholder = "Filter..."}) vEmpty
            textInput vText
        )
    , buildProbeRow
        "search-numeric"
        "surface-input"
        "searchInput and numericInput"
        "searchInput :: Text -> Var Text -> NanoUI ()"
        "themeInput"
        "Both use `themeInput`. `searchInput` tints its magnifier and clear glyphs 45% toward the well fill. `numericInput` tints its divider 40% from the border and its stepper arrows 40% from the text color."
        "searchInput \"Search\" vQuery >> numericInput vNum"
        False
        236
        84
        [ (ImpactPrimary, "Switches the base theme.")
        , (ImpactNoOp, "No effect. The stepper buttons in `numericInput` are painted from `themeInput`, not `themeButton`.")
        , (ImpactPrimary, "Colors both fields. `searchInput` tints its icons 45% toward the well fill; `numericInput` tints its divider and arrows 40% toward the well fill.")
        , (ImpactNoOp, "No effect: neither input reads `themePanel`.")
        , (ImpactPartial, "Used for the focused border and caret.")
        , (ImpactPrimary, "Turns both wells into pills.")
        , (ImpactPrimary, "Dims the wells, icons, arrows and text.")
        ]
        ( columnWith (gap 6 . fillW . tight) $ do
            vQ <- useVar ("plot" :: Text)
            vN <- useVar (42.5 :: Double)
            searchInput "Search" vQ
            numericInput vN
        )
    , buildProbeRow
        "select-vs-combo"
        "surface-button surface-input"
        "select and comboBox"
        "select :: f Text -> Var Int -> NanoUI ()"
        "select: themeButton | comboBox: themeInput"
        "`select` is a `NodeSelect` painted with `themeButton`; `comboBox` is a `NodeTextInput` painted with `themeInput`. Both open menus on `themePopup`."
        "selectWith fillW [\"Select (btn)\", ...] vIdx >> comboBox \"Pick...\" [...] vCombo"
        False
        236
        84
        [ (ImpactPrimary, "Switches the base theme on both controls.")
        , (ImpactPartial, "Styles `select` only. `comboBox` is a `NodeTextInput` and ignores `buttonStyle`.")
        , (ImpactPartial, "Styles `comboBox` only. `select` is a `NodeSelect` and ignores `inputStyle`.")
        , (ImpactNoOp, "No effect on closed controls; open menus read `themePopup`.")
        , (ImpactPartial, "Highlights the selected row in an open menu and the focused `comboBox` border.")
        , (ImpactPrimary, "Rounds `select` via `themeButton` and `comboBox` via `themeInput`.")
        , (ImpactPrimary, "Dims both controls and blocks opening their menus.")
        ]
        ( columnWith (gap 6 . fillW . tight) $ do
            vIdx <- useVar (0 :: Int)
            vCombo <- useVar ("GHC 9.14" :: Text)
            selectWith fillW ["Select (themeButton)" :: Text, "Option 2"] vIdx
            comboBox "Pick..." ["GHC 9.14" :: Text, "GHC 9.12"] vCombo
        )
    , buildProbeRow
        "textarea"
        "surface-input"
        "textArea"
        "textArea :: Var Text -> NanoUI ()"
        "themeInput"
        "Multi-line editor on `themeInput`. Focus swaps the border to `themeAccent` and the fill to `styleActiveBg`; selections use `themeSelection`."
        "textAreaWith (fillW . fixedH 64) vDoc"
        False
        236
        84
        [ (ImpactPrimary, "Switches the base theme.")
        , (ImpactNoOp, "No effect: `textArea` reads `themeInput`.")
        , (ImpactPrimary, "Colors the well, border and document text.")
        , (ImpactNoOp, "No effect: `textArea` never reads `themePanel`.")
        , (ImpactPartial, "Used for the active border and caret.")
        , (ImpactPrimary, "Rounds the editor corners.")
        , (ImpactPrimary, "Dims the well and text, and blocks editing.")
        ]
        ( do
            vDoc <- useVar ("line 1: module Main\nline 2: import NanoUI" :: Text)
            textAreaWith (fillW . fixedH 64) vDoc
        )
    , buildProbeRow
        "slider"
        "surface-input"
        "slider"
        "slider :: Float -> Float -> Var Float -> NanoUI ()"
        "themeInput + themeAccent"
        "The track outline and thumb border use `styleBorder` from `themeInput`. The unfilled track blends 35% from `styleBg` toward the border, the active fill uses `themeAccent`, and the thumb interior uses `themeOnAccent`. Border width is fixed at 1px and track radius at 3px."
        "sliderWith fillW 0 100 vVal"
        False
        236
        54
        [ (ImpactPrimary, "Switches the base theme.")
        , (ImpactNoOp, "No effect: `slider` reads `themeInput` and `themeAccent`.")
        , (ImpactPartial, "Colors the track outline and thumb border, and blends the unfilled track 35% from `styleBg` toward the border.")
        , (ImpactNoOp, "No effect: `slider` never reads `themePanel`.")
        , (ImpactPrimary, "Colors the filled track; the thumb interior uses `themeOnAccent`.")
        , (ImpactSurprise, "Ignored. `paintSliderNode` hardcodes track radius 3 and border width 1.")
        , (ImpactPrimary, "Dims the track, fill and thumb.")
        ]
        ( do
            vVal <- useVar (64 :: Float)
            sliderWith fillW 0 100 vVal
        )
    , buildProbeRow
        "custom-controls"
        "surface-button surface-panel"
        "toggleSwitch, progressBar, sparkline"
        "toggleSwitch :: Var Bool -> NanoUI ()"
        "themeButton / themePanel + themeAccent"
        "Canvas widgets from `NanoUI.Widgets.Custom`. `toggleSwitch`, `progressBar`, `circularProgress` and `knob` read `themeButton` plus `themeAccent`; `sparkline` fills its well with `themePanel`'s `styleBg`."
        "toggleSwitch vSw >> progressBar 0.68 >> sparkline [2, 7, 4, 9, 5, 11, 8, 12]"
        False
        236
        94
        [ (ImpactPrimary, "Switches the base theme for all three canvas widgets.")
        , (ImpactPartial, "Colors the track border and unfilled track of `toggleSwitch` and `progressBar`; `sparkline` ignores it.")
        , (ImpactNoOp, "No effect: these widgets never read `themeInput`.")
        , (ImpactPartial, "Colors only the `sparkline` well; `toggleSwitch` and `progressBar` ignore it.")
        , (ImpactPrimary, "Colors the switch pill, progress fill, and sparkline line and dot.")
        , (ImpactSurprise, "Ignored. The switch and progress bar use `r = h/2`; the sparkline uses `r = 3`.")
        , (ImpactPrimary, "Dims `cdcTheme` in `CustomDrawContext` for all three.")
        ]
        ( columnWith (gap 6 . fillW . tight) $ do
            vSw <- useVar True
            toggleSwitch vSw
            progressBar 0.68
            sparkline [2, 7, 4, 9, 5, 11, 8, 12]
        )
    , buildProbeRow
        "panel-card-callout"
        "surface-panel"
        "panel, card, callout"
        "panel :: NanoUI a -> NanoUI a"
        "themePanel"
        "`panel` and `card` read `styleBg`, `styleBorder`, `styleBorderWidth`, `styleBorderSides` and `styleCornerRadius` from `themePanel`. `callout` applies a tinted `panelStyle` to its whole subtree."
        "panel (label \"panel surface\") >> callout amber (label \"Warning callout\")"
        False
        236
        94
        [ (ImpactPrimary, "Switches the base theme.")
        , (ImpactNoOp, "No effect: containers read `themePanel`.")
        , (ImpactNoOp, "No effect: containers never read `themeInput`.")
        , (ImpactPrimary, "Colors the panel fill, border and child labels. `callout` applies its own tinted `panelStyle` on top.")
        , (ImpactNoOp, "No effect when `callout` uses an explicit color; `calloutTone Accent` binds `themeAccent`.")
        , (ImpactPrimary, "Rounds the panel and callout corners.")
        , (ImpactPrimary, "Dims the surfaces, borders and labels.")
        ]
        ( columnWith (gap 6 . fillW . tight) $ do
            panelWith (fillW . padAll 6) (label "panel surface")
            calloutWith (colorRGBA 235 179 56 255) (fillW . padAll 6) (label "Warning callout")
        )
    , buildProbeRow
        "scroll-bounded"
        "surface-input"
        "scrollWith (bounded)"
        "scrollWith :: (Layout -> Layout) -> NanoUI a -> NanoUI a"
        "themeInput (cornerRadius = 0)"
        "With a `Fixed` or `Fit` axis, `paintScrollSurface` draws a bordered `themeInput` well with `styleCornerRadius` forced to 0. The scrollbar blends `themeInput` with `themeSeparator`."
        "scrollWith (fillW . fixedH 64 . padAll 6) $ label \"Row 1\" >> ..."
        False
        236
        82
        [ (ImpactPrimary, "Switches the base theme.")
        , (ImpactNoOp, "No effect: scroll wells never read `themeButton`.")
        , (ImpactPrimary, "Unexpected: bounded scroll wells use `themeInput`, not `themePanel`, and blend the scrollbar with it.")
        , (ImpactNoOp, "Unexpected no-op: the well ignores `panelStyle`; only the child label text reads `themePanel`'s `styleFg`.")
        , (ImpactNoOp, "No effect: the scrollbar blends `themeInput` and `themeSeparator`.")
        , (ImpactSurprise, "Ignored. `paintScrollSurface` forces `styleCornerRadius = 0`, so the well is always square.")
        , (ImpactPrimary, "Dims the well, scrollbar and labels.")
        ]
        ( scrollWith (fillW . fixedH 64 . padAll 6 . gap 4) $ do
            label "Row 1 in scroll well"
            label "Row 2 in scroll well"
            label "Row 3 in scroll well"
            label "Row 4 in scroll well"
        )
    , buildProbeRow
        "tabs-styles"
        "surface-panel surface-button"
        "tabBar styles"
        "tabBarConfigured :: TabsConfig -> Var a -> f (Tab a b) -> NanoUI ()"
        "themePanel + themeAccent (+ themeButton)"
        "Tab headers are `NodeButton`s, but `tabHeaderVisualStyle` reads `themePanel` and `themeAccent`. Only `TabSegmented` reads `styleBorder` from `themeButton` and compares its luminance with `themePanel`."
        "tabBarConfigured (defaultTabsConfig {tabsStyle = ...}) vTab [tab 0 \"Code\" (), tab 1 \"AST\" ()]"
        False
        236
        106
        [ (ImpactPrimary, "Switches the base theme.")
        , (ImpactSurprise, "Ignored by `TabUnderline` and `TabPill`. `tabHeaderVisualStyle` reads `themePanel`; only `TabSegmented` reads `styleBorder` from `themeButton`.")
        , (ImpactNoOp, "No effect: `tabBar` never reads `themeInput`.")
        , (ImpactPrimary, "Colors inactive and active tab text, the underline rule and the `TabSegmented` well.")
        , (ImpactPrimary, "Colors the underline bar in `TabUnderline` and the active pill fill in `TabPill`.")
        , (ImpactPartial, "Rounds the `TabSegmented` well and active segment. `TabUnderline` is unaffected; `TabPill` already uses half the height.")
        , (ImpactPrimary, "Dims the tab styles and blocks switching.")
        ]
        ( columnWith (gap 4 . fillW . tight) $ do
            vTab <- useVar (0 :: Int)
            let ts = [tab (0 :: Int) "Code" (), tab 1 "AST" ()]
            tabBarConfigured (defaultTabsConfig {tabsStyle = TabUnderline}) vTab ts
            tabBarConfigured (defaultTabsConfig {tabsStyle = TabPill}) vTab ts
            tabBarConfigured (defaultTabsConfig {tabsStyle = TabSegmented}) vTab ts
        )
    , buildProbeRow
        "table-tree"
        "surface-panel surface-button"
        "simpleTable and tree"
        "simpleTable :: [Text] -> f [Text] -> NanoUI TableResponse"
        "themePanel + themeButton + themeAccent"
        "Table headers blend 55% from `themePanel` toward `themeButton`; the sorted header uses `themeButton`'s `styleFg` and unsorted headers use `themeMuted`. Tree rows alternate `themeWindow` and `themeButton` stripes and use `themeButton`'s `styleFg`."
        "simpleTable [\"Mod\", \"Surface\"] [...] >> tree \"demo-tree\" [...] vTree"
        False
        236
        138
        [ (ImpactPrimary, "Switches the base theme.")
        , (ImpactPartial, "Blends 55% into the table header fill, and colors the sorted header text, tree labels and odd tree row tint.")
        , (ImpactNoOp, "No effect: neither `simpleTable` nor `tree` reads `themeInput`.")
        , (ImpactPrimary, "Colors the table cells, grid borders, header fill and tree row fill.")
        , (ImpactPrimary, "Tints the selected tree row 25% toward `themeAccent` and draws the 2px left bar.")
        , (ImpactNoOp, "No effect: cells and rows are drawn as sharp edge-to-edge rects.")
        , (ImpactPrimary, "Dims the headers, cells, grid lines and rows.")
        ]
        ( columnWith (gap 6 . fillW . tight) $ do
            void $
              simpleTable
                ["Mod", "Surface"]
                [ ["btn", "Button"]
                , ["inp", "Input"]
                ]
            vTree <- useVar (1 :: Int)
            tree
              "demo-tree"
              [ TreeItem "Theme" [TreeItem "themeButton" []]
              ]
              vTree
        )
    , buildProbeRow
        "overlays"
        "surface-popup surface-window"
        "popup, menuItem, window"
        "window :: Var Bool -> Text -> NanoUI () -> NanoUI ()"
        "themePopup / themeFloatingWindow"
        "`modal`, `popup`, `contextMenu` and open select/combo menus use `overlayMenuStyle` (from `themePopup`, with a `themeButton` hover fallback when the popup hover fill equals its fill). `window` reads `themeFloatingWindow` and `themeSeparator`."
        "window vWin \"Inspector\" (label \"Floating window\")"
        True
        236
        106
        [ (ImpactPrimary, "Switches the base theme.")
        , (ImpactNoOp, "No effect until hover. `menuItem` falls back to `styleHoverBg` from `themeButton` only when the popup hover fill equals its fill.")
        , (ImpactNoOp, "No effect: overlays never read `themeInput`.")
        , (ImpactPrimary, "Colors the floating window chrome via `windowStyle` and popup menus via `popupStyle`.")
        , (ImpactNoOp, "No effect on standard window chrome.")
        , (ImpactPrimary, "Rounds the floating window corners.")
        , (ImpactPrimary, "Dims the window chrome, title bar and body.")
        ]
        ( do
            vWin <- useVar True
            window vWin "Inspector" $
              label "Floating window"
        )
    ]

-- ---------------------------------------------------------------------------
-- Section III: Interactive Typography Workbench & Specimens
-- ---------------------------------------------------------------------------

data TypoDemo = TypoDemo
  { tdTitle :: !Text
  , tdCode :: !Text
  , tdNote :: !Text
  , tdSvg :: !Text
  }

data TypoSectionData = TypoSectionData
  { tsdBaseSvg :: !Text
  , tsdBankValue :: !Value
  , tsdDemos :: ![TypoDemo]
  }

applyTypoMask :: Int -> Layout -> Layout
applyTypoMask mask =
  (if testBit mask 5 then fontSize 16 else id)
    . (if testBit mask 4 then fontTone Danger else id)
    . (if testBit mask 3 then fontStrike else id)
    . (if testBit mask 2 then fontUnderline else id)
    . (if testBit mask 1 then fontItalic else id)
    . (if testBit mask 0 then fontBold else id)

generateTypoDemos :: IO TypoSectionData
generateTypoDemos = do
  -- 64-combination Interactive Typography & *With Workbench (bits 0..5)
  ctx <- newSvgContext False
  let w = 480
      h = 152
      inp = offInput w h
      viewFor mask =
        let m = applyTypoMask mask
         in panelWith (fillW . fillH . padAll 10 . gap 8) $ do
              rowWith (gap 8 . alignMid . fillW) $ do
                labelWith (fixedW 142 . fontMono) "labelWith"
                labelWith m "Hamburgefonstiv 0123"
              rowWith (gap 8 . alignMid . fillW) $ do
                labelWith (fixedW 142 . fontMono) "textInput"
                vTxt <- useVar ("Hamburgefonstiv 0123" :: Text)
                textInputConfigured (defaultTextInputConfig {ticLayout = m (fillW defaultLayout)}) vTxt
              rowWith (gap 8 . alignMid . fillW) $ do
                labelWith (fixedW 142 . fontMono) "buttonWith"
                void (buttonWith (m . fillW) "Hamburgefonstiv 0123")
              rowWith (gap 8 . alignMid . fillW) $ do
                labelWith (fixedW 142 . fontMono) "buttonContent"
                void $
                  buttonContentWith fillW $
                    labelWith m "Hamburgefonstiv 0123"
  _ <- runFrame ctx inp (viewFor 0)
  typoSvgs <- forM [0 .. 63] $ \mask -> do
    (_, dd, _) <- runFrame ctx inp (viewFor mask >> damageFullNow)
    namespaceSvg "wb-typo" <$> frameToSvg defaultFrameSvgConfig ctx inp dd
  let typoBaseSvg = case typoSvgs of
        (s0 : _) -> s0
        [] -> ""
      typoBankVal = packSvgBank typoSvgs

  svgLabels <-
    renderSvg "typo-labels" 480 124 $
      panelWith (fillW . fillH . padAll 10 . gap 6) $ do
        rowWith (gap 12 . alignMid) $ do
          label "label (14px)"
          heading "heading (WeightMedium)"
          labelWith fontHeading "labelWith fontHeading (accent)"
        rowWith (gap 12 . alignMid) $ do
          muted "muted"
          mono "mono"
          danger "danger"
          bold "bold"
          italic "italic"
          underline "underline"
        rowWith (gap 12 . alignMid) $ do
          labelWith fontStrike "fontStrike"
          labelWith (fontColor (colorRGBA 56 189 248 255) . fontBold) "fontColor + fontBold"
          kv "Key" "Value"

  svgToneMatrix <-
    renderSvg "typo-tone-matrix" 480 116 $
      panelWith (fillW . fillH . padAll 10 . gap 8) $ do
        rowWith (gap 6 . alignMid) $ do
          void (styled primary (button "primary"))
          void (styled destructive (button "destructive"))
          void (styled success (button "success"))
          void (styled warning (button "warning"))
          void (styled subtle (button "subtle"))
        rowWith (gap 6 . alignMid) $ do
          void (styled (tinted themeAccent) (button "tinted Accent"))
          void (styled (tinted themeDanger) (button "tinted Danger"))
          void (styled (tinted themeSuccess) (button "tinted Success"))
          void (styled (tinted themeWarning) (button "tinted Warning"))

  pure
    TypoSectionData
      { tsdBaseSvg = typoBaseSvg
      , tsdBankValue = typoBankVal
      , tsdDemos =
          [ TypoDemo
              "Text helpers and font modifiers"
              "label, heading, labelWith fontHeading, muted, mono, danger, bold, italic, underline, fontStrike, fontColor"
              "NodeText and NodeTextInput pack font variant, tone, weight, style and decoration into styleIdx."
              svgLabels
          , TypoDemo
              "Tones and tinted surfaces"
              "styled primary / destructive / success / warning / subtle / tinted themeAccent"
              "tone fills the button with the semantic color and picks readableOn for the text. tinted blends 18% of the color into the background, draws a 55% border and keeps the color for the text."
              svgToneMatrix
          ]
      }

-- ---------------------------------------------------------------------------
-- Section IV: 12 Unexpected Interactions & Edge Cases
-- ---------------------------------------------------------------------------

data InteractionCard = InteractionCard
  { icNumber :: !Text
  , icCategory :: !Text
  , icTitle :: !Text
  , icWhere :: !Text
  , icWhy :: !Text
  , icFix :: !Text
  , icCodeBad :: !Text
  , icCodeGood :: !Text
  , icLabelA :: !Text
  , icSvgA :: !Text
  , icLabelB :: !Text
  , icSvgB :: !Text
  }

generateInteractions :: IO [InteractionCard]
generateInteractions = do
  -- 1. buttonWith / selectWith / checkboxWith ignore fontBold, fontTone, fontMono
  i1Bad <-
    renderSvg "i1-bad" 260 92 $
      panelWith (fillW . fillH . padAll 8 . gap 6) $ do
        void (buttonWith (fontBold . fontUnderline . fontDanger) "buttonWith fontDanger")
        vSel <- useVar (0 :: Int)
        selectWith (fillW . fontBold . fontDanger) ["selectWith fontDanger" :: Text] vSel
  i1Good <-
    renderSvg "i1-good" 260 92 $
      panelWith (fillW . fillH . padAll 8 . gap 6) $ do
        void (styled (tinted themeDanger) (buttonContent (labelWith (fontBold . fontUnderline . fontDanger) "buttonContent + tinted")))
        vSel <- useVar (0 :: Int)
        selectWith (fillW . fontColor (colorRGBA 248 113 113 255)) ["selectWith fontColor" :: Text] vSel

  -- 2. heading vs labelWith fontHeading
  i2Bad <-
    renderSvg "i2-bad" 260 84 $
      panelWith (fillW . fillH . padAll 8 . gap 6) $ do
        heading "heading \"Section\" (18px, panel fg)"
        label "Uses FontRegular + fontMedium (18px)"
  i2Good <-
    renderSvg "i2-good" 260 84 $
      panelWith (fillW . fillH . padAll 8 . gap 6) $ do
        labelWith fontHeading "labelWith fontHeading (14px, accent)"
        labelWith (fontMedium . fontHeading) "fontMedium . fontHeading (18px, accent)"

  -- 3. checkbox & radio 3-surface split + modal popup shift
  i3Bad <-
    renderSvg "i3-bad" 260 100 $
      styled
        ( buttonStyle
            ( background (colorRGBA 220 38 38 255)
                . borderColor (colorRGBA 56 189 248 255)
                . foreground (colorRGBA 56 189 248 255)
            )
            . inputStyle (background (colorRGBA 4 120 87 255))
        )
        $ panelWith (fillW . fillH . padAll 8 . gap 6) $ do
          v0 <- useVar False
          v1 <- useVar True
          checkbox "Unchecked (input bg + btn border)" v0
          checkbox "Checked (accent bg)" v1
          rSel <- useVar (0 :: Int)
          radio ["Radio ring (input bg + btn border)" :: Text] rSel
  i3Good <-
    renderSvg "i3-good" 260 100 $
      styled
        ( buttonStyle (foreground (colorRGBA 56 189 248 255) . borderColor (colorRGBA 56 189 248 255))
            . popupStyle (foreground (colorRGBA 251 191 36 255) . borderColor (colorRGBA 251 191 36 255))
        )
        $ do
          vOpen <- useVar True
          modal vOpen "Inside modal (reads themePopup)" $ do
            vC <- useVar False
            checkbox "Switched to popupStyle" vC

  -- 4. select (themeButton) vs comboBox (themeInput)
  i4Bad <-
    renderSvg "i4-bad" 260 92 $
      styled probeInput $
        panelWith (fillW . fillH . padAll 8 . gap 6) $ do
          vCombo <- useVar ("comboBox (styled)" :: Text)
          vSel <- useVar (0 :: Int)
          comboBox "Pick..." ["comboBox (styled)" :: Text] vCombo
          selectWith fillW ["select (ignores inputStyle)" :: Text] vSel
  i4Good <-
    renderSvg "i4-good" 260 92 $
      styled (probeInput . buttonStyle (fillColor (colorRGBA 4 120 87 255) . foreground (colorRGBA 209 250 229 255) . borderColor (colorRGBA 52 211 153 255) . borderWidth 2)) $
        panelWith (fillW . fillH . padAll 8 . gap 6) $ do
          vCombo <- useVar ("comboBox (inputStyle)" :: Text)
          vSel <- useVar (0 :: Int)
          comboBox "Pick..." ["comboBox (inputStyle)" :: Text] vCombo
          selectWith fillW ["select (buttonStyle matched)" :: Text] vSel

  -- 5. scrollWith bounded (themeInput well, cornerRadius=0) vs grow (themeWindow, no border)
  i5Bad <-
    renderSvg "i5-bad" 260 96 $
      styled (probeInput . rounded 12) $
        columnWith (fillW . fillH . padAll 8) $
          scrollWith (fillW . fixedH 80 . padAll 6 . gap 4) $ do
            label "fixedH 80: uses themeInput well"
            label "cornerRadius forced to 0"
            label "Line 3 (scrollable)"
            label "Line 4 (scrollable)"
  i5Good <-
    renderSvg "i5-good" 260 96 $
      styled (probeInput . rounded 12) $
        columnWith (fillW . fillH . padAll 8) $
          scrollWith (grow . padAll 6 . gap 4) $ do
            label "scrollWith grow (fillW . fillH):"
            label "Drops the themeInput well and border"
            label "Paints flat themeWindow instead"

  -- 6. callout leaks panelStyle into child panels
  let warnCol = colorRGBA 235 179 56 255
  i6Bad <-
    renderSvg "i6-bad" 260 104 $
      framePad $
        callout warnCol $ do
          label "Outer Warning callout"
          card (label "Nested card inherits the yellow tint")
  i6Good <-
    renderSvg "i6-good" 260 104 $
      framePad $ do
        basePanel <- themePanel <$> uiTheme
        callout warnCol $ do
          label "Outer Warning callout"
          styled (\t -> t {themePanel = basePanel}) $
            card (label "Nested card with restored themePanel")

  -- 7. toggleButton active state overwrites buttonStyle with Tone
  i7Bad <-
    renderSvg "i7-bad" 260 80 $
      styled probeButton $
        panelWith (fillW . fillH . padAll 10 . gap 8) $
          rowWith (gap 8 . alignMid) $ do
            vOff <- useVar False
            vOn <- useVar True
            toggleButton Warning "OFF (uses buttonStyle)" vOff
            toggleButton Warning "ON (Warning wins)" vOn
  i7Good <-
    renderSvg "i7-good" 260 80 $
      styled (probeButton . probeAccent) $
        panelWith (fillW . fillH . padAll 10 . gap 8) $
          rowWith (gap 8 . alignMid) $ do
            vOff <- useVar False
            vOn <- useVar True
            toggleButton Accent "OFF (buttonStyle)" vOff
            toggleButton Accent "ON (accentColor)" vOn

  -- 8. background vs fillColor on focused textInput and hovered button
  let customTeal = colorRGBA 13 148 136 255
  i8Bad <-
    renderSvgClick "i8-bad" 260 88 (V2 80 20) $
      styled (inputStyle (background customTeal . foreground colorWhite) . buttonStyle (background customTeal . foreground colorWhite)) $
        panelWith (fillW . fillH . padAll 8 . gap 6) $ do
          vFocused <- useVar ("Focused: lost teal" :: Text)
          vUnfocused <- useVar ("Unfocused: keeps teal bg" :: Text)
          textInput vFocused
          textInput vUnfocused
  i8Good <-
    renderSvgClick "i8-good" 260 88 (V2 80 20) $
      styled (inputStyle (fillColor customTeal . foreground colorWhite) . buttonStyle (fillColor customTeal . foreground colorWhite)) $
        panelWith (fillW . fillH . padAll 8 . gap 6) $ do
          vFocused <- useVar ("Focused: fillColor kept" :: Text)
          vUnfocused <- useVar ("Unfocused: fillColor kept" :: Text)
          textInput vFocused
          textInput vUnfocused

  -- 9. borderWidth & borderSides do not add layout padding
  i9Bad <-
    renderSvg "i9-bad" 260 80 $
      framePad $
        styled (panelStyle (borderColor (colorRGBA 244 63 94 255) . borderWidth 8)) $
          panelWith (fillW . tight) $
            label "tight + borderWidth 8 overlaps text"
  i9Good <-
    renderSvg "i9-good" 260 80 $
      framePad $
        styled (panelStyle (borderLeft 6 (colorRGBA 244 63 94 255))) $
          panelWith (fillW . padLRTB 12 8 8 8) $
            label "borderLeft 6 + padLeft 12 clears stroke"

  -- 10. Composition order (.) vs nested styled scopes
  i10Bad <-
    renderSvg "i10-bad" 260 84 $
      panelWith (fillW . fillH . padAll 8 . gap 6) $ do
        rowWith (gap 6) $ do
          void (styled (primary . destructive) (button "primary . destructive"))
        rowWith (gap 6) $ do
          void (buttonWith (fixedW 110 . fixedW 220) "fixedW 110 wins")
  i10Good <-
    renderSvg "i10-good" 260 84 $
      panelWith (fillW . fillH . padAll 8 . gap 6) $ do
        rowWith (gap 6) $ do
          void (styled primary (styled destructive (button "nested: inner destructive wins")))

  -- 11. Tabs (tabHeaderVisualStyle) read themePanel, not themeButton
  i11Bad <-
    renderSvg "i11-bad" 260 96 $
      styled probeButton $
        panelWith (fillW . fillH . padAll 8 . gap 6) $ do
          vT <- useVar (0 :: Int)
          let ts = [tab (0 :: Int) "Tab A" (), tab 1 "Tab B" ()]
          tabBarConfigured (defaultTabsConfig {tabsStyle = TabUnderline}) vT ts
          tabBarConfigured (defaultTabsConfig {tabsStyle = TabContained}) vT ts
  i11Good <-
    renderSvg "i11-good" 260 96 $
      styled (probePanel . probeAccent) $
        panelWith (fillW . fillH . padAll 8 . gap 6) $ do
          vT <- useVar (0 :: Int)
          let ts = [tab (0 :: Int) "Tab A" (), tab 1 "Tab B" ()]
          tabBarConfigured (defaultTabsConfig {tabsStyle = TabUnderline}) vT ts
          tabBarConfigured (defaultTabsConfig {tabsStyle = TabContained}) vT ts

  -- 12. frameToSvg specifics: setDrawSquareGeometry flattening vs rounded geometry
  i12Bad <-
    renderSvgRound "i12-bad" 260 92 $
      styled (rounded 10) $
        panelWith (fillW . fillH . padAll 8 . gap 6) $ do
          label "SquareGeo=True flattens rx to 0:"
          rowWith (gap 8 . alignMid) $ do
            void (styled primary (button "Flat Button"))
            vSw <- useVar True
            toggleSwitch vSw
  i12Good <-
    renderSvg "i12-good" 260 92 $
      styled (rounded 10) $
        panelWith (fillW . fillH . padAll 8 . gap 6) $ do
          label "SquareGeo=False keeps rx=10:"
          rowWith (gap 8 . alignMid) $ do
            void (styled primary (button "Rounded Pill"))
            vSw <- useVar True
            toggleSwitch vSw

  pure
    [ InteractionCard
        "01"
        "typography surface-button"
        "buttonWith drops font flags"
        "NanoUI.Internal.Layout.Arena (packsNodeFont), NanoUI.Internal.Frame.Spans (nodeFontStyle)"
        "Every node shares one `styleIdx` column. Only `NodeText` and `NodeTextInput` pack font data (`FontVariant`, `FontTone`, bold, italic, underline, strike) into it. `NodeButton` and `NodeSelect` overwrite `styleIdx` with widget metadata (`buttonKindToggle`, `buttonFlagChoice`, `TabStyle`, sort arrows). `fontSize` and `fontColor` use separate arena columns, so they still work on buttons and selects."
        "Use `fontSize` and `fontColor` on `buttonWith`/`selectWith`/`checkboxWith`. For bold, italic, underline, monospace or `fontTone` inside a button, wrap a `labelWith` in `buttonContent`."
        "buttonWith (fontBold . fontUnderline . fontDanger) \"Delete\"\nselectWith (fontBold . fontDanger) [\"Item\"] sel"
        "styled (tinted Danger) $\n  buttonContent (labelWith (fontBold . fontUnderline . fontDanger) \"Delete\")\nselectWith (fontColor coral) [\"Item\"] sel"
        "fontBold, fontUnderline, fontDanger dropped"
        i1Bad
        "buttonContent for font flags, fontColor for tint"
        i1Good
    , InteractionCard
        "02"
        "typography surface-panel"
        "heading and fontHeading differ"
        "NanoUI.Internal.Widgets.Display (heading), NanoUI.Internal.Style (textToneColor)"
        "`heading` is `labelWith (tight . fontMedium)`: it sets 18px with `FontRegular`, so its color is `styleFg (themePanel theme)`. The `fontHeading` modifier sets `layoutFontVariant = FontHeading` and leaves `fontSize` at 14px; `textToneColor` paints `FontHeading` with `themeAccent`."
        "Use `fontMedium . fontHeading` for an 18px accent title, or `heading` for 18px in the panel foreground color."
        "heading \"Section\" -- 18px, styleFg(themePanel)\nlabelWith fontHeading \"Section\" -- 14px, themeAccent"
        "labelWith (fontMedium . fontHeading) \"Section\" -- 18px, themeAccent"
        "heading: 18px, panel foreground"
        i2Bad
        "fontHeading: 14px, accent"
        i2Good
    , InteractionCard
        "03"
        "surface-button surface-input surface-popup"
        "checkbox and radio span three surfaces"
        "NanoUI.Internal.Frame.Chrome (widgetVisualStyle), NanoUI.Internal.Frame.Paint.Widgets (paintCheckboxNode)"
        "Choice widgets run `clearStyle (modalAware (themeButton theme))`, which zeroes `styleBg`, `styleHoverBg`, `styleActiveBg` and `styleBorderWidth`. Paint then reads the unchecked well fill from `styleBg (themeInput theme)`, the ring and label from `themeButton`'s `styleBorder` and `styleFg`, and the checked fill and mark from `themeAccent` and `themeOnAccent`. Inside a `modal`, `modalAware` swaps the ring and label to `themePopup`."
        "Set `inputStyle` for the unchecked well, `buttonStyle` for the ring and label (plus `popupStyle` inside modals), and `accentColor` for the checked state."
        "-- buttonStyle background is stripped by clearStyle\nstyled (buttonStyle (background red)) (checkbox \"Opt\" v)"
        "styled (inputStyle (background wellBg)\n      . buttonStyle (borderColor ringCol . foreground txtCol)\n      . accentColor checkCol) (checkbox \"Opt\" v)"
        "inputStyle well, buttonStyle ring and text, accentColor check"
        i3Bad
        "inside a modal, ring and label use popupStyle"
        i3Good
    , InteractionCard
        "04"
        "surface-button surface-input"
        "select and comboBox use different surfaces"
        "NanoUI.Internal.Frame.Chrome (widgetVisualStyle)"
        "Both look the same when closed. `select` is a `NodeSelect` painted with `themeButton`; `comboBox` is a `NodeTextInput` painted with `themeInput`. An `inputStyle` scope styles `textInput`, `numericInput` and `comboBox`, so `select` is left mismatched."
        "Apply the same style to `inputStyle` and `buttonStyle`, or wrap `select` in `styled (buttonStyle ...)`."
        "-- select stays on themeButton under inputStyle\nstyled (inputStyle myFieldStyle) $ do\n  comboBox opts vCombo\n  select opts vIdx"
        "styled (inputStyle myFieldStyle . buttonStyle myFieldStyle) $ do\n  comboBox opts vCombo\n  select opts vIdx"
        "inputStyle only: comboBox changes, select stays"
        i4Bad
        "inputStyle and buttonStyle match"
        i4Good
    , InteractionCard
        "05"
        "surface-input surface-panel"
        "scrollWith changes surface with its sizing"
        "NanoUI.Internal.Frame.Paint (paintScrollSurface)"
        "When either axis is `Fixed` or `Fit`, `paintScrollSurface` draws a bordered `themeInput` well with `styleCornerRadius` forced to 0. When both axes are `Fill` (`scrollWith grow`), it treats the scroller as a full-window page scroller: it drops the border and fills with `themeWindow`."
        "Give embedded scrollers a fixed or bounded height (`fixedH`, `maxH`), or wrap a `grow` scroller in a styled `panelWith`."
        "-- both axes fill: no well, no border\nscrollWith (grow . padAll 6) body"
        "-- bounded height keeps the themeInput well\nscrollWith (fillW . fixedH 80 . padAll 6) body"
        "grow: no well, no border, themeWindow fill"
        i5Bad
        "fixedH 80: bordered themeInput well, radius 0"
        i5Good
    , InteractionCard
        "06"
        "surface-panel"
        "callout tints nested panels"
        "NanoUI.Internal.Widgets.Layout (calloutWith)"
        "`calloutWith` wraps its body in `styled (panelStyle ...)`, so every `panel` or `card` inside the callout inherits the tinted background and border. `modal` and `popup` read `themePopup`, so their children keep the normal `themePanel`."
        "Capture `basePanel <- themePanel <$> uiTheme` before the callout and wrap inner panels in `styled (\\t -> t {themePanel = basePanel})`."
        "callout Warning $ do\n  label \"Warning\"\n  card (label \"Inner card turns yellow\")"
        "basePanel <- themePanel <$> uiTheme\ncallout Warning $ do\n  label \"Warning\"\n  styled (\\t -> t {themePanel = basePanel}) (card (label \"Clean card\"))"
        "nested card inherits the Warning tint"
        i6Bad
        "themePanel restored inside the callout"
        i6Good
    , InteractionCard
        "07"
        "surface-button"
        "toggleButton ignores buttonStyle when on"
        "NanoUI.Internal.Frame.Chrome (widgetVisualStyle)"
        "When `toggleButton` is off, `widgetVisualStyle` returns `themeButton`, so `buttonStyle` applies. When it is on, `widgetVisualStyle` overwrites `styleBg`, `styleHoverBg` and `styleBorder` with `toneColor theme tone`, and `styleFg` with `readableOn theme c`."
        "Change the tone field (`accentColor` for `Accent`, or the matching tone) to color the on state."
        "-- buttonStyle only reaches the off state\nstyled (buttonStyle (fillColor blue)) (toggleButton Warning \" On\" vTrue)"
        "-- pair buttonStyle (off) with accentColor (on)\nstyled (buttonStyle (fillColor blue) . accentColor rose)\n  (toggleButton Accent \"On\" vTrue)"
        "on state ignores buttonStyle"
        i7Bad
        "buttonStyle for off, accentColor for on"
        i7Good
    , InteractionCard
        "08"
        "surface-input surface-button"
        "background resets on focus"
        "NanoUI.Internal.Style (background vs fillColor), NanoUI.Internal.Frame.Chrome (widgetVisualStyle)"
        "`background c` only sets `styleBg`. When a `textInput` or `textArea` gains focus, `widgetVisualStyle` sets its background to `styleActiveBg (themeInput theme)`, so an input styled with `background teal` loses the teal when clicked."
        "Use `fillColor` (or `tinted`) on `buttonStyle`, `inputStyle` and `popupStyle`; it updates `styleBg`, `styleHoverBg` and `styleActiveBg` together."
        "-- focus swaps the background to styleActiveBg\nstyled (inputStyle (background teal)) (textInput txt)"
        "-- fillColor updates all three background fields\nstyled (inputStyle (fillColor teal)) (textInput txt)"
        "background teal: focused field reverts"
        i8Bad
        "fillColor teal: focused field keeps the color"
        i8Good
    , InteractionCard
        "09"
        "surface-panel"
        "borderWidth does not add layout space"
        "NanoUI.Internal.Frame.Chrome (strokeStyledSides), NanoUI.Internal.Layout.Solve"
        "Theme scopes never change layout geometry. `borderWidth 8` draws an 8px inset stroke without adding padding, so on a `tight` container it paints over child text. When `borderSides` is not all four sides, `strokeStyledSides` draws flat edge quads and ignores `cornerRadius`."
        "Add matching padding (`padAll`, `padLeft`, `padLRTB`) when you increase `borderWidth`."
        "styled (panelStyle (borderWidth 8)) $\n  panelWith tight (label \"Overlapped\")"
        "styled (panelStyle (borderWidth 6 . borderLeft)) $\n  panelWith (padLRTB 12 8 8 8) (label \"Padded\")"
        "borderWidth 8 on tight: stroke over text"
        i9Bad
        "borderLeft 6 with padLRTB 12 8 8 8"
        i9Good
    , InteractionCard
        "10"
        "surface-button"
        "modifier order in (.) and styled"
        "NanoUI.Internal.Style, function composition"
        "Modifiers are functions. In `(f . g) x`, `g` runs first, then `f`, so the leftmost modifier wins: `primary . destructive` is `primary`, and `fixedW 110 . fixedW 220` is 110. Nested scopes run outside in: `styled outer (styled inner view)` makes the inner scope win."
        "Put the winning override on the left in a `(.)` chain, and innermost when nesting `styled`."
        "styled (primary . destructive) (button \"Which wins?\") -- primary\nbuttonWith (fixedW 110 . fixedW 220) \"Width?\"        -- 110px"
        "styled primary $ styled destructive $ button \"Inner destructive wins\""
        "(.) chain: leftmost runs last and wins"
        i10Bad
        "nested styled: innermost wins"
        i10Good
    , InteractionCard
        "11"
        "surface-panel surface-button"
        "tabs read themePanel, not themeButton"
        "NanoUI.Internal.Frame.Chrome (tabHeaderVisualStyle)"
        "Tab headers are `NodeButton`s, but `tabHeaderVisualStyle` computes their colors from `themePanel` and `themeAccent`. `buttonStyle` has no effect on `TabUnderline`, `TabPill` or `TabContained`; only `TabSegmented` reads `styleBorder` from `themeButton` for its active border and luminance comparison."
        "Style tabs with `panelStyle` and `accentColor`."
        "-- buttonStyle does not reach TabUnderline or TabContained\nstyled (buttonStyle myStyle) (tabBar active tabsList)"
        "-- panelStyle and accentColor style the tab strip\nstyled (panelStyle myPanel . accentColor myAccent) (tabBar active tabsList)"
        "buttonStyle: TabUnderline and TabContained unchanged"
        i11Bad
        "panelStyle and accentColor: headers respond"
        i11Good
    , InteractionCard
        "12"
        "framesvg"
        "frameToSvg quirks"
        "NanoUI.Draw.FrameSvg (frameToSvg), NanoUI.Backend.Sdl (withSdl), NanoUI.Testing (newPixelContext)"
        "Three things matter for `frameToSvg`. `setDrawSquareGeometry ctx True` makes rounded rects and strokes emit 0-radius quads, flattening `cornerRadius` on `button`, `panel` and `textInput`; `NodeDrawing` widgets still render smooth rects and circles from cached ops. `newPixelContext` defaults to 16px monospace metrics, so load `inter.ttf` through `withSdl` and embed it with `@font-face` for matching `textLength`. Every document reuses clip and gradient IDs (`tclip_1`, `sclip_1`, `clip-0`, `grad-0`), so namespace them when inlining several SVGs."
        "Use `setDrawSquareGeometry ctx False`, render inside `withSdl` with `DefaultFont` and `@font-face`, and prefix clip and gradient IDs per document."
        "setDrawSquareGeometry ctx True -- flattens button and panel radius to 0\n-- newPixelContext's 16px monospace metrics distort textLength"
        "-- Render inside withSdl with DefaultFont and @font-face\nctx <- newContext\nsetDrawExternalText ctx True"
        "square geometry on: rounded corners flattened"
        i12Bad
        "square geometry off: rounded corners preserved"
        i12Good
    ]

-- ---------------------------------------------------------------------------
-- Interactive States & Styles Playground (6 States x 32 Style Masks = 192 Frames)
-- ---------------------------------------------------------------------------

applyStateStyleMask :: Int -> NanoUI () -> NanoUI ()
applyStateStyleMask smask view0 =
  let hasThemeMod = (smask .&. 0x0F) /= 0
      themeMod =
        (if testBit smask 3 then probeAccent else id)
          . (if testBit smask 2 then probePopup else id)
          . (if testBit smask 1 then probeInput else id)
          . (if testBit smask 0 then probeButton else id)
      applyStyled = if hasThemeMod then styled themeMod else id
      applyDis = if testBit smask 4 then disabledWhen True else id
   in applyStyled (applyDis view0)

generateStateStrip :: IO (Text, Value)
generateStateStrip = do
  let w = 440
      h = 122
      buildView :: Bool -> NanoUI ()
      buildView focusBtn =
        panelWith (fillW . fillH . padAll 10 . gap 8) $ do
          rowWith (gap 10 . alignMid . fillW) $ do
            rBtn <- buttonWith' (fixedW 195) "Action Button"
            if focusBtn then requestFocus (respId rBtn) else pure ()
            vInp <- useVar ("Editable Input" :: Text)
            textInputConfigured (defaultTextInputConfig {ticLayout = fillW defaultLayout}) vInp
          rowWith (gap 10 . alignMid . fillW) $ do
            vSel <- useVar (0 :: Int)
            selectWith (fixedW 195) ["Select (themeButton)" :: Text, "Popup Row 2", "Popup Row 3"] vSel
            label "Click a state or style pill"

      renderOneState :: Int -> Int -> IO Text
      renderOneState stIdx smask = do
        let ui = applyStateStyleMask smask (buildView (stIdx == 3))
            tag = "wb-state"
        case stIdx of
          0 -> renderSvg tag w h ui
          1 -> renderSvgHover tag w h (V2 95 22) ui
          2 -> renderSvgPress tag w h (V2 95 22) ui
          3 -> renderSvg tag w h ui
          4 -> renderSvgClick tag w h (V2 310 22) ui
          _ -> renderSvgClick tag w h (V2 95 54) ui

  svgs <-
    sequence
      [ renderOneState stIdx smask
      | stIdx <- [0 .. 5]
      , smask <- [0 .. 31]
      ]
  let baseSvg = case svgs of
        (s0 : _) -> s0
        [] -> ""
  pure (baseSvg, packSvgBank svgs)

-- ---------------------------------------------------------------------------
-- Hero Interactive All-Surface Workbench (128 Combinations)
-- ---------------------------------------------------------------------------

generateRainbowBanner :: IO (Text, Value)
generateRainbowBanner = do
  let compositeView :: NanoUI ()
      compositeView =
        panelWith (fillW . fillH . padAll 10 . gap 8) $ do
          rowWith (gap 8 . alignMid . fillW) $ do
            heading "Surface Anatomy"
            void (button "Button")
            vOn <- useVar True
            toggleButton Accent "Active" vOn
            vChk <- useVar False
            checkbox "Check" vChk
            vChk2 <- useVar True
            checkbox "On" vChk2
          rowWith (gap 8 . alignMid . fillW) $ do
            vTxt <- useVar ("textInput (themeInput)" :: Text)
            textInputConfigured (defaultTextInputConfig {ticLayout = fixedW 170 defaultLayout}) vTxt
            vSel <- useVar (0 :: Int)
            selectWith (fixedW 130) ["select (btn)" :: Text, "Item 2"] vSel
            vSl <- useVar (60 :: Float)
            sliderWith fillW 0 100 vSl
  renderStyleBank "hero" 520 96 False compositeView

-- ---------------------------------------------------------------------------
-- HTML Generation
-- ---------------------------------------------------------------------------

-- | 'show' as text, for ids and attribute values.
-- | Escaped text as HTML content.
txt :: Text -> Html ()
txt = toHtml

showT :: Show a => a -> Text
showT = T.pack . show

-- | Pointer enter/leave handlers, which Lucid does not name.
onmouseenter_ :: Text -> Attributes
onmouseenter_ = makeAttributes "onmouseenter"

onmouseleave_ :: Text -> Attributes
onmouseleave_ = makeAttributes "onmouseleave"

-- | Snippet markup for prose and table cells: escaped text, @<code>@, surface
-- badges, line breaks, and links. Interaction notes spell out identifiers with
-- @`backticks`@; 'prose' turns those into code elements rather than requiring
-- hand-written markup in every string.
data Snippet
  = Txt !Text
  | Code !Text
  | Badge !Text !Text
  | Brk
  | Link !Text ![Snippet]

inline :: [Snippet] -> Html ()
inline = mapM_ go
  where
    go = \case
      Txt t -> toHtml t
      Code t -> code_ (toHtml t)
      Badge cls t -> span_ [class_ ("badge " <> cls)] (toHtml t)
      Brk -> br_ []
      Link href xs -> a_ [href_ href] (inline xs)

-- | Render @`backticked`@ runs as @<code>@ and everything else as escaped text.
prose :: Text -> Html ()
prose t = sequence_ [if odd i then code_ (toHtml piece) else toHtml piece | (i, piece) <- zip [0 :: Int ..] (T.splitOn "`" t)]

-- | A section heading row: inline-markup title and summary.
sectionHead :: [Snippet] -> [Snippet] -> Html ()
sectionHead title summary = div_ [class_ "section-head"] $ do
  h2_ (inline title)
  span_ (inline summary)

-- | Section I shell: the state/focus workbench. The frames themselves are
-- pre-rendered; the script swaps them into @#state-svg-well@.
stateStripSection :: Text -> Html ()
stateStripSection baseSvg = do
  div_ [class_ "workbench-card", id_ "state-workbench"] $ do
    div_ [class_ "wb-grid"] $ do
      div_ [class_ "wb-viewport-col"] $ do
        div_ [class_ "wb-viewport-head"] $ do
          span_ [class_ "wb-active-label", id_ "state-active-label"] "State: Resting · defaultTheme"
          button_ [type_ "button", class_ "mini-btn", onclick_ "resetStateWorkbench()"] "Reset"
        div_ [class_ "svg-well wb-svg-stage", id_ "state-svg-well"] (toHtmlRaw baseSvg)
        div_ [class_ "wb-code-bar"] (code_ [id_ "state-code-preview"] "-- Resting state under defaultTheme")
      div_ [class_ "wb-controls-col"] $ do
        div_ [class_ "control-group-label"] "1. Choose a state:"
        div_ [class_ "state-pill-grid", id_ "state-mode-pills"] $
          sequence_ [statePill i title detail | (i, (title, detail)) <- zip [0 :: Int ..] statePills]
        div_ [class_ "control-group-label", style_ "margin-top:12px"] "2. Toggle modifiers:"
        div_ [class_ "toggle-chip-grid", id_ "state-style-toggles"] $
          mapM_ stateStyleChip stateChips
        div_ [class_ "wb-inspector", id_ "state-inspector"] mempty
  where
    statePills =
      [ ("Resting", "styleBg + styleBorder")
      , ("Hovered", "styleHoverBg")
      , ("Pressed", "styleActiveBg")
      , ("Focused", "outer themeFocusRing")
      , ("Focused input", "accent border, active fill")
      , ("Open select", "button box, popup menu")
      ]
    statePill :: Int -> Text -> Text -> Html ()
    statePill i title detail =
      button_
        [ type_ "button"
        , class_ (if i == 0 then "state-pill active" else "state-pill")
        , data_ "state" (showT i)
        , onclick_ ("setStateMode(" <> showT i <> ")")
        ]
        $ do
          strong_ (toHtml title)
          span_ (toHtml detail)
    stateChips =
      [ (0 :: Int, "buttonStyle", "btn", "impact-primary", "themeButton")
      , (1, "inputStyle", "inp", "impact-primary", "themeInput")
      , (2, "popupStyle", "pop", "impact-partial", "Open menu")
      , (3, "accentColor", "acc", "impact-partial", "Focus")
      , (4, "disabledWhen", "dis", "impact-surprise", "Blocked")
      ]
    stateStyleChip :: (Int, Text, Text, Text, Text) -> Html ()
    stateStyleChip (bit, name, probe, toneCls, tag) =
      button_
        [ type_ "button"
        , class_ "style-chip"
        , data_ "sbit" (showT bit)
        , data_ "probe" probe
        , onclick_ ("toggleStateStyle(" <> showT bit <> ")")
        ]
        $ do
          span_ [class_ "chip-check"] mempty
          span_ [class_ "chip-name"] (toHtml name)
          span_ [class_ ("chip-tag " <> toneCls)] (toHtml tag)


-- | A style toggle chip: checkbox square, label, and an optional impact tag.
-- @attrs@ carry the per-workbench data attributes and click handler.
styleChip :: [Attributes] -> Text -> Maybe (Text, Text) -> Html ()
styleChip attrs name mTag =
  button_ ([type_ "button", class_ "style-chip"] <> attrs) $ do
    span_ [class_ "chip-check"] mempty
    span_ [class_ "chip-name"] (toHtml name)
    case mTag of
      Just (toneCls, tag) -> span_ [class_ ("chip-tag " <> toneCls)] (toHtml tag)
      Nothing -> mempty

-- | A small action button.
miniBtn :: [Attributes] -> Text -> Html ()
miniBtn attrs btnLabel = button_ ([type_ "button", class_ "mini-btn"] <> attrs) (toHtml btnLabel)

-- | One per-widget workbench card: metadata, live expression, stage, and the
-- seven style toggles.
probeRowCard :: ProbeRow -> Html ()
probeRowCard pr = do
  article_
    [ class_ "probe-row filter-item"
    , id_ ("widget-" <> wid)
    , data_ "wid" wid
    , data_ "cat" (prCategory pr)
    ]
    $ do
      div_ [class_ "probe-card-grid"] $ do
        div_ [class_ "probe-meta"] $ do
          div_ [class_ "probe-title-line"] $ do
            h3_ [class_ "probe-title"] (toHtml (prTitle pr))
            span_ [class_ "surface-pill"] (toHtml (prPrimarySurface pr))
          code_ [class_ "probe-sig"] (toHtml (prSignature pr))
          p_ [class_ "probe-desc"] (prose (prSummary pr))
          div_ [class_ "wb-code-bar"] $ do
            div_ [class_ "code-bar-head"] (span_ "Live Haskell Expression")
            pre_ (code_ [id_ ("code-" <> wid)] (toHtml (prCodeExpr pr)))
        div_ [class_ "probe-stage-col"] $ do
          div_ [class_ "wb-viewport-head"] $ do
            span_ [class_ "wb-active-label", id_ ("status-" <> wid)] "defaultTheme (0 modifiers)"
            div_ [class_ "wb-head-actions"] $ do
              miniBtn
                [ id_ ("cmp-btn-" <> wid)
                , onclick_ ("toggleWidgetCompare('" <> wid <> "')")
                , title_ "Show defaultTheme baseline next to active SVG"
                ]
                "⇄ Compare"
              miniBtn [onclick_ ("setWidgetMask('" <> wid <> "', 0)")] "Reset"
          div_ [class_ "wb-dual-stage", id_ ("dual-" <> wid)] $ do
            div_ [class_ "svg-well wb-baseline-well hidden-compare", id_ ("base-well-" <> wid)] $ do
              div_ [class_ "well-corner-tag"] "Baseline (defaultTheme)"
              div_ [class_ "well-svg-inner", id_ ("base-svg-" <> wid)] mempty
            div_ [class_ "svg-well wb-active-well", id_ ("svg-" <> wid)] (toHtmlRaw (prBaselineSvg pr))
        div_ [class_ "probe-controls-col"] $ do
          div_ [class_ "control-group-label"] "Toggle modifiers:"
          div_ [class_ "toggle-chip-grid"] $ mapM_ (toggleChip wid) (prToggles pr)
          div_ [class_ "wb-inspector", id_ ("notes-" <> wid)] mempty
  where
    wid = prId pr
    toggleChip :: Text -> ToggleNote -> Html ()
    toggleChip w tn =
      styleChip
        [ data_ "wid" w
        , data_ "bit" (showT (tnBit tn))
        , data_ "probe" (tnKey tn)
        , data_ "impact" (impactCode (tnImpact tn))
        , onmouseenter_ ("previewWidgetNote('" <> w <> "'," <> showT (tnBit tn) <> ")")
        , onmouseleave_ ("clearWidgetPreview('" <> w <> "')")
        , onclick_ ("toggleWidgetBit('" <> w <> "'," <> showT (tnBit tn) <> ")")
        ]
        (tnShort tn)
        (Just ("impact-" <> impactCode (tnImpact tn), impactBadgeText (tnImpact tn)))

-- | Section IV: the 64-combination typography workbench plus reference cards.
typoSection :: TypoSectionData -> Html ()
typoSection tsd = do
  div_
    [ class_ "workbench-card filter-item"
    , data_ "cat" "typography"
    , id_ "typo-workbench"
    , style_ "margin-bottom:16px"
    ]
    $ do
      div_ [class_ "wb-grid"] $ do
        div_ [class_ "wb-viewport-col"] $ do
          div_ [class_ "wb-viewport-head"] $ do
            span_ [class_ "wb-active-label", id_ "typo-active-label"] "No modifiers"
            miniBtn [onclick_ "setTypoMask(0)"] "Reset"
          div_ [class_ "svg-well wb-svg-stage", id_ "typo-svg-well"] (toHtmlRaw (tsdBaseSvg tsd))
          div_ [class_ "wb-code-bar"] (code_ [id_ "typo-code-preview"] "let m = id -- toggle modifiers on the right")
        div_ [class_ "wb-controls-col"] $ do
          div_ [class_ "control-group-label"] "Toggle modifiers:"
          p_ [style_ "margin:0 0 10px 0;font-size:13px;color:var(--ink-secondary)"] $
            inline
              [ Code "labelWith", Txt ", ", Code "textInput", Txt " and ", Code "buttonContent"
              , Txt " take every modifier; ", Code "buttonWith", Txt " ignores the font flags."
              ]
          div_ [class_ "toggle-chip-grid", id_ "typo-toggles"] $ mapM_ typoChip typoChips
          div_ [style_ "margin-top:10px;display:flex;gap:6px;flex-wrap:wrap"] $ do
            miniBtn [onclick_ "setTypoMask(63)"] "Enable all"
            miniBtn [onclick_ "setTypoMask(21)"] "Bold, underline, danger"
            miniBtn [onclick_ "setTypoMask(32)"] "fontSize 16"
  mapM_ typoCard (tsdDemos tsd)
  where
    typoChips =
      [ (0 :: Int, "fontBold", "impact-partial", "Text")
      , (1, "fontItalic", "impact-partial", "Text")
      , (2, "fontUnderline", "impact-partial", "Text")
      , (3, "fontStrike", "impact-partial", "Text")
      , (4, "fontTone Danger", "impact-partial", "Text")
      , (5, "fontSize 16", "impact-primary", "All rows")
      ]
    typoChip :: (Int, Text, Text, Text) -> Html ()
    typoChip (bit, name, toneCls, tag) =
      styleChip
        [ data_ "tbit" (showT bit)
        , onclick_ ("toggleTypoBit(" <> showT bit <> ")")
        ]
        name
        (Just (toneCls, tag))
    typoCard :: TypoDemo -> Html ()
    typoCard d =
      div_ [class_ "typo-card filter-item", data_ "cat" "typography"] $ do
        div_ [class_ "typo-info"] $ do
          h3_ (toHtml (tdTitle d))
          code_ [class_ "typo-code"] (toHtml (tdCode d))
          p_ (toHtml (tdNote d))
        div_ [class_ "svg-well typo-svg"] (toHtmlRaw (tdSvg d))

-- | Section V: the twelve unexpected-interaction cards.
interactionsSection :: [InteractionCard] -> Html ()
interactionsSection = mapM_ trapCard
  where
    trapCard :: InteractionCard -> Html ()
    trapCard c = do
      let tid = icNumber c
      article_
        [ class_ "trap-card filter-item"
        , id_ ("trap-" <> tid)
        , data_ "cat" ("trap " <> icCategory c)
        ]
        $ do
          div_ [class_ "trap-header"] $ do
            div_ [style_ "display:flex;justify-content:space-between;align-items:center;flex-wrap:wrap;gap:8px"] $ do
              span_ [class_ "trap-num"] ("INTERACTION #" <> toHtml tid)
              div_ [class_ "trap-mode-bar", data_ "trap" tid] $ do
                button_ [type_ "button", class_ "mini-btn active", onclick_ ("setTrapView('" <> tid <> "','bad',this)")] "⚠ Show Trap"
                button_ [type_ "button", class_ "mini-btn", onclick_ ("setTrapView('" <> tid <> "','good',this)")] "✓ Show Fix"
                button_ [type_ "button", class_ "mini-btn", onclick_ ("setTrapView('" <> tid <> "','split',this)")] "⇄ Side-by-Side"
            h3_ [class_ "trap-title"] (toHtml (icTitle c))
            code_ [class_ "trap-where"] (toHtml (icWhere c))
          div_ [class_ "trap-body"] $ do
            div_ [class_ "trap-prose"] $ do
              div_ [class_ "trap-block"] $ do
                strong_ "Why it happens:"
                txt " "
                prose (icWhy c)
              div_ [class_ "trap-block trap-fix"] $ do
                strong_ "How to handle it:"
                txt " "
                prose (icFix c)
              div_ [class_ "trap-code-grid"] $ do
                div_ [class_ "code-pane code-bad"] $ do
                  div_ [class_ "code-tag"] "Unexpected / Trap"
                  pre_ (code_ (toHtml (icCodeBad c)))
                div_ [class_ "code-pane code-good"] $ do
                  div_ [class_ "code-tag"] "Recommended Pattern"
                  pre_ (code_ (toHtml (icCodeGood c)))
            div_ [class_ "trap-visuals single-view", id_ ("trap-vis-" <> tid)] $ do
              div_ [class_ "trap-svg-box trap-svg-bad", id_ ("trap-bad-" <> tid)] $ do
                div_ [class_ "trap-svg-label"] (toHtml (icLabelA c))
                div_ [class_ "svg-well"] (toHtmlRaw (icSvgA c))
              div_ [class_ "trap-svg-box trap-svg-good hidden-compare", id_ ("trap-good-" <> tid)] $ do
                div_ [class_ "trap-svg-label"] (toHtml (icLabelB c))
                div_ [class_ "svg-well"] (toHtmlRaw (icSvgB c))

instance ToJSON ToggleNote where
  toJSON tn =
    object
      [ "bit" .= tnBit tn
      , "key" .= tnKey tn
      , "label" .= tnLabel tn
      , "short" .= tnShort tn
      , "impact" .= impactCode (tnImpact tn)
      , "badge" .= impactBadgeText (tnImpact tn)
      , "detail" .= tnDetail tn
      ]

instance ToJSON InteractionCard where
  toJSON c =
    object
      [ "id" .= icNumber c
      , "category" .= icCategory c
      , "title" .= icTitle c
      , "where" .= icWhere c
      , "why" .= icWhy c
      , "fix" .= icFix c
      , "codeBad" .= icCodeBad c
      , "codeGood" .= icCodeGood c
      , "labelA" .= icLabelA c
      , "svgA" .= icSvgA c
      , "labelB" .= icLabelB c
      , "svgB" .= icSvgB c
      ]

buildWidgetMetaValue :: [ProbeRow] -> Value
buildWidgetMetaValue rows =
  object
    [ prId pr
        .= object
          [ "expr" .= prCodeExpr pr
          , "isOverlay" .= prIsOverlay pr
          , "notes" .= prToggles pr
          ]
    | pr <- rows
    ]

buildSvgBanksValue :: Value -> Value -> [ProbeRow] -> TypoSectionData -> Value
buildSvgBanksValue heroBank stateBank rows tsd =
  object $
    [ "hero" .= heroBank
    , "state" .= stateBank
    , "typo" .= tsdBankValue tsd
    ]
      ++ [prId pr .= prBankValue pr | pr <- rows]

buildTrapsValue :: [InteractionCard] -> Value
buildTrapsValue = toJSON

-- | The @aria-label@ attribute, which Lucid does not name.
ariaLabel_ :: Text -> Attributes
ariaLabel_ = makeAttributes "aria-label"

-- | A sidebar navigation list.
navList :: [(Text, Text)] -> Html ()
navList items =
  ul_ [class_ "nav-list"] $
    mapM_ (\(href, navLabel) -> li_ (a_ [href_ href] (toHtml navLabel))) items

-- | A surface-probe legend row.
legendRow :: Text -> Text -> Text -> Html ()
legendRow bg border rowLabel =
  div_ [class_ "legend-row"] $ do
    span_ [class_ "swatch", style_ ("background:" <> bg <> ";border:2px solid " <> border)] mempty
    span_ (toHtml rowLabel)

-- | The fixed sidebar: brand, probe colour key, and section/trap links.
sidebarNav :: Html ()
sidebarNav = do
  div_ [class_ "brand-kicker"] "nano-ui-svg Interactive Lab"
  h1_ [class_ "brand-title"] "Styling & Surface Glossary"
  p_ [class_ "brand-sub"] $ do
    txt "Toggle any combination of styles. Every state is a real headless "
    code_ "NanoUI ()"
    txt " frame exported with "
    code_ "frameToSvg"
    txt "."
  div_ [class_ "legend-box"] $ do
    div_ [class_ "nav-group-label", style_ "margin-top:0"] "Surface Probe Color Key"
    legendRow "#0369a1" "#38bdf8" "themeButton (buttonStyle)"
    legendRow "#047857" "#34d399" "themeInput (inputStyle)"
    legendRow "#581c87" "#c084fc" "themePanel (panelStyle)"
    legendRow "#b45309" "#fbbf24" "themePopup (popupStyle)"
    legendRow "#e11d48" "#fb7185" "themeAccent (accentColor)"
  div_ [class_ "nav-group-label"] "Sections"
  navList
    [ ("#sec-states", "I. Interaction States")
    , ("#sec-routing", "II. Surface Routing")
    , ("#sec-matrix", "III. Widget Styles")
    , ("#sec-typo", "IV. Typography")
    , ("#sec-traps", "V. Unexpected Interactions")
    ]
  div_ [class_ "nav-group-label"] "Unexpected Interactions"
  navList trapLinks
  where
    trapLinks =
      [ ("#trap-01", "#01 buttonWith Ignores Font Flags")
      , ("#trap-02", "#02 heading vs fontHeading")
      , ("#trap-03", "#03 checkbox 3-Surface + Modal Shift")
      , ("#trap-04", "#04 select vs comboBox Surface")
      , ("#trap-05", "#05 scrollWith grow Loses Well")
      , ("#trap-06", "#06 callout Leaks panelStyle")
      , ("#trap-07", "#07 toggleButton ON Overwrites Style")
      , ("#trap-08", "#08 background vs fillColor Focus Bug")
      , ("#trap-09", "#09 borderWidth Zero Inset & Sides")
      , ("#trap-10", "#10 (.) Order vs Nested styled")
      , ("#trap-11", "#11 tabs Read themePanel, Not Button")
      , ("#trap-12", "#12 frameToSvg Geometry & FontMetrics")
      ]

-- | The top banner: intro copy and the live composite specimen.
heroSection :: Text -> Html ()
heroSection heroBaseSvg =
  header_ [class_ "hero-banner", id_ "hero-workbench"] $ do
    div_ [class_ "hero-top"] $
      div_ $ do
        h1_ [class_ "hero-title"] $ do
          code_ "nano-ui"
          txt " Styling & Surfaces"
        p_ [class_ "hero-lead"] $ do
          txt "Widgets have no per-widget CSS. A "
          code_ "Theme"
          txt " holds five surface "
          code_ "Style"
          txt " records ("
          code_ "themeButton"
          txt ", "
          code_ "themeInput"
          txt ", "
          code_ "themePanel"
          txt ", "
          code_ "themePopup"
          txt ", "
          code_ "themeFloatingWindow"
          txt "). Toggle styles to see which surface each widget reads."
    div_ [class_ "wb-grid"] $ do
      div_ [class_ "wb-viewport-col"] $ do
        div_ [class_ "wb-viewport-head"] $ do
          span_ [class_ "wb-active-label", id_ "hero-status"] "Composite · defaultTheme (0 modifiers)"
          div_ [class_ "wb-head-actions"] $ do
            miniBtn [onclick_ "setHeroMask(30)"] "All surfaces"
            miniBtn [onclick_ "setHeroMask(0)"] "Reset"
        div_ [class_ "svg-well", id_ "hero-svg-well"] (toHtmlRaw heroBaseSvg)
        div_ [class_ "wb-code-bar"] (code_ [id_ "hero-code-preview"] "compositeDashboardView -- toggle modifiers on the right")
      div_ [class_ "wb-controls-col"] $ do
        div_ [class_ "control-group-label"] "Toggle modifiers:"
        div_ [class_ "toggle-chip-grid", id_ "hero-toggles"] $ mapM_ heroChip heroChips
  where
    heroChips =
      [ (0 :: Int, "light", "Light Theme", "impact-primary", "Base")
      , (1, "btn", "buttonStyle", "impact-primary", "Button")
      , (2, "inp", "inputStyle", "impact-primary", "Input")
      , (3, "pnl", "panelStyle", "impact-primary", "Panel")
      , (4, "acc", "accentColor", "impact-primary", "Accent")
      , (5, "rnd", "rounded 999", "impact-partial", "Partial")
      , (6, "dis", "disabledWhen", "impact-primary", "Dims")
      ]
    heroChip :: (Int, Text, Text, Text, Text) -> Html ()
    heroChip (bit, probe, name, toneCls, tag) =
      styleChip
        [ data_ "hbit" (showT bit)
        , data_ "probe" probe
        , onclick_ ("toggleHeroBit(" <> showT bit <> ")")
        ]
        name
        (Just (toneCls, tag))

-- | The section filter toolbar.
filterBar :: Html ()
filterBar = do
  div_ [class_ "filter-bar", role_ "toolbar", ariaLabel_ "Filter glossary"] $ do
    strong_ [style_ "font-family:var(--font-mono);font-size:12px;margin-right:4px;"] "Filter Sections:"
    filterBtn True "all" "All Specimens"
    filterBtn False "surface-button" "themeButton (buttonStyle)"
    filterBtn False "surface-input" "themeInput (inputStyle)"
    filterBtn False "surface-panel" "themePanel (panelStyle)"
    filterBtn False "typography" "Typography"
    filterBtn False "trap" "12 Unexpected Interactions"
    filterBtn False "framesvg" "frameToSvg Quirks"
  where
    filterBtn :: Bool -> Text -> Text -> Html ()
    filterBtn active cat btnLabel =
      button_
        [ class_ (if active then "filter-btn active" else "filter-btn")
        , onclick_ ("filterGlossary('" <> cat <> "', this)")
        ]
        (toHtml btnLabel)

-- | Section II: the widget-to-surface routing matrix.
routingTable :: Html ()
routingTable =
  div_ [class_ "routing-table-wrap"] $
    table_ [class_ "routing-table"] $ do
      thead_ (tr_ (mapM_ (\h -> th_ (toHtml h)) routingHeader))
      tbody_ (mapM_ (\cells -> tr_ (mapM_ (td_ . inline) cells)) routingRows)

routingHeader :: [Text]
routingHeader =
  [ "Widget"
  , "Arena NodeType"
  , "Resting Fill / Well"
  , "Border / Outline"
  , "Text / Glyph Color"
  , "Active / Check / Focus Behavior"
  , "Gotchas / Traps"
  ]

-- | One row per widget family; each cell is inline markup.
routingRows :: [[[Snippet]]]
routingRows =
  [ [ [Code "button", Txt ", ", Code "iconButton"]
    , [Code "NodeButton"]
    , [Badge "b-btn" "themeButton", Txt " ", Code "styleBg"]
    , [Badge "b-btn" "themeButton", Txt " ", Code "styleBorder"]
    , [Badge "b-btn" "themeButton", Txt " ", Code "styleFg"]
    , [Txt "Hover: ", Code "styleHoverBg", Txt "; Press: ", Code "styleActiveBg", Txt "; Focus: outer 2px ", Code "themeFocusRing"]
    , [Link "#trap-01" [Txt "Ignores ", Code "fontBold", Txt "/", Code "fontTone", Txt " in ", Code "buttonWith", Txt " (#01)"]]
    ]
  , [ [Code "toggleButton"]
    , [Code "NodeButton", Txt " (toggle)"]
    , [Txt "OFF: ", Badge "b-btn" "themeButton", Brk, Txt "ON: ", Badge "b-acc" "toneColor"]
    , [Txt "OFF: ", Badge "b-btn" "themeButton", Brk, Txt "ON: ", Badge "b-acc" "toneColor"]
    , [Txt "OFF: ", Badge "b-btn" "themeButton", Brk, Txt "ON: ", Code "readableOn"]
    , [Txt "When ON, overwrites ", Code "styleBg", Txt ", ", Code "styleHoverBg", Txt ", ", Code "styleBorder", Txt ", ", Code "styleFg"]
    , [Link "#trap-07" [Txt "ON state ignores ", Code "buttonStyle", Txt " colors (#07)"]]
    ]
  , [ [Code "checkbox", Txt ", ", Code "radio"]
    , [Code "NodeButton", Txt " (choice)"]
    , [Txt "Unchecked well: ", Badge "b-inp" "themeInput", Txt " ", Code "styleBg"]
    , [Txt "Unchecked ring/box: ", Badge "b-btn" "themeButton", Txt " ", Code "styleBorder"]
    , [Txt "Label: ", Badge "b-btn" "themeButton", Txt " ", Code "styleFg"]
    , [Txt "Checked box/dot: ", Badge "b-acc" "themeAccent", Txt " + ", Code "themeOnAccent"]
    , [Link "#trap-03" [Txt "3-surface split + switches to ", Code "themePopup", Txt " in ", Code "modal", Txt " (#03)"]]
    ]
  , [ [Code "select"]
    , [Code "NodeSelect"]
    , [Badge "b-btn" "themeButton", Txt " ", Code "styleBg"]
    , [Badge "b-btn" "themeButton", Txt " ", Code "styleBorder"]
    , [Badge "b-btn" "themeButton", Txt " ", Code "styleFg", Txt " (text & chevron)"]
    , [Txt "Focused border: ", Badge "b-acc" "themeAccent", Txt "; Open menu: ", Badge "b-pop" "themePopup"]
    , [Link "#trap-04" [Txt "Uses ", Code "themeButton", Txt ", not ", Code "themeInput", Txt " (#04)"]]
    ]
  , [ [Code "comboBox"]
    , [Code "NodeTextInput"]
    , [Badge "b-inp" "themeInput", Txt " ", Code "styleBg"]
    , [Badge "b-inp" "themeInput", Txt " ", Code "styleBorder"]
    , [Txt "Text: ", Badge "b-inp" "themeInput", Txt " ", Code "styleFg", Txt "; Chevron: 45% lerp to bg"]
    , [Txt "Focused border: ", Badge "b-acc" "themeAccent", Txt ", bg: ", Code "styleActiveBg", Txt "; Menu: ", Badge "b-pop" "themePopup"]
    , [Link "#trap-04" [Txt "Paired with ", Code "select", Txt " but on ", Code "themeInput", Txt " (#04)"]]
    ]
  , [ [Code "textInput", Txt ", ", Code "searchInput", Txt ", ", Code "numericInput", Txt ", ", Code "textArea"]
    , [Code "NodeTextInput", Txt " / ", Code "NodeTextArea"]
    , [Badge "b-inp" "themeInput", Txt " ", Code "styleBg"]
    , [Badge "b-inp" "themeInput", Txt " ", Code "styleBorder"]
    , [Badge "b-inp" "themeInput", Txt " ", Code "styleFg", Txt " (placeholder 40% lerp to bg)"]
    , [Txt "Focused: swaps border to ", Badge "b-acc" "themeAccent", Txt " & bg to ", Code "styleActiveBg(themeInput)", Txt " (no outer ring)"]
    , [Link "#trap-08" [Code "background", Txt " without ", Code "fillColor", Txt " reverts on focus (#08)"]]
    ]
  , [ [Code "slider"]
    , [Code "NodeSlider"]
    , [Txt "35% lerp from ", Badge "b-inp" "themeInput", Txt " ", Code "styleBg", Txt " to ", Code "styleBorder"]
    , [Badge "b-inp" "themeInput", Txt " ", Code "styleBorder", Txt " (1px fixed)"]
    , [Txt "Thumb fill: ", Code "themeOnAccent"]
    , [Txt "Filled bar: ", Badge "b-acc" "themeAccent", Txt "; Focus: outer ", Code "themeFocusRing"]
    , [Txt "Ignores ", Code "borderWidth", Txt " and ", Code "cornerRadius", Txt " (hardcoded 1px / 3px)"]
    ]
  , [ [Code "panel", Txt ", ", Code "card", Txt ", ", Code "callout"]
    , [Code "NodePanel"]
    , [Badge "b-pnl" "themePanel", Txt " ", Code "styleBg"]
    , [Badge "b-pnl" "themePanel", Txt " ", Code "styleBorder"]
    , [Txt "Inherited by child ", Code "label", Txt "s via ", Badge "b-pnl" "themePanel", Txt " ", Code "styleFg"]
    , [Txt "Clips children to ", Code "borderContentClip"]
    , [Link "#trap-06" [Code "callout", Txt " leaks tinted ", Code "panelStyle", Txt " to child panels (#06)"]]
    ]
  , [ [Code "label", Txt ", ", Code "heading", Txt ", ", Code "kv"]
    , [Code "NodeText"]
    , [Txt "Transparent (except in ", Code "frameToSvg", Txt " or striped tables)"]
    , [Txt "None"]
    , [Badge "b-pnl" "themePanel", Txt " ", Code "styleFg", Txt " (or ", Code "fontColor", Txt " / ", Code "fontTone", Txt ")"]
    , [Code "fontHeading", Txt " uses ", Badge "b-acc" "themeAccent", Txt "; ", Code "heading", Txt " uses ", Badge "b-pnl" "themePanel", Txt " ", Code "styleFg"]
    , [ Link "#trap-02" [Code "heading", Txt " vs ", Code "fontHeading", Txt " (#02)"]
      , Txt " · "
      , Link "#trap-12" [Txt "SVG bg rect (#12)"]
      ]
    ]
  , [ [Code "scrollWith"]
    , [Code "NodeScroll"]
    , [Txt "Bounded: ", Badge "b-inp" "themeInput", Txt " ", Code "styleBg", Brk, Code "grow", Txt ": ", Code "themeWindow"]
    , [Txt "Bounded: ", Badge "b-inp" "themeInput", Txt " ", Code "styleBorder", Brk, Code "grow", Txt ": None"]
    , [Txt "Scrollbar blends ", Badge "b-inp" "themeInput", Txt " + ", Code "themeSeparator"]
    , [Txt "Always forces ", Code "styleCornerRadius = 0", Txt " on the scroll well"]
    , [Link "#trap-05" [Code "scrollWith grow", Txt " drops well & border (#05)"]]
    ]
  , [ [Code "tabs", Txt ", ", Code "tabBar"]
    , [Code "NodeButton", Txt " (tab)"]
    , [Badge "b-pnl" "themePanel", Txt " / ", Badge "b-acc" "themeAccent"]
    , [Badge "b-pnl" "themePanel", Txt " (", Code "TabSegmented", Txt " uses ", Badge "b-btn" "themeButton", Txt ")"]
    , [Badge "b-pnl" "themePanel", Txt " ", Code "styleFg", Txt " (inactive dimmed to alpha 175)"]
    , [Txt "Selected indicator: ", Badge "b-acc" "themeAccent"]
    , [Link "#trap-11" [Txt "Ignores ", Code "buttonStyle", Txt " on 3 of 4 tab styles (#11)"]]
    ]
  , [ [Code "table", Txt ", ", Code "simpleTable"]
    , [Code "NodePanel", Txt " + ", Code "NodeButton", Txt " + ", Code "NodeText"]
    , [Txt "Headers: 55% ", Badge "b-pnl" "themePanel", Txt " + ", Badge "b-btn" "themeButton", Txt "; Rows: striped"]
    , [Txt "Outer: ", Badge "b-pnl" "themePanel", Txt "; Dividers: ", Code "themeSeparator"]
    , [ Txt "Cells: ", Badge "b-pnl" "themePanel", Txt " ", Code "styleFg"
      , Txt "; Sorted hdr: ", Badge "b-btn" "themeButton", Txt " ", Code "styleFg"
      , Txt "; Unsorted: ", Code "themeMuted"
      ]
    , [Txt "Selected row blends 25% toward ", Badge "b-acc" "themeAccent"]
    , [Txt "Unsorted headers ignore ", Code "buttonStyle (foreground ...)"]
    ]
  , [ [Code "tree"]
    , [Code "NodeButton", Txt " (row)"]
    , [Txt "Even: 25% ", Code "themeWindow", Txt "; Odd: 35% ", Badge "b-btn" "themeButton", Txt " over ", Badge "b-pnl" "themePanel"]
    , [Txt "None (forced 0)"]
    , [Badge "b-btn" "themeButton", Txt " ", Code "styleFg", Txt " (label & chevron)"]
    , [Txt "Selected row blends 25% toward ", Badge "b-acc" "themeAccent"]
    , [Txt "Row stripes blend 3 surfaces"]
    ]
  , [ [Code "modal", Txt ", ", Code "popup", Txt ", ", Code "menuItem"]
    , [Code "NodeFloatingPanel", Txt " / ", Code "NodeButton"]
    , [Badge "b-pop" "themePopup", Txt " ", Code "styleBg", Txt " (scrim: ", Code "themeOverlayDim", Txt ")"]
    , [Badge "b-pop" "themePopup", Txt " ", Code "styleBorder"]
    , [Badge "b-pop" "themePopup", Txt " ", Code "styleFg"]
    , [Txt "Menu hover falls back to ", Badge "b-btn" "themeButton", Txt " ", Code "styleHoverBg", Txt " when popup ", Code "hoverBg == bg"]
    , [Txt "Inner ", Code "panel", Txt "s inside a modal still read ", Code "themePanel"]
    ]
  , [ [Code "toggleSwitch", Txt ", ", Code "progressBar", Txt ", ", Code "knob", Txt ", ", Code "sparkline"]
    , [Code "NodeDrawing"]
    , [Txt "Controls: ", Badge "b-btn" "themeButton", Txt " ", Code "styleBg", Brk, Code "sparkline", Txt ": ", Badge "b-pnl" "themePanel", Txt " ", Code "styleBg"]
    , [Badge "b-btn" "themeButton", Txt " ", Code "styleBorder", Txt " (1 to 1.5px fixed)"]
    , [Badge "b-btn" "themeButton", Txt " ", Code "styleFg"]
    , [Txt "Active fill/arc/line: ", Badge "b-acc" "themeAccent", Txt " + ", Code "themeOnAccent"]
    , [Txt "Custom canvas paths ignore ", Code "cornerRadius", Txt " & ", Code "borderWidth"]
    ]
  ]

-- | Section III master controls: apply one mask to every widget card at once.
masterBar :: Html ()
masterBar = do
  div_ [class_ "master-bar"] $ do
    div_ [class_ "master-bar-top"] $ do
      span_ [class_ "master-bar-title"] "Apply a style to every widget:"
      div_ [class_ "wb-head-actions"] $ do
        miniBtn [onclick_ "setAllWidgetsMask(30)"] "All surfaces"
        miniBtn [onclick_ "setAllWidgetsMask(6)"] "Button and input"
        miniBtn [onclick_ "setAllWidgetsMask(32)"] "Rounded 999"
        miniBtn [id_ "global-cmp-btn", onclick_ "toggleGlobalCompare()"] "⇄ Compare all"
        miniBtn [onclick_ "setAllWidgetsMask(0)"] "Reset all"
    div_ [class_ "toggle-chip-grid", id_ "master-toggles"] $ mapM_ globalChip globalChips
  where
    globalChips =
      [ (0 :: Int, "light", "defaultLightTheme")
      , (1, "btn", "buttonStyle")
      , (2, "inp", "inputStyle")
      , (3, "pnl", "panelStyle")
      , (4, "acc", "accentColor")
      , (5, "rnd", "rounded 999")
      , (6, "dis", "disabledWhen True")
      ]
    globalChip :: (Int, Text, Text) -> Html ()
    globalChip (bit, probe, name) =
      styleChip
        [ data_ "gbit" (showT bit)
        , data_ "probe" probe
        , onclick_ ("toggleGlobalBit(" <> showT bit <> ")")
        ]
        name
        Nothing

-- | The whole page. Lucid escapes text and attributes; the stylesheet, script,
-- JSON payloads, and rendered SVG documents go in raw because they are not
-- prose to escape.
glossaryPage ::
  Assets ->
  (Text, Value) ->
  (Text, Value) ->
  [ProbeRow] ->
  TypoSectionData ->
  [InteractionCard] ->
  Html ()
glossaryPage assets (heroBaseSvg, heroBankVal) (stateBaseSvg, stateBankVal) probeRows typoData interactions = do
  doctype_
  html_ [lang_ "en"] $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
      title_ "nano-ui Styling & Surface Glossary"
      style_ (assetsFontCss assets <> "\n" <> assetsCss assets)
    body_ $ do
      div_ [class_ "layout-shell"] $ do
        aside_ [class_ "sidebar"] sidebarNav
        main_ [class_ "main-content"] $ do
          heroSection heroBaseSvg
          filterBar
          section_ [id_ "sec-states"] $ do
            sectionHead [Txt "I. Interaction States"] [Txt "Prerendered"]
            stateStripSection stateBaseSvg
          section_ [id_ "sec-routing"] $ do
            sectionHead [Txt "II. Surface Routing"] [Txt "Which theme slot paints what"]
            routingTable
          section_ [id_ "sec-matrix"] $ do
            sectionHead [Txt "III. Widget Styles"] [Txt "Prerendered"]
            masterBar
            mapM_ probeRowCard probeRows
          section_ [id_ "sec-typo"] $ do
            sectionHead [Txt "IV. Typography"] [Txt "Modifiers and specimens"]
            div_ [class_ "typo-grid"] (typoSection typoData)
          section_ [id_ "sec-traps"] $ do
            sectionHead [Txt "V. Unexpected Interactions"] [Txt "Click to compare"]
            interactionsSection interactions
      script_ banksJs
      script_ (assetsJs assets)
  where
    banksJs =
      "const SVG_BANKS = "
        <> encodeJsonText (buildSvgBanksValue heroBankVal stateBankVal probeRows typoData)
        <> ";\nconst WIDGET_META = "
        <> encodeJsonText (buildWidgetMetaValue probeRows)
        <> ";"

buildHtmlDocument ::
  Assets ->
  (Text, Value) ->
  (Text, Value) ->
  [ProbeRow] ->
  TypoSectionData ->
  [InteractionCard] ->
  Text
buildHtmlDocument assets hero state rows typo interactions =
  TL.toStrict (renderText (glossaryPage assets hero state rows typo interactions))

main :: IO ()
main = do
  args <- getArgs
  let outPath = case args of
        (p : _) -> p
        [] -> "docs/styling-glossary.html"
      sdlOpts =
        defaultSdlOptions
          { sdlWindowSettings =
              defaultWindowSettings
                { wsTitle = "nano-ui-svg-glossary"
                , wsMode = Hidden
                , wsSize = Size 800 600
                , wsResizable = False
                }
          , sdlAppVsync = False
          , sdlAppFont = DefaultFont
          , sdlAppMonoFont = DefaultFont
          , sdlAppFontSize = 14
          , sdlAppUiScale = 1
          }
  assets <- loadAssets
  ctxSeed <- newPixelContext
  withSdl sdlOpts ctxSeed $ \sdlCtx _env -> do
    writeIORef sdlFontCtxRef (Just sdlCtx)
    banner <- generateRainbowBanner
    stateStrip <- generateStateStrip
    probeRows <- generateProbeMatrix
    typoDemos <- generateTypoDemos
    interactions <- generateInteractions
    let html = buildHtmlDocument assets banner stateStrip probeRows typoDemos interactions
    withFile outPath WriteMode $ \h -> do
      hSetEncoding h utf8
      TIO.hPutStr h html
    putStrLn ("wrote " <> outPath <> " (" <> show (T.length html) <> " chars)")
    createDirectoryIfMissing True "book/theme"
    let banksJs =
          "window.NANO_UI_BANKS = "
            <> encodeJsonText (buildSvgBanksValue (snd banner) (snd stateStrip) probeRows typoDemos)
            <> ";\nwindow.NANO_UI_WIDGET_META = "
            <> encodeJsonText (buildWidgetMetaValue probeRows)
            <> ";\nwindow.NANO_UI_TRAPS = "
            <> encodeJsonText (buildTrapsValue interactions)
            <> ";\n"
    withFile "book/theme/nano-ui-banks.js" WriteMode $ \h -> do
      hSetEncoding h utf8
      TIO.hPutStr h banksJs
    putStrLn "wrote book/theme/nano-ui-banks.js"
