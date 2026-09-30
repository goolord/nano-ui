-- | The Markdown widget: a document drawn as nano-ui rich text and layout.
module NanoUI.Markdown.Widget
  ( MarkdownConfig (..)
  , defaultMarkdownConfig
  , MarkdownCache
  , newMarkdownCache
  , markdown
  , markdownConfigured
  ) where

import Control.Applicative ((<|>))
import Control.Monad (unless, void, when, zipWithM)
import Data.Char (isSpace)
import Data.Foldable (asum)
import Data.Hashable (Hashable, hash)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.IntMap.Strict qualified as IM
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import NanoUI
  ( NanoUI
  , Color
  , FontMetrics (fmLineHeight)
  , ImageId
  , Inline
  , Layout (..)
  , Rect (..)
  , Size (..)
  , Style (..)
  , Theme (..)
  , V2 (..)
  , alignCenter
  , alignEnd
  , alignMid
  , alignStart
  , askDefaultLayout
  , background
  , borderColor
  , borderWidth
  , box
  , buttonWith
  , checkboxBoxSize
  , colorToWord32
  , columnWith
  , CanvasConfig (..)
  , canvasConfigured
  , cornerRadius
  , defaultCanvasConfig
  , defaultLayout
  , drawCheckbox
  , drawCircle
  , drawRect
  , drawStrokeCircle
  , fillH
  , fillW
  , fixedW
  , fixedWH
  , flex
  , fontBold
  , fontColor
  , fontItalic
  , fontMono
  , fontMuted
  , fontSemiBold
  , fontSize
  , fontSizeScale
  , fontStrike
  , gap
  , gridWith
  , hyperlink
  , image'
  , inlineBackground
  , inlineText
  , inlineWith
  , labelWith
  , lerpColor
  , lineWidthUi
  , padAll
  , padXY
  , panelStyle
  , panelWith
  , resolveFontUi
  , respClicked
  , restyle
  , selectableRichTextWith
  , rowWith
  , scope
  , separator
  , setClipboard
  , styled
  , subtle
  , tight
  , tooltip
  , uiFontSize
  , uiTheme
  , whenM
  , withKey
  )
import NanoUI.Internal.Context (intKey)
import NanoUI.Internal.Monad (nextId, liftIO)
import NanoUI.Internal.Store (eqByPtr, ptrEq)
import NanoUI.Markdown.Document (MarkdownDoc, markdownBlocks)
import NanoUI.Markdown.Syntax

-- | How a document is drawn. Colours left 'Nothing' come from the theme.
-- Each style modifier is applied on top of the default look it names, so
-- @mdCodeBlock = background c@ changes a code block's background and keeps
-- its border.
data MarkdownConfig = MarkdownConfig
  { mdLayout :: !(Layout -> Layout)
  -- ^ The document's column.
  , mdText :: !(Layout -> Layout)
  -- ^ Body text font, e.g. @fontSize 15 . fontColor c@.
  , mdHeading :: !(Int -> Layout -> Layout)
  -- ^ Heading font by level (1 to 6), applied over the body text font.
  , mdLinkColor :: !(Maybe Color)
  -- ^ Link colour (default: the theme's).
  , mdInlineCode :: !(Layout -> Layout)
  -- ^ Inline code font, over monospace in the theme's orange (default: 'id').
  , mdInlineCodeBackground :: !(Maybe Color)
  -- ^ Background behind inline code (default: none).
  , mdCodeBlock :: !(Style -> Style)
  -- ^ Code block panel, over the theme's input background with a
  -- separator-coloured border and rounded corners (default: 'id').
  , mdQuote :: !(Layout -> Layout)
  -- ^ Quoted text, over the surrounding text in the theme's muted colour
  -- (default: 'id').
  , mdTableCell :: !(Bool -> Style -> Style)
  -- ^ Table cell panel, given whether it is a header cell, over the flat
  -- panel colour, tinted for headers (default: @const id@).
  , mdCopyCode :: !Bool
  -- ^ Show a copy button on code blocks.
  , mdImage :: !(Text -> Maybe (ImageId, Size))
  -- ^ The registered image for a source, and its size in logical pixels.
  -- Only an image alone in its paragraph (or alone in a link there) is
  -- drawn. Clicking it returns the link, or else the source; its title, or
  -- else the link's, is the tooltip. Other images show their alt text as a
  -- link.
  , mdBlock :: !((Block -> NanoUI (Maybe Text)) -> Block -> NanoUI (Maybe Text))
  -- ^ Custom block drawing, given the widget's own drawing to fall back to
  -- or wrap (default: 'id'). Called for every block at every depth,
  -- including inside quotes and list items, so it can highlight code, load
  -- images lazily, or add chrome. The built-in drawing calls it again for
  -- nested blocks. Return the clicked link, as 'markdown' does. Each block
  -- is keyed by position and kind, so appending to a block keeps its ids.
  --
  -- > mdBlock = \own -> \case
  -- >   CodeBlock "haskell" code -> Nothing <$ highlighted code
  -- >   b -> own b
  }

-- | A full-width column, body text in the theme's font, headings from 1.6x
-- down to 0.9x its size, theme colours, copy buttons on code blocks, and no
-- images.
defaultMarkdownConfig :: MarkdownConfig
defaultMarkdownConfig =
  MarkdownConfig
    { mdLayout = tight . fillW . gap 16
    , mdText = id
    , mdHeading = \level ->
        let scale = case level of
              1 -> 1.6
              2 -> 1.35
              3 -> 1.2
              4 -> 1.05
              5 -> 1
              _ -> 0.9
         in fontSizeScale scale . (if level <= 4 then fontBold else fontSemiBold)
    , mdLinkColor = Nothing
    , mdInlineCode = id
    , mdInlineCodeBackground = Nothing
    , mdCodeBlock = id
    , mdQuote = id
    , mdTableCell = const id
    , mdCopyCode = True
    , mdImage = const Nothing
    , mdBlock = id
    }

-- | Draw a document. Returns the destination of a link clicked this frame.
--
-- Each top-level block is keyed by its position and kind, so appending to a
-- document keeps the ids, and the cached text layout, of earlier blocks.
-- Text is not selectable; code blocks have a copy button.
markdown :: MarkdownCache -> MarkdownDoc -> NanoUI (Maybe Text)
markdown cache = markdownConfigured cache defaultMarkdownConfig

-- | 'markdown' with a configuration.
markdownConfigured :: MarkdownCache -> MarkdownConfig -> MarkdownDoc -> NanoUI (Maybe Text)
markdownConfigured cache cfg doc = do
  theme <- uiTheme
  size <- uiFontSize
  -- Pin the body size (the backend default if unset) so headings and small
  -- text scale it rather than 16.
  let text = mdText cfg . \l -> if layoutFontSize l > 0 then l else fontSize size l
  base <- text <$> askDefaultLayout
  fm <- resolveFontUi (layoutFontSize base) (layoutFontWeight base) (layoutFontStyle base) (layoutFontVariant base)
  let env =
        Env
          { envCfg = cfg
          , envCache = cache
          , envTheme = theme
          , envText = text
          , envSize = layoutFontSize base
          , envDepth = 0
          , envMetrics = fm
          }
  columnWith (mdLayout cfg) (blocks env (markdownBlocks doc))

-- Per-block drawing context. envText is the current text modifier (muted
-- inside quotes); envMetrics is the body font, which sizes list markers.
data Env = Env
  { envCfg :: !(MarkdownConfig)
  , envCache :: !MarkdownCache
  , envTheme :: !Theme
  , envText :: !(Layout -> Layout)
  , envSize :: !Float
  , envDepth :: !Int
  , envMetrics :: !FontMetrics
  }

-- | Blocks in sequence, each drawn by 'mdBlock' under a key of its position
-- and kind. A block that changes kind as text streams in (a paragraph
-- becoming a heading, table or image) gets fresh ids.
blocks :: Env -> [Block] -> NanoUI (Maybe Text)
blocks env bs = asum <$> zipWithM draw [0 :: Int ..] bs
  where
    own = block env
    draw i b = withKey (i, blockKind (envCfg env) b) (mdBlock (envCfg env) own b)

-- | A block's kind, for its key. A paragraph drawn as an image is its own
-- kind.
blockKind :: MarkdownConfig -> Block -> Int
blockKind cfg = \case
  Paragraph xs
    | isJust (soleImage cfg xs) -> 13
    | otherwise -> 0
  Heading level _ -> level
  ThematicBreak -> 7
  CodeBlock {} -> 8
  BlockQuote _ -> 9
  List (Bullet _) _ _ -> 10
  List (Ordered _ _) _ _ -> 11
  Table {} -> 12

-- | The widget's own block drawing. NOINLINE: inlined into 'blocks', its
-- closures over 'Env' would be allocated on every call.
{-# NOINLINE block #-}
block :: Env -> Block -> NanoUI (Maybe Text)
block env = \case
  Paragraph xs
    | Just (target, title, iid, Size w h) <- soleImage (envCfg env) xs -> do
        resp <- image' (fixedWH w h) iid
        unless (T.null title) (tooltip resp title)
        pure (if respClicked resp then Just target else Nothing)
    | otherwise -> selectableRichTextWith (tight . fillW . envText env) =<< keptInlines env xs
  Heading level xs ->
    let title = selectableRichTextWith (tight . fillW . mdHeading (envCfg env) level . envText env) =<< keptInlines env xs
     in if level <= 2 then columnWith (tight . fillW . gap 6) (title <* separator) else title
  ThematicBreak -> Nothing <$ separator
  CodeBlock info code -> Nothing <$ codeBlock env info code
  BlockQuote bs ->
    rowWith (tight . fillW . gap 10) $ do
      box (fixedW 3 . fillH) (themeSeparator (envTheme env))
      columnWith (tight . fillW . gap 16) $
        blocks env {envText = mdQuote (envCfg env) . fontColor (themeMuted (envTheme env)) . envText env} bs
  List ty isTight items -> listBlock env ty isTight items
  Table aligns header rows -> tableBlock env aligns header rows

-- | The image to draw for a paragraph holding only an image (or only a
-- linked image) that 'mdImage' knows: click target, title (else the link's),
-- image and size.
soleImage :: MarkdownConfig -> [Span] -> Maybe (Text, Text, ImageId, Size)
soleImage cfg = \case
  [Image src title _] -> drawn src src title
  [Link url linkTitle [Image src title _]] -> drawn url src (if T.null title then linkTitle else title)
  _ -> Nothing
  where
    drawn target src title = (\(iid, size) -> (target, title, iid, size)) <$> mdImage cfg src

-- | 'inlines' for spans drawn every frame, the same pieces as last frame's
-- while the spans are the same list (as a closed block's are) and the theme
-- and inline styles are unchanged. Rich text then reuses the paragraph's
-- layout without hashing its text again, so a long document costs little a
-- frame while a reply streams into its last block.
keptInlines :: Env -> [Span] -> NanoUI [Inline]
keptInlines env xs = do
  wid <- nextId
  liftIO $ do
    let MarkdownCache ref = envCache env
    KeptGens bound cur old <- readIORef ref
    let k = intKey wid
        found = maybe (IM.lookup k old) Just (IM.lookup k cur)
    case found of
      Just e@(KeptEntry ys theme link codeBg code pieces)
        | ptrEq xs ys
        , eqByPtr (envTheme env) theme
        , mdLinkColor cfg == link
        , mdInlineCodeBackground cfg == codeBg
        , ptrEq (mdInlineCode cfg) code -> do
            -- Found only in the older generation: carry it into this one.
            unless (IM.member k cur) (writeIORef ref (keep bound cur old k e))
            pure pieces
      _ -> do
        let pieces = inlines env xs
            e = KeptEntry xs (envTheme env) (mdLinkColor cfg) (mdInlineCodeBackground cfg) (mdInlineCode cfg) pieces
        pieces <$ writeIORef ref (keep bound cur old k e)
  where
    cfg = envCfg env
    -- Past the bound, the newer generation becomes the older one and the
    -- bound grows to twice what it held, so what a frame draws stays while
    -- blocks the view stopped drawing drop out.
    keep bound cur old k e
      | IM.size cur < bound = KeptGens bound (IM.insert k e cur) old
      | otherwise = KeptGens (max keptBound (2 * IM.size cur)) (IM.singleton k e) cur

-- | Pieces 'keptInlines' made, by widget key, in two generations.
newtype MarkdownCache = MarkdownCache (IORef KeptGens)

-- | Allocate once during component setup. The cache retains only typed inline
-- data; paragraph layout and interaction state remain in the UI context.
newMarkdownCache :: IO MarkdownCache
newMarkdownCache = MarkdownCache <$> newIORef (KeptGens keptBound IM.empty IM.empty)

data KeptGens = KeptGens !Int !(IM.IntMap KeptEntry) !(IM.IntMap KeptEntry)

-- | Spans, what styled them, and their pieces.
data KeptEntry = KeptEntry [Span] !Theme !(Maybe Color) !(Maybe Color) (Layout -> Layout) [Inline]

-- | Entries a generation holds before the older one is dropped.
keptBound :: Int
keptBound = 1024

-- | Spans as rich-text pieces. Every piece inside a link targets its
-- destination; an image's alt text targets its enclosing link, else its
-- source.
inlines :: Env -> [Span] -> [Inline]
inlines env = concatMap (go id Nothing)
  where
    theme = envTheme env
    cfg = envCfg env
    linkColor' = fromMaybe (themeLink theme) (mdLinkColor cfg)
    codeBackground = maybe id inlineBackground (mdInlineCodeBackground cfg)
    go style target = \case
      Str t -> [piece style target t]
      SoftBreak -> [piece style target " "]
      HardBreak -> [piece style target "\n"]
      Emph xs -> concatMap (go (fontItalic . style) target) xs
      Strong xs -> concatMap (go (fontBold . style) target) xs
      Strike xs -> concatMap (go (fontStrike . style) target) xs
      Code t ->
        -- Code in a link keeps the link's colour.
        let ink = if isJust target then id else fontColor (themeOrange theme)
         in [codeBackground (piece (mdInlineCode cfg . fontMono . ink . style) target t)]
      Link url _ xs -> concatMap (go (fontColor linkColor' . style) (target <|> Just url)) xs
      Image src _ alt ->
        let txt = if null alt then "image" else spansText alt
         in [piece (fontItalic . fontColor linkColor' . style) (target <|> Just src) txt]
    piece style target t = case target of
      Nothing -> inlineWith style t
      Just url -> restyle style (hyperlink url t)

-- | Monospace code on a panel, under a row with its language and a copy
-- button. Long lines wrap, keeping indentation and inner spaces. A
-- horizontal scroller would steal the wheel from the page and its bar would
-- cover the last line.
codeBlock :: Env -> Text -> Text -> NanoUI ()
codeBlock env info code = do
  let theme = envTheme env
      cfg = envCfg env
      language = T.takeWhile (not . isSpace) info
  styled (panelStyle (mdCodeBlock cfg . background (styleBg (themeInput theme)) . borderColor (themeSeparator theme) . cornerRadius 6)) $
    panelWith (padXY 10 8 . gap 4 . fillW) $ do
      scope $ when (mdCopyCode cfg || not (T.null language)) $
        rowWith (tight . fillW . alignMid) $ do
          labelWith (tight . fontMuted . fontSizeScale 0.8 . envText env) language
          flex
          when (mdCopyCode cfg) $
            whenM (styled subtle (buttonWith (padXY 6 1 . fontSize (0.8 * envSize env)) "Copy")) $
              void (setClipboard code)
      void $
        selectableRichTextWith
          (tight . fillW . fontMono . fontColor (styleFg (themeInput theme)) . envText env)
          [inlineText code]

-- | A list: each item's marker beside its blocks; tight lists have less
-- spacing. Markers take the surrounding text colour. Bullets are a disc,
-- then a ring one level deeper, then a square. Task items use a nano-ui
-- checkbox, recoloured to the text colour when that differs from the theme.
listBlock :: Env -> ListType -> Bool -> [ListItem] -> NanoUI (Maybe Text)
listBlock env ty isTight items = do
  let fm = envMetrics env
      theme = envTheme env
      lineH = fmLineHeight fm
      side = checkboxBoxSize fm
      spacing = if isTight then 6 else 16
      -- Each item's number, built once for both measuring and drawing.
      numbers = case ty of
        Ordered start delim -> [T.pack (show n) <> T.singleton delim | n <- [start ..]]
        Bullet _ -> repeat ""
  -- Measure every number: in a proportional font an earlier one can be
  -- widest ("10." vs "11.").
  numberWs <- case ty of
    Ordered _ _ -> mapM (lineWidthUi fm) (take (length items) numbers)
    Bullet _ -> pure []
  let markerW = maximum (lineH : [side | any (isJust . itemTask) items] ++ numberWs)
      ink = fromMaybe (styleFg (themePanel theme)) (layoutFontColor (envText env defaultLayout))
      marker v h shape = void (canvasConfigured defaultCanvasConfig {canvasLayout = fixedWH markerW h defaultLayout, canvasContent = v} (\(Rect x y _ _) -> shape x y))
      bullet = marker (version 1 (envDepth env, colorToWord32 ink)) lineH $ \x y ->
        let c@(V2 cx cy) = V2 (x + markerW / 2) (y + lineH / 2)
            r = max 2 (lineH * 0.15)
         in case envDepth env of
              0 -> drawCircle c r ink
              1 -> drawStrokeCircle c (r - 0.6) 1.2 ink
              _ -> drawRect (Rect (cx - r) (cy - r) (2 * r) (2 * r)) ink
      boxTheme
        | ink == styleFg (themePanel theme) = theme
        | otherwise = theme {themeAccent = ink, themeButton = (themeButton theme) {styleBorder = ink}}
      boxColors = map colorToWord32 [themeAccent boxTheme, themeOnAccent boxTheme, styleBg (themeInput boxTheme), styleBorder (themeButton boxTheme)]
      boxH = max lineH side
      checkBox done = marker (version 2 (done, boxColors)) boxH $ \x y ->
        drawCheckbox boxTheme (Rect (x + (markerW - side) / 2) (y + (boxH - side) / 2) side side) done
  columnWith (tight . fillW . gap spacing) $
    asum <$> zipWithM
      ( \(i, num) (ListItem task bs) -> withKey i $
          rowWith (tight . fillW . gap 6) $ do
            case (task, ty) of
              (Just done, _) -> checkBox done
              (Nothing, Ordered _ _) -> labelWith (tight . fixedW markerW . alignEnd . envText env) num
              (Nothing, Bullet _) -> bullet
            columnWith (tight . fillW . gap spacing) $
              blocks env {envDepth = envDepth env + 1} bs
      )
      (zip [0 :: Int ..] numbers)
      items

-- | A list marker's canvas content key: a hash of what the drawing depends on
-- besides size, with the shape tag (1 bullet, 2 checkbox) in the low bits so
-- an item becoming a task item redraws. Never 0, which a canvas reads as no
-- key.
version :: Hashable a => Int -> a -> Int
version tag deps = hash deps * 4 + tag

-- | A table as a grid of equal columns: bold header cells on a tinted row,
-- one-pixel rules between cells, and each cell aligned per its column.
tableBlock :: Env -> [CellAlign] -> [[Span]] -> [[[Span]]] -> NanoUI (Maybe Text)
tableBlock env aligns header rows = do
  let theme = envTheme env
      rule = themeSeparator theme
      bodyBg = styleBg (themePanel theme)
      headBg = lerpColor bodyBg rule 0.35
      flat c = background c . borderWidth 0 . cornerRadius 0
      cell isHeader (align, spans) =
        styled (panelStyle (mdTableCell (envCfg env) isHeader . flat (if isHeader then headBg else bodyBg))) $
          panelWith (padXY 8 5 . fillW . fillH) $
            selectableRichTextWith (tight . alignOf align . (if isHeader then fontBold else id) . envText env)
              =<< keptInlines env spans
      alignOf = \case
        CellCenter -> alignCenter
        CellRight -> alignEnd
        _ -> alignStart
  styled (panelStyle (flat rule)) $
    panelWith (padAll 1 . fillW) $
      gridWith (length aligns) (tight . fillW . gap 1) $ do
        heads <- mapM (cell True) (zip aligns header)
        cells <- mapM (mapM (cell False) . zip aligns) rows
        pure (asum heads <|> asum (map asum cells))
