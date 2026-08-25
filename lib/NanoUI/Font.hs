module NanoUI.Font
  ( GlyphQuad (..)
  , FontMetrics (..)
  , monospaceMetrics
  , measureText
  , measureTextWrapped
  , labelContentInset
  , widgetContentInset
  , widgetPadding
  , isTerminalFont
  ) where


import Data.Text (Text)
import qualified Data.Text as T

data GlyphQuad = GlyphQuad
  { gqX :: Float
  , gqY :: Float
  , gqW :: Float
  , gqH :: Float
  , gqU0 :: Float
  , gqV0 :: Float
  , gqU1 :: Float
  , gqV1 :: Float
  }
  deriving (Eq, Show)

data FontMetrics = FontMetrics
  { fmLineHeight :: Float
  , fmAscent :: Float
  , fmAdvance :: Char -> Float
  , fmGlyph :: Char -> Maybe GlyphQuad
  }

{-# INLINE monospaceMetrics #-}
monospaceMetrics :: Float -> FontMetrics
monospaceMetrics cell =
  FontMetrics
    { fmLineHeight = cell
    , fmAscent = cell * 0.8
    , fmAdvance = \_ -> cell
    , fmGlyph = \_ -> Nothing
    }

-- Cell-grid metrics get whole-cell insets; fractional padding would place rows
-- on half cells, which a terminal cannot represent.
{-# INLINE labelContentInset #-}
labelContentInset :: FontMetrics -> (Float, Float)
labelContentInset fm
  | isTerminalFont fm = (0, 0)
  | otherwise = (0.5 * fmAdvance fm ' ', 0.25 * fmLineHeight fm)

{-# INLINE widgetContentInset #-}
widgetContentInset :: FontMetrics -> (Float, Float)
widgetContentInset fm
  | isTerminalFont fm = (fmAdvance fm ' ', 0)
  | otherwise = (fmAdvance fm ' ', 0.25 * fmLineHeight fm)

{-# INLINE widgetPadding #-}
widgetPadding :: FontMetrics -> (Float, Float)
widgetPadding fm =
  let (cx, cy) = widgetContentInset fm
   in (2 * cx, 2 * cy)

{-# INLINE isTerminalFont #-}
isTerminalFont :: FontMetrics -> Bool
isTerminalFont fm = fmLineHeight fm == 1 && fmAdvance fm ' ' == 1

measureText :: FontMetrics -> Text -> (Float, Float)
measureText fm txt =
  let adv = fmAdvance fm ' '
      len = fromIntegral (T.length txt)
      w = len * adv
      h = fmLineHeight fm
   in (w, h)

measureTextWrapped :: FontMetrics -> Text -> Float -> (Float, Float)
measureTextWrapped fm txt maxW =
  let (w, h) = measureText fm txt
   in (min w maxW, h)
