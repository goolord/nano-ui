module NanoUI.Font
  ( GlyphQuad (..)
  , FontMetrics (..)
  , monospaceMetrics
  , measureText
  , measureTextWrapped
  ) where

import Data.Char (isPrint)
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
    , fmGlyph =
        \c ->
          if isPrint c
            then
              Just
                GlyphQuad
                  { gqX = 0
                  , gqY = 0
                  , gqW = cell
                  , gqH = cell
                  , gqU0 = 0
                  , gqV0 = 0
                  , gqU1 = 1
                  , gqV1 = 1
                  }
            else Nothing
    }

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
