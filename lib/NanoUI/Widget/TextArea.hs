{-# LANGUAGE BangPatterns #-}

module NanoUI.Widget.TextArea
  ( -- * Widget State
    TextAreaState (..)
  , initTextAreaState
  , setTextAreaViewport
    -- * Event Handling
  , KeyInput (..)
  , Modifiers (..)
  , handleTextAreaEvent
    -- * Layout & Rendering Helpers
  , TextAreaLayout (..)
  , VisualLine (..)
  , computeTextAreaLayout
  ) where

import Data.Char (toLower)
import qualified Data.Text as T
import NanoUI.Widget.TextBuffer as TB

data Modifiers = Modifiers
  { modShift :: !Bool
  , modCtrl :: !Bool
  , modAlt :: !Bool
  , modSuper :: !Bool
  }
  deriving (Eq, Show)

data KeyInput
  = KeyChar !Char
  | KeyEnter
  | KeyBackspace
  | KeyDelete
  | KeyLeft
  | KeyRight
  | KeyUp
  | KeyDown
  | KeyHome
  | KeyEnd
  | KeyPageUp
  | KeyPageDown
  deriving (Eq, Show)

data TextAreaState = TextAreaState
  { buffer :: !TextBuffer
  , scrollOffset :: !(Double, Double) -- ^ (scrollX, scrollY) in pixels
  , viewportSize :: !(Double, Double) -- ^ (width, height) in pixels
  , lineHeight :: !Double
  }
  deriving (Show)

initTextAreaState :: T.Text -> TextAreaState
initTextAreaState initial =
  TextAreaState
    { buffer = TB.fromText initial
    , scrollOffset = (0.0, 0.0)
    , viewportSize = (0.0, 0.0)
    , lineHeight = 16.0
    }

-- | Set the visible size and line height used for page keys and caret follow.
setTextAreaViewport :: (Double, Double) -> Double -> TextAreaState -> TextAreaState
setTextAreaViewport vp lh state =
  ensureCaretVisible state {viewportSize = vp, lineHeight = lh}

-- | Dispatches keyboard inputs into pure TextBuffer transformations.
handleTextAreaEvent :: KeyInput -> Modifiers -> TextAreaState -> TextAreaState
handleTextAreaEvent key mods state =
  let buf = buffer state
      n = pageLineCount state
      newBuf = case (key, modCtrl mods, modAlt mods) of
        (KeyChar c, False, False) -> TB.insertChar c buf
        (KeyEnter, False, False) -> TB.breakLine buf
        (KeyBackspace, False, False) -> TB.deletePrevChar buf
        (KeyBackspace, True, _) -> TB.deletePrevWord buf
        (KeyBackspace, _, True) -> TB.deletePrevWord buf
        (KeyDelete, False, False) -> TB.deleteChar buf
        (KeyDelete, True, _) -> TB.deleteNextWord buf
        (KeyLeft, False, False) -> TB.moveLeft buf
        (KeyLeft, True, _) -> TB.moveWordLeft buf
        (KeyRight, False, False) -> TB.moveRight buf
        (KeyRight, True, _) -> TB.moveWordRight buf
        (KeyUp, False, False) -> TB.moveUp buf
        (KeyDown, False, False) -> TB.moveDown buf
        (KeyHome, False, False) -> TB.moveToBOL buf
        (KeyEnd, False, False) -> TB.moveToEOL buf
        (KeyPageUp, False, False) -> applyN n TB.moveUp buf
        (KeyPageDown, False, False) -> applyN n TB.moveDown buf
        (KeyChar c, True, False)
          | toLower c == 'k' -> TB.killToEOL buf
          | toLower c == 'u' -> TB.killToBOL buf
          | toLower c == 'a' -> TB.moveToBOL buf
          | toLower c == 'e' -> TB.moveToEOL buf
          | otherwise -> buf
        _ -> buf
  in ensureCaretVisible state {buffer = newBuf}

pageLineCount :: TextAreaState -> Int
pageLineCount state =
  let h = snd (viewportSize state)
      lh = lineHeight state
  in
    if h <= 0 || lh <= 0
      then 1
      else max 1 (floor (h / lh))

applyN :: Int -> (a -> a) -> a -> a
applyN n f x = foldl' (\acc _ -> f acc) x [1 .. n]

ensureCaretVisible :: TextAreaState -> TextAreaState
ensureCaretVisible state =
  let TB.Cursor r _ = TB.getCursor (buffer state)
      lh = lineHeight state
      vh = snd (viewportSize state)
      (sx, sy) = scrollOffset state
      caretY = fromIntegral r * lh
      caretH = lh
      contentH = fromIntegral (TB.getLineCount (buffer state)) * lh
      maxSy = max 0 (contentH - vh)
      sy'
        | vh <= 0 = 0
        | caretY < sy = caretY
        | caretY + caretH > sy + vh = caretY + caretH - vh
        | otherwise = sy
  in state {scrollOffset = (sx, clampDouble 0 maxSy sy')}

clampDouble :: Double -> Double -> Double -> Double
clampDouble lo hi x = max lo (min hi x)

--------------------------------------------------------------------------------
-- Layout & Render Projection
--------------------------------------------------------------------------------

data VisualLine = VisualLine
  { visualLineIndex :: !Int
  , visualLineText :: !T.Text
  , visualLineY :: !Double
  }
  deriving (Eq, Show)

data TextAreaLayout = TextAreaLayout
  { layoutLines :: ![VisualLine]
  , layoutCaretX :: !Double
  , layoutCaretY :: !Double
  , layoutCaretH :: !Double
  }
  deriving (Eq, Show)

-- | Pure layout calculation given a text measurement callback and line height.
computeTextAreaLayout
  :: (T.Text -> Double) -- ^ Width measurement function (pixels)
  -> Double -- ^ Line height (pixels)
  -> TextAreaState
  -> TextAreaLayout
computeTextAreaLayout measureWidth lineH state =
  let buf = buffer state
      TB.Cursor r c = TB.getCursor buf
      (scrollX, scrollY) = scrollOffset state
      linesList = TB.toLines buf
      indexedLines = zip [0 ..] linesList
      visLines =
        [ VisualLine idx txt (fromIntegral idx * lineH - scrollY)
        | (idx, txt) <- indexedLines
        ]
      currentLineText =
        if r < length linesList
          then linesList !! r
          else ""
      prefixText = T.take c currentLineText
      caretX = measureWidth prefixText - scrollX
      caretY = fromIntegral r * lineH - scrollY
  in TextAreaLayout
    { layoutLines = visLines
    , layoutCaretX = caretX
    , layoutCaretY = caretY
    , layoutCaretH = lineH
    }
