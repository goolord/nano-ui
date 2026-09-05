module Cases.Font
  ( runFontSizeTest
  , runFontColorTest
  , runFontWeightStyleDecoTest
  , runFontCompositionTest
  ) where

import Control.Monad (void)
import Data.IORef (IORef)
import qualified Data.Text as T
import NanoUI
import NanoUI.Testing
import NanoUI.Testing.Assert (assert, assertEq, withInput)
import NanoUI.Layout.Arena
  ( getNodeFontSize
  , getNodeFontColor
  , getStyleIdx
  )
import NanoUI.Context (Context (..))

runFontSizeTest :: Context -> IORef Int -> IO ()
runFontSizeTest ctx failed = do
  let inp = withInput 800 600
      ui = column $ do
        void $ label "Standard Text"
        void $ labelWith (fontSize 32.0) "Scaled Text"
  _ <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  let textSpans = filter (\(_, txt, _, _, _) -> not (T.null txt)) spans
  assertEq failed (length textSpans) 2
  case textSpans of
    [(r1, _, _, _, _), (r2, _, _, _, _)] -> do
      -- The 32px font should have a span height of 32.0, whereas standard is 16.0
      assertEq failed (rectH r1) 16.0
      assertEq failed (rectH r2) 32.0
      -- And its width should be scaled proportionally
      assert failed (rectW r2 > 0)
    _ -> assert failed False

  -- Check node arena storage
  let na = ctxNodeArena ctx
  s1 <- getNodeFontSize na 1
  s2 <- getNodeFontSize na 2
  assertEq failed s1 0.0
  assertEq failed s2 32.0

runFontColorTest :: Context -> IORef Int -> IO ()
runFontColorTest ctx failed = do
  let inp = withInput 800 600
      red = colorRGBA 255 0 0 255
      blue = colorRGBA 0 0 255 255
      ui = column $ do
        void $ labelWith (fontColor red) "Red Label"
        void $ labelWith (textColor blue) "Blue Label"
  _ <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  let textSpans = filter (\(_, txt, _, _, _) -> not (T.null txt)) spans
  assertEq failed (length textSpans) 2
  case textSpans of
    [(_, _, fg1, _, _), (_, _, fg2, _, _)] -> do
      assertEq failed fg1 red
      assertEq failed fg2 blue
    _ -> assert failed False

  let na = ctxNodeArena ctx
  c1 <- getNodeFontColor na 1
  c2 <- getNodeFontColor na 2
  assertEq failed c1 (Just red)
  assertEq failed c2 (Just blue)

runFontWeightStyleDecoTest :: Context -> IORef Int -> IO ()
runFontWeightStyleDecoTest ctx failed = do
  let inp = withInput 800 600
      ui = column $ do
        void $ labelWith fontBold "Bold Text"
        void $ labelWith fontItalic "Italic Text"
        void $ labelWith fontUnderline "Underline Text"
        void $ labelWith fontStrike "Strike Text"
        bold "Bold Helper"
        italic "Italic Helper"
        underline "Underline Helper"
  _ <- runFrame ctx inp ui
  let na = ctxNodeArena ctx
  -- Nodes 1..7
  si1 <- getStyleIdx na 1
  si2 <- getStyleIdx na 2
  si3 <- getStyleIdx na 3
  si4 <- getStyleIdx na 4
  si5 <- getStyleIdx na 5
  si6 <- getStyleIdx na 6
  si7 <- getStyleIdx na 7

  assertEq failed (textNodeFontWeight si1) WeightBold
  assertEq failed (textNodeFontStyle si2) FontStyleItalic
  assertEq failed (textNodeTextDecoration si3) DecorationUnderline
  assertEq failed (textNodeTextDecoration si4) DecorationStrikethrough

  -- High-level helpers
  assertEq failed (textNodeFontWeight si5) WeightBold
  assertEq failed (textNodeFontStyle si6) FontStyleItalic
  assertEq failed (textNodeTextDecoration si7) DecorationUnderline

runFontCompositionTest :: Context -> IORef Int -> IO ()
runFontCompositionTest ctx failed = do
  let inp = withInput 800 600
      customCol = colorRGBA 12 34 56 255
      ui = column $ do
        void $ labelWith (fontSize 24.0 . fontBold . fontItalic . fontUnderline . fontColor customCol) "Composed"
  _ <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  let textSpans = filter (\(_, txt, _, _, _) -> not (T.null txt)) spans
  assertEq failed (length textSpans) 1
  case textSpans of
    [(r, _, fg, _, _)] -> do
      assertEq failed (rectH r) 24.0
      assertEq failed fg customCol
    _ -> assert failed False

  let na = ctxNodeArena ctx
  sz <- getNodeFontSize na 1
  col <- getNodeFontColor na 1
  si <- getStyleIdx na 1
  assertEq failed sz 24.0
  assertEq failed col (Just customCol)
  assertEq failed (textNodeFontWeight si) WeightBold
  assertEq failed (textNodeFontStyle si) FontStyleItalic
  assertEq failed (textNodeTextDecoration si) DecorationUnderline
