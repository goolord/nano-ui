{-# LANGUAGE OverloadedStrings #-}

module Cases.RichText
  ( runRichTextWrapTest
  , runRichTextLinkTest
  ) where

import Control.Monad (void)
import Data.IORef (IORef)
import Data.Text qualified as T
import NanoUI
import NanoUI.Context (Context (..))
import NanoUI.Testing (UiCursorKind (..), cursorKindIs, runFrame)
import NanoUI.Testing.Assert (assert, assertEq, withInput)
import NanoUI.Testing.Harness (clickPair, warmup2)

-- | A paragraph wraps at its column's width, taking a line's height per line,
-- and mixed pieces share a line.
runRichTextWrapTest :: Context -> IORef Int -> IO ()
runRichTextWrapTest ctx failed = do
  let inp = withInput 400 400
      paragraph = [inlineText (T.replicate 12 "word "), strong "bold", " end"]
      ui = columnWith (fixedW 200) $ do
        one <- fst <$> richText' ["word"]
        wrapped <- fst <$> richText' paragraph
        pure (one, wrapped)
  (one, wrapped) <- warmup2 ctx inp ui
  let Rect _ _ _ lineH = respRect one
      Rect _ _ w h = respRect wrapped
  assert failed (lineH > 0)
  assert failed (w <= 200)
  -- Twelve words and two more pieces cannot fit on one 200px line.
  assert failed (h >= 2 * lineH)
  assertEq failed 0 (round h `mod` round lineH :: Int)

-- | A link reports its target when clicked and shows the pointer cursor;
-- text beside it reports nothing.
runRichTextLinkTest :: Context -> IORef Int -> IO ()
runRichTextLinkTest ctx failed = do
  let inp0 = withInput 400 400
      ui = column (richText' ["Go to ", hyperlink "docs-target" "docs", " now"])
      fm = ctxFontMetrics ctx
  (resp, _) <- warmup2 ctx inp0 ui
  prefixW <- sum <$> mapM (lineWidthIO fm) ["Go", " ", "to", " "]
  linkW <- lineWidthIO fm "docs"
  let Rect rx ry _ rh = respRect resp
      onLink = V2 (rx + prefixW + linkW / 2) (ry + rh / 2)
      onText = V2 (rx + 2) (ry + rh / 2)
      clickAt pos = do
        let (press, release) = clickPair inp0 pos
        void (runFrame ctx inp0 {inputMousePos = pos} ui)
        void (runFrame ctx press ui)
        ((_, clicked), _, _, _) <- runFrame ctx release ui
        pure clicked
  linkClick <- clickAt onLink
  assertEq failed (Just "docs-target") linkClick
  pointer <- cursorKindIs ctx inp0 {inputMousePos = onLink} UiCursorPointer
  assert failed pointer
  textClick <- clickAt onText
  assertEq failed Nothing textClick
  plainCursor <- cursorKindIs ctx inp0 {inputMousePos = onText} UiCursorPointer
  assert failed (not plainCursor)
