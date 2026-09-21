-- | Headless UI test for the log viewer: drives it on a hidden window through
-- pinning while entries arrive, scrolling back, Jump to Bottom, filters,
-- horizontal scroll, Select All, copying, and clearing the selection.
module LogsSelftest
  ( selftest
  ) where

import Control.Monad (unless, void)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text qualified as T
import NanoUI
import NanoUI.Backend (emptyInput)
import NanoUI.Backend.Sdl
import NanoUI.Internal.Context (Context (..), getPrevRect)
import NanoUI.Testing (collectTextSpans, newPixelContext)
import NanoUI.Testing.Harness (clickPos, expectText, findExact, hasText, keyInp, requireSpan)
import SdlLogs (AppState (..), appendEntries, logsApp, newAppState)

selftest :: IO ()
selftest = do
  ctx0 <- newPixelContext
  withSdl
    defaultSdlOptions
      { sdlWindowHidden = True
      , sdlWindowSize = Size 1000 700
      , sdlWindowResizable = False
      }
    ctx0
    $ \ctx env -> do
      appStateRef <- newIORef =<< newAppState 60 False
      let baseInput = emptyInput {inputWindowSize = Size 1000 700, inputMousePos = V2 500 350}
          drawFrame inp = void (sdlDrawFrame ctx (logsApp appStateRef) env inp False)

      drawFrame baseInput
      drawFrame baseInput

      spans0 <- collectTextSpans ctx
      expectText "selftest: title 'Log Viewer' not found in spans" "Log Viewer" spans0
      expectText "selftest: initial state should be PINNED to bottom" "PINNED" spans0

      -- A row is one selectable span: id, timestamp, level, service and message.
      let hasFullLogSpan = any (\(_, txt, _, _, _) -> "#00" `T.isPrefixOf` txt && " [" `T.isInfixOf` txt) spans0
      unless hasFullLogSpan $
        fail "selftest: full log line (sequence ID, timestamp, level, service, message) not found in selectable spans"

      readIORef appStateRef >>= (`appendEntries` 40) >>= writeIORef appStateRef
      drawFrame baseInput
      drawFrame baseInput

      spans1 <- collectTextSpans ctx
      expectText "selftest: sticky scroll failed to stay PINNED after appending logs" "PINNED" spans1

      let wheelUpInput = baseInput {inputScroll = V2 0 (-15.0)}
      drawFrame wheelUpInput
      drawFrame baseInput

      spans2 <- collectTextSpans ctx
      expectText "selftest: scrolling up into history did not transition to UNPINNED" "UNPINNED" spans2

      -- Entries arriving while scrolled back leave the view where it is.
      readIORef appStateRef >>= (`appendEntries` 50) >>= writeIORef appStateRef
      drawFrame baseInput

      spans3 <- collectTextSpans ctx
      expectText "selftest: appending logs while reading history should keep state UNPINNED" "UNPINNED" spans3

      jumpPos <- requireSpan "selftest: Jump to Bottom button" (findExact "Jump to Bottom" spans3)
      clickPos drawFrame baseInput jumpPos
      drawFrame baseInput

      spans4 <- collectTextSpans ctx
      expectText "selftest: Jump to Bottom button failed to restore PINNED status" "PINNED" spans4

      -- A filter applies in the frame that clicks it, and the view clamps so
      -- the fewer rows stay visible.
      warnPos <- requireSpan "selftest: WARN filter button" (findExact "WARN" spans4)
      clickPos drawFrame baseInput warnPos
      spansWarn <- collectTextSpans ctx
      unless (hasText "filtered" spansWarn || hasText "Filtered:" spansWarn) $
        fail "selftest: filter button did not update view immediately"
      let hasWarnLogs = any (\(_, txt, _, _, _) -> "WARN" `T.isInfixOf` txt && "#" `T.isInfixOf` txt) spansWarn
      unless hasWarnLogs $
        fail "selftest: filtered log lines were blank after filtering from bottom of list"

      allPos <- requireSpan "selftest: ALL filter button" (findExact "ALL" spansWarn)
      clickPos drawFrame baseInput allPos

      stScroller <- readIORef appStateRef
      case asScrollerWid stScroller of
        Nothing -> fail "selftest: scroller WidgetId not found"
        Just scrollWid -> do
          let wheelHInput = baseInput {inputScroll = V2 20.0 0}
          drawFrame wheelHInput
          drawFrame baseInput
          off2d <- getScrollOffset2D ctx scrollWid
          unless (v2X off2d > 0) $
            fail "selftest: horizontal scroll did not update horizontal scroll offset (2D scroll failed)"

      spansCur <- collectTextSpans ctx
      selAllPos <- requireSpan "selftest: Select All button" (findExact "Select All" spansCur)
      clickPos drawFrame baseInput selAllPos
      spansSelected <- collectTextSpans ctx
      unless (hasText "ALL LOGS SELECTED" spansSelected || hasText "Deselect All" spansSelected) $
        fail "selftest: Select All failed to select all logs"

      let ctrlCInput = baseInput {inputChars = "\ETX", inputModifiers = Modifiers False True False}
      drawFrame ctrlCInput
      mClip <- ctxClipboardGet ctx
      case mClip of
        Nothing -> fail "selftest: Ctrl+C after Select All failed to copy to clipboard"
        Just clipText -> do
          unless ("#00" `T.isPrefixOf` clipText && "\n" `T.isInfixOf` clipText) $
            fail "selftest: clipboard does not contain all selected log lines"

      drawFrame (keyInp KeyEscape baseInput)
      drawFrame baseInput
      spansCleared <- collectTextSpans ctx
      expectText "selftest: ESC failed to clear Select All" "Select All" spansCleared

      -- A click on a row clears Select All.
      selAllPos2 <- requireSpan "selftest: Select All button" (findExact "Select All" spansCleared)
      clickPos drawFrame baseInput selAllPos2
      spansSel2 <- collectTextSpans ctx
      stSel <- readIORef appStateRef
      case asScrollerWid stSel of
        Nothing -> fail "selftest: scroller WidgetId not found for deselect test"
        Just scrollWid -> do
          mScrollerRect <- getPrevRect ctx scrollWid
          let rowClicks =
                [ V2 (rectX vis + 4) (rectY vis + rectH vis / 2)
                | (r, txt, _, _, _) <- spansSel2
                , "#00" `T.isPrefixOf` txt && " [" `T.isInfixOf` txt
                , Just vis <- [mScrollerRect >>= (`rectIntersect` r)]
                , rectW vis > 20
                ]
          case rowClicks of
            (pos : _) -> do
              clickPos drawFrame baseInput pos
              spansDeselected <- collectTextSpans ctx
              expectText "selftest: left click on a log row did not clear Select All" "Select All" spansDeselected
            [] -> fail "selftest: no visible selectable log row found for deselect test"

      putStrLn "logs selftest: ok"
