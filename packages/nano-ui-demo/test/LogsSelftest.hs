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
import NanoUI.Backend.Sdl
import NanoUI.Internal.Context (Context (..), getPrevRect)
import NanoUI.Testing (collectTextSpans)
import NanoUI.Testing.Harness (clickPos, expectText, findExact, hasText, keyInp, requireSpan)
import DemoApp (withHiddenWindow)
import SdlLogs (AppState (..), appendEntries, logsApp, newAppState)

selftest :: IO ()
selftest = do
  withHiddenWindow 1000 700 (V2 500 350) id $ \ctx env baseInput -> do
      appStateRef <- newIORef =<< newAppState 60 False
      let drawFrame inp = void (sdlDrawFrame ctx (logsApp appStateRef) env inp False)
          click = clickPos drawFrame baseInput
          append n = readIORef appStateRef >>= (`appendEntries` n) >>= writeIORef appStateRef
          -- The frame's text spans, failing with @msg@ unless one holds @needle@.
          spansWith msg needle = do
            spans <- collectTextSpans ctx
            spans <$ expectText msg needle spans
          expect msg needle = void (spansWith msg needle)
          scrollerId msg = maybe (fail msg) pure . asScrollerWid =<< readIORef appStateRef

      drawFrame baseInput
      drawFrame baseInput

      spans0 <- spansWith "selftest: title 'Log Viewer' not found in spans" "Log Viewer"
      expectText "selftest: initial state should be PINNED to bottom" "PINNED" spans0

      -- A row is one selectable span: id, timestamp, level, service and message.
      let hasFullLogSpan = any (\(_, txt, _, _, _) -> "#00" `T.isPrefixOf` txt && " [" `T.isInfixOf` txt) spans0
      unless hasFullLogSpan $
        fail "selftest: full log line (sequence ID, timestamp, level, service, message) not found in selectable spans"

      append 40
      drawFrame baseInput
      drawFrame baseInput
      expect "selftest: sticky scroll failed to stay PINNED after appending logs" "PINNED"

      drawFrame baseInput {inputScroll = V2 0 (-15.0)}
      drawFrame baseInput
      expect "selftest: scrolling up into history did not transition to UNPINNED" "UNPINNED"

      -- Entries arriving while scrolled back leave the view where it is.
      append 50
      drawFrame baseInput
      spans3 <- spansWith "selftest: appending logs while reading history should keep state UNPINNED" "UNPINNED"

      click =<< requireSpan "selftest: Jump to Bottom button" (findExact "Jump to Bottom" spans3)
      drawFrame baseInput
      spans4 <- spansWith "selftest: Jump to Bottom button failed to restore PINNED status" "PINNED"

      -- A filter applies in the frame that clicks it, and the view clamps so
      -- the fewer rows stay visible.
      click =<< requireSpan "selftest: WARN filter button" (findExact "WARN" spans4)
      spansWarn <- collectTextSpans ctx
      unless (hasText "filtered" spansWarn || hasText "Filtered:" spansWarn) $
        fail "selftest: filter button did not update view immediately"
      let hasWarnLogs = any (\(_, txt, _, _, _) -> "WARN" `T.isInfixOf` txt && "#" `T.isInfixOf` txt) spansWarn
      unless hasWarnLogs $
        fail "selftest: filtered log lines were blank after filtering from bottom of list"

      click =<< requireSpan "selftest: ALL filter button" (findExact "ALL" spansWarn)

      scrollWid <- scrollerId "selftest: scroller WidgetId not found"
      drawFrame baseInput {inputScroll = V2 20.0 0}
      drawFrame baseInput
      off2d <- getScrollOffset2D ctx scrollWid
      unless (v2X off2d > 0) $
        fail "selftest: horizontal scroll did not update horizontal scroll offset (2D scroll failed)"

      click =<< requireSpan "selftest: Select All button" . findExact "Select All" =<< collectTextSpans ctx
      spansSelected <- collectTextSpans ctx
      unless (hasText "ALL LOGS SELECTED" spansSelected || hasText "Deselect All" spansSelected) $
        fail "selftest: Select All failed to select all logs"

      drawFrame baseInput {inputChars = "\ETX", inputModifiers = Modifiers False True False}
      mClip <- ctxClipboardGet ctx
      case mClip of
        Nothing -> fail "selftest: Ctrl+C after Select All failed to copy to clipboard"
        Just clipText ->
          unless ("#00" `T.isPrefixOf` clipText && "\n" `T.isInfixOf` clipText) $
            fail "selftest: clipboard does not contain all selected log lines"

      drawFrame (keyInp KeyEscape baseInput)
      drawFrame baseInput
      spansCleared <- spansWith "selftest: ESC failed to clear Select All" "Select All"

      -- A click on a row clears Select All.
      click =<< requireSpan "selftest: Select All button" (findExact "Select All" spansCleared)
      spansSel2 <- collectTextSpans ctx
      mScrollerRect <- getPrevRect ctx =<< scrollerId "selftest: scroller WidgetId not found for deselect test"
      let rowClicks =
            [ V2 (rectX vis + 4) (rectY vis + rectH vis / 2)
            | (r, txt, _, _, _) <- spansSel2
            , "#00" `T.isPrefixOf` txt && " [" `T.isInfixOf` txt
            , Just vis <- [mScrollerRect >>= (`rectIntersect` r)]
            , rectW vis > 20
            ]
      case rowClicks of
        (pos : _) -> do
          click pos
          expect "selftest: left click on a log row did not clear Select All" "Select All"
        [] -> fail "selftest: no visible selectable log row found for deselect test"

      putStrLn "logs selftest: ok"
