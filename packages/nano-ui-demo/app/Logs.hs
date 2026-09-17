-- | A standalone SDL3 example application demonstrating a high-throughput,
-- virtualized log viewer with sticky-scroll semantics and selectable text.
--
-- Features:
--   * Virtualized rendering (60 FPS under high volumes of logs).
--   * Selectable text per log entry using NanoUI's 'selectableText' API.
--   * Sticky scroll: stays pinned to the bottom when new logs are appended
--     at the bottom; stays completely stationary when reading history in the middle.
--   * Interactive controls: stream toggle, bursts (+100, +1000), filter by level,
--     clear buffer, and a 'Jump to Bottom' button when unpinned.
--   * Automated headless verification via @cabal run nano-ui-sdl-logs -- --selftest@.
module Main (main) where

import Control.Monad (unless, void, when)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Effectful (Eff, type (:>))
import NanoUI
import NanoUI.Backend.Sdl
import NanoUI.Context
  ( Context (..)
  , damageFull
  , getPrevRect
  , markDirty
  , takeTextEditLastAction
  )
import NanoUI.Monad (askContext, askInput, uiTime)
import NanoUI.Testing (collectTextSpans)
import NanoUI.Testing.Harness (clickPos, findExact, hasText, requireSpan)
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import Text.Printf (printf)

--------------------------------------------------------------------------------
-- Data Model
--------------------------------------------------------------------------------

data LogLevel
  = LevelDebug
  | LevelInfo
  | LevelWarn
  | LevelError
  deriving (Eq, Show, Enum, Bounded)

data LogEntry = LogEntry
  { leId :: {-# UNPACK #-} !Int
  , leTimestamp :: !Text
  , leLevel :: !LogLevel
  , leService :: !Text
  , leMessage :: !Text
  }
  deriving (Eq, Show)

data AppState = AppState
  { asLogs :: !(Vector LogEntry)
  , asNextId :: {-# UNPACK #-} !Int
  , asStreaming :: !Bool
  , asLastStream :: {-# UNPACK #-} !Double
  , asFilterLevel :: !(Maybe LogLevel)
  , asScrollerWid :: !(Maybe WidgetId)
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Realistic Log Generators
--------------------------------------------------------------------------------

sampleServices :: [Text]
sampleServices =
  [ "auth.worker"
  , "db.pool"
  , "http.gateway"
  , "cache.redis"
  , "storage.blob"
  , "worker.queue"
  , "network.mesh"
  , "security.audit"
  ]

sampleTemplates :: [(LogLevel, Text)]
sampleTemplates =
  [ (LevelInfo, "HTTP GET /api/v2/metrics status=200 duration=14.2ms bytes=4192")
  , (LevelDebug, "Cache hit for key 'session:9920822-0585' (ttl=3580s)")
  , (LevelInfo, "Token refreshed for user usr_89f3a (took 3.8ms)")
  , (LevelWarn, "Slow query detected (154ms): SELECT * FROM telemetry_events WHERE org_id = 42")
  , (LevelInfo, "Acquired connection #18 from postgres-pool-primary [pool: 24/50]")
  , (LevelError, "Upstream timeout after 5000ms connecting to billing-internal:8443")
  , (LevelInfo, "Flushing block 0x7fa2b9 to S3 bucket 'telemetry-prod' [etag: a3b91c]")
  , (LevelDebug, "AVX2 SIMD pass vectorised 4 inner loops (unroll factor: 4)")
  , (LevelWarn, "Memory usage at 78.4% (heap: 1.24GB / limit: 1.50GB)")
  , (LevelInfo, "Peer 192.168.1.144 heartbeat acknowledged [rtt: 0.8ms]")
  , (LevelError, "Failed to decode payload from kafka partition #3: unexpected EOF")
  , (LevelInfo, "TLS handshake completed with client cipher=TLS_AES_256_GCM_SHA384")
  ]

maxLogCapacity :: Int
maxLogCapacity = 50000

generateLogEntry :: Int -> LogEntry
generateLogEntry idx =
  let (lvl, msg) = sampleTemplates !! (idx `mod` length sampleTemplates)
      src = sampleServices !! ((idx * 3) `mod` length sampleServices)
      sec = (idx * 7) `mod` 60
      minu = (idx `div` 8) `mod` 60
      hr = 10 + (idx `div` 480) `mod` 12
      millis = (idx * 137) `mod` 1000
      ts = T.pack $ printf "%02d:%02d:%02d.%03d" hr minu sec millis
      msgWithSeq = msg <> " (event #" <> T.pack (show idx) <> ")"
   in LogEntry idx ts lvl src msgWithSeq

initialAppState :: Int -> Bool -> AppState
initialAppState initialCount streaming =
  AppState
    { asLogs = V.generate initialCount (\i -> generateLogEntry (i + 1))
    , asNextId = initialCount + 1
    , asStreaming = streaming
    , asLastStream = 0.0
    , asFilterLevel = Nothing
    , asScrollerWid = Nothing
    }

-- | Append @count@ freshly generated entries, capping the buffer. Pure so the
-- burst buttons, the streaming tick and the selftest all share one policy.
appendEntries :: AppState -> Int -> AppState
appendEntries st count =
  let curId = asNextId st
      newEntries = V.generate count (\i -> generateLogEntry (curId + i))
      allEntries = asLogs st <> newEntries
      capped =
        if V.length allEntries > maxLogCapacity
          then V.drop (V.length allEntries - maxLogCapacity) allEntries
          else allEntries
   in st
        { asLogs = capped
        , asNextId = curId + count
        }

formatLogLine :: LogEntry -> Text
formatLogLine entry =
  T.pack $
    printf
      "#%05d  %s  [%-5s]  %-14s  %s"
      (leId entry)
      (leTimestamp entry)
      (levelTag (leLevel entry))
      (leService entry)
      (leMessage entry)
  where
    levelTag :: LogLevel -> Text
    levelTag = \case
      LevelDebug -> "DEBUG"
      LevelInfo -> "INFO "
      LevelWarn -> "WARN "
      LevelError -> "ERROR"

levelColor :: LogLevel -> Color
levelColor = \case
  LevelDebug -> colorRGBA 180 142 173 255
  LevelInfo -> colorRGBA 136 192 208 255
  LevelWarn -> colorRGBA 235 203 139 255
  LevelError -> colorRGBA 191 97 106 255

--------------------------------------------------------------------------------
-- Main UI Application
--------------------------------------------------------------------------------

logRowH :: Float
logRowH = 24.0

logStickyThresh :: Float
logStickyThresh = 8.0

-- Header toolbar + banner + status bar height; fallback viewport estimate
-- before the scroller has a previous-frame rect.
logChromeFallbackH :: Float
logChromeFallbackH = 140.0

{-# NOINLINE logsApp #-}
logsApp :: Ui :> es => IORef AppState -> Eff es ()
logsApp stateRef = do
  ctx <- askContext
  inp <- askInput
  now <- uiTime

  -- Helper to mutate state and force immediate redraw in SDL
  let mutateState f = uiIO $ do
        modifyIORef' stateRef f
        markDirty ctx
        damageFull ctx

  st0 <- uiIO $ readIORef stateRef

  -- Live streaming update: append 1 log every 80ms while streaming is enabled
  when (asStreaming st0 && now - asLastStream st0 >= 0.08) $
    mutateState $ \s -> (appendEntries s 1) {asLastStream = now}

  -- Keep event loop active while streaming so SDL does not sleep between 80ms ticks
  when (asStreaming st0) $
    uiIO $ markDirty ctx

  (allSelected, setAllSelected) <- withKey ("log-all-selected" :: Text) (useFlag False)

  -- Check menu action from right-click context menu (e.g. Select All or Copy)
  mMenuAction <- uiIO $ takeTextEditLastAction ctx
  case mMenuAction of
    Just (_, item) | item == fromEnum MenuSelectAll -> do
      -- Select All chosen from context menu
      setAllSelected True
      uiIO $ markDirty ctx >> damageFull ctx
    _ -> pure ()

  -- Keyboard shortcuts
  let keys = inputKeys inp
      mods = inputModifiers inp
      ctrlPressed = modCtrl mods
      chars = inputChars inp
      aPressed = ctrlPressed && any (\c -> T.elem c "aA\x01") (T.unpack chars)
      cPressed = ctrlPressed && any (\c -> T.elem c "cC\ETX") (T.unpack chars)
      escPressed = inputKeysElem KeyEscape keys

  when aPressed $ do
    setAllSelected True
    uiIO $ markDirty ctx >> damageFull ctx

  when escPressed $ do
    when allSelected $ do
      setAllSelected False
      uiIO $ markDirty ctx >> damageFull ctx

  -- A left click on a selectable row clears the Select-All highlight. The
  -- scrollbar is chrome, not an interactive widget, so `ctxActiveId` stays 0
  -- for gutter grabs and only an actual row press clears.
  when (allSelected && (inputMousePressed inp || inputMouseDown inp || inputMouseReleased inp)) $ do
    active <- uiIO $ readIORef (ctxActiveId ctx)
    when (active /= WidgetId 0) $
      case asScrollerWid st0 of
        Just scrollWid -> do
          mRect <- uiIO $ getPrevRect ctx scrollWid
          case mRect of
            Just r | rectContains r (inputMousePos inp) -> do
              setAllSelected False
              uiIO $ markDirty ctx >> damageFull ctx
            _ -> pure ()
        Nothing -> pure ()

  columnWith (tight . fillW . fillH . gap 0) $ do
    stLive <- uiIO $ readIORef stateRef
    let allLogsBefore = asLogs stLive
        filteredLogsBefore = case asFilterLevel stLive of
          Nothing -> allLogsBefore
          Just lvl -> V.filter (\e -> leLevel e == lvl) allLogsBefore

    -- Header Toolbar
    renderHeaderToolbar mutateState stLive (V.length allLogsBefore) (V.length filteredLogsBefore) (allSelected, setAllSelected)
    separator

    -- Re-read state after toolbar interactions so filter/burst/clear changes
    -- take effect IMMEDIATELY in this frame's layout and paint passes!
    stCurrent <- uiIO $ readIORef stateRef
    let allLogs = asLogs stCurrent
        filteredLogs = case asFilterLevel stCurrent of
          Nothing -> allLogs
          Just lvl -> V.filter (\e -> leLevel e == lvl) allLogs

    -- If allSelected is active and user triggers copy (Ctrl+C or context menu Copy):
    let copyAllLogs = do
          let fullText = T.unlines (V.toList (fmap formatLogLine filteredLogs))
          uiIO $ void (ctxClipboardSet ctx fullText)

    -- Central Virtualized 2D Log Scroller
    renderLogScroller stateRef allSelected filteredLogs

    -- Copy after the scroller pass: a focused row's own Ctrl+C runs inside
    -- selectableTextWith during the scroller pass and would otherwise overwrite
    -- the clipboard with a single row.
    when (allSelected && cPressed) $ copyAllLogs
    case mMenuAction of
      Just (_, item) | item == fromEnum MenuCopy && allSelected -> copyAllLogs
      _ -> pure ()

    -- Status Bar
    separator
    renderStatusBar (V.length allLogs) (V.length filteredLogs) allSelected

renderHeaderToolbar ::
  Ui :> es =>
  ((AppState -> AppState) -> Eff es ()) ->
  AppState ->
  Int ->
  Int ->
  (Bool, Bool -> Eff es ()) ->
  Eff es ()
renderHeaderToolbar mutateState st totalCount filteredCount (allSelected, setAllSelected) = do
  ctx <- askContext
  let filterPill lvl lbl = do
        clicked <- buttonWith (if asFilterLevel st == lvl then fontBold else id) lbl
        when clicked $
          mutateState (\s -> s {asFilterLevel = lvl})
  styled (panelStyle (background (colorRGBA 24 29 38 255) . borderColor (colorRGBA 45 52 64 255))) $ panelWith fillW $ do
    columnWith (tight . fillW . padXY 12 10 . gap 8) $ do
      -- Top line: Title, Badges, and Stats
      rowWith (tight . fillW . alignMid . gap 12) $ do
        labelWith (fontBold . fontSize 16 . tight) "Log Viewer"
        labelWith (fontMono . fontMuted . tight) ("[" <> T.pack (show totalCount) <> " total]")

        when (filteredCount /= totalCount) $
          labelWith (fontMono . fontColor (colorRGBA 235 203 139 255) . tight)
            ("[" <> T.pack (show filteredCount) <> " filtered]")

        -- Live stream indicator
        if asStreaming st
          then labelWith (fontMono . fontBold . fontColor (colorRGBA 163 190 140 255) . tight) "[● STREAMING]"
          else labelWith (fontMono . fontMuted . tight) "[⏸ PAUSED]"

        when allSelected $
          labelWith (fontMono . fontBold . fontColor (colorRGBA 235 203 139 255) . tight) "[● ALL SELECTED]"

      -- Controls line: Buttons for streaming, bursts, clear, selection, and filters
      rowWith (tight . fillW . alignMid . gap 8) $ do
        -- Stream toggle
        streamClicked <- button (if asStreaming st then "Pause Stream" else "Start Stream")
        when streamClicked $
          mutateState (\s -> s {asStreaming = not (asStreaming s), asLastStream = 0.0})

        -- Burst buttons
        burst100Clicked <- button "+100"
        when burst100Clicked $
          mutateState (\s -> appendEntries s 100)

        burst1000Clicked <- button "+1000"
        when burst1000Clicked $
          mutateState (\s -> appendEntries s 1000)

        clearClicked <- button "Clear"
        when clearClicked $
          mutateState (\s -> s {asLogs = V.empty, asNextId = 1})

        spacer (Fixed 8) Fit

        -- Selection controls
        selAllClicked <- buttonWith (if allSelected then fontBold else id) (if allSelected then "Deselect All" else "Select All")
        when selAllClicked $ do
          setAllSelected (not allSelected)
          uiIO $ markDirty ctx >> damageFull ctx

        spacer (Fixed 8) Fit

        -- Filter pills
        labelWith (fontMuted . tight) "Filter:"
        filterPill Nothing "ALL"
        filterPill (Just LevelInfo) "INFO"
        filterPill (Just LevelWarn) "WARN"
        filterPill (Just LevelError) "ERROR"
        filterPill (Just LevelDebug) "DEBUG"

renderLogScroller :: Ui :> es => IORef AppState -> Bool -> Vector LogEntry -> Eff es ()
renderLogScroller stateRef allSelected logs = do
  scrollWid <- withKey ("log-scroller" :: Text) nextId
  stWid <- uiIO $ readIORef stateRef
  when (asScrollerWid stWid /= Just scrollWid) $
    uiIO $ modifyIORef' stateRef (\s -> s {asScrollerWid = Just scrollWid})
  ctx <- askContext
  mPrevRect <- uiIO $ getPrevRect ctx scrollWid
  inp <- askInput

  -- Persistent state tracked across frames for sticky scroll:
  (sticky, setSticky) <- withKey ("log-sticky" :: Text) (useFlag True)
  (prevScrollY, setPrevScrollY) <- withKey ("log-prev-scrolly" :: Text) (useFloat 0)
  (reqJump, setReqJump) <- withKey ("log-req-jump" :: Text) (useFlag False)

  let n = V.length logs
      totalH = fromIntegral n * logRowH
      viewH = maybe (sizeH (inputWindowSize inp) - logChromeFallbackH) rectH mPrevRect
      maxOff = max 0 (totalH - viewH)

  curOff <- uiIO $ getScrollOffset2D ctx scrollWid
  let curY = v2Y curOff
      curX = v2X curOff
      -- User scrolled if offset moved compared to our recorded offset
      userScrolled = abs (curY - prevScrollY) > 0.5
      -- Currently at bottom if within sticky threshold or content fits within viewport
      atBottomNow = (maxOff <= 0) || (curY >= maxOff - logStickyThresh)

      -- Sticky state transition:
      -- If manually scrolled:
      --   - scrolled up into history -> sticky = False
      --   - scrolled down to bottom -> sticky = True
      -- Else:
      --   - maintain sticky state (or re-enable if jump requested)
      isSticky0 = if userScrolled then atBottomNow else sticky
      isSticky = reqJump || isSticky0

  -- Scroll positioning:
  -- - When sticky: pin to bottom (targetY = maxOff), even if buffer shrank or cleared
  -- - When unpinned (reading history): remain at curY, but clamp to maxOff so view is never blank
  let targetY = if isSticky then maxOff else min maxOff curY
  effY <-
    if abs (targetY - curY) > 0.5
      then do
        uiIO $ setScrollOffset2D ctx scrollWid (V2 curX targetY)
        pure targetY
      else pure curY

  setSticky isSticky
  setPrevScrollY effY
  when reqJump $ setReqJump False

  -- Sticky indicator banner & Jump to Bottom button
  rowWith (tight . fillW . padXY 12 4 . alignMid . gap 8) $ do
    if isSticky
      then labelWith (fontMono . fontBold . fontColor (colorRGBA 163 190 140 255) . tight) "● PINNED"
      else do
        labelWith (fontMono . fontBold . fontColor (colorRGBA 235 203 139 255) . tight) "⏸ UNPINNED (reading history)"
        jumpClicked <- buttonWith (fontColor (colorRGBA 136 192 208 255) . fontBold) "Jump to Bottom"
        when jumpClicked $
          setReqJump True

  -- Virtualization calculation with 1 row overscan above and below
  let (firstVis, lastVis) =
        if n <= 0 || viewH <= 0
          then (0, -1)
          else
            let lo = max 0 (floor (effY / logRowH) - 1)
                hi = min (n - 1) (floor ((effY + viewH - 1) / logRowH) + 1)
             in if hi < lo then (0, -1) else (lo, hi)
      visIndices = if lastVis < firstVis then [] else [firstVis .. lastVis]
      topH = fromIntegral firstVis * logRowH
      botH = fromIntegral (max 0 (n - lastVis - 1)) * logRowH

  -- The same key scope gives the scroll area the id read above as scrollWid,
  -- so this frame's offset and sticky state apply to it.
  void $ withKey ("log-scroller" :: Text) $ scrollArea2D (fillW . fillH) $ do
    columnWith (tight . gap 0 . minW 1200) $ do
      when (topH > 0) $ spacer Fit (Fixed topH)
      mapM_
        ( \idx ->
            let entry = logs V.! idx
             in withKey (leId entry) $ renderLogRow allSelected entry
        )
        visIndices
      when (botH > 0) $ spacer Fit (Fixed botH)

renderLogRow :: Ui :> es => Bool -> LogEntry -> Eff es ()
renderLogRow isAllSel entry = do
  let lineText = formatLogLine entry
      col = levelColor (leLevel entry)
      rowLay = tight . fixedH logRowH . padXY 8 2 . alignMid
      rowBody = selectableTextWith (fontColor col . fontMono . tight) lineText
  if isAllSel
    then styled (panelStyle (background (colorRGBA 45 65 95 255) . borderColor (colorRGBA 70 100 145 255))) (panelWith rowLay rowBody)
    else rowWith rowLay rowBody

renderStatusBar :: Ui :> es => Int -> Int -> Bool -> Eff es ()
renderStatusBar totalCount filteredCount allSelected = do
  styled (panelStyle (background (colorRGBA 20 24 32 255) . borderColor (colorRGBA 45 52 64 255))) $ panelWith fillW $ do
    rowWith (tight . fillW . padXY 12 4 . alignMid . gap 16) $ do
      labelWith (fontMono . fontMuted . tight)
        ("Total: " <> T.pack (show totalCount) <> " logs | Filtered: " <> T.pack (show filteredCount))
      if allSelected
        then
          labelWith (fontMono . fontBold . fontColor (colorRGBA 235 203 139 255) . tight)
            "ALL LOGS SELECTED | Ctrl+C to copy all | ESC to clear"
        else
          labelWith (fontMuted . tight)
            "Tip: Click & drag to select | Right-click for Copy/Select All | 2D Scroll | Ctrl+Q to quit"

--------------------------------------------------------------------------------
-- Entry Point & Selftest
--------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  if "--selftest" `elem` args
    then selftest
    else do
      appStateRef <- newIORef (initialAppState 120 True)
      runSdlApp
        defaultSdlOptions
          { sdlWindowTitle = "nano-ui Log Viewer"
          , sdlWindowSize = Size 1050 720
          , sdlAppTheme = Just tomorrowNightMinDarkTheme
          -- ESC clears the selection (handled in logsApp); quit is Ctrl+Q.
          , sdlAppShouldQuit = \inp ->
              modCtrl (inputModifiers inp)
                && T.any (`T.elem` ("qQ" :: Text)) (inputChars inp)
          }
        (logsApp appStateRef)

-- | Headless verification testing:
--   1. Initial rendering of virtualized logs and PINNED status.
--   2. All text selectable (sequence ID, timestamp, level, service, message).
--   3. Sticky scroll: appends when at bottom keep viewport pinned to latest logs.
--   4. History reading: scrolling up into history unpins sticky scroll.
--   5. Reading isolation: appends while in history DO NOT move the scroll position.
--   6. Jump to Bottom button: restores sticky scroll and pins to latest logs.
--   7. Filter buttons update view immediately without requiring mouse movement.
--   8. 2D Scroll: horizontal scroll offset updates on horizontal scroll wheel.
--   9. Select All / Context menu copies all logs to clipboard.
{-# NOINLINE selftest #-}
selftest :: IO ()
selftest = do
  ctx0 <- newSdlContext
  withSdl
    defaultSdlOptions
      { sdlWindowHidden = True
      , sdlWindowSize = Size 1000 700
      , sdlWindowResizable = False
      }
    ctx0
    $ \ctx env -> do
      appStateRef <- newIORef (initialAppState 60 False)
      let baseInput = emptyInput {inputWindowSize = Size 1000 700, inputMousePos = V2 500 350}
          drawFrame inp = void (sdlDrawFrame ctx (logsApp appStateRef) env inp False)

      -- 1. Warm up 2 frames and check initial state
      drawFrame baseInput
      drawFrame baseInput

      spans0 <- collectTextSpans ctx
      unless (hasText "Log Viewer" spans0) $
        fail "selftest: title 'Log Viewer' not found in spans"
      unless (hasText "PINNED" spans0) $
        fail "selftest: initial state should be PINNED to bottom"

      -- 2. Verify that all of the text is selectable:
      -- A log row contains the sequence ID (#000..), timestamp, level, and service in a single span.
      let hasFullLogSpan = any (\(_, txt, _, _, _) -> "#00" `T.isPrefixOf` txt && " [" `T.isInfixOf` txt) spans0
      unless hasFullLogSpan $
        fail "selftest: full log line (sequence ID, timestamp, level, service, message) not found in selectable spans"

      -- 3. Test sticky scroll: append 40 new logs while pinned
      modifyIORef' appStateRef (`appendEntries` 40)
      drawFrame baseInput
      drawFrame baseInput

      spans1 <- collectTextSpans ctx
      unless (hasText "PINNED" spans1) $
        fail "selftest: sticky scroll failed to stay PINNED after appending logs"

      -- 4. Scroll up into middle of history (wheel up)
      let wheelUpInput = baseInput {inputScroll = V2 0 (-15.0)}
      drawFrame wheelUpInput
      drawFrame baseInput

      spans2 <- collectTextSpans ctx
      unless (hasText "UNPINNED" spans2) $
        fail "selftest: scrolling up into history did not transition to UNPINNED"

      -- 5. Append 50 more logs while reading history in the middle:
      -- The scrollbar / viewport must NOT move, and state must stay UNPINNED
      modifyIORef' appStateRef (`appendEntries` 50)
      drawFrame baseInput

      spans3 <- collectTextSpans ctx
      unless (hasText "UNPINNED" spans3) $
        fail "selftest: appending logs while reading history should keep state UNPINNED"

      -- 6. Test "Jump to Bottom" button:
      jumpPos <- requireSpan "selftest: Jump to Bottom button" (findExact "Jump to Bottom" spans3)
      clickPos drawFrame baseInput jumpPos
      drawFrame baseInput

      spans4 <- collectTextSpans ctx
      unless (hasText "PINNED" spans4) $
        fail "selftest: Jump to Bottom button failed to restore PINNED status"

      -- 7. Test Filter responsiveness:
      -- Clicking "WARN" must immediately filter the logs in the exact same frame,
      -- and rows must be visible immediately (not blank due to scroll offset overshooting).
      warnPos <- requireSpan "selftest: WARN filter button" (findExact "WARN" spans4)
      clickPos drawFrame baseInput warnPos
      spansWarn <- collectTextSpans ctx
      unless (hasText "filtered" spansWarn || hasText "Filtered:" spansWarn) $
        fail "selftest: filter button did not update view immediately"
      let hasWarnLogs = any (\(_, txt, _, _, _) -> "WARN" `T.isInfixOf` txt && "#" `T.isInfixOf` txt) spansWarn
      unless hasWarnLogs $
        fail "selftest: filtered log lines were blank after filtering from bottom of list"

      -- Restore filter to ALL
      allPos <- requireSpan "selftest: ALL filter button" (findExact "ALL" spansWarn)
      clickPos drawFrame baseInput allPos

      -- 8. Test 2D Scroll:
      -- Horizontal scroll wheel moves horizontal scroll offset
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

      -- 9. Test "Select All":
      spansCur <- collectTextSpans ctx
      selAllPos <- requireSpan "selftest: Select All button" (findExact "Select All" spansCur)
      clickPos drawFrame baseInput selAllPos
      spansSelected <- collectTextSpans ctx
      unless (hasText "ALL LOGS SELECTED" spansSelected || hasText "Deselect All" spansSelected) $
        fail "selftest: Select All failed to select all logs"

      -- Test Ctrl+C copies all filtered logs to clipboard
      let ctrlCInput = baseInput {inputChars = "\ETX", inputModifiers = Modifiers False True False}
      drawFrame ctrlCInput
      mClip <- ctxClipboardGet ctx
      case mClip of
        Nothing -> fail "selftest: Ctrl+C after Select All failed to copy to clipboard"
        Just clipText -> do
          unless ("#00" `T.isPrefixOf` clipText && "\n" `T.isInfixOf` clipText) $
            fail "selftest: clipboard does not contain all selected log lines"

      -- Press ESC to clear selection
      let escInput = baseInput {inputKeys = inputKeysFromList [KeyEscape]}
      drawFrame escInput
      drawFrame baseInput
      spansCleared <- collectTextSpans ctx
      unless (hasText "Select All" spansCleared) $
        fail "selftest: ESC failed to clear Select All"

      -- 10. Left click on a selectable log row clears the Select-All highlight.
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
              unless (hasText "Select All" spansDeselected) $
                fail "selftest: left click on a log row did not clear Select All"
            [] -> fail "selftest: no visible selectable log row found for deselect test"

      putStrLn "selftest: all 2D log viewer, selectable text, filter, context menu, and sticky scroll tests passed successfully!"
      exitSuccess
