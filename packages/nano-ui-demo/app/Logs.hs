-- | A log viewer on the SDL3 backend: virtualized rows, selectable text, and a
-- view that follows new entries until you scroll up. @--selftest@ runs it
-- headlessly.
module Main (main) where

import Control.Monad (foldM, forM, forM_, unless, void, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector.Mutable qualified as MV
import Data.Vector.Unboxed.Mutable qualified as MU
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
import NanoUI.Testing (collectTextSpans, newPixelContext)
import NanoUI.Testing.Harness (clickPos, expectText, findExact, hasText, keyInp, requireSpan)
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import Text.Printf (printf)

data LogLevel
  = LevelDebug
  | LevelInfo
  | LevelWarn
  | LevelError
  deriving (Eq, Show, Enum, Bounded)

data LogEntry = LogEntry
  { leId :: {-# UNPACK #-} !Int
  , leLevel :: !LogLevel
  , leLine :: !Text
  }

-- | Entries sit in a ring at their id modulo 'maxLogCapacity', so a new entry
-- overwrites the oldest once the ring is full. The shown ring holds, in
-- order, the ids of live entries that pass the level filter.
data AppState = AppState
  { asEntries :: !(MV.IOVector LogEntry)
  , asCount :: {-# UNPACK #-} !Int
  , asNextId :: {-# UNPACK #-} !Int
  , asShown :: !(MU.IOVector Int)
  , asShownStart :: {-# UNPACK #-} !Int
  , asShownCount :: {-# UNPACK #-} !Int
  , asStreaming :: !Bool
  , asLastStream :: {-# UNPACK #-} !Double
  , asFilterLevel :: !(Maybe LogLevel)
  , asScrollerWid :: !(Maybe WidgetId)
  }

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
      tag :: Text
      tag = case lvl of
        LevelDebug -> "DEBUG"
        LevelInfo -> "INFO"
        LevelWarn -> "WARN"
        LevelError -> "ERROR"
   in LogEntry idx lvl $
        T.pack $
          printf "#%05d  %02d:%02d:%02d.%03d  [%-5s]  %-14s  %s (event #%d)" idx hr minu sec millis tag src msg idx

newAppState :: Int -> Bool -> IO AppState
newAppState initialCount streaming = do
  entries <- MV.new maxLogCapacity
  shown <- MU.new maxLogCapacity
  appendEntries
    AppState
      { asEntries = entries
      , asCount = 0
      , asNextId = 1
      , asShown = shown
      , asShownStart = 0
      , asShownCount = 0
      , asStreaming = streaming
      , asLastStream = 0
      , asFilterLevel = Nothing
      , asScrollerWid = Nothing
      }
    initialCount

-- | Append @count@ generated entries. The burst buttons, the streaming tick
-- and the selftest all append through here.
appendEntries :: AppState -> Int -> IO AppState
appendEntries st0 count = foldM push st0 [asNextId st0 .. asNextId st0 + count - 1]
 where
  push st i = do
    let entry = generateLogEntry i
        live = min maxLogCapacity (asCount st + 1)
    MV.write (asEntries st) (i `mod` maxLogCapacity) entry
    -- Ids below the oldest live entry were overwritten.
    let dropStale start n
          | n == 0 = pure (start, n)
          | otherwise = do
              front <- MU.read (asShown st) start
              if front <= i - live
                then dropStale ((start + 1) `mod` maxLogCapacity) (n - 1)
                else pure (start, n)
    (start, n) <- dropStale (asShownStart st) (asShownCount st)
    shown <-
      if maybe True (== leLevel entry) (asFilterLevel st)
        then do
          MU.write (asShown st) ((start + n) `mod` maxLogCapacity) i
          pure (n + 1)
        else pure n
    pure st {asCount = live, asNextId = i + 1, asShownStart = start, asShownCount = shown}

-- | Show the live entries at @level@, or all of them.
setFilter :: Maybe LogLevel -> AppState -> IO AppState
setFilter level st = do
  let firstId = asNextId st - asCount st
  shown <-
    foldM
      ( \n i -> do
          entry <- MV.read (asEntries st) (i `mod` maxLogCapacity)
          if maybe True (== leLevel entry) level
            then MU.write (asShown st) n i >> pure (n + 1)
            else pure n
      )
      0
      [firstId .. asNextId st - 1]
  pure st {asFilterLevel = level, asShownStart = 0, asShownCount = shown}

-- | The @k@th shown entry.
shownEntry :: AppState -> Int -> IO LogEntry
shownEntry st k = do
  i <- MU.read (asShown st) ((asShownStart st + k) `mod` maxLogCapacity)
  MV.read (asEntries st) (i `mod` maxLogCapacity)

levelColor :: LogLevel -> Color
levelColor = \case
  LevelDebug -> colorRGBA 180 142 173 255
  LevelInfo -> colorRGBA 136 192 208 255
  LevelWarn -> colorRGBA 235 203 139 255
  LevelError -> colorRGBA 191 97 106 255

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

  -- The state lives outside the store, so a change must wake and repaint.
  let mutateState f = uiIO $ do
        readIORef stateRef >>= f >>= writeIORef stateRef
        markDirty ctx
        damageFull ctx

  st0 <- uiIO $ readIORef stateRef

  when (asStreaming st0 && now - asLastStream st0 >= 0.08) $
    mutateState $ \s -> (\s' -> s' {asLastStream = now}) <$> appendEntries s 1

  -- Keep the loop running between streaming ticks.
  when (asStreaming st0) $
    uiIO $ markDirty ctx

  (allSelected, setAllSelected) <- withKey ("log-all-selected" :: Text) (useFlag False)

  mMenuAction <- uiIO $ takeTextEditLastAction ctx
  case mMenuAction of
    Just (_, SelectAll) -> setAllSelected True
    _ -> pure ()

  let mods = inputModifiers inp
      chars = inputChars inp
      aPressed = modCtrl mods && T.any (`T.elem` "aA\x01") chars
      cPressed = modCtrl mods && T.any (`T.elem` "cC\ETX") chars

  when aPressed $ setAllSelected True

  when (allSelected && inputKeysElem KeyEscape (inputKeys inp)) $
    setAllSelected False

  -- A left click on a selectable row clears the Select-All highlight. The
  -- scrollbar is chrome, not an interactive widget, so `ctxActiveId` stays 0
  -- for gutter grabs and only an actual row press clears.
  when (allSelected && (inputMousePressed inp || inputMouseDown inp || inputMouseReleased inp)) $ do
    active <- uiIO $ readIORef (ctxActiveId ctx)
    mRect <- uiIO $ maybe (pure Nothing) (getPrevRect ctx) (asScrollerWid st0)
    when (active /= WidgetId 0 && maybe False (`rectContains` inputMousePos inp) mRect) $
      setAllSelected False

  columnWith (tight . fillW . fillH . gap 0) $ do
    stLive <- uiIO $ readIORef stateRef
    renderHeaderToolbar mutateState stLive (allSelected, setAllSelected)
    separator

    -- Re-read after the toolbar so its changes lay out in this frame.
    st <- uiIO $ readIORef stateRef
    renderLogScroller stateRef allSelected st

    -- Copy after the scroller pass: a focused row's own Ctrl+C runs inside
    -- selectableTextWith during the scroller pass and would otherwise overwrite
    -- the clipboard with a single row.
    let copyAll = uiIO $ do
          rows <- forM [0 .. asShownCount st - 1] (fmap leLine . shownEntry st)
          void (ctxClipboardSet ctx (T.unlines rows))
    case mMenuAction of
      Just (_, Copy) | allSelected -> copyAll
      _ -> when (allSelected && cPressed) copyAll

    separator
    renderStatusBar (asCount st) (asShownCount st) allSelected

renderHeaderToolbar ::
  Ui :> es =>
  ((AppState -> IO AppState) -> Eff es ()) ->
  AppState ->
  (Bool, Bool -> Eff es ()) ->
  Eff es ()
renderHeaderToolbar mutateState st (allSelected, setAllSelected) = do
  let totalCount = asCount st
      filteredCount = asShownCount st
      filterPill lvl lbl = do
        clicked <- buttonWith (if asFilterLevel st == lvl then fontBold else id) lbl
        when clicked $
          mutateState (setFilter lvl)
  styled (panelStyle (background (colorRGBA 24 29 38 255) . borderColor (colorRGBA 45 52 64 255))) $ panelWith fillW $ do
    columnWith (tight . fillW . padXY 12 10 . gap 8) $ do
      rowWith (tight . fillW . alignMid . gap 12) $ do
        labelWith (fontBold . fontSize 16 . tight) "Log Viewer"
        labelWith (fontMono . fontMuted . tight) ("[" <> T.pack (show totalCount) <> " total]")

        when (filteredCount /= totalCount) $
          labelWith (fontMono . fontColor (colorRGBA 235 203 139 255) . tight)
            ("[" <> T.pack (show filteredCount) <> " filtered]")

        if asStreaming st
          then labelWith (fontMono . fontBold . fontColor (colorRGBA 163 190 140 255) . tight) "[● STREAMING]"
          else labelWith (fontMono . fontMuted . tight) "[⏸ PAUSED]"

        when allSelected $
          labelWith (fontMono . fontBold . fontColor (colorRGBA 235 203 139 255) . tight) "[● ALL SELECTED]"

      rowWith (tight . fillW . alignMid . gap 8) $ do
        streamClicked <- button (if asStreaming st then "Pause Stream" else "Start Stream")
        when streamClicked $
          mutateState (\s -> pure s {asStreaming = not (asStreaming s), asLastStream = 0.0})

        burst100Clicked <- button "+100"
        when burst100Clicked $
          mutateState (`appendEntries` 100)

        burst1000Clicked <- button "+1000"
        when burst1000Clicked $
          mutateState (`appendEntries` 1000)

        clearClicked <- button "Clear"
        when clearClicked $
          mutateState (\s -> pure s {asCount = 0, asNextId = 1, asShownStart = 0, asShownCount = 0})

        spacer (Fixed 8) Fit

        selAllClicked <- buttonWith (if allSelected then fontBold else id) (if allSelected then "Deselect All" else "Select All")
        when selAllClicked $
          setAllSelected (not allSelected)

        spacer (Fixed 8) Fit

        labelWith (fontMuted . tight) "Filter:"
        filterPill Nothing "ALL"
        filterPill (Just LevelInfo) "INFO"
        filterPill (Just LevelWarn) "WARN"
        filterPill (Just LevelError) "ERROR"
        filterPill (Just LevelDebug) "DEBUG"

renderLogScroller :: Ui :> es => IORef AppState -> Bool -> AppState -> Eff es ()
renderLogScroller stateRef allSelected st = do
  scrollWid <- withKey ("log-scroller" :: Text) nextId
  when (asScrollerWid st /= Just scrollWid) $
    uiIO $ writeIORef stateRef st {asScrollerWid = Just scrollWid}
  ctx <- askContext
  mPrevRect <- uiIO $ getPrevRect ctx scrollWid
  inp <- askInput

  (sticky, setSticky) <- withKey ("log-sticky" :: Text) (useFlag True)
  (prevScrollY, setPrevScrollY) <- withKey ("log-prev-scrolly" :: Text) (useFloat 0)
  (reqJump, setReqJump) <- withKey ("log-req-jump" :: Text) (useFlag False)

  let n = asShownCount st
      totalH = fromIntegral n * logRowH
      viewH = maybe (sizeH (inputWindowSize inp) - logChromeFallbackH) rectH mPrevRect
      maxOff = max 0 (totalH - viewH)

  curOff <- uiIO $ getScrollOffset2D ctx scrollWid
  let curY = v2Y curOff
      curX = v2X curOff
      -- The offset moved since last frame, so the user scrolled.
      userScrolled = abs (curY - prevScrollY) > 0.5
      atBottomNow = (maxOff <= 0) || (curY >= maxOff - logStickyThresh)
      -- Scrolling decides stickiness: to the bottom pins, anywhere else
      -- unpins. Without a scroll it holds, and a jump request pins.
      isSticky = reqJump || if userScrolled then atBottomNow else sticky
      -- A pinned view follows the bottom even when the buffer shrinks; an
      -- unpinned one stays put, clamped so it never shows blank space.
      targetY = if isSticky then maxOff else min maxOff curY
  effY <-
    if abs (targetY - curY) > 0.5
      then do
        uiIO $ setScrollOffset2D ctx scrollWid (V2 curX targetY)
        pure targetY
      else pure curY

  setSticky isSticky
  setPrevScrollY effY
  when reqJump $ setReqJump False

  rowWith (tight . fillW . padXY 12 4 . alignMid . gap 8) $ do
    if isSticky
      then labelWith (fontMono . fontBold . fontColor (colorRGBA 163 190 140 255) . tight) "● PINNED"
      else do
        labelWith (fontMono . fontBold . fontColor (colorRGBA 235 203 139 255) . tight) "⏸ UNPINNED (reading history)"
        jumpClicked <- buttonWith (fontColor (colorRGBA 136 192 208 255) . fontBold) "Jump to Bottom"
        when jumpClicked $
          setReqJump True

  -- Visible rows plus one above and below.
  let (firstVis, lastVis) =
        if n <= 0 || viewH <= 0
          then (0, -1)
          else
            let lo = max 0 (floor (effY / logRowH) - 1)
                hi = min (n - 1) (floor ((effY + viewH - 1) / logRowH) + 1)
             in if hi < lo then (0, -1) else (lo, hi)
      topH = fromIntegral firstVis * logRowH
      botH = fromIntegral (max 0 (n - lastVis - 1)) * logRowH

  -- The same key scope gives the scroll area the id read above as scrollWid,
  -- so this frame's offset and sticky state apply to it.
  void $ withKey ("log-scroller" :: Text) $ scrollArea2D (fillW . fillH) $ do
    columnWith (tight . gap 0 . minW 1200) $ do
      when (topH > 0) $ spacer Fit (Fixed topH)
      forM_ [firstVis .. lastVis] $ \idx -> do
        entry <- uiIO $ shownEntry st idx
        withKey (leId entry) $ renderLogRow allSelected entry
      when (botH > 0) $ spacer Fit (Fixed botH)

renderLogRow :: Ui :> es => Bool -> LogEntry -> Eff es ()
renderLogRow isAllSel entry = do
  let col = levelColor (leLevel entry)
      rowLay = tight . fixedH logRowH . padXY 8 2 . alignMid
      rowBody = selectableTextWith (fontColor col . fontMono . tight) (leLine entry)
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

main :: IO ()
main = do
  args <- getArgs
  if "--selftest" `elem` args
    then selftest
    else do
      appStateRef <- newIORef =<< newAppState 120 True
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

-- | Drive the viewer on a hidden window: pinning while entries arrive,
-- scrolling back, Jump to Bottom, filters, horizontal scroll, Select All,
-- copying, and clearing the selection.
{-# NOINLINE selftest #-}
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
      exitSuccess
