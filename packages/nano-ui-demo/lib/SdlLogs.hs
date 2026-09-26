-- | A log viewer on the SDL3 backend: virtualized rows, selectable text, and a
-- view that follows new entries until you scroll up.
--
-- Run with @cabal run nano-ui-sdl-logs@.
module SdlLogs
  ( main
  , AppState (..)
  , newAppState
  , appendEntries
  , logsApp
  ) where

import Control.Monad (foldM, forM, forM_, void, when)
import Data.Foldable (for_)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector.Mutable qualified as MV
import Data.Vector.Unboxed.Mutable qualified as MU
import NanoUI
import NanoUI.Backend.Sdl
import NanoUI.Internal.Context
  ( Context (..)
  , damageFull
  , getPrevRect
  , markDirty
  , takeTextEditLastAction
  )
import NanoUI.Internal.Monad (askContext)
import NanoUI.Shortcut
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
   in LogEntry idx lvl $
        T.pack $
          printf "#%05d  %02d:%02d:%02d.%03d  [%-5s]  %-14s  %s (event #%d)" idx hr minu sec millis (levelTag lvl) src msg idx

levelTag :: LogLevel -> Text
levelTag = \case
  LevelDebug -> "DEBUG"
  LevelInfo -> "INFO"
  LevelWarn -> "WARN"
  LevelError -> "ERROR"

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
-- and the tests all append through here.
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
  LevelWarn -> amber
  LevelError -> colorRGBA 191 97 106 255

amber, green :: Color
amber = colorRGBA 235 203 139 255
green = colorRGBA 163 190 140 255

-- | A bold monospace status label.
status :: Color -> Text -> NanoUI ()
status c = labelWith (fontMono . fontBold . fontColor c . tight)

-- | A panel in its own background and border colours.
tintedPanel :: Color -> Color -> (Layout -> Layout) -> NanoUI a -> NanoUI a
tintedPanel bg border f = styled (panelStyle (background bg . borderColor border)) . panelWith f

logRowH :: Float
logRowH = 24.0

logStickyThresh :: Float
logStickyThresh = 8.0

-- Header toolbar + banner + status bar height; fallback viewport estimate
-- before the scroller has a previous-frame rect.
logChromeFallbackH :: Float
logChromeFallbackH = 140.0

{-# NOINLINE logsApp #-}
logsApp :: IORef AppState -> NanoUI ()
logsApp stateRef = do
  ctx <- askContext
  inp <- askInput
  now <- uiTime

  -- The state lives outside the store, so a change must wake and repaint.
  let mutateState f = liftIO $ do
        readIORef stateRef >>= f >>= writeIORef stateRef
        markDirty ctx
        damageFull ctx

  st0 <- liftIO $ readIORef stateRef

  let streamEvery = 0.08
      sinceStream = now - asLastStream st0
      streamDue = asStreaming st0 && sinceStream >= streamEvery
  when streamDue $
    mutateState $ \s -> (\s' -> s' {asLastStream = now}) <$> appendEntries s 1

  -- Sleep until the next streaming tick. Marking the context dirty every
  -- frame to get there would run frames back to back in between.
  when (asStreaming st0) $
    wakeAfter (if streamDue then streamEvery else streamEvery - sinceStream)

  (allSelected, setAllSelected) <- withKey ("log-all-selected" :: Text) (useFlag False)

  -- Ctrl+A and Ctrl+C, or Select All and Copy from a row's context menu.
  -- The chords act on every row even while a row has the keyboard, which
  -- 'shortcut' would leave to the row, so they are matched with 'shortcutIn'.
  menuAction <- fmap snd <$> liftIO (takeTextEditLastAction ctx)
  let chosen c cmd = menuAction == Just cmd || shortcutIn (ctrl <> key c) inp

  when (chosen 'a' SelectAll) $ setAllSelected True

  escape <- keyPressed KeyEscape
  when (allSelected && escape) $
    setAllSelected False

  -- A left click on a selectable row clears the Select-All highlight. The
  -- scrollbar is chrome, not an interactive widget, so `ctxActiveId` stays 0
  -- for gutter grabs and only an actual row press clears.
  when (allSelected && (pressedIn MouseLeft inp || heldIn MouseLeft inp || releasedIn MouseLeft inp)) $ do
    active <- liftIO $ readIORef (ctxActiveId ctx)
    mRect <- liftIO $ maybe (pure Nothing) (getPrevRect ctx) (asScrollerWid st0)
    when (active /= WidgetId 0 && maybe False (`rectContains` inputMousePos inp) mRect) $
      setAllSelected False

  columnWith (tight . fillW . fillH . gap 0) $ do
    stLive <- liftIO $ readIORef stateRef
    renderHeaderToolbar mutateState stLive (allSelected, setAllSelected)
    separator

    -- Re-read after the toolbar so its changes lay out in this frame.
    st <- liftIO $ readIORef stateRef
    renderLogScroller stateRef allSelected st

    -- Copy after the scroller pass: a focused row's own Ctrl+C runs inside
    -- selectableTextWith during the scroller pass and would otherwise overwrite
    -- the clipboard with a single row.
    let copyAll = liftIO $ do
          rows <- forM [0 .. asShownCount st - 1] (fmap leLine . shownEntry st)
          void (ctxClipboardSet ctx (T.unlines rows))
    when (allSelected && chosen 'c' Copy) copyAll

    separator
    renderStatusBar (asCount st) (asShownCount st) allSelected

renderHeaderToolbar ::
  ((AppState -> IO AppState) -> NanoUI ()) ->
  AppState ->
  (Bool, Bool -> NanoUI ()) ->
  NanoUI ()
renderHeaderToolbar mutateState st (allSelected, setAllSelected) = do
  let totalCount = asCount st
      filteredCount = asShownCount st
      filterPill lvl lbl =
        whenM (buttonWith (if asFilterLevel st == lvl then fontBold else id) lbl) $
          mutateState (setFilter lvl)
  tintedPanel (colorRGBA 24 29 38 255) (colorRGBA 45 52 64 255) fillW $ do
    columnWith (tight . fillW . padXY 12 10 . gap 8) $ do
      rowWith (tight . fillW . alignMid . gap 12) $ do
        labelWith (fontBold . fontSize 16 . tight) "Log Viewer"
        labelWith (fontMono . fontMuted . tight) ("[" <> T.pack (show totalCount) <> " total]")

        when (filteredCount /= totalCount) $
          labelWith (fontMono . fontColor amber . tight) ("[" <> T.pack (show filteredCount) <> " filtered]")

        if asStreaming st
          then status green "[● STREAMING]"
          else labelWith (fontMono . fontMuted . tight) "[⏸ PAUSED]"

        when allSelected $ status amber "[● ALL SELECTED]"

      rowWith (tight . fillW . alignMid . gap 8) $ do
        whenM (button (if asStreaming st then "Pause Stream" else "Start Stream")) $
          mutateState (\s -> pure s {asStreaming = not (asStreaming s), asLastStream = 0.0})
        whenM (button "+100") $ mutateState (`appendEntries` 100)
        whenM (button "+1000") $ mutateState (`appendEntries` 1000)
        whenM (button "Clear") $
          mutateState (\s -> pure s {asCount = 0, asNextId = 1, asShownStart = 0, asShownCount = 0})

        spacer (Fixed 8) Fit

        whenM (buttonWith (if allSelected then fontBold else id) (if allSelected then "Deselect All" else "Select All")) $
          setAllSelected (not allSelected)

        spacer (Fixed 8) Fit

        labelWith (fontMuted . tight) "Filter:"
        filterPill Nothing "ALL"
        for_ [LevelInfo, LevelWarn, LevelError, LevelDebug] $ \lvl -> filterPill (Just lvl) (levelTag lvl)

renderLogScroller :: IORef AppState -> Bool -> AppState -> NanoUI ()
renderLogScroller stateRef allSelected st = do
  scrollWid <- withKey ("log-scroller" :: Text) nextId
  when (asScrollerWid st /= Just scrollWid) $
    liftIO $ writeIORef stateRef st {asScrollerWid = Just scrollWid}
  ctx <- askContext
  mPrevRect <- liftIO $ getPrevRect ctx scrollWid
  inp <- askInput

  (sticky, setSticky) <- withKey ("log-sticky" :: Text) (useFlag True)
  (prevScrollY, setPrevScrollY) <- withKey ("log-prev-scrolly" :: Text) (useFloat 0)
  (reqJump, setReqJump) <- withKey ("log-req-jump" :: Text) (useFlag False)

  let n = asShownCount st
      totalH = fromIntegral n * logRowH
      viewH = maybe (sizeH (inputWindowSize inp) - logChromeFallbackH) rectH mPrevRect
      maxOff = max 0 (totalH - viewH)

  curOff <- liftIO $ getScrollOffset2D ctx scrollWid
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
        liftIO $ setScrollOffset2D ctx scrollWid (V2 curX targetY)
        pure targetY
      else pure curY

  setSticky isSticky
  setPrevScrollY effY
  when reqJump $ setReqJump False

  rowWith (tight . fillW . padXY 12 4 . alignMid . gap 8) $ do
    if isSticky
      then status green "● PINNED"
      else do
        status amber "⏸ UNPINNED (reading history)"
        whenM (buttonWith (fontColor (colorRGBA 136 192 208 255) . fontBold) "Jump to Bottom") $
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
        entry <- liftIO $ shownEntry st idx
        withKey (leId entry) $ renderLogRow allSelected entry
      when (botH > 0) $ spacer Fit (Fixed botH)

renderLogRow :: Bool -> LogEntry -> NanoUI ()
renderLogRow isAllSel entry = do
  let col = levelColor (leLevel entry)
      rowLay = tight . fixedH logRowH . padXY 8 2 . alignMid
      rowBody = selectableTextWith (fontColor col . fontMono . tight) (leLine entry)
  if isAllSel
    then tintedPanel (colorRGBA 45 65 95 255) (colorRGBA 70 100 145 255) rowLay rowBody
    else rowWith rowLay rowBody

renderStatusBar :: Int -> Int -> Bool -> NanoUI ()
renderStatusBar totalCount filteredCount allSelected = do
  tintedPanel (colorRGBA 20 24 32 255) (colorRGBA 45 52 64 255) fillW $ do
    rowWith (tight . fillW . padXY 12 4 . alignMid . gap 16) $ do
      labelWith (fontMono . fontMuted . tight)
        ("Total: " <> T.pack (show totalCount) <> " logs | Filtered: " <> T.pack (show filteredCount))
      if allSelected
        then status amber "ALL LOGS SELECTED | Ctrl+C to copy all | ESC to clear"
        else
          labelWith (fontMuted . tight)
            "Tip: Click & drag to select | Right-click for Copy/Select All | 2D Scroll | Ctrl+Q to quit"

main :: IO ()
main = do
  appStateRef <- newIORef =<< newAppState 120 True
  runSdlApp
    defaultSdlOptions
      { sdlWindowSettings = defaultWindowSettings {wsTitle = "nano-ui Log Viewer", wsSize = Size 1050 720}
      , sdlAppTheme = Just tomorrowNightMinDarkTheme
      -- ESC clears the selection (handled in logsApp); quit is Ctrl+Q.
      , sdlAppShouldQuit = shortcutIn (ctrl <> key 'q')
      }
    (logsApp appStateRef)
