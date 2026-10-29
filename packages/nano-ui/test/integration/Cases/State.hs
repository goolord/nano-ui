{-# OPTIONS_GHC -O0 #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Cases.State (tests) where

import Spec
import Control.Concurrent (threadDelay)
import Control.Exception (ErrorCall (..), throwIO, try)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful (Eff, IOE, runEff)
import Effectful.State.Static.Local (State, evalState, get, modify)
import GHC.Clock (getMonotonicTime)
import GHC.Generics (Generic)
import NanoUI.Effectful (Ui, embedNanoUI, runFrameEff, withRunInNanoUI)
import NanoUI.Monad (localInput)
import NanoUI.Shortcut qualified as Shortcut
import NanoUI.Testing.Sim

tests :: [Spec]
tests =
  [ spec "controlled-inputs" runControlledInputsTest
  , pixelSpec "basic-widgets" runBasicWidgetsTest
  , spec "var-slots" runVarSlotTest
  , spec "var-lifecycle" runVarLifecycleTest
  , spec "var-focus" runVarFocusTest
  , spec "var-shared" runVarSharedTest
  , spec "var-late-writes" runVarLateWritesTest
  , spec "embed-state" runEmbedStateTest
  , spec "host-slot" runHostSlotTest
  , varSimSpec "search-input-debounce" 320 100 "" debounced searchDebounce
  ]

-- | Inputs show the caller's value, reporting no change for the caller's
-- own; a user edit is reported and stays only if the caller passes it back.
runControlledInputsTest :: Context -> IORef Int -> IO ()
runControlledInputsTest ctx failed = do
  model <- newIORef (False, "one")
  let inp = withInputOff 300 200
      ui = column $ do
        (checked, text) <- liftIO (readIORef model)
        (cb, checked') <- withValue checked (checkbox' "Opt")
        text' <- snd <$> withValue text textInput
        pure (cb, (respChanged cb, (checked', text')))
  _ <- warmup2 ctx inp ui
  forM_
    [ -- The caller's new values, with no input.
      (inp, (True, "two"), (False, (True, "two")))
    , -- Tab focuses the checkbox; Space toggles it and the caller keeps it.
      (tabInp inp, (True, "two"), (False, (True, "two")))
    , ((keyInp KeySpace inp) {inputChars = " "}, (True, "two"), (True, (False, "two")))
    , (tabInp inp, (False, "two"), (False, (False, "two")))
    , -- Tab moved focus to the field. A kept edit stays; an ignored one goes.
      (inp {inputChars = "x"}, (False, "two"), (False, (False, "twox")))
    , (inp, (False, "twox"), (False, (False, "twox")))
    , (inp {inputChars = "y"}, (False, "twox"), (False, (False, "twoxy")))
    , (inp, (False, "twox"), (False, (False, "twox")))
    , -- Back on the checkbox, Enter toggles it, and the caller declines.
      (tabInp inp, (False, "twox"), (False, (False, "twox")))
    , (keyInp KeyEnter inp, (False, "twox"), (True, (True, "twox")))
    , (inp, (False, "twox"), (False, (False, "twox")))
    ]
    $ \(input, value, want@(_, (checked, text))) -> do
      writeIORef model value
      (cb, got) <- evalUi ctx input ui
      assertEq failed want got
      assertEq failed (Just (if checked then 1 else 0)) =<< widgetValue ctx (respId cb)
      assert failed . any (\(_, t, _, _, _) -> t == text) =<< collectTextSpans ctx

-- | Positional Vars of every slot type keep their value, wake only on a
-- change, start over on a value of another type, and ignore widgets that
-- come and go before them. A Var map keeps a row until it is dropped.
runVarSlotTest :: Context -> IORef Int -> IO ()
runVarSlotTest ctx failed = do
  let inp = withInputOff 300 100
      keyed :: Text -> NanoUI a -> IO a
      keyed k = runNanoUI ctx inp . withKey k
      holds :: (Eq a, Show a) => Text -> NanoUI (Var a) -> a -> a -> IO ()
      holds k slot initial changed = do
        let evaluate = keyed k (slot >>= \v -> (v,) <$> readVar v)
        (v, value) <- evaluate
        assertEq failed initial value
        clearDirty ctx
        runNanoUI ctx inp (writeVar v initial)
        assertEq failed False =<< isDirty ctx
        runNanoUI ctx inp (writeVar v changed)
        assertEq failed True =<< isDirty ctx
        assertEq failed changed . snd =<< evaluate
  holds "int" (useVar (0 :: Int)) 0 12
  holds "float" (useVar (0 :: Float)) 0 1.5
  holds "double" (useVar (0 :: Double)) 0 2.5
  holds "text" (useVar ("initial" :: Text)) "initial" "changed"
  holds "flag" (useVar False) False True
  holds "enum" (useEnumVar LT) LT GT
  holds "sort" (useVar (SortCol 0 SortAsc)) (SortCol 0 SortAsc) (SortCol 2 SortDesc)
  let asInt = keyed "slot" (readVar =<< useVar (5 :: Int))
  assertEq failed 5 =<< asInt
  keyed "slot" (useVar (5 :: Int) >>= \v -> writeVar v 9)
  assertEq failed 9 =<< asInt
  assertEq failed "five" =<< keyed "slot" (readVar =<< useVar ("five" :: Text))
  assertEq failed 5 =<< asInt
  keyed "enum" (useEnumVar LT >>= \v -> writeVar v EQ)
  assertEq failed False =<< keyed "enum" (readVar =<< useEnumVar False)
  let view extra = column $ do
        when extra (void (button "extra"))
        n <- useVar (0 :: Int)
        r <- button' "+"
        when (respClicked r) (modifyVar n (+ 1))
        (r,) <$> readVar n
  (r0, _) <- warmup2 ctx inp (view False)
  assertEq failed 1 . snd =<< runClick ctx inp (view False) (centerOf r0)
  assertEq failed 1 . snd =<< warmup2 ctx inp (view True)
  m <- newVarMap
  let row' k = runNanoUI ctx inp (varAt m (k :: Int) (0 :: Int))
  a <- row' 1
  runNanoUI ctx inp (writeVar a 5)
  assertEq failed 5 =<< (readVarIO =<< row' 1)
  assertEq failed 0 =<< (readVarIO =<< row' 2)
  runNanoUI ctx inp (dropVar m 1)
  assertEq failed 0 =<< (readVarIO =<< row' 1)
  runNanoUI ctx inp (writeVar a 6 >> retainVars (/= 2) m)
  assertEq failed 0 =<< (readVarIO =<< row' 2)

-- | A key press runs once even when its write reruns the view (the Passes
-- spec counts what that costs). Owned Vars survive hiding and keyed
-- reordering, and independent ones do not share state.
runVarLifecycleTest :: Context -> IORef Int -> IO ()
runVarLifecycleTest ctx failed = do
  left <- newVar (0 :: Int)
  let inp = withInputOff 300 100
      item :: Text -> NanoUI (Var Int)
      item name = withKey name (useVar 0)
      view = do
        label . T.pack . show =<< readVar left
        whenM (shortcut (Shortcut.key 'a')) $ do
          modifyVar left (+ 1)
          modifyVar left (+ 1)
        readVar left
  _ <- warmup2 ctx inp view
  assertEq failed 2 =<< evalUi ctx (keyInp (KeyChar 'a') inp) view
  assert failed . any (\(_, text, _, _, _) -> text == "2") =<< collectTextSpans ctx
  runNanoUI ctx inp (modifyVar left (+ 3) >> writeVar left 0)
  assertEq failed 0 =<< readVarIO left
  (owned, _) <- evalUi ctx inp ((,) <$> item "left" <*> item "right")
  writeVarIO owned 7
  _ <- runFrame ctx inp (item "right")
  (right, left') <- evalUi ctx inp ((,) <$> item "right" <*> item "left")
  assertEq failed (0, 7) =<< ((,) <$> readVarIO right <*> readVarIO left')

data Item = Item {rowKey :: !Int, rowName :: !Text, rowDone :: !Bool}
  deriving (Eq, Show, Generic)

newtype Items = Items {rows :: [Item]}
  deriving (Eq, Show, Generic)

-- | Field focus and element Vars write through to the whole value, an
-- element's Var follows its key when the list is reordered, and a write
-- through a removed element's Var changes nothing, so wakes nothing.
runVarFocusTest :: Context -> IORef Int -> IO ()
runVarFocusTest ctx failed = do
  model <- newVar (Items [Item 1 "one" False, Item 2 "two" False, Item 3 "three" False])
  let inp = withInputOff 400 300
      view = column $ forEachVar rowKey model.rows $ \item -> do
        checkbox "done" item.rowDone
        label =<< readVar item.rowName
  _ <- warmup2 ctx inp view
  runNanoUI ctx inp (modifyVar model.rows reverse)
  _ <- warmup2 ctx inp view
  first : _ <- runNanoUI ctx inp (withElemVars rowKey model.rows (\xs elemVar -> pure (map elemVar xs)))
  runNanoUI ctx inp (writeVar first.rowName "THREE")
  assertEq failed ["THREE", "two", "one"] . map rowName . rows =<< readVarIO model
  runNanoUI ctx inp (modifyVar model.rows (filter ((/= 3) . rowKey)))
  clearDirty ctx
  runNanoUI ctx inp (writeVar first.rowDone True)
  assertEq failed [False, False] . map rowDone . rows =<< readVarIO model
  assertEq failed False =<< isDirty ctx
  pair <- newVar ((1 :: Int), ("a" :: Text))
  runNanoUI ctx inp (writeVar (zoomVar snd (\b (a, _) -> (a, b)) pair) "b")
  assertEq failed (1, "b") =<< readVarIO pair

-- | A Var shown in two contexts: a write in one is not a backwards write in
-- the other, and each repaints what it shows.
runVarSharedTest :: Context -> IORef Int -> IO ()
runVarSharedTest ctx failed = do
  other <- newContext
  shared <- newVar (0 :: Int)
  let inp = withInputOff 300 100
      view = column $ do
        label . T.pack . show =<< readVar shared
        r <- button' "+"
        when (respClicked r) (modifyVar shared (+ 1))
        pure r
  r <- warmup2 ctx inp view
  _ <- warmup2 other inp view
  _ <- runClick ctx inp view (centerOf r)
  assertEq failed 1 =<< readVarIO shared
  _ <- runFrame other inp view
  assert failed . any (\(_, t, _, _, _) -> t == "1") =<< collectTextSpans other

-- | Writes a frame's bookkeeping misses repaint the next frame whole: one in
-- a frame an exception cut short (leaving no rerun), and a thread's write
-- landing after the pass read the Var, also beside a write to another part.
runVarLateWritesTest :: Context -> IORef Int -> IO ()
runVarLateWritesTest ctx failed = do
  v <- newVar (0 :: Int)
  item <- newVar (Item 1 "one" False)
  let inp = withInputOff 300 100
      view = label . T.pack . show =<< readVar v
      failing = readVar v >> writeVar v 1 >> liftIO (throwIO (ErrorCall "view failed") :: IO ())
  _ <- warmup2 ctx inp view
  r <- try @ErrorCall (runFrame ctx inp failing)
  assert failed (either (const True) (const False) r)
  _ <- runFrame ctx inp view
  assertEq failed 1 =<< lastFramePasses ctx
  assertEq failed DamageFull =<< takeDamage ctx
  let named = label =<< readVar item.rowName
      -- The thread's write, as if it landed here.
      landing name = liftIO (writeVarIO item.rowName name)
  _ <- warmup2 ctx inp named
  forM_ [landing "uno", landing "ein" >> writeVar item.rowDone True] $ \late -> do
    _ <- runFrame ctx inp (named >> late)
    _ <- runFrame ctx inp named
    assertEq failed DamageFull =<< takeDamage ctx

-- | Pointer cursor over a button and a (pressed) checkbox only. A checkbox
-- shows its Var's value before any click stores state, and each click at
-- once. A slider in a filling column spans it to its maximum.
runBasicWidgetsTest :: Context -> IORef Int -> IO ()
runBasicWidgetsTest ctx failed = do
  checked <- newVar True
  level <- newVar 0
  let ui = columnWith fillW ((,,) <$> button' "Click" <*> checkbox' "Opt" checked <*> slider' 0 100 level)
      wantsPointer = liftIO . pointerCursorWanted ctx =<< getInput
      showsChecked cb on = do
        checkEq on =<< readV checked
        checkEq (Just (if on then 1 else 0)) =<< liftIO (widgetValue ctx (respId cb))
  runSim ctx failed (withInput 400 200) ui $ do
    (btn, cb, sl) <- result
    showsChecked cb True
    check =<< (moveTo (On btn) >> wantsPointer)
    check . not =<< (moveTo (At (V2 (-1) (-1))) >> wantsPointer)
    check =<< (moveTo (On cb) >> wantsPointer)
    check =<< (press (On cb) >> wantsPointer)
    _ <- release
    showsChecked cb False
    _ <- frame
    showsChecked cb False
    let Rect cx cy _ _ = respRect cb
    _ <- click (At (V2 (cx + 1) (cy + 0.5)))
    showsChecked cb True
    let Rect sx sy sw sh = respRect sl
        Rect tx ty tw th = sliderTrackBounds sx sy sw sh
    checkGt sw 300
    _ <- press (At (V2 (tx + tw - 2) (ty + th / 2)))
    flip checkGt 90 =<< readV level

-- | An Effectful view embeds NanoUI, and a body run in a NanoUI scope keeps
-- the effect's state and sees that scope's input.
runEmbedStateTest :: Context -> IORef Int -> IO ()
runEmbedStateTest ctx failed = do
  let ui :: Eff '[Ui, State Int, IOE] (Int, Float)
      ui = do
        _ <- embedNanoUI (column (pure ()))
        modify (+ (1 :: Int))
        w <- withRunInNanoUI $ \run -> column $ do
          run (modify (+ (1 :: Int)))
          inp <- askInput
          localInput inp {inputWindowSize = Size 123 45} (run (embedNanoUI windowWidth))
        (,w) <$> get
  ((n, w), _, _) <- runFrameEff (runEff . evalState (0 :: Int)) ctx (withInput 80 80) ui
  assertEq failed (2, 123) (n, w)

-- | Hosts hand the view what the app set, each of its own type, and a compact
-- host hands back the whole value.
runHostSlotTest :: Context -> IORef Int -> IO ()
runHostSlotTest ctx failed = do
  stringHost <- newHost
  intHost <- newHost
  otherHost <- newHost
  compactOwner <- newHost
  let ask :: Host a -> IO (Maybe a)
      ask h = evalUi ctx (withInput 80 80) (column (pure ()) >> askHost h)
  assertEq failed Nothing =<< ask stringHost
  setHost stringHost ("ok" :: String)
  setHost intHost (1 :: Int)
  setHost otherHost ("independent" :: String)
  assertEq failed (Just "ok") =<< ask stringHost
  assertEq failed (Just 1) =<< ask intHost
  assertEq failed (Just "independent") =<< ask otherHost
  _ <- compactHost compactOwner ([0 .. 9999] :: [Int])
  got <- evalUi ctx (withInput 80 80) (column (pure ()) >> askCompact compactOwner)
  assertEq failed (Just (10000, 9999)) ((\xs -> (length xs, last xs)) <$> got)

-- | A search field on a 20 ms debounce, bound to the Var.
debounced :: Var Text -> NanoUI (Response, Text)
debounced query = column (held query (searchInputConfigured' defaultSearchInputConfig {sicDebounceMs = 20}))

-- | A search field commits caller-set text after one pause (with no edit time
-- it once never did, waking every period), and typing on a wake 20 ms after
-- the last edit.
searchDebounce :: Var Text -> Sim (Response, Text) ()
searchDebounce query = do
  let showing want changed (resp, txt) = checkEq (want, changed) (txt, respChanged resp)
      afterPause us = liftIO (threadDelay us) >> frame
  pressTab 1
  writeV query "recent"
  showing "recent" False =<< frame
  wakeTime >>= \t -> checkGt t 0
  showing "recent" True =<< afterPause 30000
  checkEq 0 =<< wakeTime
  writeV query ""
  _ <- afterPause 30000
  showing "a" False =<< typeText "a"
  before <- liftIO getMonotonicTime
  showing "ab" False =<< typeText "b"
  after <- liftIO getMonotonicTime
  -- The edit is stamped during its frame: the wake is 20 ms past a moment in it.
  wakeTime >>= \t -> check (t >= before + 0.02 && t <= after + 0.022)
  showing "ab" True =<< afterPause 30000
  checkEq 0 =<< wakeTime
  showing "ab" False =<< afterPause 25000
