{-# LANGUAGE OverloadedRecordDot #-}

module Cases.State (tests) where

import Spec
import Control.Exception (ErrorCall (..), throwIO, try)
import Data.ByteString qualified as BS
import Data.IntMap.Strict qualified as IM
import Data.Text (Text)
import Data.Text qualified as T
import Data.Sequence qualified as Seq
import Data.Primitive.SmallArray qualified as SA
import GHC.Generics (Generic)
import NanoUI.Internal.Context (Context (..), intKey, lookupImageUv)
import NanoUI.Internal.Store (WidgetStore (..))
import NanoUI.Shortcut qualified as Shortcut

tests :: [Spec]
tests =
  [ spec "controlled-state" runControlledStateTest
  , spec "controlled-inputs" runControlledInputsTest
  , spec "var-slots" runVarSlotTest
  , spec "var-slot-identity" runVarSlotIdentityTest
  , spec "var-lifecycle" runVarLifecycleTest
  , spec "var-focus" runVarFocusTest
  , spec "var-map" runVarMapTest
  , spec "var-shared" runVarSharedTest
  , spec "var-abort" runVarAbortTest
  , spec "var-io-in-pass" runVarIoInPassTest
  , spec "collection-api" runCollectionApiTest
  ]

runCollectionApiTest :: Context -> IORef Int -> IO ()
runCollectionApiTest ctx failed = do
  seen <- newIORef []
  _ <- runFrame ctx (withInputOff 300 100) $
    hstack (SA.smallArrayFromList [liftIO (modifyIORef' seen (key :)) | key <- [7, 2, 9 :: Int]])
  assertEq failed [9, 2, 7] =<< readIORef seen
  ((emptySelect, emptyRadio, combo), _, _) <- runFrame ctx (withInputOff 300 200) $
    withKey ("collection-options" :: Text) $ column $ do
      selectIndex <- (snd <$> withValue 5 (select (SA.emptySmallArray :: SA.SmallArray Text)))
      radioIndex <- (snd <$> withValue (-1) (radio (Seq.empty :: Seq.Seq Text)))
      comboValue <- (snd <$> withValue "Beta" (comboBox "Choose" (Seq.fromList ["Alpha", "Beta"])))
      pure (selectIndex, radioIndex, comboValue)
  assertEq failed 0 emptySelect
  assertEq failed 0 emptyRadio
  assertEq failed "Beta" combo
  -- A failed image must not prevent later registrations in traversal order.
  assertEq failed False =<< registerImages ctx (Seq.fromList [(ImageId 0, 1, 1, BS.replicate 4 255), (ImageId 42, 1, 1, BS.replicate 4 255)])
  registered <- lookupImageUv ctx (ImageId 42)
  assertEq failed True (case registered of Just _ -> True; Nothing -> False)

runControlledStateTest :: Context -> IORef Int -> IO ()
runControlledStateTest ctx failed = do
  callbacks <- newIORef []
  let
    inp = withInputOff 300 200
    ui checked text value = column $ do
      expectedId <- currentId
      (check, checked') <- withValue checked (checkbox' "Controlled")
      when (respChanged check) (liftIO (modifyIORef' callbacks (<> [checked'])))
      (field, _) <- withValue text textInput'
      (range, _) <- withValue value (slider' 0 100)
      pure (expectedId, check, field, range)
  _ <- runFrame ctx inp (ui True "initial" 25)
  ((expectedId, check, field, range), _, _) <-
    runFrame ctx inp (ui False "replacement" 75)
  assertEq failed expectedId (respId check)
  store <- getStore ctx
  assertEq failed (Just 0) (IM.lookup (intKey (respId check)) (storeInt store))
  assertEq failed (Just "replacement") (IM.lookup (intKey (respId field)) (storeText store))
  assertEq failed (Just 75) (IM.lookup (intKey (respId range)) (storeFloat store))
  assertEq failed [] =<< readIORef callbacks

  -- Keyboard activation notifies the owner, which may decline the change.
  writeIORef (ctxFocusId ctx) (respId check)
  _ <- runFrame ctx (keyInp KeyEnter inp) (ui False "replacement" 75)
  assertEq failed [True] =<< readIORef callbacks
  _ <- runFrame ctx inp (ui False "replacement" 75)
  settled <- getStore ctx
  assertEq failed (Just 0) (IM.lookup (intKey (respId check)) (storeInt settled))
  assertEq failed [True] =<< readIORef callbacks

-- | Inputs show the value the caller passes: a value the caller changes
-- between frames is shown, a user edit the caller passes back is kept, and
-- one it ignores is undone on the next frame. 'runControlledStateTest' covers
-- a declined checkbox toggle.
runControlledInputsTest :: Context -> IORef Int -> IO ()
runControlledInputsTest ctx failed = do
  let
    inp = withInputOff 300 200
    ui (checked, text) = column $ do
      checked' <- (snd <$> withValue checked (checkbox "Opt"))
      text' <- (snd <$> withValue text textInput)
      pure (checked', text')
  _ <- warmup2 ctx inp (ui (False, "one"))
  forM_
    [ -- The caller's new values, with no input.
      (inp, (True, "two"), (True, "two"))
    , -- Tab focuses the checkbox; Space toggles it and the caller keeps it.
      (tabInp inp, (True, "two"), (True, "two"))
    , ((keyInp KeySpace inp) {inputChars = " "}, (True, "two"), (False, "two"))
    , (tabInp inp, (False, "two"), (False, "two"))
    , -- Tab moved focus to the field. A kept edit stays.
      (inp {inputChars = "x"}, (False, "two"), (False, "twox"))
    , (inp, (False, "twox"), (False, "twox"))
    , -- An ignored edit is undone on the following frame.
      (inp {inputChars = "y"}, (False, "twox"), (False, "twoxy"))
    , (inp, (False, "twox"), (False, "twox"))
    ]
    $ \(input, value, expected) -> do
      (result, _, _) <- runFrame ctx input (ui value)
      assertEq failed expected result
      spans <- collectTextSpans ctx
      assert failed (any (\(_, txt, _, _, _) -> txt == snd expected) spans)

-- | Positional Vars of every slot type keep a value across frames; an equal
-- write leaves the loop asleep, a changed one outside a frame wakes it.
runVarSlotTest :: Context -> IORef Int -> IO ()
runVarSlotTest ctx failed = do
  let
    inp = withInputOff 300 100
    check :: (Eq a, Show a) => Text -> NanoUI (Var a) -> a -> a -> IO ()
    check key slot initial changed = do
      let evaluate = runNanoUI ctx inp (withKey key (slot >>= \v -> (v,) <$> readVar v))
      (v, value) <- evaluate
      assertEq failed initial value
      clearDirty ctx
      runNanoUI ctx inp (writeVar v initial)
      assertEq failed False =<< isDirty ctx
      runNanoUI ctx inp (writeVar v changed)
      assertEq failed True =<< isDirty ctx
      assertEq failed changed . snd =<< evaluate
  check "int" (useVar (0 :: Int)) 0 12
  check "float" (useVar (0 :: Float)) 0 1.5
  check "double" (useVar (0 :: Double)) 0 2.5
  check "text" (useVar ("initial" :: Text)) "initial" "changed"
  check "flag" (useVar False) False True
  check "enum" (useEnumVar LT) LT GT
  check "sort" (useVar (SortCol 0 SortAsc)) (SortCol 0 SortAsc) (SortCol 2 SortDesc)

-- | A slot that finds a value of another type starts over, and widgets that
-- come and go before a positional Var leave it alone.
runVarSlotIdentityTest :: Context -> IORef Int -> IO ()
runVarSlotIdentityTest ctx failed = do
  let inp = withInputOff 300 100
      asInt = runNanoUI ctx inp (withKey ("slot" :: Text) (readVar =<< useVar (5 :: Int)))
      asText = runNanoUI ctx inp (withKey ("slot" :: Text) (readVar =<< useVar ("five" :: Text)))
  assertEq failed 5 =<< asInt
  _ <- runNanoUI ctx inp (withKey ("slot" :: Text) (useVar (5 :: Int) >>= \v -> writeVar v 9))
  assertEq failed 9 =<< asInt
  assertEq failed "five" =<< asText
  assertEq failed 5 =<< asInt
  -- Enumerations are stored by index, but one of another type starts over.
  _ <- runNanoUI ctx inp (withKey ("enum" :: Text) (useEnumVar LT >>= \v -> writeVar v EQ))
  assertEq failed False =<< runNanoUI ctx inp (withKey ("enum" :: Text) (readVar =<< useEnumVar False))
  let view extra = column $ do
        when extra (void (button "extra"))
        n <- useVar (0 :: Int)
        r <- button' "+"
        when (respClicked r) (modifyVar n (+ 1))
        (r,) <$> readVar n
  (r0, _) <- warmup2 ctx inp (view False)
  (_, after) <- runClick ctx inp (view False) (centerOf r0)
  assertEq failed 1 after
  (_, shownWithExtra) <- warmup2 ctx inp (view True)
  assertEq failed 1 shownWithExtra

-- | Owned Vars survive hiding and keyed reordering, independent ones do not
-- share state, and a key press runs once even when its write reruns the view.
runVarLifecycleTest :: Context -> IORef Int -> IO ()
runVarLifecycleTest ctx failed = do
  left <- newVar (0 :: Int)
  right <- newVar (0 :: Int)
  let inp = withInputOff 300 100
      item :: Text -> Var Int -> NanoUI Int
      item name v = withKey name $ do
        n <- readVar v
        label (T.pack (show n))
        pure n
      view = do
        n <- readVar left
        label (T.pack (show n))
        whenM (shortcut (Shortcut.key 'a')) $ do
          modifyVar left (+ 1)
          modifyVar left (+ 1)
        readVar left
  _ <- warmup2 ctx inp view
  (n, _, _) <- runFrame ctx (keyInp (KeyChar 'a') inp) view
  assertEq failed 2 n
  assertEq failed 2 =<< lastFramePasses ctx
  assertEq failed DamageFull =<< takeDamage ctx
  spans <- collectTextSpans ctx
  assert failed (any (\(_, text, _, _, _) -> text == "2") spans)
  (_, _, dirty) <- runFrame ctx inp view
  assertEq failed False dirty
  assert failed . damageIsEmpty =<< takeDamage ctx
  runNanoUI ctx inp (modifyVar left (+ 3) >> writeVar left 0)
  assertEq failed 0 =<< readVarIO left
  writeVarIO left 7
  (before, _, _) <- runFrame ctx inp ((,) <$> item "left" left <*> item "right" right)
  _ <- runFrame ctx inp (item "right" right)
  (after, _, _) <- runFrame ctx inp ((,) <$> item "right" right <*> item "left" left)
  assertEq failed (7, 0) before
  assertEq failed (0, 7) after

data Item = Item {rowKey :: !Int, rowName :: !Text, rowDone :: !Bool}
  deriving (Eq, Show, Generic)

newtype Items = Items {rows :: [Item]}
  deriving (Eq, Show, Generic)

-- | Field focus and element Vars write through to the whole value, an
-- element's Var follows its key when the list is reordered, and a write
-- through a removed element's Var changes nothing.
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
  els <- runNanoUI ctx inp (withElemVars rowKey model.rows (\xs at -> pure (map at xs)))
  case els of
    first : _ -> runNanoUI ctx inp (writeVar first.rowName "THREE")
    [] -> assert failed False
  assertEq failed ["THREE", "two", "one"] . map rowName . rows =<< readVarIO model
  runNanoUI ctx inp (modifyVar model.rows (filter ((/= 3) . rowKey)))
  clearDirty ctx
  case els of
    first : _ -> runNanoUI ctx inp (writeVar first.rowDone True)
    [] -> pure ()
  assertEq failed [False, False] . map rowDone . rows =<< readVarIO model
  -- The write changed nothing, so it wakes nothing.
  assertEq failed False =<< isDirty ctx
  pair <- newVar ((1 :: Int), ("a" :: Text))
  runNanoUI ctx inp (writeVar (zoomVar snd (\b (a, _) -> (a, b)) pair) "b")
  assertEq failed (1, "b") =<< readVarIO pair

-- | A Var map keeps a row's state under its key until the row is dropped.
runVarMapTest :: Context -> IORef Int -> IO ()
runVarMapTest ctx failed = do
  m <- newVarMap
  let inp = withInputOff 300 100
      at k = runNanoUI ctx inp (varAt m (k :: Int) (0 :: Int))
  a <- at 1
  runNanoUI ctx inp (writeVar a 5)
  assertEq failed 5 =<< (readVarIO =<< at 1)
  assertEq failed 0 =<< (readVarIO =<< at 2)
  runNanoUI ctx inp (dropVar m 1)
  assertEq failed 0 =<< (readVarIO =<< at 1)
  runNanoUI ctx inp (writeVar a 6 >> retainVars (/= 2) m)
  assertEq failed 0 =<< (readVarIO =<< at 2)

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
  spans <- collectTextSpans other
  assert failed (any (\(_, t, _, _, _) -> t == "1") spans)

-- | A frame an exception cuts short leaves no rerun behind: the next frame
-- runs its view once, and still repaints the change the failed frame made.
runVarAbortTest :: Context -> IORef Int -> IO ()
runVarAbortTest ctx failed = do
  v <- newVar (0 :: Int)
  let inp = withInputOff 300 100
      view = label . T.pack . show =<< readVar v
      failing :: NanoUI ()
      failing = do
        _ <- readVar v
        writeVar v 1
        liftIO (throwIO (ErrorCall "view failed"))
  _ <- warmup2 ctx inp view
  r <- try @ErrorCall (runFrame ctx inp failing)
  assert failed (either (const True) (const False) r)
  _ <- takeDamage ctx
  _ <- runFrame ctx inp view
  assertEq failed 1 =<< lastFramePasses ctx
  assertEq failed DamageFull =<< takeDamage ctx

-- | A write from another thread that lands in a pass after the pass read
-- the Var is new to the next frame's reads, which repaint whole, also when
-- the pass then writes another part of the same Var.
runVarIoInPassTest :: Context -> IORef Int -> IO ()
runVarIoInPassTest ctx failed = do
  item <- newVar (Item 1 "one" False)
  let inp = withInputOff 300 100
      view = label =<< readVar item.rowName
      -- The thread's write, as if it landed here.
      landing name = liftIO (writeVarIO item.rowName name)
  _ <- warmup2 ctx inp view
  _ <- runFrame ctx inp (view >> landing "uno")
  _ <- takeDamage ctx
  _ <- runFrame ctx inp view
  assertEq failed DamageFull =<< takeDamage ctx
  _ <- runFrame ctx inp (view >> landing "ein" >> writeVar item.rowDone True)
  _ <- takeDamage ctx
  _ <- runFrame ctx inp view
  assertEq failed DamageFull =<< takeDamage ctx
