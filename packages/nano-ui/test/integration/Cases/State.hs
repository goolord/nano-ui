{-# OPTIONS_GHC -O0 #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Cases.State (tests) where

import Spec
import Control.Exception (ErrorCall (..), throwIO, try)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Sequence qualified as Seq
import Data.Primitive.SmallArray qualified as SA
import GHC.Generics (Generic)
import NanoUI.Internal.Context (lookupImageUv)
import NanoUI.Shortcut qualified as Shortcut

tests :: [Spec]
tests =
  [ spec "controlled-inputs" runControlledInputsTest
  , spec "var-slots" runVarSlotTest
  , spec "var-lifecycle" runVarLifecycleTest
  , spec "var-focus" runVarFocusTest
  , spec "var-shared" runVarSharedTest
  , spec "var-late-writes" runVarLateWritesTest
  , spec "collection-api" runCollectionApiTest
  ]

-- | A stack runs its children in order, empty option lists leave the index
-- alone, and a failed image does not stop the rest of a batch.
runCollectionApiTest :: Context -> IORef Int -> IO ()
runCollectionApiTest ctx failed = do
  seen <- newIORef []
  _ <- runFrame ctx (withInputOff 300 100) $
    hstack (SA.smallArrayFromList [liftIO (modifyIORef' seen (key :)) | key <- [7, 2, 9 :: Int]])
  assertEq failed [9, 2, 7] =<< readIORef seen
  picked <- evalUi ctx (withInputOff 300 200) . withKey ("collection-options" :: Text) . column $
    (,,)
      <$> (snd <$> withValue 5 (select (SA.emptySmallArray :: SA.SmallArray Text)))
      <*> (snd <$> withValue (-1) (radio (Seq.empty :: Seq.Seq Text)))
      <*> (snd <$> withValue "Beta" (comboBox "Choose" (Seq.fromList ["Alpha", "Beta"])))
  assertEq failed (5, -1, "Beta") picked
  assertEq failed False =<< registerImages ctx (Seq.fromList [(ImageId 0, 1, 1, BS.replicate 4 255), (ImageId 42, 1, 1, BS.replicate 4 255)])
  assert failed . (/= Nothing) =<< lookupImageUv ctx (ImageId 42)

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
      at :: Text -> NanoUI a -> IO a
      at k = runNanoUI ctx inp . withKey k
      check :: (Eq a, Show a) => Text -> NanoUI (Var a) -> a -> a -> IO ()
      check k slot initial changed = do
        let evaluate = at k (slot >>= \v -> (v,) <$> readVar v)
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
  let asInt = at "slot" (readVar =<< useVar (5 :: Int))
  assertEq failed 5 =<< asInt
  at "slot" (useVar (5 :: Int) >>= \v -> writeVar v 9)
  assertEq failed 9 =<< asInt
  assertEq failed "five" =<< at "slot" (readVar =<< useVar ("five" :: Text))
  assertEq failed 5 =<< asInt
  at "enum" (useEnumVar LT >>= \v -> writeVar v EQ)
  assertEq failed False =<< at "enum" (readVar =<< useEnumVar False)
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

-- | A key press runs once even when its write reruns the view, which then
-- repaints whole and leaves nothing dirty. Owned Vars survive hiding and
-- keyed reordering, and independent ones do not share state.
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
  assertEq failed 2 =<< lastFramePasses ctx
  assertEq failed DamageFull =<< takeDamage ctx
  assert failed . any (\(_, text, _, _, _) -> text == "2") =<< collectTextSpans ctx
  (_, _, dirty) <- runFrame ctx inp view
  assertEq failed False dirty
  assert failed . damageIsEmpty =<< takeDamage ctx
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
  first : _ <- runNanoUI ctx inp (withElemVars rowKey model.rows (\xs at -> pure (map at xs)))
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
