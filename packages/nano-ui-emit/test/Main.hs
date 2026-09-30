module Main (main) where

import Control.Exception (IOException, try)
import Control.Monad (unless, void, when)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text qualified as T
import NanoUI
import NanoUI.Emit
import NanoUI.Internal.Context (ctxFocusId)
import NanoUI.Testing (Context, collectTextSpans, newContext)
import NanoUI.Testing.Assert (withInput)
import NanoUI.Testing.Harness
  ( centerOf
  , clickPair
  , keyInp
  , spanCenter
  , spanRect
  , withInputOff
  )

check :: String -> Bool -> IO ()
check name ok = unless ok (fail name)

data Msg = Inc | Dec deriving (Eq, Show)

update :: Msg -> Int -> Int
update Inc = (+ 1)
update Dec = subtract 1

-- A polymorphic message works without a Typeable constraint, including when
-- the message itself is a function over an unknown type.
polymorphicFrame :: Context -> msg -> IO [msg]
polymorphicFrame ctx msg = do
  (_, msgs, _, _) <- runFrameE ctx (withInput 80 80) (emit msg)
  pure msgs

main :: IO ()
main = do
  testReduce
  testAdapters
  testClick
  testKeyboard
  testTabs
  testRebuild
  testScopes
  testMapMessages
  testIsolation
  putStrLn "Typed emission tests passed."

testReduce :: IO ()
testReduce = do
  ctx <- newContext
  let
    inp = withInput 80 80
    view _ = withNanoUI column (emit Inc >> emit Dec >> emit Inc)
  ((), model, msgs, _, dirty) <- runFrameReduce update ctx inp 0 view
  check
    "reducer messages and dirty"
    (msgs == [Inc, Dec, Inc] && model == 1 && dirty)
  let
    identity _ = withNanoUI column (emit Inc >> emit Dec)
  ((), model2, msgs2, _, dirty2) <- runFrameReduce update ctx inp 0 identity
  check
    "unchanged model stays clean"
    (msgs2 == [Inc, Dec] && model2 == 0 && not dirty2)
  -- Order must matter, not just the net sum of increments and decrements.
  functions <- polymorphicFrame ctx ((+ 2) :: Int -> Int)
  check "polymorphic function messages" (reduceUpdates 3 functions == 5)
  (_, ordered, _, _, _) <- runFrameReduce ($) ctx inp (1 :: Int) $ \_ -> emit (+ 2) >> emit (* 3)
  check "noncommutative emission order" (ordered == 9)

testAdapters :: IO ()
testAdapters = do
  ctx <- newContext
  calls <- newIORef (0 :: Int)
  let
    control value = liftIO (modifyIORef' calls (+ 1)) >> pure (value + 1)
    adapters = do
      emitWhen (pure False) (1 :: Int)
      emitWhen (pure True) 2
      emitChanged pure 3 id
      emitChanged control 3 id
      emitEdited (\v -> pure (mempty, v + 1)) 5 id
      emitEdited (\v -> pure (mempty {rawRespChanged = True}, v)) 6 id
      emitEdited (\v -> pure (mempty {rawRespChanged = True}, v + 1)) 7 id
  (_, msgs, _, _) <- runFrameE ctx (withInput 80 80) adapters
  check "adapters distinguish edits and changes" (msgs == [2, 4, 8])
  check "control evaluated once" . (== 1) =<< readIORef calls

testClick :: IO ()
testClick = do
  ctx <- newContext
  let
    inp = withInput 240 120
    view m = do
      resp <- liftNanoUI (button' "Go")
      when (respClicked resp) (emit Inc)
      liftNanoUI (label (T.pack (show m)))
      pure resp
  void (runFrameReduce update ctx inp 0 view)
  (resp, model, _, _, _) <- runFrameReduce update ctx inp 0 view
  check "idle model" (model == 0)
  (modelR, msgs, dirty) <- runClickReduce update ctx inp 0 view (centerOf resp)
  check "click reduces once" (msgs == [Inc] && modelR == 1 && dirty)
  (_, model1, idle, _, _) <- runFrameReduce update ctx inp modelR view
  check "idle after click" (model1 == 1 && null idle)
  -- Lifting an ordinary button cannot produce implicit messages.
  let
    buttonUi = liftNanoUI (button' "Go") :: NanoUIE Msg Response
  (target, _, _, _) <- runFrameE ctx inp buttonUi
  let
    (press, release) = clickPair inp (centerOf target)
  void (runFrameE ctx press buttonUi)
  (clicked, noMessages, _, _) <- runFrameE ctx release buttonUi
  check "ordinary widgets emit nothing" (respClicked clicked && null noMessages)

testKeyboard :: IO ()
testKeyboard = do
  ctx <- newContext
  let
    inp = withInputOff 200 100
    ui = do
      wid <- liftNanoUI currentId
      emitChanged (checkbox "Emit") False id
      pure wid
  (wid, _, _, _) <- runFrameE ctx inp ui
  writeIORef (ctxFocusId ctx) wid
  (_, msgs, _, _) <- runFrameE ctx (keyInp KeyEnter inp) ui
  check "keyboard checkbox emission" (msgs == [True])

testTabs :: IO ()
testTabs = do
  ctx <- newContext
  let
    inp = withInput 300 100
    ui =
      emitChanged
        ( \active ->
            tabs
              active
              [tab False "Alpha" (label "Body A"), tab True "Beta" (label "Body B")]
        )
        False
        id
  void (runFrameE ctx inp ui)
  spans <- collectTextSpans ctx
  target <- maybe (fail "missing Beta tab") pure (spanRect "Beta" spans)
  let
    (press, release) = clickPair inp (spanCenter target)
  void (runFrameE ctx press ui)
  (_, msgs, _, _) <- runFrameE ctx release ui
  check "tab emission" (msgs == [True])

testRebuild :: IO ()
testRebuild = do
  ctx <- newContext
  seenRef <- newIORef (0 :: Int)
  let
    inp = withInputOff 300 200
    view _ = withRunInNanoUIE $ \run -> column $ do
      (loaded, setLoaded) <- useFlag False
      (vis, _) <- sensor (label (if loaded then "loaded" else "loading"))
      when (becameVisible vis) $ do
        liftIO (modifyIORef' seenRef (+ 1))
        run (emit (1 :: Int))
        setLoaded True
      pure (loaded, (visVisible vis, visEvent vis))
    frame model =
      (\(r, model', _, _, _) -> (r, model')) <$> runFrameReduce (+) ctx inp model view
  (second, model2) <- frame 0 >>= frame . snd
  model5 <- snd <$> (frame model2 >>= frame . snd >>= frame . snd)
  seen <- readIORef seenRef
  check
    "message survives mirror rebuild exactly once"
    (second == (True, (True, Nothing)) && seen == 1 && model5 == 1)

testScopes :: IO ()
testScopes = do
  ctx <- newContext
  let
    inp = withInput 120 80
    ui = withRunInNanoUIE $ \run -> column $ do
      run (emit (1 :: Int))
      disabledWhen True $ run $ do
        emitWhen (pure True) 2
        emitWhen (button "Disabled") 99
      run (withNanoUI row (emit 3))
  (_, msgs, _, _) <- runFrameE ctx (keyInp KeyEnter inp) ui
  check "nested scopes share the sink" (msgs == [1, 2, 3])
  (((), inner), outer, _, _) <- runFrameE ctx inp $ do
    emit (4 :: Int)
    liftNanoUI (runNanoUIE (emit True))
  check
    "nested collectors have independent message types"
    (inner == [True] && outer == [4])

testMapMessages :: IO ()
testMapMessages = do
  ctx <- newContext
  let child = withRunInNanoUIE $ \run -> column $ do
        run (emit True)
        run (withNanoUI row (emit False))
        pure (42 :: Int)
      ui = do
        emit (Left "before" :: Either String Int)
        result <- mapMessages Right (mapMessages (\b -> if b then 1 else 2) child)
        emit (Left "after")
        pure result
  (result, msgs, _, _) <- runFrameE ctx (withInputOff 100 100) ui
  check "mapped children keep results and message order"
    (result == 42 && msgs == [Left "before", Right 1, Right 2, Left "after"])
  cell <- newState False
  (_, rebuilt, _, _) <- runFrameE ctx (withInputOff 100 100) $
    mapMessages Just $ do
      (done, setDone) <- liftNanoUI (useState cell)
      unless done $ emit True >> liftNanoUI (setDone True)
  check "mapped messages survive a child rebuild once" (rebuilt == [Just True])

testIsolation :: IO ()
testIsolation = do
  ctx <- newContext
  let
    inp = withInput 80 80
  failed <- try @IOException $ void $ runFrameE ctx inp $ do
    emit (1 :: Int)
    liftIO (ioError (userError "failed view"))
  check "view exception propagated" (either (const True) (const False) failed)
  (_, msgs, _, _) <- runFrameE ctx inp (emit (2 :: Int))
  check "failed frame cannot leak messages" (msgs == [2])
  (_, empty, _, _) <- runFrameE ctx inp (pure () :: NanoUIE Int ())
  check "queues are per invocation" (null empty)
