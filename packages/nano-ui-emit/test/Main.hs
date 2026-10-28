module Main (main) where

import Control.Exception (IOException, TypeError (..), evaluate, try)
import Control.Monad (unless, void, when)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (isInfixOf)
import Data.Text qualified as T
import DeferredWrongMessage (wrongMessage)
import NanoUI qualified as UI
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
  testWrongMessage
  testWrappedWidgets
  putStrLn "Typed emission tests passed."

testReduce :: IO ()
testReduce = do
  ctx <- newContext
  let
    inp = withInput 80 80
    view :: Int -> NanoUIE Msg ()
    view _ = UI.column (emit Inc >> emit Dec >> emit Inc)
  ((), model, msgs, _, dirty) <- runFrameReduce update ctx inp 0 view
  check
    "reducer messages and dirty"
    (msgs == [Inc, Dec, Inc] && model == 1 && dirty)
  let
    identity :: Int -> NanoUIE Msg ()
    identity _ = UI.column (emit Inc >> emit Dec)
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
    control v = liftIO (modifyIORef' calls (+ 1)) >> UI.modifyVar v (+ 1)
    adapters :: NanoUIE Int ()
    adapters = do
      emitWhen (pure False) (1 :: Int)
      emitWhen (pure True) 2
      emitChanged (\_ -> pure ()) 3 id
      emitChanged control 3 id
      emitEdited (\v -> (mempty :: Response) <$ UI.modifyVar v (+ 1)) 5 id
      emitEdited (\_ -> pure mempty {rawRespChanged = True}) 6 id
      emitEdited (\v -> mempty {rawRespChanged = True} <$ UI.modifyVar v (+ 1)) 7 id
  (_, msgs, _, _) <- runFrameE ctx (withInput 80 80) adapters
  check "adapters distinguish edits and changes" (msgs == [2, 4, 8])
  check "control evaluated once" . (== 1) =<< readIORef calls

testClick :: IO ()
testClick = do
  ctx <- newContext
  let
    inp = withInput 240 120
    view :: Int -> NanoUIE Msg Response
    view m = do
      resp <- button' "Go"
      when (respClicked resp) (emit Inc)
      label (T.pack (show m))
      pure resp
  void (runFrameReduce update ctx inp 0 view)
  (resp, model, _, _, _) <- runFrameReduce update ctx inp 0 view
  check "idle model" (model == 0)
  (modelR, msgs, dirty) <- runClickReduce update ctx inp 0 view (centerOf resp)
  check "click reduces once" (msgs == [Inc] && modelR == 1 && dirty)
  (_, model1, idle, _, _) <- runFrameReduce update ctx inp modelR view
  check "idle after click" (model1 == 1 && null idle)
  -- An ordinary button in an emitting view produces no messages of its own.
  let
    buttonUi :: NanoUIE Msg Response
    buttonUi = UI.button' "Go"
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
    ui :: NanoUIE Bool WidgetId
    ui = do
      wid <- currentId
      emitChanged (UI.checkbox "Emit") False id
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
    ui :: NanoUIE Bool ()
    ui =
      emitChanged
        ( \active ->
            UI.tabs
              active
              [tab False "Alpha" (UI.label "Body A"), tab True "Beta" (UI.label "Body B")]
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
    view :: Int -> NanoUIE Int (Bool, (Bool, Maybe VisibilityEvent))
    view _ = UI.column $ do
      loadedVar <- UI.useVar False
      loaded <- UI.readVar loadedVar
      (vis, _) <- UI.sensor (UI.label (if loaded then "loaded" else "loading"))
      when (becameVisible vis) $ do
        liftIO (modifyIORef' seenRef (+ 1))
        emit 1
        UI.writeVar loadedVar True
      pure (loaded, (visVisible vis, visEvent vis))
    frame model =
      (\(r, model', _, _, _) -> (r, model')) <$> runFrameReduce (+) ctx inp model view
  (second, model2) <- frame 0 >>= frame . snd
  model5 <- snd <$> (frame model2 >>= frame . snd >>= frame . snd)
  seen <- readIORef seenRef
  check
    "message survives a rerun exactly once"
    (second == (True, (True, Nothing)) && seen == 1 && model5 == 1)

testScopes :: IO ()
testScopes = do
  ctx <- newContext
  let
    inp = withInput 120 80
    ui :: NanoUIE Int ()
    ui = UI.column $ do
      emit 1
      UI.disabledWhen True $ do
        emitWhen (pure True) 2
        emitWhen (UI.button "Disabled") 99
      UI.row (emit 3)
  (_, msgs, _, _) <- runFrameE ctx (keyInp KeyEnter inp) ui
  check "nested scopes share the sink" (msgs == [1, 2, 3])
  (((), inner), outer, _, _) <- runFrameE ctx inp $ do
    emit (4 :: Int)
    runNanoUIE (emit True)
  check
    "nested collectors have independent message types"
    (inner == [True] && outer == [4])

testMapMessages :: IO ()
testMapMessages = do
  ctx <- newContext
  let child :: NanoUIE Bool Int
      child = UI.column $ do
        emit True
        UI.row (emit False)
        pure 42
      ui :: NanoUIE (Either String Int) Int
      ui = do
        emit (Left "before" :: Either String Int)
        result <- mapMessages Right (mapMessages (\b -> if b then 1 else 2) child)
        emit (Left "after")
        pure result
  (result, msgs, _, _) <- runFrameE ctx (withInputOff 100 100) ui
  check "mapped children keep results and message order"
    (result == 42 && msgs == [Left "before", Right 1, Right 2, Left "after"])
  cell <- newVar False
  (_, rebuilt, _, _) <- runFrameE ctx (withInputOff 100 100) $
    mapMessages Just $ do
      done <- readVar cell
      unless done $ emit True >> writeVar cell True
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

-- | The message type is part of the view: emitting a Bool from an Int view
-- does not typecheck, so its deferred type error fires.
testWrongMessage :: IO ()
testWrongMessage = do
  ctx <- newContext
  result <- try $ do
    (_, msgs, _, _) <- runFrameE ctx (withInput 80 80) wrongMessage
    evaluate (sum msgs)
  case result of
    Left (TypeError err)
      | "Int" `isInfixOf` err && "Bool" `isInfixOf` err -> pure ()
      | otherwise -> fail ("the type error was not the Int/Bool mismatch: " <> err)
    Right _ -> fail "a view emitted a message of another type"

testWrappedWidgets :: IO ()
testWrappedWidgets = do
  ctx <- newContext
  let
    inp = withInput 240 120
    view :: Int -> NanoUIE Msg Response
    view m = column $ do
      heading "Counter"
      row $ do
        emitWhen (button "-") Dec
        label (T.pack (show m))
        b <- button' "+"
        when (respClicked b) (emit Inc)
        pure b
  -- Geometry lags a frame: the first frame's response has no rect yet, so
  -- the click targets the second's.
  _ <- runFrameReduce update ctx inp 0 view
  (resp, model, msgs, _, dirty) <- runFrameReduce update ctx inp 0 view
  check "initial view" (msgs == [] && model == 0 && not dirty)
  (model1, msgs1, dirty1) <- runClickReduce update ctx inp 0 view (centerOf resp)
  check "core button emits and reduces" (msgs1 == [Inc] && model1 == 1 && dirty1)
