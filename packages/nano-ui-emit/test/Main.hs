module Main (main) where

import Control.Exception (IOException, TypeError (..), evaluate, try)
import Control.Monad (unless, void, when)
import Data.Either (isLeft)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (isInfixOf)
import Data.Text qualified as T
import DeferredWrongMessage (wrongMessage)
import NanoUI qualified as UI
import NanoUI.Emit
import NanoUI.Testing (Context, newContext)
import NanoUI.Testing.Assert (require, requireEq, withInput)
import NanoUI.Testing.Harness (centerOf, keyInp, withInputOff)
import NanoUI.Testing.Sim
  ( Sim
  , SimOptions (..)
  , checkEq
  , click
  , defaultSimOptions
  , focusOn
  , frame
  , pressKey
  , runSimWith
  )
import NanoUI.Testing.Sim qualified as Sim

-- | Run a script against an emitting view in a fresh context, stopping at
-- its first failed check; its result is the view's with the frame's messages.
simE :: Input -> NanoUIE msg a -> Sim (a, [msg]) r -> IO r
simE inp view script = do
  ctx <- newContext
  failed <- newIORef 0
  runSimWith defaultSimOptions {simFailFast = True} ctx failed inp (runNanoUIE view) script

-- | The messages of one frame.
messages :: Context -> Input -> NanoUIE msg a -> IO [msg]
messages ctx inp view = (\(_, msgs, _, _) -> msgs) <$> runFrameE ctx inp view

data Msg = Inc | Dec deriving (Eq, Show)

update :: Msg -> Int -> Int
update Inc = (+ 1)
update Dec = subtract 1

main :: IO ()
main = do
  testReduce
  testAdapters
  testClick
  testWidgetMessages
  testRebuild
  testScopes
  testIsolation
  testWrongMessage
  putStrLn "Typed emission tests passed."

testReduce :: IO ()
testReduce = do
  ctx <- newContext
  let
    inp = withInput 80 80
    reduce :: NanoUIE Msg () -> IO (Int, [Msg], Bool)
    reduce emits = do
      ((), m, msgs, _, dirty) <- runFrameReduce update ctx inp 0 (\_ -> emits)
      pure (m, msgs, dirty)
  require "reducer messages and dirty" . (== (1, [Inc, Dec, Inc], True))
    =<< reduce (emit Inc >> emit Dec >> emit Inc)
  require "unchanged model stays clean" . (== (0, [Inc, Dec], False)) =<< reduce (emit Inc >> emit Dec)
  -- A polymorphic message works without a Typeable constraint, including
  -- when the message itself is a function over an unknown type.
  functions <- messages ctx inp (emit ((+ 2) :: Int -> Int))
  require "polymorphic function messages" (reduceUpdates 3 functions == 5)
  -- Order must matter, not just the net sum of increments and decrements.
  (_, ordered, _, _, _) <- runFrameReduce ($) ctx inp (1 :: Int) $ \_ -> emit (+ 2) >> emit (* 3)
  require "noncommutative emission order" (ordered == 9)

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
  requireEq "adapters distinguish edits and changes" [2, 4, 8] =<< messages ctx (withInput 80 80) adapters
  requireEq "control evaluated once" 1 =<< readIORef calls

-- | Core buttons emit through 'emitWhen' and a primed response, a click
-- reduces once, and an ordinary button emits nothing of its own.
testClick :: IO ()
testClick = do
  let
    inp = withInput 240 160
    view :: Int -> NanoUIE Msg Response
    view m = column $ do
      heading "Counter"
      row $ do
        emitWhen (button "-") Dec
        label (T.pack (show m))
        b <- button' "+"
        when (respClicked b) (emit Inc)
        pure b
  simE inp (view 0) $ do
    (_, idle) <- frame
    checkEq [] idle
    (_, decs) <- click "-"
    checkEq [Dec] decs
  simE inp (UI.button' "Plain" :: NanoUIE Msg Response) $ do
    (plain, none) <- click "Plain"
    checkEq (True, []) (respClicked plain, none)
  ctx <- newContext
  -- Geometry lags a frame: the first frame's response has no rect yet.
  _ <- runFrameReduce update ctx inp 0 view
  (inc, model, _, _, dirty) <- runFrameReduce update ctx inp 0 view
  require "initial view" (model == 0 && not dirty)
  (model1, msgs1, dirty1) <- runClickReduce update ctx inp 0 view (centerOf inc)
  require "click reduces once" (msgs1 == [Inc] && model1 == 1 && dirty1)
  (_, model2, idle, _, _) <- runFrameReduce update ctx inp model1 view
  require "idle after click" (model2 == 1 && null idle)

-- | 'emitChanged' reports a checkbox toggled from the keyboard and a tab
-- chosen with the mouse.
testWidgetMessages :: IO ()
testWidgetMessages = do
  let
    ui :: NanoUIE (Either Bool Bool) WidgetId
    ui = UI.column $ do
      wid <- currentId
      emitChanged (UI.checkbox "Emit") False Left
      emitChanged
        (\active -> UI.tabs active [tab False "Alpha" (UI.label "Body A"), tab True "Beta" (UI.label "Body B")])
        False
        Right
      pure wid
  simE (withInputOff 300 120) ui $ do
    focusOn . fst =<< Sim.result
    (_, keyed) <- pressKey KeyEnter
    checkEq [Left True] keyed
    (_, clicked) <- click "Beta"
    checkEq [Right True] clicked

-- | A message emitted before a write that reruns the view survives the
-- rerun exactly once.
testRebuild :: IO ()
testRebuild = do
  ctx <- newContext
  seenRef <- newIORef (0 :: Int)
  let
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
    step model = do
      (r, model', _, _, _) <- runFrameReduce (+) ctx (withInputOff 300 200) model view
      pure (r, model')
  (second, model2) <- step 0 >>= step . snd
  model5 <- snd <$> (step model2 >>= step . snd >>= step . snd)
  seen <- readIORef seenRef
  requireEq "message survives a rerun exactly once" ((True, (True, Nothing)), 1, 1) (second, seen, model5)

-- | Nested layout and disabled scopes share the sink; nested collectors and
-- 'mapMessages' children keep their own types, results and order.
testScopes :: IO ()
testScopes = do
  ctx <- newContext
  let inp = withInput 120 80
  shared <- messages ctx (keyInp KeyEnter inp) $ UI.column $ do
    emit (1 :: Int)
    UI.disabledWhen True $ do
      emitWhen (pure True) 2
      emitWhen (UI.button "Disabled") 99
    UI.row (emit 3)
  require "nested scopes share the sink" (shared == [1, 2, 3])
  (((), inner), outer, _, _) <- runFrameE ctx inp $ do
    emit (4 :: Int)
    runNanoUIE (emit True)
  require "nested collectors have independent message types" (inner == [True] && outer == [4])
  let
    child :: NanoUIE Bool Int
    child = UI.column $ do
      emit True
      UI.row (emit False)
      pure 42
  (mapped, msgs, _, _) <- runFrameE ctx inp $ do
    emit (Left "before" :: Either String Int)
    r <- mapMessages Right (mapMessages (\b -> if b then 1 else 2) child)
    emit (Left "after")
    pure r
  require "mapped children keep results and message order" $
    mapped == 42 && msgs == [Left "before", Right 1, Right 2, Left "after"]
  cell <- newVar False
  rebuilt <- messages ctx inp $ mapMessages Just $ do
    done <- readVar cell
    unless done $ emit True >> writeVar cell True
  require "mapped messages survive a child rebuild once" (rebuilt == [Just True])

testIsolation :: IO ()
testIsolation = do
  ctx <- newContext
  let inp = withInput 80 80
  failed <- try @IOException $ void $ runFrameE ctx inp $ do
    emit (1 :: Int)
    liftIO (ioError (userError "failed view"))
  require "view exception propagated" (isLeft failed)
  requireEq "failed frame cannot leak messages" [2] =<< messages ctx inp (emit (2 :: Int))
  require "queues are per invocation" . null =<< messages ctx inp (pure () :: NanoUIE Int ())

-- | The message type is part of the view: emitting a Bool from an Int view
-- does not typecheck, so its deferred type error fires.
testWrongMessage :: IO ()
testWrongMessage = do
  ctx <- newContext
  result <- try (evaluate . sum =<< messages ctx (withInput 80 80) wrongMessage)
  case result of
    Left (TypeError err)
      | "Int" `isInfixOf` err && "Bool" `isInfixOf` err -> pure ()
      | otherwise -> fail ("the type error was not the Int/Bool mismatch: " <> err)
    Right _ -> fail "a view emitted a message of another type"
