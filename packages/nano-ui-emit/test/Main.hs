module Main (main) where

import Control.Exception (TypeError (..), evaluate, try)
import Control.Monad (unless, when)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (isInfixOf)
import Data.Text qualified as T
import DeferredWrongMessage (wrongMessage)
import NanoUI qualified as UI
import NanoUI.Emit
import NanoUI.Testing (Context, newContext)
import NanoUI.Testing.Assert (require, requireEq, withInput)
import NanoUI.Testing.Harness (centerOf)
import NanoUI.Testing.Sim hiding (result)

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
  testScopes
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
  -- A message can be a function over a type the view does not know, and
  -- functions apply in emission order, not just as a net sum.
  functions <- messages ctx inp (emit ((+ 2) :: Int -> Int) >> emit (* 3))
  require "function messages in emission order" (reduceUpdates 1 functions == 9)

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

-- | Core buttons emit through 'emitWhen' and a primed response, and a click
-- reduces once.
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
  ctx <- newContext
  -- Geometry lags a frame: the first frame's response has no rect yet.
  _ <- runFrameReduce update ctx inp 0 view
  (inc, model, _, _, dirty) <- runFrameReduce update ctx inp 0 view
  require "initial view" (model == 0 && not dirty)
  (model1, msgs1, dirty1) <- runClickReduce update ctx inp 0 view (centerOf inc)
  require "click reduces once" (msgs1 == [Inc] && model1 == 1 && dirty1)
  (_, model2, idle, _, _) <- runFrameReduce update ctx inp model1 view
  require "idle after click" (model2 == 1 && null idle)

-- | 'mapMessages' children keep their results and message order, and their
-- messages survive a rebuild once.
testScopes :: IO ()
testScopes = do
  ctx <- newContext
  let inp = withInput 120 80
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
