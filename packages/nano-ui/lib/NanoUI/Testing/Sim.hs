{-# LANGUAGE DerivingVia #-}

-- | A simulated user driving a view headlessly, in the style of iced's
-- simulator: find a widget by its text or rect, click it, type, press keys,
-- and check what the next frame shows.
--
-- > runSim ctx failed (withInput 300 200) counterView $ do
-- >   click "+"
-- >   click "+"
-- >   expect "2"
--
-- 'runSim' settles the view first (frames until it asks for no more), so
-- 'respRect's and spans are current. Each action then runs the frames a real
-- event would: 'click' is a press frame and a release frame, 'typeText' one
-- frame. The pointer stays where the last action left it, as a mouse does.
-- One-shot events (presses, keys, text, wheel) last a single frame; held
-- buttons, modifiers and the window size persist.
--
-- Checks ('check', 'checkEq', 'expect') count a failure on the shared
-- counter and carry on. Aiming at a 'Target' that is not on screen throws,
-- which the test runner reports as a failure of the whole test.
module NanoUI.Testing.Sim
  ( -- * Running
    Sim
  , runSim
  , simContext
  , simFailures
    -- * Frames
  , frame
  , frameWith
  , settle
  , result
  , wantsFrame
  , modifyInput
  , getInput
  , resize
  , advance
    -- * Pointer
  , Target (..)
  , targetPoint
  , moveTo
  , press
  , pressButton
  , release
  , releaseButton
  , click
  , clickWith
  , rightClick
  , doubleClick
  , drag
  , wheel
    -- * Keyboard
  , typeText
  , pressKey
  , chord
  , pressTab
    -- * Reading the frame
  , spans
  , findSpan
  , rectOf
  , expect
  , expectNot
    -- * Checks
  , check
  , checkEq
  , checkGt
  , checkLt
  , checkJust
  ) where

import Control.Exception (throwIO)
import Control.Monad (replicateM_, unless, void)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (listToMaybe)
import Data.String (IsString (..))
import Data.Text qualified as T
import GHC.Stack (HasCallStack, withFrozenCallStack)
import NanoUI hiding (liftIO)
import NanoUI.Backend
import NanoUI.Shortcut (Shortcut)
import NanoUI.Testing (getPrevRect)
import NanoUI.Testing.Assert qualified as A
import NanoUI.Testing.Harness (chordInp, keyInp, pressWith, releaseWith, spanLabel)

data Env a = Env
  { envCtx :: Context
  , envFailed :: IORef Int
  , envView :: NanoUI a
  , envInput :: IORef Input
  , envResult :: IORef (a, Bool)
  }

-- | A scripted session against one view. @a@ is the view's result.
newtype Sim a r = Sim (Env a -> IO r)
  deriving (Functor, Applicative, Monad, MonadFail, MonadIO) via ReaderT (Env a) IO

-- | Settle the view on @input@ (its window size, usually from 'A.withInput'),
-- then run the script. Failures go to the shared counter.
runSim :: Context -> IORef Int -> Input -> NanoUI a -> Sim a r -> IO r
runSim ctx failed inp view (Sim script) = do
  (a, _, again) <- runFrame ctx inp view
  env <- Env ctx failed view <$> newIORef (clearEphemeral inp) <*> newIORef (a, again)
  let Sim settled = settle
  _ <- settled env
  script env

-- | The context the script drives.
simContext :: Sim a Context
simContext = Sim (pure . envCtx)

-- | The shared failure counter, for the assertions of "NanoUI.Testing.Assert".
simFailures :: Sim a (IORef Int)
simFailures = Sim (pure . envFailed)

-- | Run one frame on the current input and return the view's result. The
-- frame's one-shot events are cleared afterwards.
frame :: Sim a a
frame = frameWith id

-- | Run one frame with @f@ applied to the current input for that frame only,
-- then keep its held state (buttons, keys, pointer) and drop its one-shot
-- events.
frameWith :: (Input -> Input) -> Sim a a
frameWith f = Sim $ \env -> do
  inp <- f <$> readIORef (envInput env)
  (a, _, again) <- runFrame (envCtx env) inp (envView env)
  writeIORef (envInput env) (clearEphemeral inp)
  writeIORef (envResult env) (a, again)
  pure a

-- | Run frames until the view stops asking for another (at least two, so
-- geometry from the first is used by the second; at most twenty, for a view
-- that animates). Check 'wantsFrame' afterwards to tell the two apart.
settle :: Sim a a
settle = go (0 :: Int)
  where
    go n = do
      a <- frame
      again <- wantsFrame
      if n < 1 || (again && n < 19) then go (n + 1) else pure a

-- | The result of the last frame.
result :: Sim a a
result = Sim (fmap fst . readIORef . envResult)

-- | Whether the last frame asked for another (a redraw, animation, or a
-- change it made that the view must see).
wantsFrame :: Sim a Bool
wantsFrame = Sim (fmap snd . readIORef . envResult)

-- | Change the input that every later frame starts from.
modifyInput :: (Input -> Input) -> Sim a ()
modifyInput f = Sim (\env -> modifyIORef' (envInput env) f)

-- | The input the next frame starts from.
getInput :: Sim a Input
getInput = Sim (readIORef . envInput)

-- | Resize the window and settle.
resize :: Float -> Float -> Sim a a
resize w h = modifyInput (\i -> i {inputWindowSize = Size w h}) >> settle

-- | One frame in which @dt@ seconds pass, as for animations and delays.
advance :: Float -> Sim a a
advance dt = frameWith (\i -> i {inputDeltaTime = dt})

-- | Where the pointer is aimed. A string literal is a 'Labelled' target, so
-- @click "Save"@ clicks the text "Save".
data Target
  = -- | This point.
    At V2
  | -- | The rect's centre.
    Within Rect
  | -- | The centre of the widget's rect from the last frame.
    On Response
  | -- | The centre of that widget's rect from the last frame.
    OnWidget WidgetId
  | -- | The centre of the visible text span labelled exactly this, ignoring
    -- surrounding space (popups and menus included; see 'findSpan').
    Labelled T.Text

instance IsString Target where
  fromString = Labelled . T.pack

-- | The point a target aims at, throwing when it is not on screen.
targetPoint :: HasCallStack => Target -> Sim a V2
targetPoint = \case
  At p -> pure p
  Within (Rect x y w h) -> pure (V2 (x + w / 2) (y + h / 2))
  On resp -> targetPoint (Within (respRect resp))
  OnWidget wid -> do
    ctx <- simContext
    liftIO (getPrevRect ctx wid) >>= maybe (missing ("widget " <> show wid)) (targetPoint . Within)
  Labelled txt -> rectOf txt >>= targetPoint . Within

missing :: String -> Sim a b
missing what = liftIO (throwIO (userError ("Sim: no " <> what <> " on screen")))

-- | Move the pointer to the target for a frame.
moveTo :: HasCallStack => Target -> Sim a a
moveTo t = do
  p <- targetPoint t
  frameWith (\i -> i {inputMousePos = p})

-- | Press the left button on the target and hold it.
press :: HasCallStack => Target -> Sim a a
press = pressButton MouseLeft

-- | Press @b@ on the target and hold it.
pressButton :: HasCallStack => MouseButton -> Target -> Sim a a
pressButton b t = do
  p <- targetPoint t
  frameWith (\i -> pressWith b i p)

-- | Release the left button where the pointer is.
release :: Sim a a
release = releaseButton MouseLeft

-- | Release @b@ where the pointer is.
releaseButton :: MouseButton -> Sim a a
releaseButton b = frameWith (releaseWith b)

-- | Press and release the left button on the target; the release frame's
-- result.
click :: HasCallStack => Target -> Sim a a
click = clickWith MouseLeft

-- | Press and release @b@ on the target.
clickWith :: HasCallStack => MouseButton -> Target -> Sim a a
clickWith b t = pressButton b t >> releaseButton b

-- | 'clickWith' 'MouseRight', as opens a context menu.
rightClick :: HasCallStack => Target -> Sim a a
rightClick = clickWith MouseRight

-- | Two quick clicks on the target; the second press reports a double click.
doubleClick :: HasCallStack => Target -> Sim a a
doubleClick t = do
  p <- targetPoint t
  void (click (At p))
  void (frameWith (\i -> (pressWith MouseLeft i p) {inputMouseClicks = 2}))
  release

-- | Press on @from@, move to @to@ in @steps@ held frames, and release there.
drag :: HasCallStack => Target -> Target -> Int -> Sim a a
drag from to steps = do
  V2 x0 y0 <- targetPoint from
  V2 x1 y1 <- targetPoint to
  void (press (At (V2 x0 y0)))
  let n = max 1 steps
      at k = let s = fromIntegral k / fromIntegral n in V2 (x0 + (x1 - x0) * s) (y0 + (y1 - y0) * s)
  mapM_ (\k -> frameWith (\i -> i {inputMousePos = at k})) [1 .. n]
  release

-- | Hover the target for a frame, then turn the wheel there by @d@
-- (positive @y@ scrolls down).
wheel :: HasCallStack => Target -> V2 -> Sim a a
wheel t d = moveTo t >> frameWith (\i -> i {inputScroll = d})

-- | One frame that types @txt@ into whatever has focus.
typeText :: T.Text -> Sim a a
typeText txt = frameWith (\i -> i {inputChars = txt})

-- | One frame pressing @k@ (not an auto-repeat).
pressKey :: Key -> Sim a a
pressKey k = frameWith (keyInp k)

-- | One frame pressing a chord such as @cmdOrCtrl <> key 'z'@, with exactly
-- its modifiers held for that frame.
chord :: Shortcut -> Sim a a
chord s = do
  mods <- inputModifiers <$> getInput
  a <- frameWith (chordInp s)
  modifyInput (\i -> i {inputModifiers = mods})
  pure a

-- | Press Tab @n@ times, a frame each, moving focus forward.
pressTab :: Int -> Sim a ()
pressTab n = replicateM_ n (void (pressKey KeyTab))

-- | The text spans of the last frame: the base layer, then overlays.
spans :: Sim a [(Rect, T.Text, Color, Color, Rect)]
spans = Sim $ \env -> do
  inp <- readIORef (envInput env)
  (base, over) <- collectRasterSpans (envCtx env) inp
  pure (base <> over)

-- | The rect of the visible span labelled exactly @txt@ (ignoring
-- surrounding space and blank-glyph markers). When several match, the
-- topmost layer's last one, as the pointer would hit it.
findSpan :: T.Text -> Sim a (Maybe Rect)
findSpan txt = do
  ss <- spans
  pure (listToMaybe (reverse [r | (r@(Rect _ _ w h), t, _, _, _) <- ss, w > 1, h > 1, spanLabel t == txt]))

-- | 'findSpan', throwing when nothing matches.
rectOf :: T.Text -> Sim a Rect
rectOf txt = findSpan txt >>= maybe (missing ("text " <> show txt)) pure

-- | Check that some span of the last frame contains @txt@.
expect :: HasCallStack => T.Text -> Sim a ()
expect txt = withFrozenCallStack (checkSpans ("expected text " <> show txt) (any (T.isInfixOf txt)))

-- | Check that no span of the last frame contains @txt@.
expectNot :: HasCallStack => T.Text -> Sim a ()
expectNot txt = withFrozenCallStack (checkSpans ("unexpected text " <> show txt) (not . any (T.isInfixOf txt)))

checkSpans :: HasCallStack => String -> ([T.Text] -> Bool) -> Sim a ()
checkSpans what ok = do
  ss <- spans
  unless (ok [t | (_, t, _, _, _) <- ss]) $ do
    liftIO (putStrLn what)
    withFrozenCallStack (check False)

-- | Count a failure unless the condition holds.
check :: HasCallStack => Bool -> Sim a ()
check ok = Sim (\env -> withFrozenCallStack (A.assert (envFailed env) ok))

-- | Count a failure unless the values are equal, printing both.
checkEq :: (HasCallStack, Eq b, Show b) => b -> b -> Sim a ()
checkEq x y = Sim (\env -> withFrozenCallStack (A.assertEq (envFailed env) x y))

-- | Count a failure unless the first value is greater.
checkGt :: (HasCallStack, Ord b, Show b) => b -> b -> Sim a ()
checkGt x y = Sim (\env -> withFrozenCallStack (A.assertGt (envFailed env) x y))

-- | Count a failure unless the first value is less.
checkLt :: (HasCallStack, Ord b, Show b) => b -> b -> Sim a ()
checkLt x y = Sim (\env -> withFrozenCallStack (A.assertLt (envFailed env) x y))

-- | Continue with the value, or count a failure on 'Nothing'.
checkJust :: HasCallStack => Maybe b -> (b -> Sim a ()) -> Sim a ()
checkJust m k = maybe (withFrozenCallStack (check False)) k m
