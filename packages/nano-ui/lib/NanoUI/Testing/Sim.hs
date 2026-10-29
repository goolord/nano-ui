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
-- counter and carry on ('runSimWith' can make them stop the script instead).
-- Aiming at a 'Target' that is not on screen throws, which the test runner
-- reports as a failure of the whole test.
module NanoUI.Testing.Sim
  ( -- * Running
    Sim
  , runSim
  , runSimWith
  , SimOptions (..)
  , defaultSimOptions
  , simContext
  , onContext
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
  , afterView
    -- * Pointer
  , Target (..)
  , at
  , targetPoint
  , prevRect
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
  , cursorAt
  , pointerCursor
  , hotId
    -- * Keyboard
  , typeText
  , pressKey
  , chord
  , pressTab
  , focusedId
  , focusOn
  , focusRing
    -- * Reading the frame
  , spans
  , findSpan
  , rectOf
  , labelShown
  , textShown
  , expect
  , expectNot
  , frameDamage
  , wouldRedraw
  , wakeTime
  , scrollOffsetOf
  , scrollOffset2DOf
    -- * State
  , readV
  , writeV
    -- * Checks
  , check
  , checkEq
  , checkGt
  , checkLt
  , checkJust
  ) where

import Control.Exception (finally, throwIO)
import Control.Monad (replicateM_, unless, void, when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (isJust, listToMaybe)
import Data.String (IsString (..))
import Data.Text qualified as T
import GHC.Stack (HasCallStack, withFrozenCallStack)
import NanoUI hiding (liftIO)
import NanoUI.Backend
import NanoUI.Shortcut (Shortcut)
import NanoUI.Internal.Context (getFocusVisible)
import NanoUI.Testing (getHotId, getPrevRect, needsRedraw)
import NanoUI.Testing.Assert qualified as A
import NanoUI.Testing.Harness (chordInp, keyInp, pressWith, releaseWith, setFocus, spanLabel)

data Env a = Env
  { envCtx :: Context
  , envFailed :: IORef Int
  , envOptions :: SimOptions a
  , envView :: NanoUI a
  , envAfter :: IORef (Maybe (NanoUI ()))
  , envInput :: IORef Input
  , envResult :: IORef (a, Bool)
  }

-- | A scripted session against one view. @a@ is the view's result.
newtype Sim a r = Sim (Env a -> IO r)
  deriving (Functor, Applicative, Monad, MonadFail, MonadIO) via ReaderT (Env a) IO

-- | How 'runSimWith' runs frames and treats a failed check.
data SimOptions a = SimOptions
  { simFrameRunner :: Context -> Input -> NanoUI a -> IO (a, Bool)
  -- ^ Run one frame of the view on this input: the view's result, and
  -- whether the frame asks for another. A backend's own frame function
  -- drives a real window this way.
  , simFailFast :: Bool
  -- ^ Throw at the first failed check, after printing it, instead of
  -- counting it and carrying on.
  }

-- | Headless frames ('runFrame'), and failed checks counted.
defaultSimOptions :: SimOptions a
defaultSimOptions = SimOptions {simFrameRunner = headlessFrame, simFailFast = False}
  where
    headlessFrame ctx inp view = (\(a, _, again) -> (a, again)) <$> runFrame ctx inp view

-- | Settle the view on @input@ (its window size, usually from 'A.withInput'),
-- then run the script. Failures go to the shared counter.
runSim :: Context -> IORef Int -> Input -> NanoUI a -> Sim a r -> IO r
runSim = runSimWith defaultSimOptions

-- | 'runSim' with other frames or fail-fast checks.
runSimWith :: SimOptions a -> Context -> IORef Int -> Input -> NanoUI a -> Sim a r -> IO r
runSimWith opts ctx failed inp view (Sim script) = do
  first <- simFrameRunner opts ctx inp view
  after <- newIORef Nothing
  env <- Env ctx failed opts view after <$> newIORef (clearEphemeral inp) <*> newIORef first
  let Sim settled = settle
  _ <- settled env
  script env

-- | The context the script drives.
simContext :: Sim a Context
simContext = Sim (pure . envCtx)

-- | Run an action on the context, such as a read the script has no helper
-- for.
onContext :: (Context -> IO b) -> Sim a b
onContext f = Sim (f . envCtx)

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
  view <- maybe (envView env) (envView env <*) <$> readIORef (envAfter env)
  (a, again) <- simFrameRunner (envOptions env) (envCtx env) inp view
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

-- | One frame whose view runs @act@ after the script's view, and what @act@
-- returned. Running last, it takes no widget id from the view, so it can
-- aim commands such as 'requestFocus', 'focusNext' or 'runTextCommand' at
-- widgets the view declared.
afterView :: NanoUI b -> Sim a b
afterView act = Sim $ \env -> do
  out <- newIORef Nothing
  writeIORef (envAfter env) (Just (act >>= liftIO . writeIORef out . Just))
  let Sim run = frame
  _ <- run env `finally` writeIORef (envAfter env) Nothing
  readIORef out >>= maybe (throwIO (userError "Sim: the view never reached afterView's action")) pure

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

-- | @'At' ('V2' x y)@.
at :: Float -> Float -> Target
at x y = At (V2 x y)

-- | The point a target aims at, throwing when it is not on screen.
targetPoint :: HasCallStack => Target -> Sim a V2
targetPoint = \case
  At p -> pure p
  Within (Rect x y w h) -> pure (V2 (x + w / 2) (y + h / 2))
  On resp -> targetPoint (Within (respRect resp))
  OnWidget wid -> prevRect wid >>= targetPoint . Within
  Labelled txt -> rectOf txt >>= targetPoint . Within

-- | The widget's rect from the last frame, throwing when it has none.
prevRect :: WidgetId -> Sim a Rect
prevRect wid = onContext (`getPrevRect` wid) >>= maybe (missing ("widget " <> show wid)) pure

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
      step k = let s = fromIntegral k / fromIntegral n in V2 (x0 + (x1 - x0) * s) (y0 + (y1 - y0) * s)
  mapM_ (\k -> frameWith (\i -> i {inputMousePos = step k})) [1 .. n]
  release

-- | Hover the target for a frame, then turn the wheel there by @d@
-- (positive @y@ scrolls down).
wheel :: HasCallStack => Target -> V2 -> Sim a a
wheel t d = moveTo t >> frameWith (\i -> i {inputScroll = d})

-- | The pointer cursor after a frame with the pointer on the target.
cursorAt :: HasCallStack => Target -> Sim a UiCursorKind
cursorAt t = moveTo t >> pointerCursor

-- | The pointer cursor the last frame's layout gives the current input.
pointerCursor :: Sim a UiCursorKind
pointerCursor = getInput >>= onContext . flip uiCursorKind

-- | The widget the pointer is over, which a press would go to ('WidgetId'
-- 0 for none). It lags a pointer move by a frame.
hotId :: Sim a WidgetId
hotId = onContext getHotId

-- | One frame that types @txt@ into whatever has focus.
typeText :: T.Text -> Sim a a
typeText txt = frameWith (\i -> i {inputChars = txt})

-- | One frame pressing @k@ (not an auto-repeat).
pressKey :: Key -> Sim a a
pressKey k = frameWith (keyInp k)

-- | One frame pressing a chord such as @cmdOrCtrl <> key 'z'@, with exactly
-- its modifiers held for that frame. The key and modifiers are let go
-- afterwards, so the same chord again is a new press, not an auto-repeat.
chord :: Shortcut -> Sim a a
chord s = do
  before <- getInput
  a <- frameWith (chordInp s)
  modifyInput (\i -> i {inputModifiers = inputModifiers before, inputKeysHeld = inputKeysHeld before})
  pure a

-- | Press Tab @n@ times, a frame each, moving focus forward.
pressTab :: Int -> Sim a ()
pressTab n = replicateM_ n (void (pressKey KeyTab))

-- | The widget with keyboard focus.
focusedId :: Sim a WidgetId
focusedId = onContext getFocusId

-- | Give @wid@ keyboard focus directly, as focus kept from earlier frames
-- does; the next frame sees it.
focusOn :: WidgetId -> Sim a ()
focusOn wid = onContext (`setFocus` wid)

-- | Whether the focus ring shows, as it does after keyboard navigation and
-- not after a click.
focusRing :: Sim a Bool
focusRing = onContext getFocusVisible

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

-- | Whether the last frame shows a span labelled exactly @txt@, as
-- 'findSpan' matches.
labelShown :: T.Text -> Sim a Bool
labelShown txt = isJust <$> findSpan txt

-- | Whether some span of the last frame contains @txt@.
textShown :: T.Text -> Sim a Bool
textShown txt = any (\(_, t, _, _, _) -> T.isInfixOf txt t) <$> spans

-- | Check that some span of the last frame contains @txt@.
expect :: HasCallStack => T.Text -> Sim a ()
expect txt = withFrozenCallStack (checkShown ("expected text " <> show txt) =<< textShown txt)

-- | Check that no span of the last frame contains @txt@.
expectNot :: HasCallStack => T.Text -> Sim a ()
expectNot txt = withFrozenCallStack (checkShown ("unexpected text " <> show txt) . not =<< textShown txt)

checkShown :: HasCallStack => String -> Bool -> Sim a ()
checkShown what ok = unless ok $ do
  liftIO (putStrLn what)
  withFrozenCallStack (check False)

-- | The damage of the frames since the last call, which this takes, as a
-- backend does before it paints ('takeDamage').
frameDamage :: Sim a Damage
frameDamage = onContext takeDamage

-- | Whether a frame on the current input changed by @f@ would need drawing,
-- as a backend's loop asks before it runs one ('needsRedraw'). Runs no
-- frame.
wouldRedraw :: (Input -> Input) -> Sim a Bool
wouldRedraw f = getInput >>= \i -> onContext (\ctx -> needsRedraw ctx i (f i))

-- | When the view next wants waking, in monotonic seconds, or 0 for no
-- timed wake ('getWakeAt').
wakeTime :: Sim a Double
wakeTime = onContext getWakeAt

-- | The vertical scroll offset of the scroller @wid@.
scrollOffsetOf :: WidgetId -> Sim a Float
scrollOffsetOf wid = onContext (`getScrollOffset` wid)

-- | Both scroll offsets of the scroller @wid@.
scrollOffset2DOf :: WidgetId -> Sim a V2
scrollOffset2DOf wid = onContext (`getScrollOffset2D` wid)

-- | The value of a 'Var', as the next frame will see it.
readV :: Var v -> Sim a v
readV = liftIO . readVarIO

-- | Write a 'Var' from outside the view, as a worker thread would; the next
-- frame sees the value.
writeV :: Eq v => Var v -> v -> Sim a ()
writeV v = liftIO . writeVarIO v

-- | Count a failure unless the condition holds.
check :: HasCallStack => Bool -> Sim a ()
check ok = counted (\r -> withFrozenCallStack (A.assert r ok))

-- | Count a failure unless the values are equal, printing both.
checkEq :: (HasCallStack, Eq b, Show b) => b -> b -> Sim a ()
checkEq x y = counted (\r -> withFrozenCallStack (A.assertEq r x y))

-- | Count a failure unless the first value is greater.
checkGt :: (HasCallStack, Ord b, Show b) => b -> b -> Sim a ()
checkGt x y = counted (\r -> withFrozenCallStack (A.assertGt r x y))

-- | Count a failure unless the first value is less.
checkLt :: (HasCallStack, Ord b, Show b) => b -> b -> Sim a ()
checkLt x y = counted (\r -> withFrozenCallStack (A.assertLt r x y))

-- Run an assertion on the failure counter, and stop the script if it failed
-- and the session is fail-fast.
counted :: (IORef Int -> IO ()) -> Sim a ()
counted assertion = Sim $ \env -> do
  before <- readIORef (envFailed env)
  assertion (envFailed env)
  after <- readIORef (envFailed env)
  when (simFailFast (envOptions env) && after > before) (throwIO (userError "Sim: a check failed"))

-- | Continue with the value, or count a failure on 'Nothing'.
checkJust :: HasCallStack => Maybe b -> (b -> Sim a ()) -> Sim a ()
checkJust m k = maybe (withFrozenCallStack (check False)) k m
