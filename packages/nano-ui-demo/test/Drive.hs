-- | A scripted user for the demo apps on a hidden SDL window: a
-- "NanoUI.Testing.Sim" session whose frames the SDL backend draws and
-- presents, as the app's own loop would. Each gesture ends with idle frames,
-- so what it changed is presented before the next step looks. Checks fail
-- the test at once with the caller's line, since a later step makes no sense
-- after a miss.
module Drive
  ( Drive
  , runDrive
  , frame
  , frameWith
  , clickAt
  , click
  , clickRow
  , dragTo
  , typeText
  , pressKey
  , chord
  , wheelAt
  , spans
  , overlay
  , find
  , expect
  , expectNot
  , check
  ) where

import Control.Monad (replicateM_, unless, void)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef)
import Data.Text qualified as T
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import NanoUI (Input (..), Key, NanoUI, V2 (..))
import NanoUI.Backend.Sdl (SdlEnv, sdlDrawFrame)
import NanoUI.Shortcut (Shortcut)
import NanoUI.Testing (Context, collectOverlayTextSpans)
import NanoUI.Testing.Harness (DemoSpan, findExact, findRightmost)
import NanoUI.Testing.Sim
  ( Sim
  , SimOptions (..)
  , Target (..)
  , defaultSimOptions
  , expect
  , expectNot
  , frame
  , frameWith
  , getInput
  , onContext
  , spans
  )
import NanoUI.Testing.Sim qualified as Sim

type Drive = Sim ()

-- | Run a script against @view@ from @base@ (the window size and pointer),
-- after the view settles.
runDrive :: Context -> SdlEnv -> Input -> NanoUI () -> Drive a -> IO a
runDrive ctx sdl base view script = do
  failed <- newIORef 0
  Sim.runSimWith options ctx failed base view script
  where
    options = defaultSimOptions {simFrameRunner = present, simFailFast = True}
    present c inp v = (,) () <$> sdlDrawFrame c v sdl inp False

-- | The frames after a gesture.
idle :: Int -> Drive ()
idle n = replicateM_ n (void frame)

-- | Press at @from@, hold at @to@ and release there, then two idle frames.
dragTo :: V2 -> V2 -> Drive ()
dragTo from to = Sim.drag (At from) (At to) 1 >> idle 2

clickAt :: V2 -> Drive ()
clickAt p = dragTo p p

-- | Click the visible text labelled exactly @label@, an overlay's first.
click :: HasCallStack => T.Text -> Drive ()
click label = clickAt =<< find label

-- | Click the rightmost overlay span containing @needle@, as a menu row
-- holds its shortcut too.
clickRow :: HasCallStack => T.Text -> Drive ()
clickRow needle = overlay >>= maybe (failAt ("no menu row " <> show needle)) clickAt . findRightmost needle

-- | The centre of the text labelled exactly @label@, an overlay's first.
find :: HasCallStack => T.Text -> Drive V2
find label = do
  found <- maybe (findExact label <$> spans) (pure . Just) . findExact label =<< overlay
  maybe (failAt ("no text " <> show label)) pure found

typeText :: T.Text -> Drive ()
typeText txt = Sim.typeText txt >> idle 1

pressKey :: Key -> Drive ()
pressKey k = Sim.pressKey k >> idle 1

chord :: Shortcut -> Drive ()
chord s = Sim.chord s >> idle 1

-- | Turn the wheel by @d@ over @p@, then an idle frame.
wheelAt :: V2 -> V2 -> Drive ()
wheelAt p d = frameWith (\i -> i {inputMousePos = p, inputScroll = d}) >> idle 1

-- | The overlays' text spans.
overlay :: Drive [DemoSpan]
overlay = getInput >>= onContext . flip collectOverlayTextSpans

check :: HasCallStack => String -> Bool -> Drive ()
check what ok = unless ok (failAt what)

failAt :: HasCallStack => String -> Drive a
failAt what = liftIO (ioError (userError (what <> "\n" <> prettyCallStack callStack)))
