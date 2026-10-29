{-# LANGUAGE DerivingVia #-}

-- | A scripted user for the demo apps on a hidden SDL window, in the manner
-- of "NanoUI.Testing.Sim", except that the SDL backend draws and presents
-- every frame, as the app's own loop would. Checks fail the test at once
-- with the caller's line, since a later step makes no sense after a miss.
module Drive
  ( Drive
  , runDrive
  , context
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
  , liftIO
  ) where

import Control.Monad (unless, void)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Text qualified as T
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import NanoUI (Input (..), Key, NanoUI, V2 (..))
import NanoUI.Backend.Sdl (SdlEnv, sdlDrawFrame)
import NanoUI.Shortcut (Shortcut)
import NanoUI.Testing (Context, collectOverlayTextSpans, collectTextSpans)
import NanoUI.Testing.Harness
  (DemoSpan, chordInp, findExact, findRightmost, hasText, holdAt, keyInp, pressAt, releaseAt)

data Env = Env {envCtx :: Context, envSdl :: SdlEnv, envView :: NanoUI (), envBase :: Input}

newtype Drive a = Drive (Env -> IO a)
  deriving (Functor, Applicative, Monad, MonadFail, MonadIO) via ReaderT Env IO

-- | Run a script against @view@, every frame starting from @base@.
runDrive :: Context -> SdlEnv -> Input -> NanoUI () -> Drive a -> IO a
runDrive ctx sdl base view (Drive script) = script (Env ctx sdl view base)

context :: Drive Context
context = Drive (pure . envCtx)

-- | Draw and present one frame on the base input changed by @f@.
frameWith :: (Input -> Input) -> Drive ()
frameWith f = Drive $ \e -> void (sdlDrawFrame (envCtx e) (envView e) (envSdl e) (f (envBase e)) False)

frame :: Drive ()
frame = frameWith id

-- | Press at @from@, hold at @to@ and release there, then two idle frames.
dragTo :: V2 -> V2 -> Drive ()
dragTo from to = do
  frameWith (`pressAt` from)
  frameWith (`holdAt` to)
  frameWith (releaseAt . (`holdAt` to))
  frame
  frame

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
typeText txt = frameWith (\i -> i {inputChars = txt}) >> frame

pressKey :: Key -> Drive ()
pressKey k = frameWith (keyInp k) >> frame

chord :: Shortcut -> Drive ()
chord s = frameWith (chordInp s) >> frame

-- | Turn the wheel by @d@ over @p@, then an idle frame.
wheelAt :: V2 -> V2 -> Drive ()
wheelAt p d = frameWith (\i -> i {inputMousePos = p, inputScroll = d}) >> frame

-- | The base layer's text spans, and the overlays'.
spans, overlay :: Drive [DemoSpan]
spans = Drive (collectTextSpans . envCtx)
overlay = Drive (\e -> collectOverlayTextSpans (envCtx e) (envBase e))

-- | Some span of either layer contains @needle@, or not.
expect, expectNot :: HasCallStack => T.Text -> Drive ()
expect needle = shown needle >>= \ok -> unless ok (failAt ("expected text " <> show needle))
expectNot needle = shown needle >>= \ok -> unless (not ok) (failAt ("unexpected text " <> show needle))

shown :: T.Text -> Drive Bool
shown needle = (||) <$> (hasText needle <$> spans) <*> (hasText needle <$> overlay)

check :: HasCallStack => String -> Bool -> Drive ()
check what ok = unless ok (failAt what)

failAt :: HasCallStack => String -> Drive a
failAt what = liftIO (ioError (userError (what <> "\n" <> prettyCallStack callStack)))
