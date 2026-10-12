-- | What one interaction costs: view passes in the frame that saw it, the
-- kind of damage it produced, and the frames that followed before the view
-- settled. Each row runs on a fresh context.
module Cases.Passes (tests) where

import Spec
import Data.Text (Text)
import Data.Text qualified as T

tests :: [Spec]
tests = [spec "passes-per-interaction" runPassesTest]

data Kind = Full | Clip | NoDamage
  deriving (Eq, Show)

kindOf :: Damage -> Kind
kindOf DamageFull = Full
kindOf d
  | damageIsEmpty d = NoDamage
  | otherwise = Clip

-- | Passes, damage kind and follow-up frames.
type Cost = (Int, Kind, Int)

costOf :: FrameCost -> Cost
costOf (FrameCost p d f) = (p, kindOf d, f)

inp0 :: Input
inp0 = (withInputOff 400 300) {inputDeltaTime = 0.5}

-- | One keystroke into the view's first focusable, a text field.
typeInto :: NanoUI a -> IO Cost
typeInto ui = do
  ctx <- newContext
  warmupFocused ctx inp0 ui
  _ <- run2Frames ctx inp0 ui
  costOf . snd <$> frameCost ctx (inp0 {inputChars = "a"}) inp0 ui

-- | A press on the response the view returns, then the measured release.
clickOn :: NanoUI Response -> IO Cost
clickOn ui = do
  ctx <- newContext
  r <- warmup2 ctx inp0 ui
  let pos = centerOf r
      hover = inp0 {inputMousePos = pos}
      (press, release) = clickPair hover pos
  _ <- run2Frames ctx hover ui
  _ <- runFrame ctx press ui
  costOf . snd <$> frameCost ctx release hover ui

-- | A frame with no input, once the view has settled.
idleFrame :: NanoUI a -> IO Cost
idleFrame ui = do
  ctx <- newContext
  _ <- run2Frames ctx inp0 ui
  costOf . snd <$> frameCost ctx inp0 inp0 ui

-- | A press on the returned slider's centre, then a measured held move.
dragOn :: NanoUI Response -> IO Cost
dragOn ui = do
  ctx <- newContext
  r <- warmup2 ctx inp0 ui
  let pos = centerOf r
      hover = inp0 {inputMousePos = pos}
      to = let V2 x y = pos in V2 (x + 30) y
  _ <- run2Frames ctx hover ui
  _ <- runFrame ctx (pressAt hover pos) ui
  costOf . snd <$> frameCost ctx (holdAt hover to) (holdAt hover to) ui

-- | Measured with the hooks this replaced (23217753): a hook-bound field,
-- slider or checkbox, and a hook display above or below its button, each
-- cost (2, Full, 1); a field held in an IORef (1, Clip, 1); a plain button
-- (1, Clip, 0).
runPassesTest :: Context -> IORef Int -> IO ()
runPassesTest _ failed = do
  let fresh :: a -> (Var a -> IO b) -> IO b
      fresh x k = newVar x >>= k
      counter above v = column $ do
        when above (label . T.pack . show =<< readVar v)
        r <- button' "+"
        when (respClicked r) (modifyVar v (+ (1 :: Int)))
        unless above (label . T.pack . show =<< readVar v)
        pure r
  rows <-
    sequence
      [ ("field", (1, Clip, 1),) <$> fresh "" (\v -> typeInto (column (textInput' v)))
      , ("field, label below", (1, Full, 1),) <$> fresh "" (\v -> typeInto (column (textInput' v <* (label =<< readVar v))))
      , ("field, label above", (2, Full, 1),) <$> fresh "" (\v -> typeInto (column ((label =<< readVar v) >> textInput' v)))
      , ("field read before, written after", (2, Full, 1),) <$> fresh "" (\v -> typeInto (column (do x <- readVar v; (r, x') <- withValue x textInput'; writeVar v x'; pure r)))
      , ("withValue field", (1, Clip, 1),) <$> (newIORef ("" :: Text) >>= \ref -> typeInto (column (do x <- liftIO (readIORef ref); (r, x') <- withValue x textInput'; liftIO (writeIORef ref x'); pure r)))
      , ("slider", (1, Clip, 1),) <$> fresh 50 (\v -> dragOn (column (sliderWith' (fixedW 200) 0 100 v)))
      , ("checkbox", (1, Clip, 1),) <$> fresh False (\v -> clickOn (column (checkbox' "Opt" v)))
      , ("plain button", (1, Clip, 0),) <$> clickOn (column (button' "+"))
      , ("display above", (2, Full, 0),) <$> fresh 0 (clickOn . counter True)
      , ("display below", (1, Full, 0),) <$> fresh 0 (clickOn . counter False)
      , ("equal writes, idle", (1, NoDamage, 0),) <$> fresh "x" (\v -> idleFrame (column (writeVar v "x" >> textInput' v)))
      ]
  forM_ rows $ \(name, expected, actual) -> do
    putStrLn ("  " <> name <> ": " <> show actual)
    when (expected /= actual) (putStrLn ("  expected " <> show expected) >> bump failed)
