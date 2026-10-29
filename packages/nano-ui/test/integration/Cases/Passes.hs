{-# LANGUAGE OverloadedRecordDot #-}

-- | What one interaction costs: view passes in the frame that saw it, the
-- kind of damage it produced, and the frames that followed before the view
-- settled. Each row runs on a fresh context.
module Cases.Passes (tests) where

import Spec
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

-- | A model kept whole in one Var, for the rows that focus its fields.
data Model = Model {title :: !Text, volume :: !Float, name :: !Text}
  deriving (Eq, Generic)

data Entry = Entry {rowId :: !Int, rowName :: !Text, rowDone :: !Bool}
  deriving (Eq, Generic)

rows20 :: [Entry]
rows20 = [Entry i ("Row " <> T.pack (show i)) False | i <- [0 .. 19]]

-- | The response of the fourth row a view collects, after the view runs.
fourth :: IORef [Response] -> NanoUI () -> NanoUI Response
fourth hits ui = do
  liftIO (writeIORef hits [])
  ui
  rs <- liftIO (readIORef hits)
  pure (reverse rs !! 3)

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

-- | A press on the response the view returns, then a measured release, or
-- a held move 30 px right when @dragging@.
pressOn :: Bool -> NanoUI Response -> IO Cost
pressOn dragging ui = do
  ctx <- newContext
  pos@(V2 x y) <- centerOf <$> warmup2 ctx inp0 ui
  let hover = inp0 {inputMousePos = pos}
      moved = holdAt hover (V2 (x + 30) y)
      (next, after) = if dragging then (moved, moved) else (releaseAt (pressAt hover pos), hover)
  _ <- run2Frames ctx hover ui
  _ <- runFrame ctx (pressAt hover pos) ui
  costOf . snd <$> frameCost ctx next after ui

clickOn, dragOn :: NanoUI Response -> IO Cost
clickOn = pressOn False
dragOn = pressOn True

-- | A frame with no input, once the view has settled.
idleFrame :: NanoUI a -> IO Cost
idleFrame ui = do
  ctx <- newContext
  _ <- run2Frames ctx inp0 ui
  costOf . snd <$> frameCost ctx inp0 inp0 ui

-- | The hooks Vars replaced (23217753) cost (2, Full, 1) for a bound field.
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
      -- A list in one Var, a checkbox per row and the row's name after it.
      checkRows xs = do
        v <- newVar xs
        hits <- newIORef []
        clickOn . fourth hits . column . forEachVar rowId v $ \r -> row $ do
          resp <- checkbox' "" r.rowDone
          label =<< readVar r.rowName
          liftIO (modifyIORef hits (resp :))
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
      , -- One Var holding a record: a field read elsewhere is another part.
        ("model: field above slider", (1, Clip, 1),) <$> fresh (Model "t" 50 "") (\m -> dragOn (column (do label =<< readVar m.title; sliderWith' (fixedW 200) 0 100 m.volume)))
      , ("model: field below slider", (1, Clip, 1),) <$> fresh (Model "t" 50 "") (\m -> dragOn (column (sliderWith' (fixedW 200) 0 100 m.volume <* (label =<< readVar m.title))))
      , ("model: field above field", (1, Clip, 1),) <$> fresh (Model "t" 50 "") (\m -> typeInto (column (do label =<< readVar m.title; textInput' m.name)))
      , ("model: whole read above field", (2, Full, 1),) <$> fresh (Model "t" 50 "") (\m -> typeInto (column (do x <- readVar m; label x.title; textInput' m.name)))
      , ("model: zoomVar part above field", (2, Full, 1),) <$> fresh (Model "t" 50 "") (\m -> typeInto (column (do label =<< readVar (zoomVar (.title) (\t x -> x {title = t}) m); textInput' m.name)))
      , ("forEachVar rows: click row 3", (1, Clip, 1),) <$> checkRows rows20
      , -- Rows are told apart by one of 63 bits of their key's hash, so
        -- keys 64 apart, as from the 64th row on, share it: the earlier
        -- rows' checkboxes count as showing the one clicked.
        ("forEachVar rows, keys 64 apart: click row 3", (2, Clip, 1),) <$> checkRows (map (\e -> e {rowId = e.rowId * 64}) rows20)
      , ("withElemVars rows: click row 3", (2, Full, 1),) <$> do
          v <- newVar rows20
          hits <- newIORef []
          clickOn . fourth hits . column . withElemVars rowId v $ \xs at -> forM_ xs $ \x -> withKey (rowId x) . row $ do
            resp <- checkbox' "" (at x).rowDone
            label =<< readVar (at x).rowName
            liftIO (modifyIORef hits (resp :))
      , -- A row that changes its own key changes the list's order of keys,
        -- which forEachVar laid the rows out by.
        ("forEachVar row rekeys itself", (2, Full, 0),) <$> do
          v <- newVar rows20
          hits <- newIORef []
          clickOn . fourth hits . column . forEachVar rowId v $ \r -> row $ do
            resp <- button' "Rekey"
            when (respClicked resp) (modifyVar r.rowId (+ 100))
            liftIO (modifyIORef hits (resp :))
      ]
  forM_ rows $ \(name, expected, actual) -> do
    putStrLn ("  " <> name <> ": " <> show actual)
    when (expected /= actual) (putStrLn ("  expected " <> show expected) >> bump failed)
