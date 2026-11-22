{-# OPTIONS_GHC -O0 #-}
-- | Random input streams over views rich in widgets. After every frame the
-- context is consistent: focus is on nothing or on a widget the frame laid
-- out, and with no button held nothing is active and no pointer drag goes
-- on. After the stream, a view left alone settles: it asks for no frame and
-- no timed wake, as idle costs nothing.
module Cases.SimFuzz (tests) where

import Spec
import Data.Primitive.SmallArray (smallArrayFromList)
import Data.Text qualified as T
import NanoUI.Internal.Context (Context (..))
import NanoUI.Internal.Layout.Arena (lookupNodeByWidgetId)
import NanoUI.Testing.Sim qualified as Sim
import Test.QuickCheck (Arbitrary (..), Gen, Property, choose, counterexample, elements, frequency, ioProperty, vectorOf)

tests :: [Spec]
tests =
  [propSpec "sim-fuzz" 11 150 fuzzView]

width, height :: Float
width = 480
height = 360

-- | One input event or pause.
data Ev
  = EMove V2
  | EClick V2
  | ERightClick V2
  | EDoubleClick V2
  | EDrag V2 V2
  | EWheel V2 V2
  | EType T.Text
  | EKey Key
  | EWait Float
  deriving (Show)

genPoint :: Gen V2
genPoint = V2 <$> coord width <*> coord height
  where
    coord m = (\k -> m * fromIntegral k / 64) <$> choose (0 :: Int, 64)

instance Arbitrary Ev where
  arbitrary =
    frequency
      [ (3, EMove <$> genPoint)
      , (5, EClick <$> genPoint)
      , (1, ERightClick <$> genPoint)
      , (1, EDoubleClick <$> genPoint)
      , (2, EDrag <$> genPoint <*> genPoint)
      , (1, EWheel <$> genPoint <*> elements [V2 0 3, V2 0 (-3), V2 2 0, V2 0 1])
      , (2, EType <$> elements ["a", "hello", "12", " ", "\x732B\x72AC", "e\x0301"])
      , (3, EKey <$> elements [KeyTab, KeyEscape, KeyLeft, KeyRight, KeyUp, KeyDown, KeyEnter, KeyBackspace, KeyDelete, KeySpace, KeyHome, KeyEnd, KeyPageDown])
      , (2, EWait <$> elements [0.016, 0.1, 0.5, 2])
      ]

-- | Which view a stream drives, and the stream.
data Events = Events Int [Ev]
  deriving (Show)

instance Arbitrary Events where
  arbitrary = Events <$> choose (0, length views - 1) <*> (choose (5, 40) >>= (`vectorOf` arbitrary))
  shrink (Events v evs) = [Events v (take i evs <> drop (i + 1) evs) | i <- [0 .. length evs - 1]]

type Row = (T.Text, T.Text, T.Text)

-- | The views, each built fresh for a stream: every focusable control in a
-- scroll area; tabs over a table, a tree and a text area; a modal opened by
-- a button beside a menu and a context menu; a select, a combo box and a
-- numeric field over a long list.
views :: [IO (NanoUI ())]
views =
  [ pure . scrollWith (fillW . fillH) . columnWith (gap 4) $ mapM_ (void . snd) everyControl
  , do
      active <- newVar (0 :: Int)
      sortCol <- newVar (SortCol 0 SortAsc)
      sel <- newVar 0
      notes <- newVar (T.unlines (replicate 12 "a line of notes"))
      let cols = headed "Name" (\(a, _, _) -> a) <> headed "Dept" (\(_, b, _) -> b) <> headed "City" (\(_, _, c) -> c)
          rows = [(T.pack ("name " <> show i), "Eng", "Austin") | i <- [1 .. 20 :: Int]] :: [Row]
          items = smallArrayFromList [TreeItem "root" [TreeItem "child" [TreeItem "leaf" []], TreeItem "other" []], TreeItem "second" []]
      pure . columnWith (fillW . fillH) $
        tabs active
          [ tab 0 "Table" (table "people" cols rows sortCol)
          , tab 1 "Tree" (tree "files" items sel)
          , tab 2 "Notes" (textAreaWith (fillW . fixedH 200) notes)
          ]
  , do
      open <- newVar False
      menuOpen <- newVar False
      name <- newVar ("name" :: T.Text)
      pure . columnWith (padAll 10 . gap 6) $ do
        m <- readVar menuOpen
        file <- menuButton' "File" m
        when (respClicked file) (writeVar menuOpen (not m))
        let cfg = (defaultPopupConfig (AnchorRect (respRect file))) {cfgPlacement = PlacementBelow}
        (pop, _) <- popup' (constVar m) cfg (void (menuItem "New") >> void (menuItem "Open"))
        when (respClicked pop) (writeVar menuOpen False)
        b <- button' "Open dialog"
        when (respClicked b) (writeVar open True)
        _ <- contextMenu b (void (menuItem "Cut") >> void (menuItem "Copy"))
        void . modal' open "Dialog" $ do
          textInput name
          close <- button "Close"
          when close (writeVar open False)
  , do
      sel <- newVar 0
      combo <- newVar ("a" :: T.Text)
      num <- newVar 3
      pure . scrollWith (fillW . fillH) . columnWith (gap 4) $ do
        selectWith (fixedW 160) ["one", "two", "three", "four"] sel
        comboBox "Pick" ["a", "b", "c", "alpha"] combo
        numericInput num
        forM_ [1 .. 40 :: Int] $ \i -> label (T.pack ("line " <> show i))
  ]

-- | The broken invariants after a frame on @inp@.
frameProblems :: Context -> Input -> IO [String]
frameProblems ctx inp = do
  focus <- getFocusId ctx
  focusNode <- lookupNodeByWidgetId (ctxNodeArena ctx) focus
  active <- readIORef (ctxActiveId ctx)
  dragging <- pointerDragActive ctx
  let up = inputButtonsHeld inp == mempty && inputButtonsPressed inp == mempty
  pure $
    ["focus on " <> show focus <> ", which has no node" | focus /= WidgetId 0, focusNode == Nothing]
      <> ["active " <> show active <> " with no button held" | up, active /= WidgetId 0]
      <> ["a pointer drag with no button held" | up, dragging]

-- | Run a frame, then record what it broke.
checkedFrame :: IORef [String] -> Context -> Input -> NanoUI a -> IO (a, Bool)
checkedFrame found ctx inp view = do
  (a, _, again) <- runFrame ctx inp view
  problems <- frameProblems ctx inp
  unless (null problems) $ modifyIORef' found (<> problems)
  pure (a, again)

runEv :: Ev -> Sim.Sim () ()
runEv = \case
  EMove p -> void (Sim.moveTo (Sim.At p))
  EClick p -> void (Sim.click (Sim.At p))
  ERightClick p -> void (Sim.rightClick (Sim.At p))
  EDoubleClick p -> void (Sim.doubleClick (Sim.At p))
  EDrag p q -> void (Sim.drag (Sim.At p) (Sim.At q) 4)
  EWheel p d -> void (Sim.wheel (Sim.At p) d)
  EType t -> void (Sim.typeText t)
  EKey k -> void (Sim.pressKey k)
  EWait dt -> void (Sim.advance dt)

fuzzView :: Events -> Property
fuzzView (Events which evs) = ioProperty $ do
  view <- views !! which
  ctx <- newContext
  quiet <- newIORef 0
  found <- newIORef []
  let opts = Sim.defaultSimOptions {Sim.simFrameRunner = checkedFrame found}
  idle <- Sim.runSimWith opts ctx quiet (withInput width height) view $ do
    mapM_ runEv evs
    -- Left alone: the pointer off the window, and time past every
    -- animation and delay.
    Sim.modifyInput (\i -> i {inputMousePos = V2 (-10) (-10)})
    replicateM_ 3 (Sim.advance 5)
    _ <- Sim.settle
    (,) <$> Sim.wantsFrame <*> Sim.wakeTime
  problems <- readIORef found
  let settled = ["still wants frames or a wake: " <> show idle | idle /= (False, 0)]
  pure (counterexample (unlines (take 6 (problems <> settled))) (null problems && null settled))
