-- | The program on the project's landing page: four tabs in one window.
--
-- "Bloom" is the drawing from "Bloom". The other three are the stock
-- widgets, grouped so that a page fits a phone's screen with nothing to
-- scroll: buttons, choices and numbers on "Controls", text entry on
-- "Text", a table and a tree on "Data".
--
-- Every widget here is bound to a positional 'useVar', and each group reads
-- its own 'Var's back to show the value a control wrote: the slider's
-- number beside it, the name typed into the greeting, the tree's selected
-- row. Only the open tab's body runs, so a hidden tab costs nothing and
-- Bloom's spin asks for frames only while Bloom shows.
--
-- The view is shared by two programs: @cabal run nano-ui-sdl-landing@ opens
-- it in an SDL window, and @nano-ui-web-landing@ is the canvas on the
-- landing page (@site/index.html@), which quotes 'numbers' as its code
-- sample.
module Landing (landingView) where

import Control.Monad (void)
import Data.Text qualified as T
import NanoUI

import Bloom (bloomViewWith)

-- | The tabs, in order. 'enumTabBar' labels a tab with its constructor.
data Page = Bloom | Controls | Text | Data
  deriving (Bounded, Enum, Eq, Show)

-- | The landing page's view. The argument is whether Bloom starts spinning.
landingView :: Bool -> NanoUI ()
landingView spin = do
  page <- useEnumVar Bloom
  wide <- (>= 640) <$> windowWidth
  columnWith (tight . grow) $ do
    enumTabBar page
    -- 'enumTabs' would run the body too, in a column as tall as its
    -- content. Bloom's canvas takes the height left under the headers, so
    -- the body is run here, in a column that grows. Each page runs under
    -- its own key, as 'enumTabs' runs its bodies, so one page's positional
    -- 'Var's are not the next page's.
    shown <- readVar page
    columnWith (tight . grow) . withKey (fromEnum shown) $ case shown of
      Bloom -> bloomViewWith spin
      Controls -> groups wide [("Buttons", buttons), ("Choices", choices), ("Numbers", numbers)]
      Text -> groups wide [("Fields", fields), ("Notes", notes)]
      Data -> groups wide [("Table: click a header to sort", peakTable wide), ("Tree", fileTree)]

-- | A page of titled panels: side by side and equally tall in a wide
-- window, stacked in a narrow one.
groups :: Bool -> [(T.Text, NanoUI ())] -> NanoUI ()
groups wide panels =
  (if wide then rowWith (fillW . padAll 16 . gap 16) else columnWith (fillW . padAll 12 . gap 10)) $
    mapM_ group panels
  where
    group (title, body) = panelWith ((if wide then fillH else id) . fillW . padAll 12 . gap 8) $ do
      labelWith (tight . fontMuted . fontMedium) title
      body

-- | A caption over a widget.
field :: T.Text -> NanoUI a -> NanoUI a
field caption widget = columnWith (tight . gap 3 . fillW) $ do
  labelWith (tight . fontMuted) caption
  widget

-- Controls --------------------------------------------------------------------

buttons :: NanoUI ()
buttons = do
  clicks <- useVar (0 :: Int)
  pinned <- useVar False
  asking <- useVar False
  rowWith (tight . wrap . gap 8 . lineGap 8 . fillW) $ do
    whenM (button "Button") (modifyVar clicks (+ 1))
    whenM (styled primary (button "Primary")) (modifyVar clicks (+ 1))
    whenM (styled (tone Danger) (button "Delete...")) (writeVar asking True)
    toggleButton Accent "Pin" pinned
    disabledWhen True (void (button "Disabled"))
    hint <- button' "Hover me"
    tooltip hint "A tooltip opens when the pointer rests here."
  n <- readVar clicks
  isPinned <- readVar pinned
  muted (T.pack (show n) <> (if n == 1 then " click" else " clicks") <> (if isPinned then ", pinned" else ""))
  -- The dialog is declared on every frame; the Var says whether it shows.
  modal asking "Reset the count?" $ do
    label "A modal blocks the page behind it until it closes."
    rowWith (tight . gap 8) $ do
      whenM (styled (tone Danger) (button "Reset")) (writeVar clicks 0 >> writeVar asking False)
      whenM (button "Cancel") (writeVar asking False)

choices :: NanoUI ()
choices = do
  wrapLines <- useVar True
  live <- useVar False
  size <- useVar (1 :: Int)
  every <- useVar (0 :: Int)
  checkbox "Wrap long lines" wrapLines
  rowWith (tight . gap 8 . alignMid) $ do
    toggleSwitch live
    label "Live preview"
  radio ["Small", "Medium", "Large"] size
  selectWith fillW ["Back up every hour", "Back up every day", "Back up every week"] every

numbers :: NanoUI ()
numbers = do
  volume <- useVar 40
  count <- useVar (12 :: Double)
  field "Volume" $ rowWith (tight . gap 10 . alignMid . fillW) $ do
    sliderWith fillW 0 100 volume
    labelWith (tight . fixedW 36 . alignEnd) . percentage =<< readVar volume
  -- A bar drawn from the value the slider wrote on this frame.
  progressBar . (/ 100) =<< readVar volume
  field "Count: type, or step with the arrow keys" (numericInput count)
  where
    percentage v = T.pack (show (round v :: Int)) <> "%"

-- Text ------------------------------------------------------------------------

fields :: NanoUI ()
fields = do
  name <- useVar ""
  secret <- useVar "opensesame"
  city <- useVar ""
  field "Name" (textInputConfigured defaultTextInputConfig {ticPlaceholder = "Type your name"} name)
  typed <- readVar name
  labelWith (tight . fontSize 20 . fontMedium) ("Hello, " <> (if T.null typed then "you" else typed) <> ".")
  field "Password" (textInputConfigured defaultTextInputConfig {ticPassword = True} secret)
  field "City: type to filter the list" (comboBox "City" cities city)

notes :: NanoUI ()
notes = do
  text <- useVar "Select with the mouse, or with Shift\nand the arrow keys. Ctrl+Z undoes.\n\nCounts below update as you type."
  textAreaWith (fillW . fixedH 120) text
  written <- readVar text
  muted (count (T.words written) "word" <> ", " <> count (T.lines written) "line")
  where
    count xs noun = T.pack (show (length xs)) <> " " <> noun <> (if length xs == 1 then "" else "s")

cities :: [T.Text]
cities = ["Amsterdam", "Berlin", "Copenhagen", "Dublin", "Edinburgh", "Helsinki", "Lisbon", "Madrid", "Oslo", "Prague", "Stockholm", "Vienna"]

-- Data ------------------------------------------------------------------------

-- | A narrow window drops the middle column and the lower rows, so the
-- table and the tree under it both fit.
peakTable :: Bool -> NanoUI ()
peakTable wide
  | wide = void (simpleTable ["Peak", "Range", "Metres"] peaks)
  | otherwise = void (simpleTable ["Peak", "Metres"] [[peak, metres] | [peak, _, metres] <- take 5 peaks])

fileTree :: NanoUI ()
fileTree = do
  selected <- useVar (0 :: Int)
  tree "files" files selected
  muted . ("Row " <>) . (<> " selected") . T.pack . show =<< readVar selected

peaks :: [[T.Text]]
peaks =
  [ ["Everest", "Himalaya", "8849"]
  , ["K2", "Karakoram", "8611"]
  , ["Kangchenjunga", "Himalaya", "8586"]
  , ["Aconcagua", "Andes", "6961"]
  , ["Denali", "Alaska Range", "6190"]
  , ["Kilimanjaro", "Eastern Rift", "5895"]
  , ["Elbrus", "Caucasus", "5642"]
  , ["Mont Blanc", "Alps", "4806"]
  ]

files :: [TreeItem]
files =
  [ TreeItem
      "app"
      [ TreeItem "Main.hs" []
      , TreeItem "View" [TreeItem "Sidebar.hs" [], TreeItem "Editor.hs" []]
      ]
  , TreeItem "test" [TreeItem "Spec.hs" []]
  , TreeItem "app.cabal" []
  , TreeItem "README.md" []
  ]
