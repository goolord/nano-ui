-- | Interactive four-tab demo embedded on the nano-ui landing page.
--
-- The Bloom tab runs the animated rose-curve canvas from "Bloom". Controls,
-- Text, and Data showcase built-in widgets grouped into compact panels that
-- fit on mobile viewports without scrolling.
--
-- Every control binds to a view-local 'useVar' and reads it back in the same
-- pass so live readouts update immediately (slider percentage, greeting text,
-- word count, selected tree row). Only the active tab's body runs, so hidden
-- tabs cost nothing and Bloom only animates while its tab is open.
--
-- Shared by two executables: @cabal run nano-ui-sdl-landing@ opens it in a
-- native SDL3 window, and @nano-ui-web-landing@ compiles it to WebAssembly
-- for @site/index.html@ (which also quotes 'numbers' verbatim as a code sample).
-- Keep all visible strings pure ASCII so the web build needs no fallback font.
module Landing (landingView) where

import Control.Monad (void)
import Data.Text qualified as T
import NanoUI

import Bloom (bloomViewWith)

-- | Landing demo tabs in display order. 'enumTabBar' uses constructor names
-- directly as tab labels.
data Page = Bloom | Controls | Text | Data
  deriving (Bounded, Enum, Eq, Show)

-- | Root view for the landing page demo. Pass 'False' to start Bloom paused
-- (used when the browser prefers reduced motion).
landingView :: Bool -> NanoUI ()
landingView spin = do
  page <- useEnumVar Bloom
  wide <- (>= 640) <$> windowWidth
  columnWith (tight . grow) $ do
    enumTabBar page
    -- Run the active tab body manually inside a growing column so Bloom's
    -- canvas expands to fill the remaining height beneath the tab bar. Keying
    -- by tab index keeps each page's positional 'useVar' slots isolated.
    shown <- readVar page
    columnWith (tight . grow) . withKey (fromEnum shown) $ case shown of
      Bloom -> bloomViewWith spin
      Controls -> groups wide [("Buttons", buttons), ("Choices", choices), ("Numbers", numbers)]
      Text -> groups wide [("Fields", fields), ("Notes", notes)]
      Data -> groups wide [("Table (click a header to sort)", peakTable wide), ("Tree", fileTree)]

-- | Lay out titled panels side by side at equal height on wide viewports, or
-- stacked vertically on narrow ones.
groups :: Bool -> [(T.Text, NanoUI ())] -> NanoUI ()
groups wide panels =
  (if wide then rowWith (fillW . padAll 16 . gap 16) else columnWith (fillW . padAll 12 . gap 10)) $
    mapM_ group panels
  where
    group (title, body) = panelWith ((if wide then fillH else id) . fillW . padAll 12 . gap 8) $ do
      labelWith (tight . fontMuted . fontMedium) title
      body

-- | Render a muted caption directly above a control.
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
    tooltip hint "Tooltips appear when you rest the pointer over a widget."
  n <- readVar clicks
  isPinned <- readVar pinned
  muted (T.pack (show n) <> (if n == 1 then " click" else " clicks") <> (if isPinned then ", pinned" else ""))
  -- Always call modals unconditionally; the Var controls visibility.
  modal asking "Reset click count?" $ do
    label "Modals pause interaction with the page behind them until dismissed."
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
  -- Reads the value written by the slider in the same frame.
  progressBar . (/ 100) =<< readVar volume
  field "Count (type or use arrow keys)" (numericInput count)
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
  field "City (type to filter)" (comboBox "City" cities city)

notes :: NanoUI ()
notes = do
  text <- useVar "Select text with the mouse, or with Shift\nand the arrow keys. Press Ctrl+Z to undo.\n\nWord and line counts below update live."
  textAreaWith (fillW . fixedH 120) text
  written <- readVar text
  muted (count (T.words written) "word" <> ", " <> count (T.lines written) "line")
  where
    count xs noun = T.pack (show (length xs)) <> " " <> noun <> (if length xs == 1 then "" else "s")

cities :: [T.Text]
cities = ["Amsterdam", "Berlin", "Copenhagen", "Dublin", "Edinburgh", "Helsinki", "Lisbon", "Madrid", "Oslo", "Prague", "Stockholm", "Vienna"]

-- Data ------------------------------------------------------------------------

-- | On narrow viewports, drop the middle column and bottom rows so the table
-- and tree both fit cleanly without scrolling.
peakTable :: Bool -> NanoUI ()
peakTable wide
  | wide = void (simpleTable ["Peak", "Range", "Meters"] peaks)
  | otherwise = void (simpleTable ["Peak", "Meters"] [[peak, meters] | [peak, _, meters] <- take 5 peaks])

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
