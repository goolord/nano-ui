{-# LANGUAGE StrictData #-}

-- | Icon glyphs for the terminal chrome.
--
-- A terminal cannot report which font is loaded, so the tier is picked from the
-- environment (see @NanoUI.Term.Icons@) and falls back to ASCII. Glyph tiers use
-- only the Font Awesome block (U+F000 to U+F2E0): every Nerd Font ships it, and a
-- bare Font Awesome font covers it too, so one table serves both.
--
-- Font Awesome PUA (U+F000 to U+F2E0) is wcwidth 1 on glibc / Nerd Mono.
-- A lone FA codepoint therefore paints one cell ('terminalPaintColumns').
-- FA inside a longer string still reserves two columns via
-- 'terminalTextColumns' so following text does not sit on the glyph.
module NanoUI.Icons
  ( IconSet (..)
  , Icons (..)
  , asciiIcons
  , glyphIcons
  , iconsFor
  , parseIconSet
  , iconSetName
  , checkboxMark
  , checkboxPrefixes
  , radioMark
  , radioPrefixes
  , treeExpandMark
  , treeExpandPrefixes
  , fontAwesomeIcon
  , loneFontAwesome
  , terminalCharColumns
  , terminalTextColumns
  , terminalPaintColumns
  , terminalTextPositions
  , wideTrailChar
  ) where

import Data.Char (ord, toLower)
import Data.Text (Text)
import qualified Data.Text as T

-- | Which icon table to draw the TUI chrome with.
--
-- 'IconsFontAwesome' and 'IconsNerd' share 'glyphIcons' today; they stay
-- separate so detection can report what it found.
data IconSet
  = IconsAscii
  | IconsFontAwesome
  | IconsNerd
  deriving (Eq, Show)

-- | Glyphs the terminal backend substitutes into widget text.
data Icons = Icons
  { iconChecked :: Text
  -- ^ Checkbox prefix when set. Includes its trailing space.
  , iconUnchecked :: Text
  -- ^ Checkbox prefix when clear. Same terminal column count as 'iconChecked'.
  , iconRadioChecked :: Text
  -- ^ Radio prefix when selected. Includes its trailing space.
  , iconRadioUnchecked :: Text
  -- ^ Radio prefix when clear. Same terminal column count as 'iconRadioChecked'.
  , iconTreeExpanded :: Text
  -- ^ Tree parent prefix when open. Includes its trailing space.
  , iconTreeCollapsed :: Text
  -- ^ Tree parent prefix when closed. Same column count as 'iconTreeExpanded'.
  , iconTreeLeaf :: Text
  -- ^ Tree leaf prefix (spacer). Same column count as parent marks.
  , iconClose :: Text
  -- ^ Window and modal close button.
  , iconSelectOpen :: Text
  -- ^ Select caret while the dropdown is open. Includes its leading space.
  , iconSelectClosed :: Text
  -- ^ Select caret while the dropdown is closed.
  , iconScrollUp :: Text
  -- ^ Scrollbar end cap, start of the track. Empty means no cap.
  , iconScrollDown :: Text
  -- ^ Scrollbar end cap, end of the track.
  , iconWindowTitle :: Text
  -- ^ Title bar prefix for a window. Empty means no mark.
  , iconModalTitle :: Text
  -- ^ Title bar prefix for a modal.
  }
  deriving (Eq, Show)

-- | Brackets and letters. The default, and correct for any font.
asciiIcons :: Icons
asciiIcons =
  Icons
    { iconChecked = "[x] "
    , iconUnchecked = "[ ] "
    , iconRadioChecked = "(*) "
    , iconRadioUnchecked = "( ) "
    , iconTreeExpanded = "v "
    , iconTreeCollapsed = "> "
    , iconTreeLeaf = "  "
    , iconClose = "X"
    , iconSelectOpen = " v"
    , iconSelectClosed = " >"
    , iconScrollUp = ""
    , iconScrollDown = ""
    , iconWindowTitle = ""
    , iconModalTitle = ""
    }

-- | Font Awesome codepoints, shared by every Nerd Font.
glyphIcons :: Icons
glyphIcons =
  Icons
    { iconChecked = "\xf046 " -- check-square-o
    , iconUnchecked = "\xf096 " -- square-o
    , iconRadioChecked = "\xf192 " -- dot-circle-o
    , iconRadioUnchecked = "\xf10c " -- circle-o
    , iconTreeExpanded = "\xf078 " -- chevron-down
    , iconTreeCollapsed = "\xf054 " -- chevron-right
    , iconTreeLeaf = "  "
    , iconClose = "\xf00d" -- times
    , iconSelectOpen = " \xf078" -- chevron-down
    , iconSelectClosed = " \xf054" -- chevron-right
    , iconScrollUp = "\xf0d8" -- caret-up
    , iconScrollDown = "\xf0d7" -- caret-down
    , iconWindowTitle = "\xf2d0 " -- window-maximize
    , iconModalTitle = "\xf05a " -- info-circle
    }

iconsFor :: IconSet -> Icons
iconsFor IconsAscii = asciiIcons
iconsFor IconsFontAwesome = glyphIcons
iconsFor IconsNerd = glyphIcons

-- | Parse a user-supplied tier name. 'Nothing' means "decide automatically".
parseIconSet :: String -> Maybe IconSet
parseIconSet raw =
  case map toLower (trim raw) of
    "nerd" -> Just IconsNerd
    "nerdfont" -> Just IconsNerd
    "nerd-font" -> Just IconsNerd
    "fa" -> Just IconsFontAwesome
    "fontawesome" -> Just IconsFontAwesome
    "font-awesome" -> Just IconsFontAwesome
    "ascii" -> Just IconsAscii
    "none" -> Just IconsAscii
    "off" -> Just IconsAscii
    "0" -> Just IconsAscii
    _ -> Nothing
  where
    trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

iconSetName :: IconSet -> Text
iconSetName IconsAscii = "ascii"
iconSetName IconsFontAwesome = "fontawesome"
iconSetName IconsNerd = "nerd"

checkboxMark :: Icons -> Bool -> Text
checkboxMark icons value = if value then iconChecked icons else iconUnchecked icons

radioMark :: Icons -> Bool -> Text
radioMark icons value = if value then iconRadioChecked icons else iconRadioUnchecked icons

treeExpandMark :: Icons -> Bool -> Bool -> Text
treeExpandMark icons hasChildren expanded
  | not hasChildren = iconTreeLeaf icons
  | expanded = iconTreeExpanded icons
  | otherwise = iconTreeCollapsed icons

-- | Every prefix a checkbox label may carry, so stripping works after a tier
-- change (or a terminal node rendered through the SDL path).
checkboxPrefixes :: [Text]
checkboxPrefixes =
  [ iconChecked asciiIcons
  , iconUnchecked asciiIcons
  , iconChecked glyphIcons
  , iconUnchecked glyphIcons
  ]

radioPrefixes :: [Text]
radioPrefixes =
  [ iconRadioChecked asciiIcons
  , iconRadioUnchecked asciiIcons
  , iconRadioChecked glyphIcons
  , iconRadioUnchecked glyphIcons
  ]

treeExpandPrefixes :: [Text]
treeExpandPrefixes =
  [ iconTreeExpanded asciiIcons
  , iconTreeCollapsed asciiIcons
  , iconTreeExpanded glyphIcons
  , iconTreeCollapsed glyphIcons
  ]

-- | Font Awesome private-use block. Every Nerd Font maps these.
fontAwesomeIcon :: Char -> Bool
fontAwesomeIcon c =
  let o = ord c
   in o >= 0xF000 && o <= 0xF2E0

-- | A single Font Awesome codepoint with no neighbors.
loneFontAwesome :: Text -> Bool
loneFontAwesome txt =
  case T.uncons txt of
    Just (c, rest) -> T.null rest && fontAwesomeIcon c
    Nothing -> False

-- | Layout reserve: FA keeps a trailing column so mixed runs do not collide.
terminalCharColumns :: Char -> Int
terminalCharColumns c = if fontAwesomeIcon c then 2 else 1

terminalTextColumns :: Text -> Int
terminalTextColumns =
  T.foldl' (\n c -> n + terminalCharColumns c) 0

-- | Paint / measure width. Lone FA is one cell (Nerd Mono / wcwidth 1).
terminalPaintColumns :: Text -> Int
terminalPaintColumns txt
  | loneFontAwesome txt = 1
  | otherwise = terminalTextColumns txt

-- | Second cell of a double-width glyph. The ANSI writer skips this slot.
wideTrailChar :: Char
wideTrailChar = '\0'

-- | Raster column offsets for a span, including wide-glyph trail cells.
terminalTextPositions :: Text -> [(Int, Char)]
terminalTextPositions txt = go 0 (T.unpack txt)
  where
    go _ [] = []
    go col (c : cs) =
      let w = terminalCharColumns c
          trail =
            if w == 2
              then [(col + 1, wideTrailChar)]
              else []
       in (col, c) : trail ++ go (col + w) cs
