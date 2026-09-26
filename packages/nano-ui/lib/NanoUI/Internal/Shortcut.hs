-- | Key chords: a key plus the exact modifiers held with it. Build them with
-- '<>' (@ctrl <> shift <> key 's'@), parse them from xmonad EZConfig-style
-- text (@C-S-s@), and label them for menus (@Ctrl+Shift+S@).
module NanoUI.Internal.Shortcut
  ( Shortcut (..)
  , ToKey (..)
  , key
  , ctrl
  , shift
  , alt
  , super
  , cmdOrCtrl
  , parseShortcut
  , shortcutLabel
  , keyLabel
  , shortcutIn
  ) where

import Control.Applicative ((<|>))
import Data.Char (toLower, toUpper)
import Data.Text (Text)
import Data.Text qualified as T
import NanoUI.Internal.Input
import Text.ParserCombinators.ReadP (choice, eof, get, many, readP_to_S, string, (+++))

-- | A key pressed while exactly these modifiers are held, so
-- @ctrl <> key 's'@ does not match Ctrl+Shift+S. '<>' unions the modifiers
-- and keeps the right-hand key if it has one. A chord with no key, such as
-- @ctrl <> shift@, never matches.
data Shortcut = Shortcut
  { shortcutKey :: !(Maybe Key)
  , shortcutModifiers :: !Modifiers
  }
  deriving (Eq, Ord, Show)

instance Semigroup Shortcut where
  Shortcut k m <> Shortcut k' m' = Shortcut (k' <|> k) (m <> m')

instance Monoid Shortcut where
  mempty = Shortcut Nothing mempty

-- | A 'Key', or a 'Char' naming the key that types it without Shift
-- ('KeyChar'). Letters may be either case.
class ToKey a where
  toKey :: a -> Key

instance ToKey Key where
  toKey = id

instance ToKey Char where
  toKey = KeyChar . toLower

-- | The key of a chord: @key 's'@, @key KeyEnter@, @key (KeyF 5)@. Name a
-- symbol by its unshifted key: on a US layout Ctrl++ is
-- @ctrl <> shift <> key '='@, while @ctrl <> key '+'@ is keypad plus.
key :: ToKey k => k -> Shortcut
key k = Shortcut (Just (toKey k)) mempty

-- | Ctrl held.
ctrl :: Shortcut
ctrl = modifier noModifiers {modCtrl = True}

-- | Shift held.
shift :: Shortcut
shift = modifier noModifiers {modShift = True}

-- | Alt (Option) held.
alt :: Shortcut
alt = modifier noModifiers {modAlt = True}

-- | Super (Command, the Windows key) held.
super :: Shortcut
super = modifier noModifiers {modSuper = True}

-- | Command held on macOS, Ctrl elsewhere ('modPrimary'), so
-- @cmdOrCtrl <> key 's'@ works on both.
cmdOrCtrl :: Shortcut
cmdOrCtrl = modifier primaryModifiers

modifier :: Modifiers -> Shortcut
modifier = Shortcut Nothing

-- | Parse a chord from text, such as a settings file: modifier prefixes,
-- then a key. In code, prefer '<>' (@cmdOrCtrl <> shift <> key 'p'@), which
-- cannot be misspelled.
--
-- * Modifiers: @C-@ Ctrl, @S-@ Shift, @A-@ Alt (Option), @s-@ Super
--   (Command, the Windows key), @M-@ Command on macOS and Ctrl elsewhere
--   ('modPrimary').
-- * Key: one character, as typed without Shift ('KeyChar'; letters in
--   either case), or a named key in angle brackets: @<F1>@ to @<F24>@, or a
--   'Key' constructor without its @Key@ prefix, such as @<Enter>@,
--   @<Escape>@, @<PageDown>@ or @<Space>@.
--
-- > C-s   M-S-p   A-<Enter>   <F5>   C--   C-S-=
--
-- Name a symbol by its unshifted key: on a US layout Ctrl++ is @C-S-=@,
-- while @C-+@ is keypad plus. Returns 'Left' for text that is not a chord.
parseShortcut :: Text -> Either Text Shortcut
parseShortcut txt =
  case [s | (s, "") <- readP_to_S chord (T.unpack txt)] of
    s : _ -> Right s
    [] -> Left ("not a chord: " <> txt)
  where
    chord = do
      mods <- many (choice [m <$ string [prefix, '-'] | (prefix, m) <- modifierPrefixes])
      k <- choice [named <$ string ('<' : name ++ ">") | (name, named) <- keyNames] +++ (toKey <$> get)
      eof
      pure (mconcat mods <> key k)

modifierPrefixes :: [(Char, Shortcut)]
modifierPrefixes = [('C', ctrl), ('S', shift), ('A', alt), ('s', super), ('M', cmdOrCtrl)]

-- | Keys written in angle brackets, by their 'keyLabel'.
keyNames :: [(String, Key)]
keyNames =
  [ (T.unpack (keyLabel k), k)
  | k <-
      [ KeyBackspace, KeyDelete, KeyEnter, KeyEscape, KeyTab, KeyLeft, KeyRight, KeyUp, KeyDown, KeyHome, KeyEnd
      , KeyPageUp, KeyPageDown, KeyInsert, KeySpace, KeyPrintScreen, KeyPause, KeyCapsLock, KeyNumLock
      , KeyScrollLock, KeyMenu
      ]
        ++ map KeyF [1 .. 24]
  ]

-- | The chord as a menu label: modifiers in the order Ctrl, Alt, Shift,
-- Super (Option and Cmd on macOS), then the key ('keyLabel'), joined by @+@.
shortcutLabel :: Shortcut -> Text
shortcutLabel (Shortcut k mods) =
  T.intercalate "+" ([name | (True, name) <- held] ++ maybe [] (pure . keyLabel) k)
  where
    held =
      [ (modCtrl mods, "Ctrl")
      , (modAlt mods, if onMac then "Option" else "Alt")
      , (modShift mods, "Shift")
      , (modSuper mods, if onMac then "Cmd" else "Super")
      ]

-- | A key's name: letters upper-cased, other characters as is, named keys
-- by constructor without the @Key@ prefix.
keyLabel :: Key -> Text
keyLabel = \case
  KeyChar c -> T.singleton (toUpper c)
  KeyF n -> "F" <> T.pack (show n)
  k -> T.pack (drop 3 (show k))

-- | Whether the input presses the chord: its key was pressed this frame with
-- exactly its modifiers held. Ignores focus, modals and other shortcuts that
-- claimed the key; views use 'NanoUI.shortcut' instead.
shortcutIn :: Shortcut -> Input -> Bool
shortcutIn (Shortcut k mods) inp =
  inputModifiers inp == mods && any (`pressedIn` inp) k
