-- | Key chords: a key and the exact modifiers held with it, put together
-- with '<>' (@ctrl <> shift <> key 's'@), read from text written as
-- xmonad's EZConfig writes it (@C-S-s@), and shown in a menu as a label
-- (@Ctrl+Shift+S@).
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

-- | A key chord: a key pressed while exactly these modifiers are held, so
-- @ctrl <> key 's'@ is not @ctrl <> shift <> key 's'@. Chords combine with
-- '<>': the modifiers of both, and the key of the right one if it has one.
-- A chord with no key, such as @ctrl <> shift@ alone, is never pressed.
data Shortcut = Shortcut
  { shortcutKey :: !(Maybe Key)
  , shortcutModifiers :: !Modifiers
  }
  deriving (Eq, Ord, Show)

instance Semigroup Shortcut where
  Shortcut k m <> Shortcut k' m' = Shortcut (k' <|> k) (m <> m')

instance Monoid Shortcut where
  mempty = Shortcut Nothing mempty

-- | What names a key: a 'Key', or a 'Char' for the character key that types
-- it without Shift ('KeyChar'; a letter in either case).
class ToKey a where
  toKey :: a -> Key

instance ToKey Key where
  toKey = id

instance ToKey Char where
  toKey = KeyChar . toLower

-- | The key of a chord: @key 's'@, @key KeyEnter@, @key (KeyF 5)@. A
-- shifted symbol names the key that types it unshifted: on a US layout
-- @ctrl <> shift <> key '='@ is the chord people call Ctrl++, and
-- @ctrl <> key '+'@ is the keypad's plus.
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

-- | The platform's command modifier held ('modPrimary': Command on macOS,
-- Ctrl elsewhere), so @cmdOrCtrl <> key 's'@ saves on both.
cmdOrCtrl :: Shortcut
cmdOrCtrl = modifier primaryModifiers

modifier :: Modifiers -> Shortcut
modifier = Shortcut Nothing

-- | Read a chord written as text, such as one from a settings file: modifier
-- prefixes, then a key. In code, put the chord together with '<>' instead
-- (@cmdOrCtrl <> shift <> key 'p'@), which cannot be misspelled.
--
-- * The modifiers are @C-@ Ctrl, @S-@ Shift, @A-@ Alt (Option), @s-@ Super
--   (Command, the Windows key), and @M-@, the platform's command modifier
--   ('modPrimary': Command on macOS, Ctrl elsewhere).
-- * The key is one character, the one the key types without Shift
--   ('KeyChar'; a letter in either case), or a named key in angle brackets:
--   @<F1>@ to @<F24>@, or a 'Key' constructor without its @Key@ prefix, as
--   in @<Enter>@, @<Escape>@, @<PageDown>@ or @<Space>@.
--
-- > C-s   M-S-p   A-<Enter>   <F5>   C--   C-S-=
--
-- A shifted symbol names the key that types it unshifted: on a US layout
-- @C-S-=@ is the chord people call Ctrl++, and @C-+@ is the keypad's plus.
-- 'Left' says the text is not a chord.
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

-- | Each modifier's prefix letter, and the modifier.
modifierPrefixes :: [(Char, Shortcut)]
modifierPrefixes = [('C', ctrl), ('S', shift), ('A', alt), ('s', super), ('M', cmdOrCtrl)]

-- | The keys a chord names in angle brackets, by their 'keyLabel'.
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

-- | The chord as a menu shows it: the held modifiers in the order Ctrl, Alt,
-- Shift, Super (Option and Cmd on macOS), then the key ('keyLabel'), joined
-- by @+@.
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

-- | A key's name: a letter in upper case, another character as itself, and a
-- named key by its constructor without the @Key@ prefix.
keyLabel :: Key -> Text
keyLabel = \case
  KeyChar c -> T.singleton (toUpper c)
  KeyF n -> "F" <> T.pack (show n)
  k -> T.pack (drop 3 (show k))

-- | Whether this input presses the chord: its key is among the frame's key
-- presses and the modifiers held are exactly the chord's. The input as it
-- stands, with no regard for focus, modals, or another shortcut that took
-- the key; a view asks 'NanoUI.shortcut' instead.
shortcutIn :: Shortcut -> Input -> Bool
shortcutIn (Shortcut k mods) inp =
  inputModifiers inp == mods && any (`inputKeysElem` inputKeys inp) k
