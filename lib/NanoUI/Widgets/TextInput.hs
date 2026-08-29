module NanoUI.Widgets.TextInput
  ( TextInputState (..)
  , textInputLayout
  , processTextInput
  , applyTextInputMenuAction
  ) where

import Control.Monad (foldM, void, when)
import Data.IORef (writeIORef)
import qualified Data.IntMap.Strict as IM
import NanoUI.Context
  ( Context (..)
  , getStore
  , intKey
  , markDirty
  , setStore
  )
import NanoUI.Id (WidgetId)
import NanoUI.Input (Input (..), Key (..), Modifiers (..), inputChars, inputKeys, inputModifiers)
import NanoUI.Style (Layout (..), Sizing (..), defaultLayout)
import NanoUI.Store (WidgetStore (..))

textInputLayout :: Layout
textInputLayout =
  defaultLayout
    { layoutWidth = Grow 1
    , layoutMinW = 160
    }

data TextInputState = TextInputState
  { tisText :: String
  , tisCursor :: Int
  , tisAnchor :: Int
  }
  deriving (Eq, Show)

textInputSelRange :: TextInputState -> Maybe (Int, Int)
textInputSelRange s
  | tisAnchor s == tisCursor s = Nothing
  | otherwise = Just (min (tisAnchor s) (tisCursor s), max (tisAnchor s) (tisCursor s))

selectionText :: TextInputState -> Maybe String
selectionText s =
  case textInputSelRange s of
    Nothing -> Nothing
    Just (lo, hi) -> Just (take (hi - lo) (drop lo (tisText s)))

selectAllTextInput :: TextInputState -> TextInputState
selectAllTextInput s =
  s {tisAnchor = 0, tisCursor = length (tisText s)}

textInputCopy :: Context -> TextInputState -> IO ()
textInputCopy ctx s = do
  let txt =
        case selectionText s of
          Just slice -> slice
          Nothing -> tisText s
  when (not (null txt)) $
    void (ctxClipboardSet ctx txt)

textInputCut :: Context -> TextInputState -> IO TextInputState
textInputCut ctx s = do
  case selectionText s of
    Nothing -> pure s
    Just slice -> do
      void (ctxClipboardSet ctx slice)
      pure (deleteBackward s)

textInputPaste :: Context -> TextInputState -> IO TextInputState
textInputPaste ctx s = do
  mtxt <- ctxClipboardGet ctx
  case mtxt of
    Nothing -> pure s
    Just paste ->
      let p = paste
          pos = case textInputSelRange s of
            Nothing -> tisCursor s
            Just (lo, _) -> lo
          t = tisText s
          t' =
            case textInputSelRange s of
              Nothing -> take pos t ++ p ++ drop pos t
              Just (lo, hi) -> take lo t ++ p ++ drop hi t
          end = pos + length p
       in pure s {tisText = t', tisCursor = end, tisAnchor = end}

applyTextInputMenuAction :: Context -> WidgetId -> Int -> IO ()
applyTextInputMenuAction ctx wid item = do
  store <- getStore ctx
  let key = intKey wid
      text = IM.findWithDefault "" key (storeText store)
      cursor = IM.findWithDefault (length text) key (storeCursor store)
      anchor = IM.findWithDefault cursor key (storeSelAnchor store)
      s0 = TextInputState text cursor anchor
  s1 <-
    case item of
      0 -> textInputCut ctx s0
      1 -> textInputCopy ctx s0 >> pure s0
      2 -> textInputPaste ctx s0
      3 -> pure (selectAllTextInput s0)
      _ -> pure s0
  setStore
    ctx
    ( store
        { storeText = IM.insert key (tisText s1) (storeText store)
        , storeCursor = IM.insert key (tisCursor s1) (storeCursor store)
        , storeSelAnchor = IM.insert key (tisAnchor s1) (storeSelAnchor store)
        }
    )
  writeIORef (ctxTextInputMenu ctx) Nothing
  markDirty ctx

processTextInput :: Context -> Input -> TextInputState -> IO TextInputState
processTextInput ctx inp s0 = do
  let mods = inputModifiers inp
      ctrl = modCtrl mods
      shift = modShift mods
      keys = inputKeys inp
      chars = inputChars inp
  s1 <-
    if ctrl
      then foldM (handleCtrlChar ctx) s0 chars
      else pure s0
  let filtered = filter (not . isCtrlCombo ctrl) chars
      s2 = foldl insertChar s1 filtered
  pure (foldl (applyKey shift) s2 keys)
  where
    isCtrlCombo c ch = c && ch `elem` ("aAcCxXvV\x01" :: String)

handleCtrlChar :: Context -> TextInputState -> Char -> IO TextInputState
handleCtrlChar ctx s ch
  | ch `elem` ('a' : 'A' : '\x01' : []) = pure (selectAllTextInput s)
  | ch `elem` ('c' : 'C' : '\ETX' : []) = textInputCopy ctx s >> pure s
  | ch `elem` ('x' : 'X' : []) = textInputCut ctx s
  | ch `elem` ('v' : 'V' : []) = textInputPaste ctx s
  | otherwise = pure s

insertChar :: TextInputState -> Char -> TextInputState
insertChar s ch =
  case textInputSelRange s of
    Nothing ->
      let t = tisText s
          c = tisCursor s
          pos = c + 1
       in s {tisText = take c t ++ [ch] ++ drop c t, tisCursor = pos, tisAnchor = pos}
    Just (lo, hi) ->
      let t = tisText s
          pos = lo + 1
       in s {tisText = take lo t ++ [ch] ++ drop hi t, tisCursor = pos, tisAnchor = pos}

applyKey :: Bool -> TextInputState -> Key -> TextInputState
applyKey shift s key =
  case key of
    KeyBackspace -> deleteBackward s
    KeyDelete -> deleteForward s
    KeyLeft -> moveCursor s (max 0 (tisCursor s - 1)) shift
    KeyRight -> moveCursor s (min (length (tisText s)) (tisCursor s + 1)) shift
    KeyHome -> moveCursor s 0 shift
    KeyEnd -> moveCursor s (length (tisText s)) shift
    _ -> s

deleteBackward :: TextInputState -> TextInputState
deleteBackward s =
  case textInputSelRange s of
    Just (lo, hi) -> s {tisText = take lo (tisText s) ++ drop hi (tisText s), tisCursor = lo, tisAnchor = lo}
    Nothing ->
      let c = tisCursor s
       in if c > 0
            then
              let t = tisText s
                  pos = c - 1
               in s {tisText = take pos t ++ drop c t, tisCursor = pos, tisAnchor = pos}
            else s

deleteForward :: TextInputState -> TextInputState
deleteForward s =
  case textInputSelRange s of
    Just (lo, hi) -> s {tisText = take lo (tisText s) ++ drop hi (tisText s), tisCursor = lo, tisAnchor = lo}
    Nothing ->
      let c = tisCursor s
          t = tisText s
       in if c < length t
            then s {tisText = take c t ++ drop (c + 1) t}
            else s

moveCursor :: TextInputState -> Int -> Bool -> TextInputState
moveCursor s pos shift =
  if shift
    then s {tisCursor = pos}
    else s {tisCursor = pos, tisAnchor = pos}
