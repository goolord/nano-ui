module NanoUI.Widgets.TextInput
  ( TextInputState (..)
  , textInputLayout
  , processTextInput
  , applyTextInputMenuAction
  , textInputMenuActionEnabled
  ) where

import Control.Monad (void, when)
import Data.Char (isPrint)
import Data.IORef (writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.IntMap.Strict as IM
import NanoUI.Context
  ( Context (..)
  , getStore
  , intKey
  , markDirty
  , setStore
  )
import NanoUI.Id (WidgetId)
import NanoUI.Input
  ( Input (..)
  , Key (..)
  , Modifiers (..)
  , foldInputKeys
  , inputChars
  , inputKeys
  , inputModifiers
  )
import NanoUI.Style (Layout (..), Sizing (..), defaultLayout)
import NanoUI.Store (WidgetStore (..), slotAnchor, slotCursor, slotKey)
import qualified NanoUI.Widgets.TextBuffer as TB

textInputLayout :: Layout
textInputLayout =
  defaultLayout
    { layoutWidth = Grow 1
    , layoutMinW = 160
    }

data TextInputState = TextInputState
  { tisText :: !Text
  , tisCursor :: !Int
  , tisAnchor :: !Int
  }
  deriving (Eq, Show)

toBuffer :: TextInputState -> (TB.TextBuffer, TB.Cursor)
toBuffer s =
  let buf0 = TB.fromText (tisText s)
      cur = TB.Cursor 0 (tisCursor s)
      anc = TB.Cursor 0 (tisAnchor s)
   in (TB.withCursor cur buf0, anc)

selectAllTextInput :: TextInputState -> TextInputState
selectAllTextInput s =
  s {tisAnchor = 0, tisCursor = T.length (tisText s)}

textInputCopy :: Context -> TextInputState -> IO ()
textInputCopy ctx s = do
  let (buf, anc) = toBuffer s
      cur = TB.getCursor buf
      txt = if anc /= cur then TB.selectedText anc cur buf else tisText s
  when (not (T.null txt)) $
    void (ctxClipboardSet ctx txt)

textInputCut :: Context -> TextInputState -> IO TextInputState
textInputCut ctx s = do
  let (buf, anc) = toBuffer s
      cur = TB.getCursor buf
  if anc /= cur
    then do
      let txt = TB.selectedText anc cur buf
      when (not (T.null txt)) $
        void (ctxClipboardSet ctx txt)
      let buf' = TB.deleteRange anc cur buf
          TB.Cursor _ c = TB.getCursor buf'
      pure (TextInputState (TB.toText buf') c c)
    else do
      when (not (T.null (tisText s))) $
        void (ctxClipboardSet ctx (tisText s))
      pure (TextInputState T.empty 0 0)

textInputPaste :: Context -> TextInputState -> IO TextInputState
textInputPaste ctx s = do
  mtxt <- ctxClipboardGet ctx
  case mtxt of
    Nothing -> pure s
    Just rawPaste -> do
      let paste = T.filter (/= '\n') rawPaste
          (buf, anc) = toBuffer s
          cur = TB.getCursor buf
          buf' =
            if anc /= cur
              then TB.replaceRange paste anc cur buf
              else TB.insertText paste buf
          TB.Cursor _ c = TB.getCursor buf'
      pure (TextInputState (TB.toText buf') c c)

applyTextInputMenuAction :: Context -> WidgetId -> Int -> IO ()
applyTextInputMenuAction ctx wid item = do
  store <- getStore ctx
  let key = intKey wid
      text = IM.findWithDefault "" key (storeText store)
      cursor = IM.findWithDefault (T.length text) (slotKey slotCursor key) (storeInt store)
      anchor = IM.findWithDefault cursor (slotKey slotAnchor key) (storeInt store)
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
        , storeInt =
            IM.insert (slotKey slotCursor key) (tisCursor s1) $
              IM.insert (slotKey slotAnchor key) (tisAnchor s1) (storeInt store)
        }
    )
  writeIORef (ctxTextInputMenu ctx) Nothing
  markDirty ctx

textInputMenuActionEnabled :: Context -> WidgetId -> Int -> IO Bool
textInputMenuActionEnabled ctx wid item = do
  store <- getStore ctx
  let key = intKey wid
      text = IM.findWithDefault "" key (storeText store)
  mclip <- ctxClipboardGet ctx
  let clipTxt = maybe "" id mclip
  pure $
    case item of
      0 -> not (T.null text)
      1 -> not (T.null text)
      2 -> not (T.null clipTxt)
      3 -> not (T.null text)
      _ -> False

processTextInput :: Context -> Input -> TextInputState -> IO TextInputState
processTextInput ctx inp s0 = do
  let mods = inputModifiers inp
      ctrl = modCtrl mods
      shift = modShift mods
      keys = inputKeys inp
      chars = inputChars inp
  s1 <-
    if ctrl
      then T.foldlM' (handleCtrlChar ctx) s0 chars
      else pure s0
  let filtered = T.filter (\ch -> not (isCtrlCombo ctrl ch) && isPrint ch && ch /= '\n') chars
      s2 = T.foldl' insertChar s1 filtered
  pure (foldInputKeys (applyKey shift) s2 keys)
  where
    isCtrlCombo c ch = c && T.elem ch "aAcCxXvV\x01\x03\x16\x18"

handleCtrlChar :: Context -> TextInputState -> Char -> IO TextInputState
handleCtrlChar ctx s ch
  | ch `elem` ('a' : 'A' : '\x01' : []) = pure (selectAllTextInput s)
  | ch `elem` ('c' : 'C' : '\ETX' : []) = textInputCopy ctx s >> pure s
  | ch `elem` ('x' : 'X' : '\x18' : []) = textInputCut ctx s
  | ch `elem` ('v' : 'V' : '\x16' : []) = textInputPaste ctx s
  | otherwise = pure s

insertChar :: TextInputState -> Char -> TextInputState
insertChar s ch =
  let (buf, anc) = toBuffer s
      cur = TB.getCursor buf
      buf' =
        if anc /= cur
          then TB.replaceRange (T.singleton ch) anc cur buf
          else TB.insertChar ch buf
      TB.Cursor _ c = TB.getCursor buf'
   in TextInputState (TB.toText buf') c c

applyKey :: Bool -> TextInputState -> Key -> TextInputState
applyKey shift s key =
  let (buf, anc) = toBuffer s
      cur = TB.getCursor buf
      hasSel = anc /= cur
   in case key of
        KeyBackspace
          | hasSel ->
              let buf' = TB.deleteRange anc cur buf
                  TB.Cursor _ c = TB.getCursor buf'
               in TextInputState (TB.toText buf') c c
          | otherwise ->
              let buf' = TB.deletePrevChar buf
                  TB.Cursor _ c = TB.getCursor buf'
               in TextInputState (TB.toText buf') c c
        KeyDelete
          | hasSel ->
              let buf' = TB.deleteRange anc cur buf
                  TB.Cursor _ c = TB.getCursor buf'
               in TextInputState (TB.toText buf') c c
          | otherwise ->
              let buf' = TB.deleteChar buf
                  TB.Cursor _ c = TB.getCursor buf'
               in TextInputState (TB.toText buf') c c
        KeyLeft -> moveWith shift buf anc TB.moveLeft
        KeyRight -> moveWith shift buf anc TB.moveRight
        KeyHome -> moveWith shift buf anc TB.moveToBOL
        KeyEnd -> moveWith shift buf anc TB.moveToEOL
        _ -> s

moveWith :: Bool -> TB.TextBuffer -> TB.Cursor -> (TB.TextBuffer -> TB.TextBuffer) -> TextInputState
moveWith shift buf anc f =
  let buf' = f buf
      TB.Cursor _ c = TB.getCursor buf'
      a = if shift then TB.cursorCol anc else c
   in TextInputState (TB.toText buf') c a
