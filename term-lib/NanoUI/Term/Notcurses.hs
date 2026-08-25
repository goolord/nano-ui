-- | Thin notcurses-core wrapper for terminal init, render, and input.
module NanoUI.Term.Notcurses
  ( Notcurses
  , NotcursesCtx (..)
  , withNotcurses
  , ncSize
  , ncRead
  , ncBlitCells
  ) where

import Control.Exception (bracket)
import Control.Monad (when)
import Data.Bits ((.&.))
import Data.Char (chr, isValidCodePoint)
import Data.Maybe (maybeToList)
import Data.Primitive.PrimArray (copyPrimArrayToPtr, sizeofPrimArray)
import Data.Word (Word32)
import Foreign.C.Types (CInt (..), CSize (..), CUInt (..))
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr_, withForeignPtr)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import NanoUI (Key (..), Modifiers (..))
import NanoUI.Term.Cells (Cells, cellsData, cellsH, cellsW)
import NanoUI.Term.Event
  ( MouseAction (..)
  , MouseBtn (..)
  , TermEvent (..)
  )

data Notcurses

newtype NotcursesCtx = NotcursesCtx (ForeignPtr Notcurses)

withNotcurses :: (NotcursesCtx -> IO a) -> IO a
withNotcurses act =
  bracket initNC finiNC $ \nc -> do
    r <- withForeignPtr nc c_mouse_enable
    when (r /= 0) $ fail "notcurses_mice_enable failed"
    act (NotcursesCtx nc)
  where
    initNC = do
      p <- c_init
      when (p == nullPtr) $ fail "notcurses init failed"
      newForeignPtr_ p
    finiNC fp = withForeignPtr fp c_fini

ncSize :: NotcursesCtx -> IO (Int, Int)
ncSize (NotcursesCtx fp) =
  withForeignPtr fp $ \p ->
    alloca $ \rows ->
      alloca $ \cols -> do
        r <- c_dim p rows cols
        when (r /= 0) $ fail "notcurses dim failed"
        rowCount <- peek rows
        colCount <- peek cols
        pure (fromIntegral colCount, fromIntegral rowCount)

ncBlitCells :: NotcursesCtx -> Maybe Cells -> Cells -> IO ()
ncBlitCells (NotcursesCtx fp) mPrev cells =
  let arr = cellsData cells
      len = sizeofPrimArray arr
      w = cellsW cells
      h = cellsH cells
      expected = w * h * 3
   in if expected <= 0 || len /= expected
        then fail "notcurses blit: cell buffer size mismatch"
        else withPrev mPrev $ \mPrevPtr prevW prevH ->
          allocaArray len $ \ptr -> do
            copyPrimArrayToPtr ptr arr 0 len
            withForeignPtr fp $ \p -> do
              r <- c_blit_cells p (fromIntegral w) (fromIntegral h) ptr mPrevPtr prevW prevH
              when (r /= 0) $ fail "notcurses blit failed"
  where
    withPrev Nothing k = k nullPtr 0 0
    withPrev (Just prev) k =
      let prevArr = cellsData prev
          prevLen = sizeofPrimArray prevArr
          prevExpected = cellsW prev * cellsH prev * 3
       in if prevExpected <= 0 || prevLen /= prevExpected
            then fail "notcurses blit: previous cell buffer size mismatch"
            else allocaArray prevLen $ \prevPtr -> do
              copyPrimArrayToPtr prevPtr prevArr 0 prevLen
              k prevPtr (fromIntegral (cellsW prev)) (fromIntegral (cellsH prev))

ncRead :: NotcursesCtx -> Int -> IO [TermEvent]
ncRead ctx timeoutMs = go 0
  where
    go retries =
      withForeignPtr (ctxPtr ctx) $ \p ->
        withNcInput $ \nip -> do
          r <- c_get p (fromIntegral timeoutMs) nip
          if r == 0
            then pure []
            else if r == maxBound
              then
                if retries < 3
                  then go (retries + 1)
                  else fail "notcurses_get failed"
              else eventFromInput p nip

ctxPtr :: NotcursesCtx -> ForeignPtr Notcurses
ctxPtr (NotcursesCtx fp) = fp

withNcInput :: (Ptr () -> IO a) -> IO a
withNcInput k = do
  sz <- c_ncinput_size
  allocaBytes (fromIntegral sz) $ \p -> k p

eventFromInput :: Ptr Notcurses -> Ptr () -> IO [TermEvent]
eventFromInput p nip = do
  evId <- c_input_id nip
  y <- c_input_y nip
  x <- c_input_x nip
  mods <- c_input_modifiers nip
  evtype <- c_input_evtype nip
  if evId == nckeyResize
    then resizeEvent p
    else pure (maybeToList (mapEvent evId y x mods evtype))

resizeEvent :: Ptr Notcurses -> IO [TermEvent]
resizeEvent p =
  alloca $ \rows ->
    alloca $ \cols -> do
      r <- c_dim p rows cols
      when (r /= 0) $ fail "notcurses dim failed"
      rowCount <- peek rows
      colCount <- peek cols
      pure [EvResize (fromIntegral colCount, fromIntegral rowCount)]

mapEvent :: Word32 -> CInt -> CInt -> CUInt -> CInt -> Maybe TermEvent
mapEvent evId y x mods evtype
  | evId >= nckeyMotion && evId <= nckeyButton11 =
      mouseEvent evId (fromIntegral x) (fromIntegral y) mods evtype
  | evId == nckeyInvalid || evId == nckeySignal = Nothing
  | evId == nckeyTab = Just (EvKey KeyTab (toMods mods))
  | synthesized evId = keyEvent evId mods
  | otherwise = charEvent evId mods

charEvent :: Word32 -> CUInt -> Maybe TermEvent
charEvent evId mods
  | evId == nckeyTab = Just (EvKey KeyTab (toMods mods))
  | otherwise =
      case codePointToChar evId of
        Nothing -> Nothing
        Just c -> Just (EvChar c (toMods mods))

codePointToChar :: Word32 -> Maybe Char
codePointToChar w =
  let i = fromIntegral w
   in if i >= 0 && isValidCodePoint i then Just (chr i) else Nothing

mouseEvent :: Word32 -> Int -> Int -> CUInt -> CInt -> Maybe TermEvent
mouseEvent evId col row mods evtype =
  let m = toMods mods
   in case evId of
        k | k == nckeyMotion -> Just (EvMouse MouseMove col row m)
        k | k == nckeyButton4 -> Just (EvMouse MouseScrollUp col row m)
        k | k == nckeyButton5 -> Just (EvMouse MouseScrollDown col row m)
        k | k == nckeyButton6 -> Just (EvMouse MouseScrollLeft col row m)
        k | k == nckeyButton7 -> Just (EvMouse MouseScrollRight col row m)
        k | k >= nckeyButton1 && k <= nckeyButton3 ->
            let btn = case k - nckeyButton1 of
                  0 -> BtnLeft
                  1 -> BtnMiddle
                  _ -> BtnRight
             in case evtype of
                  n | n == nctypePress -> Just (EvMouse (MousePress btn) col row m)
                  n | n == nctypeRelease -> Just (EvMouse (MouseRelease (Just btn)) col row m)
                  _ -> Just (EvMouse (MouseDrag btn) col row m)
        _ -> Nothing

keyEvent :: Word32 -> CUInt -> Maybe TermEvent
keyEvent evId mods =
  let m = toMods mods
   in case evId of
        k | k == nckeyUp -> Just (EvKey KeyUp m)
        k | k == nckeyDown -> Just (EvKey KeyDown m)
        k | k == nckeyLeft -> Just (EvKey KeyLeft m)
        k | k == nckeyRight -> Just (EvKey KeyRight m)
        k | k == nckeyHome -> Just (EvKey KeyHome m)
        k | k == nckeyEnd -> Just (EvKey KeyEnd m)
        k | k == nckeyBackspace -> Just (EvKey KeyBackspace m)
        k | k == nckeyDel -> Just (EvKey KeyDelete m)
        k | k == nckeyEnter -> Just (EvKey KeyEnter m)
        k | k == nckeyTab -> Just (EvKey KeyTab m)
        k | k == nckeyEsc -> Just (EvKey KeyEscape m)
        _ -> Nothing

toMods :: CUInt -> Modifiers
toMods w =
  Modifiers
    { modShift = w .&. modShiftMask /= 0
    , modCtrl = w .&. modCtrlMask /= 0
    , modAlt = w .&. modAltMask /= 0
    }

synthesized :: Word32 -> Bool
synthesized w = w >= preterUnicodeBase && w <= nckeyEOF

preterUnicodeBase :: Word32
preterUnicodeBase = 1115000

nckeyInvalid :: Word32
nckeyInvalid = preterUnicodeBase + 0

nckeyResize :: Word32
nckeyResize = preterUnicodeBase + 1

nckeyUp :: Word32
nckeyUp = preterUnicodeBase + 2

nckeyDown :: Word32
nckeyDown = preterUnicodeBase + 4

nckeyLeft :: Word32
nckeyLeft = preterUnicodeBase + 5

nckeyRight :: Word32
nckeyRight = preterUnicodeBase + 3

nckeyBackspace :: Word32
nckeyBackspace = preterUnicodeBase + 8

nckeyDel :: Word32
nckeyDel = preterUnicodeBase + 7

nckeyHome :: Word32
nckeyHome = preterUnicodeBase + 11

nckeyEnd :: Word32
nckeyEnd = preterUnicodeBase + 12

nckeyEnter :: Word32
nckeyEnter = preterUnicodeBase + 121

nckeyTab :: Word32
nckeyTab = 0x09

nckeyEsc :: Word32
nckeyEsc = 0x1b

nckeyMotion :: Word32
nckeyMotion = preterUnicodeBase + 200

nckeyButton1 :: Word32
nckeyButton1 = preterUnicodeBase + 201

nckeyButton3 :: Word32
nckeyButton3 = preterUnicodeBase + 203

nckeyButton4 :: Word32
nckeyButton4 = preterUnicodeBase + 204

nckeyButton5 :: Word32
nckeyButton5 = preterUnicodeBase + 205

nckeyButton6 :: Word32
nckeyButton6 = preterUnicodeBase + 206

nckeyButton7 :: Word32
nckeyButton7 = preterUnicodeBase + 207

nckeyButton11 :: Word32
nckeyButton11 = preterUnicodeBase + 211

nckeySignal :: Word32
nckeySignal = preterUnicodeBase + 400

nckeyEOF :: Word32
nckeyEOF = preterUnicodeBase + 500

nctypePress :: CInt
nctypePress = 1

nctypeRelease :: CInt
nctypeRelease = 3

modShiftMask :: CUInt
modShiftMask = 1

modAltMask :: CUInt
modAltMask = 2

modCtrlMask :: CUInt
modCtrlMask = 4

foreign import ccall unsafe "nano_ui_nc_init"
  c_init :: IO (Ptr Notcurses)

foreign import ccall unsafe "nano_ui_nc_fini"
  c_fini :: Ptr Notcurses -> IO ()

foreign import ccall unsafe "nano_ui_nc_dim"
  c_dim :: Ptr Notcurses -> Ptr CUInt -> Ptr CUInt -> IO CInt

foreign import ccall unsafe "nano_ui_nc_mouse_enable"
  c_mouse_enable :: Ptr Notcurses -> IO CInt

foreign import ccall unsafe "nano_ui_nc_blit_cells"
  c_blit_cells ::
    Ptr Notcurses ->
    CInt ->
    CInt ->
    Ptr Word32 ->
    Ptr Word32 ->
    CInt ->
    CInt ->
    IO CInt

foreign import ccall unsafe "nano_ui_nc_get"
  c_get :: Ptr Notcurses -> CInt -> Ptr () -> IO Word32

foreign import ccall unsafe "nano_ui_ncinput_size"
  c_ncinput_size :: IO CSize

foreign import ccall unsafe "nano_ui_ncinput_id"
  c_input_id :: Ptr () -> IO Word32

foreign import ccall unsafe "nano_ui_ncinput_y"
  c_input_y :: Ptr () -> IO CInt

foreign import ccall unsafe "nano_ui_ncinput_x"
  c_input_x :: Ptr () -> IO CInt

foreign import ccall unsafe "nano_ui_ncinput_modifiers"
  c_input_modifiers :: Ptr () -> IO CUInt

foreign import ccall unsafe "nano_ui_ncinput_evtype"
  c_input_evtype :: Ptr () -> IO CInt

maybeToList :: Maybe a -> [a]
maybeToList = maybe [] pure
