-- | Numeric field: a text field that only accepts numbers, with an up / down
-- stepper, arrow-key steps, and an optional hexadecimal mode.
module NanoUI.Internal.Widgets.NumericInput
  ( NumericInputConfig (..)
  , defaultNumericInputConfig
  , numericInput
  , numericInput'
  , numericInputConfigured
  , numericInputConfigured'
  )
where

import Control.Monad (when)
import Data.Char (isDigit, isHexDigit)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Effectful (Eff, type (:>))
import GHC.Clock (getMonotonicTime)
import NanoUI.Internal.Context (getStore, intKey, registerFocusable, requestWakeAt, modifyStore)
import NanoUI.Internal.Input (Key (..), inputKeys, inputKeysElem, inputModifiers, inputMouseDown, inputMousePos, inputMousePressed, modShift)
import NanoUI.Internal.Layout.Arena (NodeType (..))
import NanoUI.Internal.Monad (Ui, askContext, askInput, nextId, uiIO)
import NanoUI.Internal.Store (Slot (..), deleteSlot, fieldDouble, fieldInt, fieldText, findSlot, insertSlot, lookupSlot, slotKey)
import NanoUI.Internal.Style (Layout (..), Sizing (..), defaultLayout)
import NanoUI.Internal.Types (Rect (..), clamp, rectContains)
import NanoUI.Internal.WidgetText (numericStepperRects, textInputFlagNumeric)
import NanoUI.Internal.Widgets.Behavior (keyboardFocused)
import NanoUI.Internal.Widgets.Node (Response, addWidgetStyled, respHovered, respRect, setChanged, setSubmitted)
import NanoUI.Widgets.TextEditor (singleLineMode)
import NanoUI.Internal.Widgets.TextInput (TextInputState (..), editTextInput, editorTextState, loadTextInputState, saveTextEditor, saveTextInputState)
import Numeric (showFFloat, showHex)

-- | How a numeric field reads, shows, and steps its value.
data NumericInputConfig = NumericInputConfig
  { nicMin :: !Double
    -- ^ Smallest value (default: no limit).
  , nicMax :: !Double
    -- ^ Largest value (default: no limit).
  , nicStep :: !Double
    -- ^ What one arrow key or stepper click adds (default 1). Shift steps ten
    -- times as far.
  , nicDecimals :: !Int
    -- ^ Digits after the decimal point, both shown and accepted (default 0).
  , nicHex :: !Bool
    -- ^ Show and accept the value as a whole hexadecimal number (default
    -- 'False'). 'nicDecimals' is ignored in this mode.
  , nicLayout :: !Layout
  }
  deriving (Eq, Show)

-- | Unbounded decimal range, integer display, step 1, and an 80-pixel minimum width.
defaultNumericInputConfig :: NumericInputConfig
defaultNumericInputConfig =
  NumericInputConfig
    { nicMin = -1 / 0
    , nicMax = 1 / 0
    , nicStep = 1
    , nicDecimals = 0
    , nicHex = False
    , nicLayout = defaultLayout {layoutWidth = Grow 1, layoutMinW = 80}
    }

-- | Numeric field over whole numbers. Pass the current value; the result is
-- the value after this frame's typing, arrow keys, and stepper clicks.
--
-- Only digits, and a leading minus sign when the range reaches below zero, can
-- be typed. Up and Down step the value, Shift steps ten times as far, and
-- holding a stepper arrow repeats. Enter, a step, or leaving the field rewrites
-- the text as the value, clamped to the range.
{-# INLINE numericInput #-}
numericInput :: Ui :> es => Double -> Eff es Double
numericInput value = snd <$> numericInputConfigured' defaultNumericInputConfig value

-- | 'numericInput' returning @(response, updatedValue)@.
{-# INLINE numericInput' #-}
numericInput' :: Ui :> es => Double -> Eff es (Response, Double)
numericInput' = numericInputConfigured' defaultNumericInputConfig

-- | 'numericInput' with a range, a step, decimal places, hexadecimal mode, or
-- its own layout.
--
-- > byte' <- numericInputConfigured defaultNumericInputConfig {nicMin = 0, nicMax = 255, nicHex = True} byte
{-# INLINE numericInputConfigured #-}
numericInputConfigured :: Ui :> es => NumericInputConfig -> Double -> Eff es Double
numericInputConfigured cfg value = snd <$> numericInputConfigured' cfg value

-- | 'numericInputConfigured' returning @(response, updatedValue)@.
numericInputConfigured' :: Ui :> es => NumericInputConfig -> Double -> Eff es (Response, Double)
numericInputConfigured' cfg value = do
  wid <- nextId
  ctx <- askContext
  inp <- askInput
  uiIO $ registerFocusable ctx wid
  store <- uiIO (getStore ctx)
  isFocus <- keyboardFocused wid
  let
    key = intKey wid
    given = clampNumber cfg value
    stored = lookupSlot fieldText key store
    -- Unfocused, the field shows the caller's value; focused, it keeps the
    -- text being typed.
    text0 = if isFocus then fromMaybe (formatNumber cfg given) stored else formatNumber cfg given
    s0 = loadTextInputState store key text0
    lastValue = findSlot fieldDouble given key store
  mEdited <- if isFocus then uiIO (editTextInput ctx singleLineMode inp store key s0) else pure Nothing
  resp <- addWidgetStyled wid NodeTextInput "" 0 (nicLayout cfg) textInputFlagNumeric
  let
    -- An edit that would leave text no number can start with is dropped.
    typed = maybe s0 editorTextState mEdited
    s1 = if acceptsNumberText cfg (tisText typed) then typed else s0
    current
      | isFocus = maybe lastValue (clampNumber cfg) (parseNumber cfg (tisText s1))
      | otherwise = given
    Rect rx ry rw rh = respRect resp
    (upRect, downRect) = numericStepperRects rx ry rw rh
    mouse = inputMousePos inp
    keys = inputKeys inp
    over r dir = if respHovered resp && rectContains r mouse then dir else 0
    pressDir
      | inputMousePressed inp = over upRect 1 + over downRect (-1)
      | otherwise = 0 :: Int
    heldK = slotKey SlotNumericHeld key
    repeatK = slotKey SlotNumericRepeat key
    held0 = findSlot fieldInt 0 heldK store
    holding =
      held0 /= 0
        && inputMouseDown inp
        && over (if held0 > 0 then upRect else downRect) held0 /= 0
    keyDir
      | not isFocus = 0
      | inputKeysElem KeyUp keys = 1
      | inputKeysElem KeyDown keys = -1
      | otherwise = 0
  now <- if pressDir /= 0 || holding then uiIO getMonotonicTime else pure 0
  let
    repeatAt0 = findSlot fieldDouble 0 repeatK store
    -- A held arrow repeats after a pause.
    repeatDir = if pressDir == 0 && holding && now >= repeatAt0 then held0 else 0
    dir
      | pressDir /= 0 = pressDir
      | repeatDir /= 0 = repeatDir
      | otherwise = keyDir
    held1
      | pressDir /= 0 = pressDir
      | holding = held0
      | otherwise = 0
    repeatAt1
      | pressDir /= 0 = now + 0.4
      | repeatDir /= 0 = now + 0.06
      | held1 == 0 = 0
      | otherwise = repeatAt0
    scale = if modShift (inputModifiers inp) then 10 else 1
    final
      | dir /= 0 = clampNumber cfg (roundNumber cfg (current + fromIntegral dir * scale * nicStep cfg))
      | otherwise = current
    submitted = isFocus && inputKeysElem KeyEnter keys
    -- A step or Enter rewrites the text as the value, caret at its end.
    s2
      | dir /= 0 || submitted =
          let t = formatNumber cfg final
           in TextInputState t (T.length t) (T.length t)
      | otherwise = s1
    dirty =
      stored /= Just (tisText s2)
        || s2 /= s0
        || lookupSlot fieldDouble key store /= Just final
        || held1 /= held0
        || repeatAt1 /= repeatAt0
  when dirty $
    uiIO $ do
      -- An accepted edit keeps its undo history; a rejected one or a step
      -- rewrites the text without it.
      let save = case mEdited of
            Just ed | editorTextState ed == s2 -> saveTextEditor key ed
            _ -> saveTextInputState key s2
      modifyStore ctx $
        (if held1 == 0 then deleteSlot fieldInt heldK else insertSlot fieldInt heldK held1)
          . insertSlot fieldDouble key final
          . insertSlot fieldDouble repeatK repeatAt1
          . save
  -- A held arrow repeats on a schedule: ask for the frame of its next step
  -- instead of running frames back to back until then.
  when (held1 /= 0) $ uiIO (requestWakeAt ctx repeatAt1)
  pure (setSubmitted submitted (setChanged (final /= value) resp), final)

clampNumber :: NumericInputConfig -> Double -> Double
clampNumber cfg = clamp (nicMin cfg) (nicMax cfg)

-- | The value rounded to what the field shows.
roundNumber :: NumericInputConfig -> Double -> Double
roundNumber cfg v
  | nicHex cfg || nicDecimals cfg <= 0 = fromInteger (round v)
  | otherwise = fromInteger (round (v * scale)) / scale
  where
    scale = 10 ^ nicDecimals cfg

formatNumber :: NumericInputConfig -> Double -> Text
formatNumber cfg v
  | nicHex cfg =
      let n = round v :: Integer
       in (if n < 0 then "-" else "") <> T.toUpper (T.pack (showHex (abs n) ""))
  | nicDecimals cfg <= 0 = T.pack (show (round v :: Integer))
  | otherwise = T.pack (showFFloat (Just (nicDecimals cfg)) v "")

-- | Whether @t@ can stand in the field mid-edit: an optional minus sign when
-- the range reaches below zero, then digits (hexadecimal ones in hex mode),
-- and with decimal places, one point followed by at most that many digits.
acceptsNumberText :: NumericInputConfig -> Text -> Bool
acceptsNumberText cfg t =
  signOk && case T.splitOn "." body of
    [whole] -> T.all digit whole
    [whole, frac] -> decimals > 0 && T.all isDigit whole && T.all isDigit frac && T.length frac <= decimals
    _ -> False
  where
    (negative, body) = maybe (False, t) ((,) True) (T.stripPrefix "-" t)
    signOk = not negative || nicMin cfg < 0
    digit = if nicHex cfg then isHexDigit else isDigit
    decimals = if nicHex cfg then 0 else nicDecimals cfg

-- | The value the text reads as, once it reads as one.
parseNumber :: NumericInputConfig -> Text -> Maybe Double
parseNumber cfg t
  | nicHex cfg = case TR.signed TR.hexadecimal t of
      Right (n, rest) | T.null rest -> Just (fromInteger n)
      _ -> Nothing
  | otherwise = case TR.signed TR.rational (T.dropWhileEnd (== '.') t) of
      Right (v, rest) | T.null rest -> Just v
      _ -> Nothing
