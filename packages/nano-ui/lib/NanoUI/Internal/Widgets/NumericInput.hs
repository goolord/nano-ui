-- | Numeric field: a text field that only accepts numbers, with an up / down
-- stepper, arrow-key steps, and an optional hexadecimal mode.
module NanoUI.Internal.Widgets.NumericInput
  ( NumericInputConfig (..)
  , defaultNumericInputConfig
  , numericInput
  , numericInput'
  , numericInputConfigured
  , numericInputConfigured'
  , numericInputCore
  )
where

import Control.Monad (mfilter, void, when)
import Data.Bits ((.|.))
import Data.Char (isDigit, isHexDigit)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import GHC.Clock (getMonotonicTime)
import NanoUI.Internal.Context (getStore, intKey, registerFocusable, requestInputMethod, requestWakeAt, modifyStore)
import NanoUI.Internal.Equality (sameDouble)
import NanoUI.Internal.Input
import NanoUI.Internal.Layout.Arena (NodeType (..))
import NanoUI.Internal.Monad (NanoUI, askInput, freshWidget, liftIO)
import NanoUI.Internal.Store (Slot (..), deleteSlot, fieldDouble, fieldInt, fieldText, findSlot, insertSlot, lookupSlot, slotKey)
import NanoUI.Internal.Style (Layout (..), Sizing (..), defaultLayout)
import NanoUI.Internal.Types (Rect (..), clamp, rectContains)
import NanoUI.Internal.WidgetText (numericStepperRects, packTextNodeStyle, textInputFlagNumeric)
import NanoUI.Internal.Widgets.Behavior (keyboardFocused)
import NanoUI.Internal.Widgets.Node (Response, addWidgetStyled, respHovered, respRect, setChanged, setSubmitted)
import NanoUI.Widgets.TextEditor (singleLineMode)
import NanoUI.Internal.Widgets.TextInput (TextInputState (..), editTextInput, editorTextState, loadTextInputState, saveTextEditor, saveTextInputState)
import NanoUI.Internal.Var (Binding (..), bindVarBy)
import NanoUI.Internal.Var.Types (Var)
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

-- | Numeric field over whole numbers bound to mutable state ('Var'): typing,
-- arrow keys and stepper clicks mutate it in place.
--
-- Only digits, and a leading minus sign when the range reaches below zero, can
-- be typed. Up and Down step the value, Shift steps ten times as far, and
-- holding a stepper arrow repeats. Enter, a step, or leaving the field rewrites
-- the text as the value, clamped to the range. A value written to the Var
-- while the field is focused replaces the text being typed.
{-# INLINE numericInput #-}
numericInput :: Var Double -> NanoUI ()
numericInput v = void (numericInputConfigured' defaultNumericInputConfig v)

-- | 'numericInput' returning its 'Response'.
{-# INLINE numericInput' #-}
numericInput' :: Var Double -> NanoUI Response
numericInput' = numericInputConfigured' defaultNumericInputConfig

-- | 'numericInput' with a range, a step, decimal places, hexadecimal mode, or
-- its own layout.
--
-- > numericInputConfigured defaultNumericInputConfig {nicMin = 0, nicMax = 255, nicHex = True} byte
{-# INLINE numericInputConfigured #-}
numericInputConfigured :: NumericInputConfig -> Var Double -> NanoUI ()
numericInputConfigured cfg v = void (numericInputConfigured' cfg v)

-- | 'numericInputConfigured' returning its 'Response'.
{-# INLINE numericInputConfigured' #-}
numericInputConfigured' :: NumericInputConfig -> Var Double -> NanoUI Response
numericInputConfigured' cfg v = bindVarBy sameDouble RenderFinal v (numericInputCore cfg)

-- | 'numericInputConfigured' on a plain value: the response and the value
-- after this frame.
numericInputCore :: NumericInputConfig -> Double -> NanoUI (Response, Double)
numericInputCore cfg value = do
  (wid, ctx) <- freshWidget
  inp <- askInput
  liftIO $ registerFocusable ctx wid
  store <- liftIO (getStore ctx)
  isFocus <- keyboardFocused wid
  -- Hex input needs letters, so it gets the normal text keyboard.
  when isFocus $ liftIO (requestInputMethod ctx wid Nothing (if nicHex cfg then InputNormal else InputNumeric))
  let
    key = intKey wid
    given = clampNumber cfg value
    stored = lookupSlot fieldText key store
    -- A value other than the last one the field returned was set by the
    -- caller, and replaces the text even while the field is focused.
    adopted = isFocus && maybe False (/= given) (lookupSlot fieldDouble key store)
    -- Unfocused, the field shows the caller's value; focused, it keeps the
    -- text being typed.
    -- Stored text that fails 'acceptsNumberText' (pasted from the context
    -- menu, which skips the filter) is replaced so subsequent edits are not
    -- rejected.
    text0 = if isFocus && not adopted then fromMaybe (formatNumber cfg given) (mfilter (acceptsNumberText cfg) stored) else formatNumber cfg given
    s0 = loadTextInputState store key text0
    lastValue = if adopted then given else findSlot fieldDouble given key store
  mEdited <- if isFocus then liftIO (editTextInput ctx singleLineMode inp store key s0) else pure Nothing
  -- Its font face, weight and slant ride in the style bits, as on a label.
  resp <- addWidgetStyled wid NodeTextInput "" 0 (nicLayout cfg) (textInputFlagNumeric .|. packTextNodeStyle (nicLayout cfg) 0)

  let
    -- An edit that would leave text no number can start with is dropped.
    typed = maybe s0 editorTextState mEdited
    s1 = if acceptsNumberText cfg (tisText typed) then typed else s0
    current
      -- Text that still reads as the last value formatted is that value,
      -- which can hold more precision than the text shows.
      | isFocus && tisText s1 == formatNumber cfg lastValue = lastValue
      | isFocus = maybe lastValue (clampNumber cfg) (parseNumber cfg (tisText s1))
      | otherwise = given
    heldK = slotKey SlotNumericHeld key
    repeatK = slotKey SlotNumericRepeat key
    held0 = findSlot fieldInt 0 heldK store
    repeatAt0 = findSlot fieldDouble 0 repeatK store
  (dir, held1, repeatAt1) <- liftIO (stepNumericStepper resp isFocus inp held0 repeatAt0)
  let
    scale = if modShift (inputModifiers inp) then 10 else 1
    final
      | dir /= 0 = clampNumber cfg (roundNumber cfg (current + fromIntegral dir * scale * nicStep cfg))
      | otherwise = current
    submitted = isFocus && pressedOnceIn KeyEnter inp
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
    liftIO $ do
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
  when (held1 /= 0) $ liftIO (requestWakeAt ctx repeatAt1)
  pure (setSubmitted submitted (setChanged (final /= value) resp), final)

-- | Step the stepper arrows' press, auto-repeat timer, and Up/Down keys,
-- returning @(stepDir, heldDir, nextRepeatAt)@.
stepNumericStepper :: Response -> Bool -> Input -> Int -> Double -> IO (Int, Int, Double)
stepNumericStepper resp isFocus inp held0 repeatAt0 = do
  let Rect rx ry rw rh = respRect resp
      (upRect, downRect) = numericStepperRects rx ry rw rh
      mouse = inputMousePos inp
      over r d = if respHovered resp && rectContains r mouse then d else 0
      pressDir
        | pressedIn MouseLeft inp = over upRect 1 + over downRect (-1)
        | otherwise = 0
      holding =
        held0 /= 0
          && heldIn MouseLeft inp
          && over (if held0 > 0 then upRect else downRect) held0 /= 0
      keyDir
        | not isFocus = 0
        | pressedIn KeyUp inp = 1
        | pressedIn KeyDown inp = -1
        | otherwise = 0
  now <- if pressDir /= 0 || holding then getMonotonicTime else pure 0
  let repeatDir = if pressDir == 0 && holding && now >= repeatAt0 then held0 else 0
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
  pure (dir, held1, repeatAt1)

clampNumber :: NumericInputConfig -> Double -> Double
clampNumber cfg = clamp (nicMin cfg) (nicMax cfg)

roundNumber :: NumericInputConfig -> Double -> Double
roundNumber cfg v
  | nicHex cfg || nicDecimals cfg <= 0 = fromInteger (round v)
  | otherwise = fromInteger (round (v * scale)) / scale
  where
    scale = 10 ^ nicDecimals cfg

formatNumber :: NumericInputConfig -> Double -> Text
formatNumber cfg v0
  -- NaN and infinity format as empty text because their string representations
  -- fail 'acceptsNumberText'.
  | isNaN v0 || isInfinite v0 = T.empty
  | nicHex cfg =
      let n = round v :: Integer
       in (if n < 0 then "-" else "") <> T.toUpper (T.pack (showHex (abs n) ""))
  | nicDecimals cfg <= 0 = T.pack (show (round v :: Integer))
  | otherwise = T.pack (showFFloat (Just (nicDecimals cfg)) v "")
  where
    -- Normalize -0.0 to +0.0 so a non-negative field does not format "-0.00"
    -- and reject it.
    v = v0 + 0

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

parseNumber :: NumericInputConfig -> Text -> Maybe Double
parseNumber cfg t
  | nicHex cfg = case TR.signed TR.hexadecimal t of
      Right (n, rest) | T.null rest -> Just (fromInteger n)
      _ -> Nothing
  | otherwise = case TR.signed TR.rational (T.dropWhileEnd (== '.') t) of
      Right (v, rest) | T.null rest -> Just v
      _ -> Nothing
