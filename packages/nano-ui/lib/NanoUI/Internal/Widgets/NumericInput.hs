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
import Data.Primitive.SmallArray (SmallArray, indexSmallArray, smallArrayFromList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import GHC.Clock (getMonotonicTime)
import NanoUI.Internal.Context (getStore, intKey, requestWakeAt, modifyStore)
import NanoUI.Internal.Equality (sameDouble)
import NanoUI.Internal.Input
import NanoUI.Internal.Layout.Arena (NodeType (..))
import NanoUI.Internal.Monad (NanoUI, askInput, freshWidget, liftIO)
import NanoUI.Internal.Store (deleteSpec, fieldDouble, fieldText, findSlot, findSpec, insertSlot, insertSpec, lookupSlot, memberSpec)
import NanoUI.Internal.Store.Slots (slotNumericHeld, slotNumericRepeat, slotTextChanged, slotTextMode)
import NanoUI.Internal.Style (Layout (..), LayoutModifier, Sizing (..), defaultLayout)
import NanoUI.Internal.Types (Rect (..), clamp, rectContains)
import NanoUI.Internal.WidgetText (numericStepperRects, packTextNodeStyle, textInputFlagNumeric)
import NanoUI.Internal.Widgets.Combinators (widgetBaseLayout)
import NanoUI.Internal.Widgets.Node (Response, addWidgetStyled, respHovered, respRect, setChanged, setSubmitted)
import NanoUI.Widgets.TextEditor (singleLineMode)
import NanoUI.Internal.Widgets.TextInput (TextInputState (..), editTextInput, editorTextState, focusAndIme, loadTextInputState, saveTextEditor, saveTextInputState)
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
    -- times as far. A step is rounded to 'nicDecimals' places, so those must
    -- show it: a step of 0.1 needs at least one decimal.
  , nicDecimals :: !Int
    -- ^ Digits after the decimal point, both shown and accepted (default 0).
  , nicHex :: !Bool
    -- ^ Show and accept the value as a whole hexadecimal number (default
    -- 'False'). 'nicDecimals' is ignored in this mode.
  , nicLayout :: !LayoutModifier
    -- ^ Layout modifier applied on top of the default numeric layout (grows to
    -- fill, at least 80 wide).
  }

-- | Unbounded decimal range, integer display, step 1, and an 80-pixel minimum width.
defaultNumericInputConfig :: NumericInputConfig
defaultNumericInputConfig =
  NumericInputConfig
    { nicMin = -1 / 0
    , nicMax = 1 / 0
    , nicStep = 1
    , nicDecimals = 0
    , nicHex = False
    , nicLayout = id
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

numericBaseLayout :: Layout
numericBaseLayout = defaultLayout {layoutWidth = Grow 1, layoutMinW = 80}

-- | Preformatted decimal strings for common small non-negative integers so
-- idle numeric fields (such as colour-picker channels) allocate nothing.
smallIntTexts :: SmallArray Text
smallIntTexts = smallArrayFromList [T.pack (show i) | i <- [0 .. 512 :: Int]]

-- | 'numericInputConfigured' on a plain value: the response and the value
-- after this frame.
numericInputCore :: NumericInputConfig -> Double -> NanoUI (Response, Double)
numericInputCore cfg value = do
  (wid, ctx) <- freshWidget
  inp <- askInput
  -- Hex input needs letters, so it gets the normal text keyboard.
  isFocus <- focusAndIme ctx wid True (if nicHex cfg then InputNormal else InputNumeric)
  store <- liftIO (getStore ctx)
  let
    !key = intKey wid
    !given = clampNumber cfg value
    stored = lookupSlot fieldText key store
    !held0 = findSpec slotNumericHeld 0 key store
    !repeatAt0 = findSpec slotNumericRepeat 0 key store
  lay <- widgetBaseLayout numericBaseLayout (nicLayout cfg)
  -- Its font face, weight and slant ride in the style bits, as on a label.
  resp <- addWidgetStyled wid NodeTextInput "" 0 lay (textInputFlagNumeric .|. packTextNodeStyle lay 0)
  if not isFocus
    && held0 == 0
    && repeatAt0 == 0
    && not (pressedIn MouseLeft inp)
    && lookupSlot fieldDouble key store == Just given
    && not (memberSpec slotTextChanged key store)
    && stored == Just (formatNumber cfg given)
    then pure (setChanged (given /= value) resp, given)
    else do
      let
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
            || memberSpec slotTextChanged key store
      when dirty $
        liftIO $ do
          -- An accepted edit keeps its undo history; a rejected one or a step
          -- rewrites the text without it.
          let save = case mEdited of
                Just ed | editorTextState ed == s2 -> saveTextEditor key ed
                _ -> saveTextInputState key s2
          -- Commands sent between frames read the mode ('runTextCommand'); the
          -- change pulse an edit of theirs leaves is reported as the value's
          -- change.
          modifyStore ctx $
            (if held1 == 0 then deleteSpec slotNumericHeld key else insertSpec slotNumericHeld key held1)
              . insertSlot fieldDouble key final
              . insertSpec slotNumericRepeat key repeatAt1
              . insertSpec slotTextMode key singleLineMode
              . deleteSpec slotTextChanged key
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
  let step
        | pressDir /= 0 = (pressDir, pressDir, now + 0.4)
        | holding && now >= repeatAt0 = (held0, held0, now + 0.06)
        | holding = (keyDir, held0, repeatAt0)
        | otherwise = (keyDir, 0, 0)
  pure step

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
  | nicDecimals cfg <= 0 =
      let n = round v :: Integer
       in if n >= 0 && n <= 512
            then indexSmallArray smallIntTexts (fromInteger n)
            else T.pack (show n)
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
  | otherwise = case TR.signed (TR.rational . leadingZero) (T.dropWhileEnd (== '.') t) of
      Right (v, rest) | T.null rest -> Just v
      _ -> Nothing
  where
    -- 'TR.rational' needs a digit before the point, which ".5" leaves out.
    leadingZero s = if "." `T.isPrefixOf` s then "0" <> s else s
