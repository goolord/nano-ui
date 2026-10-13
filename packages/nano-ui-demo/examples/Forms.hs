{-# LANGUAGE OverloadedRecordDot #-}

-- | Forms: how do I validate input, disable a button, submit on Enter, and
-- reset a form?
--
-- The whole form is one record in one 'Var'. Each widget gets a Var for its
-- field, @form.formName@, and the user's edits go straight into the record;
-- nothing else needs storing. Resetting is a single write of 'emptyForm':
-- every widget shows what its Var holds, so the fields empty themselves.
--
-- Validation rules are plain functions of the record, shared by the error
-- labels and the submit check. Errors stay hidden until the first submit
-- attempt, then follow the fields live as they are fixed. Showing them while
-- the user is still typing their first answer only nags; showing them after
-- a submit tells them exactly what blocked it.
--
-- 'disabledWhen' greys out the Sign up button until the agreement box is
-- ticked. Enter in any text field submits too ('respSubmitted' on a primed
-- widget's response), and obeys the same rule: 'disabledWhen' only blocks
-- the widgets inside it, so the Enter path checks the box itself.
--
-- Tab and Shift+Tab move between the fields in the order they are declared;
-- nothing needs wiring. Escape closes the welcome dialog, so it does not
-- quit here: close the window to quit. See TodoList.hs for list state kept
-- per row, and Overlays.hs for more about modals.
--
-- Run it with @cabal run nano-ui-example-forms@.
module Main (main) where

import Control.Monad (unless, when)
import Data.Maybe (isNothing)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Data.Text as T
import NanoUI
import NanoUI.Backend.Sdl (SdlOptions (..), defaultSdlOptions, runSdlApp)

data Form = Form
  { formName :: !Text
  , formEmail :: !Text
  , formAge :: !Double -- numericInput works in Double, even for whole numbers
  , formPassword :: !Text
  , formConfirm :: !Text
  , formCountry :: !Int -- index into 'countries'
  , formAgreed :: !Bool
  , formTried :: !Bool -- a submit was attempted, so errors show
  }
  deriving (Eq, Generic)

emptyForm :: Form
emptyForm = Form "" "" 0 "" "" 0 False False

countries :: [Text]
countries = ["Choose a country", "Canada", "France", "Japan", "Kenya", "United States"]

-- | Each rule names the problem, or 'Nothing' when the field is fine.
nameError, emailError, ageError, passwordError, confirmError, countryError :: Form -> Maybe Text
nameError f = problem (T.null (T.strip (formName f))) "Enter your name."
emailError f = problem (not ("@" `T.isInfixOf` formEmail f)) "An email address contains an @."
ageError f = problem (formAge f < 13) "You must be 13 or older."
passwordError f = problem (T.length (formPassword f) < 8) "Use at least 8 characters."
confirmError f = problem (formConfirm f /= formPassword f) "The passwords do not match."
countryError f = problem (formCountry f == 0) "Pick a country."

problem :: Bool -> Text -> Maybe Text
problem bad msg = if bad then Just msg else Nothing

formValid :: Form -> Bool
formValid f = all (isNothing . ($ f)) [nameError, emailError, ageError, passwordError, confirmError, countryError]

-- | Explicitly owned state, made once before the window opens.
newtype App = App {appForm :: Var Form}

newApp :: IO App
newApp = App <$> newVar emptyForm

main :: IO ()
main = do
  app <- newApp
  runSdlApp
    defaultSdlOptions
      { sdlWindowSettings = defaultWindowSettings {wsTitle = "Forms", wsSize = Size 560 720}
      }
    (signUp app)

signUp :: App -> NanoUI ()
signUp app = do
  let form = appForm app
  welcomed <- useVar False
  scrollWith (padAll 24 . grow) $
    columnWith (tight . gap 14 . maxW 460 . fillW) $ do
      heading "Create an account"
      nameR <-
        field form "Name" nameError $
          textInputConfigured' defaultTextInputConfig {ticPlaceholder = "Ada Lovelace"} form.formName
      emailR <-
        field form "Email" emailError $
          textInputConfigured' defaultTextInputConfig {ticPlaceholder = "ada@example.com"} form.formEmail
      -- The range is enforced by the field itself: no minus sign can be
      -- typed, and the value is clamped to 0-130. "13 or older" is a rule
      -- instead, since clamping to 13 would fight someone typing "17".
      ageR <-
        field form "Age" ageError $
          numericInputConfigured' defaultNumericInputConfig {nicMin = 0, nicMax = 130} form.formAge
      -- ticPassword masks what is shown and stops copying; the value is
      -- still the plain text.
      let secret = defaultTextInputConfig {ticPassword = True}
      passwordR <- field form "Password" passwordError (textInputConfigured' secret form.formPassword)
      confirmR <- field form "Confirm password" confirmError (textInputConfigured' secret form.formConfirm)
      field form "Country" countryError (select countries form.formCountry)
      checkbox "I agree to the terms" form.formAgreed
      agreed <- readVar form.formAgreed
      clicked <-
        rowWith (tight . gap 10 . alignMid . fillW) $ do
          c <- disabledWhen (not agreed) (styled primary (button "Sign up"))
          -- The hint comes and goes, so it sits in a scope.
          scope (unless agreed (muted "Tick the agreement to sign up."))
          pure c
      let entered = any respSubmitted [nameR, emailR, ageR, passwordR, confirmR]
      when (clicked || (entered && agreed)) $ do
        writeVar form.formTried True
        valid <- formValid <$> readVar form
        when valid (writeVar welcomed True)
  -- Escape, a click outside, or the close button close the modal.
  modal welcomed "Welcome aboard" $ do
    f <- readVar form
    kv "Name" (formName f)
    kv "Email" (formEmail f)
    kv "Age" (T.pack (show (round (formAge f) :: Int)))
    kv "Country" (countries !! formCountry f)
    rowWith (tight . fillW) $ do
      flex
      whenM (button "Start over") $ do
        writeVar form emptyForm
        writeVar welcomed False

-- | A caption, the widget, and the error under it once a submit was tried.
-- The error comes and goes, so it sits in a 'scope', which takes one id
-- whether or not it shows.
field :: Var Form -> Text -> (Form -> Maybe Text) -> NanoUI a -> NanoUI a
field form caption rule widget =
  columnWith (tight . gap 4 . fillW) $ do
    labelWith (tight . fontMuted) caption
    r <- widget
    f <- readVar form
    scope (mapM_ danger (if formTried f then rule f else Nothing))
    pure r
