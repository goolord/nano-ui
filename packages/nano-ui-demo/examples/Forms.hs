-- | Forms: how do I validate input, disable a button, submit on Enter, and
-- reset a form?
--
-- The whole form is one record in one 'StateCell'. Each widget is passed its
-- field and returns the edited value; the view builds the next record from
-- those values and stores it once. Resetting is then a single write of
-- 'emptyForm': every controlled widget shows whatever it is passed, so the
-- fields empty themselves on the next frame.
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
  deriving (Eq)

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

-- | Explicitly owned state, allocated once before the window opens.
newtype App = App {appForm :: StateCell Form}

newApp :: IO App
newApp = App <$> newState emptyForm

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
  (form, setForm) <- useState (appForm app)
  (welcomed, setWelcomed) <- useFlag False
  let shown rule = if formTried form then rule form else Nothing
  scrollWith (padAll 24 . grow) $
    columnWith (tight . gap 14 . maxW 460 . fillW) $ do
      heading "Create an account"
      (nameR, name) <-
        field "Name" (shown nameError) $
          textInputConfigured' defaultTextInputConfig {ticPlaceholder = "Ada Lovelace"} (formName form)
      (emailR, email) <-
        field "Email" (shown emailError) $
          textInputConfigured' defaultTextInputConfig {ticPlaceholder = "ada@example.com"} (formEmail form)
      -- The range is enforced by the field itself: no minus sign can be
      -- typed, and the value is clamped to 0-130. "13 or older" is a rule
      -- instead, since clamping to 13 would fight someone typing "17".
      (ageR, age) <-
        field "Age" (shown ageError) $
          numericInputConfigured' defaultNumericInputConfig {nicMin = 0, nicMax = 130} (formAge form)
      -- ticPassword masks what is shown and stops copying; the value is
      -- still the plain text.
      let secret = defaultTextInputConfig {ticPassword = True}
      (passwordR, password) <- field "Password" (shown passwordError) (textInputConfigured' secret (formPassword form))
      (confirmR, confirm) <- field "Confirm password" (shown confirmError) (textInputConfigured' secret (formConfirm form))
      country <- field "Country" (shown countryError) (select countries (formCountry form))
      agreed <- checkbox "I agree to the terms" (formAgreed form)
      clicked <-
        rowWith (tight . gap 10 . alignMid . fillW) $ do
          c <- disabledWhen (not agreed) (styled primary (button "Sign up"))
          -- The hint comes and goes, so it sits in a scope.
          scope (unless agreed (muted "Tick the agreement to sign up."))
          pure c
      let entered = any respSubmitted [nameR, emailR, ageR, passwordR, confirmR]
          attempt = clicked || (entered && agreed)
          form' = Form name email age password confirm country agreed (formTried form || attempt)
      setForm form'
      when (attempt && formValid form') (setWelcomed True)
  -- Escape, a click outside, or the close button ask the modal to close.
  (closeR, _) <- modal welcomed "Welcome aboard" $ do
    kv "Name" (formName form)
    kv "Email" (formEmail form)
    kv "Age" (T.pack (show (round (formAge form) :: Int)))
    kv "Country" (countries !! formCountry form)
    rowWith (tight . fillW) $ do
      flex
      -- Declared after the form stored its values, so this write wins.
      whenM (button "Start over") $ do
        setForm emptyForm
        setWelcomed False
  when (respClicked closeR) (setWelcomed False)

-- | A caption, the widget, and the error under it. The error comes and goes,
-- so it sits in a 'scope', which takes one id whether or not it shows.
field :: Text -> Maybe Text -> NanoUI a -> NanoUI a
field caption err widget =
  columnWith (tight . gap 4 . fillW) $ do
    labelWith (tight . fontMuted) caption
    r <- widget
    scope (mapM_ danger err)
    pure r
