{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM_, void, when)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Ditto.Types as Ditto
import NanoUI
  ( Color
  , Input (..)
  , Key (KeyEscape)
  , NanoUI
  , Size (..)
  , button
  , card
  , colorPickerToHex
  , colorRGBA
  , columnWith
  , danger
  , fillW
  , flex
  , fontMono
  , gap
  , grow
  , heading
  , inputKeys
  , inputKeysElem
  , kv
  , labelWith
  , maxW
  , minW
  , muted
  , padAll
  , respClicked
  , rowWith
  , scrollWith
  , sep
  , tight
  , toolbar
  , useText
  )
import NanoUI.Backend.Sdl
  ( SdlOptions (..)
  , defaultSdlOptions
  , runSdlApp
  )
import NanoUI.Form
  ( Form
  , FormView (..)
  , inRange
  , inputCheckbox
  , inputColor
  , inputEnumSelect
  , inputPassword
  , inputSlider
  , inputTextArea
  , inputTextWithPlaceholder
  , maxLength
  , minLength
  , notEmpty
  , prove
  , resetForm
  , runNanoForm
  , validEmail
  , withFieldErrors
  )

-- | User plan tiers demonstrating enum dropdowns.
data AccountTier = Starter | Developer | Professional | Enterprise
  deriving (Eq, Show, Bounded, Enum)

-- | Registration record demonstrating multiple typed inputs.
data Registration = Registration
  { regUsername   :: !Text
  , regEmail      :: !Text
  , regPassword   :: !Text
  , regAge        :: !Float
  , regTier       :: !AccountTier
  , regThemeColor :: !Color
  , regSubscribe  :: !Bool
  , regBio        :: !Text
  } deriving (Eq, Show)

-- | Registration form definition composed using applicative formlets.
registrationForm :: Form Text Registration
registrationForm =
  Registration
    <$> withFieldErrors
          (inputTextWithPlaceholder "e.g. adalovelace" "Username" "Ada"
            `prove` notEmpty "Username is required"
            `prove` minLength 3 (const "Must be at least 3 characters")
            `prove` maxLength 20 (const "Must be 20 characters or fewer"))
    <*> withFieldErrors
          (inputTextWithPlaceholder "e.g. ada@example.com" "Email" "ada@example.com"
            `prove` notEmpty "Email address is required"
            `prove` validEmail (const "Invalid email address format (e.g. name@domain.com)"))
    <*> withFieldErrors
          (inputPassword "Password" "correcthorse"
            `prove` notEmpty "Password is required"
            `prove` minLength 8 (const "Password must be at least 8 characters long"))
    <*> withFieldErrors
          (inputSlider "Age" 13 100 28
            `prove` inRange 18 100 (const "Must be at least 18 years old for this account tier"))
    <*> inputEnumSelect "Account Tier" Developer
    <*> inputColor "Accent Color" (colorRGBA 99 102 241 255)
    <*> inputCheckbox "Subscribe to release announcements and updates" True
    <*> withFieldErrors
          (inputTextArea "Developer Bio" "Building high-performance GUI applications in Haskell with nano-ui and ditto."
            `prove` maxLength 160 (const "Bio must be 160 characters or fewer"))

formatRegistration :: Registration -> Text
formatRegistration r =
  "User @" <> regUsername r <> " (" <> regEmail r <> "), Age: "
    <> T.pack (show (round (regAge r) :: Int))
    <> ", Tier: " <> T.pack (show (regTier r))
    <> ", Color: " <> colorPickerToHex (regThemeColor r)
    <> ", Subscribed: " <> (if regSubscribe r then "Yes" else "No")

formDemoUi :: NanoUI ()
formDemoUi = do
  (submittedMsg, setSubmitted) <- useText ""
  (view', res) <- runNanoForm "user_reg" registrationForm
  let mReg = case res of
        Ditto.Ok (Ditto.Proved _ a) -> Just a
        Ditto.Error _               -> Nothing
      renderedView = case res of
        Ditto.Error errs -> Ditto.unView view' errs
        Ditto.Ok _       -> Ditto.unView view' []
  scrollWith (tight . grow) $
    columnWith (padAll 20 . gap 16 . fillW) $ do
      toolbar $ do
        columnWith (tight . gap 2) $ do
          heading "nano-ui-form"
          muted "Type-safe, composable immediate-mode forms powered by ditto & rendered via SDL3"
        flex
        muted "Press ESC to exit"
      sep
      rowWith (tight . gap 20 . fillW) $ do
        -- Left Column: Interactive Formlet
        columnWith (tight . gap 12 . fillW) $ do
          card $ do
            heading "User Profile & Registration"
            muted "All inputs validate live using composable applicative proofs."
            sep
            runFormView renderedView
            sep
            rowWith (tight . gap 10 . fillW) $ do
              btnSubmit <- button "Submit Registration"
              btnReset  <- button "Reset Form"
              when (respClicked btnSubmit) $ do
                case mReg of
                  Just reg -> setSubmitted ("Successfully registered: " <> formatRegistration reg)
                  Nothing  -> setSubmitted "Submission failed: Please fix the highlighted validation errors."
              when (respClicked btnReset) $ do
                resetForm "user_reg"
                setSubmitted "Form has been reset to defaults."

        -- Right Column: Live Telemetry & Inspector
        columnWith (tight . gap 12 . minW 340 . maxW 380) $ do
          card $ do
            heading "Live Form Inspector"
            muted "Real-time decode and proof telemetry:"
            sep
            case res of
              Ditto.Ok (Ditto.Proved _ reg) -> do
                heading "Status: VALID"
                sep
                kv "Username" (regUsername reg)
                kv "Email" (regEmail reg)
                kv "Age" (T.pack (show (round (regAge reg) :: Int)) <> " years old")
                kv "Account Tier" (T.pack (show (regTier reg)))
                kv "Color Hex" (colorPickerToHex (regThemeColor reg))
                kv "Newsletter" (if regSubscribe reg then "Active" else "Inactive")
                sep
                columnWith (tight . gap 4 . fillW) $ do
                  muted "Bio:"
                  void $ labelWith (tight . fillW . maxW 350 . fontMono) (regBio reg)
              Ditto.Error errs -> do
                danger "Status: INVALID / INCOMPLETE"
                sep
                heading "Active Validation Errors:"
                forM_ errs $ \(_, errMsg) -> do
                  danger ("• " <> errMsg)

          card $ do
            heading "Submission Activity"
            muted "Record of last form submission:"
            sep
            if T.null submittedMsg
              then muted "No submission attempted yet."
              else void $ labelWith (tight . fillW . maxW 350 . fontMono) submittedMsg

main :: IO ()
main =
  runSdlApp
    defaultSdlOptions
      { sdlWindowTitle = "nano-ui-form · Ditto Formlets (SDL3)"
      , sdlWindowSize = Size 1100 800
      , sdlAppShouldQuit = \inp -> inputKeysElem KeyEscape (inputKeys inp)
      }
    formDemoUi
