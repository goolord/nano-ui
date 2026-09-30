-- | The ditto environment forms run in: field input values, form prefixes and
-- submitted state, retained by an explicit typed owner.
module NanoUI.Form.Internal.Backend
  ( FormInput (..)
  , formInputToText
  , FormUI (..)
  , FormState
  , newFormState
  , FormScope (..)
  , askFormScope
  , runFormUI
  , liftNanoUI
  , withFormWidgets
  , updateFieldInput
  , markFormSubmitted
  , isFormSubmitted
  , resetFormState
    -- * Stored form state, for tests
  , FormStateStore (..)
  , emptyFormStateStore
  , getFormStore
  , setFormStore
  ) where

import Control.Monad (when, (<$!>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import qualified Data.Text.Lazy.Builder.RealFloat as TB
import Data.Maybe (fromMaybe)
import qualified Ditto.Backend as Ditto
import Ditto.Backend
  ( FormError (..)
  , commonFormErrorText
  )
import Ditto.Core (Environment (..))
import Ditto.Types (Value (..), encodeFormId)
import GHC.Generics (Generic)
import NanoUI (NanoUI, liftIO, withKey)
import NanoUI.Internal.Context (Context, markDirty)

-- | A form field's raw input value, before parsing.
data FormInput
  = FormInputText !Text
  | FormInputBool !Bool
  | FormInputInt !Int
  | FormInputFloat !Float
  | FormInputList ![Text]
  deriving stock (Eq, Show, Generic)

-- | String representation of a 'FormInput'
formInputToText :: FormInput -> Text
formInputToText (FormInputText t) = t
formInputToText (FormInputBool b) = if b then "true" else "false"
formInputToText (FormInputInt i) = TL.toStrict (TB.toLazyText (TB.decimal i))
formInputToText (FormInputFloat f) = TL.toStrict (TB.toLazyText (TB.realFloat f))
formInputToText (FormInputList ts) = T.intercalate "," ts

-- | Internal store for form state across UI frames.
data FormStateStore = FormStateStore
  { fssInputs    :: !(Map.Map Text FormInput)
  , fssSubmitted :: !Bool
  } deriving stock (Eq, Show, Generic)

-- | Empty form state store.
emptyFormStateStore :: FormStateStore
emptyFormStateStore = FormStateStore Map.empty False

-- Keep reset identity alongside the form's values without exposing it in the
-- public FormStateStore. A new generation starts fresh form-local widget state,
-- including composite controls and text-area buffers.
data StoredForm = StoredForm
  { sfGeneration :: !Int
  , sfState      :: !FormStateStore
  }
  deriving (Eq)

-- | Allocate once per component/session. Form prefixes distinguish the forms
-- owned by this handle. The map is keyed by the full prefix, without hashing.
newtype FormState = FormState (IORef (Map.Map Text StoredForm))

newFormState :: IO FormState
newFormState = FormState <$> newIORef Map.empty

-- | Lexical ownership captured by field views during evaluation.
data FormScope = FormScope !FormState !Text

-- | Form evaluation reads its owner through a typed lexical environment.
newtype FormUI a = FormUI { unFormUI :: ReaderT FormScope NanoUI a }
  deriving newtype (Functor, Applicative, Monad)

askFormScope :: FormUI FormScope
askFormScope = FormUI ask

runFormUI :: FormState -> Text -> FormUI a -> NanoUI a
runFormUI owner prefix (FormUI action) = runReaderT action (FormScope owner prefix)

-- | Lift a 'NanoUI' action into 'FormUI'.
liftNanoUI :: NanoUI a -> FormUI a
liftNanoUI = FormUI . lift

-- | Decode scalar values as text and preserve text lists for multi-value fields.
instance Ditto.FormInput FormInput where
  type FileType FormInput = ()

  getInputText (FormInputText t) = Right t
  getInputText (FormInputList (t : _)) = Right t
  getInputText other = Right (formInputToText other)

  getInputTexts (FormInputList ts) = ts
  getInputTexts other = [formInputToText other]

  getInputString fi = T.unpack <$> Ditto.getInputText fi

  getInputFile _ = Right ()

-- | 'FormError' instance translating common form errors into 'Text'.
instance FormError FormInput Text where
  commonFormError = commonFormErrorText formInputToText

-- | Stable widget identity for a form, renewed when its state is reset.
withFormWidgets :: FormState -> Text -> NanoUI a -> NanoUI a
withFormWidgets owner prefix action = do
  stored <- liftIO (getStoredForm owner prefix)
  withKey (prefix, sfGeneration stored) action

getStoredForm :: FormState -> Text -> IO StoredForm
getStoredForm (FormState ref) prefix = do
  forms <- readIORef ref
  pure $! fromMaybe (StoredForm 0 emptyFormStateStore) (Map.lookup prefix forms)

setStoredForm :: FormState -> Text -> StoredForm -> IO ()
setStoredForm (FormState ref) prefix !stored = modifyIORef' ref (Map.insert prefix stored)

-- | Retrieve the 'FormStateStore' for a given form prefix.
getFormStore :: FormState -> Text -> IO FormStateStore
getFormStore owner prefix = sfState <$!> getStoredForm owner prefix

-- | Persist the 'FormStateStore' for a given form prefix.
setFormStore :: FormState -> Context -> Text -> FormStateStore -> IO ()
setFormStore owner ctx prefix fss = modifyFormStore owner ctx prefix (const fss)

-- Keep equality and redraw notification at the single mutation boundary.
modifyFormStore :: FormState -> Context -> Text -> (FormStateStore -> FormStateStore) -> IO ()
modifyFormStore owner ctx prefix update =
  modifyStoredForm owner ctx prefix (\stored -> stored {sfState = update (sfState stored)})

modifyStoredForm :: FormState -> Context -> Text -> (StoredForm -> StoredForm) -> IO ()
modifyStoredForm owner ctx prefix update = do
  previous <- getStoredForm owner prefix
  let next = update previous
  when (next /= previous) $ do
    setStoredForm owner prefix next
    markDirty ctx

-- | Update a specific field's input in the form store.
updateFieldInput :: FormState -> Context -> Text -> Text -> FormInput -> IO ()
updateFieldInput owner ctx prefix fieldKey inputVal =
  modifyFormStore owner ctx prefix $ \fss ->
    fss {fssInputs = Map.insert fieldKey inputVal (fssInputs fss)}

-- | Mark a form as submitted.
markFormSubmitted :: FormState -> Context -> Text -> Bool -> IO ()
markFormSubmitted owner ctx prefix isSubmitted =
  modifyFormStore owner ctx prefix (\fss -> fss {fssSubmitted = isSubmitted})

-- | Check if a form has been submitted.
isFormSubmitted :: FormState -> Text -> IO Bool
isFormSubmitted owner prefix = fssSubmitted <$> getFormStore owner prefix

-- | Reset values and renew widget identity so cached control state cannot
-- repopulate the form with its old values on the next frame.
resetFormState :: FormState -> Context -> Text -> IO ()
resetFormState owner ctx prefix = modifyStoredForm owner ctx prefix $ \stored ->
  if sfState stored == emptyFormStateStore
    then stored
    else StoredForm (sfGeneration stored + 1) emptyFormStateStore

-- | Ditto reads from the typed owner supplied by the form runner.
instance Environment FormUI FormInput where
  environment fid = do
    FormScope owner prefix <- askFormScope
    fss <- liftNanoUI (liftIO (getFormStore owner prefix))
    let fieldKey = encodeFormId fid
    pure $ case Map.lookup fieldKey (fssInputs fss) of
      Just val -> Found val
      Nothing  -> Default
