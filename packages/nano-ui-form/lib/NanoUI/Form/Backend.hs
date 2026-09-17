-- | The ditto environment forms run in: field input values, form prefixes and
-- submitted state, kept in the widget store.
module NanoUI.Form.Backend
  ( FormInput (..)
  , formInputToText
  , FormUI (..)
  , liftNanoUI
  , getActiveFormPrefix
  , withFormPrefix
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
  , setActiveFormPrefix
  ) where

import Control.Monad (when, (<$!>))
import Data.Dynamic (fromDynamic, toDyn)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import qualified Data.Text.Lazy.Builder.RealFloat as TB
import Data.Hashable (hash)
import Data.Maybe (fromMaybe)
import qualified Ditto.Backend as Ditto
import Ditto.Backend
  ( FormError (..)
  , commonFormErrorText
  )
import Ditto.Core (Environment (..))
import Ditto.Types (Value (..), encodeFormId)
import GHC.Generics (Generic)
import Effectful.Exception (bracket)
import NanoUI (NanoUI, uiIO, withKey)
import NanoUI.Monad (askContext)
import NanoUI.Context (Context, getStore, markDirty, setStore)
import NanoUI.Store (WidgetStore (..))

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

-- | Form execution monad wrapping 'NanoUI'.
newtype FormUI a = FormUI { unFormUI :: NanoUI a }
  deriving newtype (Functor, Applicative, Monad)

-- | Lift a 'NanoUI' action into 'FormUI'.
liftNanoUI :: NanoUI a -> FormUI a
liftNanoUI = FormUI

-- | 'FormInput' instance for 'FormInput' allowing ditto decoding.
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

-- | Well-known slot key in 'storeDyn' for the dynamically scoped form prefix.
activePrefixSlot :: Int
activePrefixSlot = -0x464F524D -- -'FORM'

-- | Hash a form prefix to a unique 'IntMap' key.
formStoreKey :: Text -> Int
formStoreKey prefix = hash ("nano-ui-form:" :: Text, prefix)

-- | Retrieve the active form prefix in the current context.
getActiveFormPrefix :: Context -> IO Text
getActiveFormPrefix ctx = do
  ws <- getStore ctx
  pure $! fromMaybe "" (IM.lookup activePrefixSlot (storeDyn ws) >>= fromDynamic)

-- | Set the active form prefix in the current context.
setActiveFormPrefix :: Context -> Text -> IO ()
setActiveFormPrefix ctx prefix = do
  ws <- getStore ctx
  setStore ctx ws {storeDyn = IM.insert activePrefixSlot (toDyn prefix) (storeDyn ws)}

-- | Evaluate or render a form under its own prefix, restoring the enclosing
-- prefix afterwards. Restore only this slot, so field updates survive the scope.
withFormPrefix :: Text -> NanoUI a -> NanoUI a
withFormPrefix prefix action = do
  ctx <- askContext
  let restorePrefix previous = uiIO $ do
        ws <- getStore ctx
        setStore ctx ws
          { storeDyn = IM.alter (const previous) activePrefixSlot (storeDyn ws)
          }
  bracket
    (uiIO $ IM.lookup activePrefixSlot . storeDyn <$> getStore ctx)
    restorePrefix
    (\_ -> uiIO (setActiveFormPrefix ctx prefix) >> action)

-- | Stable widget identity for a form, renewed when its state is reset.
withFormWidgets :: Text -> NanoUI a -> NanoUI a
withFormWidgets prefix action = do
  ctx <- askContext
  stored <- uiIO (getStoredForm ctx prefix)
  withKey (prefix, sfGeneration stored) action

getStoredForm :: Context -> Text -> IO StoredForm
getStoredForm ctx prefix = do
  ws <- getStore ctx
  -- Resolve the lookup here rather than returning a thunk over the whole store.
  pure $! fromMaybe (StoredForm 0 emptyFormStateStore) (IM.lookup (formStoreKey prefix) (storeDyn ws) >>= fromDynamic)

setStoredForm :: Context -> Text -> StoredForm -> IO ()
setStoredForm ctx prefix !stored = do
  ws <- getStore ctx
  setStore ctx ws {storeDyn = IM.insert (formStoreKey prefix) (toDyn stored) (storeDyn ws)}

-- | Retrieve the 'FormStateStore' for a given form prefix.
getFormStore :: Context -> Text -> IO FormStateStore
getFormStore ctx prefix = sfState <$!> getStoredForm ctx prefix

-- | Persist the 'FormStateStore' for a given form prefix.
setFormStore :: Context -> Text -> FormStateStore -> IO ()
setFormStore ctx prefix fss = do
  stored <- getStoredForm ctx prefix
  setStoredForm ctx prefix stored {sfState = fss}

-- Form state lives in a Dynamic slot, which the core cannot compare. Keep
-- equality and redraw notification here rather than in each mutation.
modifyFormStore :: Context -> Text -> (FormStateStore -> FormStateStore) -> IO ()
modifyFormStore ctx prefix update =
  modifyStoredForm ctx prefix (\stored -> stored {sfState = update (sfState stored)})

modifyStoredForm :: Context -> Text -> (StoredForm -> StoredForm) -> IO ()
modifyStoredForm ctx prefix update = do
  previous <- getStoredForm ctx prefix
  let next = update previous
  when (next /= previous) $ do
    setStoredForm ctx prefix next
    markDirty ctx

-- | Update a specific field's input in the form store.
updateFieldInput :: Context -> Text -> Text -> FormInput -> IO ()
updateFieldInput ctx prefix fieldKey inputVal =
  modifyFormStore ctx prefix $ \fss ->
    fss {fssInputs = Map.insert fieldKey inputVal (fssInputs fss)}

-- | Mark a form as submitted.
markFormSubmitted :: Context -> Text -> Bool -> IO ()
markFormSubmitted ctx prefix isSubmitted =
  modifyFormStore ctx prefix (\fss -> fss {fssSubmitted = isSubmitted})

-- | Check if a form has been submitted.
isFormSubmitted :: Context -> Text -> IO Bool
isFormSubmitted ctx prefix = do
  fss <- getFormStore ctx prefix
  pure (fssSubmitted fss)

-- | Reset values and renew widget identity so cached control state cannot
-- repopulate the form with its old values on the next frame.
resetFormState :: Context -> Text -> IO ()
resetFormState ctx prefix = modifyStoredForm ctx prefix $ \stored ->
  if sfState stored == emptyFormStateStore
    then stored
    else StoredForm (sfGeneration stored + 1) emptyFormStateStore

-- | Environment instance for 'FormUI' connecting ditto to nano-ui's context store.
instance Environment FormUI FormInput where
  environment fid = FormUI $ do
    ctx <- askContext
    prefix <- uiIO (getActiveFormPrefix ctx)
    fss <- uiIO (getFormStore ctx prefix)
    let fieldKey = encodeFormId fid
    pure $ case Map.lookup fieldKey (fssInputs fss) of
      Just val -> Found val
      Nothing  -> Default
