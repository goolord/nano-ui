{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module NanoUI.Form.Backend
  ( FormInput (..)
  , formInputToText
  , FormStateStore (..)
  , emptyFormStateStore
  , FormUI (..)
  , liftNanoUI
  , getFormStore
  , setFormStore
  , getActiveFormPrefix
  , setActiveFormPrefix
  , clearActiveFormPrefix
  , withFormPrefix
  , updateFieldInput
  , markFormSubmitted
  , isFormSubmitted
  , resetFormState
  ) where

import Control.Monad (when)
import Data.Dynamic (fromDynamic, toDyn)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Hashable (hash)
import qualified Ditto.Backend as Ditto
import Ditto.Backend
  ( FormError (..)
  , commonFormErrorText
  )
import Ditto.Core (Environment (..))
import Ditto.Types (Value (..), encodeFormId)
import GHC.Generics (Generic)
import Effectful.Exception (bracket)
import NanoUI (NanoUI, uiIO)
import NanoUI.Monad (askContext)
import NanoUI.Context (Context, getStore, markDirty, setStore)
import NanoUI.Store (WidgetStore (..))

-- | Unified input representation for form field values.
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
formInputToText (FormInputInt i) = T.pack (show i)
formInputToText (FormInputFloat f) = T.pack (show f)
formInputToText (FormInputList ts) = T.intercalate "," ts

-- | Internal store for form state across UI frames.
data FormStateStore = FormStateStore
  { fssInputs    :: !(Map.Map Text FormInput)
  , fssSubmitted :: !Bool
  , fssDirty     :: !(Set.Set Text)
  } deriving stock (Eq, Show, Generic)

-- | Empty form state store.
emptyFormStateStore :: FormStateStore
emptyFormStateStore = FormStateStore Map.empty False Set.empty

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
  getInputStrings fi = map T.unpack (Ditto.getInputTexts fi)

  getInputFile _ = Right ()

-- | 'FormError' instance translating common form errors into 'Text'.
instance FormError FormInput Text where
  commonFormError = commonFormErrorText formInputToText

-- | Well-known slot key in 'storeDyn' for the dynamically scoped form prefix.
activePrefixSlot :: Int
activePrefixSlot = -0x464F524D -- -'FORM'

-- | Hash a form prefix to a unique 'IntMap' key.
formStoreKey :: Text -> Int
formStoreKey prefix = hash ("nano-ui-form:" <> prefix)

-- | Retrieve the active form prefix in the current context.
getActiveFormPrefix :: Context -> IO Text
getActiveFormPrefix ctx = do
  ws <- getStore ctx
  case IM.lookup activePrefixSlot (storeDyn ws) >>= fromDynamic of
    Just (p :: Text) -> pure p
    _                -> pure ""

-- | Set the active form prefix in the current context.
setActiveFormPrefix :: Context -> Text -> IO ()
setActiveFormPrefix ctx prefix = do
  ws <- getStore ctx
  let ws' = ws { storeDyn = IM.insert activePrefixSlot (toDyn prefix) (storeDyn ws) }
  setStore ctx ws'

-- | Clear the active form prefix in the current context.
clearActiveFormPrefix :: Context -> IO ()
clearActiveFormPrefix ctx = do
  ws <- getStore ctx
  let ws' = ws { storeDyn = IM.delete activePrefixSlot (storeDyn ws) }
  setStore ctx ws'

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

-- | Retrieve the 'FormStateStore' for a given form prefix.
getFormStore :: Context -> Text -> IO FormStateStore
getFormStore ctx prefix = do
  ws <- getStore ctx
  case IM.lookup (formStoreKey prefix) (storeDyn ws) >>= fromDynamic of
    Just fss -> pure fss
    Nothing  -> pure emptyFormStateStore

-- | Persist the 'FormStateStore' for a given form prefix.
setFormStore :: Context -> Text -> FormStateStore -> IO ()
setFormStore ctx prefix fss = do
  ws <- getStore ctx
  let ws' = ws { storeDyn = IM.insert (formStoreKey prefix) (toDyn fss) (storeDyn ws) }
  setStore ctx ws'

-- Form state lives in a Dynamic slot, which the core cannot compare. Keep
-- equality and redraw notification here rather than in each mutation.
modifyFormStore :: Context -> Text -> (FormStateStore -> FormStateStore) -> IO ()
modifyFormStore ctx prefix update = do
  previous <- getFormStore ctx prefix
  let next = update previous
  when (next /= previous) $ do
    setFormStore ctx prefix next
    markDirty ctx

-- | Update a specific field's input in the form store.
updateFieldInput :: Context -> Text -> Text -> FormInput -> IO ()
updateFieldInput ctx prefix fieldKey inputVal =
  modifyFormStore ctx prefix $ \fss ->
    fss
      { fssInputs = Map.insert fieldKey inputVal (fssInputs fss)
      , fssDirty = Set.insert fieldKey (fssDirty fss)
      }

-- | Mark a form as submitted.
markFormSubmitted :: Context -> Text -> Bool -> IO ()
markFormSubmitted ctx prefix isSubmitted =
  modifyFormStore ctx prefix (\fss -> fss {fssSubmitted = isSubmitted})

-- | Check if a form has been submitted.
isFormSubmitted :: Context -> Text -> IO Bool
isFormSubmitted ctx prefix = do
  fss <- getFormStore ctx prefix
  pure (fssSubmitted fss)

-- | Reset form state back to empty.
resetFormState :: Context -> Text -> IO ()
resetFormState ctx prefix = modifyFormStore ctx prefix (const emptyFormStateStore)

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
