{-# LANGUAGE TypeFamilies #-}

-- | The value a backend keeps for its session (an SDL window's environment,
-- say), stored on the context and read back at its own type. A backend is
-- named at the type level; its singleton picks the value out, and matching
-- two singletons proves their backends the same, so the lookup needs no
-- runtime type representation or cast.
module NanoUI.Internal.BackendSession
  ( Backend (..)
  , SBackend (..)
  , BackendSession
  , SomeBackendSession (..)
  , sessionFor
  ) where

import Data.Kind (Type)
import Data.Type.Equality (TestEquality (..), (:~:) (..))
import GHC.TypeLits (SSymbol, Symbol)

-- | The backends that can drive a context, as a kind. A backend outside this
-- repository names itself with 'Custom'.
data Backend = Sdl | Rgfw | Custom Symbol

-- | The singleton of a 'Backend'.
type SBackend :: Backend -> Type
data SBackend b where
  SSdl :: SBackend 'Sdl
  SRgfw :: SBackend 'Rgfw
  SCustom :: SSymbol s -> SBackend ('Custom s)

instance TestEquality SBackend where
  testEquality SSdl SSdl = Just Refl
  testEquality SRgfw SRgfw = Just Refl
  testEquality (SCustom a) (SCustom b) = (\Refl -> Refl) <$> testEquality a b
  testEquality _ _ = Nothing

-- | What a backend keeps for its session. Each backend package gives its own
-- instance, such as @type instance BackendSession 'Sdl = SdlEnv@.
type family BackendSession (b :: Backend) :: Type

-- | A session value together with the backend it belongs to.
data SomeBackendSession = forall b. SomeBackendSession !(SBackend b) (BackendSession b)

-- | The session value, if it belongs to the backend asked for.
sessionFor :: SBackend b -> SomeBackendSession -> Maybe (BackendSession b)
sessionFor want (SomeBackendSession have session) = case testEquality want have of
  Just Refl -> Just session
  Nothing -> Nothing
