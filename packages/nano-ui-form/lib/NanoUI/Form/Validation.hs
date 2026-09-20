-- | Proofs that parse and validate field values: numbers, text length,
-- ranges, email addresses, and custom predicates.
module NanoUI.Form.Validation
  ( -- * Proof combinators
    Proof (..)
  , prove
  , transformEither
  , transformEitherM
  , notNullProof
  , decimal
  , signedDecimal
  , realFrac
  , realFracSigned
    -- * Common UI validations
  , validate
  , satisfies
  , notEmpty
  , minLength
  , maxLength
  , inRange
  , validEmail
  , matches
  , customProof
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Ditto.Proof
  ( Proof (..)
  , decimal
  , notNullProof
  , prove
  , realFrac
  , realFracSigned
  , signedDecimal
  , transformEither
  , transformEitherM
  )

-- | Keep values that pass the check, rejecting the rest with an error built
-- from the rejected value.
validate :: Applicative m => (a -> Bool) -> (a -> err) -> Proof m err a a
validate ok mkErr = Proof (\x -> pure (if ok x then Right x else Left (mkErr x))) id

-- | Validate with an arbitrary predicate.
satisfies :: Applicative m => (a -> Bool) -> err -> Proof m err a a
satisfies ok err = validate ok (const err)

-- | Validate that a text string is not blank or whitespace-only.
notEmpty :: Applicative m => err -> Proof m err Text Text
notEmpty = satisfies (not . T.null . T.strip)

-- | Require at least the given number of Unicode characters. The error
-- builder receives the actual length; this is not a grapheme-cluster count.
minLength :: Applicative m => Int -> (Int -> err) -> Proof m err Text Text
minLength minLen mkErr = validate ((>= minLen) . T.length) (mkErr . T.length)

-- | Require at most the given number of Unicode characters. The error
-- builder receives the actual length.
maxLength :: Applicative m => Int -> (Int -> err) -> Proof m err Text Text
maxLength maxLen mkErr = validate ((<= maxLen) . T.length) (mkErr . T.length)

-- | Validate that a value falls within the inclusive range @[minVal, maxVal]@.
inRange :: (Applicative m, Ord a) => a -> a -> (a -> err) -> Proof m err a a
inRange minVal maxVal = validate (\x -> not (x < minVal || x > maxVal))

-- | Check for one @\@@, a nonempty local part, and a dot inside the domain.
-- This is a small input check, not full email-syntax or deliverability validation.
validEmail :: Applicative m => (Text -> err) -> Proof m err Text Text
validEmail = validate isEmail
  where
    isEmail t =
      case T.splitOn "@" t of
        [user, domain] ->
          not (T.null user)
            && T.isInfixOf "." domain
            && not (T.isPrefixOf "." domain)
            && not (T.isSuffixOf "." domain)
        _ -> False

-- | Validate that a value equals an expected value (e.g. password confirmation).
matches :: (Applicative m, Eq a) => a -> err -> Proof m err a a
matches target = satisfies (== target)

-- | Create a proof from an 'Either' function and default initial fallback.
customProof :: Applicative m => (a -> Either err b) -> (a -> b) -> Proof m err a b
customProof f fallback = Proof (pure . f) fallback
