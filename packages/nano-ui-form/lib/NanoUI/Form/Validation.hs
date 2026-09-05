{-# LANGUAGE OverloadedStrings #-}

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
  , notEmpty
  , minLength
  , maxLength
  , inRange
  , validEmail
  , matches
  , satisfies
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

-- | Validate that a text string is not blank or whitespace-only.
notEmpty :: Applicative m => err -> Proof m err Text Text
notEmpty err = Proof (pure . check) id
  where
    check t
      | T.null (T.strip t) = Left err
      | otherwise          = Right t

-- | Validate minimum string length.
minLength :: Applicative m => Int -> (Int -> err) -> Proof m err Text Text
minLength minLen mkErr = Proof (pure . check) id
  where
    check t
      | T.length t < minLen = Left (mkErr (T.length t))
      | otherwise           = Right t

-- | Validate maximum string length.
maxLength :: Applicative m => Int -> (Int -> err) -> Proof m err Text Text
maxLength maxLen mkErr = Proof (pure . check) id
  where
    check t
      | T.length t > maxLen = Left (mkErr (T.length t))
      | otherwise           = Right t

-- | Validate that a value falls within the inclusive range @[minVal, maxVal]@.
inRange :: (Applicative m, Ord a) => a -> a -> (a -> err) -> Proof m err a a
inRange minVal maxVal mkErr = Proof (pure . check) id
  where
    check x
      | x < minVal || x > maxVal = Left (mkErr x)
      | otherwise                = Right x

-- | Validate basic email structure (@user@domain.tld@).
validEmail :: Applicative m => (Text -> err) -> Proof m err Text Text
validEmail mkErr = Proof (pure . check) id
  where
    check t =
      case T.splitOn "@" t of
        [user, domain]
          | not (T.null user)
          , T.isInfixOf "." domain
          , not (T.isPrefixOf "." domain)
          , not (T.isSuffixOf "." domain) -> Right t
        _ -> Left (mkErr t)

-- | Validate that a value equals an expected value (e.g. password confirmation).
matches :: (Applicative m, Eq a) => a -> err -> Proof m err a a
matches target err = Proof (pure . check) id
  where
    check x
      | x == target = Right x
      | otherwise   = Left err

-- | Validate with an arbitrary predicate.
satisfies :: Applicative m => (a -> Bool) -> err -> Proof m err a a
satisfies predicate err = Proof (pure . check) id
  where
    check x
      | predicate x = Right x
      | otherwise   = Left err

-- | Create a proof from an 'Either' function and default initial fallback.
customProof :: Applicative m => (a -> Either err b) -> (a -> b) -> Proof m err a b
customProof f fallback = Proof (pure . f) fallback
