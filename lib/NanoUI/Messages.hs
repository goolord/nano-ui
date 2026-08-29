module NanoUI.Messages
  ( FrameMsg (..)
  , decodeMessages
  , reduceMessages
  , reduceUpdates
  ) where

import Data.Maybe (mapMaybe)
import Data.Typeable (Typeable, cast)

data FrameMsg where
  FrameMsg :: Typeable a => a -> FrameMsg

-- Recover same-type messages. Other payloads (widget String tags, other
-- app types) are dropped.
decodeMessages :: Typeable a => [FrameMsg] -> [a]
decodeMessages = mapMaybe (\(FrameMsg x) -> cast x)

-- Elm-style fold: apply update to each decoded message, in emit order.
reduceMessages :: Typeable msg => (msg -> model -> model) -> model -> [FrameMsg] -> model
reduceMessages update model = foldl' (flip update) model . decodeMessages

-- Fold emitted (model -> model) functions. Same frame-end contract as
-- reduceMessages, without a named Msg type.
reduceUpdates :: Typeable model => model -> [FrameMsg] -> model
reduceUpdates = reduceMessages ($)
