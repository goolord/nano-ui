-- | Code for the book's introduction (@book/src/README.md@).
module Book.Intro where

import Data.Text qualified as T
import NanoUI

-- ANCHOR: counter
counter :: NanoUI ()
counter = row $ do
  n <- useVar (0 :: Int)
  whenM (button "-") (modifyVar n (subtract 1))
  label . T.pack . show =<< readVar n
  whenM (button "+") (modifyVar n (+ 1))
-- ANCHOR_END: counter
