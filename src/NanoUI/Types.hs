{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Types where

import Control.Monad.Freer hiding (translate)
import Control.Monad.Freer.Reader
import Control.Monad.Freer.TH
import Data.HashMap.Strict (HashMap)
import Data.IORef
import Graphics.Gloss hiding (text)
import Graphics.Gloss.Interface.IO.Interact (MouseButton (..), KeyState (..))
import qualified Graphics.Text.TrueType as TT
import Data.IntMap (IntMap)

data GUI a where
  Button ::
       GUI () -- ^ label
    -> Float -- ^ x
    -> Float -- ^ y
    -> GUI Bool -- ^ True == pressed this frame
  Padding ::
       Float -- ^ x
    -> Float -- ^ y
    -> GUI ()
  Input ::
       Int -- ID
    -> String -- placeholder
    -> String -- initial value
    -> GUI String -- current user input
  PictureI :: Picture -> GUI ()
  Columns :: GUI a -> GUI a
  Rows :: GUI a -> GUI a

makeEffect ''GUI

data ElState = Active | Hot | Inactive

data BBox = BBox
  { bboxBR :: !Point
  , bboxTL :: !Point
  }

instance Semigroup BBox where
  (BBox brL tlL) <> (BBox brR tlR) = BBox (ptBR brL brR) (ptTL tlL tlR)

instance Monoid BBox where
  mempty = BBox (0.0, 0.0) (0.0, 0.0)

ptBR :: (Ord a, Ord b) => (a, b) -> (a, b) -> (a, b)
ptBR (x1,y1) (x2,y2) = (max x1 x2, min y1 y2)

ptTL :: (Ord a, Ord b) => (a, b) -> (a, b) -> (a, b)
ptTL (x1,y1) (x2,y2) = (min x1 x2, max y1 y2)

data AppState = AppState
  { fontCache :: !TT.FontCache
  , loadedFontCache :: !(IORef (HashMap TT.FontDescriptor TT.Font))
  , inputState :: !(IORef (IntMap InputState))
  , mouse :: !(IORef Mouse)
  , windowSize :: !(IORef (Int, Int))
  }

data InputState = InputState (IORef String) (IORef InputCursor) InputActive

data InputActive
  = InputActive
  | InputInactive

type InputCursor = Int

data Settings = Settings
  { tickRate :: !Int
  , bgColor :: !Color
  , mainWindow :: !Display
  , stylesheet :: !Stylesheet
  }

type GUIM = Eff [GUI, Reader AppState, Reader Settings, IO]

data Stylesheet = Stylesheet
  { xPad :: !Float
  , yPad :: !Float
  }

data Mouse = Hovering Point | MB Point MouseButton KeyState
  deriving Show

mousePosPt :: Mouse -> Point
mousePosPt (Hovering p) = p
mousePosPt (MB p _ _) = p

data World = World
  { worldGui :: GUIM ()
  , pictureCache :: IORef (Maybe Picture)
  }

clearCache :: World -> IO ()
clearCache w = do
  writeIORef (pictureCache w) Nothing

