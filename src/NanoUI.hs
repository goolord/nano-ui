{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module NanoUI where

import Control.Monad.Freer hiding (translate)
import Control.Monad.Freer.State
import Control.Monad.Freer.TH
import Control.Monad.Freer.Writer
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Bifunctor
import Data.DList
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.IORef
import GHC.Generics (Generic)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (playIO, Event (..))
import qualified Data.DList as DList
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as VU
import qualified Graphics.Text.TrueType as TT

deriving instance Generic TT.FontDescriptor
deriving instance Generic TT.FontStyle
deriving instance Generic TT.PointSize
instance Hashable TT.FontDescriptor
instance Hashable TT.FontStyle
instance Hashable TT.PointSize

data GUI a where
  Button :: Picture -> Float -> Float -> GUI Bool
  PictureI :: Picture -> GUI ()

makeEffect ''GUI

data AppState = AppState
  { fontCache :: TT.FontCache
  , loadedFontCache :: !(IORef (HashMap (TT.FontDescriptor, TT.PointSize) TT.Font))
  , cursorPos :: !(IORef Point)
  }

data Settings = Settings
  { tickRate :: !Int
  , bgColor :: !Color
  , mainWindow :: !Display
  }

defaultSettings :: Settings
defaultSettings = Settings
  { tickRate = 60
  , bgColor = makeColorI 0x28 0x28 0x28 0xff
  , mainWindow = InWindow "Wave" (800, 600) (100, 100)
  }

type GUIM = Eff [GUI, State AppState, IO]

defaultMain :: GUIM () -> IO ()
defaultMain = mainWith defaultSettings

newState :: IO AppState
newState = AppState <$> TT.buildCache <*> newIORef mempty <*> newIORef (0, 0)

renderFont :: TT.FontDescriptor -> TT.PointSize -> String -> GUIM Picture
renderFont fontd pt str = do
  state <- get
  loaded <- liftIO $ readIORef $ loadedFontCache state
  f <- case HM.lookup (fontd, pt) loaded of
    Just f -> pure f
    Nothing -> do
      case TT.findFontInCache (fontCache state) fontd of
        Nothing -> error $ unwords ["font", show fontd, "missing"]
        Just fp -> do
          f <- either (error . show) id <$> (liftIO $ TT.loadFontFile fp)
          liftIO $ modifyIORef' (loadedFontCache state) (HM.insert (fontd, pt) f)
          pure f
  -- todo: interpret as Polygon instead of Line
  pure $ foldMap (line . fmap (second negate) . VU.toList) $ mconcat $ TT.getStringCurveAtPoint
    96 -- DPI
    (0.0, 0.0)
    [(f,pt,str)]

mainWith :: Settings -> GUIM () -> IO ()
mainWith settings gui' = do
  state <- newState
  let
  playIO
    (mainWindow settings)
    (bgColor settings)
    (tickRate settings)
    gui'
    (\gui -> do
      gui2 <- runM $ evalState state $ render gui
      pure $ Pictures gui2
    )
    (\e gui -> case e of
      EventResize _dims -> pure gui
      EventMotion p -> do
        writeIORef (cursorPos state) p
        pure gui
      EventKey _key _keyState _mods _coords -> pure gui
    )
    (\_t gui -> pure $ gui)

guiIO :: (LastMember IO r, Member IO r) => Eff (GUI ': r) a -> Eff r a
guiIO = interpretM @GUI @IO $ \case
  Button {} -> pure False
  PictureI {} -> pure ()

render :: Member IO r => Eff (GUI : r) b -> Eff r [Picture]
render = fmap (DList.toList . snd) . runWriter . runGUI

runGUI :: forall r a. Member IO r => Eff (GUI ': r) a -> Eff (Writer (DList Picture) ': r) a
runGUI sem = evalState (0.0 :: Float) $ reinterpret2 go sem
  where
  go :: GUI x -> Eff (State Float : Writer (DList Picture) : r) x
  go = \case
    Button p x y -> do
      height <- get
      tell $ DList.fromList 
        [ translate 0.0 height $ rectangleSolid x y
        , translate 0.0 height $ translate (negate $ x / 2) (negate $ y / 2) p
        ]
      modify (\height' -> height' - y)
      pure False
    PictureI p -> do
      height <- get
      tell (DList.singleton $ translate 0.0 height p)
      -- modify (\height' -> height' - y)
      pure ()
