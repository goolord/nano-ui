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
import Graphics.Gloss.Interface.IO.Interact (interactIO, Event (..))
import qualified Data.DList as DList
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as VU
import qualified Graphics.Text.TrueType as TT

deriving instance Generic TT.FontDescriptor
deriving instance Generic TT.FontStyle
instance Hashable TT.FontDescriptor
instance Hashable TT.FontStyle

data GUI a where
  Button :: GUI () -> Float -> Float -> GUI Bool
  PictureI :: Picture -> GUI ()
  Columns :: GUI a -> GUI a
  Rows :: GUI a -> GUI a

makeEffect ''GUI

data AppState = AppState
  { fontCache :: TT.FontCache
  , loadedFontCache :: !(IORef (HashMap TT.FontDescriptor TT.Font))
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
  f <- case HM.lookup fontd loaded of
    Just f -> pure f
    Nothing -> do
      case TT.findFontInCache (fontCache state) fontd of
        Nothing -> error $ unwords ["font", show fontd, "missing"]
        Just fp -> do
          f <- either (error . show) id <$> (liftIO $ TT.loadFontFile fp)
          liftIO $ modifyIORef' (loadedFontCache state) (HM.insert fontd f)
          pure f
  -- todo: interpret as Polygon instead of Line
  pure $ foldMap (line . fmap (second negate) . VU.toList) $ mconcat $ TT.getStringCurveAtPoint
    96 -- DPI
    (0.0, 0.0)
    [(f,pt,str)]

data World = World
  { worldGui :: GUIM ()
  , pictureCache :: IORef (Maybe Picture)
  }

nocache :: World -> IO ()
nocache w = do 
  writeIORef (pictureCache w) Nothing

mainWith :: Settings -> GUIM () -> IO ()
mainWith settings gui' = do
  state <- newState
  let render' = runM . evalState state . render
  initGui <- render' gui'
  initPcache <- newIORef (Just $ Pictures initGui)
  interactIO
    (mainWindow settings)
    (bgColor settings)
    (World
      { worldGui = gui'
      , pictureCache = initPcache
      }
    )
    (\world -> do
      pcache <- readIORef (pictureCache world)
      case pcache of
        Just g -> pure g
        Nothing -> Pictures <$> render' (worldGui world)
    )
    (\e world -> case e of
      EventResize _dims -> pure world
      EventMotion p -> do
        writeIORef (cursorPos state) p
        nocache world -- move to the controller handler
        pure world
      EventKey _key _keyState _mods _coords -> pure world
    )
    (\controller -> do
      -- update every n seconds
      -- nocache world
      pure ()
    )

guiIO :: forall r a. (LastMember IO r, Member IO r) => Eff (GUI ': r) a -> Eff r a
guiIO = interpretM @GUI @IO go
  where
  go :: forall x. GUI x -> IO x
  go = \case
    Button {} -> pure False
    PictureI {} -> pure ()
    Columns g -> go g
    Rows g -> go g

render :: Member IO r => Eff (GUI : r) b -> Eff r [Picture]
render = fmap (DList.toList . snd) . runWriter . runGUI

runGUI :: forall r a. Member IO r => Eff (GUI ': r) a -> Eff (Writer (DList Picture) ': r) a
runGUI sem = evalState (0.0, 0.0) $ reinterpret2 withRows sem
  where
  withColumns g = do
    (_xo1 :: Float, yo1 :: Float) <- get
    res <- go g
    (xo2 :: Float, _yo2 :: Float) <- get
    -- bounding <- getBoundingBox
    put (xo2, yo1)
    pure res
  withRows g = do
    (xo1 :: Float, _yo1 :: Float) <- get
    res <- go g
    (_xo2 :: Float, yo2 :: Float) <- get
    -- bounding <- getBoundingBox
    put (xo1, yo2)
    pure res
  go :: forall x r'. GUI x -> Eff (State (Float, Float) : Writer (DList Picture) : r') x
  go = \case
    Button gp x y -> do
      (xo, yo) <- get
      (_, p) <- runWriter $ evalState (0.0, 0.0) $ go gp
      tell $ DList.fromList
        [ translate xo yo $ rectangleSolid x y
        , translate xo yo $ translate (negate $ x / 2) (negate $ y / 2) $ Pictures $ DList.toList p
        ]
      modify (\(xo', yo') -> (xo' + (x / 2) :: Float, yo' - y))
      pure False
    PictureI p -> do
      (xo, yo) <- get
      tell (DList.singleton $ translate xo yo p)
      -- modify (\yo' -> yo' - y)
      pure ()
    Columns g -> withColumns g
    Rows g -> withRows g
