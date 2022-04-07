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

module NanoUI where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (playIO, Event (..))
import Polysemy
import Polysemy.Writer
import qualified Data.Vector.Unboxed as VU
import qualified Graphics.Text.TrueType as TT
import Polysemy.State
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Bifunctor

data AppState = AppState
  { fontCache :: TT.FontCache
  }
  deriving (Show)

type GUIM = Sem '[GUI, State AppState, Embed IO]

data GUI m a where
  Button :: [Picture] -> Float -> Float -> GUI m Bool

makeSem ''GUI

data Settings = Settings 
  { tickRate :: Int
  , bgColor :: Color
  , mainWindow :: Display
  }

defaultSettings :: Settings
defaultSettings = Settings
  { tickRate = 60
  , bgColor = makeColorI 0x28 0x28 0x28 0xff
  , mainWindow = InWindow "Wave" (800, 600) (100, 100)
  }

defaultMain :: Sem '[GUI, State AppState, Embed IO] () -> IO ()
defaultMain = mainWith defaultSettings

newState :: IO AppState
newState = AppState <$> TT.buildCache

renderFont :: TT.FontDescriptor -> TT.PointSize -> String -> GUIM Picture
renderFont fontd pt str = do 
  state <- get
  case TT.findFontInCache (fontCache state) fontd of
    Nothing -> error $ unwords ["font", show fontd, "missing"]
    Just fp -> do
      f <- either (error . show) id <$> (liftIO $ TT.loadFontFile fp)
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
      EventMotion _coords -> pure gui
      EventResize _dims -> pure gui
      EventKey _key _keyState _mods _coords -> pure gui
    )
    (\_t gui -> pure $ gui)

guiIO :: Member (Embed IO) r => Sem (GUI ': r) a -> Sem r a
guiIO = interpret \case
  Button {} -> embed @IO (pure False)

render :: Sem (GUI : r) b -> Sem r [Picture]
render = fmap fst . runWriter . runGUIPure

runGUIPure :: forall r a. Sem (GUI ': r) a -> Sem (Writer [Picture] ': r) a
runGUIPure sem = reinterpret go sem
  where
  go :: GUI (Sem rInitial) x -> Sem (Writer [Picture] : r) x
  go = \case
    Button p x y -> do
      tell [ rectangleSolid x y
           , translate (negate $ x / 2) (negate $ y / 2) (Pictures p)
           ]
      pure False
