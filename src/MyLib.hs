{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs, TypeApplications , FlexibleContexts, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables #-}

module MyLib where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (playIO)
import Polysemy
import Polysemy.Input
import Polysemy.Output
import Polysemy.Writer
import Polysemy.Embed (runEmbedded)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Graphics.Text.TrueType as TT
import Polysemy.State
import Control.Monad.IO.Class (MonadIO(liftIO))

data AppState = AppState
  { fontCache :: TT.FontCache
  }

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
  , bgColor = makeColorI 0xff 0xff 0xff 0xff
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
      pure $ foldMap (line . VU.toList) $ mconcat $ TT.getStringCurveAtPoint
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
    (\e gui -> pure $ gui)
    (\t gui -> pure $ gui)
  where
  go = render

guiToIO :: Member (Embed IO) r => Sem (GUI ': r) a -> Sem r a
guiToIO = interpret \case
  Button p x y -> embed @IO (pure False)

render :: Sem (GUI : r) b -> Sem r [Picture]
render = fmap fst . runWriter . runGUIPure

runGUIPure :: forall r m a. Sem (GUI ': r) a -> Sem (Writer [Picture] ': r) a
runGUIPure sem = reinterpret go sem
  where
  go :: GUI (Sem rInitial) x -> Sem (Writer [Picture] : r) x
  go = \case
    (Button p x y) -> tell (rectangleSolid x y : p) *> pure False
