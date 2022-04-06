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

data GUI m a where
  Button :: Picture -> Float -> Float -> GUI m Bool

makeSem ''GUI

data Settings = Settings 
  { tickRate :: Int
  , bgColor :: Color
  }

defaultSettings :: Settings
defaultSettings = Settings
  { tickRate = 60
  , bgColor = makeColorI 0xff 0xff 0xff 0xff
  }

defaultMainWith :: Settings -> Sem '[GUI, Embed IO] () -> IO ()
defaultMainWith settings gui' = do
  playIO
    FullScreen
    (bgColor settings)
    (tickRate settings)
    gui'
    (\gui -> do
      gui2 <- runM $ render gui
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
    (Button p x y) -> tell [rectangleSolid x y] *> pure False
