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

module NanoUI where

import Control.Monad.Freer hiding (translate)
import Control.Monad.Freer.State
import Control.Monad.Freer.Reader
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
import Graphics.Gloss.Interface.IO.Interact (interactIO, Event (..), Key (..), MouseButton (..), KeyState (..))
import qualified Data.DList as DList
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as VU
import qualified Graphics.Text.TrueType as TT
import Graphics.Gloss.Data.Point (pointInBox)
import Data.Maybe (mapMaybe)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Semigroup (Semigroup(sconcat))

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

data ElState = Active | Hot | Inactive

data BBox = BBox
  { bboxBR :: !Point
  , bboxTL :: !Point
  }

instance Semigroup BBox where
  (BBox brL tlL) <> (BBox brR tlR) = BBox (ptBR brL brR) (ptTL tlL tlR)

ptBR :: (Ord a, Ord b) => (a, b) -> (a, b) -> (a, b)
ptBR (x1,y1) (x2,y2) = (max x1 x2, min y1 y2)

ptTL :: (Ord a, Ord b) => (a, b) -> (a, b) -> (a, b)
ptTL (x1,y1) (x2,y2) = (min x1 x2, max y1 y2)

data AppState = AppState
  { fontCache :: !TT.FontCache
  , loadedFontCache :: !(IORef (HashMap TT.FontDescriptor TT.Font))
  , mouse :: !(IORef Mouse)
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

type GUIM = Eff [GUI, Reader AppState, IO]

defaultMain :: GUIM () -> IO ()
defaultMain = mainWith defaultSettings

newState :: IO AppState
newState = do
  fontCache       <- TT.buildCache 
  loadedFontCache <- newIORef HM.empty
  mouse           <- newIORef (Hovering (0, 0))
  pure AppState {..}

renderFont :: TT.FontDescriptor -> TT.PointSize -> String -> GUIM Picture
renderFont fontd pt str = do
  state <- ask
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

clearCache :: World -> IO ()
clearCache w = do
  writeIORef (pictureCache w) Nothing

data Mouse = Hovering Point | MB Point MouseButton KeyState
  deriving Show

mouseInteractionButton :: Mouse -> BBox -> Picture -> Picture
mouseInteractionButton (Hovering p) BBox {..} = case pointInBox p bboxBR bboxTL of
  True -> color red
  False -> id
mouseInteractionButton (MB p mb ks) BBox {..} = case pointInBox p bboxBR bboxTL of
  True -> case mb of
    LeftButton -> case ks of
      Down -> color green
      Up -> color white
    _ -> id
  False -> id

didPress :: Mouse -> BBox -> Bool
didPress (MB p LeftButton Up) BBox {..} = pointInBox p bboxBR bboxTL
didPress _ _ = False

mainWith :: Settings -> GUIM () -> IO ()
mainWith settings gui' = do
  state <- newState
  let render' = runM . runReader state . render
  initGui <- render' gui'
  initPcache <- newIORef $ Just $ Pictures initGui
  interactIO
    (mainWindow settings)
    (bgColor settings)
    World
      { worldGui = gui'
      , pictureCache = initPcache
      }
    (\world -> do
      pcache <- readIORef (pictureCache world)
      mouse' <- liftIO $ readIORef $ mouse state
      x <- case pcache of
        Just g -> pure g
        Nothing -> Pictures <$> render' (worldGui world)
      case mouse' of
        MB p LeftButton Up -> writeIORef (mouse state) (Hovering p) -- we have to manually reset this since we're effectively doing (Event MousePos -> Dynamic MousePos) by storing the mousepos in an IORef. this seems kinda hacky
        _ -> pure ()
      pure x
    )
    (\e world -> case e of
      EventResize _dims -> pure world
      EventMotion p -> do
        writeIORef (mouse state) (Hovering p)
        clearCache world -- move to the controller handler
        pure world
      EventKey (MouseButton mb) keyState _mods p -> do
        writeIORef (mouse state) (MB p mb keyState)
        clearCache world -- move to the controller handler
        pure world
      EventKey _key _keyState _mods _coords -> pure world
    )
    (\controller -> do
      -- update every n seconds
      -- clearCache world
      pure ()
    )

guiIO :: forall r a. (LastMember IO r) => Eff (GUI : r) a -> Eff r a
guiIO = interpretM @GUI @IO go
  where
  go :: forall x. GUI x -> IO x
  go = \case
    Button {} -> pure False
    PictureI {} -> pure ()
    Columns g -> go g
    Rows g -> go g

render :: (LastMember IO r, Member (Reader AppState) r) => Eff (GUI : r) b -> Eff r [Picture]
render gui = do
  appState <- ask
  fmap (DList.toList . snd) $ runWriter $ runGUI appState gui

pictureBBox :: Picture -> Maybe BBox
pictureBBox = \case
  Line p -> pathBBox p
  Polygon p -> pathBBox p
  Pictures ps -> fmap sconcat $ NonEmpty.nonEmpty $ mapMaybe pictureBBox ps
  _ -> Nothing
  where
  pathBBox :: Path -> Maybe BBox
  pathBBox [] = Nothing
  pathBBox [_] = Nothing
  pathBBox xs = Just $ BBox
    { bboxBR = (maximum $ fmap fst xs, minimum $ fmap snd xs)
    , bboxTL = (minimum $ fmap fst xs, maximum $ fmap snd xs)
    }

runGUI :: forall r a. LastMember IO r => AppState -> Eff (GUI : r) a -> Eff (Writer (DList Picture) : r) a
runGUI appState sem = do
  evalState (0.0, 0.0) $ reinterpret2 withRows sem
  where
  withColumns :: forall x r'. LastMember IO r' => GUI x -> Eff (State (Float, Float) : Writer (DList Picture) : r') x
  withColumns g = do
    (_xo1 :: Float, yo1 :: Float) <- get
    res <- go g
    (xo2 :: Float, _yo2 :: Float) <- get
    -- bounding <- askBoundingBox
    put (xo2, yo1) -- each gui element only increments its x offset,
                   -- meaning all children of 'g' ask layed out left to right
    pure res
  withRows :: forall x r'. LastMember IO r' => GUI x -> Eff (State (Float, Float) : Writer (DList Picture) : r') x
  withRows g = do
    (xo1 :: Float, _yo1 :: Float) <- get
    res <- go g
    (_xo2 :: Float, yo2 :: Float) <- get
    -- bounding <- askBoundingBox
    put (xo1, yo2) -- each gui element only increments its y offset
                   -- meaning all children of 'g' ask layed out top to bottom
    pure res
  go :: forall x r'. LastMember IO r' => GUI x -> Eff (State (Float, Float) : Writer (DList Picture) : r') x
  go = \case
    Button gp x y -> do
      (xo, yo) <- get
      -- bboxes <- liftIO $ readIORef $ boundingBoxes appState
      let br = (xo + x / 2, yo - y / 2)
          tl = (xo - x / 2, yo + y / 2)
      let bbox = BBox br tl
      mouse' <- liftIO $ readIORef $ mouse appState
      (_, p) <- runWriter $ evalState (0.0, 0.0) $ go gp
      tell $ DList.fromList
        [ mouseInteractionButton mouse' bbox $ translate xo yo $ rectangleSolid x y
        , translate xo yo $ translate (negate $ x / 2) (negate $ y / 2) $ Pictures $ DList.toList p
        ]
      modify (\(xo', yo') -> (xo' + (x / 2) :: Float, yo' - y))
      pure $ didPress mouse' bbox
    PictureI p -> do
      (xo, yo) <- get
      tell (DList.singleton $ translate xo yo p)
      case pictureBBox p of
        Nothing -> pure ()
        Just (BBox (_, bottom) _) ->
          modify (\(xo', yo') -> (xo' :: Float, yo' - bottom))
      modify (\(xo', yo') -> (xo' :: Float, yo' - 20.0 :: Float))
      pure ()
    Columns g -> withColumns g
    Rows g -> withRows g
