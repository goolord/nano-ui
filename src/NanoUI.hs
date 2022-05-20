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

module NanoUI
  ( module NanoUI
  , module NanoUI.Types
  )
  where

import NanoUI.Types

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
import Graphics.Gloss hiding (text)
import Graphics.Gloss.Interface.IO.Interact (interactIO, Event (..), Key (..), MouseButton (..), KeyState (..))
import qualified Data.DList as DList
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as VU
import qualified Graphics.Text.TrueType as TT
import Graphics.Gloss.Data.Point (pointInBox)
import Data.Maybe (mapMaybe)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Semigroup (Semigroup(sconcat))
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap (IntMap)
import Control.Monad (when)
import Data.Foldable (for_)

deriving instance Generic TT.FontDescriptor
deriving instance Generic TT.FontStyle
instance Hashable TT.FontDescriptor
instance Hashable TT.FontStyle

openSans :: TT.FontDescriptor
openSans = TT.FontDescriptor "Open Sans" (TT.FontStyle False False)

textP :: (Member (Reader AppState) r, LastMember IO r) => String -> Eff r Picture
textP s = do
  renderFont openSans (TT.PointSize 16) s

text :: (Member (Reader AppState) r, LastMember IO r, Member GUI r) => String -> Eff r ()
text s = do
  p <- renderFont openSans (TT.PointSize 16) s
  send $ PictureI $ decorateText p

decorateText :: Picture -> Picture
decorateText = color white

defaultMain :: GUIM () -> IO ()
defaultMain = mainWith defaultSettings

newState :: IO AppState
newState = do
  fontCache       <- TT.buildCache 
  loadedFontCache <- newIORef HM.empty
  mouse           <- newIORef (Hovering (0, 0))
  inputState      <- newIORef IntMap.empty
  pure AppState {..}

renderFont :: (Member (Reader AppState) r, LastMember IO r) => TT.FontDescriptor -> TT.PointSize -> String -> Eff r Picture
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

clearCache :: World -> IO ()
clearCache w = do
  writeIORef (pictureCache w) Nothing

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

defaultSettings :: Settings
defaultSettings = Settings
  { tickRate = 60
  , bgColor = makeColorI 0x28 0x28 0x28 0xff
  , mainWindow = InWindow "Wave" (800, 600) (100, 100)
  }

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
        case keyState of
          Up -> modifyIORef' (inputState state) (fmap disableInput)
          _ -> pure ()
        clearCache world -- move to the controller handler
        pure world
      EventKey (Char c) Down _mods _coords -> do
        inputMap <- readIORef (inputState state)
        for_ (IntMap.filter inputIsActive inputMap) $ \(InputState strRef _) -> do
          modifyIORef' strRef (<> [c])
        clearCache world -- move to the controller handler
        pure world
      EventKey _key _keyState _mods _coords -> pure world
    )
    (\controller -> do
      -- update every n seconds
      -- clearCache world
      pure ()
    )

disableInput :: InputState -> InputState
disableInput (InputState strRef _) = InputState strRef InputInactive

inputIsActive :: InputState -> Bool
inputIsActive (InputState _ InputActive) = True
inputIsActive _ = False

{-
guiIO :: forall r a. (LastMember IO r) => Eff (GUI : r) a -> Eff r a
guiIO = interpretM @GUI @IO go
  where
  go :: forall x. GUI x -> IO x
  go = \case
    Button {} -> pure False
    PictureI {} -> pure ()
    Columns g -> go g
    Rows g -> go g
    Padding {} -> pure ()
    Input {} -> pure ""
-}

render :: (LastMember IO r, Member (Reader AppState) r) => Eff (GUI : r) b -> Eff r [Picture]
render gui = do
  appState <- ask
  fmap (DList.toList . snd) $ runWriter $ runGUI appState gui

pictureBBox :: Picture -> Maybe BBox
pictureBBox = \case
  Color _ p -> pictureBBox p
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
      let
        br = (xo + x, yo - y / 2)
        tl = (xo - x, yo + y / 2)
      let bbox = BBox br tl
      mouse' <- liftIO $ readIORef $ mouse appState
      (_, p) <- runWriter $ evalState (0.0, 0.0) $ go gp
      tell $ DList.fromList
        [ mouseInteractionButton mouse' bbox $ translate (xo + (x / 2)) yo $ rectangleSolid x y
        , translate xo yo $ translate 0 (negate $ (y / 2) / 2) $ Pictures $ DList.toList p
        ]
      modify (\(xo', yo') -> (xo' + (x / 2) :: Float, yo' - y))
      pure $ didPress mouse' bbox
    PictureI p -> do
      (xo, yo) <- get
      tell (DList.singleton $ translate xo yo p)
      case pictureBBox p of
        Nothing -> pure () -- error (show p)
        Just (BBox (_, _) (_, top)) ->
          modify (\(xo', yo') -> (xo' :: Float, yo' - top))
      pure ()
    Padding x y -> do
      modify (\(xo', yo') -> (xo' + x, yo' - y))
    Input ident _placeholder initialValue -> do
      (xo, yo) <- get
      inputMap <- liftIO $ readIORef (inputState appState)
      InputState strRef ia <- case IntMap.lookup ident inputMap of
        Nothing -> liftIO $ do
          strRef <- newIORef initialValue
          let is = InputState strRef InputInactive
          modifyIORef' (inputState appState) (IntMap.insert ident is)
          pure is
        Just strRef -> pure strRef
      str <- liftIO $ readIORef strRef
      mouse' <- liftIO $ readIORef $ mouse appState
      strPic <- runReader appState $ textP str
      let x = 100.0
          y = 30.0
      let
        br = (xo + x, yo - y / 2)
        tl = (xo - x, yo + y / 2)
      let bbox = BBox br tl
      tell $ DList.fromList
        [ translate (xo + (x / 2)) yo $ color white $ rectangleSolid x y
        , translate xo yo $ translate 0 (negate $ (y / 2) / 2) $ color black strPic
        -- , translate xo yo $ translate 0 (negate $ (y / 2) / 2) $ Pictures $ DList.toList [blank]
        ]
      modify (\(xo', yo') -> (xo' + (x / 2) :: Float, yo' - y))
      when (didPress mouse' bbox) $ do
        liftIO $ modifyIORef' (inputState appState) (IntMap.insert ident (InputState strRef InputActive))
      pure str
    Columns g -> withColumns g
    Rows g -> withRows g
