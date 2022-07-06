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
  , module NanoUI.Widgets
  )
  where

import NanoUI.Types
import NanoUI.Widgets

import Control.Monad.Freer hiding (translate)
import Control.Monad.Freer.Reader
import Control.Monad.Freer.State
import Control.Monad.Freer.Writer
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.DList (DList)
import Data.IORef
import Data.Maybe (mapMaybe)
import Data.Semigroup (Semigroup(sconcat))
import Graphics.Gloss hiding (text)
import Graphics.Gloss.Interface.Environment (getScreenSize)
import Graphics.Gloss.Interface.IO.Interact (interactIO, Event (..), Key (..), MouseButton (..), KeyState (..), Controller (..))
import qualified Data.DList as DList
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List.NonEmpty as NonEmpty
import qualified Graphics.Text.TrueType as TT
import Control.Monad (unless)

{-
todo: 
- render font as a bitmap
  so we can do proper font rendering
  and also cull the text you shouldn't see in the inputs

- factor out the global font size constants
-}

defaultMain :: GUIM () -> IO ()
defaultMain = mainWith defaultSettings

newState :: Settings -> IO AppState
newState Settings {..} = do
  fontCache       <- TT.buildCache
  loadedFontCache <- newIORef HM.empty
  mouse           <- newIORef (Hovering (0, 0))
  inputState      <- newIORef IntMap.empty
  windowSize      <- newIORef =<< case mainWindow of
    InWindow _ sz _ -> pure sz
    FullScreen -> getScreenSize
  pure AppState {..}

defaultSettings :: Settings
defaultSettings = Settings
  { tickRate = 60
  , bgColor = makeColorI 0x28 0x28 0x28 0xff
  , mainWindow = InWindow "Wave" (800, 600) (100, 100)
  , stylesheet = defaultStylesheet
  }

defaultStylesheet :: Stylesheet
defaultStylesheet = Stylesheet
  { xPad = 5.0
  , yPad = 15.0
  , font = TT.FontDescriptor "Open Sans" (TT.FontStyle False False)
  , fontPt = TT.PointSize 16
  }

mainWith :: Settings -> GUIM () -> IO ()
mainWith settings gui' = do
  state <- newState settings
  let render' = runM . runReader settings . runReader state . render
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
    (\e world -> do
      world2 <- inputEvents e world state
      case e of
        EventResize dims -> do
          writeIORef (windowSize state) dims
          pure world2
        EventMotion p -> do
          writeIORef (mouse state) (Hovering p)
          clearCache world2 -- move to the controller handler
          pure world2
        EventKey (MouseButton mb) keyState _mods p -> do
          writeIORef (mouse state) (MB p mb keyState)
          clearCache world2 -- move to the controller handler
          pure world2
        _ -> pure world2
    )
    (\(Controller _redraw _modifyViewPort) -> do
      -- update every n seconds
      -- clearCache world
      -- modifyViewPort $ \vp -> do
      --   (x, y) <- readIORef (windowSize state)
      --   pure vp {
      --     viewPortTranslate =
      --       ( fromIntegral $ negate $ x `div` 2
      --       , fromIntegral          $ y `div` 2
      --       )
      --   }
      pure ()
    )

render :: (LastMember IO r, Member (Reader AppState) r, Member (Reader Settings) r) => Eff (GUI : r) b -> Eff r [Picture]
render gui = do
  appState <- ask
  settings <- ask
  fmap (DList.toList . snd) $ runWriter $ runGUI settings appState gui

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

runGUI :: forall r a. LastMember IO r => Settings -> AppState -> Eff (GUI : r) a -> Eff (Writer (DList Picture) : r) a
runGUI settings appState sem = do
  (wX, wY) <- send $ readIORef $ windowSize appState
  let left = (fromIntegral $ negate $ wX `div` 2) + 5.0
      top  = (fromIntegral          $ wY `div` 2) - 22.0
  evalState (left, top) $ reinterpret2 withRows sem
  where
  withColumns :: forall x r'. LastMember IO r' => GUI x -> Eff (State (Float, Float) : Writer (DList Picture) : r') x
  withColumns g = do
    (_xo1 :: Float, yo1 :: Float) <- get
    res <- go g
    (xo2 :: Float, _yo2 :: Float) <- get
    -- bounding <- askBoundingBox
    put (xo2, yo1) -- each gui element only increments its x offset,
                   -- meaning all children of 'g' ask layed out left to right
    _ <- go $ Padding (xPad $ stylesheet settings) 0.0
    pure res
  withRows :: forall x r'. LastMember IO r' => GUI x -> Eff (State (Float, Float) : Writer (DList Picture) : r') x
  withRows g = do
    (xo1 :: Float, _yo1 :: Float) <- get
    res <- go g
    (_xo2 :: Float, yo2 :: Float) <- get
    -- bounding <- askBoundingBox
    put (xo1, yo2) -- each gui element only increments its y offset
                   -- meaning all children of 'g' ask layed out top to bottom
    _ <- go $ Padding 0.0 (yPad $ stylesheet settings)
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
      InputState strRef ixRef ia <- case IntMap.lookup ident inputMap of
        Nothing -> do
          liftIO $ do
            strRef <- newIORef initialValue
            ixRef <- newIORef 0
            let is = InputState strRef ixRef InputInactive
            modifyIORef' (inputState appState) (IntMap.insert ident is)
            pure is
        Just strRef -> pure strRef
      str <- liftIO $ readIORef strRef
      mouse' <- liftIO $ readIORef $ mouse appState
      strPic <- runReader appState $ textP str
      let x = 200.0
          y = 30.0
      let
        br = (xo + x, yo - y / 2)
        tl = (xo - x, yo + y / 2)
      let bbox = BBox br tl
      let pressed = didPress mouse' bbox
      case mouse' of
        MB _ LeftButton Down -> do
          send $ unless pressed $ modifyIORef' (inputState appState) (IntMap.adjust disableInput ident)
        _ -> pure ()
      -- unhardcode this somehow
      f <- runReader appState $ lookupOrInsertFont openSans
      let pt = TT.PointSize 16
      let notPressed = do
            ix <- liftIO $ readIORef ixRef
            runReader appState $ textBBox $ take ix str
      BBox (cursorOffset, _) _ <-
        if pressed
        then do
          let p = mousePosPt mouse'
          BBox (co, _) _ <- notPressed
          let prevTl = if co > x
                then (co - x)
                else 0.0
          let (bb, pressedIx) = closestChar (prevTl + fst p - xo, snd p - yo) f dpi pt str
          ixRef' <- liftIO $ newIORef pressedIx
          liftIO $ modifyIORef' (inputState appState) (IntMap.insert ident (InputState strRef ixRef' InputActive))
          pure bb
        else notPressed
      let tlText =
            if cursorOffset > x
            then translate (negate $ cursorOffset - x) 0.0
            else id
      let cursor = case ia of
            InputInactive -> blank
            _ -> translate (xo + 1.0 + cursorOffset) yo $ color black $ rectangleSolid 2.0 (y - 8.0)
      tell $ DList.fromList
        [ translate (xo + (x / 2)) yo $ color white $ rectangleSolid x y
        , translate xo yo $ translate 0 (negate $ (y / 2) / 2) $ tlText $ color black strPic
        , tlText cursor
        ]
      modify (\(xo', yo') -> (xo' + (x / 2) :: Float, yo' - y))
      pure str
    Columns g -> withColumns g
    Rows g -> withRows g

