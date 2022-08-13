{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

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
- cull the text you shouldn't see in the inputs
-}

defaultMain :: GUIM () -> IO ()
defaultMain = mainWith defaultSettings

newState :: Settings -> IO AppState
newState Settings {..} = do
#ifdef darwin_HOST_OS
  let fontCache   = TT.emptyFontCache -- too many file descriptors otherwise??
#else
  fontCache       <- TT.buildCache
#endif
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
  , dpi = 96
  , textConfig = defaultTextConfig
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
      mouse' <- readIORef $ mouse state
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

render :: (Member IO r, Member (Reader AppState) r, Member (Reader Settings) r) => Eff (GUI : r) b -> Eff r [Picture]
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

runGUI :: forall r a. Member IO r => Settings -> AppState -> Eff (GUI : r) a -> Eff (Writer (DList Picture) : r) a
runGUI settings appState sem = do
  (wX, wY) <- send $ readIORef $ windowSize appState
  let left = (fromIntegral $ negate $ wX `div` 2) + 5.0
      top  = (fromIntegral          $ wY `div` 2) - 22.0
  evalState (left, top) $ reinterpret2 withRows sem
  where
  withColumns :: forall x r'. Member IO r' => GUI x -> Eff (State (Float, Float) : Writer (DList Picture) : r') x
  withColumns g = do
    (_xo1 :: Float, yo1 :: Float) <- get
    res <- go g
    (xo2 :: Float, _yo2 :: Float) <- get
    -- bounding <- askBoundingBox
    put (xo2, yo1) -- each gui element only increments its x offset,
                   -- meaning all children of 'g' ask layed out left to right
    _ <- go $ Padding (xPad $ stylesheet settings) 0.0
    pure res
  withRows :: forall x r'. Member IO r' => GUI x -> Eff (State (Float, Float) : Writer (DList Picture) : r') x
  withRows g = do
    (xo1 :: Float, _yo1 :: Float) <- get
    res <- go g
    (_xo2 :: Float, yo2 :: Float) <- get
    -- bounding <- askBoundingBox
    put (xo1, yo2) -- each gui element only increments its y offset
                   -- meaning all children of 'g' ask layed out top to bottom
    _ <- go $ Padding 0.0 (yPad $ stylesheet settings)
    pure res
  go :: forall x r'. Member IO r' => GUI x -> Eff (State (Float, Float) : Writer (DList Picture) : r') x
  go = \case
    Button gp x y -> do
      (xo, yo) <- get
      -- bboxes <- send $ readIORef $ boundingBoxes appState
      let
        br = (xo + x, yo - y / 2)
        tl = (xo - x, yo + y / 2)
      let bbox = BBox br tl
      mouse' <- send $ readIORef $ mouse appState
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
      inputMap <- send $ readIORef (inputState appState)
      InputState strRef ixRef ia <- case IntMap.lookup ident inputMap of
        Nothing -> do
          send $ do
            strRef <- newIORef initialValue
            ixRef <- newIORef 0
            let is = InputState strRef ixRef InputInactive
            modifyIORef' (inputState appState) (IntMap.insert ident is)
            pure is
        Just strRef -> pure strRef
      str <- send $ readIORef strRef
      mouse' <- send $ readIORef $ mouse appState
      strPic <- runReader appState $ runReader settings $ textP' ((textConfig $ stylesheet settings) { texture = blackTexture }) str
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
      f <- runReader appState $ lookupOrInsertFont defaultFont
      let Stylesheet{..} = stylesheet settings
      let notPressed = do
            ix <- send $ readIORef ixRef
            runReader appState $ runReader settings $ textBBox textConfig $ take ix str
      BBox (cursorOffset, _) _ <-
        if pressed
        then do
          let p = mousePosPt mouse'
          BBox (co, _) _ <- notPressed
          let prevTl = if co > x
                then (co - x)
                else 0.0
          let (bb, pressedIx) = closestChar (prevTl + fst p - xo, snd p - yo) f dpi (ptsz textConfig) str
          ixRef' <- send $ newIORef pressedIx
          send $ modifyIORef' (inputState appState) (IntMap.insert ident (InputState strRef ixRef' InputActive))
          pure bb
        else notPressed
      let tlText =
            if cursorOffset > x
            then translate (-4.0 + ((negate $ cursorOffset - x))) 0.0
            else id
      let cursor = case ia of
            InputInactive -> blank
            _ -> translate (xo + 1.0 + cursorOffset) yo $ color black $ rectangleSolid 2.0 (y - 8.0)
          -- recBitmapSection
          -- rectangle = Rectangle
          --   { rectPos = (0, 0)
          --   , rectSize = (floor x, floor y)
          --   }
      tell $ DList.fromList
        [ translate (xo + (x / 2)) yo $ color white $ rectangleSolid x y
        , translate xo yo $ translate 0 (negate $ y / 4) $ tlText $ strPic
        , tlText cursor
        ]
      modify (\(xo', yo') -> (xo' + (x / 2) :: Float, yo' - y))
      pure str
    Text' (TextConfig {..}) s -> do
      (xo, yo) <- get
      strPic <- runReader appState $ runReader settings $ renderFont font ptsz texture s
      let top = TT.pointInPixelAtDpi ptsz (dpi $ stylesheet settings)
      BBox (right, _) (_, _) <- runReader appState $ runReader settings $ textBBox (textConfig $ stylesheet settings) s
      tell $ DList.singleton $
        translate 0 (negate $ top/4) $ translate xo yo $ strPic
      modify (\(xo', yo') -> (xo' + right, yo' - top))
      pure ()
    Columns g -> withColumns g
    Rows g -> withRows g

recBitmapSection :: Rectangle -> Picture -> Picture
recBitmapSection r p' = case p' of
  Bitmap bmd -> bitmapSection r bmd
  Color c p -> Color c (recBitmapSection r p)
  Translate x y p -> Translate x y (recBitmapSection r p)
  Rotate f p -> Rotate f (recBitmapSection r p)
  Scale x y p -> Scale x y (recBitmapSection r p)
  Pictures ps -> Pictures (fmap (recBitmapSection r) ps)
  x -> x
