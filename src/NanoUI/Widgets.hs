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

module NanoUI.Widgets where

import NanoUI.Types
import NanoUI.Internal

import Control.Monad.Freer hiding (translate)
import Control.Monad.Freer.Reader
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Bifunctor
import Data.Hashable (Hashable)
import Data.IORef
import GHC.Generics (Generic)
import Graphics.Gloss hiding (text)
import Graphics.Gloss.Interface.IO.Interact (Event (..), Key (..), MouseButton (..), KeyState (..), SpecialKey (..), Modifiers (..))
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as VU
import qualified Graphics.Text.TrueType as TT
import Graphics.Gloss.Data.Point (pointInBox)
import qualified Data.IntMap.Strict as IntMap
import Data.Foldable (for_)
import Data.List (dropWhileEnd, minimumBy, inits)
import Data.Char (isAlphaNum, isSpace, isPunctuation)
import Data.Ord (comparing)

deriving instance Generic TT.FontDescriptor
deriving instance Generic TT.FontStyle
instance Hashable TT.FontDescriptor
instance Hashable TT.FontStyle

---------------------------------------------------------------------
-- constants
---------------------------------------------------------------------

dpi :: TT.Dpi
dpi = 96

openSans :: TT.FontDescriptor
openSans = TT.FontDescriptor "Open Sans" (TT.FontStyle False False)

---------------------------------------------------------------------
-- Widgets & helper functions
---------------------------------------------------------------------
textP :: (Member (Reader AppState) r, LastMember IO r) => String -> Eff r Picture
textP s = do
  renderFont openSans (TT.PointSize 16) s

text :: (Member (Reader AppState) r, LastMember IO r, Member GUI r) => String -> Eff r ()
text s = do
  p <- renderFont openSans (TT.PointSize 16) s
  send $ PictureI $ decorateText p

textBBox :: (Member (Reader AppState) r, LastMember IO r) => String -> Eff r BBox
textBBox s = do
  f <- lookupOrInsertFont openSans
  pure $ ttBoundingBox $ TT.stringBoundingBox f dpi (TT.PointSize 16) s

ttBoundingBox :: TT.BoundingBox -> BBox
ttBoundingBox bb =
  let baseline = TT._baselineHeight bb
  in BBox (TT._xMax bb + baseline, TT._yMax bb + baseline) (TT._xMin bb + baseline, TT._yMax bb + baseline)

closestChar :: Point -> TT.Font -> TT.Dpi -> TT.PointSize -> String -> (BBox, Int)
closestChar (x, y) font dpi' size str =
  let bboxes = fmap (TT.stringBoundingBox font dpi' size) $ inits str
  in first ttBoundingBox $ minimumBy (comparing (\(bb, _) -> abs $ TT._xMax bb - x)) $ zip bboxes [0..]

decorateText :: Picture -> Picture
decorateText = color white

lookupOrInsertFont :: (LastMember IO effs, Member (Reader AppState) effs) => TT.FontDescriptor -> Eff effs TT.Font
lookupOrInsertFont fontd = do
  state <- ask
  loaded <- liftIO $ readIORef $ loadedFontCache state
  case HM.lookup fontd loaded of
    Just f -> pure f
    Nothing -> do
      case TT.findFontInCache (fontCache state) fontd of
        Nothing -> error $ unwords ["font", show fontd, "missing"]
        Just fp -> do
          f <- either (error . show) id <$> (liftIO $ TT.loadFontFile fp)
          liftIO $ modifyIORef' (loadedFontCache state) (HM.insert fontd f)
          pure f

renderFont :: (Member (Reader AppState) r, LastMember IO r) => TT.FontDescriptor -> TT.PointSize -> String -> Eff r Picture
renderFont fontd pt str = do
  f <- lookupOrInsertFont fontd
  -- todo: interpret as Polygon instead of Line
  pure $ foldMap (line . fmap (second negate) . VU.toList) $ mconcat $ TT.getStringCurveAtPoint
    dpi
    (0.0, 0.0)
    [(f,pt,str)]

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

dropWordL :: [Char] -> [Char]
dropWordL = dropWhileEnd (\x -> isAlphaNum x || isPunctuation x) . dropWhileEnd isSpace

dropWordR :: [Char] -> [Char]
dropWordR = dropWhile (\x -> isAlphaNum x || isPunctuation x) . dropWhile isSpace

inputEvents :: Event -> World -> AppState -> IO World
inputEvents e world state = case e of
  EventKey (Char '\b') Down mods _coords ->
    case ctrl mods of
      Down -> inputEv dropWordL id
      Up -> inputEv safeInit id
  EventKey (SpecialKey KeyDelete) Down mods _coords ->
    case ctrl mods of
      Down -> inputEv id dropWordR
      Up -> inputEv id safeTail
  EventKey (Char c) Down _mods _coords -> do
    inputEv (<> [c]) id
    -- print c
  EventKey (SpecialKey KeyLeft) Down mods _coords -> do
    is <- readIORef $ inputState state
    case ctrl mods of
      Up -> do
        mapM_ (overIndex (\_ x -> x-1)) is
        pure world
      Down -> do -- TODO: move by words
        mapM_ (overIndex (\str ix ->
            let (strL, _) = splitAt ix str
                newStr = dropWordL strL
            in ix-(length strL - length newStr)
          )) is
        pure world
  EventKey (SpecialKey KeyRight) Down mods _coords -> do
    is <- readIORef $ inputState state
    case ctrl mods of
      Up -> do
        mapM_ (overIndex (\_ x -> x+1)) is
        pure world
      Down -> do -- TODO: move by words
        mapM_ (overIndex (\str ix ->
            let (_, strR) = splitAt ix str
                newStr = dropWordR strR
            in ix+(length strR - length newStr)
          )) is
        pure world
  EventKey (SpecialKey KeyHome) Down _mods _coords -> do
    is <- readIORef $ inputState state
    mapM_ (overIndex (\_ _ -> 0)) is
    pure world
  EventKey (SpecialKey KeyEnd) Down _mods _coords -> do
    is <- readIORef $ inputState state
    mapM_ (overIndex (\str _ -> length str)) is
    pure world
  EventKey (SpecialKey KeySpace) Down _mods _coords ->
    inputEv (<> " ") id
  _ -> pure world
  where
  inputEv :: (String -> String) -> (String -> String) -> IO World -- move this to a state handler or maybe the drawing stage
  inputEv fL fR = do
    inputMap <- readIORef (inputState state)
    for_ (IntMap.filter inputIsActive inputMap) $ \(InputState strRef (InputActive ixRef)) -> do
      initString <- readIORef strRef
      ix <- readIORef ixRef
      let (strL, strR) = splitAt ix initString
          newStringL = fL strL
          newStringR = fR strR
          diffL = length newStringL - length strL
          newString = newStringL <> newStringR
      writeIORef strRef newString
      modifyIORef' ixRef (+ diffL)
    -- print c
    clearCache world -- move to the controller handler
    pure world

overIndex :: (String -> Int -> Int) -> InputState -> IO ()
overIndex f (InputState strRef (InputActive ixRef)) = do
  str <- readIORef strRef
  ix <- readIORef ixRef
  writeIORef ixRef $ max 0 $ min (length str) (f str ix)
overIndex _ _ = pure ()

disableInput :: InputState -> InputState
disableInput (InputState strRef _) = InputState strRef InputInactive

inputIsActive :: InputState -> Bool
inputIsActive (InputState _ InputActive{}) = True
inputIsActive _ = False

didPress :: Mouse -> BBox -> Bool
didPress (MB p LeftButton Down) BBox {..} = pointInBox p bboxBR bboxTL
didPress _ _ = False

