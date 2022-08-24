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
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE CPP #-}

module NanoUI.Widgets where

import NanoUI.Types
import NanoUI.Internal

import Control.Monad.Freer hiding (translate)
import Control.Monad.Freer.Reader
import Data.Bifunctor
import Data.Hashable (Hashable)
import Data.IORef
import GHC.Generics (Generic)
import Graphics.Gloss hiding (text)
import Graphics.Gloss.Interface.IO.Interact (Event (..), Key (..), MouseButton (..), KeyState (..), SpecialKey (..), Modifiers (..))
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Graphics.Text.TrueType as TT
import Graphics.Gloss.Data.Point (pointInBox)
import qualified Data.IntMap.Strict as IntMap
import Data.Foldable (for_, traverse_)
import Data.List (dropWhileEnd, minimumBy, inits)
import Data.Char (isAlphaNum, isSpace, isPunctuation)
import Data.Ord (comparing)
import Graphics.Rasterific (V2 (..), renderDrawing, TextRange (..), Texture)
import Graphics.Rasterific.Texture (uniformTexture)
import Codec.Picture (PixelRGBA8(..), encodeBitmap)
import Codec.BMP (parseBMP)
import Graphics.Rasterific.Immediate (textToDrawOrders, orderToDrawing)
-- import System.Clipboard
import Data.Maybe (fromMaybe)
import System.Clipboard (getClipboardString)

deriving instance Generic TT.FontDescriptor
deriving instance Generic TT.FontStyle
instance Hashable TT.FontDescriptor
instance Hashable TT.FontStyle

---------------------------------------------------------------------
-- constants
---------------------------------------------------------------------

defaultFont :: TT.FontDescriptor
defaultFont =
#ifdef darwin_HOST_OS
  TT.FontDescriptor "Arial" (TT.FontStyle False False)
#else
  TT.FontDescriptor "Open Sans" (TT.FontStyle False False)
#endif

---------------------------------------------------------------------
-- Widgets & helper functions
---------------------------------------------------------------------

defaultTextConfig :: TextConfig
defaultTextConfig = TextConfig
  { font = defaultFont
  , texture = whiteTexture
  , ptsz = TT.PointSize 16
  }

whiteTexture :: Texture PixelRGBA8
whiteTexture = (uniformTexture $ PixelRGBA8 255 255 255 255)

blackTexture :: Texture PixelRGBA8
blackTexture = (uniformTexture $ PixelRGBA8 0 0 0 255)

text :: (Member (Reader AppState) r, Member IO r, Member GUI r) => String -> Eff r ()
text = text' defaultTextConfig

-- TODO: consider banning
textP :: (Member (Reader AppState) r, Member (Reader Settings) r, Member IO r) => String -> Eff r Picture
textP = textP' defaultTextConfig

textP' :: (Member (Reader AppState) r, Member (Reader Settings) r, Member IO r) => TextConfig -> String -> Eff r Picture
textP' (TextConfig{..}) s = do
  renderFont font ptsz texture s

-- text' :: (Member (Reader AppState) r, Member IO r, Member GUI r) => TextConfig -> String -> Eff r ()
-- text' tc s = do
--   textP' tc s >>= send . PictureI

textBBox :: (Member (Reader AppState) r, Member IO r, Member (Reader Settings) r) => TextConfig -> String -> Eff r BBox
textBBox TextConfig{..} s = do
  settings <- ask
  f <- lookupOrInsertFont font
  pure $ ttBoundingBox $ TT.stringBoundingBox f (dpi $ stylesheet settings) ptsz s

ttBoundingBox :: TT.BoundingBox -> BBox
ttBoundingBox bb =
  let baseline = TT._baselineHeight bb
  in BBox (TT._xMax bb, TT._yMin bb + baseline) (TT._xMin bb, TT._yMax bb + baseline)

-- todo: do we even care about y in `closestChar`?
-- text with newlines might have to be split up in such a way
-- that the y comparison here is unecessary
closestChar :: Point -> TT.Font -> TT.Dpi -> TT.PointSize -> String -> (BBox, Int)
closestChar (x, y) font dpi' size str =
  let bboxes = fmap (TT.stringBoundingBox font dpi' size) $ inits str
  in first ttBoundingBox $ minimumBy (comparing (\(bb, _) -> (abs $ TT._xMax bb - x, abs $ TT._yMax bb - y))) $ zip bboxes [0..]

-- data BBox = BBox
--   { bboxBR :: !Point
--   , bboxTL :: !Point
--   }

drawBBox :: BBox -> Picture
drawBBox bb = color red $ line
  [ (minX bb, minY bb), bboxBR bb
  , (maxX bb, maxY bb), bboxTL bb
  , (minX bb, minY bb)
  ]

lookupOrInsertFont :: (Member IO effs, Member (Reader AppState) effs) => TT.FontDescriptor -> Eff effs TT.Font
lookupOrInsertFont fontd = do
  state <- ask
  loaded <- send $ readIORef $ loadedFontCache state
  case HM.lookup fontd loaded of
    Just f -> pure f
    Nothing -> do
      fp <- case TT.findFontInCache (fontCache state) fontd of
        Nothing -> do
          mfp <- send $ TT.findFontOfFamily (T.unpack $ TT._descriptorFamilyName fontd) (TT._descriptorStyle fontd)
          case mfp of
            Nothing -> error $ unwords ["font", show fontd, "missing"]
            Just fp -> pure fp
        Just fp -> pure fp
      f <- either (error . show) id <$> (send $ TT.loadFontFile fp)
      send $ modifyIORef' (loadedFontCache state) (HM.insert fontd f)
      pure f

renderFont :: (Member (Reader AppState) r, Member IO r, Member (Reader Settings) r) => TT.FontDescriptor -> TT.PointSize -> (Texture PixelRGBA8) -> String -> Eff r Picture
renderFont fontd pt texture str = do
  Stylesheet{..} <- stylesheet <$> ask
  f <- lookupOrInsertFont fontd
  let aaBuffer = 3
  let bb = ttBoundingBox $ TT.stringBoundingBox f dpi pt str
      w = (aaBuffer + maxX bb - minX bb)
      h = TT.pointInPixelAtDpi pt dpi
  let bs = encodeBitmap $ renderDrawing (round w) (round h * 2) (PixelRGBA8 255 0 0 0) $
        traverse_ orderToDrawing $ textToDrawOrders dpi texture (V2 0.0 h) [TextRange f pt str Nothing]
  let bmp = either (error . show) id $ parseBMP bs
  pure $ translate
    ((w / 2) - minX bb + 2)
    0 $ bitmapOfBMP bmp

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
  EventKey (Char '\SYN') Down (Modifiers {ctrl=Down}) _coords -> do
    mcb <- getClipboardString
    inputEv (<> fromMaybe mempty mcb) id
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
    for_ (IntMap.filter inputIsActive inputMap) $ \(InputState strRef ixRef InputActive) -> do
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
overIndex f (InputState strRef ixRef InputActive) = do
  str <- readIORef strRef
  ix <- readIORef ixRef
  writeIORef ixRef $ max 0 $ min (length str) (f str ix)
overIndex _ _ = pure ()

disableInput :: InputState -> InputState
disableInput (InputState strRef ixRef _) = InputState strRef ixRef InputInactive

inputIsActive :: InputState -> Bool
inputIsActive (InputState _ _ InputActive{}) = True
inputIsActive _ = False

didPress :: Mouse -> BBox -> Bool
didPress (MB p LeftButton Down) BBox {..} = pointInBox p bboxBR bboxTL
didPress _ _ = False
