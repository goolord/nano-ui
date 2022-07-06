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
import System.Clipboard
import Data.Maybe (fromMaybe)

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

data TextConfig = TextConfig
  { font :: TT.FontDescriptor
  , texture :: Texture PixelRGBA8
  , ptsz :: TT.PointSize
  }

defaultTextConfig :: TextConfig
defaultTextConfig = TextConfig
  { font = openSans
  , texture = whiteTexture
  , ptsz = TT.PointSize 16
  }

whiteTexture :: Texture PixelRGBA8
whiteTexture = (uniformTexture $ PixelRGBA8 255 255 255 255)

blackTexture :: Texture PixelRGBA8
blackTexture = (uniformTexture $ PixelRGBA8 0 0 0 255)

text :: (Member (Reader AppState) r, LastMember IO r, Member GUI r) => String -> Eff r ()
text = text' defaultTextConfig

textP :: (Member (Reader AppState) r, LastMember IO r) => String -> Eff r Picture
textP = textP' defaultTextConfig

textP' :: (Member (Reader AppState) r, LastMember IO r) => TextConfig -> String -> Eff r Picture
textP' (TextConfig{..}) s = do
  renderFont font ptsz texture s

text' :: (Member (Reader AppState) r, LastMember IO r, Member GUI r) => TextConfig -> String -> Eff r ()
text' tc s = do
  textP' tc s >>= send . PictureI

textBBox :: (Member (Reader AppState) r, LastMember IO r) => String -> Eff r BBox
textBBox s = do
  f <- lookupOrInsertFont openSans
  pure $ ttBoundingBox $ TT.stringBoundingBox f dpi (TT.PointSize 16) s

ttBoundingBox :: TT.BoundingBox -> BBox
ttBoundingBox bb =
  let baseline = TT._baselineHeight bb
  in BBox (TT._xMax bb, TT._yMin bb + baseline) (TT._xMin bb, TT._yMax bb + baseline)

closestChar :: Point -> TT.Font -> TT.Dpi -> TT.PointSize -> String -> (BBox, Int)
closestChar (x, y) font dpi' size str =
  let bboxes = fmap (TT.stringBoundingBox font dpi' size) $ inits str
  in first ttBoundingBox $ minimumBy (comparing (\(bb, _) -> abs $ TT._xMax bb - x)) $ zip bboxes [0..]

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

renderFont :: (Member (Reader AppState) r, LastMember IO r) => TT.FontDescriptor -> TT.PointSize -> (Texture PixelRGBA8) -> String -> Eff r Picture
renderFont fontd pt texture str = do
  -- let ptF = getPointSize pt
  -- TODO: TODO: fsr the bottom of the text is cutoff until something like
  -- 'q' is present in the string
  --
  -- and the right is cut off
  --
  -- it's also shaking around when i type so something is not getting done properly
  -- maybe something to do with the translate calculation `~`
  --
  -- the font might be rendering bigger than
  -- using the old method + line
  f <- lookupOrInsertFont fontd
  let bb = ttBoundingBox $ TT.stringBoundingBox f dpi pt str
      w = (maxX bb - minX bb)
      h = (maxY bb - minY bb)
  let bs = encodeBitmap $ renderDrawing (floor w) (floor h) (PixelRGBA8 255 0 0 0) $
        traverse_ orderToDrawing $ textToDrawOrders dpi texture (V2 0.0 (maxY bb)) [TextRange f pt str Nothing]
  let bmp = either (error . show) id $ parseBMP bs
  pure $
    color red (line [bboxBR bb, bboxTL bb])
    <> (translate ((w / 2) - minX bb) ((h / 2) + minY bb) $ bitmapOfBMP bmp)

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

