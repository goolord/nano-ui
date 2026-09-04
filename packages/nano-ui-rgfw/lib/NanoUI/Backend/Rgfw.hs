module NanoUI.Backend.Rgfw
  ( runRgfwApp
  , runRgfwAppReduce
  , runRgfwAppReduceCustom
  , RgfwOptions (..)
  , defaultRgfwOptions
  , RgfwTheme (..)
  , defaultDarkTheme
  , defaultLightTheme
  , tomorrowMinLightTheme
  , tomorrowNightMinDarkTheme
  , tomorrowMidnightMinDarkTheme
  , RgfwDebugSnapshot (..)
  , RgfwDebugSampler
  , RgfwDebugHost (..)
  , askRgfwDebug
  , debugWindowBody
  , allDebugRows
  , frameRows
  , layoutRows
  , displayRows
  , rtsRows
  , emptyRgfwDebug
  , newRgfwDebugSampler
  , readRgfwDebug
  ) where

import Data.Typeable (Typeable)
import NanoUI (NanoUI)
import NanoUI.Rgfw.Debug
  ( RgfwDebugHost (..)
  , RgfwDebugSampler
  , RgfwDebugSnapshot (..)
  , allDebugRows
  , askRgfwDebug
  , debugWindowBody
  , displayRows
  , emptyRgfwDebug
  , frameRows
  , layoutRows
  , newRgfwDebugSampler
  , readRgfwDebug
  , rtsRows
  )
import NanoUI.Rgfw.Session
  ( RgfwOptions (..)
  , defaultRgfwOptions
  , runRgfwSession
  , runRgfwSessionReduce
  , runRgfwSessionReduceCustom
  )
import NanoUI.Rgfw.Theme
  ( RgfwTheme (..)
  , defaultDarkTheme
  , defaultLightTheme
  , tomorrowMinLightTheme
  , tomorrowNightMinDarkTheme
  , tomorrowMidnightMinDarkTheme
  )

runRgfwApp :: RgfwOptions -> NanoUI () -> IO ()
runRgfwApp = runRgfwSession

runRgfwAppReduce ::
  (Typeable msg, Eq model) =>
  RgfwOptions ->
  (msg -> model -> model) ->
  model ->
  (model -> NanoUI ()) ->
  IO ()
runRgfwAppReduce = runRgfwSessionReduce

runRgfwAppReduceCustom ::
  (Typeable msg, Eq model) =>
  RgfwOptions ->
  (model -> (RgfwTheme, Float)) ->
  (msg -> model -> model) ->
  model ->
  (model -> NanoUI ()) ->
  IO ()
runRgfwAppReduceCustom = runRgfwSessionReduceCustom
