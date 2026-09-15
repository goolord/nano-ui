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
  , rgfwCoreTheme
  , newRgfwContext
  , applyRgfwTheme
  , RgfwDebugSnapshot (..)
  , askRgfwDebug
  , debugWindowBody
  , emptyRgfwDebug
  ) where

import NanoUI.Rgfw.Context (applyRgfwTheme, newRgfwContext)
import NanoUI.Rgfw.Debug
  ( RgfwDebugSnapshot (..)
  , askRgfwDebug
  , debugWindowBody
  , emptyRgfwDebug
  )
import NanoUI.Rgfw.Session
  ( RgfwOptions (..)
  , defaultRgfwOptions
  , runRgfwApp
  , runRgfwAppReduce
  , runRgfwAppReduceCustom
  )
import NanoUI.Rgfw.Theme
  ( RgfwTheme (..)
  , defaultDarkTheme
  , defaultLightTheme
  , tomorrowMinLightTheme
  , tomorrowNightMinDarkTheme
  , tomorrowMidnightMinDarkTheme
  , rgfwCoreTheme
  )
