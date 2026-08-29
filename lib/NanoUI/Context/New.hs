module NanoUI.Context.New
  ( newContext
  ) where

import Data.IORef (newIORef)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as Map
import qualified NanoUI.Atlas as Atlas
import NanoUI.Draw (newDrawArena)
import NanoUI.Types (Damage (..), Size (..))
import NanoUI.Store (emptyWidgetStore)
import NanoUI.Font (measureText, monospaceMetrics, stripWidgetMarkers)
import NanoUI.Host (HostProfile (..))
import NanoUI.Icons (asciiIcons)
import NanoUI.Id (WidgetId (..))
import NanoUI.Layout.Arena (newNodeArena)
import NanoUI.Style (defaultTheme)
import NanoUI.Context.Internal (Context (..))

{-# INLINE newContext #-}
newContext :: IO Context
newContext = do
  nodeArena <- newNodeArena
  drawArena <- newDrawArena
  ctxHotId <- newIORef (WidgetId 0)
  ctxLastHotId <- newIORef (WidgetId 0)
  ctxActiveId <- newIORef (WidgetId 0)
  ctxFocusId <- newIORef (WidgetId 0)
  ctxPrevRects <- newIORef IM.empty
  ctxStore <- newIORef emptyWidgetStore
  ctxAnimations <- newIORef IM.empty
  ctxAnimRest <- newIORef IM.empty
  ctxAnyAnimating <- newIORef False
  ctxAnimSettled <- newIORef False
  ctxDirty <- newIORef True
  ctxDamage <- newIORef DamageFull
  ctxLastWindowSize <- newIORef (Size 0 0)
  ctxIdSalt <- newIORef 0
  ctxContainerStack <- newIORef []
  ctxMessages <- newIORef []
  ctxFocusables <- newIORef []
  ctxScrollDrag <- newIORef Nothing
  ctxTextInputDrag <- newIORef Nothing
  ctxTextInputMenu <- newIORef Nothing
  ctxTooltips <- newIORef []
  ctxWidgetNodeTypes <- newIORef Nothing
  ctxSelectDropPress <- newIORef False
  ctxModalWasActive <- newIORef False
  ctxModalActive <- newIORef False
  ctxModalDepth <- newIORef 0
  ctxEscapeConsumed <- newIORef False
  ctxWindowDrag <- newIORef Nothing
  ctxWindowResize <- newIORef Nothing
  ctxPrevFloatingRects <- newIORef IM.empty
  ctxImageAtlas <- Atlas.newImageAtlas
  ctxWakeLoop <- newIORef Nothing
  ctxHost <- newIORef Map.empty
  let fm0 = monospaceMetrics 12
  pure
    Context
      { ctxNodeArena = nodeArena
      , ctxDrawArena = drawArena
      , ctxHotId
      , ctxLastHotId
      , ctxActiveId
      , ctxFocusId
      , ctxPrevRects
      , ctxStore
      , ctxAnimations
      , ctxAnimRest
      , ctxAnyAnimating
      , ctxAnimSettled
      , ctxDirty
      , ctxDamage
      , ctxLastWindowSize
      , ctxIdSalt
      , ctxFontMetrics = fm0
      , ctxMonoFontMetrics = fm0
      , ctxMeasureText = \txt -> pure (measureText PixelHost fm0 (stripWidgetMarkers txt))
      , ctxMeasureCache = Nothing
      , ctxExternalText = False
      , ctxTheme = defaultTheme
      , ctxIcons = asciiIcons
      , ctxContainerStack
      , ctxMessages
      , ctxFocusables
      , ctxScrollDrag
      , ctxTextInputDrag
      , ctxTextInputMenu
      , ctxClipboardGet = pure Nothing
      , ctxClipboardSet = \_ -> pure False
      , ctxTooltips
      , ctxWidgetNodeTypes
      , ctxSelectDropPress
      , ctxModalWasActive
      , ctxModalActive
      , ctxModalDepth
      , ctxEscapeConsumed
      , ctxWindowDrag
      , ctxWindowResize
      , ctxPrevFloatingRects
      , ctxImageAtlas
      , ctxWakeLoop
      , ctxHost
      , ctxHostProfile = PixelHost
      }
