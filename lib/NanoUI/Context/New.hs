module NanoUI.Context.New
  ( newContext
  , newPixelHostContext
  )
where

import Data.IORef (newIORef)
import Data.IntMap.Strict qualified as IM
import Data.Map.Strict qualified as Map
import Data.Primitive.PrimArray (newPrimArray)
import NanoUI.Atlas qualified as Atlas
import NanoUI.Context.Config
  ( enableMeasureCache
  , withExternalText
  , withFontMetrics
  , withTheme
  )
import NanoUI.Context.Internal (Context (..))
import NanoUI.Draw (newDrawArena)
import NanoUI.Font (measureText, monospaceMetrics, stripWidgetMarkers)
import NanoUI.Host (HostProfile (..))
import NanoUI.Icons (asciiIcons)
import NanoUI.Id (WidgetId (..), initialIdContext)
import NanoUI.Frame.SpanArena (newSpanArena)
import NanoUI.Layout.Arena (newNodeArena)
import NanoUI.Store (emptyWidgetStore)
import NanoUI.Style (defaultTheme)
import NanoUI.Types (Damage (..), Size (..))

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
  ctxPrevNodeTexts <- newIORef IM.empty
  ctxStore <- newIORef emptyWidgetStore
  ctxAnimations <- newIORef IM.empty
  ctxAnimRest <- newIORef IM.empty
  ctxAnyAnimating <- newIORef False
  ctxAnimSettled <- newIORef False
  ctxDirty <- newIORef True
  ctxDamage <- newIORef DamageFull
  ctxLastWindowSize <- newIORef (Size 0 0)
  ctxIdContext <- newIORef initialIdContext
  ctxContainerStack <- newIORef []
  ctxMessages <- newIORef []
  let
    initCap = 64
  ctxFocusables <- newIORef =<< newPrimArray initCap
  ctxFocusablesCount <- newIORef 0
  ctxFocusablesCap <- newIORef initCap
  ctxSpanBase <- newSpanArena 64
  ctxSpanOverlay <- newSpanArena 64
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
  ctxPrevFloatingOrder <- newIORef []
  ctxOverlayTopmostCache <- newIORef Nothing
  ctxCurrentFloatingId <- newIORef Nothing
  ctxLastPointerBlocked <- newIORef False
  ctxImageAtlas <- Atlas.newImageAtlas
  ctxWakeLoop <- newIORef Nothing
  ctxHost <- newIORef Map.empty
  let
    fm0 = monospaceMetrics 12
  pure
    Context
      { ctxNodeArena = nodeArena
      , ctxDrawArena = drawArena
      , ctxHotId
      , ctxLastHotId
      , ctxActiveId
      , ctxFocusId
      , ctxPrevRects
      , ctxPrevNodeTexts
      , ctxStore
      , ctxAnimations
      , ctxAnimRest
      , ctxAnyAnimating
      , ctxAnimSettled
      , ctxDirty
      , ctxDamage
      , ctxLastWindowSize
      , ctxIdContext
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
      , ctxFocusablesCount
      , ctxFocusablesCap
      , ctxSpanBase
      , ctxSpanOverlay
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
      , ctxPrevFloatingOrder
      , ctxOverlayTopmostCache
      , ctxCurrentFloatingId
      , ctxLastPointerBlocked
      , ctxImageAtlas
      , ctxWakeLoop
      , ctxHost
      , ctxHostProfile = PixelHost
      }

{-# INLINE newPixelHostContext #-}
newPixelHostContext :: IO Context
newPixelHostContext = do
  ctx0 <- newContext
  ctx <- enableMeasureCache ctx0
  pure
    ( withExternalText
        (withTheme (withFontMetrics ctx (monospaceMetrics 16)) defaultTheme)
        True
    )
