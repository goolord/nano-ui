{-# LANGUAGE StrictData #-}

module NanoUI.Context.Internal
  ( Context (..)
  , PendingTooltip (..)
  , TextInputMenu (..)
  , TextInputDrag (..)
  , WindowResizeEdge (..)
  , WindowResizeDrag (..)
  , MeasureCacheKey
  , intKey
  , markDirty
  , modifyIORefList
  ) where

import Data.Dynamic (Dynamic)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.IntMap.Strict (IntMap)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Typeable (TypeRep)
import Data.Word (Word64)
import NanoUI.Animation (Animation)
import NanoUI.Atlas (ImageAtlas)
import NanoUI.Draw (DrawArena)
import NanoUI.Font (FontMetrics)
import NanoUI.Host (HostProfile)
import NanoUI.Icons (Icons)
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Layout.Arena (NodeArena, NodeType)
import NanoUI.Messages (FrameMsg)
import NanoUI.Store (WidgetStore)
import NanoUI.Style (Theme)
import NanoUI.Types (Damage (..), Rect (..), Size (..))

data PendingTooltip = PendingTooltip
  { pendingTooltipWidget :: WidgetId
  , pendingTooltipRect :: Rect
  , pendingTooltipText :: Text
  }
  deriving (Eq, Show)

data TextInputMenu = TextInputMenu
  { textInputMenuWidget :: WidgetId
  , textInputMenuRect :: Rect
  }
  deriving (Eq, Show)

data TextInputDrag = TextInputDrag
  { textInputDragWidget :: WidgetId
  , textInputDragAnchor :: Int
  , textInputDragClicks :: Int
  }
  deriving (Eq, Show)

data WindowResizeEdge
  = ResizeN
  | ResizeS
  | ResizeE
  | ResizeW
  | ResizeNE
  | ResizeNW
  | ResizeSE
  | ResizeSW
  deriving (Eq, Show)

data WindowResizeDrag = WindowResizeDrag
  { wrdWidget :: WidgetId
  , wrdEdge :: WindowResizeEdge
  , wrdGrabX :: Float
  , wrdGrabY :: Float
  , wrdStartX :: Float
  , wrdStartY :: Float
  , wrdStartW :: Float
  , wrdStartH :: Float
  , wrdMinW :: Float
  , wrdMinH :: Float
  , wrdMaxW :: Float
  , wrdMaxH :: Float
  }
  deriving (Eq, Show)

type MeasureCacheKey = (Text, Bool, Float)

data Context = Context
  { ctxNodeArena :: NodeArena
  , ctxDrawArena :: DrawArena
  , ctxHotId :: IORef WidgetId
  , ctxLastHotId :: IORef WidgetId
  , ctxActiveId :: IORef WidgetId
  , ctxFocusId :: IORef WidgetId
  , ctxPrevRects :: IORef (IntMap Rect)
  , ctxStore :: IORef WidgetStore
  , ctxAnimations :: IORef (IntMap Animation)
  , ctxAnimRest :: IORef (IntMap Float)
  , ctxAnyAnimating :: IORef Bool
  , ctxAnimSettled :: IORef Bool
  , ctxDirty :: IORef Bool
  , ctxDamage :: IORef Damage
  , ctxLastWindowSize :: IORef Size
  , ctxIdSalt :: IORef Word64
  , ctxFontMetrics :: FontMetrics
  , ctxMonoFontMetrics :: FontMetrics
  , ctxMeasureText :: Text -> IO (Float, Float)
  , ctxMeasureCache :: Maybe (IORef (Map MeasureCacheKey (Float, Float)))
  , ctxExternalText :: Bool
  , ctxTheme :: Theme
  , ctxIcons :: Icons
  , ctxContainerStack :: IORef [Int]
  , ctxMessages :: IORef [FrameMsg]
  , ctxFocusables :: IORef [WidgetId]
  , ctxScrollDrag :: IORef (Maybe (WidgetId, Float))
  , ctxTextInputDrag :: IORef (Maybe TextInputDrag)
  , ctxTextInputMenu :: IORef (Maybe TextInputMenu)
  , ctxClipboardGet :: IO (Maybe String)
  , ctxClipboardSet :: String -> IO Bool
  , ctxTooltips :: IORef [PendingTooltip]
  , ctxWidgetNodeTypes :: IORef (Maybe (IntMap NodeType))
  , ctxSelectDropPress :: IORef Bool
  , ctxModalWasActive :: IORef Bool
  , ctxModalActive :: IORef Bool
  , ctxModalDepth :: IORef Int
  , ctxEscapeConsumed :: IORef Bool
  , ctxWindowDrag :: IORef (Maybe (WidgetId, Float, Float))
  , ctxWindowResize :: IORef (Maybe WindowResizeDrag)
  , ctxPrevFloatingRects :: IORef (IntMap Rect)
  , ctxImageAtlas :: ImageAtlas
  , ctxWakeLoop :: IORef (Maybe (IO ()))
  , ctxHost :: IORef (Map TypeRep Dynamic)
  , ctxHostProfile :: HostProfile
  }

{-# INLINE intKey #-}
intKey :: WidgetId -> Int
intKey wid = fromIntegral (hashWidgetId wid)

{-# INLINE markDirty #-}
markDirty :: Context -> IO ()
markDirty ctx = do
  writeIORef (ctxDirty ctx) True
  mWake <- readIORef (ctxWakeLoop ctx)
  case mWake of
    Just wake -> wake
    Nothing -> pure ()

modifyIORefList :: IORef [a] -> ([a] -> [a]) -> IO ()
modifyIORefList ref f = readIORef ref >>= writeIORef ref . f
